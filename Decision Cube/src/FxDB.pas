unit FxDB;

interface

uses
  Variants, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, DB, FxArrays, FxStore, FxCommon, FxCache, FxMap;

type
  EDimIndexError = class(Exception);

  TDimGroup = (dgRow, dgCol, dgPage, dgNone);

  TRowStates= (rcNextOpen, rcPrevOpen, rcNextClosed, rcPrevClosed);
  TRowState = set of TRowStates;

  TDimState  = (dmClosed, dmOpen, dmDrilled, dmPaged, dmNone);
  TDimStates = set of TDimState;

  TDecisionControlType = (xtCheck, xtRadio, xtRadioEx);

  PDimInfo = ^TDimInfo;
  TDimInfo = Record
    iState: TDimState;
    iValue: Integer;       // Drilled/Paged Value
    iIndex: Integer;       // Index in the group
    iActiveIndex: Integer; // если открыт, то индекс, иначе -1
    iGroup: TDimGroup;
    Name  : string;        // The same as TFxMapItem.Name
    Dim   : Integer;       // Map to TFxCube.Dimensions[]
  end;

  TDimRange = Record
    First: Integer;
    Last: Integer;
  end;

  TDimInfoList=class(TList)
  private
    FActiveCount:Integer;
    function GetItems(const Index: Integer): PDimInfo;
    procedure SetItems(const Index: Integer; const Value: PDimInfo);
  public
    function Add(Item:PDimInfo):Integer;
    function IndexOfActive(const Index:Integer):Integer;
    procedure Delete(Index:Integer);
    procedure Insert(Index:Integer; Item:PDimInfo);
    property ActiveCount:Integer read FActiveCount;
    property Items[const Index:Integer]:PDimInfo read GetItems write SetItems;default;
  end;

  TDimInfoArray = class
  private
    FElements: array of TDimInfo;
    procedure Assign( Value: TDimInfoArray);
    function GetGroupSize(Group: TDimGroup; bOpen: Boolean): Integer;
    //function IsEqual( Value: TDimInfoArray): Boolean;
    function GetCount:Integer;
    function  GetItem(Index: Integer): PDimInfo;
  protected
  public
    constructor Create(ACount: Integer);
    destructor Destroy; override;
    function Find(const Name: string; out pos: Integer): Boolean;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: PDimInfo read GetItem; default;
  end;

  TPivotState=class(TPersistent)
  private
    FDims: Integer;
    FSums: Integer;
    FCurrentSum: Integer;
    FRowSubs: Boolean;
    FColSubs: Boolean;
    FRowSparse: Boolean;
    FColSparse: Boolean;
    FList:array[dgRow..dgCol]of TDimInfoList;
    DimInfo: TDimInfoArray;
    function GetItems(Index: Integer): PDimInfo;
    procedure RebuildList(const AGroup:TDimGroup);
    function GetActiveColCount: Integer;
    function GetActiveRowCount: Integer;
    function GetColCount: Integer;
    function GetRowCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source:TPersistent);override;
    function FindByState(const AGroup:TDimGroup; const AStates:TDimStates):PDimInfo;
    function IndexInfo(AGroup:TDimGroup; Index:Integer; Active:Boolean):PDimInfo;
    procedure Update(ACube:TFxCustomStore);
    property ActiveColCount:Integer read GetActiveColCount;
    property ActiveRowCount:Integer read GetActiveRowCount;
    property ColCount:Integer read GetColCount;
    property RowCount:Integer read GetRowCount;
    property Items[Index:Integer]:PDimInfo read GetItems;default;
  end;


  TFxCube = class;
  TFxSource = class;

  TDecisionDataLink = class(TPersistent)
  private
  protected
    FDecisionSource: TFxSource;
    FBlocked: Boolean;
    procedure SetDecisionSource(source: TFxSource);
    procedure DecisionDataEvent(Event: TDecisionDataEvent); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property DecisionSource: TFxSource read FDecisionSource write SetDecisionSource;
  end;

  TFxSource = class(TComponent)
  private
    FChangeCount: Integer;
    FState: TCubeState;
    FBlocked: Boolean;
    FDecisionCube: TFxCube;
    FControlType: TDecisionControlType;
    FDataLinks: TList;
    FSavePivotState: TPivotState;
    FData: TPivotState;
    RowLookup: TTwoDimArray;
    ColLookup: TTwoDimArray;
    FRowMax: Integer;
    FColMax: Integer;
    FOnBeforePivot: TNotifyEvent;
    FOnAfterPivot: TNotifyEvent;
    FOnStateChange: TNotifyEvent;
    FOnNewDimensions: TNotifyEvent;
    FOnLayoutChange: TNotifyEvent;
    FOnSummaryChange: TNotifyEvent;
    function GetActive: Boolean;
    function GetReady: Boolean;
    function GetExampleRepCount(dimGroup: TDimGroup; level: Integer): Integer;
    function GetRowSparsing: Boolean;
    function GetColSparsing: Boolean;
    function GetDims: Integer;
    function GetSums: Integer;
    function GetCurrentSum: Integer;
    procedure RebuildPivotState;
    procedure BuildLookups;
    procedure AddDatalink(const DataLink: TDecisionDatalink);
    procedure RemoveDatalink (const DataLink: TDecisionDatalink);
    procedure NotifyDataLinks(Event: TDecisionDataEvent);
    procedure ReadDimCount(Reader: TReader);
    procedure WriteDimCount(Writer: TWriter);
    procedure ReadSumCount(Reader: TReader);
    procedure WriteSumCount(Writer: TWriter);
    procedure ReadCurrentSum(Reader: TReader);
    procedure WriteCurrentSum(Writer: TWriter);
    procedure ReadRowSparse(Reader: TReader);
    procedure WriteRowSparse(Writer: TWriter);
    procedure ReadColSparse(Reader: TReader);
    procedure WriteColSparse(Writer: TWriter);
    procedure ReadDimInfo(Reader: TReader);
    procedure WriteDimInfo(Writer: TWriter);
    procedure DataEvent(Event: TDecisionDataEvent);
    procedure SetRowSparsing(Value: Boolean);
    procedure SetColSparsing(Value: Boolean);
    procedure SetDecisionCube(Value: TFxCube);
    procedure ProcessPivotState(FState: TPivotState);
    procedure BeginChange; virtual;
    procedure EndChange(event: TDecisionDataEvent); virtual;
    procedure SetState(Value:TCubeState);
    procedure UpdateState;
  protected
    procedure SetDimState(iDim: Integer; State: TDimState; ValueIndex: Integer);
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure DefineProperties(Filer: TFiler); override;
    { Meta data and data fetching }
    function DimDef(Ix:Integer):TFxMapItem;
    function SumDef(Ix:Integer):TFxMapItem;
    function GetMemberAsString(iDim: Integer; ValueIndex: Integer): String;
    function GetMemberAsVariant(iDim: Integer; ValueIndex: Integer): Variant;
    function GetDimensionName(iDim: Integer): String;
    function GetDimensionMemberCount(iDim: Integer): Integer;
    function GetSummaryName(iSum: Integer): String;
    procedure SetCurrentSummary(Value: Integer);
    { Graph specific data fetching }
    function Get2DDataAsVariant(iDimA, iDimB: Integer; aValueIndex, bValueIndex:Integer): Variant;
    { Grid specific data fetching }
    function GetDataAsString(ARow, ACol:Integer; var SubLevel: Integer): String;
    function GetDataAsVariant(Arow, ACol: Integer; out SubLevel: Integer): Variant;
    function GetValueIndex(dimGroup: TDimGroup; Index: Integer; Cell: Integer; out isBreak: Boolean; var isSum: Boolean) : Integer;
    function GetValueArray(ACol, ARow: Integer; var ValueArray: TSmallIntArray): Boolean;
    function GetGroupExtent(dimGroup: TDimGroup; Index: Integer; Cell: Integer): TDimRange;
    { Active or Inactive Row/Col relative pivoting functions }
    procedure DrillValue(iDim: Integer; ValueIndex: Integer);
    procedure ToggleDim(Dim:Integer);
    procedure OpenDimIndexRight(dimGroup: TDimGroup; Index: Integer; bOpen: Boolean);
    procedure CloseDimIndexRight(dimGroup: TDimGroup; Index: Integer; bOpen: Boolean);
    procedure OpenDimIndexLeft(dimGroup: TDimGroup; Index: Integer; bOpen: Boolean);
    procedure ToggleDimIndex(dimGroup: TDimGroup; Index: Integer; bOpen: Boolean);
    procedure DrillDimIndex(dimGroup: TDimGroup; Index: Integer; ValueIndex: Integer; bOpen:Boolean);
    procedure MoveDimIndexes(SdimGroup,DdimGroup:TDimGroup; Src,Dest:Integer; bOpen:Boolean);
    procedure SwapDimIndexes(SdimGroup,DdimGroup:TDimGroup; Src,Dest:Integer; bOpen:Boolean);
    function GetGroupCount(dimGroup: TDimGroup; bOpen: Boolean):Integer;
    function GetRowState(iDim: Integer): TRowState;
    property Active:Boolean read GetActive;
    property Ready: Boolean read GetReady;
    { pivot State Information }
    property nDims: Integer read GetDims;
    property nSums: Integer read GetSums;
    property nDataRows: Integer read FRowMax;
    property nDataCols: Integer read FColMax;
    property CurrentSum: Integer read GetCurrentSum;
    property Pivot: TPivotState read FData;
  published
    property ControlType: TDecisionControlType read FControlType write FControlType;
    property DecisionCube: TFxCube read FDecisionCube write SetDecisionCube;
    property Name;
    property SparseRows: Boolean read GetRowSparsing write SetRowSparsing default False;
    property SparseCols: Boolean read GetColSparsing write SetColSparsing default False;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property OnNewDimensions: TNotifyEvent read FOnNewDimensions write FOnNewDimensions;
    property OnLayoutChange: TNotifyEvent read FOnLayoutChange write FOnLayoutChange;
    property OnSummaryChange: TNotifyEvent read FOnSummaryChange write FOnSummaryChange;
    property OnBeforePivot: TNotifyEvent read FOnBeforePivot write FOnBeforePivot;
    property OnAfterPivot: TNotifyEvent read FOnAfterPivot write FOnAfterPivot;
  end;

  TQADecisionSource = class(TFxSource);

  TFxCube = class(TFxCustomStore)
  private
    FDecisionSources:TList;
    FBlocked: Boolean;
    function GetSparsing: Boolean;
    procedure SetSparsing(Value: Boolean );
    procedure AddDataSource(source: TFxSource);
    procedure RemoveDataSource(source: TFxSource);
    property Sparsing: Boolean read GetSparsing write SetSparsing;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure NotifyDataSources(Event: TDecisionDataEvent);override;
    procedure StateChanged; override;
  published
    property Active;
    property DataSet;
    property DesignState;
    property DimensionMap;
    property Externals;
    property MaxDimensions;
    property MaxSummaries;
    property MaxCells;
    property ShowProgressDialog;
    property OnLowCapacity;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property OnCall;
    property OnRefresh;
    property OnGetSQL;
  end;

implementation

uses Math, FxDCube;

resourcestring
  SOutofBounds          = 'Out of Bounds';

const
  defDimSize = 20;

{ TFxCube }

constructor TFxCube.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDecisionSources := TList.Create;
  FBlocked := False;
  DesignState := dsAllData;
  RCS;
end;

destructor TFxCube.Destroy;
begin
  while FDecisionSources.Count > 0 do
    TFxSource(FDecisionSources.Last).DecisionCube := nil;
  assert(FDecisionSources.count = 0, 'Decision Sources did not free correctly');  //$$$ leave asserts? [adm]
  FDecisionSources.Free;
  inherited Destroy;
end;

procedure TFxCube.AddDataSource(Source:TFxSource);
begin
  FDecisionSources.Add(Source);
  Source.FDecisionCube:= Self;
  Source.UpdateState;
end;

procedure TFxCube.RemoveDataSource(source: TFxSource);
begin
  Source.FDecisionCube := nil;
  FDecisionSources.Remove(Source);
  Source.UpdateState;
end;

procedure TFxCube.NotifyDataSources(Event: TDecisionDataEvent);
var
  I:Integer;
begin
  for I:=FDecisionSources.Count - 1 downto 0 do
    with TFxSource(FDecisionSources[I]) do
      DataEvent(Event);
end;

procedure TFxCube.StateChanged;
var
  I:Integer;
begin
  if State in [dcBrowseMemberData,dcBrowseAllData] then begin
    with DataCache do
      for I:=0 to Dimensions.Count-1 do
        Dimensions[I].Def.ValueCount:=Dimensions[I].Count;
  end;
  NotifyDataSources(xeStateChanged);
end;

function TFxCube.GetSparsing: Boolean;
begin
  Result := DataCache.Sparsing;
end;

procedure TFxCube.SetSparsing(Value: Boolean);
begin
  DataCache.Sparsing := Value;
end;

{ TFxSource }

constructor TFxSource.Create( AOwner: TComponent );
begin
  inherited Create(AOwner);
  FDataLinks:= TList.Create;
  FState   := dcInactive;
  FBlocked := False;
  FData := TPivotState.create;
  FSavePivotState := TPivotState.Create;
//  SetUpData;
  RCS;
end;

destructor TFxSource.Destroy;
begin
  FOnStateChange:=nil;
  SetDecisionCube(nil);
  while FDataLinks.Count>0 do
    RemoveDataLink(FDataLinks.Last);
  FDataLinks.Free;
  FreeAndNil(FData);
  FreeAndNil(FSavePivotState);
  inherited Destroy;
end;

procedure TFxSource.SetDimState(iDim: Integer; State: TDimState; ValueIndex: Integer);
var
  aDimInfo: pDimInfo;
begin
  aDimInfo := FData.DimInfo[iDim];
  if (aDimInfo.iState = State) and (aDimInfo.iValue = ValueIndex) then Exit;
  BeginChange;
  aDimInfo.iState := State;
  if State = dmDrilled then
    aDimInfo.iValue := ValueIndex
  else if State = dmPaged then
    aDimInfo.iValue := 0
  else
    aDimInfo.iValue := -1;
  EndChange(xePivot);
end;

function TFxSource.GetGroupCount(dimGroup: TDimGroup; bOpen: Boolean): Integer;
begin
  Result := FData.DimInfo.GetGroupSize(dimGroup, bOpen);
end;

function TFxSource.GetRowState(iDim: Integer): TRowState;
var
  List:TDimInfoList;
  I:Integer;
begin
  Result:=[];
  with FData.DimInfo.FElements[iDim]do begin
    List:=FData.FList[iGroup];
    if iIndex<List.Count then begin
      I:=iIndex+1;
      while (I<List.Count)and not(List[I].iState in [dmOpen,dmClosed]) do
        Inc(I);
      if I<List.Count then begin
        if List[I].iState=dmOpen then
          Include(Result,rcNextOpen)
        else
          Include(Result,rcNextClosed);
      end;
      I:=iIndex-1;
      while (I>0)and not(List[I].iState in [dmOpen,dmClosed]) do
        Dec(I);
      if I>=0 then begin
        if List[I].iState=dmOpen then
          Include(Result,rcPrevOpen)
        else
          Include(Result,rcPrevClosed);
      end;
    end;
  end;
end;

function TFxSource.GetDims: Integer;
begin
  Result := FData.FDims;
end;

function TFxSource.GetSums: Integer;
begin
  Result := FData.FSums;
end;

function TFxSource.GetCurrentSum: Integer;
begin
  Result := FData.FCurrentSum;
end;

function TFxSource.GetRowSparsing: Boolean;
begin
  Result := FData.FRowSparse;
end;

procedure TFxSource.SetRowSparsing(Value: Boolean);
begin
  if FData.FRowSparse <> Value then begin
    if Ready then begin
      BeginChange;
      FData.FRowSparse := Value;
      EndChange(xePivot);
      UpdateDesigner(self);
    end;
  end;
end;

function TFxSource.GetColSparsing: Boolean;
begin
  Result := FData.FColSparse;
end;

procedure TFxSource.SetColSparsing(Value: Boolean);
begin
  if FData.FColSparse <> Value then
  begin
    if Ready then
    begin
      BeginChange;
      FData.FColSparse := Value;
      EndChange(xePivot);
      UpdateDesigner(self);
    end;
  end;
end;

procedure TFxSource.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('DimensionCount',ReadDimCount,WriteDimCount,True);
  Filer.DefineProperty('SummaryCount',ReadSumCount,WriteSumCount,True);
  Filer.DefineProperty('CurrentSummary',ReadCurrentSum,WriteCurrentSum,True);
  Filer.DefineProperty('SparseRows',ReadRowSparse,WriteRowSparse,True);
  Filer.DefineProperty('SparseCols',ReadColSparse,WriteColSparse,True);
  Filer.DefineProperty('DimensionInfo',ReadDimInfo,WriteDimInfo,True);
end;

procedure TFxSource.ReadDimCount(Reader: TReader);
begin
  FSavePivotState.FDims := Reader.ReadInteger;
end;

procedure TFxSource.WriteDimCount(Writer: TWriter);
begin
  if Assigned(FData.DimInfo) then
    Writer.WriteInteger(FData.DimInfo.Count)
  else
    Writer.WriteInteger(0);
end;

procedure TFxSource.ReadSumCount(Reader: TReader);
begin
  FSavePivotState.FSums := Reader.ReadInteger;
end;

procedure TFxSource.WriteSumCount(Writer: TWriter);
begin
  Writer.WriteInteger(Fdata.FSums);
end;
procedure TFxSource.ReadCurrentSum(Reader: TReader);
begin
  FSavePivotState.FCurrentSum := Reader.ReadInteger;
end;

procedure TFxSource.WriteCurrentSum(Writer: TWriter);
begin
  Writer.WriteInteger(FData.FCurrentSum)
end;

procedure TFxSource.ReadRowSparse(Reader: TReader);
begin
  FSavePivotState.FRowSparse := Reader.ReadBoolean;
end;

procedure TFxSource.WriteRowSparse(Writer: TWriter);
begin
  Writer.WriteBoolean(FData.FRowSparse)
end;

procedure TFxSource.ReadColSparse(Reader: TReader);
begin
  FSavePivotState.FColSparse := Reader.ReadBoolean;
end;

procedure TFxSource.WriteColSparse(Writer: TWriter);
begin
  Writer.WriteBoolean(FData.FColSparse)
end;

procedure TFxSource.ReadDimInfo(Reader: TReader);
var
  i,x: Integer;
  aDimInfo: PDimInfo;
begin
  FSavePivotState.DimInfo.Free;
  FSavePivotState.DimInfo:= TDimInfoArray.Create(FSavePivotState.FDims);
  with FData do begin
    Reader.ReadListBegin;
    for i := 0 to FSavePivotState.DimInfo.Count-1 do
    begin
      aDimInfo := FSavePivotState.DimInfo[i];
      x := Reader.ReadInteger;
      case x of
        0: aDimInfo.iGroup := dgNone;
        1: aDimInfo.iGroup := dgRow;
        2: aDimInfo.iGroup := dgCol;
      end;
      aDimInfo.iActiveIndex := Reader.ReadInteger;
      case Reader.ReadInteger of
        0: aDimInfo.iState := dmNone;
        1: aDimInfo.iState := dmOpen;
        2: aDimInfo.iState := dmClosed;
        3: aDimInfo.iState := dmDrilled;
        4: aDimInfo.iState := dmPaged;
      end;
      aDimINfo.iIndex := Reader.ReadInteger;
      aDimInfo.iValue := Reader.ReadInteger;
      if Reader.NextValue=vaString then
        aDimInfo.Name   := Reader.ReadString;
    end;
    Reader.ReadListEnd;
    ProcessPivotState(FSavePivotState);
    FSavePivotState.DimInfo.Free;
    FSavePivotState.DimInfo:= nil;
    //RebuildPivotState;
    //Buildlookups;
    //NotifyDataLinks(xeNewMetaData);
  end;
end;

procedure TFxSource.WriteDimInfo(Writer: TWriter);
var
  i: Integer;
  aValue: Integer;
  aDimInfo: PDimInfo;
begin
  Writer.WriteListBegin;
  if Assigned(FData.DimInfo) then
    with FData do
    begin
      for i := 0 to FData.DimInfo.Count-1 do
      begin
        aDimInfo := DimInfo[i];
        if aDimInfo.iGroup = dgRow then
          aValue := 1
        else if aDimInfo.iGroup = dgCol then
          aValue := 2
        else
          aValue := 0;
        Writer.WriteInteger(aValue);
        Writer.WriteInteger(aDimInfo.iActiveIndex);
        if aDimInfo.iState = dmOpen then
          aValue := 1
        else if aDimInfo.iState = dmClosed then
          aValue := 2
        else if aDimInfo.iState = dmDrilled then
          aValue := 3
        else if aDimInfo.iState = dmPaged then
          aValue := 4
        else
          aValue := 0;
        Writer.WriteInteger(aValue);
        Writer.WriteInteger(aDimINfo.iIndex);
        Writer.WriteInteger(aDimInfo.iValue);
        Writer.WriteString(aDimInfo.Name);
      end;
    end;
  Writer.WriteListEnd;
end;

procedure TFxSource.BeginChange;
begin
  FChangeCount := FChangeCount + 1;
end;

procedure TFxSource.EndChange(Event: TDecisionDataEvent);
begin
  RebuildPivotState;
  if FChangeCount <= 1 then begin
    if Assigned(FOnBeforePivot) then begin
      FOnBeforePivot(self);
      RebuildPivotState;
    end;
    BuildLookups;
    FChangeCount:= 0;
    NotifyDataLinks(Event);
    if Assigned(FOnAfterPivot) then FOnAfterPivot(Self);
  end else begin
    FChangeCount := FChangeCount-1;
  end;
end;

{
  Reset the DimInfo information IIndex and IRowState

  Coming here, RowAllDim and ColAllDim contain the information about
  row and column placement, and DimInfo is correct for everything else.
  This routine brings these two into correspondence, and also resets
  FAllRows, FAllCols, FActiveRows, FActiveCols
}

procedure TFxSource.RebuildPivotState;
begin
  with FData do begin
    RebuildList(dgRow);
    RebuildList(dgCol);
    if FCurrentSum >= FSums then FCurrentSum := 0;
  end;
end;

procedure TFxSource.BuildLookups;
var
  I: Integer;
  RowDim, ColDim: array of Integer;
  Vector:TVector;
begin
  RowLookup:= nil;
  ColLookup:= nil;
  FRowMax:=1;
  FColMax:=1;
  {
    rebuild the lookup tables from the Row and Col Dimension Info
    This needs to be done at every pivot or drill
  }
  with FData do begin
    if (not Ready) or (FDims=0) or (FSums=0) then Exit;
    { Now initialize the summaries to reflect the correct FActiveSum }
    if FCurrentSum>=FSums then
      FCurrentSum:= 0;
    DecisionCube.SetCurrentSummary(FCurrentSum);
    if (FState=dcBrowseAllData)or(FState=dcBrowseMemberData)then begin
      SetLength(RowDim,ActiveRowCount);
      SetLength(ColDim,ActiveColCount);
      SetLength(Vector,DecisionCube.DataCache.Dimensions.Count);
      for I:=0 to High(RowDim) do RowDim[I]:=0;
      for I:=0 to High(ColDim) do ColDim[I]:=0;
      for I:=0 to DecisionCube.DataCache.Dimensions.Count-1 do Vector[I]:=-1;
      for I:=0 to DimInfo.Count-1 do begin
        if DimInfo[I].iState=dmOpen then begin
          if DimInfo[I].iGroup=dgRow then begin
            Assert(DimInfo[I].iActiveIndex<=High(RowDim), 'Error is active rows');
            RowDim[DimInfo[I].iActiveIndex]:=I;
          end;
          if DimInfo[I].iGroup=dgCol then begin
            Assert(DimInfo[I].iActiveIndex<=High(ColDim), 'Error is active cols');
            ColDim[DimInfo[I].iActiveIndex]:=I;
          end;
        end;
      end;{for}
      if FState<>dcBrowseAllData then
        FDecisionCube.Sparsing:=False
      else
        FDecisionCube.Sparsing:= not FRowSparse;
      for I:=0 to DecisionCube.DataCache.Dimensions.Count-1 do begin
        Vector[I]:=-1;
        if DimInfo[I].iState=dmDrilled then Vector[I]:=DimInfo[I].iValue;
      end;
      FRowMax:=FDecisionCube.GetDomain(RowDim,Vector,FRowSubs,RowLookup);
      if FState<>dcBrowseAllData then
        FDecisionCube.Sparsing:=False
      else
        FDecisionCube.Sparsing:= not FColSparse;
      for I:=0 to DecisionCube.DataCache.Dimensions.Count-1 do begin
        Vector[I]:=-1;
        if DimInfo[I].iState=dmDrilled then Vector[I]:=DimInfo[I].iValue;
      end;
      FColMax:=FDecisionCube.GetDomain(ColDim,Vector,FColSubs,ColLookup);
    end else if (FState=dcBrowseMetaData)and(ActiveColCount<>0) then begin
      FColMax := ActiveColCount;
    end;
  end;{with}
end;

procedure TFxSource.ProcessPivotState(FState: TPivotState);
begin
  with FData do begin
    FDims := FState.FDims;
    FSums := FState.FSums;
    if not assigned(DimInfo) then
      DimInfo := TDimInfoArray.create(FDims);
    DimInfo.Assign(FState.DimInfo);
    FCurrentSum:= FState.FCurrentSum;
    FRowSparse := FState.FRowSparse;
    FColSparse := FState.FColSparse;
    FRowSubs := True;
    FColSubs := True;
  end;
end;

procedure TFxSource.AddDataLink(const DataLink:TDecisionDataLink);
begin
  FDataLinks.Add(DataLink);
  DataLink.FDecisionSource:=Self;
end;

procedure TFxSource.RemoveDataLink(const DataLink: TDecisionDataLink);
begin
  DataLink.FDecisionSource:= nil;
  FDataLinks.Remove(DataLink);
  //DataLink.UpdateState;
end;

procedure TFxSource.NotifyDataLinks(Event: TDecisionDataEvent);
var
  I: Integer;
begin
  if FChangeCount>0 then Exit;
  for I := FDataLinks.Count - 1 downto 0 do
    with TDecisionDataLink(FDataLinks[I]) do
      DecisionDataEvent(Event);
    if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    begin
      if Event in [xePivot, xeSummaryChanged] then
        UpdateDesigner(self);
    end;
    case Event of
    xePivot:
      if assigned(FOnLayoutChange) then
        FOnLayoutChange(self);
    xeNewMetaData:
      if assigned(FOnNewDimensions) then
        FOnNewDimensions(self);
    xeSummaryChanged:
      if assigned(FOnSummaryChange) then
        FOnSummaryChange(self);
    end;
end;

{
  Pivoting functions

  open the first inactive row/col to the immediate right of the Active row/col = Index
  if Index = -1, it means open the first row
}

procedure TFxSource.OpenDimIndexRight(dimGroup: TDimGroup; Index: Integer; bOpen: Boolean);
var
  List:TDimInfoList;
begin
  with FData do begin
    List:=FList[dimGroup];
    if bOpen then Index:=List.IndexOfActive(Index);
    for Index:=Index+1 to List.Count-1 do
      if List[Index].iState=dmClosed then begin
        BeginChange;
        List[Index].iState:=dmOpen;
        EndChange(xePivot);
        Break;
      end;
  end;
end;

{
  close all active rows/cols to the immediate right of the Active row/col = Index
  if Index = -1, it means start with the first row/col
}

procedure TFxSource.CloseDimIndexRight(dimGroup: TDimGroup; Index: Integer; bOpen: Boolean);
var
  List:TDimInfoList;
begin
  with FData do begin
    List:=FList[dimGroup];
    if bOpen then Index:=List.IndexOfActive(Index);
    for Index:=Index+1 to List.Count-1 do
      if List[Index].iState=dmOpen then begin
        BeginChange;
        List[Index].iState:=dmClosed;
        EndChange(xePivot);
      end;
  end;
end;

procedure TFxSource.OpenDimIndexLeft(dimGroup: TDimGroup; Index: Integer; bOpen: Boolean);
var
  List:TDimInfoList;
begin
  with FData do  begin
    List:=FList[dimGroup];
    if bOpen then Index:=List.IndexOfActive(Index);
    for Index:=Index-1 downto 0 do
      if List[Index].iState=dmClosed then begin
        BeginChange;
        List[Index].iState:=dmOpen;
        EndChange(xePivot);
        Break;
      end;
  end;
end;

procedure TFxSource.ToggleDimIndex(dimGroup: TDimGroup; Index: Integer; bOpen: Boolean);
var
  List: TDimInfoList;
begin
  with FData do begin
    List:=FList[dimGroup];
    if bOpen then Index:=List.IndexOfActive(Index);
    with List[Index]^ do begin
      BeginChange;
      case iState of
      dmOpen : iState:=dmClosed;
      dmPaged: Exit;
      else
        iState:=dmOpen;
        iValue:=-1;
      end;
      EndChange(xePivot);
    end;
  end;
end;

// Data Access Functions

function  TFxSource.GetReady: Boolean;
begin
  Result:= FState<>dcInactive;
end;

function  TFxSource.GetDataAsString(ARow,ACol:Integer; var SubLevel: Integer): String;
var
  I, iLook: Integer;
  DI: pDimInfo;
  Coord: TVector;
begin
  Result:='';
  if Ready and (FState=dcBrowseAllData) then
    with FData do begin
      SetLength(Coord,FDims);
      SubLevel:=0;
      for I:=0 to FData.DimInfo.Count-1 do begin
        DI:=DimInfo[i];
        if (DI.IState=dmOpen) and (DI.iGroup=dgRow) then begin
          iLook:=RowLookup[DI.iActiveIndex, aRow];
          if iLook=Subtotal then SubLevel:=SubLevel + 1;
        end else if (DI.IState=dmOpen) and (DI.iGroup=dgCol) then begin
          iLook := ColLookup[DI.iActiveIndex, aCol];
          if (iLook = Subtotal) then SubLevel := SubLevel + 1;
        end else if DimInfo[I].IState in [dmDrilled, dmPaged] then
          iLook:= DimInfo[I].IValue
        else
          iLook:= SubTotal;
        Coord[i]:= iLook;
      end;
      Result := FDecisionCube.GetSummaryAsString(Coord);
    end;
end;

function TFxSource.GetMemberAsString(iDim:Integer; ValueIndex:Integer): String;
begin
  if Ready and (FState in [dcBrowseAllData,dcBrowseMemberData]) then
    Result:= FDecisionCube.DataCache.Dimensions[iDim].AsString(ValueIndex)
  else
    Result:= '';
end;

function TFxSource.GetMemberAsVariant(iDim: Integer; ValueIndex: Integer): Variant;
begin
  if Ready and ((FState = dcBrowseAllData) or (FState = dcBrowseMemberData)) then
    Result := FDecisionCube.DataCache.Dimensions[iDim][ValueIndex]
  else
    Result := '';
end;

function TFxSource.GetSummaryName(iSum: Integer): String;
begin
  Result := '';
  if Ready then
    Result := FDecisionCube.DataCache.Summaries[ISum].Name;
end;

procedure TFxSource.SetCurrentSummary(Value: Integer);
begin
  if Assigned(DecisionCube) and (Value<DecisionCube.DataCache.Summaries.Count) and (Value >= 0) then
  begin
    FData.FCurrentSum := Value;
    DecisionCube.SetCurrentSummary(Value);
  end;
//  NotifyDataLinks(xeSummaryChanged);
end;

function  TFxSource.GetDimensionName(iDim: Integer): String;
begin
  Result := '';
  if Ready then
    Result := FDecisionCube.DataCache.Dimensions[iDim].Name;
end;

function TFxSource.GetDimensionMemberCount(iDim: Integer): Integer;
begin
  if Ready and ((FState = dcBrowseAllData) or (FState = dcBrowseMemberData)) then
    Result := FDecisionCube.DataCache.Dimensions[iDim].Count
  else
    Result := 0;
end;

function  TFxSource.GetDataAsVariant(ARow, ACol: Integer; out SubLevel: Integer): Variant;
var
  I, iLook: Integer;
  DI: pDimInfo;
  Coord: TVector;
begin
  if Ready and (FState = dcBrowseAllData) then
    with FData do begin
      SetLength(Coord,FDims);
      SubLevel := 0;
      for i := 0 to FData.DimInfo.Count-1 do begin
        DI := DimInfo[i];
        if (DI.IState = dmOpen) and (DI.iGroup = dgRow) then begin
          iLook := RowLookup[DI.iActiveIndex, aRow];
          if (iLook = Subtotal) then SubLevel := SubLevel + 1;
        end else if (DI.IState = dmOpen) and (DI.iGroup = dgCol) then begin
          iLook := ColLookup[DI.iActiveIndex, aCol];
          if (iLook = Subtotal) then SubLevel := SubLevel + 1;
        end else if (DimInfo[I].IState in [dmDrilled, dmPaged]) then
          iLook := DimInfo[I].IValue
        else
          iLook := subtotal;
        Coord[i] := iLook;
      end;
      Result := FDecisionCube.GetSummaryAsVariant(Coord);
    end
  else
    Result := '';
end;

function TFxSource.Get2DDataAsVariant(iDimA, iDimB: Integer;
         aValueIndex, bValueIndex: Integer): Variant;
var
  I: Integer;
  Coord: TVector;
begin
  if Ready and (FState = dcBrowseAllData) then
    with FData do begin
      SetLength(Coord,FDims);
      for I:=0 to High(Coord) do  begin
        if DimInfo[I].IState in [dmDrilled, dmPaged] then
          Coord[I]:= DimInfo[I].IValue
        else if I=iDimA then
          Coord[i]:= aValueIndex
        else if I=iDimB then
          Coord[i]:=bValueIndex
        else
          Coord[I]:=Subtotal;
      end;
      Result:= FDecisionCube.GetSummaryAsVariant(Coord);
      if VarIsNull(Result) then Result:=0;
    end
  else
    Result := 0;
end;

function TFxSource.GetValueIndex(dimGroup:TDimGroup; Index:Integer; Cell:Integer;
             out isBreak:Boolean; var isSum: Boolean) : Integer;
var
  I,J,Temp: Integer;
  subs: Boolean;
  LastVal: Integer;
begin
  with FData do
    if dimGroup = dgRow then
      subs := FrowSubs
    else
      subs := FcolSubs;
  if FState in [dcBrowseAllData,dcBrowseMemberData] then
  begin
    if dimGroup=dgRow then
      Result:= RowLookup[Index,Cell]
    else
      Result:= ColLookup[Index,Cell];
    IsSum:= Result<0;
    if Cell=0 then
      isBreak:= True
    else begin
      if dimGroup=dgRow then
        LastVal:= RowLookup[Index,Cell-1]
      else
        LastVal:= ColLookup[Index,Cell-1];
      isBreak:= Result <> LastVal;
    end;
  end else begin
    Temp := Cell;
    for I:= 0 to Index do begin
      j:= GetExampleRepCount(dimGroup, I);
      Temp:= Temp mod j;
      if Temp = j-1 then
        if subs then begin
      	  isSum  := True;
          IsBreak:= I=Index;
	        Result := Subtotal;
          Exit;
        end;
    end;
    j := GetExampleRepCount(dimGroup, Index+1);
    if (Temp mod j) = 0 then
      isBreak := True
    else
      isBreak := False;
    Temp:= Temp div j;
    Result:= Temp;
    isSum := False;
  end;
end;


//  Find the extent of the Group (including the sum) which is within Row or Column
//  Dimension Index and at the Position Cell.


function TFxSource.GetGroupExtent(dimGroup: TDimGroup; Index: Integer; Cell: Integer): TDimRange;
var
  isBreak, isSum: Boolean;
  iMax: Integer;
begin
  Result.Last := Cell;
  Result.First := Cell;
  isBreak := False;
  {  Scan backward for the first group break which is not a sum }
  while (Result.First > 0) and (not isBreak) do
  begin
    GetValueIndex(dimGroup,Index,Result.First,isBreak,isSum);
    if (not isBreak) then
      Result.First := Result.First - 1;
  end;
  { Scan forward for the first break, then back off }
  if (dimGroup = dgRow) then
    iMax := FRowMax
  else
    iMax := FColMax;
  isBreak := False;
  while (Result.Last < iMax-1) and (not isBreak) do
  begin
    GetValueIndex(dimGroup,Index,Result.Last+1,isBreak,isSum);
    if (not isBreak) then
      Result.Last := Result.Last + 1;
  end;
end;


//  These are the functions which are calls to the data cube through the source.
//  They are not allowed if the source is not active.


function TFxSource.GetValueArray(ACol, ARow: Integer; var ValueArray: TSmallIntArray): Boolean;
var
  i: Integer;
begin
  with FData do
  begin
    ValueArray.clear;
    for i := 0 to FDims-1 do
    begin
      if (DimInfo[i].iState in [dmDrilled, dmPaged]) then
        ValueArray.Add(DimInfo[i].iValue)
      else if (DimInfo[i].iState = dmClosed) then
        ValueArray.add(subtotal)
      else if (DimInfo[i].iGroup = dgRow) then
      begin
        if (ARow < 0) then
          ValueArray.add(subtotal)
        else
          ValueArray.add(RowLookup[DimInfo[i].iActiveIndex, ARow]);
      end
      else
      begin
        if (ACol < 0) then
          ValueArray.add(subtotal)
        else
          ValueArray.add(ColLookup[DimInfo[i].iActiveIndex, ACol]);
      end;
    end;
    Result := True;
  end;
end;

procedure TFxSource.SetDecisionCube(Value: TFxCube);
begin
  if FDecisionCube<>nil then
    FDecisionCube.RemoveDataSource(Self);
  if Value<>nil then
    Value.AddDataSource(Self);
end;

procedure TFxSource.DataEvent(Event: TDecisionDataEvent);
begin
  if FBlocked then Exit;
  FBlocked := True;
  try
    if Event=xeStateChanged then
      UpdateState
    else
      NotifyDataLinks(Event);
  finally
    FBlocked := False;
  end;
end;

function TFxSource.DimDef(Ix:Integer):TFxMapItem;
begin
  Result:=nil;
  if Ready then
    Result:=DecisionCube.DataCache.Dimensions[Ix].Def;
end;

function TFxSource.SumDef(Ix:Integer):TFxMapItem;
begin
  Result:=nil;
  if Ready then
    Result:=DecisionCube.DataCache.Summaries[Ix].Def;
end;

procedure TFxSource.ToggleDim(Dim: Integer);
begin
  with Pivot[Dim]^ do begin
    if IState<>dmPaged then begin
      BeginChange;
      if IState=dmOpen then
        IState:=dmClosed
      else begin
        IState := dmOpen;
        IValue := 0;
      end;
      EndChange(xePivot);
    end;
  end;
end;

procedure TFxSource.UpdateState;
begin
  if DecisionCube<>nil then
    SetState(DecisionCube.State)
  else
    SetState(dcInactive);
end;

procedure TFxSource.SetState(Value: TCubeState);
begin
  if FState <> Value then begin
    FState:= Value;
    if FState<>dcInactive then begin
       FData.Update(DecisionCube);
       FData.RebuildList(dgRow);
       FData.RebuildList(dgCol);
       BuildLookups;
    end;
    NotifyDataLinks(xeStateChanged);
    if not (csDestroying in ComponentState)
      and Assigned(FOnStateChange)
    then
      FOnStateChange(Self);
  end;
end;

function TFxSource.GetActive: Boolean;
begin
  Result:= FState<>dcInactive;
end;

{ TDecisionDataLink }

constructor TDecisionDataLink.Create;
begin
  FBlocked := False;
end;

destructor TDecisionDataLink.Destroy;
begin
  SetDecisionSource(nil);
  inherited Destroy;
end;

procedure TDecisionDataLink.DecisionDataEvent(Event: TDecisionDataEvent);
begin
end;

procedure TDecisionDataLink.SetDecisionSource(Source: TFxSource);
begin
  if FDecisionSource<>Source then begin
    if FDecisionSource<>nil then
      FDecisionSource.RemoveDataLink(Self);
    if Source<>nil then
      Source.AddDataLink(Self);
    FDecisionSource:= Source;
    DecisionDataEvent(xeSourceChange);
  end;
end;

function TFxSource.GetExampleRepCount(dimGroup: TDimGroup; level: Integer): Integer;
const
  Elements:array [0..4] of Integer=(2,3,4,3,2);
var
  max, times: Integer;
  subs: Boolean;
begin
  with FData do begin
    Max:=FList[dimGroup].ActiveCount;
    if dimGroup=dgRow then
      subs:= FRowSubs
    else
      subs:= FColSubs;
    if level>=max then
      Result:= 1
    else begin
      Level := FList[dimGroup].IndexOfActive(Level);
      times := Elements[Level];
      Result:= GetExampleRepCount(dimGroup, level+1)*times;
      if subs then Result:=Result + 1;
    end;
  end;
end;

{ Row/Col oriented Access Functions }

procedure TFxSource.DrillDimIndex(dimGroup:TDimGroup; Index:Integer; ValueIndex: Integer; bOpen: Boolean);
var
  iDim: Integer;
  List: TDimInfoList;
begin
  with FData do begin
    List:=FList[dimGroup];
    if bOpen then Index:=List.IndexOfActive(Index);
    if Index>=0 then begin
      iDim:=0;
      while DimInfo[iDim]<>List[Index] do Inc(iDim);
      DrillValue(iDim, ValueIndex);
    end;
  end;
end;

procedure TFxSource.MoveDimIndexes(SdimGroup,DdimGroup:TDimGroup; Src,Dest:Integer; bOpen:Boolean);
var
  Item:PDimInfo;
  SList,DList:TDimInfoList;
begin
  with FData do begin
    if (SdimGroup=DdimGroup) and (Src=Dest) then Exit;
    SList:=FList[SDimGroup];
    if bOpen then Src:=SList.IndexOfActive(Src);
    DList:=FList[DDimGroup];
    if bOpen then Dest:=DList.IndexOfActive(Dest);
    BeginChange;
    Item:=SList[Src];
    SList.Delete(Src);
    Item.iGroup:=DdimGroup;
    if Dest<>-1 then
      DList.Insert(Dest,Item)
    else
      DList.Add(Item);
    EndChange(xePivot);
  end;
end;

procedure TFxSource.SwapDimIndexes(SdimGroup,DdimGroup:TDimGroup; Src,Dest:Integer; bOpen: Boolean);
var
  SList,DList:TDimInfoList;
begin
  with FData do begin
    if (SdimGroup = DdimGroup) and (Src = Dest) then Exit;
    SList:=FList[SDimGroup];
    DList:=FList[DDimGroup];
    if bOpen then begin
      Src :=SList.IndexOfActive(Src);
      Dest:=DList.IndexOfActive(Dest);
    end;
    BeginChange;
    SList[Src ].iGroup:=DDimGroup;
    SList[Src ].iIndex:=Dest;
    DList[Dest].iGroup:=SDimGroup;
    DList[Dest].iIndex:=Src;
    EndChange(xePivot);
  end;
end;

procedure TFxSource.DrillValue(iDim: Integer; ValueIndex: Integer);
begin
  Assert(ValueIndex<DecisionCube.Dimensions[iDim].Count, 'Illegal value selected');
  with FData do begin
    if DimInfo[iDim].iState=dmPaged then Exit;
    BeginChange;
    DimInfo[iDim].IValue := ValueIndex;
    DimInfo[iDim].IState := dmDrilled;
    EndChange(xePivot);
  end;
end;

{ TDimInfoArray }

constructor TDimInfoArray.Create(ACount: Integer);
var
  I:Integer;
begin
  inherited Create;
  SetLength(FElements,ACount);
  for I:=0 to High(FElements)do
    FElements[I].Dim:=I;
end;

destructor TDimInfoArray.Destroy;
begin
  FElements:=nil;
  inherited;
end;

procedure TDimInfoArray.Assign(Value: TDimInfoArray);
begin
  FElements:=Value.FElements;
end;

{
function TDimInfoArray.isEqual(Value: TDimInfoArray): Boolean;
var
  I:Integer;
begin
  Result := False;
  if Count<>Value.Count then Exit;
  for I:=0 to Count-1 do begin
    with Value.FElements[I] do begin
      if FElements[I].Name<>Name then Exit;
      if FElements[I].iState<>iState then Exit;
      if FElements[I].iValue<>iValue then Exit;
      if FElements[I].iIndex<>iIndex then Exit;
      if FElements[I].iActiveIndex<>iActiveIndex then Exit;
      if FElements[I].iGroup<>iGroup then Exit;
    end;
  end;
  Result := True;
end;
}

function TDimInfoArray.GetGroupSize(Group: TDimGroup; bOpen: Boolean): Integer;
var
  i: Integer;
begin
  Result:=0;
  for i:=0 to Count-1 do begin
    if items[i].iGroup=Group then begin
      if not bOpen or (items[i].iState=dmOpen) then
        Result := Result + 1;
    end;
  end;
end;

function TDimInfoArray.GetItem(Index: Integer): PDimInfo;
begin
  if (Index < 0) or (Index > High(FElements)) then
    raise EArrayError.Create(SOutOfbounds)
  else
    Result:= @FElements[Index];
end;

function TDimInfoArray.Find(const Name:string; out pos:Integer): Boolean;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
  begin
    if FElements[i].Name = Name then
    begin
      pos := i;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TDimInfoArray.GetCount: Integer;
begin
  Result:=Length(FElements);
end;

{ TPivotState }

constructor TPivotState.Create;
begin
  DimInfo := TDimInfoArray.Create(0);
  FList[dgCol]:= TDimInfoList.Create;
  FList[dgRow]:= TDimInfoList.Create;
  FSums := 0;
  FDims := 0;
  FCurrentSum := 0;
  FRowSparse := False;
  FRowSubs := True;
  FColSparse := False;
  FColSubs := True;
end;

destructor TPivotState.Destroy;
begin
  FreeAndNil(FList[dgCol]);
  FreeAndNil(FList[dgRow]);
  FreeAndNil(DimInfo);
  inherited;
end;

{
procedure TPivotState.Assign(Value: TPivotState);
begin
  FDims := Value.FDims;
  FSums := Value.FSums;
  FCurrentSum := Value.FCurrentSum;
  FRowSparse := Value.FRowSparse;
  FColSparse := Value.FColSparse;
  FRowSubs := Value.FRowSubs;
  FColSubs := Value.FColSubs;
  DimInfo.Assign(Value.DimInfo);
end;

function TPivotState.IsEqual(Value: TPivotState): Boolean;
begin
  Result := False;
  if (FDims <> Value.FDims) then Exit;
  if (FSums <> Value.FSums) then Exit;
  if (FCurrentSum <> Value.FCurrentSum) then Exit;
  if (FRowSparse <> Value.FRowSparse) then Exit;
  if (FColSparse <> Value.FColSparse) then Exit;
  if (FRowSubs <> Value.FRowSubs) then Exit;
  if (FColSubs <> Value.FColSubs) then Exit;
  Result := DimInfo.isEqual(Value.DimInfo);
end;
}

function TPivotState.GetItems(Index: Integer): PDimInfo;
begin
  Result:=DimInfo[Index];
end;

function TPivotState.FindByState(const AGroup:TDimGroup; const AStates:TDimStates):PDimInfo;
var
  I:Integer;
  List:TDimInfoList;
begin
  Result:=nil;
  List:=FList[AGroup];
  for I:=0 to List.Count-1 do
    if List[I].iState in AStates then begin
      Result:=List[I];
      Break;
    end;
end;

procedure TPivotState.Update(ACube: TFxCustomStore);
var
  I,Ix, OldI: Integer;
  DM: TFxMapItem;
  OldArray: TDimInfoArray;
  HasActiveCol:Boolean;
  NewState:TDimState;
begin
  FDims:= ACube.Dimensions.Count;
  FSums:= ACube.Summaries.Count;
  FList[dgRow].Clear;
  FList[dgCol].Clear;
  FList[dgRow].Count:=FDims;
  FList[dgCol].Count:=FDims;
  if FCurrentSum >= ACube.Summaries.Count then
    FCurrentSum:= 0;
  // set up the diminfo and row and column arrays
  HasActiveCol:=False;
  oldArray:= DimInfo;
  DimInfo := TDimInfoArray.Create(FDims);
  for I:=0 to DimInfo.Count-1 do begin
    with DimInfo[I]^ do begin
      iGroup:= dgNone;
      iState:= dmNone;
      iValue:= -1;
      DM:= ACube.Dimensions[I].Def;
      Name:=DM.Name;
      if Assigned(OldArray) and OldArray.Find(Name,oldI)then begin
        iGroup:=oldArray[oldI]^.iGroup;
        iState:=oldArray[oldI]^.iState;
        iValue:=oldArray[oldI]^.iValue;
        iIndex:=oldArray[oldI]^.iIndex;
        FList[iGroup][iIndex]:=DimInfo[I];
        HasActiveCol:= HasActiveCol or ((iGroup=dgCol)and(iState=dmOpen));
      end;
    end;
  end;
  for I:=0 to DimInfo.Count-1 do begin
    if DimInfo[I].iGroup=dgNone then begin
      if HasActiveCol then begin
        Ix:=0;
        while FList[dgRow][Ix]<>nil do Inc(Ix);
        DimInfo[I].iGroup:=dgRow;
        FList[dgRow][Ix]:=DimInfo[I]
      end else begin
        Ix:=0;
        while FList[dgCol][Ix]<>nil do Inc(Ix);
        DimInfo[I].iGroup:=dgCol;
        DimInfo[I].iState:=dmOpen;
        FList[dgCol][Ix]:=DimInfo[I];
        HasActiveCol:=True;
      end;
    end;
  end;
  FList[dgRow].Pack;
  FList[dgCol].Pack;
  NewState:=dmOpen;
  for I:=0 to FList[dgRow].Count-1 do begin
    with FList[dgRow][I]^ do
      if iState=dmOpen then
        NewState:=dmClosed
      else if iState=dmNone then begin
        iState:=NewState;
        NewState:=dmClosed;
      end;
  end;
  for I:=0 to FList[dgRow].Count-1 do
    FList[dgRow][I].iIndex:=I;
  for I:=0 to FList[dgCol].Count-1 do
    FList[dgCol][I].iIndex:=I;
  FreeAndNil(OldArray);
  FRowSubs:= True;
  FColSubs:= True;
end;

procedure TPivotState.RebuildList(const AGroup: TDimGroup);
var
  I:Integer;
begin
  with FList[AGroup]do begin
    Clear;
    Count:=Length(DimInfo.FElements);
    for I:=0 to High(DimInfo.FElements)do begin
      with DimInfo[I]^do
        if iGroup=AGroup then
          Items[iIndex]:=DimInfo[I];
    end;
    Pack;
    for I:=0 to Count-1 do
      Items[I]^.iIndex:=I;
    FActiveCount:=0;
    for I:=0 to Count-1 do
      with Items[I]^do
        if iState<>dmOpen then
          iActiveIndex:=-1
        else begin
          iActiveIndex:=FActiveCount;
          Inc(FActiveCount);
        end;
  end;
end;

function TPivotState.GetActiveColCount: Integer;
begin
  Result:=FList[dgCol].ActiveCount;
end;

function TPivotState.GetActiveRowCount: Integer;
begin
  Result:=FList[dgRow].ActiveCount;
end;

function TPivotState.GetColCount: Integer;
begin
  Result:=FList[dgCol].Count;
end;

function TPivotState.GetRowCount: Integer;
begin
  Result:=FList[dgRow].Count;
end;

function TPivotState.IndexInfo(AGroup:TDimGroup; Index:Integer; Active:Boolean):PDimInfo;
var
  List:TDimInfoList;
begin
  List:=FList[AGroup];
  if Active then Index:=List.IndexOfActive(Index);
  Result:=List[Index];
end;

procedure TPivotState.Assign(Source: TPersistent);
begin
  if Source is TPivotState then begin
    FDims:= TPivotState(Source).FDims;
    FSums:= TPivotState(Source).FSums;
    if not assigned(DimInfo) then
      DimInfo := TDimInfoArray.create(FDims);
    DimInfo.Assign(TPivotState(Source).DimInfo);
    FCurrentSum:= TPivotState(Source).FCurrentSum;
    FRowSparse := TPivotState(Source).FRowSparse;
    FColSparse := TPivotState(Source).FColSparse;
    FRowSubs   := TPivotState(Source).FRowSubs;
    FColSubs   := TPivotState(Source).FColSubs;
  end else
   inherited;
end;

{ TDimInfoList }

function TDimInfoList.Add(Item: PDimInfo): Integer;
begin
  Result:=inherited Add(Item);
  Items[Result].iIndex:=Result;
end;

procedure TDimInfoList.Delete(Index: Integer);
begin
  inherited Delete(Index);
  while Index<Count do begin
    Items[Index].iIndex:=Index;
    Inc(Index);
  end;
end;

function TDimInfoList.GetItems(const Index: Integer): PDimInfo;
begin
  Result:=inherited Items[Index];
end;

function TDimInfoList.IndexOfActive(const Index: Integer): Integer;
var
  I:Integer;
begin
  I:=0;
  for Result:=0 to Count-1 do
    if Items[Result].iState=dmOpen then begin
      if I=Index then Exit;
      Inc(I);
    end;
  Result:=-1;
end;

procedure TDimInfoList.Insert(Index: Integer; Item: PDimInfo);
begin
  inherited Insert(Index,Item);
  while Index<Count do begin
    Items[Index]^.iIndex:=Index;
    Inc(Index);
  end;
end;

procedure TDimInfoList.SetItems(const Index:Integer;  const Value:PDimInfo);
begin
  inherited Items[Index]:=Value;
end;

initialization
  StartClassGroup(TControl);
  GroupDescendentsWith(TFxCube, TControl);
  GroupDescendentsWith(TFxSource, TControl);
end.

