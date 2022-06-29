{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{                                                       }
{       Copyright (c) 1997,99 Inprise Corporation       }
{                                                       }
{*******************************************************}

unit FxGrid;

interface

uses
  Windows, SysUtils, Classes, Dialogs, Controls, StdCtrls, Graphics, DB,
  Menus, Grids, Forms, FxArrays, FxConsts, FxDB, FxButton, FxCommon, ImgList;

type
  TValueArray = class(TSmallIntArray);
  TDecisionGridOption = (cgGridLines, cgOutliner, cgPivotable);

  TDecisionGridOptions = set of TDecisionGridOption;

  TDecisionGridState = (csNormal, csPivoting);
  TDecisionDrawStates = (dsGroupStart, dsRowCaption, dsColCaption, dsSum,
                         dsRowValue, dsColValue, dsData, dsOpenAfter, dsCloseAfter,
                         dsCloseBefore, dsOpenBefore, dsRowIndicator, dsColIndicator,
                         dsRowPlus, dsColPlus, dsNone);
  TDecisionPivotState = (psNone, psLeftPivot, psMiddlePivot, psRightPivot, psTopPivot);
  TDecisionDrawState = set of TDecisionDrawStates;

  TDecisionDrawCellEvent = procedure (Sender: TObject; Col, Row: Longint;
                                      var Value: string; var aFont: TFont;
                                      var aColor: TColor; AState: TGridDrawState;
                                      aDrawState: TDecisionDrawState) of Object;

  TDecisionExamineCellEvent = procedure (Sender: TObject; iCol, iRow: Longint;
                                         iSum: Integer; const ValueArray: TValueArray) of Object;

  TDecisionCellType = (ctNone, ctCaptionRow, ctCaptionCol, ctNewCaptionCol,
                       ctRowLabel, ctColLabel, ctData, ctRowPlus, ctColPlus,
                       ctRowIndicator, ctColIndicator);

  TButtonPlace = (bpLeft, bpMiddle, bpRight);

  TCustomFxGrid = class;

  TFxGridDataLink = class(TDecisionDataLink)
  private
    FGrid: TCustomFxGrid;
  protected
    procedure DecisionDataEvent(Event: TDecisionDataEvent); override;
  public
    constructor Create(AGrid: TCustomFxGrid);
    destructor Destroy; override;
  end;

  TDDNotifyType = (tdDisplay, tdSubTotals, tdMetaData);

  TDisplayDim = class(TCollectionItem)
  private
    FName: String;
    FFieldName: String;
    FFormat: String;
    FAlignment: TAlignment;
    FColor: TColor;
    FSubs: Boolean;
    FOwner: TCollection;
    FWidth: Integer;
    procedure SetName(Value: string);
    procedure SetFieldName(Value: string);
    procedure SetFormat(Value: String);
    procedure SetAlignment(Value: TAlignment);
    procedure SetColor(Value: TColor);
    procedure SetSubs(Value: Boolean);
    procedure SetWidth(Value: Integer);
    procedure NotifyCollection(aType: TDDNotifyType);
  protected
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Value: TPersistent); override;
  published
    property DisplayName: string read FName write SetName;
    property FieldName: string read FFieldName write SetFieldName;
    property Color: TColor read FColor write SetColor;
    property Format: String read FFormat write SetFormat;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property Subtotals: Boolean read FSubs write SetSubs;
    property Width:Integer read FWidth write SetWidth default 20;
  end;

  TDisplayDimClass = class of TDisplayDim;

  TDisplayDims = class(TCollection)
  private
    bQuiet: boolean;
    function GetDisplayDim(Index: Integer): TDisplayDim;
    procedure SetDisplayDim(Index: Integer; Value: TDisplayDim);
    constructor Create(Grid: TCustomFxGrid; ItemClass: TDisplayDimClass);
  protected
    FGrid: TCustomFxGrid;
    function GetOwner: TPersistent; override;
    procedure NotifyOwner(aType: TDDNotifyType);
  public
    property Items[Index: Integer]:TDisplayDim read GetDisplayDim write SetDisplayDim; default;
  end;

  TCustomFxGrid = class(TCustomGrid)
  private
    FActiveGrid: boolean;
    FMenu: TQuickMenu;
    FDataLink: TFxGridDataLink;
    FDisplayDims: TDisplayDims;
    FOptions: TDecisionGridOptions;
    FIndicators: TImageList;
    FSourceCell: TGridCoord;               { grid coords of cell user began dragging or pivoting }
    FTargetCell: TGridCoord;               { grid coords of target cell (is updated each mouse move }
    FTargetSwitch: Boolean;
    FCaptionRow: Byte;                    { = 0,1,2 caption row active, and are there any inactive categories }
    FCaptionCol: Byte;                    { = 0,1,2 caption row active, and are there any inactive categories }
    FChanging: Boolean;
    FRowOffset: Byte;
    fColOffset: Byte;
    FActRows: Integer;
    FActCols: Integer;
    FTotRows: Integer;
    FTotCols: Integer;
    FColWidth: Integer;
    FRowHeight: Integer;
    FLabelFont: TFont;
    FLabelColor: TColor;
    FLabelSumColor: TColor;
    FCaptionColor: TColor;
    FCaptionFont: TFont;
    FDataFont: TFont;
    FDataColor: TColor;
    FDataSumColor: TColor;
    FGridLineColor: TColor;
    FGridLineWidth: Integer;
    FShowCubeEditor:boolean;
    FOnDecisionExamineCell: TDecisionExamineCellEvent;
    FOnDecisionDrawCell: TDecisionDrawCellEvent;
    FOnTopLeftChanged: TNotifyEvent;
    FLabelSumFont: TFont;
    procedure InvalidateTargetCell;
    function GetHorzButtonPlace(X,Y: Integer): TButtonPlace;
    procedure PerformPivot;
    procedure RawToDataCoord(var X,Y: LongInt);
    procedure DataToRawCoord(var X,Y: LongInt);
    function ConvertWidth(Value:Integer):Integer;
    function DataToRawX(X: LongInt): LongInt;
    function DataToRawY(Y: LongInt): LongInt;
    procedure DrawSpecialState(ACanvas: TCanvas; ARect: TRect;
                               DrawState: TDecisionDrawState; PivotState: TDecisionPivotState);
    function GetSpecialState(ARow,AColumn: Integer): TDecisionPivotState;
    function GetDataPoint(ARow,AColumn: LongInt;  out State:TDecisionDrawState; out Alignment:TAlignment): String;
    function WhichCoord( Coord: TGridCoord ): TDecisionCellType;
    function WhichCoordExCap( Coord: TGridCoord ): TDecisionCellType;
    function GetDimensionIndex(cellType: TDecisionCellType; Coord: TGridCoord;
                               var dimGroup: TDimGroup;var bExists: Boolean): Integer;
    function  MouseToDataCoord( X,Y: Integer): TGridCoord;
    procedure SetNearestTargetCell(X,Y: Integer);
    procedure GetHitTypes(ARow,AColumn: Integer; var ValueIndex: Integer;
                          out DrawState: TDecisionDrawState; out CellType: TDecisionCellType);
    procedure NewDataStructure;
    procedure NewGridLayout;
    function GetData(ARow, AColumn: Integer; var SubLevel: Integer): string;
    function GetCaption(dimGroup: TDimGroup; Index: Integer): string;
    function GetLabel(dimGroup: TDimGroup; Index: Integer; ValueIndex: Integer): string;
    function GeTDecisionSource: TFxSource;
    procedure SetDecisionSource(Value: TFxSource);
    procedure SetColWidth(Value: Integer);
    procedure SetRowHeight(Value: Integer);
    procedure SetGridLineWidth(Value: Integer);
    procedure SetGridLineColor(Value: TColor);
    procedure SetLabelFont(Value: TFont);
    procedure SetLabelColor(Value: TColor);
    procedure SetLabelSumColor(Value: TColor);
    procedure SetCaptionFont(Value: TFont);
    procedure SetCaptionColor(Value: TColor);
    procedure SetDataFont(Value: TFont);
    procedure SetDataColor(Value: TColor);
    procedure SetDataSumColor(Value: TColor);
    procedure FontChanged(Sender: TObject);
    procedure SetOptions(Value: TDecisionGridOptions);
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure RightMouse(Sender: TObject);
    procedure SelectDimOptions(Sender: TObject);
    procedure SelectGridOptions(Sender: TObject);
    procedure InitializeGridCells;
    procedure SetTotals(Value: boolean);
    function GetTotals: boolean;
    function GetFixedRows: integer;
    function GetFixedCols: integer;
    function GetRowCount: integer;
    function GetColCount: integer;
    property ColWidth: Integer read FColWidth write SetColWidth;
    property RowHeight: Integer read FRowHeight write SetRowHeight;
    procedure SetLabelSumFont(const Value: TFont);
  protected
    FGridStateX: TDecisionGridState;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    function SelectCell(Arow, AColumn:LongInt): Boolean; override;
    procedure TopLeftChanged; override;
    procedure ColWidthsChanged; override;
    procedure RowHeightsChanged; override;
    procedure Paint; override;
    property Options: TDecisionGridOptions read FOptions write SetOptions default [cgGridLines, cgOutLiner, cgPivotable];
    property GridLineWidth: Integer read FGridLineWidth write SetGridLineWidth;
    property GridLineColor: TColor read FGridLineColor write SetGridLineColor;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property CaptionColor: TColor read FCaptionColor write SetCaptionColor;
    property DataFont: TFont read FDataFont write SetDataFont;
    property DataColor: TColor read FDataColor write SetDataColor;
    property DataSumColor: TColor read FDataSumColor write SetDataSumColor;
    property LabelFont: TFont read FLabelFont write SetLabelFont;
    property LabelColor: TColor read FLabelColor write SetLabelColor;
    property LabelSumColor: TColor read FLabelSumColor write SetLabelSumColor;
    property LabelSumFont : TFont read FLabelSumFont write SetLabelSumFont;
    property Dimensions: TDisplayDims read FDisplayDims write FDisplayDims;
    function GetCells(ACol, ARow: Integer): String;
    property FixedRows:integer read GetFixedRows;
    property RowCount:integer read GetRowCount;
    property FixedCols:integer read GetFixedCols;
    property ColCount:integer read GetColCount;
    property DefaultColWidth: Integer read FColWidth write SetColWidth;
    property DefaultRowHeight: Integer read FRowHeight write SetRowHeight;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property DecisionSource: TFxSource read GeTDecisionSource write SetDecisionSource;
    function CellRect(ACol, ARow: Longint): TRect;
    function CellValueArray(ACol, ARow: Integer; var ValueArray: TValueArray): boolean;
    function CellDrawState(ACol, ARow: Integer; var Value: string; var DrawState: TDecisionDrawState): boolean;
    property Totals: boolean read GetTotals write SetTotals;
    property OnDecisionExamineCell: TDecisionExamineCellEvent read FOnDecisionExamineCell write FOnDecisionExamineCell;
    property OnDecisionDrawCell: TDecisionDrawCellEvent read FOnDecisionDrawCell write FOnDecisionDrawCell;
    property OnTopLeftChanged: TNotifyEvent read FOnTopLeftChanged write FOnTopLeftChanged;
    property Cells[ACol, ARow: Integer]: String read GetCells;
    property ShowCubeEditor:boolean read FShowCubeEditor write FShowCubeEditor;
  end;

  TFxGrid = class(TCustomFxGrid)
  public
    property RowCount;
    property ColCount;
    property FixedRows;
    property FixedCols;
  published
    property Options;
    property DefaultColWidth;
    property DefaultRowHeight;
    property CaptionColor;
    property CaptionFont;
    property DataColor;
    property DataSumColor;
    property DataFont;
    property LabelFont;
    property LabelColor;
    property LabelSumColor;
    property LabelSumFont;
    property DecisionSource;
    property Dimensions;
    property Totals;
    property ShowCubeEditor;
    { Inherited properties and events }
    property Align;
    property Anchors;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DefaultDrawing;
    property DragCursor;
    property DragMode;
    property Enabled;
    property GridLineWidth;
    property GridLineColor;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDecisionDrawCell;
    property OnDecisionExamineCell;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnTopLeftChanged;
  end;

implementation

uses Math;

type
  BitmapId = (biLeftArrow, biRightArrow, biCaption, biPivot, biDOpen, biDClose);

  TDecisionCoord = Record
    XY: TGridCoord;
    CellType: TDecisionCellType;
  end;

const
  PlusWidth = 16;
  NoSpace = 0;
  SubTotal = -1;
  BitmapArray: Array[BitmapId] of String = ('LeftArrow', 'RightArrow', 'Caption', 'Pivot', 'DOpen', 'DClose');

var
  DrawBitmap: TBitmap;
  UserCount: Integer;

procedure UsesBitmap;
begin
  if (UserCount = 0) then DrawBitmap := TBitmap.Create;
  Inc(UserCount);
end;

procedure ReleaseBitmap;
begin
  Dec(UserCount);
  if (UserCount = 0) then DrawBitmap.Free;
end;

procedure WriteText(ACanvas: TCanvas; ARect: TRect; DX, DY: Integer;
  const Text: string; Alignment: TAlignment);
const
  AlignFlags : array [TAlignment] of Integer =
    (DT_LEFT or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX,
     DT_RIGHT or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX,
     DT_CENTER or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX);
var
  B, R: TRect;
  Left: Integer;
  I: TColorRef;
begin
  I := ColorToRGB(ACanvas.Brush.Color);
  if (GetNearestColor(ACanvas.Handle, I) = I) then
  begin                       { Use ExtTextOut for solid colors }
    case Alignment of
      taLeftJustify  : Left := ARect.Left + DX;
      taRightJustify : Left := ARect.Right - ACanvas.TextWidth(Text) - 3;
    else { taCenter }
      Left := ARect.Left + (ARect.Right - ARect.Left) shr 1
              - (ACanvas.TextWidth(Text) shr 1);
    end;
    ExtTextOut(ACanvas.Handle, Left, ARect.Top + DY, ETO_OPAQUE or
      ETO_CLIPPED, @ARect, PChar(Text), Length(Text), nil);
  end
  else begin                  { Use FillRect and Drawtext for dithered colors }
    with DrawBitmap, ARect do { Use offscreen bitmap to eliminate flicker and }
    begin                     { brush origin tics in painting / scrolling.    }
      Width := Max(Width, Right - Left);
      Height := Max(Height, Bottom - Top);
      R := Rect(DX, DY, Right - Left - 1, Bottom - Top - 1);
      B := Rect(0, 0, Right - Left, Bottom - Top);
    end;
    with DrawBitmap.Canvas do
    begin
      Font := ACanvas.Font;
      Font.Color := ACanvas.Font.Color;
      Brush := ACanvas.Brush;
      Brush.Style := bsSolid;
      FillRect(B);
      SetBkMode(Handle, TRANSPARENT);
      DrawText(Handle, PChar(Text), Length(Text), R, AlignFlags[Alignment]);
    end;
    ACanvas.CopyRect(ARect, DrawBitmap.Canvas, B);
  end;
end;

constructor TCustomFxGrid.Create(AOwner: TComponent);
var
  BmpId: BitmapId;
  Bmp: TBitmap;
begin
  inherited Create(AOwner);
  inherited BorderStyle := bsSingle;
  inherited Options := [goDrawFocusSelected,goTabs, goRangeSelect,
                        goColSizing, goRowSizing];
  inherited DefaultDrawing := True;
  ScrollBars := ssBoth;
  FOptions := [cgGridLines, cgOutliner, cgPivotable];
  FGridStateX := csNormal;
  FDataLink := TFxGridDataLink.Create(Self);
  Color := clBtnFace;
  ParentColor:= False;
  FLabelFont := TFont.Create;
  FLabelFont.OnChange := FontChanged;
  FLabelColor := clBtnFace;
  FLabelSumColor := clInactiveCaption;
  FLabelSumFont:=TFont.Create;
  FLabelSumFont.OnChange:=FontChanged;
  FCaptionFont := TFont.Create;
  FCaptionColor := clActiveCaption;
  FCaptionFont.Color := clCaptionText;
  FCaptionFont.OnChange := FontChanged;
  FDataFont := TFont.Create;
  FDataFont.OnChange := FontChanged;
  FDataColor := clInfoBk;
  FDataSumColor := clNone;
  SetColWidth(100);			{ default column width }
  SetRowHeight(20);
  FGridLineColor := clWindowText;
  FGridLineWidth := 1;
  FShowCubeEditor := false;
  FSaveCellExtents:= False;
  FDisplayDims := TDisplayDims.Create(self, TDisplayDim);
  FChanging := False;
  NewDataStructure;					{ to initialize the dimension information }
  HideEditor;
  FMenu := TQuickMenu.Create(self);
  FActiveGrid := false;
  inherited FixedCols := 0;
  inherited FixedRows := 0;
  inherited RowCount := 1;
  inherited ColCount := 1;
  { Initialize Decision data structure }
  Bmp := TBitmap.Create;
  try
    FIndicators := TImageList.CreateSize(15, 15);
    for BmpId := Low(BitmapId) to High(BitmapId) do
    begin
      Bmp.Handle := LoadBitmap(HInstance, PChar(BitmapArray[BmpId]));
      FIndicators.AddMasked(Bmp, clMaroon);
    end;
  finally
    Bmp.Free;
  end;
  UsesBitmap;
  RCS;
end;

destructor TCustomFxGrid.Destroy;
begin
  FIndicators.Free;
  FIndicators := nil;
  FDataLink.Free;
  FDataLink := nil;
  FLabelFont.Free;
  FLabelFont := nil;
  FLabelSumFont.Free;
  FLabelSumFont:=nil;
  FCaptionFont.Free;
  FCaptionFont := nil;
  FDataFont.Free;
  FDataFont := nil;
  FDisplayDims.Clear;
  FDisplayDims.Free;
  FMenu.Free;
  FMenu := nil;
  inherited Destroy;
  ReleaseBitmap;
end;

procedure TCustomFxGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent is TPivotButton) and (Operation = opInsert) then
  begin
    if assigned(DecisionSource) then
      TPivotButton(AComponent).DecisionSource := DecisionSource;
  end;
end;

// Translate the raw grid X,Y coordinate into cell positions in the cross tab

procedure TCustomFxGrid.RawToDataCoord(var X,Y: LongInt);
begin
  Dec(X, fColOffset);
  Dec(X, FCaptionCol);
  Dec(Y, FRowOffset);
  Dec(Y, FCaptionRow);
end;

{Translate the cell coordinates in the Decision to raw positions in the grid}
procedure TCustomFxGrid.DataToRawCoord(var X,Y: LongInt);
begin
  Inc(X, fColOffset);
  Inc(X, FCaptionCol);
  Inc(Y, FRowOffset);
  Inc(Y, FCaptionRow);
end;

function TCustomFxGrid.DataToRawX(X: LongInt): LongInt;
begin
  Result := X + FColOffset + FCaptionCol;
end;

function TCustomFxGrid.DataToRawY(Y: LongInt): LongInt;
begin
  Result := Y + FRowOffset + FCaptionRow;
end;

{
  These routines should be called only when the number of dimensions
  could have changed (i.e., when the data cube has been attached or
  detached, on initialization, or when the data cube goes active).
  By convention, the data cube is attached or detached at those times.
}
procedure TCustomFxGrid.NewDataStructure;
var
  i: Integer;
  X: TDisplayDim;
  bCreating: boolean;
begin
  if assigned(DecisionSource) and DecisionSource.Ready then
    with DecisionSource do
    begin
      FDisplayDims.bQuiet := true;
      {
        For now, do a simple test to see if the number of dimensions has changed.
        if not, we can assume that the datasource is the some as before and use
        the old FDisplayDims.  Eventually, some code which is resistant to
        datasource restructures would be good.
      }
      bCreating := not (FDisplayDims.count=(nDims+nSums));
      if bCreating then FDisplayDims.Clear;
      for i:=0 to nDims-1 do begin
        if bCreating then FDisplayDims.Add;
        X := FDisplayDims[i];
        if X.FieldName<>DecisionSource.GetDimensionName(i) then
          X.FFieldName := DecisionSource.GetDimensionName(i);
        if DecisionSource.DimDef(I)<>nil then begin
          X.FAlignment:=DecisionSource.DimDef(I).Alignment;
          X.FWidth:=ConvertWidth(DecisionSource.DimDef(I).Width);
        end;
      end;
      for i:=0 to nSums-1 do begin
        if bCreating then FDisplayDims.Add;
        X := FDisplayDims[i + nDims];
        if X.FieldName<>DecisionSource.GetSummaryName(i) then
          X.FFieldName := DecisionSource.GetSummaryName(i);
        if DecisionSource.SumDef(I)<>nil then begin
          X.FAlignment:=DecisionSource.SumDef(I).Alignment;
          X.FWidth:=ConvertWidth(DecisionSource.SumDef(I).Width);
        end;
      end;
      FDisplayDims.bQuiet := false;
    end;
  NewGridLayout;
end;

  { NewGridLayout:  assumes that the FDecisionData has been set up }

procedure TCustomFxGrid.NewGridLayout;
var
  i: Integer;
begin
  if Assigned(DecisionSource) and DecisionSource.Ready then begin
    with DecisionSource do begin
      FRowOffset:= Pivot.ActiveColCount;
      fColOffset:= Pivot.ActiveRowCount;
      FActRows  := Pivot.ActiveRowCount;
      FActCols  := Pivot.ActiveColCount;
      FTotRows  := Pivot.RowCount;
      FTotCols  := Pivot.ColCount;
      FCaptionCol := 0;
      if ((FActCols = 0) and (FTotCols > 0)) then
        FCaptionRow := 2
      else
        FCaptionRow := 1;
      if cgOutliner in Options then
        FCaptionCol := FCaptionCol + 1;
      inherited FixedCols := 0;
      inherited FixedRows := 0;
      inherited RowCount := FRowOffset + FCaptionRow + DecisionSource.nDataRows;
      inherited ColCount := fColOffset + FCaptionCol + DecisionSource.nDataCols;
      if DecisionSource.nDataRows > 0 then
        inherited FixedCols := fColOffset+FCaptionCol;
      if DecisionSource.nDataCols > 0 then
        inherited FixedRows := FRowOffset + FCaptionRow;
      FActiveGrid := true;
      InitializeGridCells;
    end;{with}
  end else begin
    if FActiveGrid then begin
      FActiveGrid := false;
      inherited FixedCols := 0;
      inherited FixedRows := 0;
      inherited RowCount := 1;
      inherited ColCount := 1;
    end;
    FRowOffset := 0;
    fColOffset := 0;
    FActRows := 0;
    FActCols := 0;
    FTotRows := 0;
    FTotCols := 0;
    FCaptionCol := 0;
    FCaptionRow := 0;
  end;{if}
  { Call any buttons we contain to tell them it is time to initialize }
  for i := 0 to ControlCount-1 do begin
    if Controls[i] is TPivotButton then
      TPivotButton(Controls[i]).NewState;
  end;
end;

procedure TCustomFxGrid.InitializeGridCells;
var
  i,j: Integer;
  isBreak, isSum: boolean;
  iDim: Integer;
  W:Word;
begin
  if Assigned(DecisionSource) and Assigned(FDisplayDims) then
  with DecisionSource do
  begin
    FChanging := True;
    if cgOutliner in Options then colWidths[0] := PlusWidth;
    for i := FCaptionCol to ColCount-1 do
      colWidths[i] := colWidth;
    for i := FCaptionRow to RowCount-1 do
      rowHeights[i] := rowHeight;
    for i := 0 to FActRows-1 do begin
      iDim:=Pivot.IndexInfo(dgRow,I,True).Dim;
      colWidths[FCaptionCol+i]:=Dimensions[iDim].Width;
      if not FDisplayDims[iDim].FSubs then begin
        for j := 0 to RowCount-FixedRows-1 do begin
          DecisionSource.GetValueIndex(dgRow,i,j,isBreak,isSum);
          if isBreak and isSum then RowHeights[FixedRows + j] := NoSpace;
        end;
      end;
    end;{for}
    W:=colWidth;
    for i:=0 to FActCols-1 do begin
      iDim:=Pivot.IndexInfo(dgCol,I,True).Dim;
      W:=Max(W,FDisplayDims[iDim].Width);
      if (not FDisplayDims[iDim].FSubs) then begin
        for j := 0 to ColCount-FixedCols-1 do begin
          DecisionSource.GetValueIndex(dgCol,i,j,isBreak,isSum);
          if isBreak and isSum then ColWidths[FixedCols + j] := NoSpace;
        end;
      end;
    end;
    for I:=FCaptionCol+FColOffset to ColCount-1 do
      ColWidths[I]:=W;
    FChanging := False;
  end;
end;

{
  These are internal routines to service external hooks.
  These are used to drive the grid through an external pivot,
  and are not used any more.
}

procedure TCustomFxGrid.GetHitTypes(ARow,AColumn: Integer;
                                          var ValueIndex: Integer;
                                          out DrawState: TDecisionDrawState;
                                          out CellType: TDecisionCellType);
var
  IDim: Integer;
  isSum, isBreak: Boolean;
  aRowState: TRowState;
  Coord: TGridCoord;
  iRange: TDimRange;
  i, rawRow, rawCol: Integer;
  Info:PDimInfo;
begin
  DrawState:= [];
  Coord.X:= AColumn;
  Coord.Y:= ARow;
  CellType := WhichCoord(Coord);
  if not Assigned(DecisionSource) then Exit;
  with DecisionSource do
    case CellType of
      ctData:
        DrawState := [dsData];
      ctCaptionCol:
      begin
        DrawState := [dsColCaption];
        if cgOutliner in Options then begin
          iDim := Pivot.IndexInfo(dgCol,AColumn,true).Dim;
          aRowState := GetRowState(iDim);
          if rcNextClosed in aRowState then
            DrawState := DrawState + [dsOpenAfter]
          else if rcNextOpen in aRowState then
            DrawState := DrawState + [dsCloseAfter];
        end;
      end;
      ctCaptionRow:
      begin
        DrawState:= [dsRowCaption];
        if cgOutliner in Options then begin
          iDim:= Pivot.IndexInfo(dgRow,AColumn+FColOffset,True).Dim;
          aRowState := GetRowState(iDim);
          if rcNextClosed in aRowState then
            DrawState := DrawState + [dsOpenAfter]
          else if rcNextOpen in aRowState then
            DrawState := DrawState + [dsCloseAfter];
        end;
      end;
      ctColLabel:
      begin
        DrawState := [dsColValue];
        ValueIndex := DecisionSource.GetValueIndex(dgCol,ARow+FRowOffset,AColumn,isBreak,isSum);
        if isSum then
          DrawState:=DrawState + [dsSum];
        if isBreak then
          DrawState:= DrawState + [dsGroupStart]
        else begin  { not on a break, then see }
          rawCol:= DataToRawX(AColumn);
          if (rawCol > 0) and (colWidths[rawCol-1] = NoSpace) then
          begin
            iRange:=  DecisionSource.GetGroupExtent(dgCol, ARow+FRowOffset, AColumn);
            iRange.First := DataToRawX(iRange.First);
            DrawState := DrawState + [dsGroupStart];
            for i := iRange.First to rawCol-1 do
              if (colWidths[i] > NoSpace) then
                DrawState := DrawState - [dsGroupStart];
          end;
        end;
      end;
      ctRowLabel:
      begin
        DrawState := [dsRowValue];
        ValueIndex := DecisionSource.GetValueIndex(dgRow,AColumn + fColOffset,ARow,isBreak,isSum);
        if isSum then
          DrawState := DrawState + [dsSum];
        if isBreak then
          DrawState := DrawState + [dsGroupStart]
        else begin
          rawRow := DataToRawY(ARow);
          if (rawRow > 0) and (rowHeights[rawRow-1] = NoSpace) then
          begin
            iRange := DecisionSOurce.GetGroupExtent(dgRow, AColumn+FColOffset, ARow);
            iRange.First := DataToRawY(iRange.First);
            DrawState := DrawState + [dsGroupStart];
            for i := iRange.First to rawRow-1 do
              if (rowHeights[i] > NoSpace) then
                DrawState := DrawState - [dsGroupStart];
          end;
        end;
      end;
      ctRowIndicator: DrawState := [dsRowIndicator];
      ctColIndicator: DrawState := [dsColIndicator];
      ctRowPlus:
      begin
        DrawState := [dsRowPlus];
        Info:=DecisionSource.Pivot.FindByState(dgRow,[dmOpen,dmClosed]);
        if (Info<>nil) and (cgOutliner in Options) then
        begin
          if Info.iState=dmClosed then
            DrawState:= DrawState + [dsOpenAfter]
          else if Info.iState=dmOpen then
            DrawState:= DrawState + [dsCloseAfter];
        end;
      end;
      ctColPlus:
      begin
        DrawState := [dsColPlus];
        Info:=DecisionSource.Pivot.FindByState(dgCol,[dmOpen,dmClosed]);
        if (Info<>nil) and (cgOutliner in Options) then
        begin
          if Info.iState=dmClosed then
            DrawState:= DrawState + [dsOpenAfter]
          else if Info.iState=dmOpen then
            DrawState:= DrawState + [dsCloseAfter];
        end;
      end;
    else //ctNone:
      DrawState := [dsNone];
    end;
end;

function TCustomFxGrid.GetSpecialState(ARow,AColumn: Integer): TDecisionPivotState;
begin
  Result := psNone;
  case FGridStateX of
    csPivoting:
      if (FTargetCell.Y = ARow) then
      begin
        if FTargetSwitch then
        begin
          if (FTargetCell.X = AColumn) then Result := psMiddlePivot;
        end
        else if (FTargetCell.X > FActCols) then
        begin
          if (FTargetCell.X - 1 = AColumn) then Result := psTopPivot;
        end
        else if (FTargetCell.X = AColumn) then
          Result := psLeftPivot
        else if (FTargetCell.X - 1 = AColumn) then
          Result := psRightPivot;
      end;
  end;
end;

{
  Fetches data values for the grid, including the row and column
  labels and captions corresponding to dimension data values.
}

function TCustomFxGrid.GetDataPoint(ARow,AColumn: LongInt;
                                          out State: TDecisionDrawState;
                                          out Alignment: TAlignment): String;

var
  aCellType: TDecisionCellType;
  ValueIndex, SubLevel: Integer;
begin
  Result:= '';
  State := [];
  Alignment:= taCenter;
  if Assigned(DecisionSource) then
    with DecisionSource do begin
      GetHitTypes(ARow,AColumn,ValueIndex,State,aCellType);
      case aCellType of
        ctData:
        begin
          Result := GetData(Arow, AColumn, SubLevel);
          if (SubLevel > 0) then State := State + [dsSum];
   	    end;
        ctCaptionCol:
  	    begin
    	    Result := GetCaption(dgCol, AColumn);
        end;
      	ctCaptionRow:
        begin
   	      Result := GetCaption(dgRow, AColumn+FcolOffset);
        end;
       	ctColLabel:
        begin
     	    if dsGroupStart in State then begin
            if dsSum in State then
              Result:= sTotalCaption
            else
              Result:= GetLabel(dgCol, ARow + FRowOffset ,ValueIndex);
          end;
        end;
        ctRowLabel:
        begin
     	  if dsGroupStart in State then begin
            if dsSum in State then
              Result:= sTotalCaption
            else
              Result:= GetLabel(dgRow, AColumn+FColOffset,ValueIndex)
          end;
        end;
      end;
    end;
end;

procedure TCustomFxGrid.SetColWidth(Value: Integer);
begin
  FColWidth := Value;
  inherited DefaultColWidth := Value;
  NewGridLayout;
end;

procedure TCustomFxGrid.SetRowHeight(Value: Integer);
begin
  if (FRowHeight <> Value) then
  begin
    FRowHeight := Value;
    inherited DefaultRowHeight := Value;
    Invalidate;
  end;
end;

procedure TCustomFxGrid.SetGridLineWidth(Value: Integer);
begin
  FGridLineWidth := Value;
  Invalidate;
end;
procedure TCustomFxGrid.SetGridLineColor(Value: TColor);
begin
  FGridLineColor := Value;
  Invalidate;
end;
procedure TCustomFxGrid.SetLabelFont(Value: TFont);
begin
  FLabelFont.Assign(Value);
  Invalidate;
end;
procedure TCustomFxGrid.SetLabelColor(Value: TColor);
begin
  FLabelColor := Value;
  Invalidate;
end;

procedure TCustomFxGrid.SetLabelSumColor(Value: TColor);
begin
  FLabelSumColor := Value;
  Invalidate;
end;

procedure TCustomFxGrid.SetCaptionFont(Value: TFont);
begin
  FCaptionFont.Assign(Value);
  Invalidate;
end;

procedure TCustomFxGrid.SetCaptionColor(Value: TColor);
begin
  FCaptionColor := Value;
  Invalidate; { Only invalidate captions ... }
end;

procedure TCustomFxGrid.SetDataFont(Value: TFont);
begin
  FDataFont.Assign(Value);
  Invalidate;
end;

procedure TCustomFxGrid.SetDataColor(Value: TColor);
begin
  FDataColor := Value;
  Invalidate;   { Only invalidate headers ... }
end;

procedure TCustomFxGrid.SetDataSumColor(Value: TColor);
begin
  FDataSumColor := Value;
  Invalidate;   { Only invalidate headers ... }
end;

procedure TCustomFxGrid.FontChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TCustomFxGrid.SetOptions(Value: TDecisionGridOptions);
begin
  if (FOptions <> Value) then
  begin
    FOptions := Value;
    NewGridLayout;
  end;
end;

procedure TCustomFxGrid.DrawSpecialState(ACanvas: TCanvas; ARect: TRect;
                                               DrawState: TDecisionDrawState;
                                               PivotState: TDecisionPivotState);
var
  X, Y: Integer;
begin
  with ARect do begin
    if dsOpenAfter in DrawState then
    begin
      X := Right - FIndicators.Width;
      Y := (Top + Bottom - FIndicators.Height) shr 1;
      FIndicators.Draw(ACanvas, X, Y, Integer(biDOpen));
    end;
    if (dsCloseAfter in DrawState) then
    begin
      X := Right - FIndicators.Width;
      Y := (Top + Bottom - FIndicators.Height) shr 1;
      FIndicators.Draw(ACanvas, X, Y, Integer(biDClose));
    end;
    if (dsCloseBefore in DrawState) then
    begin
      X := Left;
      Y := (Top + Bottom - FIndicators.Height) shr 1;
      FIndicators.Draw(ACanvas, X, Y, Integer(biDClose));
    end;
    if (dsOpenBefore in DrawState) then
    begin
      X := Left;
      Y := (Top + Bottom - FIndicators.Height) shr 1;
      FIndicators.Draw(ACanvas, X, Y, Integer(biDOpen));
    end;
    if (PivotState = psLeftPivot) then
    begin
      Y := (Top + Bottom - FIndicators.Height) shr 1;
      FIndicators.Draw(ACanvas, Left, Y, Integer(biLeftArrow));
      ACanvas.Pen.Color := clRed;
      ACanvas.Pen.Width := 2;
      ACanvas.MoveTo(Left,Top);
      ACanvas.LineTo(Left,Bottom);
    end;
    if (PivotState = psRightPivot) then
    begin
      X := Right - FIndicators.Width;
      Y := (Top + Bottom - FIndicators.Height) shr 1;
      FIndicators.Draw(ACanvas, X, Y, Integer(biRightArrow));
    end;
    if (PivotState = psMiddlePivot) then
    begin
      X := (Left + Right - FIndicators.Width) div 2;
      Y := (Top + Bottom - FIndicators.Height) div 2;
      FIndicators.Draw(ACanvas, X, Y, Integer(biPivot));
    end;
    if (PivotState = psTopPivot) then
    begin
      ACanvas.Pen.Color := clRed;
      ACanvas.Pen.Width := 2;
      ACanvas.MoveTo(Left,Top+1);
      ACanvas.LineTo(Right,Top+1);
    end;
  end;
end;

function TCustomFxGrid.SelectCell(Arow, AColumn:LongInt): Boolean;
begin
  Result := True;
end;

procedure TCustomFxGrid.TopLeftChanged;
begin
  inherited TopLeftChanged;
  if Assigned(FOnTopLeftChanged) then FOnTopLeftChanged(Self);
end;

procedure TCustomFxGrid.ColWidthsChanged;
var
  i, newWidth: Integer;
begin
  {
    In design mode, permit only the first summary row to be modified in width
    Keep everything else to that size
  }
  if {(csDesigning in ComponentState) and} not FChanging then
  begin
    FChanging:=True;
    NewWidth := FColWidth;
    for I:= FCaptionCol+FActRows to ColCount-1 do
      if (ColWidths[i] > NoSpace) and (Colwidths[i] <> FColWidth) then
        NewWidth := ColWidths[i];
    for i := FCaptionCol+FActRows+1 to ColCount-1 do
    begin
      ColWidths[i] := NewWidth;
    end;
    SetColWidth(NewWidth);
    FChanging := False;
  end;
end;

procedure TCustomFxGrid.RowHeightsChanged;
var
  i: Integer;
  newHeight: Integer;
begin
  {
    In design mode, permit only the first summary row to be modified in width
    Keep everything else to that size
  }
  if {(csDesigning in ComponentState) and} not FCHanging then
  begin
    FChanging := True;
    NewHeight := FRowHeight;
    for i := 0 to RowCount - 1 do
    begin
      if (RowHeights[i] > NoSpace) and (RowHeights[i] <> FRowHeight) then
        NewHeight := RowHeights[i];
    end;
    for i := 0 to RowCount-1 do
      RowHeights[i] := NewHeight;
    SetRowHeight(NewHeight);
    FChanging := False;
  end;
end;

function TCustomFxGrid.CellRect(ACol, ARow: Longint): TRect;
begin
  Result := BoxRect(ACol+FixedCols, ARow+FixedRows, ACol+FixedCols, ARow+FixedRows);
end;

procedure TCustomFxGrid.Paint;
var
  Rect: TRect;
begin
  inherited;
  if not (assigned(DecisionSource) and DecisionSource.Ready and factivegrid) then
    Exit;
  Rect :=  BoxRect(0, 0, ColCount, RowCount);
  if (cgGridLines in Options) and (GridLineWidth > 0) then
    with Canvas do
    begin
      Pen.Color := FGridLineColor;
      Pen.Width := FGridLineWidth;
      with Rect do
      begin
        MoveTo(Right, Top-1);
        LineTo(Right, Bottom);
        LineTo(Left-1, Bottom);
        if (BorderStyle = bsNone) and not (cgOutliner in Options) then
        begin
          MoveTo(Left,Bottom);
          LineTo(Left,Top);
        end;
      end;
    end;
end;

procedure TCustomFxGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  Value: string;
  x: integer;
  FrameOffs: Byte;
  Elevated: Boolean;
  DrawState: TDecisionDrawState;
  PivotState: TDecisionPivotState;
  Specials: TDecisionDrawState;
  Alignment: TAlignment;
  aColor: TColor;
  aFont: TFont;
  aDisplayDim: TDisplayDim;
begin
  if not (assigned(DecisionSource) and DecisionSource.Ready and factivegrid) then
    Exit;
  if (csLoading in ComponentState) then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ARect);
    Exit;
  end;
  if (rowHeights[ARow] <= NoSpace) or (colWidths[ACol] <= NoSpace) then
    Exit;
  RawToDataCoord(ACol,ARow);
  aDisplayDim := nil;
  with DecisionSource,Canvas do begin
    // Elevate applies to either
    Value := GetDataPoint(ARow,ACol,DrawState,Alignment);
    Elevated := (gdFixed in AState) and (cgGridLines in Options) and
    		([dsRowCaption,dsRowIndicator,dsColCaption,dsRowPlus,dsColPlus,dsNone] * DrawState <> []);
    if gdFocused in AState then begin
      Brush.Color := clHighlight;
      Font.Color := clHighlightText;
    end else if (gdFixed in AState) then begin
      if [dsRowIndicator, dsNone, dsColPlus, dsColIndicator]*DrawState <> [] then begin
        Brush.Color := Color;
      end else if (dsRowCaption in DrawState) or (dsColCaption in DrawState) then begin
        Font:= FCaptionFont;
        Brush.Color:= FCaptionColor;
      end else if dsRowValue in DrawState then begin
        Font := FLabelFont;
        Brush.Color := FLabelColor;
        if assigned(FDisplayDims) and assigned(DecisionSource) then begin
          aDisplayDim:=TDisplayDim(FDisplayDims[Pivot.IndexInfo(dgRow,FActRows+ACol,True).Dim]);
          if aDisplayDim.FColor<>clNone then
            Brush.Color := aDisplayDim.FColor;
          Alignment:= aDisplayDim.FAlignment;
        end;
        if dsSum in DrawState then begin
          Font:=FLabelSumFont;
          if FLabelSumColor<>clNone then
            Brush.Color := FLabelSumColor;
        end;
      end else if dsColValue in DrawState then begin
        Font := FLabelFont;
        Brush.Color := FLabelColor;
        if assigned(FDisplayDims) and assigned(DecisionSource) then begin
          aDisplayDim:= TDisplayDim(FDisplayDims[Pivot.IndexInfo(dgCol, FActCols+ARow,true).Dim]);
          if aDisplayDim.FColor<>clNone then
            Brush.Color := aDisplayDim.FColor;
          Alignment:= aDisplayDim.FAlignment;
        end;
        if dsSum in DrawState then begin
          Font:=FLabelSumFont;
          if FLabelSumColor<>clNone then
            Brush.Color:= FLabelSumColor;
        end;
      end else begin
        Font := Self.Font;
        Brush.Color := Self.Color;
      end;
    end
    else
    begin
      Font := FDataFont;
      Brush.Color := FDataColor;
      if Assigned(FDisplayDims) and Assigned(DecisionSource) then
      begin
        aDisplayDim := TDisplayDim(FDisplayDims[DecisionSource.nDims + DecisionSource.CurrentSum]);
        Alignment := aDisplayDim.FAlignment;
      end;
      if (dsSum in DrawState) then
      begin
        if FDataSumColor <> clNone then Brush.Color := FDataSumColor;
      end
      else if Assigned(aDisplayDim) and (aDisplayDim.FColor <> clNone) then
        Brush.Color := aDisplayDim.FColor;
    end;
    if not Elevated then
      FrameOffs := 2
    else
    begin
      InflateRect(ARect, -1, -1);
      FrameOffs := 1;
    end;
    if assigned(FOnDecisionDrawCell) then
    begin
      aFont := Font;
      aColor := Brush.Color;
      FOnDecisionDrawCell(Self, ACol, ARow, Value, aFont, aColor, AState, DrawState);
      Font := aFont;
      Brush.Color := aColor;
      if not DefaultDrawing then Exit;
    end;
    if (Value = '') then
      FillRect(ARect)
    else
      if ((dsRowCaption in DrawState) or (dsColCaption in DrawState)) and (dsOpenAfter in DrawState) or (dsCloseAfter in DrawState) then
      begin
        FillRect(ARect);
        ARect.Right := ARect.Right-FIndicators.Width;
        if TextWidth(Value) > (ARect.Right-ARect.Left) then
          Alignment := taLeftJustify;
        WriteText(Canvas, ARect, FrameOffs, FrameOffs, Value, Alignment);
        ARect.Right := ARect.Right+FIndicators.Width;
      end
      else
      begin
        if TextWidth(Value) > (ARect.Right-ARect.Left) then
          Alignment := taLeftJustify;
        WriteText(Canvas, ARect, FrameOffs, FrameOffs, Value, Alignment);
      end;
    Pen.Color := FGridLineColor;
    Pen.Width := FGridLineWidth;
    { drawlines }
    if ([dsSum, dsData, dsRowValue, dsColValue]*DrawState <> [])
    and (FGridLineWidth > 0) and (cgGridLines in Options) then
    begin
      MoveTo(ARect.Left, ARect.Bottom);
      { draw the left border }
      if ((aCol = -FActRows) or ((dsColValue in DrawState) and ([dsGroupStart,dsSum]*DrawState = []))) then
        MoveTo(ARect.Left, ARect.Top)
      else
      begin
        LineTo(ARect.Left, ARect.Top-1);
        MoveTo(Arect.Left, Arect.Top);
      end;
      { draw the top border }
      if (((dsRowValue in DrawState) and ([dsGroupStart,dsSum]*DrawState = []))) then
        MoveTo(ARect.Right, ARect.Top)
      else
        LineTo(ARect.Right, ARect.Top);
      { draw the right border }
      if (((aCol <> colCount-FixedCols-1) and ([dsGroupStart,dsSum]*DrawState = []))) then
        MoveTo(ARect.Right, ARect.Bottom)
      else
      begin
        MoveTo(ARect.Right, ARect.Top-1);
        LineTo(ARect.Right, ARect.Bottom);
      end;
      { draw the BOTTOM border }
      if ((aRow = RowCount-FixedRows-1) or ([dsGroupStart,dsSum]*DrawState <> [])) then
        LineTo(ARect.Left, ARect.Bottom);
    end;
    if [dsRowPlus,dsColPlus] * DrawState <> [] then
    begin
      if (ARect.Left < (ARect.Right-PlusWidth)) and Elevated then
      begin
        x := ARect.Right;
        ARect.Right := ARect.Right-PlusWidth;
        InflateRect(ARect, 1, 1);
        DrawEdge(Canvas.Handle, ARect, BDR_RAISEDINNER, BF_BOTTOMRIGHT);
        DrawEdge(Canvas.Handle, ARect, BDR_RAISEDINNER, BF_TOPLEFT);
        InflateRect(ARect, -1, -1);
        ARect.Right := x;
      end;
      ARect.Left := ARect.Right-PlusWidth;
      Font := FCaptionFont;
      Brush.Color := FCaptionColor;
      FillRect(ARect);
    end;
    if Elevated then
    begin
      InflateRect(ARect, 1, 1);
      DrawEdge(Canvas.Handle, ARect, BDR_RAISEDINNER, BF_BOTTOMRIGHT);
      DrawEdge(Canvas.Handle, ARect, BDR_RAISEDINNER, BF_TOPLEFT);
      InflateRect(ARect, -1, -1);
    end;
    Specials := [dsOpenAfter,dsOpenBefore,dsCloseBefore,dsCloseAfter,dsRowPlus,dsColPlus];
    PivotState := GetSpecialState(ARow, ACol);
    if (DrawState * Specials <> []) or (PivotState <> psNone) then
      DrawSpecialState(Canvas, ARect, DrawState, PivotState);
  end;
end;

{check to see if the coordinate passed in is in the caption row, caption column,
 or the header.  Set Result to one of the values in the }

function TCustomFxGrid.WhichCoord( Coord: TGridCoord ): TDecisionCellType;
begin
  Result := ctNone;
  with Coord do
  begin
    if (X >= 0) and (Y >= 0) then
      Result := ctData
      // this is one of the row dimension names
    else if (FActRows > 0) and (Y = -1) and (X < 0) and (x >= -FActRows) then
      Result := ctCaptionRow
      { this is one of the col dimension names }
    else if (FActCols > 0) and (Y = -FRowOffset-1) and (X >= 0) and (X < FActCols) then
      Result := ctCaptionCol
      { this is a row field vale }
    else if (Y >= 0) and (X < 0) and (X >= -FColOffset) then
      Result := ctRowLabel
      { this is a column field value }
    else if (X >= 0) and (Y < 0) and (Y > -FRowOffset-1) then
      Result := ctColLabel
    else if ((cgOutliner in Options) and (FTotCols > 0) and (((FActCols > 0) and (Y = -FActCols-1) and
    (X = -1)) or ((X = -1) and (Y = -2) and (FActCols = 0)))) then
      Result := ctColPlus
    else if ((cgOutliner in Options) and (FTotRows > 0) and (X = -FColOffset-FCaptionCol) and (Y = -1)) then
      Result := ctRowPlus
    else if ((cgOutliner in Options) and (X = -fColOffset-FCaptionCol)) then
      Result := ctRowIndicator;
  end;
end;

  {Function:	this performs the same function when in the middle of a pivot }

function TCustomFxGrid.WhichCoordExCap(Coord: TGridCoord): TDecisionCellType;
begin
  Result := ctNone;
  with Coord do
  begin
    if (Y = -1-FRowOffset) and (X = -fColOffset) and (FActCols > 0) and (FActRows > 0) then
      Result := ctNone { ctHeader }
    else if (Y = -1) and (X <= 0) then
      Result := ctCaptionRow
    else if (Y = -FRowOffset-1) and (X >= 0) and (X <= FActCols) then
      Result := ctCaptionCol
    else if (Y = -1) and (X = 1) and (FActCols = 0) then
      Result := ctNewCaptionCol;
  end;
end;

{Translate a mouse coordinate X,Y into a Decision cell position}

function TCustomFxGrid.MouseToDataCoord(X,Y: Integer): TGridCoord;
begin
  Result := MouseCoord(X,Y);
  if Result.X = -1 then
  begin
    Result.X := -1000;
    Exit;
  end;
  RawToDataCoord(Result.X,Result.Y);
end;

procedure TCustomFxGrid.InvalidateTargetCell;
var
  X, Y: LongInt;
begin
  X := FTargetCell.X;
  Y := FTargetCell.Y;
  DataToRawCoord(X, Y);
  InvalidateCell(X, Y);
  InvalidateCell(X-1, Y);
end;
  { This routine figures out whether the mouse is }
function TCustomFxGrid.GetHorzButtonPlace(X,Y: Integer): TButtonPlace;
var
  Coord, Coord2, Coord3: TGridCoord;
  Width: Integer;
begin
  Coord := MouseCoord(X,Y);                { grid coordinates where mouse is. }
  Width := ColWidths[Coord.X] div 4;
  Coord2 := MouseCoord(X-Width,Y);         { grid coordinate 1/4 col to left }
  Coord3 := MouseCoord(X+Width,Y);         { grid coordinate 1/4 col to right }
  if (Coord.X = -1) then Coord.X := ColCount;
  if (Coord3.X = -1) then Coord3.X := ColCount;
  {
    Set result to left if we are in the left quarter or the left column
    Set result to right if we are in the right quarter
    Else set result to middle.
  }
  if (Coord2.X < Coord.X) or ((Coord.X = 0) and (X <= Width)) then
    Result := bpLeft
  else if (Coord3.X > Coord.X) then
    Result := bpRight
  else
    Result := bpMiddle;
end;

{
  This routine is used when the user is in the middle of a pivot.  The mouse
  coordinates passed in are used to deduce whether the user is over a target
  zone.  If so, the appropriate
}

procedure TCustomFxGrid.SetNearestTargetCell(X, Y: Integer);
var
  Place: TButtonPlace;
  Coord: TGridCoord;
  XMax, YLabel, XLabel: Integer;
  bSwitch: Boolean;
  bNewCondition: Boolean;
begin
  if (FGridStateX = csPivoting) then
  begin
    Coord := MouseToDataCoord(X,Y);             { get grid coordinates }
    Place := GetHorzButtonPlace(X,Y);           { left, right, or middle? }
    YLabel := Coord.Y + FRowOffset;
    XLabel := Coord.X + fColOffset;
    bSwitch := (Place = bpMiddle);
    with Coord do
    begin
      if (XLabel < 0) then
        Exit
      else if (FActCols > 0) and (FActRows > 0) and
      (YLabel = -1) and (XLabel = 0) and (Place = bpLeft) then
      begin
        { Target is now the first inactive dimension } 
        bSwitch := False;
      end
      else if (Y >= -1) and (FActRows = 0) then
      begin
        { Target is now the first row dimension }
        Y := -1;
        X := 0;
        bSwitch := False;
      end
      else if (Y = -1) and (X >= 0) and (FActCols = 0) and not ((X = 0) and (Place = bpLeft)) then
      begin
        { Target is now the first column dimension }
        X := 1;
        Y := -1;
        bSwitch := False;
      end
      else if ((YLabel = -1) or (Y < FSourceCell.Y)) and (FActCols > 0) then
      begin
        { Target is in the existing column dimensions }
        Y := -1 - FRowOffset;
        if (Place = bpRight) then Inc(X);
        XMax := FActCols;
        if (X >= XMax) then
        begin
          bSwitch := False;
          X := XMax;
        end
        else if (X < 0) then
        begin
          bSwitch := False;
          X := 0;
        end
      end
      else
      begin
        { All other cases makes this a row target }
        Y := -1;
        if (Place = bpRight) then Inc(X);
        XMax := FActRows - fColOffset;
        if (X >= XMax) then
        begin
          bSwitch := False;
          X := XMax;
        end
      end
    end;
    {
      We got a new pivot situation if we have a new target cell or stop or start
      moving instead of switching dimensions.
    }
    bNewCondition := ((FTargetSwitch <> bSwitch) or (FTargetCell.X <> Coord.X)
                       or (FTargetCell.Y <> Coord.Y));
    {
     If we are going to move a dimension: eliminate both of the 2 positions where
     then dimension isn't moved at all.
    }
    if not bSwitch and bNewCondition then
      bNewCondition := ((FSourceCell.X > Coord.X) or (FSourceCell.X+1 < Coord.X)
                         or (FSourceCell.Y <> Coord.Y));
    if bNewCondition then
    begin
      InvalidateTargetCell;
      FTargetCell := Coord;
      FTargetSwitch := bSwitch;
      InvalidateTargetCell;
    end;
  end;
end;

{
  input a cell type and grid coordinate.
  return a row or column array and index
}

function TCustomFxGrid.GetDimensionIndex(cellType: TDecisionCellType;
 Coord: TGridCoord; var dimGroup: TDimGroup; var bExists: Boolean): Integer;
begin
  case cellType of
    ctCaptionRow:
    begin
      Result := Coord.X + fColOffset;
      dimGroup := dgRow;
      bExists := True;
    end;
    ctCaptionCol:
    begin
      Result := Coord.X;
      dimGroup := dgCol;
      bExists := True;
    end;
    else
    begin
      dimGroup := dgCol;
      Result := 0;
      bExists := False;  { Indicates: this is not an existing dimension cell. }
    end;
  end;
end;

procedure TCustomFxGrid.PerformPivot;
var
  wCoord: TDecisionCellType;
  II1, II2: Integer;
  bExist: Boolean;
  SdimGroup, DdimGroup: TDimGroup;
begin
  if cgPivotable in FOptions then begin
    wCoord:= WhichCoord(FSourceCell);
    II1:= GetDimensionIndex(wCoord, FSourceCell, SdimGroup, bExist);
    if not bExist then Exit;
    if FTargetSwitch then begin
      // This code is for switching two existing dimensions
      wCoord := WhichCoord(FTargetCell);
      II2 := GetDimensionIndex(wCoord, FTargetCell, DdimGroup, bExist);
      if not bExist then Exit;
      if assigned(DecisionSource) then
        DecisionSource.SwapDimIndexes(SdimGroup, DdimGroup, II1, II2, true);
    end else begin
      // this is a move from one place to another
      if not assigned(DecisionSource) then Exit;
      wCoord := WhichCoordExCap(FTargetCell);
      II2:= GetDimensionIndex(wCoord, FTargetCell, DdimGroup, bExist);
      if (II2 >= 0) then
        DecisionSource.MoveDimIndexes(SdimGroup, DdimGroup, II1, II2, true);
    end;
  end;
end;

procedure TCustomFxGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  sCellType:TDecisionCellType;
  sDrawState: TDecisionDrawState;
  Index: Integer;
  ActiveIndexes: Integer;
  TPlace: TButtonPlace;
  dimGroup: TDimGroup;
  ValueIndex: Integer;
  coord: TGridCoord;
  aRect: TRect;
  i: integer;
  iRange: TDimRange;
  aString: string;
  passArray: TValueArray;

begin
  inherited MouseDown(Button, Shift, X, Y);
  { Allow pivoting on left mouse down only if gridstate is not already pivoting }
  { and the user has hit a cell in the row or column cpation or the header }
  if (FGridState <> gsNormal) then Exit;
  FSourceCell := MouseToDataCoord(X,Y);          { source cell is where mouse down occurs }
  GetHitTypes(FSourceCell.Y, FSourceCell.X, ValueIndex,sDrawState, sCellType);
  if (Button = mbRight) then
  begin
    if (sCellType = ctData) then
    begin
      if assigned (DecisionSource) and DecisionSource.ready then
        with DecisionSource do
        begin
          passArray := TValueArray.Create(0);
          try
            passArray.SortOrder := tsNone;
            if CellValueArray(FSourceCell.X, FSourceCell.Y, passArray) then
            begin
              if assigned (FOnDecisionExamineCell) then
              begin
                FOnDecisionExamineCell(self, FSourceCell.X, FSourceCell.Y, DecisionSource.CurrentSum,
                TValueArray(passArray));
              end
              else
              begin
              end;
            end;
          finally
            passArray.free;
          end;
        end;
    end
    else if not (sCellType in [ctRowLabel,ctColLabel,ctCaptionRow,ctCaptionCol]) then
    begin
      FMenu.Clear;
      FMenu.AddLine(SGridMenu1, tmNone,0);
      if ShowCubeEditor then
      begin
        FMenu.AddLine(SGridMenu2, tmNone,1);
      end;
      if assigned(DecisionSource) and DecisionSource.ready and (cgPivotable in Options) then
        with DecisionSource do
        begin
          if (nDims > 0) or (nSums > 0) then
            FMenu.AddLine('-', tmNone,-1);
          for i := 0 to nDims-1 do
          begin
            aString := GetDimensionName(i);
            if Pivot[I].iState=dmOpen then
              FMenu.Addline(aString, tmChecked, i+100)
            else
              FMenu.AddLine(aString, tmNone, i+100);
          end;
          for i := 0 to nSums-1 do
          begin
            aString := GetSummaryName(i);
            if (i = DecisionSOurce.CurrentSum) then
              FMenu.Addline(aString, tmChecked, i+200)
            else
              FMenu.AddLine(aString, tmNone, i+200);
          end;
        end;
        FMenu.OnSelected := SelectGridOptions;
        FMenu.PopUpAtMe(Self, X,Y);
      end
    else if sCellType in [ctCaptionRow, ctCaptionCol, ctColPlus, ctRowPlus] then
      with DecisionSource,FMenu do
      begin
        Clear;
        if sCellType in [ctCaptionRow, ctCaptionCol] then
        begin
          if (sCellType = ctCaptionRow) then
            dimGroup := dgRow
          else
            dimGroup := dgCol;
          if (dimGroup = dgRow) then
            Index := FSourceCell.X+FColOffset
          else
            Index := FSourceCell.X;
          if FDisplayDims[Pivot.IndexInfo(dimGroup,Index,True).Dim].SubTotals then
          begin
            FMenu.AddLine(SCaptionMenu1, tmRadio,0);
            FMenu.AddLine(SCaptionMenu2, tmNone,1);
          end
          else
          begin
            FMenu.AddLine(SCaptionMenu1, tmNone,0);
            FMenu.AddLine(SCaptionMenu2, tmRadio,1);
          end;
        end;
        if assigned(DecisionSource) and DecisionSource.ready and (cgPivotable in Options) then
          with DecisionSource do
          begin
            if ((nDims + nSums) > 0) then
              FMenu.AddLine('-', tmNone,-1);
            for i := 0 to nDims-1 do
            begin
              aString := GetDimensionName(i);
              if Pivot[I].iState=dmOpen then
                FMenu.Addline(aString, tmChecked, i+100)
              else
                FMenu.AddLine(aString, tmNone, i+100);
            end;
            for i := 0 to nSums-1 do
            begin
              aString := GetSummaryName(i);
              if (i = DecisionSOurce.CurrentSum) then
                FMenu.Addline(aString, tmChecked, i+200)
              else
                FMenu.AddLine(aString, tmNone, i+200);
            end;
          end;
        FMenu.OnSelected := SelectDimOptions;
        aRect := inherited CellRect(DataToRawX(FSourceCell.X), DataToRawY(FSourceCell.Y)+1);
        FMenu.PopUpAtMe(Self, aRect.Left,aRect.Top);
      end
    else
      with FMenu do
      begin
        if (sCellType = ctRowLabel) then
          dimGroup := dgRow
        else
          dimGroup := dgCol;
        isGroupStart := dsGroupStart in sDrawState;
        if (dimGroup = dgRow) then
        begin
          ActiveIndexes := FActRows;
          Index := FSourceCell.x + fColOffset;
          Cell := FSourceCell.y;
        end
        else
        begin
          ActiveIndexes := FActCols;
          dimGroup := dgCol;
          Index := FSourceCell.Y + FRowOffset;
          Cell := FSourceCell.X;
        end;
        Clear;
        if (Index < ActiveIndexes-1) then
        begin
          iRange := DecisionSource.GetGroupExtent(dimGroup, Index, Cell);
          if (dimGroup = dgRow) then
          begin
            iRange.First := DatatoRawY(iRange.First);
            iRange.Last := DatatoRawY(iRange.Last);
          end
          else
          begin
            iRange.First := DatatoRawX(iRange.First);
            iRange.Last := DatatoRawX(iRange.Last);
          end;
          if (dimGroup = dgRow) then
          begin
            if RowHeights[iRange.First] = NoSpace then
              i := 3
            else if RowHeights[iRange.Last] = NoSpace then
              i := 2
            else
              i := 1;
          end
          else
          begin
            if ColWidths[iRange.First] = NoSpace then
              i := 3
            else if ColWidths[iRange.Last] = NoSpace then
              i := 2
            else
              i := 1;
          end;
          if (i = 1) then
            AddLine(SCaptionMenu1, tmRadio, 1)
          else
            AddLine(SCaptionMenu1, tmNone,1);
          if (i = 2) then
            AddLine(SCaptionMenu2, tmRadio, 2)
          else
            AddLine(SCaptionMenu2, tmNone,2);
          if (i = 3) then
            AddLine(SCaptionMenu3, tmRadio, 3)
          else
            AddLine(SCaptionMenu3, tmNone,3);
          AddLine('-', tmNone,-1);
        end;
        AddLine(SDrillIn, tmNone, 0);
        FMenu.OnSelected := RightMouse;
        aRect := inherited CellRect(DataToRawX(FSourceCell.X), DataToRawY(FSourceCell.Y)+1);
        FMenu.PopUpAtMe(Self, aRect.Left,aRect.Top);
      end;
  end
  else
  begin
    if (SCellType = ctColPlus) then
    begin
      Coord := MouseCoord(X,Y);
      aRect := BoxRect(Coord.X, Coord.Y, Coord.X, Coord.Y);
      if (X >= (ARect.Right-PlusWidth)) then
      begin
        if (dsOpenAfter in sDrawState) then
          DecisionSource.OpenDimIndexRight(dgCol, -1,true);
        if (dsCloseAfter in sDrawState) then
          DecisionSource.CloseDimIndexRight(dgCol, -1,true);
      end;
    end;
    if (SCellType = ctRowPlus) then
    begin
      if (dsOpenAfter in sDrawState) then
        DecisionSource.OpenDimIndexRight(dgRow, -1,true);
      if (dsCloseAfter in sDrawState) then
        DecisionSource.CloseDimIndexRight(dgRow, -1,true)
    end;
    if (sCellType in [ctCaptionRow,ctCaptionCol]) then
    begin
      TPlace := GetHorzButtonPlace(X,Y);
      if (sCellType in [ctCaptionRow]) then
        dimGroup := dgRow
      else
        dimGroup := dgCol;
      if (TPlace = bpMiddle) and (cgPivotable in Options) then
      begin
        FGridStateX := csPivoting;  { set state to pivoting } 
        FTargetCell := FSourceCell; { remember which cell is being pivoted }
        InvalidateTargetCell;
      end
      else if (TPlace = bpRight) then
      begin
        if (dimGroup = dgRow) then
          Index := FsourceCell.x + fColOffset
        else
          Index := FSourceCell.x;
        with DecisionSource do
        begin
          if ((cgOutliner in Options) and (rcNextOpen in GetRowState(Pivot.IndexInfo(dimGroup,Index,True).Dim))) then
            DecisionSource.CloseDimIndexRight(dimGroup, Index,true)
          else
            DecisionSource.OpenDimIndexRight(dimGroup, Index,true);
        end;
      end;
    end;
  end;
end;

procedure TCustomFxGrid.SelectGridOptions(Sender: TObject);
var
  Action: Integer;
begin
  Action := FMenu.FAction;
  case Action of
    0:  Totals := not Totals;
    1:
    begin
      if Assigned (DecisionSource) and Assigned(DecisionSource.DecisionCube) then
      begin
        //DecisionSOurce.DecisionCube.SHowCubeDialog;
      end;
    end;
    else if Assigned (DecisionSource) then
      with DecisionSource do
      begin
        if (Action < (nDims + 100)) then
        begin
          ToggleDimIndex(Pivot[Action-100].iGroup, Pivot[Action-100].iIndex, false);
        end
        else if (Action < (nSums + 200)) then
        begin
          DecisionSource.SetCurrentSummary(Action-200);
        end;
      end;
  end;
end;

procedure TCustomFxGrid.SelectDimOptions(Sender: TObject);
var
  iDim, Action: Integer;
begin
  Action := FMenu.FAction;
  if (Action < 100) then
  begin
    if (FMenu.Index >= 0) then
    begin
      iDim:= DecisionSource.Pivot.IndexInfo(FMenu.dimGroup,Fmenu.Index,True).Dim;
      if (iDim >= 0) then
        FDisplayDims[iDim].SubTotals := (Action = 0);
    end;
  end
  else if Assigned (DecisionSource) then
    with DecisionSource do
    begin
      if (Action < (nDims + 100)) then
      begin
        ToggleDimIndex(Pivot[Action-100].iGroup, Pivot[Action-100].iIndex, false);
      end
      else if (Action < (nSums + 200)) then
      begin
        DecisionSource.SetCurrentSummary(Action-200);
      end;
    end;
end;

procedure TCustomFxGrid.RightMouse(Sender: TObject);
var
  isSum,isBreak: boolean;
  iRange: TDimRange;
  Action: Integer;
  i: Integer;
begin
{
  Popup a menu and get one of the following user choices for this
  combination of dimension and value indicated by the Row/Col Label
}
  with FMenu do
  begin
    if (dimGroup = dgRow) and (FActRows = 0) then Exit;
    if (not (dimGroup = dgRow)) and (FActCols = 0) then Exit;
    Action := FAction;
    if (Action = 0) then
    begin
      valueIndex := DecisionSource.GetValueIndex(dimGroup,Index,Cell,isBreak,isSum);
      DecisionSource.DrillDimIndex(dimGroup,Index,ValueIndex,true);
    end
    else
    begin
      iRange := DecisionSource.GetGroupExtent(dimGroup, Index, Cell);
      if (dimGroup = dgRow) then
      begin
        iRange.First := DatatoRawY(iRange.First);
        iRange.Last := DatatoRawY(iRange.Last);
      end
      else
      begin
        iRange.First := DatatoRawX(iRange.First);
        iRange.Last := DatatoRawX(iRange.Last);
      end;
      if (iRange.First = iRange.Last) then Exit;
      FChanging := True;
      if (Action = 1) then
      begin
        for i := iRange.First to iRange.Last do
        begin
          if (dimGroup = dgRow) then
            RowHeights[i] := RowHeight
          else
            ColWidths[i] := ColWidth;
        end;
      end
      else if (Action = 2) then
      begin
        for i := iRange.First to iRange.Last-1 do
        begin
          if (dimGroup = dgRow) then
            RowHeights[i] := RowHeight
          else
            ColWidths[i] := ColWidth;
        end;
        if (dimGroup = dgRow) then
          RowHeights[iRange.Last] := NoSpace
        else
          ColWidths[iRange.Last] := NoSpace;
      end
      else if (Action = 3) then
      begin
        for i := iRange.First to iRange.Last-1 do
        begin
          if (dimGroup = dgRow) then
            RowHeights[i] := NoSpace
          else
            ColWidths[i] := NoSpace;
        end;
        if (dimGroup = dgRow) then
          RowHeights[iRange.Last] := RowHeight
        else
          ColWidths[iRange.Last] := ColWidth;
      end;
      FChanging := False;
    end;
  end;
end;

procedure TCustomFxGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if (FGridStateX = csPivoting) then
    SetNearestTargetCell(X,Y);
end;

procedure TCustomFxGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (FGridStateX = csPivoting) then
  begin
    SetNearestTargetCell(X, Y);
    InvalidateTargetCell;
    FGridStateX := csNormal;
    PerformPivot;
  end;
end;

function TCustomFxGrid.GetDecisionSource: TFxSource;
begin
  Result := TFxSource(FDataLink.DecisionSource);
end;

procedure TCustomFxGrid.SetDecisionSource(Value: TFxSource);
var
  oldSource: TFxSource;
begin
  oldSource := FDatalink.DecisionSource;
  FDataLink.DecisionSource := Value;
  if (Value <> oldSource) then NewDataStructure;
end;

procedure TCustomFxGrid.CMDesignHitTest(var Msg: TCMDesignHitTest);
var
  coord: TGridCoord;
  iIndex: Integer;
  iState: TDecisionDrawState;
  iType: TDecisionCellType;
  aRect: TRect;
begin
  inherited;
  if (msg.Result <> 0) then Exit;
  if (FGridStateX = csPivoting) then
  begin
    Msg.Result := 1;
    Exit;
  end;
  Coord := MousetoDataCoord(Msg.Pos.X, Msg.Pos.Y);
  GetHitTypes(Coord.Y, Coord.X, iIndex, iState, iType);
  if (iTYPE in [ctCaptionCol, ctCaptionRow, ctColLabel, ctRowLabel, ctRowPlus]) then
  begin
    msg.Result := 1;
  end
  else if (iType = ctColPlus) then
  begin
    ARect := CellRect(Coord.X, Coord.Y);
    if (Msg.Pos.x < (ARect.Right-FIndicators.Width)) then
      msg.Result := 0
    else
      msg.Result := 1;
  end
  else
    msg.Result := 0;
end;

function TCustomFxGrid.GetData(ARow, AColumn: Integer; var SubLevel: Integer): string;
var
  iDim: Integer;
  aFormat: String;
begin
  if assigned(DecisionSource) then
  begin
    iDim := DecisionSource.CurrentSum + DecisionSource.nDims;
    aFormat := FDisplayDims[iDim].FFormat;
    if (aFormat <> '') then
    begin
      Result := FormatVariant(DecisionSource.GetDataAsVariant(Arow, AColumn, subLevel), aFormat);
    end
    else
      Result := DecisionSource.GetDataAsString(Arow, AColumn, subLevel);
  end
  else
    Result := '';
end;

function TCustomFxGrid.GetCaption(dimGroup: TDimGroup; Index: Integer): String;
var
  iDim: Integer;
begin
  Result := '';
  if assigned(DecisionSource) then begin
    iDim:=DecisionSource.Pivot.IndexInfo(dimGroup,Index,True).Dim;
    Result:=FDisplayDims[iDim].DisplayName;
    if Result='' then Result:= DecisionSource.GetDimensionName(iDim);
  end;
end;

function TCustomFxGrid.GetLabel(dimGroup:TDimGroup; Index:Integer;
				      ValueIndex: Integer):string;
var
  iDim: Integer;
  aFormat: String;
begin
  if assigned(DecisionSource) and (ValueIndex >= 0) then
    with DecisionSource do begin
      iDim:=Pivot.IndexInfo(dimGroup,Index,True).Dim;
      aFormat:=FDisplayDims[iDim].FFormat;
      if aFormat<>'' then
        Result := FormatVariant(GetMemberAsVariant(iDim, ValueIndex), aFormat)
      else
        Result:= DecisionSource.GetMemberAsString(iDim, ValueIndex);
    end;
end;

function TCustomFxGrid.GetCells(ACol, ARow: Integer): String;
var
  DrawState: TDecisionDrawState;
  Alignment: TAlignment;
begin
  if (ACol < -FixedCols) or (ARow < -FixedRows) or (ACol >= (ColCount - FixedCols))
  or (ARow >= (RowCount - FixedRows)) then
    Result := sGridCellError
  else
    Result := GetDataPoint(ARow, ACol, DrawState, Alignment);
end;

function TCustomFxGrid.CellDrawState(ACol, ARow: Integer;	var Value: string;
                                           var DrawState: TDecisionDrawState): boolean;
var
  Alignment: TAlignment;
begin
  if (ACol < -FixedCols) or (ARow<-FixedRows) or (ACol >= (ColCount - FixedCols))
  or (ARow >= (RowCount - FixedRows)) then
    Result := false
  else
  begin
    Value := GetDataPoint( ARow, ACol, DrawState, Alignment);
    Result := true;
  end;
end;

function TCustomFxGrid.CellValueArray(ACol, ARow: Integer; var ValueArray: TValueArray): boolean;
begin
  if not assigned(DecisionSource) then Result := false
  else
  begin
    Result := DecisionSource.GetValueArray(ACol, ARow, TSmallIntArray(ValueArray));
  end;
end;

function TCustomFxGrid.GetTotals: boolean;
var
  i: Integer;
begin
  Result := False;
  if assigned (FDisplayDims) then
  begin
    for i := 0 to FDisplayDims.Count-1 do
    begin
      if FDisplayDims[i].subtotals then
        Result := True;;
    end;
  end;
end;

function TCustomFxGrid.GetFixedRows: integer;
begin
  Result := inherited FixedRows;
end;

function TCustomFxGrid.GetFixedCols: integer;
begin
  Result := inherited FixedCols;
end;

function TCustomFxGrid.GetRowCount: integer;
begin
  Result := inherited RowCount;
end;

function TCustomFxGrid.GetColCount: integer;
begin
  Result := inherited ColCount
end;

procedure TCustomFxGrid.SetTotals(Value: boolean);
var
  i: Integer;
begin
  if assigned (FDisplayDims) then
  begin
    for i := 0 to FDisplayDims.Count-1 do
    begin
      FDisplayDims[i].subtotals := Value;
    end;
  end;
end;

  { Datalink methods }

procedure TFxGridDataLink.DecisionDataEvent(Event: TDecisionDataEvent);
begin
  if FBlocked then Exit;
  FBlocked := True;
  case Event of
    xeSummaryChanged:
    begin
      if assigned(FGrid) then
        FGrid.Invalidate;
    end;
    xePivot:
    begin
      if assigned(FGrid) then
        FGrid.NewGridLayout;
    end;
    xeNewMetaData:
    begin
      if assigned(FGrid) then
        FGrid.NewDataStructure;
    end;
    xeStateChanged:
    begin
      if assigned(FGrid) then
        FGrid.NewDataStructure;
    end;
    xeSourceChange:
    begin
      FGrid.SetDecisionSource(FDecisionSource);
      FGrid.NewDataStructure;
    end;
  end;
  FBlocked := False;
end;

constructor TFxGridDataLink.Create(AGrid: TCustomFxGrid);
begin
  inherited Create;
  FGrid := AGrid;
end;

destructor TFxGridDataLink.Destroy;
begin
  inherited Destroy;
end;

constructor TDisplayDims.Create(Grid: TCustomFxGrid; ItemClass: TDisplayDimClass);
begin
  inherited Create(ItemClass);
  FGrid := Grid;
  bQuiet := True;
end;

{
  The TDisplayDims class is a collection which is used to keep persistent
  properties on a dimension by dimension basis.  The collection class handles
  persistence enables the collection editor, and communicates changes in the
  collection members to the owner of the collection (in this case a grid).
}

function TDisplayDims.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

function TDisplayDims.GetDisplayDim(Index: Integer): TDisplayDim;
begin
  Result := TDisplayDim(inherited Items[Index]);
end;

{
  Call the correct owner function to service changes in the
  collection properties, depending on the type of change
}

procedure TDisplayDims.NotifyOwner(aType: TDDNotifyType);
begin
  if bQuiet then Exit;
  if not (csLoading in FGrid.ComponentState) then
    case aType of
    tdDisplay: if assigned(FGrid) then
                 FGrid.NewGridLayout;
    tdMetaData:	if assigned(FGrid) then
                  FGrid.NewDataStructure;
    tdSubTotals: if assigned (FGrid) then
                   FGrid.InitializeGridCells;
    end;
end;

procedure TDisplayDims.SetDisplayDim(Index: Integer; Value: TDisplayDim);
begin
  Items[Index].Assign(Value);
end;

{
  TDisplayDim is the collection item allocated one for each dimension.  Its
  job is to allow property settings per dimension and notify the collection
  when properties change.
}

constructor TDisplayDim.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FOwner := Collection;
  FName := '';
  FColor := clNone;
  FSubs := True;
  FAlignment := taCenter;
  FFormat := '';
  FWidth:=20;
end;

procedure TDisplayDim.assign(Value: TPersistent);
begin
  if (Value is TDisplayDim) then
  begin
    FName := TDisplayDim(Value).FName;
    FColor:= TDisplayDim(Value).FColor;
    FSubs := TDisplayDim(Value).FSubs;
    FAlignment:= TDisplayDim(Value).FAlignment;
    FFormat:= TDisplayDim(Value).FFormat;
    FWidth := TDisplayDim(Value).FWidth;
  end;
end;

procedure TDisplayDim.SetName(Value: string);
begin
  fName := Value;
  NotifyCollection(tdDisplay);
end;

procedure TDisplayDim.SetFieldName(Value: string);
begin
  if (not assigned(FOwner)) or (not assigned(TDisplayDims(FOwner).FGrid)) then
    Exit;
  if (csLoading in TDisplayDims(FOwner).FGrid.ComponentState) then
  begin
    fFieldName := Value;
    NotifyCollection(tdMetaData);
  end;
end;

procedure TDisplayDim.SetFormat(Value: String);
begin
  FFormat := Value;
  NotifyCollection(tdMetaData);
end;

procedure TDisplayDim.SetAlignment(Value: TAlignment);
begin
  fAlignment := Value;
  NotifyCollection(tdDisplay);
end;

procedure TDisplayDim.SetColor(Value: TColor);
begin
  fColor := Value;
  NotifyCollection(tdDisplay);
end;

procedure TDisplayDim.SetSubs(Value: Boolean);
begin
  fSubs := Value;
  NotifyCollection(tdMetaData);
end;

procedure TDisplayDim.SetWidth(Value:Integer);
begin
  FWidth:=Value;
  NotifyCollection(tdDisplay);
end;

procedure TDisplayDim.NotifyCollection(aType: TDDNotifyType);
begin
  TDisplayDims(FOwner).NotifyOwner(aType);
end;

function TCustomFxGrid.ConvertWidth(Value: Integer): Integer;
begin
  Result:=(Canvas.TextWidth('W0') div 2)*Value;
end;

procedure TCustomFxGrid.SetLabelSumFont(const Value: TFont);
begin
  FLabelSumFont.Assign(Value);
  Invalidate;
end;

initialization
  StartClassGroup(TControl);
  GroupDescendentsWith(TFxGrid, TControl);
  RegisterClass(TFxGrid);
end.


