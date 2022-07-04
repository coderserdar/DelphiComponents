{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{                                                       }
{       Copyright (c) 1997,99 Inprise Corporation       }
{                                                       }
{*******************************************************}

unit FxPivSrc;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Grids, Buttons, Controls, StdCtrls,
  Forms, Dialogs, DB, Menus, ExtCtrls,
  FxConsts, FxDB, FxButton, FxStore, FxCommon;

type
  TDecisionButtonPosition = (xtHorizontal, xtVertical, xtLeftTop);

  TDecisionPivotOption = (xtRows, xtColumns, xtSummaries);
  TDecisionPivotOptions = set of TDecisionPivotOption;

  TFxPivot = class;
  TPivotDataLink = class(TDecisionDataLink)
  private
    FPivot: TFxPivot;
  protected
    procedure DecisionDataEvent(Event: TDecisionDataEvent); override;
  public
    constructor Create(aPivot: TFxPivot);
    destructor Destroy; override;
  end;

  TFxPivot = class(TCustomPanel)
  private
    FActive: Boolean;
    FDataLink: TPivotDataLink;
    FSource: TFxSource;
    FControls: TList;
    FSummaryBox: TPivotButton;
    FInActiveBox: TPivotButton;
    FRowTarget: TPivotButton;
    FColTarget: TPivotButton;
    FRows: Integer;
    FCols: Integer;
    FPages: Integer;
    FExtras: Integer;
    FAutosize: Boolean;
    FStyle:  TDecisionButtonPosition;
    FContents: TDecisionPivotOptions;
    FSpacing: Integer;
    FControlWidth: Integer;
    FControlHeight: Integer;
    FGroupSpacing: Integer;
    FTargetSize: Integer;
    procedure SetAutoSize(Value: Boolean);reintroduce;
    procedure GetButtonSizes(var CellWidth, CellHeight: Integer);
    procedure SetStyle(Style: TDecisionButtonPosition);
    procedure SetContents(Contents: TDecisionPivotOptions);
    procedure SetSpacing(Value: Integer);
    procedure SetControlWidth(Value: Integer);
    procedure SetControlHeight(Value: Integer);
    procedure SetGroupSpacing(Value: Integer);
    procedure SeTDecisionSource(aSource: TFxSource);
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure SetBorderWidth(Value: TBorderWidth);
    function GetBorderWidth: TBorderWidth;
    procedure SetBorderStyle(Value: TBorderStyle);
    function GetBorderStyle: TBorderStyle;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure NewPanelSetup;
    procedure NewDimLayout;
    procedure DimStateChange(iDim:Integer);
    property Rows: Integer read FRows;
    property Cols: Integer read FCols;
    property Pages: Integer read FPages;
    property Extras: Integer read FExtras;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(Left, Top, Height, Width: Integer); override;
  published
    property ButtonAutoSize: Boolean read FAutoSize write SetAutoSize;
    property DecisionSource: TFxSource read FSource write SeTDecisionSource;
    property GroupLayout: TDecisionButtonPosition read FStyle write SetStyle;
    property Groups: TDecisionPivotOptions read FContents write SetContents;
    property ButtonSpacing: Integer read FSpacing write SetSpacing;
    property ButtonWidth: Integer read FControlWidth write SetControlWidth;
    property ButtonHeight: Integer read FControlHeight write SetControlHeight;
    property GroupSpacing: Integer read FGroupSpacing write SetGroupSpacing;
    property BorderWidth: TBorderWidth read GetBorderWidth write SetBorderWidth;
    property BorderStyle: TBorderStyle read GetBorderStyle write SetBorderStyle;
    { from the base class }
    property Align;
    property Alignment;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Caption;
    property Color;
    property Ctl3D;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
    property OnStartDrag;
  end;

implementation

uses FxCache, FxMap;

constructor TFxPivot.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { Set default properties of the Decision }
  FActive := True;
  FDataLink := TPivotDataLink.Create(Self);
  FDataLink.FPivot := Self;
  FStyle := xtHorizontal;
  FControlWidth := 64;
  FControlHeight :=24;
  FGroupSpacing := 10;
  FContents := [xtRows, xtColumns, xtSummaries];
  FSpacing := 0;
  FAutosize := True;
  BorderWidth := 0;
  BorderStyle := bsNone;
  NewPanelSetup;
  RCS;
end;

destructor TFxPivot.Destroy;
var
  i: integer;
begin
  {
    This shouldn't be necessary since these are all ultimately
    owned by the Decision control
  }
  if assigned(FControls) then begin
    for I := 0 to FControls.count-1 do begin
      TWinControl(FControls[i]).free;
      FControls[i] := nil;
    end;
    FControls.Free;
    FControls := nil;
  end;
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TFxPivot.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent is TPivotButton) and (Operation = opInsert) then begin
    if assigned(DecisionSource) then
      TPivotButton(AComponent).DecisionSource := DecisionSource;
  end;
end;

procedure TFxPivot.SetBounds(Left, Top, Height, Width: Integer);
begin
  inherited SetBounds(Left,Top,Height,Width);
  NewPanelSetup;
end;

var
  Freeing: Boolean = False;
  Lock: Boolean = False;

  { Called to set up a whole new layout or change the style }

procedure TFxPivot.NewPanelSetup;
var
  I: Integer;
  aDimInfo: PDimInfo;
  aButton: TPivotButton;
  DM: TFxMapItem;
begin
  if Lock then Exit;
  if (not assigned(DecisionSource)) or not(DecisionSource.Ready) then begin
//   inherited Caption := inherited Name;
    if assigned(FControls) then begin
      DisableAlign;
      try
        for I := 0 to FControls.count-1 do begin
          TWinControl(FControls[i]).free;
          FControls[i] := nil;
        end;
      finally
        EnableAlign;
      end;
    end;

    if Freeing then Exit;
    Freeing := True;
    FControls.Free;
    FControls := nil;
    FRowTarget.free;
    FRowTarget := nil;
    FColTarget.free;
    FColTarget := nil;
    FSummaryBox.free;
    FSummaryBox := nil;
    FInactiveBox.free;
    FInactiveBox := nil;
    FRows := 0;
    FCols := 0;
    FPages := 0;
    Freeing := False;
    Exit;
  end;
  { Decision source available, so initialize }
  inherited Caption := '';
  FRows := DecisionSource.GetGroupCOunt(dgRow, false);
  FCols := DecisionSource.GetGroupCount(dgCol, false);
  FPages:= DecisionSource.GetGroupCount(dgPage, false);
  if assigned(FControls) then begin
    if (FControls.count <> (Pages + Rows + Cols)) then begin
      for I:= 0 to FControls.count-1 do begin
        TWinControl(FControls[i]).free;
        FControls[i] := nil;
      end;
      FControls.Free;
      FControls := nil;
    end;
  end;

  if not assigned(FControls) then FControls := TList.Create;
  while (FControls.count < (Pages + Rows + Cols)) do
    FControls.Insert(FControls.count, nil);
  { create the summary drop down. }
  if not assigned(FRowTarget) then begin
    FRowTarget:= TPivotButton.Create(self);
    FRowTarget.Parent := self;
    FRowTarget.iDim := -1;
    FRowTarget.SetType(pbTarget);
    FTargetSize:=ButtonHeight;
    aDimInfo:= @(FRowTarget.DimInfo);
    aDimInfo.iGroup := dgRow;
  end;
  if not assigned(FColTarget) then begin
    FColTarget := TPivotButton.Create(self);
    FColTarget.Parent := self;
    FColTarget.iDim := -1;
    FColTarget.SetType(pbTarget);
    aDimInfo := @(FColTarget.DimInfo);
    aDimInfo.iGroup := dgCol;
  end;
  if xtSummaries in FContents then begin
    if not assigned(FSummaryBox) then begin
      Lock := True;
      aButton := TPivotButton.Create(self);
      aButton.parent := self;
      aButton.iDim := -1;
      aButton.SetType(pbSummary);
      aButton.Caption := '';
      FSummaryBox := aButton;
      Lock := False;
    end;
    FExtras := 1;
    for i := 0 to DecisionSource.DecisionCube.DimensionMap.count-1 do begin
      DM := DecisionSource.DecisionCube.DimensionMap[i];
      if (not DM.Active) and (DM.ActiveFlag <> diInActive) then begin
        FExtras := 2;
        break;
      end;
    end;
    if FExtras=2 then begin
      if not assigned(FInActiveBox) then begin
        aButton := TPivotButton.Create(self);
        aButton.parent := self;
        aButton.iDim := -1;
        aButton.SetType(pbInactive);
        aButton.Caption := '';
        FInActiveBox := aButton;
      end;
    end else begin
      FInactiveBox.free;
      FInactiveBox := nil;
    end;
  end else begin
    Lock := True;
    FSummaryBox.Free;
    FSummaryBox := nil;
    FInactiveBox.free;
    FInactiveBox := nil;
    Lock := False;
  end;
  { now scan the layout and place individual controls }
  NewDimLayout;
end;

{
  This code lays out the controls for the individual dimensions.
  It is called to initialize the controls, then again whenever the
  layout of the controls changes.  Note that it attempts to save existing
  controls where possible, since this code is called for simple pivots as
  well as large scale changes
}

procedure TFxPivot.NewDimLayout;
var
  i: Integer;
  xControl: TWinControl;
  nSums, nRows, nCols, hmargin, vmargin: Integer;
  CellWidth, CellHeight:Integer;
  delta: Integer;
  IDim: Integer;
  RowX, RowDeltaX, RowY, RowDeltaY: Integer;
  ColX, ColDeltaX, ColY, ColDeltaY: Integer;
  SumX, SumDeltaX, SumY, SumDeltaY: Integer;
begin
  if (not assigned(DecisionSource)) or not(DecisionSource.Ready) then Exit;
  hmargin := inherited BorderWidth;
  vmargin := inherited BorderWidth;
  if xtRows in FContents then
    nRows:=Rows
  else
    nRows:=0;
  if xtColumns in FContents then
    nCols:=Cols
  else
    nCols:=0;
  if xtSummaries in FContents then
    nSums:=Pages+Extras
  else
    nSums:=0;
  GetButtonSizes(CellWidth, CellHeight);
  {
    The row and column controls are placed separately.  An initial position X,Y
    increment DeltaX, DeltaY are set up for both row and column
    The controls are then layed out relative to the position of the first.
  }
  case FStyle of
    xtHorizontal:begin
      SumX := hmargin;
      SumY := vmargin;
      SumDeltaX := CellWidth+FSpacing;
      SumDeltaY := 0;
      RowY := vmargin;
      if (xtSummaries in Fcontents) then
        RowX := hMargin + (NSums * (CellWidth + FSpacing)) + FGroupSpacing - FSpacing
      else
        RowX := hmargin;
      RowDeltaX := CellWidth+FSpacing;
      RowDeltaY := 0;
      if (xtRows in FContents) then
        ColX := RowX + GroupSpacing + (RowDeltaX * nRows) + FTargetSize
      else
        ColX := RowX;
      ColDeltaX := RowDeltaX;
      ColY := RowY;
      ColDeltaY := RowDeltaY;
      FRowTarget.Width := FTargetSize;
      FRowTarget.Height := CellHeight;
      FColTarget.Width := FTargetSize;
      FColTarget.Height := CellHeight;
      FRowTarget.Left := RowX;
      FRowTarget.Top := RowY;
      FColTarget.Left := ColX;
      FColTarget.Top := ColY;
      RowX := RowX + FTargetSize + FSpacing;
      ColX := ColX + FTargetSize + FSpacing;
    end;
    xtVertical:begin
      SumX := hmargin;
      SumY := vmargin;
      SumDeltaX := 0;
      SumDeltaY := CellHeight + FSpacing;
      RowX := hmargin;
      if xtSummaries in FContents then
        RowY:= vmargin+(nSums*(CellHeight+FSpacing))+FGroupSpacing-Fspacing
      else
        RowY := vmargin;
      RowDeltaX := 0;
      RowDeltaY := CellHeight + FSpacing;
      ColX := RowX;
      ColDeltaX:=RowDeltaX;
      if xtRows in FContents then
        ColY:= RowY+(nRows*RowDeltaY)+FGroupSpacing+FTargetSize
      else
        ColY:= RowY;
      ColDeltaY := RowDeltaY;
      FRowTarget.Width := CellWidth;
      FRowTarget.Height := FTargetSize;
      FColTarget.Width := CellWidth;
      FColTarget.Height := FTargetSize;
      FRowTarget.Left := RowX;
      FRowTarget.Top := RowY;
      FColTarget.Left := ColX;
      FColTarget.Top := ColY;
      RowY := RowY + FTargetSize + FSpacing;
      ColY := ColY + FTargetSize + FSpacing;
    end;
    else begin
      SumX := hmargin;
      SumY := vmargin;
      SumDeltaX := CellWidth+FSpacing;
      SumDeltaY := 0;
      RowX := hmargin;
      RowDeltaX := 0;
      if xtSummaries in FContents then
        RowY := cellHeight + vmargin
      else
        RowY := vmargin;
      RowDeltaY := FSpacing + CellHeight;
      delta := (Height - (nRows * RowDeltaY) - FTargetSize - RowY - vmargin)div 2;
      if delta>0 then RowY := RowY + delta;
      if xtSummaries in Fcontents then
        ColX := cellWidth + hmargin
      else
        ColX := hmargin;
      ColDeltaX := CellWidth + FSpacing;
      delta := (Width - (nCols * ColDeltaX) - FTargetSize - ColX - hmargin) div 2;
      if delta >= 0 then ColX:=ColX + delta;
      ColY := vmargin;
      ColDeltaY := 0;
      FRowTarget.Width := CellWidth;
      FRowTarget.Height:= FTargetSize;
      FColTarget.Width := FTargetSize;
      FColTarget.Height:= CellHeight;
      FRowTarget.Left  := RowX;
      FRowTarget.Top   := RowY;
      FColTarget.Left  := ColX;
      FColTarget.Top   := ColY;
      RowY := RowY + FTargetSize + FSpacing;
      ColX := ColX + FTargetSize + FSpacing;
    end;
  end;{case}
  if not (xtRows in FContents) then FRowTarget.Left := 10000;
  if not (xtColumns in FContents) then FColTarget.Left := 10000;
  { Scan the rows, creating the necessary control and positioning it }
  if assigned(FSummaryBox) then begin
    TPivotButton(FSummaryBox).SetBounds(SumX, SumY, cellWidth, cellHeight);
    SumX := SumX + SumDeltaX;
    SumY := SumY + SumDeltaY;
  end;
  if assigned(FInactiveBox) then begin
    TPivotButton(FInactiveBox).SetBounds(SumX, SumY, cellWidth, cellHeight);
    SumX := SumX + SumDeltaX;
    SumY := SumY + SumDeltaY;
  end;
  if (Pages > 0) then
    for I := 0 to Pages-1 do begin
      with DecisionSource do
        iDim := Pivot.IndexInfo(dgPage,I,False).Dim;
      DimStateChange(iDim);  { set up the control for this dimension }
      xControl := FControls[iDim];
      xControl.Height := CellHeight;
      xControl.Width := CellWidth;
      xControl.Left := SumX;
      xControl.Top := SumY;
      if not (xtSummaries in FContents) then
        xControl.Left := 10000;
      SumX := SumX + SumDeltaX;
      SumY := SumY + SumDeltaY;
    end;
  if (Rows > 0) then
    for I:= 0 to Rows-1 do begin
      with DecisionSource do
        iDim := Pivot.IndexInfo(dgRow,I,False).Dim;
      DimStateChange(iDim);  { set up the control for this dimension }
      xControl:= FControls[iDim];
      xControl.Height := CellHeight;
      xControl.Width := CellWidth;
      xControl.Left := RowX;
      xControl.Top := RowY;
      if not (xtRows in FContents) then xControl.Left := 10000;
      RowX := RowX + RowDeltaX;
      RowY := RowY + RowDeltaY;
    end;
    { Scan the Columns, creating the necessary control and positioning it }
    if Cols>0 then
      for I := 0 to Cols-1 do begin
        with DecisionSource do
          IDim := Pivot.IndexInfo(dgCol,I,False).Dim;
        DimStateChange(iDim);  { set up the control for this dimension }
        xControl := FControls[iDim];
        xControl.Height := CellHeight;
        xControl.Width := CellWidth;
        xControl.Left := ColX;
        xControl.Top := ColY;
        if not (xtColumns in FContents) then xControl.Left := 10000;
        ColX := ColX + ColDeltaX;
        ColY := ColY + ColDeltaY;
      end;
  { Now set up all the owned components }
  for i:=0 to ControlCount-1 do begin
    if (Controls[i] is TPivotButton) then begin
      TPivotButton(Controls[i]).NewState;
    end;
  end;
end;

procedure TFxPivot.GetButtonSizes(var CellWidth, CellHeight: Integer);
var
  hmargin, vmargin, nRows, nCols: Integer;
  temp: Integer;
  nSummary: Integer;
begin
  hmargin := inherited BorderWidth;
  vmargin := inherited BorderWidth;
  if (xtRows in FContents) then
    nRows := Rows
  else
    nRows := 0;
  if (xtColumns in FContents) then
    nCols := Cols
  else
    nCols := 0;
  if (xtSummaries in FContents) then
    nSummary := Pages + Extras
  else
    nSummary := 0;
  CellWidth := FControlWidth;
  CellHeight := FControlHeight;
  if FAutoSize then begin
    if (FStyle = xtHorizontal) then
    begin
      CellHeight := Height - 2*vmargin;
      CellWidth := Width - 2*hmargin;
      if (xtSummaries in FContents) then
      begin
        if ((nRows + nCols) > 0) then CellWidth := CellWidth - FGroupSpacing;
      end;
      temp := nSummary;
      if (temp > 1) then
        CellWidth := CellWidth - ((temp-1) * FSpacing);
      temp := nRows;
      if (xtRows in FContents) then temp := temp + 1;
      if (temp > 1) then
        CellWidth := CellWidth - ((temp-1) * FSpacing);
      temp := nCols;
      if (xtColumns in FContents) then temp := temp + 1;
      if (temp > 1) then
        CellWidth := CellWidth - ((temp-1) * FSpacing);
      if (xtRows in FContents) then
        CellWidth := CellWidth - FTargetSize;
      if (xtColumns in FContents) then
        CellWidth := CellWidth - FTargetSize;
      if (xtRows in FContents) and (xtColumns in FContents) then
        CellWidth := CellWidth - FGroupSpacing;
      if ((nRows + nCols + nSummary) > 1) then
        CellWidth := CellWidth div (nRows + nCols + nSummary);
    end;
    if (FStyle = xtVertical) then begin
      CellWidth := Width - 2*hmargin;
      { now calculate the height }
      CellHeight := Height - 2*vmargin;
      if (xtSummaries in FContents) then
      begin
        if ((nRows + nCols) > 0) then
          CellHeight := CellHeight - FGroupSpacing;
      end;
      temp := nSummary;
      if (temp > 1) then
        CellHeight := CellHeight - ((temp-1) * FSpacing);
      temp := nRows;
      if (xtRows in FContents) then temp := temp + 1;
      if (temp > 1) then
        CellHeight := CellHeight - ((temp-1) * FSpacing);
      temp := nCols;
      if (xtColumns in FContents) then temp := temp + 1;
      if (temp > 1) then
        CellHeight := CellHeight - ((temp-1) * FSpacing);
      if (xtRows in FContents) then
        CellHeight := CellHeight - FTargetSize;
      if (xtColumns in FContents) then
        CellHeight := CellHeight - FTargetSize;
      if (xtRows in FContents) and (xtColumns in FContents) then
        CellHeight := CellHeight - FGroupSpacing;
      if ((nRows + nCols + nSummary) > 1) then
        CellHeight := CellHeight div (nRows + nCols + nSummary);
    end;
  end;
end;

{
  This code is used to create and maintain a control for a single dimension
  Note that once created, the control is kept around if it is still of
  the appropriate type.
}

procedure TFxPivot.DimStateChange(iDim: Integer);
var
  xControl: TWinControl;
  xButton: TPivotButton;
  aString: String;
begin
  if not DecisionSource.ready then Exit;
  xControl := FControls[iDim];  { get any old control for this dim. }
  if (xControl = nil) or (xControl.Classname <> 'TPivotButton') then
  begin
    if assigned(xControl) then aString := xControl.Classname;
    xButton := TPivotButton.Create(self);
    xButton.parent := self;
    xButton.AllowAllUp := true;
    xButton.GroupIndex := iDim + 100;
    if assigned(xControl) then
      xButton.setBounds(xControl.Left,xControl.Top,xControl.Width,xControl.Height);
    xControl.Free;
    xControl := TWinControl(xButton);
  end;
  xButton := TPivotButton(XControl);
  xButton.iDim := iDim;
  FControls[iDim] := xControl;  { Keep the control in the control list } 																// to manage the control palette
end;

{
  Handle exposed properties
}
procedure TFxPivot.SeTDecisionSource(aSource: TFxSource);
begin
  { hook the change events of the Decision grid }
  FSource := aSource;
  FDataLink.DecisionSource := aSource;
  if (FSource <> nil) then
  begin
    NewPanelSetup;
  end;
end;

procedure TFxPivot.SetStyle(Style: TDecisionButtonPosition);
begin
  FStyle := Style;
  NewPanelSetup;
end;

procedure TFxPivot.SetContents(Contents: TDecisionPivotOptions);
begin
  FContents := Contents;
  NewPanelSetup;
end;

procedure TFxPivot.SetAutoSize(Value: boolean);

begin
  FAutosize := Value;
  NewPanelSetup;
end;

procedure TFxPivot.SetSpacing(Value: Integer);
begin
  FSpacing := Value;
  NewPanelSetup;
end;

procedure TFxPivot.SetControlWidth(Value: Integer);
begin
  FControlWidth := Value;
  NewPanelSetup;
end;

procedure TFxPivot.SetControlHeight(Value: Integer);
begin
  FControlHeight := Value;
  NewPanelSetup;
end;

procedure TFxPivot.SetGroupSpacing(Value: Integer);
begin
  FGroupSpacing := Value;
  NewPanelSetup;
end;

function TFxPivot.GetBorderWidth: TBorderWidth;
begin
  Result := inherited BorderWidth;
end;

function TFxPivot.GetBorderStyle: TBorderStyle;
begin
  Result := inherited BorderStyle;
end;

procedure TFxPivot.AdjustClientRect(var Rect: TRect);
begin
  case FStyle of
    xtLeftTop:
    begin
      Rect.Left := ButtonWidth + BorderWidth;
      Rect.Top := ButtonHeight + BorderWidth;
    end;
    xtVertical:
    begin
      Rect.Left := ButtonWidth + BorderWidth;
    end;
    xtHorizontal:
    begin
      Rect.Top := ButtonHeight + BorderWidth;
    end;
  end;
  inherited AdjustClientRect(Rect);
end;

procedure TFxPivot.SetBorderWidth(Value: TBorderWidth);
begin
  inherited BorderWidth := Value;
  NewPanelSetup;
end;

procedure TFxPivot.SetBorderStyle(Value: TBorderStyle);
begin
  inherited BorderStyle := Value;
  NewPanelSetup;
end;

  { Event handlers for other objects external events }

  { Datalink Methods }

procedure TPivotDataLink.DecisionDataEvent(Event: TDecisionDataEvent);
begin
  if not assigned(fPivot) then Exit;
  if FBlocked then Exit;
  FBlocked := True;
  case Event of
    xeSummaryChanged: FPivot.Invalidate;
    xePivot:          FPivot.NewPanelSetup;
    xeStateChanged:   FPivot.NewPanelSetup;
    xeNewMetaData:    FPivot.NewPanelSetup;
    xeSourceChange:begin
      FPivot.SetDecisionSource(FDecisionSource);
      FPivot.NewPanelSetup;
    end;
  end;
  FBlocked := False;
end;

constructor  TPivotDataLink.Create(aPivot: TFxPivot);
begin
  FPivot := APivot;
end;

destructor TPivotDataLink.Destroy;
begin
  inherited Destroy;
end;

end.

