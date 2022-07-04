{*******************************************************}
{                                                       }
{              CA SweetDrawing Library                  }
{                                                       }
{  Copyright (c) 1996,2004 CodeAccelerate Corporation   }
{                                                       }
{*******************************************************}

unit SCDrawingCommons;

{$I SweetDrawing.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Math, SCDECommon, SCDEConsts;

type
  TDoublePoint = record
    x: Double;
    y: Double;
  end;
  PDoublePoint = ^TDoublePoint;

  TDoubleRect = record
    case Integer of
      0: (Left, Top, Right, Bottom: Double);
      1: (TopLeft, BottomRight: TDoublePoint);
  end;
  PDoubleRect = ^TDoubleRect;

  TPointArray = array of TPoint;
  PPointArray = ^TPointArray;

  TDPointArray = array of TDoublePoint;
  PDPointArray = ^TDPointArray;

  TSCDeSelectionType = (scdsHottrack, scdsSelected, scdsMoving, scdsSizing);

  TSCShapeType = (sctyRectangle, sctyPolyPoints, sctyFreeHand);

  TSCDeShapeStyle = (scssIsContainer, scssAcceptsPoint, scssCanRemovePoint,
    scssSingleClickCreation, scssCanRotate, scssCanHottrack, scssUsesCaption,
    scssUsesLabel, scssUsesPointControls, scssUsesControls, scssUsesPicture,
    scssUsesBrush, scssUsesPen, scssUsesFont, scssUsesGradient);

  TSCDeShapeStyles = set of TSCDeShapeStyle;

  TSCDeShapeState = (scstCreating, scstLoading, scstDestroying, scstInUndo,
    scstInRedo);

  TSCDeAction = (scacChanging, scacChangingBrush, scacChangingPen,
    scacChangingFont, scacChangingPicture, scacChangingGradient, scacSizing,
    scacClearing, scacInserting, scacInserted, scacRemoving, scacRemoved,
    scacGrouping, scacPacking, scacGrouped, scacPacked, scacUngrouping,
    scacUnpacking, scacSendingBack, scacSendingBackward, scacBringingFront,
    scacBringingForward);

  TSCDeShapeHitState = (schsNone, schsHot, schsSelected);

  TSCDeHitType = (schtNone, schtGuide, schtLayer, schtRulerHorz,
    schtRulerVert, schtShape, schtPoint, schtControl, schtPointControl);

  TSCDeShapePart = (scspNone, scspShape, scspBorder, scspPoint,
    scspControl, scspPointControl);

  TSCDeLayerTransparency = (scltNone, scltChecked, scltFull);

  TSCDeGridType = (scgtNone, scgtDot, scgtLine);

  TSCDePointType = (scptNone, scptPoint, scptPointControl, scptControl);

  TSCDeSizeRotation = (scsrSouthEast, scsrSouthWest, scsrNorthWest, scsrNorthEast);

  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array[0..1024] of TRGBQuad;

  TSCDeGradientColors = array[0..255] of TRGBQuad;

  TSCDeGradientShift = -100..100;
  TSCDeGradientRotation = -100..100;

  TSCDeShapeGradientStyle = (scdgsNone, scdgsRadialC, scdgsRadialT,
    scdgsRadialB, scdgsRadialL, scdgsRadialR, scdgsRadialTL, scdgsRadialTR,
    scdgsRadialBL, scdgsRadialBR, scdgsLinearH, scdgsLinearV, scdgsReflectedH,
    scdgsReflectedV, scdgsDiagonalLF, scdgsDiagonalLB, scdgsDiagonalRF,
    scdgsDiagonalRB, scdgsArrowL, scdgsArrowR, scdgsArrowU, scdgsArrowD,
    scdgsDiamond, scdgsButterfly);

  TSCDeCustomGradientEvent = procedure(Sender: TObject;
    const Colors: TSCDeGradientColors; Pattern: TBitmap) of object;

  TSCDeShapeHit = record
    Test: TDoublePoint;
    Part: TSCDeShapePart;
    PartIndex: Integer;
    SubIndex: Integer;
  end;

  TSCDePenRec = record
    Color: TColor;
    Mode: TPenMode;
    Style: TPenStyle;
    Width: Integer;
  end;
  PSCDePenRec = ^TSCDePenRec;

  TSCDeBrushRec = record
    Color: TColor;
    Style: TBrushStyle;
  end;
  PSCDeBrushRec = ^TSCDeBrushRec;

  TSCDeShapeGradienthRec = record
    ColorBegin: TColor;
    ColorEnd: TColor;
    Reverse: Boolean;
    Rotation: TSCDeGradientRotation;
    Shift: TSCDeGradientShift;
    Style: TSCDeShapeGradientStyle;
  end;
  PSCDeShapeGradienthRec = ^TSCDeShapeGradienthRec;

  TSCDeShapeFontRec = record
    Charset: TFontCharset;
    Color: TColor;
    Name: TFontName;
    Size: Integer;
    Style: TFontStyles;
  end;
  PSCDeShapeFontRec = ^TSCDeShapeFontRec;

  TSCDeEditOption = (scdoHottrack, scdoCanUndo, scdoCanSelect,
    scdoMultiSelect, scdoKeyActions, scdoMouseActions, scdoDblClickEndsCreation,
    scdoRightClickEndsCreation, scdoShowCursorGuide, scdoShowGuides,
    scdoSnapToGrid);

  TSCDeEditOptions = set of TSCDeEditOption;

  TSCDeGuideType = (scgtHorizontal, scgtVertical);

  TSCDeEditState = (scesNone, scesRectSelect, scesZoomRect, scesMoving, scesSizing,
    scesRotating, scesNewGuide, scesDragGuide, scesToolEdit, scesCreateNew, 
    scesAddPoint, scesRemovePoint);

    
  TSCDeShiftKey = (scskShift, scskAlt, scskCtrl);
  TSCDeShiftKeys = set of TSCDeShiftKey;

  TSCDeRulerType = (scdrHorizontal, scdrVertical);
  TSCDeRulerMode = (scrmPixel, scrmCm, scrmInch);

  TSCDeLineOrientation = (scloPoint, scloHorizontal, scloVertical);

  TSCDeUtils = class
  protected
    class function  DotProdInt2D(const A, B: TDoublePoint): Double;
    class function  VecMinInt2D(const A, B: TDoublePoint): TDoublePoint;
    class function  NormSquaredInt2D(const A: TDoublePoint): Double;
    class function  DistSquaredInt2D(const A, B: TDoublePoint): Double;
    class procedure SimplifyInt2D(var Tol2: Double;
      const Original: array of TDoublePoint; var Marker: array of Boolean;
      J, K: Integer);
  public
    class function  Point(const P: TDoublePoint): TPoint; overload;
    class function  Point(const P: TPoint): TDoublePoint; overload;
    class function  Point(AX, AY: Double): TDoublePoint; overload;
    class function  Point(AX, AY: Integer): TDoublePoint; overload;

    class function  Rect(const R: TDoubleRect): TRect; overload;
    class function  Rect(const R: TRect): TDoubleRect; overload;
    class function  Rect(ALeft, ATop, ARight, ABottom: Double): TDoubleRect; overload;

    class function  DoublesToPoints(const Ps: TDPointArray): TPointArray;
    class function  PointsToDoubles(const Ps: TPointArray): TDPointArray;
    class function  OffsetPoints(const Ps: TDPointArray; X, Y: Double): TDPointArray;

    class function  Round(D: Double): Int64;
    class function  EqualPoint(P1, P2: TDoublePoint): Boolean;
    class function  EmptyPoint: TDoublePoint;

    class procedure OffsetRect(var R: TDoubleRect; X, Y: Double);
    class procedure InflateRect(var R: TDoubleRect; X, Y: Double);
    class function  IsRectEmpty(R: TDoubleRect): Boolean;
    class function  IsRectInRect(InnerRect, OuterRect: TDoubleRect): Boolean;
    class function  PtInRect(R: TDoubleRect; P: TDoublePoint): Boolean;
    class function  EqualRect(const R1, R2: TDoubleRect): Boolean;
    class function  IntersectRect(out Dst: TDoubleRect;
      const R1, R2: TDoubleRect): Boolean; overload;
    class function  IntersectRect(out Dst: TRect; const R1, R2: TRect): Boolean; overload;
    class function  EmptyRect: TDoubleRect;

    class procedure ZoomPoint(var P: TDoublePoint; Zoom: Double);
    class procedure ZoomRect(var R: TDoubleRect; Zoom: Double);

    class procedure Swap(var I1, I2: Integer); overload;
    class procedure Swap(var D1, D2: Double); overload;

    class function  SubtractPoints(const PointA, PointB: TDoublePoint): TDoublePoint;
    class procedure CalcLineParameters(const PointA, PointB: TDoublePoint;
      var Slope, Intercept: Double; var LineOrientation: TSCDeLineOrientation);
    class function  NearLine(const P, P1, P2: TDoublePoint; Fuzz: Double): Boolean;

    class function  DoLinesIntersect(P1, P2, P3, P4: TDoublePoint;
      out POut: TDoublePoint): Boolean;
    class function  DoLineIntersectRect(P1, P2: TDoublePoint; R: TDoubleRect): Boolean;

    class function  GetEllipsePoints(const Center: TDoublePoint; XRadius, YRadius: Double;
      StartAngle: Double = 0; EndAngle: Double = 360): TDPointArray;

    class function  PtInPolygon(const Pts: TDPointArray; P: TDoublePoint): Boolean; overload;
    class function  PtInEllipse(const P: TDoublePoint; R: TDoubleRect): Boolean;

    class function  FixToDouble(AFix: TFixed): Double;

    class function  ShiftToScShift(Shift: TShiftState): TSCDeShiftKeys;
    class function  ScShiftToShift(Shift: TSCDeShiftKeys): TShiftState;

    class function  PolySimplifyInt2D(Tolerance: Double;
      const Original: array of TDoublePoint; var Simple: array of TDoublePoint): Integer;

    class function  GenerateGlobalUniqueID: DWord;

    class function  AngleOfPoint(Pt: TDoublePoint): Double;
    class function  AngleOfPoints(Pt1, Pt2: TDoublePoint): Double;
  end;

  TSCDeGraphicUtils = class
    class procedure Line(Canvas: TCanvas; const P1, P2: TPoint);
    class procedure LineTo(Canvas: TCanvas; const P1, P2: TPoint);
    class procedure Rectangle(Canvas: TCanvas; const R: TRect;
      Fill: Boolean = False);
  end;

  
const
  DeAllEditOptions = [scdoHottrack .. scdoSnapToGrid];
  DeDefaultEditOptions = [scdoHottrack .. scdoSnapToGrid] - [scdoShowCursorGuide];
  DeMouseOptions = [scdoCanSelect, scdoMouseActions];

  BlockedEditActions = [scesZoomRect, scesToolEdit, scesCreateNew];
  ContinuousEditActions = [scesToolEdit, scesCreateNew, scesZoomRect,
    scesAddPoint, scesRemovePoint];

  DeMouseActions = [scesSizing, scesMoving, scesRectSelect, scesZoomRect,
    scesAddPoint, scesRemovePoint, scesCreateNew, scesRotating, scesNewGuide,
    scesDragGuide, scesToolEdit];

implementation

type
  PGUID = ^TGUID;
  TGUID = record
    D1: DWord;
    D2: Word;
    D3: Word;
    D4: array[0..7] of Byte;
  end;
  TCLSID = TGUID;

  T128Bit = array [0..3] of DWORD;

{$EXTERNALSYM CoCreateGuid}
function CoCreateGuid(var guid: TGUID): HResult; stdcall; external 'ole32.dll' name 'CoCreateGuid';

{ TSCDeUtils }

class function TSCDeUtils.DoublesToPoints(const Ps: TDPointArray): TPointArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Ps));

  for I := 0 to Length(Result) - 1 do
  begin
    Result[I].x := TSCDeUtils.Round(Ps[I].x);
    Result[I].y := TSCDeUtils.Round(Ps[I].y);
  end;
end;

class function TSCDeUtils.Point(const P: TDoublePoint): TPoint;
begin
  Result.x := TSCDeUtils.Round(P.x);
  Result.y := TSCDeUtils.Round(P.y);
end;

class function TSCDeUtils.Rect(const R: TDoubleRect): TRect;
begin
  Result.Left   := TSCDeUtils.Round(R.Left);
  Result.Top    := TSCDeUtils.Round(R.Top);
  Result.Right  := TSCDeUtils.Round(R.Right);
  Result.Bottom := TSCDeUtils.Round(R.Bottom);
end;

class function TSCDeUtils.EmptyRect: TDoubleRect;
begin
  FillChar(Result, SizeOf(TDoubleRect), 0);
end;

class function TSCDeUtils.EqualPoint(P1, P2: TDoublePoint): Boolean;
begin
  Result := (P1.x = P2.x) and (P1.y = P2.y);
end;

class function TSCDeUtils.EqualRect(const R1, R2: TDoubleRect): Boolean;
begin
  Result := CompareMem(@R1, @R2, SizeOf(TDoubleRect));
end;

class procedure TSCDeUtils.InflateRect(var R: TDoubleRect; X,
  Y: Double);
begin
  R.Left   := R.Left   - X;
  R.Top    := R.Top    - Y;
  R.Right  := R.Right  + X;
  R.Bottom := R.Bottom + Y;
end;

class function TSCDeUtils.IntersectRect(out Dst: TDoubleRect; const R1,
  R2: TDoubleRect): Boolean;
begin
  Dst.Left   := Max(R1.Left,   R2.Left);
  Dst.Right  := Min(R1.Right,  R2.Right);
  Dst.Top    := Max(R1.Top,    R2.Top);
  Dst.Bottom := Min(R1.Bottom, R2.Bottom);

  Result := (Dst.Right >= Dst.Left) and (Dst.Bottom >= Dst.Top);
  if not Result then Dst := EmptyRect;
end;

class function TSCDeUtils.IsRectEmpty(R: TDoubleRect): Boolean;
begin
  Result := (R.Right <= R.Left) or (R.Bottom <= R.Top);
end;

class function TSCDeUtils.IsRectInRect(InnerRect, OuterRect: TDoubleRect): Boolean;
begin
  Result := (InnerRect.Left >= OuterRect.Left) and
    (InnerRect.Right <= OuterRect.Right) and
    (InnerRect.Top >= OuterRect.Top) and
    (InnerRect.Bottom <= OuterRect.Bottom);
end;

class function TSCDeUtils.OffsetPoints(const Ps: TDPointArray;
  X, Y: Double): TDPointArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Ps));

  for I := 0 to Length(Result) - 1 do
  begin
    Result[I].x := Ps[I].x + X;
    Result[I].y := Ps[I].y + Y;
  end;
end;

class procedure TSCDeUtils.OffsetRect(var R: TDoubleRect; X,
  Y: Double);
begin
  R.Left   := R.Left   + X;
  R.Right  := R.Right  + X;
  R.Top    := R.Top    + Y;
  R.Bottom := R.Bottom + Y;
end;

class function TSCDeUtils.Point(AX, AY: Double): TDoublePoint;
begin
  with Result do
  begin
    X := AX;
    Y := AY;
  end;
end;

class function TSCDeUtils.PointsToDoubles(const Ps: TPointArray): TDPointArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Ps));

  for I := 0 to Length(Result) - 1 do
  begin
    Result[I].x := Ps[I].x;
    Result[I].y := Ps[I].y;
  end;
end;

class function TSCDeUtils.Point(const P: TPoint): TDoublePoint;
begin
  Result.x := P.x;
  Result.y := P.y;
end;

class function TSCDeUtils.PtInRect(R: TDoubleRect;
  P: TDoublePoint): Boolean;
begin
  Result := not TSCDeUtils.IsRectEmpty(R) and
    (P.x >= R.Left) and (P.x < R.Right) and
    (P.y >= R.Top) and (P.y < R.Bottom);
end;

class function TSCDeUtils.Rect(ALeft, ATop, ARight,
  ABottom: Double): TDoubleRect;
begin
  with Result do
  begin
    Left   := ALeft;
    Top    := ATop;
    Right  := ARight;
    Bottom := ABottom;
  end;
end;

class function TSCDeUtils.Rect(const R: TRect): TDoubleRect;
begin
  with Result do
  begin
    Left   := R.Left;
    Top    := R.Top;
    Right  := R.Right;
    Bottom := R.Bottom;
  end;
end;

class function TSCDeUtils.Round(D: Double): Int64;
var
  F: Double;
begin
  F := Frac(D);
  Result := Floor(D);
  if Abs(F) >= 0.5 then Inc(Result);
end;

class procedure TSCDeUtils.Swap(var D1, D2: Double);
var
  Temp: Double;
begin
  Temp := D1;
  D1 := D2;
  D2 := Temp;
end;

class procedure TSCDeUtils.Swap(var I1, I2: Integer);
var
  Temp: Integer;
begin
  Temp := I1;
  I1 := I2;
  I2 := Temp;
end;

class procedure TSCDeUtils.ZoomPoint(var P: TDoublePoint;
  Zoom: Double);
begin
  P.x := Zoom * P.x;
  P.y := Zoom * P.y;
end;

class procedure TSCDeUtils.ZoomRect(var R: TDoubleRect;
  Zoom: Double);
begin
  R.Left   := Zoom * R.Left;
  R.Top    := Zoom * R.Top;
  R.Right  := Zoom * R.Right;
  R.Bottom := Zoom * R.Bottom;
end;

class procedure TSCDeUtils.CalcLineParameters(const PointA,
  PointB: TDoublePoint; var Slope, Intercept: Double;
  var LineOrientation: TSCDeLineOrientation);
var
  Delta: TDoublePoint;
begin
  Delta := SubtractPoints(PointB, PointA);

  if (Delta.X = 0) and (Delta.Y = 0) then
  begin
    // This special CASE should never happen if iMinPixels > 0
    LineOrientation := scloPoint;
    Slope     := 0.0;
    Intercept := 0.0
  end else
  begin
    if Abs(Delta.X) >= Abs(Delta.Y) then
    begin
      // The line is more horizontal than vertical.  Determine values FOR
      // equation:  Y = slope*X + intercept

      LineOrientation := scloHorizontal;
      try
        Slope := 0.0;
        if Delta.X <> 0 then Slope := Delta.Y / Delta.X; {conventional slope in geometry}
      except
        Slope := 0.0
      end;
      
      Intercept := PointA.Y - PointA.X*Slope
    end else
    begin
      // The line is more vertical than horizontal.  Determine values FOR
      // equation:  X = slope*Y + intercept

      LineOrientation := scloVertical;
      try
        Slope := 0.0;
        if Delta.Y <> 0 then Slope := Delta.X / Delta.Y; {reciprocal of conventional slope}
      except
        Slope := 0.0
      end;

      Intercept := PointA.X - PointA.Y*Slope;
    end;
  end;
end;

class function TSCDeUtils.DoLineIntersectRect(P1, P2: TDoublePoint;
  R: TDoubleRect): Boolean;
var
  P3, P4, POut: TDoublePoint;
begin
  Result := False;
  if (R.Top <> R.Bottom) or (R.Left <> R.Right) then
  begin
    Result := TSCDeUtils.PtInRect(R, P1);
    if Result then Exit;

    Result := TSCDeUtils.PtInRect(R, P2);
    if Result then Exit;

    if R.Top = R.Bottom then
    begin
      P3.x := R.Left;
      P3.y := R.Top;
      P4.x := R.Right;
      P4.y := R.Top;

      Result := DoLinesIntersect(P1, P2, P3, P4, POut);
    end else
    if R.Left = R.Right then
    begin
      P3.x := R.Left;
      P3.y := R.Top;
      P4.x := R.Left;
      P4.y := R.Bottom;

      Result := DoLinesIntersect(P1, P2, P3, P4, POut);
    end else
    begin
      // top line
      P3.x := R.Left;
      P3.y := R.Top;
      P4.x := R.Right;
      P4.y := R.Top;

      Result := DoLinesIntersect(P1, P2, P3, P4, POut);
      if Result then Exit;

      // bottom line
      P3.x := R.Left;
      P3.y := R.Bottom;
      P4.x := R.Right;
      P4.y := R.Bottom;

      Result := DoLinesIntersect(P1, P2, P3, P4, POut);
      if Result then Exit;

      // left line
      P3.x := R.Left;
      P3.y := R.Top;
      P4.x := R.Left;
      P4.y := R.Bottom;

      Result := DoLinesIntersect(P1, P2, P3, P4, POut);
      if Result then Exit;

      // right line
      P3.x := R.Right;
      P3.y := R.Top;
      P4.x := R.Right;
      P4.y := R.Bottom;

      Result := DoLinesIntersect(P1, P2, P3, P4, POut);
    end;
  end;
end;

class function TSCDeUtils.DoLinesIntersect(P1, P2, P3, P4: TDoublePoint;
  out POut: TDoublePoint): Boolean;
var
  r, s, t, b: Double;
begin
  POut.x := 0;
  POut.y := 0;

  Result := False;

  t := ((P1.y - P3.y)*(P4.x - P3.x)) - ((P1.x - P3.x)*(P4.y - P3.y));
  b := ((P2.x - P1.x)*(P4.y - P3.y)) - ((P2.y - P1.y)*(P4.x - P3.x));

  r := -1;
  if b <> 0 then r := t/b;

  if (r >= 0) and (r <= 1) then
  begin
    t := ((P1.y - P3.y)*(P2.x - P1.x)) - ((P1.x - P3.x)*(P2.y - P1.y));
    b := ((P2.x - P1.x)*(P4.y - P3.y)) - ((P2.y - P1.y)*(P4.x - P3.x));

    s := -1;
    if b <> 0 then s := t/b;

    if (s >= 0) and (s <= 1) then
    begin
      Result := True;
      
      POut.x := P1.x + r*(P2.x - P1.x);
      POut.y := P1.y + r*(P2.y - P1.y);
    end;
  end;
end;

class function TSCDeUtils.IntersectRect(out Dst: TRect; const R1,
  R2: TRect): Boolean;
begin
  Dst.Left   := Max(R1.Left,   R2.Left);
  Dst.Right  := Min(R1.Right,  R2.Right);
  Dst.Top    := Max(R1.Top,    R2.Top);
  Dst.Bottom := Min(R1.Bottom, R2.Bottom);

  Result := (Dst.Right >= Dst.Left) and (Dst.Bottom >= Dst.Top);
  if not Result then FillChar(Dst, SizeOf(TRect), 0);
end;

class function TSCDeUtils.NearLine(const P, P1, P2: TDoublePoint; Fuzz: Double): Boolean;
var
  R: TDoubleRect;
  Intercept, Slope: Double;
  LineOrientation: TSCDeLineOrientation;
  maxX, maxY, minX, minY, X, Y: Double;
begin
  Result := False;

  // Pixel "fuzz" used in line selection
  if Fuzz < 0.0 then Fuzz := 0.0;

  minX := MinValue([P1.x, P2.x]);
  maxX := MaxValue([P1.x, P2.x]);

  minY := MinValue([P1.y, P2.y]);
  maxY := MaxValue([P1.y, P2.y]);

  // if Slope = 0 check X
  if P1.x = P2.x then
  begin
    Result := (Abs(P.x - P1.x) <= Fuzz) and
      (P.y >= minY) and (P.y <= maxY);

    if Result then Exit;
  end;

  // if Slope = 0 check Y
  if P1.y = P2.y then
  begin
    Result := (Abs(P.y - P1.y) <= Fuzz) and
      (P.x >= minX) and (P.x <= maxX);

    if Result then Exit;
  end;

  // check in Rect
  if (minX < maxX) and (minY < maxY) then
  begin
    R := TSCDeUtils.Rect(minX, minY, maxX, maxY);
    if Fuzz > 0 then TSCDeUtils.InflateRect(R, Fuzz, Fuzz);

    if not TSCDeUtils.PtInRect(R, P) then
      Exit;
  end;

  // If an Endpoint is not selected, was part of line selected?
  CalcLineParameters(P1, P2, Slope, Intercept, LineOrientation);

  case LineOrientation of
    scloHorizontal:
    begin
      // first check if selection within horizontal range of line
      if (P.x >= minX) and (P.x <= maxX) then
      begin
        // Since X is within range of line, now see if Y value is close
        // enough to the calculated Y value FOR the line to be selected.
        Y := Round( Slope*P.x + Intercept);
        Result := Abs(Y - P.y) <= Fuzz;
      end;
    end;
    scloVertical:
    begin
      // first check if selection within vertical range of line
      if (P.y >= minY) and (P.y <= maxY) then
      begin
        // Since Y is within range of line, now see if X value is close
        // enough to the calculated X value FOR the line to be selected.
        X := Round( Slope*P.y + Intercept );
        Result := Abs(X - P.x) <= Fuzz;
      end;
    end;
    scloPoint:
      // Do nothing -- should not occur
  end;
end;

class function TSCDeUtils.PtInEllipse(const P: TDoublePoint; R: TDoubleRect): Boolean;
var
  Center: TDoublePoint;
  Width, Height: Double;
begin
  Center.x := (R.Left + R.Right) / 2;
  Center.y := (R.Top + R.Bottom) / 2;

  Width  := Sqr(Center.x - R.Left);
  Height := Sqr(Center.y - R.Top);

  Result := (Width <> 0) and (Height <> 0) and
    ((Sqr(1.0*P.x - Center.x) / Width ) + (Sqr(1.0*P.y - Center.y) / Height) <= 1);
end;

class function TSCDeUtils.PtInPolygon(const Pts: TDPointArray;
  P: TDoublePoint): Boolean;
var
  Cnt, I, J: Integer;
begin
  Result := False;

  Cnt := Length(Pts);
  J := Cnt-1;

  for I := 0 to Cnt-1 do
  begin
    if ((Pts[I].y <= P.y) and (P.y < Pts[J].y)) or
      ((Pts[J].y <= P.y) and (P.y < Pts[I].y)) then
    begin
      if (P.x < (Pts[J].x - Pts[I].x) * (P.y - Pts[I].y) /
        (Pts[J].y - Pts[I].y) + Pts[I].x) then
        Result := not Result;
    end;

    J := I;
  end;
end;

class function TSCDeUtils.SubtractPoints(const PointA, PointB: TDoublePoint): TDoublePoint;
begin
  with Result do
  begin
    X := PointA.X - PointB.X;
    Y := PointA.Y - PointB.Y
  end;
end;

class function TSCDeUtils.FixToDouble(AFix: TFixed): Double;
begin
  Result := AFix.fract/65536.0 + AFix.value;
end;

class function TSCDeUtils.Point(AX, AY: Integer): TDoublePoint;
begin
  with Result do
  begin
    X := AX;
    Y := AY;
  end;
end;

class function TSCDeUtils.ScShiftToShift(Shift: TSCDeShiftKeys): TShiftState;
begin
  Result := [];

  if scskShift in Shift then Include(Result, ssShift);
  if scskAlt in Shift then Include(Result, ssAlt);
  if scskCtrl in Shift then Include(Result, ssCtrl);
end;

class function TSCDeUtils.ShiftToScShift(Shift: TShiftState): TSCDeShiftKeys;
begin
  Result := [];

  if ssShift in Shift then Include(Result, scskShift);
  if ssAlt in Shift then Include(Result, scskAlt);
  if ssCtrl in Shift then Include(Result, scskCtrl);
end;

class function TSCDeUtils.EmptyPoint: TDoublePoint;
begin
  FillChar(Result, SizeOf(TDoublePoint), 0);
end;

class function TSCDeUtils.PolySimplifyInt2D(Tolerance: Double;
  const Original: array of TDoublePoint; var Simple: array of TDoublePoint): Integer;
var
  Tol2: Double;
  I, N: Integer;
  Marker: array of Boolean;
begin
  Result := 0;
  if Length(Original) > 1 then
  begin
    Tol2 := Sqr(Tolerance);

    // Create a marker array
    N := Length(Original);
    SetLength(Marker, N);

    // Include first and last point
    Marker[0]     := True;
    Marker[N - 1] := True;

    // Exclude intermediate for now
    for I := 1 to N - 2 do
      Marker[I] := False;

    // Simplify
    TSCDeUtils.SimplifyInt2D(Tol2, Original, Marker, 0, N - 1);

    // Copy to resulting list
    for I := 0 to N - 1 do begin
      if Marker[I] then
      begin
        Simple[Result] := Original[I];
        Inc(Result);
      end;
    end;
  end;
end;

class procedure TSCDeUtils.SimplifyInt2D(var Tol2: Double;
  const Original: array of TDoublePoint; var Marker: array of Boolean; J,
  K: Integer);
// Simplify polyline in OrigList between j and k. Marker[] will be set to True
// for each point that must be included
var
  MaxD2: Double;    // Maximum value squared
  I, MaxI: Integer; // Index at maximum value
  CU, CW, B, DV2: Double;
  P0, P1, PB, U, W: TDoublePoint;
begin
  // Is there anything to simplify?
  if K <= J + 1 then Exit;

  P0 := Original[J];
  P1 := Original[K];
  U  := TSCDeUtils.VecMinInt2D(P1, P0); // Segment vector
  CU := TSCDeUtils.DotProdInt2D(U, U);  // Segment length squared
  MaxD2 := 0;
  MaxI  := 0;

  // Loop through points and detect the one furthest away
  for I := J + 1 to K - 1 do
  begin
    W  := TSCDeUtils.VecMinInt2D(Original[I], P0);
    CW := TSCDeUtils.DotProdInt2D(W, U);

    // Distance of point Orig[i] from segment
    if CW <= 0 then
    begin
      // Before segment
      DV2 := TSCDeUtils.DistSquaredInt2D(Original[I], P0)
    end else
    begin
      if CW > CU then
      begin
        // Past segment
        DV2 := TSCDeUtils.DistSquaredInt2D(Original[I], P1);
      end else
      begin
        // Fraction of the segment
        try
          B := CW / CU;
        except
          B := 0; // in case CU = 0
        end;

        PB.X := TSCDeUtils.Round(P0.X + B * U.X);
        PB.Y := TSCDeUtils.Round(P0.Y + B * U.Y);
        DV2  := TSCDeUtils.DistSquaredInt2D(Original[i], PB);
      end;
    end;

    // test with current max distance squared
    if DV2 > MaxD2 then
    begin
      // Orig[i] is a new max vertex
      MaxI  := i;
      MaxD2 := DV2;
    end;
  end;

  // If the furthest point is outside tolerance we must split
  if MaxD2 > Tol2 then // error is worse than the tolerance
  begin
    // split the polyline at the farthest vertex from S
    Marker[MaxI] := True;  // mark Orig[maxi] for the simplified polyline

    // recursively simplify the two subpolylines at Orig[maxi]
    TSCDeUtils.SimplifyInt2D(Tol2, Original, Marker, J, MaxI); // polyline Orig[j] to Orig[maxi]
    TSCDeUtils.SimplifyInt2D(Tol2, Original, Marker, MaxI, K); // polyline Orig[maxi] to Orig[k]
  end;
end;

class function TSCDeUtils.DistSquaredInt2D(const A, B: TDoublePoint): Double;
// Square of the distance from A to B
begin
  Result := TSCDeUtils.NormSquaredInt2D(TSCDeUtils.VecMinInt2D(A, B));
end;

class function TSCDeUtils.NormSquaredInt2D(const A: TDoublePoint): Double;
// Square of the norm |A|
begin
  Result := A.X * A.X + A.Y * A.Y;
end;

class function TSCDeUtils.VecMinInt2D(const A, B: TDoublePoint): TDoublePoint;
// Result = A - B
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

class function TSCDeUtils.DotProdInt2D(const A, B: TDoublePoint): Double;
// Dotproduct = A * B
begin
  Result := A.X * B.X + A.Y * B.Y;
end;

procedure Mix128(var X : T128Bit);
var
  AA, BB, CC, DD : LongInt;
begin
  AA := X[0];  BB := X[1];  CC := X[2];  DD := X[3];

  AA := AA + DD;  DD := DD + AA;  AA := AA xor (AA shr 7);
  BB := BB + AA;  AA := AA + BB;  BB := BB xor (BB shl 13);
  CC := CC + BB;  BB := BB + CC;  CC := CC xor (CC shr 17);
  DD := DD + CC;  CC := CC + DD;  DD := DD xor (DD shl 9);
  AA := AA + DD;  DD := DD + AA;  AA := AA xor (AA shr 3);
  BB := BB + AA;  AA := AA + BB;  BB := BB xor (BB shl 7);
  CC := CC + BB;  BB := BB + CC;  CC := CC xor (DD shr 15);
  DD := DD + CC;  CC := CC + DD;  DD := DD xor (DD shl 11);

  X[0] := AA;  X[1] := BB;  X[2] := CC;  X[3] := DD;
end;

class function TSCDeUtils.GenerateGlobalUniqueID: DWord;
var
  ID : TGUID;
begin
  CoCreateGuid(ID);
  Mix128(T128Bit(ID));
  Result := T128Bit(ID)[3];
end;

class function TSCDeUtils.AngleOfPoint(Pt: TDoublePoint): Double;
begin
  // Calculate angle
  if Pt.x = 0.0 then
  begin
    if Pt.y = 0.0 then
      Result := 0.0
    else if Pt.y > 0.0 then
      Result := Pi / 2.0
    else
      Result := Pi * 3.0/2.0;
  end else
  if Pt.y = 0.0 then
  begin
    if  Pt.x > 0.0 then
      Result := 0.0
    else
      Result := Pi;
  end else
  begin
    if Pt.x < 0.0 then
      Result := ArcTan(Pt.y/Pt.x) + Pi
    else if Pt.y < 0.0 then
      Result := ArcTan(Pt.y/Pt.x) + (2*Pi)
    else
      Result := ArcTan(Pt.y/Pt.x);
  end;

  // Convert to degrees
  Result := 360 - (Result * 180/Pi);
end;

class function TSCDeUtils.AngleOfPoints(Pt1, Pt2: TDoublePoint): Double;
begin
  Pt2.x := Pt2.x - Pt1.x;
  Pt2.y := Pt2.y - Pt1.y;

  Result := TSCDeUtils.AngleOfPoint(Pt2);
end;

class function TSCDeUtils.GetEllipsePoints(const Center: TDoublePoint;
  XRadius, YRadius, StartAngle, EndAngle: Double): TDPointArray;
var
  TempAng: Double;
  I, Ln, S, E, SA, EA: Integer;
begin
  SetLength(Result, 0);

  SA := TSCDeUtils.Round(StartAngle);
  EA := TSCDeUtils.Round(EndAngle);

  S := SA div 360;
  E := EA div 360;

  SA := SA mod 360;
  EA := EA mod 360;

  if (S > 0) and (SA = 0) then SA := 360;
  if (E > 0) and (EA = 0) then EA := 360;

  if SA > EA then
  begin
    I  := EA;
    EA := SA;
    SA := I;
  end;

  if SA = EA then
  begin
    SA := 0;
    EA := 360;
  end;

  Ln := 0;
  for I := SA To EA do
  begin
    Inc(Ln);
    SetLength(Result, Ln);

    TempAng := I*Pi/180;

    Result[I - 1].x := Center.x + XRadius*Cos(TempAng);
    Result[I - 1].y := Center.y - YRadius*Sin(TempAng);
  end;
end;

{ TSCDeGraphicUtils }

class procedure TSCDeGraphicUtils.Line(Canvas: TCanvas; const P1,
  P2: TPoint);
var
  Pts: array[0..3] of TPoint;
begin
  Pts[0] := P1;
  Pts[3] := P2;

  Pts[1] := Pts[0];
  Pts[2] := Pts[3];

  Canvas.PolyBezier(Pts);
end;

class procedure TSCDeGraphicUtils.LineTo(Canvas: TCanvas; const P1,
  P2: TPoint);
var
  Pts: array[0..2] of TPoint;
begin
  Pts[0] := P1;
  Pts[2] := P2;

  Pts[1] := Pts[0];

  Canvas.PolyBezierTo(Pts);
end;

class procedure TSCDeGraphicUtils.Rectangle(Canvas: TCanvas;
  const R: TRect; Fill: Boolean);
begin
  if Fill then Windows.BeginPath(Canvas.Handle);
  try
    Canvas.MoveTo(R.Left, R.Top);

    with TSCDeGraphicUtils do
    begin
      LineTo(Canvas, Point(R.Left, R.Top), Point(R.Right, R.Top));
      LineTo(Canvas, Point(R.Right, R.Top), Point(R.Right, R.Bottom));
      LineTo(Canvas, Point(R.Right, R.Bottom), Point(R.Left, R.Bottom));
      LineTo(Canvas, Point(R.Left, R.Bottom), Point(R.Left, R.Top));
    end;
  finally
    if Fill then
    begin
      EndPath(Canvas.Handle);
      StrokeAndFillPath(Canvas.Handle);
    end;
  end;
end;

end.
