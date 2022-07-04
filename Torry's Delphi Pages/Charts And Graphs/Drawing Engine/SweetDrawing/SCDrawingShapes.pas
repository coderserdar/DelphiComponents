{*******************************************************}
{                                                       }
{              CA SweetDrawing Library                  }
{                                                       }
{  Copyright (c) 1996,2004 CodeAccelerate Corporation   }
{                                                       }
{*******************************************************}

unit SCDrawingShapes;

{$I SweetDrawing.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Math, SCDECommon, SCDEConsts, SCDrawingCommons, SCDrawingTTF, SCXML,
  SCDrawingSurface;

type
  TSCDeCustomRectangle = class(TSCDeShape)
  private
    FP1: TSCDePoint;
    FP2: TSCDePoint;
    FP3: TSCDePoint;
    FP4: TSCDePoint;
    FP12: TSCDePoint;
    FP23: TSCDePoint;
    FP34: TSCDePoint;
    FP41: TSCDePoint;
    FExtraHandles: Boolean;
    procedure UpdateExtraHandles;
    procedure SetExtraHandles(Value: Boolean);
  protected
    procedure RefreshExtraHandles; dynamic;

    function  GetGravityCenter: TDoublePoint; override;

    procedure PointChanged(P: TSCDePoint); override;
    procedure PointRemoved(P: TSCDePoint); override;

    procedure Add(P: TSCDePoint); overload; override;
    function  Add(x, y: Double): TSCDePoint; overload; override;
    procedure Insert(Index: Integer; P: TSCDePoint); override;
    procedure Delete(Index: Integer); override;
    procedure Remove(P: TSCDePoint); override;
    procedure Extract(P: TSCDePoint); override;

    property ExtraHandles: Boolean read FExtraHandles write SetExtraHandles default False;
  public
    constructor Create(ASurface: TSCDeSurface; AOwner: TSCDeContainer); override;

    procedure Paint(C: TCanvas; X, Y: Integer; Zoom: Double); override;
    procedure DrawSelection(C: TCanvas; X, Y: Double; const DragPoint: TSCDeDragPoint;
      const Data: TSCDeSelectionData; Zoom: Double); override;

    procedure MoveBy(X, Y: Double); override;
    procedure Resize(NewWidth, NewHeight: Double); override;
    function  GetDefaultBounds: TDoubleRect; override;

    procedure LoadBoundsFromXML(Node: TSCDomElement); override;
    procedure SaveBoundsToXML(Node: TSCDomElement); override;

    function  InRect(const R: TDoubleRect): Boolean; override;
    function  PointOnShape(P: TDoublePoint; Fuzz: Double;
      Editing: Boolean = True): Boolean; override;
  end;

  TSCDeRectangle = class(TSCDeCustomRectangle)
  public
    constructor Create(ASurface: TSCDeSurface; AOwner: TSCDeContainer); override;

    procedure LoadFromXML(Node: TSCDomElement); override;
    procedure SaveToXML(Node: TSCDomElement); override;
  published
    property Brush;
    property Pen;
    property Gradient;
    property Picture;
    property PictureStyle;
  end;

  TSCDePicture = class(TSCDeCustomRectangle)
  public
    constructor Create(ASurface: TSCDeSurface; AOwner: TSCDeContainer); override;

    procedure Paint(C: TCanvas; X, Y: Integer; Zoom: Double); override;
    function  PointOnShape(P: TDoublePoint; Fuzz: Double;
      Editing: Boolean = True): Boolean; override;

    procedure LoadFromXML(Node: TSCDomElement); override;
    procedure SaveToXML(Node: TSCDomElement); override;
  published
    property Picture;
    property PictureStyle;
  end;

  TSCDeTextLayout = (sctlTop, sctlCenter, sctlBottom);

  TSCDeLabel = class(TSCDeCustomRectangle)
  private
    FAlignment: TAlignment;
    FLayout: TSCDeTextLayout;
    FWordWrap: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetLayout(Value: TSCDeTextLayout);
    procedure SetWordWrap(Value: Boolean);
  protected
    procedure DoDrawText(C: TCanvas; var R: TRect; Flags: Longint;
      Zoom: Double); dynamic;
  public
    constructor Create(ASurface: TSCDeSurface; AOwner: TSCDeContainer); override;

    procedure Paint(C: TCanvas; X, Y: Integer; Zoom: Double); override;
    function  PointOnShape(P: TDoublePoint; Fuzz: Double;
      Editing: Boolean = True): Boolean; override;

    procedure LoadFromXML(Node: TSCDomElement); override;
    procedure SaveToXML(Node: TSCDomElement); override;
  published
    property Brush;
    property Font;
    property Caption;
    property Gradient;
    property Pen;
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taCenter;
    property Layout: TSCDeTextLayout read FLayout write SetLayout default sctlCenter;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
  end;

  TSCDeCustomEllipse = class(TSCDeCustomRectangle)
  public
    procedure Paint(C: TCanvas; X, Y: Integer; Zoom: Double); override;
    procedure DrawSelection(C: TCanvas; X, Y: Double; const DragPoint: TSCDeDragPoint;
      const Data: TSCDeSelectionData; Zoom: Double); override;

    function  PointOnShape(P: TDoublePoint; Fuzz: Double;
      Editing: Boolean = True): Boolean; override;
    function  IsOnLine(P: TDoublePoint): Boolean; override;
  end;

  TSCDeEllipse = class(TSCDeCustomEllipse)
  public
    constructor Create(ASurface: TSCDeSurface; AOwner: TSCDeContainer); override;

    procedure LoadFromXML(Node: TSCDomElement); override;
    procedure SaveToXML(Node: TSCDomElement); override;
  published
    property Brush;
    property Gradient;
    property Pen;
    property Picture;
    property PictureStyle;
  end;

  TSCDeCircle = class(TSCDeCustomEllipse)
  protected
    procedure PointChanged(P: TSCDePoint); override;
    procedure ArrangeBoundsRect(var R: TDoubleRect); override;
  public
    constructor Create(ASurface: TSCDeSurface; AOwner: TSCDeContainer); override;

    procedure DrawSelection(C: TCanvas; X, Y: Double; const DragPoint: TSCDeDragPoint;
      const Data: TSCDeSelectionData; Zoom: Double); override;

    procedure Resize(NewWidth, NewHeight: Double); override;
    function  PointOnShape(P: TDoublePoint; Fuzz: Double;
      Editing: Boolean = True): Boolean; override;

    procedure LoadFromXML(Node: TSCDomElement); override;
    procedure SaveToXML(Node: TSCDomElement); override;
  published
    property Brush;
    property Gradient;
    property Pen;
    property Picture;
    property PictureStyle;
  end;

  TSCDeArcStyle = (scasArc, scasPie, scasChord);

  TSCDeArc = class(TSCDeCustomEllipse)
  private
    FC1: TSCDeControl;
    FC2: TSCDeControl;
    FStyle: TSCDeArcStyle;
    FArcPoints: TList;
    FStartAngle: Double;
    FEndAngle: Double;
    procedure SetStyle(Value: TSCDeArcStyle);

    procedure ClearArcPoints;
    procedure GenerateArcPoints;
    procedure CalculateAngles(StartC, EndC: Boolean);
    procedure UpdateControlCoords(StartC, EndC: Boolean);
  protected
    procedure UpdateRegion; override;

    procedure PointChanged(P: TSCDePoint); override;
    procedure ControlChanged(C: TSCDeControl); override;
    procedure ControlRemoved(C: TSCDeControl); override;

    property ExtraHandles default False;
  public
    constructor Create(ASurface: TSCDeSurface; AOwner: TSCDeContainer); override;
    destructor Destroy; override;

    procedure Paint(C: TCanvas; X, Y: Integer; Zoom: Double); override;
    procedure DrawSelection(C: TCanvas; X, Y: Double; const DragPoint: TSCDeDragPoint;
      const Data: TSCDeSelectionData; Zoom: Double); override;

    function  InRect(const R: TDoubleRect): Boolean; override;
    function  PointOnShape(P: TDoublePoint; Fuzz: Double;
      Editing: Boolean = True): Boolean; override;
    function  ControlAt(P: TDoublePoint; Fuzz: Double): Integer; override;

    procedure LoadBoundsFromXML(Node: TSCDomElement); override;
    procedure SaveBoundsToXML(Node: TSCDomElement); override;

    procedure LoadFromXML(Node: TSCDomElement); override;
    procedure SaveToXML(Node: TSCDomElement); override;

    property Control1: TSCDeControl read FC1;
    property Control2: TSCDeControl read FC2;
  published
    property Brush;
    property Pen;
    property Style: TSCDeArcStyle read FStyle write SetStyle default scasArc;
  end;

  TSCDeCustomPolyLine = class(TSCDeShape)
  private
    FIsClosed: Boolean;
  protected
    function  GetIsClosed: Boolean; virtual;
    procedure SetIsClosed(Value: Boolean); virtual;

    function  DefaultClosed: Boolean; virtual;
    function  IsClosedStored: Boolean; virtual;

    function GetPointArray: TDPointArray; override;
    property IsClosed: Boolean read GetIsClosed write SetIsClosed
      stored IsClosedStored default False;
  public
    constructor Create(ASurface: TSCDeSurface; AOwner: TSCDeContainer); override;

    function  GetShapeType: TSCShapeType; override;

    procedure Paint(C: TCanvas; X, Y: Integer; Zoom: Double); override;
    procedure DrawSelection(C: TCanvas; X, Y: Double; const DragPoint: TSCDeDragPoint;
      const Data: TSCDeSelectionData; Zoom: Double); override;

    function  InRect(const R: TDoubleRect): Boolean; override;
    function  PointOnShape(P: TDoublePoint; Fuzz: Double;
      Editing: Boolean = True): Boolean; override;
    procedure Resize(NewWidth, NewHeight: Double); override;

    procedure Add(P: TSCDePoint); overload; override;
    function  Add(x, y: Double): TSCDePoint; overload; override;
    procedure Insert(Index: Integer; P: TSCDePoint); overload; override;
    function  ApproxInsert(P: TDoublePoint; Fuzz: Double): TSCDePoint; overload; override;
    procedure Delete(Index: Integer); override;
    procedure Remove(P: TSCDePoint); override;
    procedure Extract(P: TSCDePoint); override;
    procedure Clear; override;
  end;

  TSCDePolyLine = class(TSCDeCustomPolyLine)
  public
    procedure LoadFromXML(Node: TSCDomElement); override;
    procedure SaveToXML(Node: TSCDomElement); override;
  published
    property Pen;
    property IsClosed;
  end;

  TSCDeCustomPolyBezier = class(TSCDeCustomPolyLine)
  private
    FBeziers: TList;
    FSegmentLimit: Byte;
    FBounds: TDoubleRect;
    procedure SetSegmentLimit(Value: Byte);

    function  BezierPoint(U: Double; P0, P1, P2, P3: TDoublePoint): TDoublePoint;
    procedure CalculateBeziers(Pl: TList; P0, P1, P2, P3: TDoublePoint;
      SqLimit, A, B: Double);
  protected
    procedure ClearBeziers(Pl: TList);
    function  RecreateBeziers: TList; dynamic;

    procedure UpdateRegion; override;

    property SegmentLimit: Byte read FSegmentLimit write SetSegmentLimit default 3;
    property Pen;
  public
    constructor Create(ASurface: TSCDeSurface; AOwner: TSCDeContainer); override;
    destructor Destroy; override;

    procedure Paint(C: TCanvas; X, Y: Integer; Zoom: Double); override;
    procedure DrawSelection(C: TCanvas; X, Y: Double; const DragPoint: TSCDeDragPoint;
      const Data: TSCDeSelectionData; Zoom: Double); override;

    function  InRect(const R: TDoubleRect): Boolean; overload; override;
    function  PointOnShape(P: TDoublePoint; Fuzz: Double;
      Editing: Boolean = True): Boolean; override;

    function  GetBounds: TDoubleRect; override;
    procedure RecalculateBounds(Force: Boolean); override;
    procedure Resize(NewWidth, NewHeight: Double); override;

    function  PointControlAt(P: TDoublePoint; Fuzz: Double;
      IsSelected: Boolean; var PointIndex: Integer): Integer; override;

    function  ApproxInsert(P: TDoublePoint; Fuzz: Double): TSCDePoint; overload; override;
  end;

  TSCDePolyBezier = class(TSCDeCustomPolyBezier)
  public
    procedure LoadFromXML(Node: TSCDomElement); override;
    procedure SaveToXML(Node: TSCDomElement); override;
  published
    property Brush;
    property Pen;
    property IsClosed;
  end;

  TSCDeFreeLine = class(TSCDeCustomPolyBezier)
  private
    FPrecision: Double;
  protected
    function  GetIsClosed: Boolean; override;
    function  IsClosedStored: Boolean; override;
    function  DefaultClosed: Boolean; override;

    procedure Refine(Zoom: Double); override;

    property IsClosed default False;
  public
    constructor Create(ASurface: TSCDeSurface; AOwner: TSCDeContainer); override;
    function GetShapeType: TSCShapeType; override;

    procedure LoadFromXML(Node: TSCDomElement); override;
    procedure SaveToXML(Node: TSCDomElement); override;

    property Precision: Double read FPrecision write FPrecision;
  published
    property Brush;
    property Gradient;
    property Pen;
  end;

  TSCDePolygon = class(TSCDeCustomPolyLine)
  protected
    function DefaultClosed: Boolean; override;

    function GetPointArray: TDPointArray; override;
    function CreatePolyRegion(const Points: array of TPoint): HRgn;

    property IsClosed default True;
  public
    constructor Create(ASurface: TSCDeSurface; AOwner: TSCDeContainer); override;

    procedure Paint(C: TCanvas; X, Y: Integer; Zoom: Double); override;
    procedure DrawSelection(C: TCanvas; X, Y: Double; const DragPoint: TSCDeDragPoint;
      const Data: TSCDeSelectionData; Zoom: Double); override;

    function  ApproxInsert(P: TDoublePoint; Fuzz: Double): TSCDePoint; overload; override;

    function  InRect(const R: TDoubleRect): Boolean; override;
    function  PointOnShape(P: TDoublePoint; Fuzz: Double;
      Editing: Boolean = True): Boolean; override;

    procedure LoadFromXML(Node: TSCDomElement); override;
    procedure SaveToXML(Node: TSCDomElement); override;
  published
    property Brush;
    property Gradient;
    property Pen;
    property Picture;
    property PictureStyle;
  end;

  TSCDeCustomPolyPolygon = class(TSCDePackage)
  private
    FCombined: Boolean;
    procedure SetCombined(Value: Boolean);
  protected
    property Combined: Boolean read FCombined write SetCombined default True;
  public
    constructor Create(ASurface: TSCDeSurface; AOwner: TSCDeContainer); override;

    procedure Paint(C: TCanvas; X, Y: Integer; Zoom: Double); override;
    procedure PaintIn(C: TCanvas; X, Y: Integer; Zoom: Double;
      R: TDoubleRect); override;

    procedure MoveBy(X, Y: Double); override;
    procedure Resize(NewWidth, NewHeight: Double); override;
  end;

  TSCDePolyPolygon = class(TSCDeCustomPolyPolygon)
  public
    procedure LoadFromXML(Node: TSCDomElement); override;
    procedure SaveToXML(Node: TSCDomElement); override;
  published
    property Combined;
  end;

  TSCDeChar = class(TSCDeCustomPolyPolygon)
  public
    procedure LoadFromXML(Node: TSCDomElement); override;
    procedure SaveToXML(Node: TSCDomElement); override;
  end;

  TSCDeText = class(TSCDePackage)
  protected
    procedure SetCaption(const Value: TCaption); override;

    procedure RecreateCharacters(ACaption: String); dynamic;
    procedure UpdateCharacters; dynamic;

    procedure BrushChanged(Sender: TObject); override;
    procedure PenChanged(Sender: TObject); override;
  public
    constructor Create(ASurface: TSCDeSurface; AOwner: TSCDeContainer); override;

    procedure Resize(NewWidth, NewHeight: Double); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Double); override;

    procedure Paint(C: TCanvas; X, Y: Integer; Zoom: Double); override;
    procedure PaintIn(C: TCanvas; X, Y: Integer; Zoom: Double;
      R: TDoubleRect); override;

    procedure LoadFromXML(Node: TSCDomElement); override;
    procedure SaveToXML(Node: TSCDomElement); override;
  published
    property Caption;
    property Brush;
    property Font;
    property Pen;
  end;

implementation

type
  TBezierHolder = record
    Point: TSCDePoint;
    List: TList;
  end;
  PBezierHolder = ^TBezierHolder;

  PPoints = ^TPoints;
  TPoints = array[0..0] of TPoint;
  
{ TSCDeCustomRectangle }

procedure TSCDeCustomRectangle.Add(P: TSCDePoint);
begin
  //
end;

function TSCDeCustomRectangle.Add(x, y: Double): TSCDePoint;
begin
  Result := nil;
end;

constructor TSCDeCustomRectangle.Create(ASurface: TSCDeSurface;
  AOwner: TSCDeContainer);
begin
  inherited Create(ASurface, AOwner);
  SetShapeStyle(ShapeStyle + [scssUsesBrush, scssUsesGradient] - [scssUsesPicture]);
  FExtraHandles := False;

  BeginUpdate;
  try
    BeginPointUpdate;
    try
      FP1  := TSCDePoint.Create(Self);
      FP2  := TSCDePoint.Create(Self);
      FP3  := TSCDePoint.Create(Self);
      FP4  := TSCDePoint.Create(Self);

      FP12 := TSCDePoint.Create(Self);
      FP23 := TSCDePoint.Create(Self);
      FP34 := TSCDePoint.Create(Self);
      FP41 := TSCDePoint.Create(Self);
    finally
      EndPointUpdate;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TSCDeCustomRectangle.Delete(Index: Integer);
begin
  //
end;

procedure TSCDeCustomRectangle.DrawSelection(C: TCanvas; X, Y: Double;
  const DragPoint: TSCDeDragPoint; const Data: TSCDeSelectionData;
  Zoom: Double);
var
  R, PR: TRect;
  P: TSCDePoint;
  DR: TDoubleRect;
  OutCl, InCl: TColor;
  I, OX, OY, AX, AY: Integer;
  PtsLn: array[0..3] of TPoint;
  PtsBz: array[0..12] of TPoint;
begin
  DR := GetBounds;

  if (DragPoint.PointType = scptPoint) and
    ((DragPoint.Offset.x <> 0) or (DragPoint.Offset.y <> 0)) then
    with DragPoint do
      case DragPoint.PartIndex of
        0:
        begin
          DR.Left := DR.Left + Offset.x;
          DR.Top := DR.Top + Offset.y;
        end;
        1:
        begin
          DR.Right := DR.Right + Offset.x;
          DR.Top := DR.Top + Offset.y;
        end;
        2:
        begin
          DR.Right := DR.Right + Offset.x;
          DR.Bottom := DR.Bottom + Offset.y;
        end;
        3:
        begin
          DR.Left := DR.Left + Offset.x;
          DR.Bottom := DR.Bottom + Offset.y;
        end;
        4: DR.Top := DR.Top + Offset.y;
        5: DR.Right := DR.Right + Offset.x;
        6: DR.Bottom := DR.Bottom + Offset.y;
        7: DR.Left := DR.Left + Offset.x;
      end;

  TSCDeUtils.ZoomRect(DR, Zoom);
  TSCDeUtils.OffsetRect(DR, X, Y);

  if DR.Right < DR.Left then TSCDeUtils.Swap(DR.Left, DR.Right);
  if DR.Bottom < DR.Top then TSCDeUtils.Swap(DR.Top, DR.Bottom);

  R  := TSCDeUtils.Rect(DR);

  with C do
  begin
    Brush.Style := bsClear;

    Pen.Style := Data.LineStyle;
    Pen.Mode  := Data.LineMode;
    Pen.Color := Data.LineColor;
    Pen.Width := 1;
  end;

  if Data.LineColor <> clNone then
  begin
    if (R.Left < R.Right) and (R.Top < R.Bottom) then
    begin
      Dec(R.Right);
      Dec(R.Bottom);

      PtsBz[0]   := R.TopLeft;
      PtsBz[1]   := PtsBz[0];

      PtsBz[3].x := R.Right;
      PtsBz[3].y := R.Top;
      PtsBz[2]   := PtsBz[3];
      PtsBz[4]   := PtsBz[3];

      PtsBz[6]   := R.BottomRight;
      PtsBz[5]   := PtsBz[6];
      PtsBz[7]   := PtsBz[6];

      PtsBz[9].x := R.Left;
      PtsBz[9].y := R.Bottom;
      PtsBz[8]   := PtsBz[9];
      PtsBz[10]  := PtsBz[9];

      PtsBz[12] := R.TopLeft;
      PtsBz[11] := PtsBz[12];

      C.PolyBezier(PtsBz);
    end else
    if R.Top = R.Bottom then
    begin
      PtsLn[0]   := R.TopLeft;
      PtsLn[3].x := R.Right;
      PtsLn[3].y := R.Top;

      PtsLn[1]   := PtsLn[0];
      PtsLn[2]   := PtsLn[3];

      C.PolyBezier(PtsLn);
    end else
    if R.Left = R.Right then
    begin
      PtsLn[0]   := R.TopLeft;
      PtsLn[3].x := R.Left;
      PtsLn[3].y := R.Bottom;

      PtsLn[1]   := PtsLn[0];
      PtsLn[2]   := PtsLn[3];

      C.PolyBezier(PtsLn);
    end;
  end;

  OutCl := Data.PointOutColor;
  InCl  := Data.PointInColor;

  if OutCl = clNone then OutCl := InCl;

  if (DragPoint.PartIndex = -1) and ((InCl <> clNone) or
    (OutCl <> clNone)) and (Data.PointSize > 1) then
  begin
    OX := -(Data.PointSize div 2);
    OY := OX;

    for I := 0 to PointCount - 1 do
    begin
      P := Points[I];

      AX := TSCDeUtils.Round(X + Zoom*P.x);
      AY := TSCDeUtils.Round(Y + Zoom*P.y);

      PR := Rect(AX, AY, AX + Data.PointSize, AY + Data.PointSize);
      OffsetRect(PR, OX, OY);

      if not IsSelected(P) then
        DrawPoint(C, PR, InCl, OutCl)
      else if OutCl <> clNone then
        DrawPoint(C, PR, OutCl, OutCl)
      else
        DrawPoint(C, PR, InCl, InCl);
    end;
  end;
end;

procedure TSCDeCustomRectangle.Extract(P: TSCDePoint);
begin
  //
end;

function TSCDeCustomRectangle.GetDefaultBounds: TDoubleRect;
begin
  Result := TSCDeUtils.Rect(0, 0, 100, 50);
end;

function TSCDeCustomRectangle.GetGravityCenter: TDoublePoint;
var
  R: TDoubleRect;
begin
  R := GetBounds;

  Result.x := (R.Right - R.Left) / 2;
  Result.y := (R.Bottom - R.Top) / 2;
end;

function TSCDeCustomRectangle.InRect(const R: TDoubleRect): Boolean;
var
  P1, P2: TDoublePoint;
begin
  Result := inherited InRect(R);

  if not Result then
  begin
    P1.x := FP1.x;
    P1.y := FP1.y;
    P2.x := FP1.x;
    P2.y := FP3.y;

    Result := TSCDeUtils.DoLineIntersectRect(P1, P2, R);
  end;
end;

procedure TSCDeCustomRectangle.Insert(Index: Integer; P: TSCDePoint);
begin
  //
end;

procedure TSCDeCustomRectangle.LoadBoundsFromXML(Node: TSCDomElement);
var
  S: String;
  R: TDoubleRect;
  Prop: TSCDomElement;
begin
  BeginUpdate;
  try
    R := TSCDeUtils.Rect(0, 0, 0, 0);

    Prop := Node.ElementByName('rect');
    if Prop <> nil then
    begin
      S := GetProperty(Prop, 'left');
      if S <> '' then R.Left := StrToFloat(S);

      S := GetProperty(Prop, 'top');
      if S <> '' then R.Top := StrToFloat(S);

      S := GetProperty(Prop, 'right');
      if S <> '' then R.Right := StrToFloat(S);

      S := GetProperty(Prop, 'bottom');
      if S <> '' then R.Bottom := StrToFloat(S);

      if R.Right < R.Left then R.Right := R.Left;
      if R.Bottom < R.Top then R.Bottom := R.Top;
    end;

    Self.SetBounds(R);
  finally
    EndUpdate;
  end;
end;

procedure TSCDeCustomRectangle.MoveBy(X, Y: Double);
begin
  if not InPointUpdate and ((X <> 0) or (Y <> 0)) then
  begin
    BeginUpdate;
    try
      BeginPointUpdate;
      try
        BeginNotifyLock;
        try
          NotifyUndo(Self, scacSizing, '');

          FP1.SetPosition(FP1.x + X, FP1.y + Y);
          FP2.SetPosition(FP2.x + X, FP2.y + Y);
          FP3.SetPosition(FP3.x + X, FP3.y + Y);
          FP4.SetPosition(FP4.x + X, FP4.y + Y);

          RefreshExtraHandles;
        finally
          EndNotifyLock;
        end;
      finally
        EndPointUpdate;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSCDeCustomRectangle.Paint(C: TCanvas; X, Y: Integer; Zoom: Double);
var
  R: TRect;
  Rgn: HRGN;
  SR: Integer;
  DR: TDoubleRect;
begin
  DR := GetBounds;
  TSCDeUtils.ZoomRect(DR, Zoom);

  R := TSCDeUtils.Rect(DR);

  if (Self.HasPen or Self.HasBrush) and
    ((R.Left <> R.Right) or (R.Top <> R.Bottom)) then
  begin
    Pen.AssignTo(C.Pen);
    Brush.AssignTo(C.Brush);

    if Pen.Color = clNone then C.Pen.Style := psClear;
    if Brush.Color = clNone then C.Brush.Style := bsClear;

    OffsetRect(R, X, Y);

    if R.Left = R.Right then
      Inc(R.Right)
    else if R.Top = R.Bottom then
      Inc(R.Bottom);

    C.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
  end;

  if not IsRectEmpty(R) and (Self.HasGradient or Self.HasPicture) then
  begin
    if Self.HasGradient then
      Self.Gradient.Paint(C, R);

    if Self.HasPicture then
    begin
      Rgn := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
      try
        SR := ExtSelectClipRgn(C.Handle, Rgn, RGN_COPY);
        DeleteObject(Rgn);
        Rgn := 0;

        if SR <> NULLREGION then
          Self.DrawPicture(C, R);
      finally
        SelectClipRgn(C.Handle, 0);
        if Rgn <> 0 then
          DeleteObject(Rgn);
      end;
    end;

    if Self.HasPen then
    begin
      Pen.AssignTo(C.Pen);

      C.Brush.Style := bsClear;
      if Pen.Color = clNone then C.Pen.Style := psClear;

      C.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    end;
  end;
end;

procedure TSCDeCustomRectangle.PointChanged(P: TSCDePoint);
var
  R: TDoubleRect;
begin
  if not InPointUpdate then
  begin
    BeginUpdate;
    try
      BeginPointUpdate;
      try
        if P = FP1 then
        begin
          FP2.y := P.y;
          FP4.x := P.x;
        end else
        if P = FP2 then
        begin
          FP1.y := P.y;
          FP3.x := P.x;
        end else
        if P = FP3 then
        begin
          FP2.x := P.x;
          FP4.y := P.y;
        end else
        if P = FP4 then
        begin
          FP1.x := P.x;
          FP3.y := P.y;
        end else
        if P = FP12 then
        begin
          FP1.y := P.y;
          FP2.y := P.y;
        end else
        if P = FP23 then
        begin
          FP2.x := P.x;
          FP3.x := P.x;
        end else
        if P = FP34 then
        begin
          FP3.y := P.y;
          FP4.y := P.y;
        end else
        if P = FP41 then
        begin
          FP4.x := P.x;
          FP1.x := P.x;
        end;

        RefreshExtraHandles;

        R := GetBounds;

        FP1.SetPosition(R.Left, R.Top);
        FP2.SetPosition(R.Right, R.Top);
        FP3.SetPosition(R.Right, R.Bottom);
        FP4.SetPosition(R.Left, R.Bottom);

        RefreshExtraHandles;
      finally
        EndPointUpdate;
      end;
    finally
      Changed;
      EndUpdate;
    end;
  end;
end;

function TSCDeCustomRectangle.PointOnShape(P: TDoublePoint; Fuzz: Double;
  Editing: Boolean): Boolean;
var
  R: TDoubleRect;
begin
  if Fuzz < 0.0 then Fuzz := 0.0;

  R := GetBounds;
  TSCDeUtils.InflateRect(R, Fuzz, Fuzz);

  Result := not TSCDeUtils.IsRectEmpty(R) and TSCDeUtils.PtInRect(R, P);

  if Result and not (Self.HasBrush or
    Self.HasPicture or Self.HasGradient) then
  begin
    TSCDeUtils.InflateRect(R, -2*Fuzz, -2*Fuzz);
    Result := TSCDeUtils.IsRectEmpty(R) or
      not TSCDeUtils.PtInRect(R, P);
  end;
end;

procedure TSCDeCustomRectangle.PointRemoved(P: TSCDePoint);
begin
  if P = FP1 then FP1 := nil;
  if P = FP2 then FP2 := nil;
  if P = FP3 then FP3 := nil;
  if P = FP4 then FP4 := nil;

  if P = FP12 then FP12 := nil;
  if P = FP23 then FP23 := nil;
  if P = FP34 then FP34 := nil;
  if P = FP41 then FP41 := nil;
end;

procedure TSCDeCustomRectangle.RefreshExtraHandles;
var
  OX, OY: Double;
begin
  BeginPointUpdate;
  try
    OX := FP1.x + (FP2.x - FP1.x)/2;
    OY := FP2.y + (FP3.y - FP2.y)/2;

    if FP12 <> nil then FP12.SetPosition(OX, FP1.y);
    if FP23 <> nil then FP23.SetPosition(FP2.x, OY);
    if FP34 <> nil then FP34.SetPosition(OX, FP3.y);
    if FP41 <> nil then FP41.SetPosition(FP4.x, OY);
  finally
    EndPointUpdate;
  end;
end;

procedure TSCDeCustomRectangle.Remove(P: TSCDePoint);
begin
  //
end;

procedure TSCDeCustomRectangle.Resize(NewWidth, NewHeight: Double);
var
  R: TDoubleRect;
begin
  if not InPointUpdate then
  begin
    if NewWidth < 0 then NewWidth := 0.0;
    if NewHeight < 0 then NewHeight := 0.0;

    R := GetBounds;
    TSCDeUtils.OffsetRect(R, -R.Top, -R.Left);

    if (NewWidth <> R.Right) or (NewHeight <> R.Top) then
    begin
      BeginUpdate;
      try
        BeginPointUpdate;
        try
          BeginNotifyLock;
          try
            NotifyUndo(Self, scacSizing, '');

            FP2.SetPosition(FP1.x + NewWidth, FP1.y);
            FP3.SetPosition(FP1.x + NewWidth, FP1.y + NewHeight);
            FP4.SetPosition(FP1.x, FP1.y + NewHeight);

            RefreshExtraHandles;
          finally
            EndNotifyLock;
          end;
        finally
          EndPointUpdate;
        end;
      finally
        EndUpdate;
      end;
    end;  
  end;
end;

procedure TSCDeCustomRectangle.SaveBoundsToXML(Node: TSCDomElement);
var
  R: TDoubleRect;
  Prop: TSCDomElement;
begin
  R := GetBounds;

  Prop := TSCDomElement.Create(Node);
  Prop.Name := 'rect';
  Node.AddElement(Prop);

  AddProperty(Prop, 'left',   FloatToStr(R.Left));
  AddProperty(Prop, 'top',    FloatToStr(R.Top));
  AddProperty(Prop, 'right',  FloatToStr(R.Right));
  AddProperty(Prop, 'bottom', FloatToStr(R.Bottom));
end;

procedure TSCDeCustomRectangle.SetExtraHandles(Value: Boolean);
begin
  if FExtraHandles <> Value then
  begin
    FExtraHandles := Value;
    UpdateExtraHandles;
  end;
end;

procedure TSCDeCustomRectangle.UpdateExtraHandles;
begin
  BeginUpdate;
  try
    BeginPointUpdate;
    try
      if not FExtraHandles then
      begin
        if FP12 <> nil then FreeAndNil(FP12);
        if FP23 <> nil then FreeAndNil(FP23);
        if FP34 <> nil then FreeAndNil(FP34);
        if FP41 <> nil then FreeAndNil(FP41);
      end else
      begin
        if FP12 = nil then FP12 := TSCDePoint.Create(Self);
        if FP23 = nil then FP23 := TSCDePoint.Create(Self);
        if FP34 = nil then FP34 := TSCDePoint.Create(Self);
        if FP41 = nil then FP41 := TSCDePoint.Create(Self);

        RefreshExtraHandles;
      end;
    finally
      EndPointUpdate;
    end;
  finally
    EndUpdate;
  end;
end;

{ TSCDePicture }

constructor TSCDePicture.Create(ASurface: TSCDeSurface;
  AOwner: TSCDeContainer);
begin
  inherited Create(ASurface, AOwner);
  SetShapeStyle(ShapeStyle + [scssCanRotate, scssUsesPicture]);
end;

procedure TSCDePicture.LoadFromXML(Node: TSCDomElement);
begin
  Changing;
  BeginUpdate;
  try
    inherited LoadFromXML(Node);
    LoadBoundsFromXML(Node);
  finally
    EndUpdate;
  end;
end;

procedure TSCDePicture.Paint(C: TCanvas; X, Y: Integer; Zoom: Double);
var
  R: TRect;
  Rgn: HRGN;
  SR: Integer;
  DR: TDoubleRect;
begin
  DR := GetBounds;
  TSCDeUtils.ZoomRect(DR, Zoom);

  R := TSCDeUtils.Rect(DR);

  if (R.Left <> R.Right) or (R.Top <> R.Bottom) then
  begin
    OffsetRect(R, X, Y);

    if IsRectEmpty(R) or not (Self.HasPicture or Self.HasGradient) then
    begin
      if R.Left = R.Right then
        Inc(R.Right)
      else if R.Top = R.Bottom then
        Inc(R.Bottom);

      with C do
      begin
        Brush.Style := bsClear;

        Pen.Color := clBlack;
        Pen.Style := psDot;
        Pen.Mode  := pmMergePenNot;
        Pen.Width := 1;

        Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      end;
    end else
    if not IsRectEmpty(R) then
    begin
      if Self.HasGradient then
        Self.Gradient.Paint(C, R);

      if Self.HasPicture then
      begin
        Rgn := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
        try
          SR := ExtSelectClipRgn(C.Handle, Rgn, RGN_COPY);
          DeleteObject(Rgn);
          Rgn := 0;

          if SR <> NULLREGION then
            Self.DrawPicture(C, R);
        finally
          SelectClipRgn(C.Handle, 0);
          if Rgn <> 0 then
            DeleteObject(Rgn);
        end;
      end;
    end;
  end;
end;

function TSCDePicture.PointOnShape(P: TDoublePoint; Fuzz: Double;
  Editing: Boolean): Boolean;
var
  R: TDoubleRect;
begin
  if Fuzz < 0.0 then Fuzz := 0.0;

  R := GetBounds;
  TSCDeUtils.InflateRect(R, Fuzz, Fuzz);

  Result := not TSCDeUtils.IsRectEmpty(R) and TSCDeUtils.PtInRect(R, P);
end;

procedure TSCDePicture.SaveToXML(Node: TSCDomElement);
begin
  inherited SaveToXML(Node);
  SaveBoundsToXML(Node);
end;

{ TSCDeCustomPolyLine }

procedure TSCDeCustomPolyLine.Add(P: TSCDePoint);
begin
  inherited Add(P);
end;

function TSCDeCustomPolyLine.Add(x, y: Double): TSCDePoint;
begin
  Result := inherited Add(x, y);
end;

procedure TSCDeCustomPolyLine.Clear;
begin
  inherited Clear;
end;

constructor TSCDeCustomPolyLine.Create(ASurface: TSCDeSurface;
  AOwner: TSCDeContainer);
begin
  inherited Create(ASurface, AOwner);
  SetShapeStyle(ShapeStyle + [scssCanRotate, scssAcceptsPoint,
    scssCanRemovePoint, scssUsesBrush]);
end;

procedure TSCDeCustomPolyLine.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

procedure TSCDeCustomPolyLine.DrawSelection(C: TCanvas; X, Y: Double;
  const DragPoint: TSCDeDragPoint; const Data: TSCDeSelectionData;
  Zoom: Double);
var
  PR: TRect;
  Pp: TPoint;
  P: TDoublePoint;
  P1, P2: TSCDePoint;
  Pa: array[0..2] of TPoint;
  LineCl, OutCl, InCl: TColor;
  Pd: array[0..2] of TDoublePoint;
  I, J, Cnt, OX, OY, AX, AY: Integer;
begin
  if PointCount > 0 then
  begin
    LineCl := Data.LineColor;

    if (LineCl = clNone) and (Data.SelectionType = scdsHottrack) then
    begin
      LineCl := Data.PointOutColor;
      if LineCl = clNone then LineCl := Data.PointInColor;
    end;

    with C do
    begin
      Brush.Style := bsClear;

      Pen.Color := LineCl;
      Pen.Mode  := Data.LineMode;
      Pen.Style := Data.LineStyle;
      Pen.Width := 1;
    end;

    if LineCl <> clNone then
    begin
      if (Data.LineStyle = psClear) and (Data.SelectionType = scdsHottrack) then
        C.Pen.Style := psSolid;

      if PointCount = 1 then
      begin
        P1 := Points[0];

        P.x := X + Zoom*P1.x;
        P.y := Y + Zoom*P1.y;

        with DragPoint do
          if (PointType = scptPoint) and (PartIndex = 0) and
            ((Offset.x <> 0) or (Offset.y <> 0)) then
          begin
            P.x := P.x + Zoom*Offset.x;
            P.y := P.y + Zoom*Offset.y;
          end;

        Pp := TSCDeUtils.Point(P);

        C.MoveTo(Pp.x, Pp.y);
        C.LineTo(Pp.x + 1, Pp.y);
      end else
      begin
        Cnt := PointCount;

        for I := 0 to Cnt-2 do
        begin
          P1 := Points[I];
          P2 := Points[I + 1];

          Pd[0].x := X + P1.x;
          Pd[0].y := Y + P1.y;

          Pd[2].x := X + P2.x;
          Pd[2].y := Y + P2.y;

          with DragPoint do
            if (PointType = scptPoint) and
              ((Offset.x <> 0) or (Offset.y <> 0)) then
            begin
              if I = PartIndex then
              begin
                Pd[0].x := Pd[0].x + Offset.x;
                Pd[0].y := Pd[0].y + Offset.y;
              end else
              if PartIndex = I + 1 then
              begin
                Pd[2].x := Pd[2].x + Offset.x;
                Pd[2].y := Pd[2].y + Offset.y;
              end;
            end;

          Pd[1] := Pd[2];

          for J := 0 to 2 do
          begin
            Pa[J].x := TSCDeUtils.Round(Zoom*Pd[J].x);
            Pa[J].y := TSCDeUtils.Round(Zoom*Pd[J].y);
          end;

          if I = 0 then C.MoveTo(Pa[0].x, Pa[0].y);
          C.PolyBezierTo(Pa);
        end;

        if GetIsClosed and (Cnt > 2) then
        begin
          P1 := Points[Cnt-1];
          P2 := Points[0];

          Pd[0].x := X + P1.x;
          Pd[0].y := Y + P1.y;

          Pd[2].x := X + P2.x;
          Pd[2].y := Y + P2.y;

          with DragPoint do
            if (PointType = scptPoint) and
              ((Offset.x <> 0) or (Offset.y <> 0)) then
            begin
              if PartIndex = Cnt-1 then
              begin
                Pd[0].x := Pd[0].x + Offset.x;
                Pd[0].y := Pd[0].y + Offset.y;
              end else
              if PartIndex = 0 then
              begin
                Pd[2].x := Pd[2].x + Offset.x;
                Pd[2].y := Pd[2].y + Offset.y;
              end;
            end;

          Pd[1] := Pd[2];
          
          for J := 0 to 2 do
          begin
            Pa[J].x := TSCDeUtils.Round(Zoom*Pd[J].x);
            Pa[J].y := TSCDeUtils.Round(Zoom*Pd[J].y);
          end;

          C.PolyBezierTo(Pa);
        end;
      end;
    end;

    OutCl := Data.PointOutColor;
    InCl  := Data.PointInColor;

    if OutCl = clNone then OutCl := InCl;

    if (DragPoint.PartIndex = -1) and ((InCl <> clNone) or
      (OutCl <> clNone)) and (Data.PointSize > 1) then
    begin
      OX := -(Data.PointSize div 2);
      OY := OX;

      for I := 0 to PointCount-1 do
      begin
        P1 := Points[I];

        AX := TSCDeUtils.Round(X + Zoom*P1.x);
        AY := TSCDeUtils.Round(Y + Zoom*P1.y);

        PR := Rect(AX, AY, AX + Data.PointSize, AY + Data.PointSize);
        OffsetRect(PR, OX, OY);

        if not IsSelected(P1) then
          DrawPoint(C, PR, InCl, OutCl)
        else if OutCl <> clNone then
          DrawPoint(C, PR, OutCl, OutCl)
        else
          DrawPoint(C, PR, InCl, InCl);
      end;
    end;
  end;  
end;

procedure TSCDeCustomPolyLine.Extract(P: TSCDePoint);
begin
  inherited Extract(P);
end;

function TSCDeCustomPolyLine.GetPointArray: TDPointArray;
var
  L: TList;
  I: Integer;
  P1, P2: TSCDePoint;
begin
  SetLength(Result, 0);

  if PointCount > 0 then
  begin
    L := TList.Create;
    try
      P1 := Points[0];

      L.Add(P1);

      for I := 1 to PointCount-1 do
      begin
        P2 := Points[I];

        if (P1.x <> P2.x) or (P1.y <> P2.y) then
          L.Add(P2);

        P1 := P2;  
      end;

      if (L.Count > 2) and GetIsClosed then
        L.Add(Points[0]);

      SetLength(Result, L.Count);

      for I := 0 to L.Count - 1 do
      begin
        P1 := TSCDePoint(L[I]);

        Result[I].x := P1.x;
        Result[I].y := P1.y;
      end;
    finally
      L.Free;
    end;
  end;
end;

function TSCDeCustomPolyLine.GetShapeType: TSCShapeType;
begin
  Result := sctyPolyPoints;
end;

function TSCDeCustomPolyLine.InRect(const R: TDoubleRect): Boolean;
var
  P1, P2: TDoublePoint;
begin
  Result := inherited InRect(R);

  if not Result and GetIsClosed and (PointCount > 2) then
  begin
    P1.x := Points[0].x;
    P1.y := Points[0].y;
    P2.x := Points[PointCount-1].x;
    P2.y := Points[PointCount-1].y;

    Result := TSCDeUtils.DoLineIntersectRect(P1, P2, R);
  end;
end;

procedure TSCDeCustomPolyLine.Insert(Index: Integer; P: TSCDePoint);
begin
  inherited Insert(Index, P);
end;

function TSCDeCustomPolyLine.ApproxInsert(P: TDoublePoint; Fuzz: Double): TSCDePoint;
var
  L: TList;
  I: Integer;
  Pt, Pt2: TSCDePoint;
  P1, P2: TDoublePoint;
begin
  Result := nil;
  if Fuzz < 0.0 then Fuzz := 0.0;

  if PointCount > 1 then
  begin
    L := TList.Create;
    try
      Pt := Points[0];
      L.Add(Pt);

      for I := 1 to PointCount-1 do
      begin
        Pt2 := Points[I];
        if (Pt.x = Pt2.x) and (Pt.y = Pt2.y) then
          Continue;

        L.Add(Pt2);
        Pt := Pt2;
      end;

      if L.Count > 1 then
      begin
        if GetIsClosed and (L.Count > 2) then
          L.Add(L[0]);

        Pt := TSCDePoint(L[0]);

        P1.x := Pt.x;
        P1.y := Pt.y;

        for I := 1 to L.Count-1 do
        begin
          Pt := TSCDePoint(L[I]);

          P2.x := Pt.x;
          P2.y := Pt.y;

          if (P1.x = P2.x) and (P1.y = P2.y) then
            Continue;

          if TSCDeUtils.NearLine(P, P1, P2, Fuzz) then
          begin
            Result := TSCDePoint.Create(nil);

            Result.x := P.x;
            Result.y := P.y;

            Self.Insert(I, Result);

            Exit;
          end;

          P1 := P2;
        end;
      end;
    finally
      L.Free;
    end;
  end;
end;

procedure TSCDeCustomPolyLine.Paint(C: TCanvas; X, Y: Integer; Zoom: Double);
var
  Pp: TPoint;
  P: TDoublePoint;
  I, J, Cnt: Integer;
  P1, P2: TSCDePoint;
  Pa: array[0..2] of TPoint;
  Pd: array[0..2] of TDoublePoint;
begin
  if Self.HasPen and (PointCount > 0) then
  begin
    Pen.AssignTo(C.Pen);
    if Pen.Color = clNone then C.Pen.Style := psClear;

    if PointCount = 1 then
    begin
      P1 := Points[0];

      P.x := X + Zoom*P1.x;
      P.y := Y + Zoom*P1.y;

      Pp := TSCDeUtils.Point(P);

      C.MoveTo(Pp.x, Pp.y);
      C.LineTo(Pp.x + 1, Pp.y);
    end else
    begin
      Cnt := PointCount;

      for I := 0 to Cnt-2 do
      begin
        P1 := Points[I];
        P2 := Points[I + 1];

        Pd[0].x := X + P1.x;
        Pd[0].y := Y + P1.y;

        Pd[2].x := X + P2.x;
        Pd[2].y := Y + P2.y;

        Pd[1] := Pd[2];

        for J := 0 to 2 do
        begin
          Pa[J].x := TSCDeUtils.Round(Zoom*Pd[J].x);
          Pa[J].y := TSCDeUtils.Round(Zoom*Pd[J].y);
        end;

        if I = 0 then C.MoveTo(Pa[0].x, Pa[0].y);
        C.PolyBezierTo(Pa);
      end;

      if GetIsClosed and (Cnt > 2) then
      begin
        P1 := Points[Cnt-1];
        P2 := Points[0];

        Pd[0].x := X + P1.x;
        Pd[0].y := Y + P1.y;

        Pd[2].x := X + P2.x;
        Pd[2].y := Y + P2.y;

        Pd[1] := Pd[2];
          
        for J := 0 to 2 do
        begin
          Pa[J].x := TSCDeUtils.Round(Zoom*Pd[J].x);
          Pa[J].y := TSCDeUtils.Round(Zoom*Pd[J].y);
        end;

        C.PolyBezierTo(Pa);
      end;
    end;
  end;
end;

function TSCDeCustomPolyLine.PointOnShape(P: TDoublePoint; Fuzz: Double;
  Editing: Boolean): Boolean;
var
  L: TList;
  I: Integer;
  R: TDoubleRect;
  Pt, Pt2: TSCDePoint;
  P1, P2: TDoublePoint;
begin
  if Fuzz < 0.0 then Fuzz := 0.0;

  Result := False;

  if PointCount = 1 then
  begin
    P1 := Points[0].Point;

    if Fuzz = 0 then
      Result := (P1.x = P.x) and (P1.y = P.y)
    else begin
      R.Left := P1.x - (Fuzz / 2);
      R.Top := P1.y - (Fuzz / 2);

      R.Right := R.Left + Fuzz;
      R.Bottom := R.Top + Fuzz;

      Result := TSCDeUtils.PtInRect(R, P);
    end;
  end else
  if PointCount > 1 then
  begin
    L := TList.Create;
    try
      Pt := Points[0];
      L.Add(Pt);

      for I := 1 to PointCount-1 do
      begin
        Pt2 := Points[I];
        if (Pt.x = Pt2.x) and (Pt.y = Pt2.y) then
          Continue;

        L.Add(Pt2);
        Pt := Pt2;
      end;

      if L.Count > 1 then
      begin
        if GetIsClosed and (L.Count > 2) then
          L.Add(L[0]);

        Pt := TSCDePoint(L[0]);

        P1.x := Pt.x;
        P1.y := Pt.y;

        for I := 1 to L.Count-1 do
        begin
          Pt := TSCDePoint(L[I]);

          P2.x := Pt.x;
          P2.y := Pt.y;

          if (P1.x = P2.x) and (P1.y = P2.y) then
            Continue;

          Result := TSCDeUtils.NearLine(P, P1, P2, Fuzz);
          if Result then Exit;

          P1 := P2;
        end;
      end;
    finally
      L.Free;
    end;
  end;
end;

procedure TSCDeCustomPolyLine.Remove(P: TSCDePoint);
begin
  inherited Remove(P);
end;

procedure TSCDeCustomPolyLine.Resize(NewWidth, NewHeight: Double);
begin
  if NewWidth < 0.01 then NewWidth := 0.01;
  if NewHeight < 0.01 then NewHeight := 0.01;

  inherited Resize(NewWidth, NewHeight);
end;

procedure TSCDeCustomPolyLine.SetIsClosed(Value: Boolean);
begin
  if FIsClosed <> Value then
  begin
    FIsClosed := Value;
    if PointCount > 1 then
      Changed;
  end;
end;

function TSCDeCustomPolyLine.GetIsClosed: Boolean;
begin
  Result := FIsClosed;
end;

function TSCDeCustomPolyLine.IsClosedStored: Boolean;
begin
  Result := True;
end;

function TSCDeCustomPolyLine.DefaultClosed: Boolean;
begin
  Result := False;
end;

{ TSCDeCustomEllipse }

procedure TSCDeCustomEllipse.DrawSelection(C: TCanvas; X, Y: Double;
  const DragPoint: TSCDeDragPoint; const Data: TSCDeSelectionData;
  Zoom: Double);
var
  R, PR: TRect;
  P: TSCDePoint;
  DR: TDoubleRect;
  OutCl, InCl: TColor;
  I, OX, OY, AX, AY: Integer;
begin
  DR := GetBounds;

  with DragPoint do
    if (PointType = scptPoint) and
      ((Offset.x <> 0) or (Offset.y <> 0)) then
      case DragPoint.PartIndex of
        0:
        begin
          DR.Left := DR.Left + Offset.x;
          DR.Top := DR.Top + Offset.y;
        end;
        1:
        begin
          DR.Right := DR.Right + Offset.x;
          DR.Top := DR.Top + Offset.y;
        end;
        2:
        begin
          DR.Right := DR.Right + Offset.x;
          DR.Bottom := DR.Bottom + Offset.y;
        end;
        3:
        begin
          DR.Left := DR.Left + Offset.x;
          DR.Bottom := DR.Bottom + Offset.y;
        end;
        4: DR.Top := DR.Top + Offset.y;
        5: DR.Right := DR.Right + Offset.x;
        6: DR.Bottom := DR.Bottom + Offset.y;
        7: DR.Left := DR.Left + Offset.x;
      end;

  TSCDeUtils.ZoomRect(DR, Zoom);
  TSCDeUtils.OffsetRect(DR, X, Y);

  if DR.Right < DR.Left then
    TSCDeUtils.Swap(DR.Left, DR.Right);

  if DR.Bottom < DR.Top then
    TSCDeUtils.Swap(DR.Top, DR.Bottom);

  R  := TSCDeUtils.Rect(DR);

  with C do
  begin
    Brush.Style := bsClear;

    Pen.Style := Data.LineStyle;
    Pen.Mode  := Data.LineMode;
    Pen.Color := Data.LineColor;
    Pen.Width := 1;
  end;

  if Data.LineColor <> clNone then
  begin
    if (R.Left < R.Right) and (R.Top = R.Bottom) then
      TSCDeGraphicUtils.Line(C, Point(R.Left, R.Top), Point(R.Right, R.Top))
    else
    if (R.Left = R.Right) and (R.Top < R.Bottom) then
      TSCDeGraphicUtils.Line(C, Point(R.Left, R.Top), Point(R.Left, R.Bottom))
    else
    if (R.Left < R.Right) and (R.Top < R.Bottom) then
      C.Ellipse(R.Left, R.Top, R.Right, R.Bottom);
  end;

  OutCl := Data.PointOutColor;
  InCl  := Data.PointInColor;

  if OutCl = clNone then OutCl := InCl;

  if (DragPoint.PartIndex = -1) and ((InCl <> clNone) or
    (OutCl <> clNone)) and (Data.PointSize > 1) then
  begin
    OX := -(Data.PointSize div 2);
    OY := OX;

    for I := 0 to PointCount-1 do
    begin
      P := Points[I];

      AX := TSCDeUtils.Round(X + Zoom*P.x);
      AY := TSCDeUtils.Round(Y + Zoom*P.y);

      PR := Rect(AX, AY, AX + Data.PointSize, AY + Data.PointSize);
      OffsetRect(PR, OX, OY);

      if not IsSelected(P) then
        DrawPoint(C, PR, InCl, OutCl)
      else if OutCl <> clNone then
        DrawPoint(C, PR, OutCl, OutCl)
      else
        DrawPoint(C, PR, InCl, InCl);
    end;
  end;
end;

function TSCDeCustomEllipse.IsOnLine(P: TDoublePoint): Boolean;
begin
  Result := False;
end;

procedure TSCDeCustomEllipse.Paint(C: TCanvas; X, Y: Integer; Zoom: Double);
var
  R: TRect;
  Rgn: HRGN;
  SR: Integer;
  DR: TDoubleRect;
begin
  DR := GetBounds;
  TSCDeUtils.ZoomRect(DR, Zoom);

  R := TSCDeUtils.Rect(DR);

  if (Self.HasPen or Self.HasBrush) and
    ((R.Left <> R.Right) or (R.Top <> R.Bottom)) then
  begin
    Pen.AssignTo(C.Pen);
    Brush.AssignTo(C.Brush);

    if Brush.Color = clNone then C.Brush.Style := bsClear;

    if C.Pen.Color = clNone then
      with C.Pen do
      begin
        Style := psSolid;
        Color := Self.Brush.Color;

        if Color = clNone then Style := psClear;
      end;

    OffsetRect(R, X, Y);

    if R.Left = R.Right then
      Inc(R.Right)
    else if R.Top = R.Bottom then
      Inc(R.Bottom);

    C.Ellipse(R.Left, R.Top, R.Right, R.Bottom);
  end;

  if not IsRectEmpty(R) and (Self.HasGradient or Self.HasPicture) then
  begin
    Rgn := CreateEllipticRgn(R.Left, R.Top, R.Right + 1, R.Bottom + 1);
    try
      SR := ExtSelectClipRgn(C.Handle, Rgn, RGN_COPY);
      DeleteObject(Rgn);
      Rgn := 0;

      if SR <> NULLREGION then
      begin
        if Self.HasGradient then
          Self.Gradient.Paint(C, R);

        C.Brush.Style := bsClear;

        if Self.HasPicture then
          Self.DrawPicture(C, R);

        if Self.HasPen then
        begin
          Pen.AssignTo(C.Pen);
          Brush.AssignTo(C.Brush);
          
          C.Brush.Style := bsClear;

          if C.Pen.Color = clNone then
            with C.Pen do
            begin
              Style := psSolid;
              Color := Self.Brush.Color;

              if Color = clNone then Style := psClear;
            end;

          C.Ellipse(R.Left, R.Top, R.Right, R.Bottom);
        end;
      end;
    finally
      SelectClipRgn(C.Handle, 0);
      if Rgn <> 0 then
        DeleteObject(Rgn);
    end;
  end;
end;

function TSCDeCustomEllipse.PointOnShape(P: TDoublePoint; Fuzz: Double;
  Editing: Boolean): Boolean;
var
  DR, R: TDoubleRect;
  AGradient: TSCDeShapeGradient;
  UsePicture, UseGradient: Boolean;
begin
  if Fuzz < 0.0 then Fuzz := 0.0;

  DR := GetBounds;

  if (DR.Left = DR.Right) or (DR.Top = DR.Bottom) then
  begin
    TSCDeUtils.InflateRect(DR, Fuzz, Fuzz);
    Result := TSCDeUtils.PtInRect(DR, TSCDeUtils.Point(P.x, P.y));
  end else
  begin
    R := DR;
    TSCDeUtils.InflateRect(R, Fuzz, Fuzz);

    Result := TSCDeUtils.PtInEllipse(P, R);

    if Result and (DR.Right - DR.Left > Fuzz) and (DR.Bottom - DR.Top > Fuzz) then
    begin
      UsePicture := (scssUsesPicture in ShapeStyle) and Self.HasPicture;

      UseGradient := False;
      if (scssUsesGradient in ShapeStyle) then
      begin
        AGradient := GetGradient;
        UseGradient := (AGradient <> nil) and (AGradient.Style <> scdgsNone) and
          (AGradient.ColorBegin <> clNone) and (AGradient.ColorEnd <> clNone);
      end;

      if not (UsePicture or UseGradient) and ((Brush.Style = bsClear) or (Brush.Color = clNone)) then
      begin
        R := DR;
        TSCDeUtils.InflateRect(R, -Fuzz, -Fuzz);

        Result := not TSCDeUtils.PtInEllipse(P, R);
      end;
    end;
  end;
end;

{ TSCDeCircle }

procedure TSCDeCircle.ArrangeBoundsRect(var R: TDoubleRect);
var
  W, H, NS: Double;
begin
  inherited ArrangeBoundsRect(R);

  W := R.Right - R.Left;
  H := R.Bottom - R.Top;

  NS := W;
  if H > W then NS := H;

  R.Right := R.Left + NS;
  R.Bottom := R.Top + NS;
end;

constructor TSCDeCircle.Create(ASurface: TSCDeSurface;
  AOwner: TSCDeContainer);
begin
  BeginUpdate;
  try
    BeginPointUpdate;
    try
      inherited Create(ASurface, AOwner);
      SetShapeStyle(ShapeStyle + [scssUsesPicture, scssUsesBrush,
        scssUsesPen]);

      SetBounds(0, 0, 100, 100);
    finally
      EndPointUpdate;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TSCDeCircle.DrawSelection(C: TCanvas; X, Y: Double;
  const DragPoint: TSCDeDragPoint; const Data: TSCDeSelectionData;
  Zoom: Double);
var
  W, H: Double;
  R, PR: TRect;
  P: TSCDePoint;
  DR: TDoubleRect;
  OutCl, InCl: TColor;
  Ps, Pe: TDoublePoint;
  I, OX, OY, AX, AY: Integer;
  Rotation: TSCDeSizeRotation;
begin
  DR := GetBounds;

  with DragPoint do
    if (PointType = scptPoint) and (PartIndex in [0..7]) and
      ((Offset.x <> 0) or (Offset.y <> 0)) then
    begin
      case DragPoint.PartIndex of
        0:
        begin
          Ps := DR.BottomRight;

          Pe.x := DR.Left + Offset.x;
          Pe.y := DR.Top + Offset.y;
        end;
        1:
        begin
          Ps.x := DR.Left;
          Ps.y := DR.Bottom;

          Pe.x := DR.Right + Offset.x;
          Pe.y := DR.Top + Offset.y;
        end;
        2:
        begin
          Ps := DR.TopLeft;

          Pe.x := DR.Right + Offset.x;
          Pe.y := DR.Bottom + Offset.y;
        end;
        3:
        begin
          Ps.x := DR.Right;
          Ps.y := DR.Top;

          Pe.x := DR.Left + Offset.x;
          Pe.y := DR.Bottom + Offset.y;
        end;
        4:
        begin
          Ps.x := DR.Left;
          Ps.y := DR.Bottom;

          Pe.y := DR.Top + Offset.y;
          Pe.x := DR.Right - Offset.y;

          if Pe.x < Ps.x then Pe.x := Ps.x;
        end;
        5:
        begin
          Ps.x := DR.Left;
          Ps.y := DR.Top;

          Pe.x := DR.Right + Offset.x;
          Pe.y := DR.Bottom + Offset.x;

          if Pe.y < Ps.y then Pe.y := Ps.y;
        end;
        6:
        begin
          Ps.x := DR.Left;
          Ps.y := DR.Top;

          Pe.y := DR.Bottom + Offset.y;
          Pe.x := DR.Right + Offset.y;

          if Pe.x < Ps.x then Pe.x := Ps.x;
        end;
        7:
        begin
          Ps.x := DR.Right;
          Ps.y := DR.Top;

          Pe.x := DR.Left + Offset.x;
          Pe.y := DR.Bottom - Offset.x;

          if Pe.y < Ps.y then Pe.y := Ps.y;
        end;
      end;

      if Pe.x - Ps.x < 0.0 then
      begin
        Rotation := scsrSouthWest;
        if Pe.y - Ps.y < 0.0 then Rotation := scsrNorthWest;
      end else
      begin
        Rotation := scsrSouthEast;
        if Pe.y - Ps.y < 0.0 then Rotation := scsrNorthEast;
      end;

      DR.TopLeft := Ps;
      DR.BottomRight := Pe;

      Self.ArrangeBoundsRect(DR);

      W := Abs(DR.Right - DR.Left);
      H := Abs(DR.Bottom - DR.Top);

      DR.TopLeft := Ps;
      DR.BottomRight := DR.TopLeft;

      case Rotation of
        scsrSouthEast:
        begin
          DR.Right := DR.Left + W;
          DR.Bottom := DR.Top + H;
        end;
        scsrSouthWest:
        begin
          DR.Left := DR.Right - W;
          DR.Bottom := DR.Top + H;
        end;
        scsrNorthWest:
        begin
          DR.Left := DR.Right - W;
          DR.Top := DR.Bottom - H;
        end;
        scsrNorthEast:
        begin
          DR.Right := DR.Left + W;
          DR.Top := DR.Bottom - H;
        end;
      end;
    end;

  TSCDeUtils.ZoomRect(DR, Zoom);
  TSCDeUtils.OffsetRect(DR, X, Y);

  if DR.Right < DR.Left then TSCDeUtils.Swap(DR.Left, DR.Right);
  if DR.Bottom < DR.Top then TSCDeUtils.Swap(DR.Top, DR.Bottom);

  R  := TSCDeUtils.Rect(DR);

  with C do
  begin
    Brush.Style := bsClear;

    Pen.Style := Data.LineStyle;
    Pen.Mode  := Data.LineMode;
    Pen.Color := Data.LineColor;
    Pen.Width := 1;
  end;

  if Data.LineColor <> clNone then
  begin
    if (R.Left < R.Right) and (R.Top = R.Bottom) then
      TSCDeGraphicUtils.Line(C, Point(R.Left, R.Top), Point(R.Right, R.Top))
    else
    if (R.Left = R.Right) and (R.Top < R.Bottom) then
      TSCDeGraphicUtils.Line(C, Point(R.Left, R.Top), Point(R.Left, R.Bottom))
    else
    if (R.Left < R.Right) and (R.Top < R.Bottom) then
      C.Ellipse(R.Left, R.Top, R.Right, R.Bottom);
  end;

  OutCl := Data.PointOutColor;
  InCl  := Data.PointInColor;

  if OutCl = clNone then OutCl := InCl;

  if (DragPoint.PartIndex = -1) and ((InCl <> clNone) or
    (OutCl <> clNone)) and (Data.PointSize > 1) then
  begin
    OX := -(Data.PointSize div 2);
    OY := OX;

    for I := 0 to PointCount-1 do
    begin
      P := Points[I];

      AX := TSCDeUtils.Round(X + Zoom*P.x);
      AY := TSCDeUtils.Round(Y + Zoom*P.y);

      PR := Rect(AX, AY, AX + Data.PointSize, AY + Data.PointSize);
      OffsetRect(PR, OX, OY);

      if not IsSelected(P) then
        DrawPoint(C, PR, InCl, OutCl)
      else if OutCl <> clNone then
        DrawPoint(C, PR, OutCl, OutCl)
      else
        DrawPoint(C, PR, InCl, InCl);
    end;
  end;
end;

procedure TSCDeCircle.LoadFromXML(Node: TSCDomElement);
var
  Prop: TSCDomElement;
begin
  Changing;
  BeginUpdate;
  try
    inherited LoadFromXML(Node);

    Prop := Node.ElementByName('brush');
    if Prop <> nil then Brush.LoadFromXML(Prop);

    Prop := Node.ElementByName('pen');
    if Prop <> nil then Pen.LoadFromXML(Prop);

    LoadBoundsFromXML(Node);
  finally
    EndUpdate;
  end;
end;

procedure TSCDeCircle.PointChanged(P: TSCDePoint);
var
  DR: TDoubleRect;
  Ps, Pe: TDoublePoint;
  W, H, POffset: Double;
  Rotation: TSCDeSizeRotation;
begin
  if not InPointUpdate then
  begin
    BeginUpdate;
    try
      BeginPointUpdate;
      try
        DR := GetBounds;

        if P = FP1 then
        begin
          Ps := FP3.Point;
          Pe := FP1.Point;
        end else
        if P = FP2 then
        begin
          Ps := FP4.Point;
          Pe := FP2.Point;
        end else
        if P = FP3 then
        begin
          Ps := FP1.Point;
          Pe := FP3.Point;
        end else
        if P = FP4 then
        begin
          Ps := FP2.Point;
          Pe := FP4.Point;
        end else
        if P = FP12 then
        begin
          Ps := FP4.Point;
          Pe := FP2.Point;

          POffset := FP12.y - FP1.y;

          Pe.y := Pe.y + POffset;
          Pe.x := Pe.x - POffset;

          if Pe.x < Ps.x then Pe.x := Ps.x;
        end else
        if P = FP23 then
        begin
          Ps := FP1.Point;
          Pe := FP3.Point;

          POffset := FP23.x - FP2.x;

          Pe.x := Pe.x + POffset;
          Pe.y := Pe.y + POffset;

          if Pe.y < Ps.y then Pe.y := Ps.y;
        end else
        if P = FP34 then
        begin
          Ps := FP1.Point;
          Pe := FP3.Point;

          POffset := FP34.y - FP3.y;

          Pe.y := Pe.y + POffset;
          Pe.x := Pe.x + POffset;

          if Pe.x < Ps.x then Pe.x := Ps.x;
        end else
        if P = FP41 then
        begin
          Ps := FP2.Point;
          Pe := FP4.Point;

          POffset := FP41.x - FP1.x;

          Pe.x := Pe.x + POffset;
          Pe.y := Pe.y - POffset;

          if Pe.y < Ps.y then Pe.y := Ps.y;
        end;

        if Pe.x - Ps.x < 0.0 then
        begin
          Rotation := scsrSouthWest;
          if Pe.y - Ps.y < 0.0 then Rotation := scsrNorthWest;
        end else
        begin
          Rotation := scsrSouthEast;
          if Pe.y - Ps.y < 0.0 then Rotation := scsrNorthEast;
        end;

        DR.TopLeft := Ps;
        DR.BottomRight := Pe;

        Self.ArrangeBoundsRect(DR);

        W := Abs(DR.Right - DR.Left);
        H := Abs(DR.Bottom - DR.Top);

        DR.TopLeft := Ps;
        DR.BottomRight := DR.TopLeft;

        case Rotation of
          scsrSouthEast:
          begin
            DR.Right := DR.Left + W;
            DR.Bottom := DR.Top + H;
          end;
          scsrSouthWest:
          begin
            DR.Left := DR.Right - W;
            DR.Bottom := DR.Top + H;
          end;
          scsrNorthWest:
          begin
            DR.Left := DR.Right - W;
            DR.Top := DR.Bottom - H;
          end;
          scsrNorthEast:
          begin
            DR.Right := DR.Left + W;
            DR.Top := DR.Bottom - H;
          end;
        end;

        if DR.Right < DR.Left then TSCDeUtils.Swap(DR.Left, DR.Right);
        if DR.Bottom < DR.Top then TSCDeUtils.Swap(DR.Top, DR.Bottom);

        FP1.SetPosition(DR.Left, DR.Top);
        FP2.SetPosition(DR.Right, DR.Top);
        FP3.SetPosition(DR.Right, DR.Bottom);
        FP4.SetPosition(DR.Left, DR.Bottom);

        RefreshExtraHandles;
      finally
        EndPointUpdate;
      end;
    finally
      Changed;
      EndUpdate;
    end;
  end;
end;

function TSCDeCircle.PointOnShape(P: TDoublePoint; Fuzz: Double;
  Editing: Boolean): Boolean;
var
  R: TDoubleRect;
  Rd, Dist: Double;
  Mid: TDoublePoint;
  AGradient: TSCDeShapeGradient;
  UsePicture, UseGradient: Boolean;
begin
  if Fuzz < 0.0 then Fuzz := 0.0;

  R := GetBounds;
  
  if (R.Right > R.Left) and (R.Bottom = R.Top) then // Horz line
    Result := TSCDeUtils.NearLine(P, TSCDeUtils.Point(R.Left, R.Top),
      TSCDeUtils.Point(R.Right, R.Top), Fuzz)
  else
  if (R.Right = R.Left) and (R.Bottom > R.Top) then // Horz line
    Result := TSCDeUtils.NearLine(P, TSCDeUtils.Point(R.Left, R.Top),
      TSCDeUtils.Point(R.Left, R.Bottom), Fuzz)
  else
  if (R.Left = R.Right) and (R.Top = R.Bottom) then
  begin
    TSCDeUtils.InflateRect(R, Fuzz, Fuzz);
    Result := TSCDeUtils.PtInRect(R, P);
  end else
  begin
    Rd := (R.Right - R.Left) / 2;

    Mid.x := R.Left + Rd;
    Mid.y := R.Top + Rd;

    Dist := Hypot(Abs(Mid.x - P.x), Abs(Mid.y - P.y));
    Result := Dist <= Rd + Fuzz;

    if Result then
    begin
      UsePicture := (scssUsesPicture in ShapeStyle) and Self.HasPicture;

      UseGradient := False;
      if (scssUsesGradient in ShapeStyle) then
      begin
        AGradient := GetGradient;
        UseGradient := (AGradient <> nil) and (AGradient.Style <> scdgsNone) and
          (AGradient.ColorBegin <> clNone) and (AGradient.ColorEnd <> clNone);
      end;

      if not (UsePicture or UseGradient) and ((Brush.Style = bsClear) or (Brush.Color = clNone)) then
        Result := Dist >= Rd - Fuzz;
    end;
  end;
end;

procedure TSCDeCircle.Resize(NewWidth, NewHeight: Double);
var
  R: TDoubleRect;
  W, H, NS: Double;
begin
  R := GetBounds;

  W := R.Right - R.Left;
  H := R.Bottom - R.Top;

  if NewWidth < 0.0 then NewWidth := W;
  if NewHeight < 0.0 then NewHeight := H;

  if not InPointUpdate and ((W <> NewWidth) or (H <> NewHeight)) then
  begin
    NS := NewWidth;
    if (W = NewWidth) and (H <> NewHeight) then
      NS := NewHeight
    else
    if (W <> NewWidth) and (H <> NewHeight) and (NewHeight > NewWidth) then
      NS := NewHeight;

    inherited Resize(NS, NS);
  end;
end;

procedure TSCDeCircle.SaveToXML(Node: TSCDomElement);
var
  Prop: TSCDomElement;
begin
  inherited SaveToXML(Node);

  SaveBoundsToXML(Node);

  Prop := TSCDomElement.Create(Node);
  Prop.Name := 'brush';
  Node.AddElement(Prop);

  Brush.SaveToXML(Prop);

  Prop := TSCDomElement.Create(Node);
  Prop.Name := 'pen';
  Node.AddElement(Prop);

  Pen.SaveToXML(Prop);
end;

{ TSCDePolygon }

constructor TSCDePolygon.Create(ASurface: TSCDeSurface;
  AOwner: TSCDeContainer);
begin
  FIsClosed := True;
  inherited Create(ASurface, AOwner);
  SetShapeStyle(Self.ShapeStyle + [scssUsesGradient, scssUsesPicture]);
end;

procedure TSCDePolygon.DrawSelection(C: TCanvas; X, Y: Double;
  const DragPoint: TSCDeDragPoint; const Data: TSCDeSelectionData;
  Zoom: Double);
var
  PR: TRect;
  P: TSCDePoint;
  Px, Py: Double;
  Pts: array of TPoint;
  LineCl, OutCl, InCl: TColor;
  I, OX, OY, AX, AY: Integer;
begin
  if PointCount > 0 then
  begin
    LineCl := Data.LineColor;

    if (LineCl = clNone) and (Data.SelectionType = scdsHottrack) and
      ((Self.Brush.Color = clNone) or (Self.Brush.Style = bsClear)) then
    begin
      LineCl := Data.PointOutColor;
      if LineCl = clNone then LineCl := Data.PointInColor;
    end;

    with C do
    begin
      Brush.Style := bsClear;

      Pen.Color := LineCl;
      Pen.Mode  := Data.LineMode;
      Pen.Style := Data.LineStyle;
      Pen.Width := 1;
    end;

    if LineCl <> clNone then
    begin
      if (Data.LineStyle = psClear) and (Data.SelectionType = scdsHottrack) and
        ((Self.Brush.Color = clNone) or (Self.Brush.Style = bsClear)) then
        C.Pen.Style := psSolid;

      SetLength(Pts, PointCount);

      for I := 0 to PointCount-1 do
      begin
        P := Points[I];

        Px := X + Zoom*P.x;
        Py := Y + Zoom*P.y;

        with DragPoint do
          if (PointType = scptPoint) and (I = PartIndex) then
          begin
            Px := Px + Zoom*Offset.x;
            Py := Py + Zoom*Offset.y;
          end;

        Pts[I].x := TSCDeUtils.Round(Px);
        Pts[I].y := TSCDeUtils.Round(Py);
      end;

      if Length(Pts) > 1 then
        C.Polygon(Pts)
      else
      if Length(Pts) = 1 then
      begin
        C.MoveTo(Pts[0].x, Pts[0].y);
        C.LineTo(Pts[0].x + 1, Pts[0].y);
      end;
    end;

    if Length(Pts) = 0 then
      Exit;

    OutCl := Data.PointOutColor;
    InCl  := Data.PointInColor;

    if OutCl = clNone then OutCl := InCl;

    if (DragPoint.PartIndex = -1) and ((InCl <> clNone) or
      (OutCl <> clNone)) and (Data.PointSize > 1) then
    begin
      OX := -(Data.PointSize div 2);
      OY := OX;

      for I := 0 to PointCount-1 do
      begin
        P := Points[I];

        AX := TSCDeUtils.Round(X + Zoom*P.x);
        AY := TSCDeUtils.Round(Y + Zoom*P.y);

        PR := Rect(AX, AY, AX + Data.PointSize, AY + Data.PointSize);
        OffsetRect(PR, OX, OY);

        if not IsSelected(P) then
          DrawPoint(C, PR, InCl, OutCl)
        else if OutCl <> clNone then
          DrawPoint(C, PR, OutCl, OutCl)
        else
          DrawPoint(C, PR, InCl, InCl);
      end;
    end;
  end;  
end;

function TSCDePolygon.GetPointArray: TDPointArray;
var
  L: TList;
  I: Integer;
  P1, P2: TSCDePoint;
begin
  SetLength(Result, 0);

  if PointCount > 0 then
  begin
    L := TList.Create;
    try
      P1 := Points[0];

      L.Add(P1);

      for I := 1 to PointCount-1 do
      begin
        P2 := Points[I];

        if (P1.x <> P2.x) or (P1.y <> P2.y) then
          L.Add(P2);

        P1 := P2;
      end;

      if L.Count > 2 then
        L.Add(Points[0]);

      SetLength(Result, L.Count);

      for I := 0 to L.Count - 1 do
      begin
        P1 := TSCDePoint(L[I]);

        Result[I].x := P1.x;
        Result[I].y := P1.y;
      end;
    finally
      L.Free;
    end;
  end;
end;

function TSCDePolygon.InRect(const R: TDoubleRect): Boolean;
var
  P1, P2: TDoublePoint;
begin
  Result := inherited InRect(R);

  if not Result and (PointCount > 2) then
  begin
    P1.x := Points[0].x;
    P1.y := Points[0].y;
    P2.x := Points[PointCount-1].x;
    P2.y := Points[PointCount-1].y;

    Result := TSCDeUtils.DoLineIntersectRect(P1, P2, R);
  end;
end;

function TSCDePolygon.ApproxInsert(P: TDoublePoint; Fuzz: Double): TSCDePoint;
var
  L: TList;
  I: Integer;
  Pt, Pt2: TSCDePoint;
  P1, P2: TDoublePoint;
begin
  Result := nil;
  if Fuzz < 0.0 then Fuzz := 0.0;

  if PointCount > 1 then
  begin
    L := TList.Create;
    try
      Pt := Points[0];
      L.Add(Pt);

      for I := 1 to PointCount-1 do
      begin
        Pt2 := Points[I];
        if (Pt.x = Pt2.x) and (Pt.y = Pt2.y) then
          Continue;

        L.Add(Pt2);
        Pt := Pt2;
      end;

      if L.Count > 1 then
      begin
        if L.Count > 2 then
          L.Add(L[0]);

        Pt := TSCDePoint(L[0]);

        P1.x := Pt.x;
        P1.y := Pt.y;

        for I := 1 to L.Count-1 do
        begin
          Pt := TSCDePoint(L[I]);

          P2.x := Pt.x;
          P2.y := Pt.y;

          if (P1.x = P2.x) and (P1.y = P2.y) then
            Continue;

          if TSCDeUtils.NearLine(P, P1, P2, Fuzz) then
          begin
            Result := TSCDePoint.Create(nil);

            Result.x := P.x;
            Result.y := P.y;

            Self.Insert(I, Result);

            Exit;
          end;

          P1 := P2;
        end;
      end;
    finally
      L.Free;
    end;
  end;
end;

procedure TSCDePolygon.LoadFromXML(Node: TSCDomElement);
var
  Prop: TSCDomElement;
begin
  Changing;
  BeginUpdate;
  try
    BeginPointUpdate;
    try
      inherited LoadFromXML(Node);

      Prop := Node.ElementByName('brush');
      if Prop <> nil then Brush.LoadFromXML(Prop);

      Prop := Node.ElementByName('pen');
      if Prop <> nil then Pen.LoadFromXML(Prop);

      LoadBoundsFromXML(Node);
    finally
      EndPointUpdate;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TSCDePolygon.Paint(C: TCanvas; X, Y: Integer; Zoom: Double);
var
  L: TList;
  Pt: TPoint;
  Pp: PPoint;
  I: Integer;
  R: TRect;
  Rgn: HRGN;
  SR: Integer;
  P: TSCDePoint;
  DR: TDoubleRect;
  Pts: array of TPoint;
  XChanged, YChanged: Boolean;
begin
  SetLength(Pts, 0);

  XChanged := False;
  YChanged := False;

  if PointCount > 0 then
  begin
    L := TList.Create;
    try
      P := Points[0];

      Pt.x := TSCDeUtils.Round(X + Zoom*P.x);
      Pt.y := TSCDeUtils.Round(Y + Zoom*P.y);

      New(Pp);
      Pp^.x := Pt.x;
      Pp^.y := Pt.y;

      L.Add(Pp);

      for I := 1 to PointCount-1 do
      begin
        P := Points[I];

        Pt.x := TSCDeUtils.Round(X + Zoom*P.x);
        Pt.y := TSCDeUtils.Round(Y + Zoom*P.y);

        if (Pp^.x <> Pt.x) or (Pp^.y <> Pt.y) then
        begin
          New(Pp);
          Pp^.x := Pt.x;
          Pp^.y := Pt.y;

          L.Add(Pp);
        end;
      end;

      if L.Count > 2 then
      begin
        P := Points[0];

        Pt.x := TSCDeUtils.Round(X + Zoom*P.x);
        Pt.y := TSCDeUtils.Round(Y + Zoom*P.y);

        New(Pp);
        Pp^.x := Pt.x;
        Pp^.y := Pt.y;

        L.Add(Pp);
      end;

      SetLength(Pts, L.Count);

      for I := 0 to L.Count - 1 do
      begin
        Pp := PPoint(L[I]);

        Pts[I].x := Pp^.x;
        Pts[I].y := Pp^.y;

        Dispose(Pp);

        if I > 0 then
        begin
          XChanged := XChanged or (Pts[I - 1].x <> Pts[I].x);
          YChanged := YChanged or (Pts[I - 1].y <> Pts[I].y);
        end;
      end;
    finally
      L.Free;
    end;
  end;

  if Length(Pts) = 0 then
    Exit;

  if Self.HasPen or Self.HasBrush then
  begin
    Pen.AssignTo(C.Pen);
    Brush.AssignTo(C.Brush);

    if Pen.Color = clNone then C.Pen.Style := psClear;
    if Brush.Color = clNone then C.Brush.Style := bsClear;

    if Length(Pts) > 1 then
    begin
      if not Self.HasBrush then
        C.PolyLine(Pts)
      else
      if XChanged and YChanged then
        C.Polygon(Pts)
      else
      if Self.HasPen then
        C.PolyLine(Pts);
    end else
    if Self.HasPen then
    begin
      C.MoveTo(Pts[0].x, Pts[0].y);
      C.LineTo(Pts[0].x + 1, Pts[0].y);
    end;
  end;

  if (Length(Pts) > 2) and (Self.HasGradient or Self.HasPicture) then
  begin
    DR := GetBounds;
    TSCDeUtils.ZoomRect(DR, Zoom);

    R := TSCDeUtils.Rect(DR);

    if not IsRectEmpty(R) then
    begin
      Rgn := CreatePolyRegion(Pts);

      if Rgn <> 0 then
      begin
        try
          SR := ExtSelectClipRgn(C.Handle, Rgn, RGN_COPY);
          DeleteObject(Rgn);
          Rgn := 0;

          if SR <> NULLREGION then
          begin
            if Self.HasGradient then
              Self.Gradient.Paint(C, R);

            if Self.HasPicture then
              Self.DrawPicture(C, R);

            if Self.HasPen then
            begin
              Pen.AssignTo(C.Pen);

              Brush.AssignTo(C.Brush);
              C.Brush.Style := bsClear;

              C.PolyLine(Pts);
            end;
          end;
        finally
          SelectClipRgn(C.Handle, 0);
          if Rgn <> 0 then
            DeleteObject(Rgn);
        end;
      end;
    end;
  end;
end;

function TSCDePolygon.PointOnShape(P: TDoublePoint; Fuzz: Double;
  Editing: Boolean): Boolean;
var
  L: TList;
  I: Integer;
  X, Y: Double;
  R: TDoubleRect;
  Pts: TDPointArray;
  Pt, Pt2: TSCDePoint;
  P1, P2: TDoublePoint;
  AngleChanged: Boolean;
  Angle, PrevAngle: Double;
begin
  if Fuzz < 0.0 then Fuzz := 0.0;

  Result := False;

  if PointCount = 1 then
  begin
    P1 := Points[0].Point;

    if Fuzz = 0 then
      Result := (P1.x = P.x) and (P1.y = P.y)
    else begin
      R.Left := P1.x - (Fuzz / 2);
      R.Top := P1.y - (Fuzz / 2);

      R.Right := R.Left + Fuzz;
      R.Bottom := R.Top + Fuzz;

      Result := TSCDeUtils.PtInRect(R, P);
    end;
  end else
  if PointCount > 1 then
  begin
    L := TList.Create;
    try
      Pt := Points[0];
      L.Add(Pt);

      for I := 1 to PointCount-1 do
      begin
        Pt2 := Points[I];
        if (Pt.x = Pt2.x) and (Pt.y = Pt2.y) then
          Continue;

        L.Add(Pt2);
        Pt := Pt2;
      end;

      if L.Count > 1 then
      begin
        SetLength(Pts, L.Count);

        PrevAngle := 0;
        AngleChanged := False;

        Pt := TSCDePoint(L[0]);

        P1.x := Pt.x;
        P1.y := Pt.y;

        Pts[0] := P1;

        L.Add(L[0]); // combine Points[Last] and Points[0]

        for I := 1 to L.Count-1 do
        begin
          Pt := TSCDePoint(L[I]);

          P2.x := Pt.x;
          P2.y := Pt.y;

          if (P1.x = P2.x) and (P1.y = P2.y) then
            Continue;

          Result := TSCDeUtils.NearLine(P, P1, P2, Fuzz);
          if Result then Exit;

          if not AngleChanged then
          begin
            X := P2.x - P1.x;
            Y := P2.y - P1.y;

            Angle := -1.0;

            if Y <> 0 then
            begin
              Angle := X / Y;
              if Angle < 0.0 then Angle := 360.0 - Angle;
            end;

            AngleChanged := ((I > Low(Pts) + 1) and (PrevAngle <> Angle));
          end;

          P1 := P2;
          if I < L.Count-1 then
            Pts[I] := P2;
        end;

        if AngleChanged and (Brush.Style <> bsClear) and (Brush.Color <> clNone) then
          Result := TSCDeUtils.PtInPolygon(Pts, P);
      end;
    finally
      L.Free;
    end;
  end;
end;

procedure TSCDePolygon.SaveToXML(Node: TSCDomElement);
var
  Prop: TSCDomElement;
begin
  inherited SaveToXML(Node);

  Prop := TSCDomElement.Create(Node);
  Prop.Name := 'brush';
  Node.AddElement(Prop);

  Brush.SaveToXML(Prop);

  Prop := TSCDomElement.Create(Node);
  Prop.Name := 'pen';
  Node.AddElement(Prop);

  Pen.SaveToXML(Prop);

  SaveBoundsToXML(Node);
end;

function TSCDePolygon.CreatePolyRegion(const Points: array of TPoint): HRgn;
begin
  Result := CreatePolygonRgn(PPoints(@Points)^, Length(Points), ALTERNATE);
end;

function TSCDePolygon.DefaultClosed: Boolean;
begin
  Result := True;
end;

{ TSCDeArc }

procedure TSCDeArc.CalculateAngles(StartC, EndC: Boolean);
var
  W, H: Double;
  DR: TDoubleRect;
  Center, C1, C2: TDoublePoint;
begin
  if not InDestroy and (StartC or EndC) and
    ((FC1 <> nil) or (FC2 <> nil)) then
  begin
    DR := GetBounds;

    Center.x := (DR.Right - DR.Left)/2;
    Center.y := (DR.Bottom - DR.Top)/2;

    W := Abs(DR.Right - DR.Left);
    H := Abs(DR.Bottom - DR.Top);

    if StartC then
    begin
      FStartAngle := 0.0;

      if FC1 <> nil then
      begin
        C1 := FC1.Point;

        C1.x := C1.x - Center.x;
        C1.y := C1.y - Center.y;

        if H <> 0.0 then C1.x := C1.x / (W/H);

        FStartAngle := TSCDeUtils.AngleOfPoint(C1);
        if FStartAngle > 360 then
          FStartAngle := FStartAngle - Math.Floor(FStartAngle/360)*360;
      end;
    end;

    if EndC then
    begin
      FEndAngle := 0.0;

      if FC2 <> nil then
      begin
        C2 := FC2.Point;

        C2.x := C2.x - Center.x;
        C2.y := C2.y - Center.y;

        if H <> 0.0 then C2.x := C2.x / (W/H);

        FEndAngle := TSCDeUtils.AngleOfPoint(C2);
        if FEndAngle > 360 then
          FEndAngle := FEndAngle - Math.Floor(FEndAngle/360)*360;
      end;
    end;
  end;
end;

procedure TSCDeArc.ClearArcPoints;
var
  I: Integer;
  P: PDoublePoint;
begin
  if FArcPoints <> nil then
    for I := FArcPoints.Count-1 downto 0 do
    begin
      P := FArcPoints[I];
      FArcPoints.Delete(I);

      Dispose(P);
    end;
end;

function TSCDeArc.ControlAt(P: TDoublePoint; Fuzz: Double): Integer;
var
  R: TDoubleRect;
begin
  Result := -1;
  if ControlCount > 0 then
  begin
    R := GetBounds;
    Result := inherited ControlAt(TSCDeUtils.Point(P.x - R.Left, P.y - R.Top), Fuzz);
  end;
end;

procedure TSCDeArc.ControlChanged(C: TSCDeControl);
begin
  if not InControlUpdate then
  begin
    CalculateAngles(C = FC1, C = FC2);
    UpdateControlCoords(C = FC1, C = FC2);
  end;
end;

procedure TSCDeArc.ControlRemoved(C: TSCDeControl);
begin
  inherited ControlRemoved(C);
  if C = FC1 then FC1 := nil;
  if C = FC2 then FC2 := nil;
end;

constructor TSCDeArc.Create(ASurface: TSCDeSurface;
  AOwner: TSCDeContainer);
begin
  FArcPoints := TList.Create;

  FStyle := scasArc;

  FStartAngle := 0.0;
  FEndAngle   := 0.0;

  BeginUpdate;
  try
    BeginControlUpdate;
    try
      inherited Create(ASurface, AOwner);
      SetShapeStyle(ShapeStyle + [scssCanRotate, scssUsesControls,
        scssUsesBrush] - [scssUsesGradient]);

      SetExtraHandles(False);
      UpdateExtraHandles;

      FC1 := TSCDeControl.Create(Self);
      FC2 := TSCDeControl.Create(Self);
    finally
      EndControlUpdate;
    end;
  finally
    EndUpdate;
  end;
end;

destructor TSCDeArc.Destroy;
begin
  ClearArcPoints;
  FreeAndNil(FArcPoints);
  inherited Destroy;
end;

procedure TSCDeArc.DrawSelection(C: TCanvas; X, Y: Double;
  const DragPoint: TSCDeDragPoint; const Data: TSCDeSelectionData;
  Zoom: Double);
var
  R, PR: TRect;
  P: TSCDePoint;
  DR: TDoubleRect;
  Ctrl: TSCDeControl;
  OutCl, InCl: TColor;
  I, OX, OY, AX, AY: Integer;
  Center, P1, P2: TDoublePoint;
  XRadius, YRadius, TempAngle,
  StartAngle, EndAngle: Double;
begin
  if Data.LineColor <> clNone then
  begin
    DR := GetBounds;

    with DragPoint do
      if (PointType = scptPoint) and ((Offset.x <> 0) or (Offset.y <> 0)) then
        case DragPoint.PartIndex of
          0:
          begin
            DR.Left := DR.Left + Offset.x;
            DR.Top := DR.Top + Offset.y;
          end;
          1:
          begin
            DR.Right := DR.Right + Offset.x;
            DR.Top := DR.Top + Offset.y;
          end;
          2:
          begin
            DR.Right := DR.Right + Offset.x;
            DR.Bottom := DR.Bottom + Offset.y;
          end;
          3:
          begin
            DR.Left := DR.Left + Offset.x;
            DR.Bottom := DR.Bottom + Offset.y;
          end;
        end;

    TSCDeUtils.ZoomRect(DR, Zoom);
    TSCDeUtils.OffsetRect(DR, X, Y);

    if DR.Right < DR.Left then TSCDeUtils.Swap(DR.Left, DR.Right);
    if DR.Bottom < DR.Top then TSCDeUtils.Swap(DR.Top, DR.Bottom);

    R  := TSCDeUtils.Rect(DR);

    with C do
    begin
      Brush.Style := bsClear;

      Pen.Style := Data.LineStyle;
      Pen.Mode  := Data.LineMode;
      Pen.Color := Data.LineColor;
      Pen.Width := 1;
    end;

    P1.x := DR.Left + Zoom*FC1.x;
    P1.y := DR.Top + Zoom*FC1.y;

    P2.x := DR.Left + Zoom*FC2.x;
    P2.y := DR.Top + Zoom*FC2.y;

    if (DragPoint.PointType = scptPoint) and
      ((DragPoint.Offset.x <> 0) or (DragPoint.Offset.y <> 0)) then
    begin
      Center.x := DR.Left + (DR.Right - DR.Left)/2;
      Center.y := DR.Top + (DR.Bottom - DR.Top)/2;

      XRadius := Center.x - DR.Left;
      YRadius := Center.y - DR.Top;

      StartAngle := FStartAngle;
      EndAngle := FEndAngle;

      if StartAngle > EndAngle then
      begin
        TempAngle  := EndAngle;
        EndAngle   := StartAngle;
        StartAngle := TempAngle;
      end;

      P1.x := Center.x + XRadius*Cos(StartAngle*Pi/180);
      P1.y := Center.y - YRadius*Sin(StartAngle*Pi/180);

      P2.x := Center.x + XRadius*Cos(EndAngle*Pi/180);
      P2.y := Center.y - YRadius*Sin(EndAngle*Pi/180);
    end else
    if (DragPoint.PointType = scptControl) and
      ((DragPoint.Offset.x <> 0) or (DragPoint.Offset.y <> 0)) then
    begin
      if DragPoint.PartIndex = 0 then
      begin
        P1.x := P1.x + DragPoint.Offset.x;
        P1.y := P1.y + DragPoint.Offset.y;
      end else
      begin
        P2.x := P2.x + DragPoint.Offset.x;
        P2.y := P2.y + DragPoint.Offset.y;
      end;
    end;

    if (R.Left < R.Right) and (R.Top = R.Bottom) then
      TSCDeGraphicUtils.Line(C, Point(R.Left, R.Top), Point(R.Right, R.Top))
    else
    if (R.Left = R.Right) and (R.Top < R.Bottom) then
      TSCDeGraphicUtils.Line(C, Point(R.Left, R.Top), Point(R.Left, R.Bottom))
    else
    if (R.Left < R.Right) and (R.Top < R.Bottom) then
    begin
      C.Brush.Style := bsClear;

      if FStyle = scasArc then
        C.Arc(R.Left, R.Top, R.Right, R.Bottom,
          TSCDeUtils.Round(P1.x), TSCDeUtils.Round(P1.y),
          TSCDeUtils.Round(P2.x), TSCDeUtils.Round(P2.y))
      else if FStyle = scasPie then
        C.Pie(R.Left, R.Top, R.Right, R.Bottom,
          TSCDeUtils.Round(P1.x), TSCDeUtils.Round(P1.y),
          TSCDeUtils.Round(P2.x), TSCDeUtils.Round(P2.y))
      else
        C.Chord(R.Left, R.Top, R.Right, R.Bottom,
          TSCDeUtils.Round(P1.x), TSCDeUtils.Round(P1.y),
          TSCDeUtils.Round(P2.x), TSCDeUtils.Round(P2.y));
    end;
  end;

  OutCl := Data.PointOutColor;
  InCl  := Data.PointInColor;

  if OutCl = clNone then OutCl := InCl;

  if (DragPoint.PartIndex = -1) and ((InCl <> clNone) or
    (OutCl <> clNone)) and (Data.PointSize > 1) then
  begin
    OX := -(Data.PointSize div 2);
    OY := OX;

    for I := 0 to PointCount-1 do
    begin
      P := Points[I];

      AX := TSCDeUtils.Round(X + Zoom*P.x);
      AY := TSCDeUtils.Round(Y + Zoom*P.y);

      PR := Rect(AX, AY, AX + Data.PointSize, AY + Data.PointSize);
      OffsetRect(PR, OX, OY);

      if not IsSelected(P) then
        DrawPoint(C, PR, InCl, OutCl)
      else if OutCl <> clNone then
        DrawPoint(C, PR, OutCl, OutCl)
      else
        DrawPoint(C, PR, InCl, InCl);
    end;
  end;

  OutCl := Data.CtrlOutColor;
  InCl  := Data.CtrlInColor;

  if OutCl = clNone then OutCl := InCl;

  if (DragPoint.PartIndex = -1) and ((InCl <> clNone) or
    (OutCl <> clNone)) and (Data.PointSize > 1) then
  begin
    DR := GetBounds;

    OX := -(Data.PointSize div 2);
    OY := OX;

    for I := 0 to ControlCount-1 do
    begin
      Ctrl := Controls[I];

      AX := TSCDeUtils.Round(X + Zoom*(DR.Left + Ctrl.x));
      AY := TSCDeUtils.Round(Y + Zoom*(DR.Top + Ctrl.y));

      PR := Rect(AX, AY, AX + Data.PointSize, AY + Data.PointSize);
      OffsetRect(PR, OX, OY);

      DrawPoint(C, PR, InCl, OutCl);
    end;
  end;
end;

procedure TSCDeArc.GenerateArcPoints;
var
  R: TRect;
  P: PDoublePoint;
  DR: TDoubleRect;
  Center: TDoublePoint;
  I, StartAngle, EndAngle: Integer;
  TempAngle, XRadius, YRadius: Double;
begin
  if not InDestroy and (FArcPoints <> nil) then
  begin
    ClearArcPoints;

    DR := GetBounds;
    R := TSCDeUtils.Rect(DR);

    Center.x := DR.Left + (DR.Right - DR.Left)/2;
    Center.y := DR.Top + (DR.Bottom - DR.Top)/2;

    XRadius := Center.x - DR.Left;
    YRadius := Center.y - DR.Top;

    StartAngle := TSCDeUtils.Round(FStartAngle);
    EndAngle := TSCDeUtils.Round(FEndAngle);

    if EndAngle <= StartAngle then
      Inc(EndAngle, 360);

    for I := StartAngle to EndAngle do
    begin
      TempAngle := I*Pi/180;

      New(P);
      P^.x := Center.x + XRadius*Cos(TempAngle);
      P^.y := Center.y - YRadius*Sin(TempAngle);

      FArcPoints.Add(P);
    end;
  end;
end;

function TSCDeArc.InRect(const R: TDoubleRect): Boolean;
var
  I: Integer;
  DR: TDoubleRect;
  Pp: PDoublePoint;
  Center, P1, P2: TDoublePoint;
begin
  Result := False;

  if (FArcPoints <> nil) and (FArcPoints.Count > 0) then
  begin
    if FArcPoints.Count = 1 then
    begin
      Pp := FArcPoints[0];

      P1.x := Pp^.x;
      P1.y := Pp^.y;

      Result := TSCDeUtils.PtInRect(R, P1);
      if Result then Exit;
    end;

    for I := 0 to FArcPoints.Count-2 do
    begin
      Pp := FArcPoints[I];

      P1.x := Pp^.x;
      P1.y := Pp^.y;

      Pp := FArcPoints[I + 1];

      P2.x := Pp^.x;
      P2.y := Pp^.y;

      Result := TSCDeUtils.DoLineIntersectRect(P1, P2, R);
      if Result then Exit;
    end;

    DR := GetBounds;

    if DR.Left = DR.Right then
    begin
      P1.x := R.Left;
      P1.y := R.Top;

      P2.x := R.Left;
      P2.y := R.Bottom;

      Result := TSCDeUtils.DoLineIntersectRect(P1, P2, R);
    end else
    if DR.Bottom = DR.Top then
    begin
      P1.x := R.Left;
      P1.y := R.Top;

      P2.x := R.Right;
      P2.y := R.Top;

      Result := TSCDeUtils.DoLineIntersectRect(P1, P2, R);
    end;

    if not Result and (FStyle = scasChord) and (FArcPoints.Count > 2) then
    begin
      Pp := FArcPoints[0];
      P1.x := Pp^.x;
      P1.y := Pp^.y;

      Pp := FArcPoints[FArcPoints.Count-1];
      P2.x := Pp^.x;
      P2.y := Pp^.y;

      Result := TSCDeUtils.DoLineIntersectRect(P1, P2, R);
    end;

    if not Result and (FStyle = scasPie) and (FArcPoints.Count > 2) then
    begin
      Center.x := DR.Left + (DR.Right - DR.Left)/2;
      Center.y := DR.Top + (DR.Bottom - DR.Top)/2;

      Pp := FArcPoints[0];
      P1.x := Pp^.x;
      P1.y := Pp^.y;

      Result := TSCDeUtils.DoLineIntersectRect(P1, Center, R);

      if not Result then
      begin
        Pp := FArcPoints[FArcPoints.Count-1];
        P1.x := Pp^.x;
        P1.y := Pp^.y;

        Result := TSCDeUtils.DoLineIntersectRect(P1, Center, R);
      end;
    end;
  end;
end;

procedure TSCDeArc.LoadBoundsFromXML(Node: TSCDomElement);
var
  S: String;
  Prop: TSCDomElement;
begin
  BeginUpdate;
  try
    Prop := Node.ElementByName('rect');
    if Prop <> nil then
    begin
      FStartAngle := 0;
      FEndAngle := 0;

      S := GetProperty(Prop, 'start_angle');
      if S <> '' then FStartAngle := StrToFloat(S);

      S := GetProperty(Prop, 'end_angle');
      if S <> '' then FEndAngle := StrToFloat(S);
    end;

    inherited LoadBoundsFromXML(Node);

    UpdateControlCoords(True, True);
  finally
    EndUpdate;
  end;
end;

procedure TSCDeArc.LoadFromXML(Node: TSCDomElement);
var
  S: String;
  Prop: TSCDomElement;
begin
  Changing;
  BeginUpdate;
  try
    inherited LoadFromXML(Node);

    S := GetProperty(Node, 'style');
    if S <> '' then FStyle := TSCDeArcStyle(StrToIntDef(S, Integer(scasArc)));

    Prop := Node.ElementByName('pen');
    if Prop <> nil then Pen.LoadFromXML(Prop);

    Prop := Node.ElementByName('brush');
    if Prop <> nil then Brush.LoadFromXML(Prop);

    LoadBoundsFromXML(Node);
  finally
    EndUpdate;
  end;
end;

procedure TSCDeArc.Paint(C: TCanvas; X, Y: Integer; Zoom: Double);
var
  R: TRect;
  DR: TDoubleRect;
  P1, P2: TDoublePoint;
begin
  DR := GetBounds;

  TSCDeUtils.ZoomRect(DR, Zoom);
  TSCDeUtils.OffsetRect(DR, X, Y);

  R := TSCDeUtils.Rect(DR);

  if (Self.HasPen or ((FStyle <> scasArc) and Self.HasBrush)) and
    ((R.Left <> R.Right) or (R.Top <> R.Bottom)) then
  begin
    Pen.AssignTo(C.Pen);
    if Pen.Color = clNone then C.Pen.Style := psClear;

    Brush.AssignTo(C.Brush);
    if Brush.Color = clNone then C.Brush.Style := bsClear;

    if R.Left = R.Right then
      Inc(R.Right)
    else if R.Top = R.Bottom then
      Inc(R.Bottom);

    P1.x := DR.Left + Zoom*FC1.x;
    P1.y := DR.Top + Zoom*FC1.y;

    P2.x := DR.Left + Zoom*FC2.x;
    P2.y := DR.Top + Zoom*FC2.y;

    if FStyle = scasArc then
      C.Arc(R.Left, R.Top, R.Right, R.Bottom,
        TSCDeUtils.Round(P1.x), TSCDeUtils.Round(P1.y),
        TSCDeUtils.Round(P2.x), TSCDeUtils.Round(P2.y))
    else if FStyle = scasPie then
      C.Pie(R.Left, R.Top, R.Right, R.Bottom,
        TSCDeUtils.Round(P1.x), TSCDeUtils.Round(P1.y),
        TSCDeUtils.Round(P2.x), TSCDeUtils.Round(P2.y))
    else
      C.Chord(R.Left, R.Top, R.Right, R.Bottom,
        TSCDeUtils.Round(P1.x), TSCDeUtils.Round(P1.y),
        TSCDeUtils.Round(P2.x), TSCDeUtils.Round(P2.y));
  end;
end;

procedure TSCDeArc.PointChanged(P: TSCDePoint);
begin
  inherited PointChanged(P);
  UpdateControlCoords(True, True);
end;

function TSCDeArc.PointOnShape(P: TDoublePoint; Fuzz: Double;
  Editing: Boolean): Boolean;
var
  I: Integer;
  DR: TDoubleRect;
  Pp: PDoublePoint;
  P1, P2: TDoublePoint;
begin
  Result := False;
  if Fuzz < 0.0 then Fuzz := 0.0;

  DR := GetBounds;

  if (DR.Left = DR.Right) or (DR.Top = DR.Bottom) then
  begin
    TSCDeUtils.InflateRect(DR, Fuzz, Fuzz);
    Result := TSCDeUtils.PtInRect(DR, TSCDeUtils.Point(P.x, P.y));
  end else
  if FArcPoints <> nil then
  begin
    for I := 0 to FArcPoints.Count-2 do
    begin
      Pp := FArcPoints[I];

      P1.x := Pp^.x;
      P1.y := Pp^.y;

      Pp := FArcPoints[I + 1];

      P2.x := Pp^.x;
      P2.y := Pp^.y;

      if TSCDeUtils.NearLine(P, P1, P2, Fuzz) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

procedure TSCDeArc.SaveBoundsToXML(Node: TSCDomElement);
var
  Prop: TSCDomElement;
begin
  inherited SaveBoundsToXML(Node);

  Prop := Node.ElementByName('rect');
  if Prop = nil then
  begin
    Prop := TSCDomElement.Create(Node);
    Prop.Name := 'rect';
    Node.AddElement(Prop);
  end;

  AddProperty(Prop, 'start_angle',   FloatToStr(FStartAngle));
  AddProperty(Prop, 'end_angle',    FloatToStr(FEndAngle));
end;

procedure TSCDeArc.SaveToXML(Node: TSCDomElement);
var
  Prop: TSCDomElement;
begin
  inherited SaveToXML(Node);

  SaveBoundsToXML(Node);

  AddProperty(Node, 'style', IntToStr(Integer(FStyle)));

  Prop := TSCDomElement.Create(Node);
  Prop.Name := 'pen';
  Node.AddElement(Prop);

  Pen.SaveToXML(Prop);

  Prop := TSCDomElement.Create(Node);
  Prop.Name := 'brush';
  Node.AddElement(Prop);

  Brush.SaveToXML(Prop);
end;

procedure TSCDeArc.SetStyle(Value: TSCDeArcStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed;
  end;
end;

procedure TSCDeArc.UpdateControlCoords(StartC, EndC: Boolean);
var
  DR: TDoubleRect;
  Center, P: TDoublePoint;
  XRadius, YRadius: Double;
begin
  if not (InDestroy or InControlUpdate) and (StartC or EndC) and
    ((FC1 <> nil) or (FC2 <> nil)) then
  begin
    BeginUpdate;
    try
      BeginControlUpdate;
      try
        DR := GetBounds;

        Center.x := DR.Left + (DR.Right - DR.Left)/2;
        Center.y := DR.Top + (DR.Bottom - DR.Top)/2;

        XRadius := Center.x - DR.Left;
        YRadius := Center.y - DR.Top;

        if StartC and (FC1 <> nil) then
        begin
          P.x := Center.x + XRadius*Cos(FStartAngle*Pi/180);
          P.y := Center.y - YRadius*Sin(FStartAngle*Pi/180);

          P.x := P.x - DR.Left;
          P.y := P.y - DR.Top;

          FC1.SetPosition(P.x, P.y);
        end;

        if EndC and (FC2 <> nil) then
        begin
          P.x := Center.x + XRadius*Cos(FEndAngle*Pi/180);
          P.y := Center.y - YRadius*Sin(FEndAngle*Pi/180);

          P.x := P.x - DR.Left;
          P.y := P.y - DR.Top;

          FC2.SetPosition(P.x, P.y);
        end;
      finally
        EndControlUpdate;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSCDeArc.UpdateRegion;
begin
  GenerateArcPoints;
end;

{ TSCDeCustomPolyPolygon }

constructor TSCDeCustomPolyPolygon.Create(ASurface: TSCDeSurface;
  AOwner: TSCDeContainer);
begin
  inherited Create(ASurface, AOwner);
  FCombined := True;
  SetShapeStyle(ShapeStyle + [scssUsesBrush, scssUsesPen,
    scssUsesFont]);
end;

procedure TSCDeCustomPolyPolygon.MoveBy(X, Y: Double);
var
  I: Integer;
  S: TSCDeShapeBase;
begin
  if ItemCount > 0 then
  begin
    BeginUpdate;
    try
      BeginNotifyLock;
      try
        NotifyUndo(Self, scacSizing, '');

        for I := 0 to ItemCount-1 do
        begin
          S := Items[I];
          S.MoveBy(X, Y);
        end;
      finally
        EndNotifyLock;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSCDeCustomPolyPolygon.Paint(C: TCanvas; X, Y: Integer;
  Zoom: Double);
var
  Pp: PPoint;
  Ip: Pointer;
  Pa: TDPointArray;
  L, L2, I, J: Integer;
  Points: array of TPoint;
  Integers: array of Integer;
begin
  if ItemCount > 0 then
  begin
    Pen.AssignTo(C.Pen);
    Brush.AssignTo(C.Brush);

    if Pen.Color = clNone then C.Pen.Style := psClear;
    if Brush.Color = clNone then C.Brush.Style := bsClear;

    SetLength(Points, 0);
    SetLength(Integers, 0);

    SetLength(Pa, 0);

    for I := 0 to ItemCount-1 do
    begin
      if not (Items[I] is TSCDePolygon) then
        Items[I].Paint(C, X, Y, Zoom)
      else begin
        Pa := TSCDePolygon(Items[I]).GetPointArray;

        if Pa <> nil then
        begin
          L := Length(Pa);

          if L > 0 then
          begin
            SetLength(Integers, Length(Integers) + 1);
            Integers[Length(Integers)-1] := L;

            L2 := Length(Points);
            SetLength(Points, L2 + L);

            for J := 0 to L-1 do
            begin
              Points[L2 + J].x := TSCDeUtils.Round(X + Zoom*Pa[J].x);
              Points[L2 + J].y := TSCDeUtils.Round(Y + Zoom*Pa[J].y);
            end;
          end;
        end;
      end;
    end;

    if (Length(Integers) > 0) and (Length(Points) > 0) then
    begin
      Pp := @Points[0];
      Ip := @Integers[0];

      if FCombined then
        PolyPolygon(C.Handle, Pp^, Ip^, Length(Integers))
      else
        PolyPolyline(C.Handle, Pp^, Ip^, Length(Integers));
    end;
  end;
end;

procedure TSCDeCustomPolyPolygon.PaintIn(C: TCanvas; X, Y: Integer; Zoom: Double;
  R: TDoubleRect);
var
  Pp: PPoint;
  Ip: Pointer;
  Br: TDoubleRect;
  Pa: TDPointArray;
  L, L2, IL, I, J: Integer;
  Points: array of TPoint;
  Integers: array of Integer;
begin
  if ItemCount > 0 then
  begin
    Pen.AssignTo(C.Pen);
    Brush.AssignTo(C.Brush);

    if Pen.Color = clNone then C.Pen.Style := psClear;
    if Brush.Color = clNone then C.Brush.Style := bsClear;

    SetLength(Points, 0);
    SetLength(Integers, 0);

    SetLength(Pa, 0);

    Br := TSCDeUtils.Rect(R.Left, R.Top, R.Left, R.Top);

    for I := 0 to ItemCount-1 do
    begin
      if not (Items[I] is TSCDePolygon) then
        Items[I].PaintIn(C, X, Y, Zoom, R)
      else begin
        Pa := TSCDePolygon(Items[I]).GetPointArray;

        if Pa <> nil then
        begin
          L := Length(Pa);

          if L > 0 then
          begin
            IL := Length(Integers);

            SetLength(Integers, IL + 1);
            Integers[IL] := L;

            L2 := Length(Points);
            SetLength(Points, L2 + L);

            IL := Length(Integers);
            
            if IL = 1 then
            begin
              Br.Left := Pa[0].x;
              Br.Right := Pa[0].x;
              Br.Top := Pa[0].y;
              Br.Bottom := Pa[0].y;
            end;

            for J := 0 to L-1 do
            begin
              if (IL > 1) or (J > 0) then
              begin
                if Pa[J].x < Br.Left then Br.Left := Pa[J].x;
                if Pa[J].x > Br.Right then Br.Right := Pa[J].x;
                if Pa[J].y < Br.Top then Br.Top := Pa[J].y;
                if Pa[J].y > Br.Bottom then Br.Bottom := Pa[J].y;
              end;
              
              Points[L2 + J].x := TSCDeUtils.Round(X + Zoom*Pa[J].x);
              Points[L2 + J].y := TSCDeUtils.Round(Y + Zoom*Pa[J].y);
            end;
          end;
        end;
      end;
    end;

    IL := Length(Integers);

    if (IL > 0) and (Length(Points) > 0) and (Self.InRect(R) or
      TSCDeUtils.IntersectRect(Br, Br, R)) then
    begin
      Pp := @Points[0];
      Ip := @Integers[0];

      if FCombined then
        PolyPolygon(C.Handle, Pp^, Ip^, IL)
      else
        PolyPolyline(C.Handle, Pp^, Ip^, IL);
    end;
  end;
end;

procedure TSCDeCustomPolyPolygon.Resize(NewWidth, NewHeight: Double);
var
  I: Integer;
  CR, R: TDoubleRect;
  S: TSCDeShapeBase;
  W, H, L, T, ZoomX, ZoomY: Double;
begin
  if not InPointUpdate and (ItemCount > 0) then
  begin
    CR := GetBounds;

    W := CR.Right - CR.Left;
    if W < 0 then W := 0;

    H := CR.Bottom - CR.Top;
    if H < 0 then H := 0;

    if NewWidth < 0 then NewWidth := W;
    if NewHeight < 0 then NewHeight := H;

    if (W <> NewWidth) or (H <> NewHeight) then
    begin
      ZoomX := 1.0;
      if W <> 0.0 then ZoomX := NewWidth / W;

      ZoomY := 1.0;
      if H <> 0.0 then ZoomY := NewHeight / H;

      if (ZoomX <> 1.0) or (ZoomY <> 1.0) then
      begin
        BeginUpdate;
        try
          BeginNotifyLock;
          try
            NotifyUndo(Self, scacSizing, '');

            for I := 0 to ItemCount-1 do
            begin
              S := Items[I];
              R := S.GetBounds;

              W := R.Right - R.Left;
              if W < 0 then W := 0;

              W := W * ZoomX;

              H := R.Bottom - R.Top;
              if H < 0 then H := 0;

              H := H * ZoomY;

              L := CR.Left + (ZoomX * (R.Left - CR.Left));
              T := CR.Top + (ZoomY * (R.Top - CR.Top));

              S.SetBounds(L, T, W, H);
            end;
          finally
            EndNotifyLock;
          end;
        finally
          EndUpdate;
        end;
      end;
    end;
  end;
end;

procedure TSCDeCustomPolyPolygon.SetCombined(Value: Boolean);
begin
  if FCombined <> Value then
  begin
    FCombined := Value;
    Changed;
  end;
end;

{ TSCDeText }

procedure TSCDeText.BrushChanged(Sender: TObject);
begin
  inherited BrushChanged(Sender);
  UpdateCharacters; 
end;

constructor TSCDeText.Create(ASurface: TSCDeSurface;
  AOwner: TSCDeContainer);
begin
  inherited Create(ASurface, AOwner);
  SetShapeStyle(ShapeStyle + [scssUsesCaption, scssUsesBrush,
    scssUsesPen, scssUsesFont] - [scssIsContainer]);

  SetCaption('x');
end;

procedure TSCDeText.LoadFromXML(Node: TSCDomElement);
var
  S: String;
  Prop: TSCDomElement;
begin
  Changing;
  BeginUpdate;
  try
    inherited LoadFromXML(Node);

    Prop := Node.ElementByName('brush');
    if Prop <> nil then Brush.LoadFromXML(Prop);

    Prop := Node.ElementByName('pen');
    if Prop <> nil then Pen.LoadFromXML(Prop);

    Prop := Node.ElementByName('font');
    if Prop <> nil then Font.LoadFromXML(Prop);

    S := GetProperty(Node, 'caption');
    SetCaption(S);

    LoadBoundsFromXML(Node);
  finally
    EndUpdate;
  end;
end;

procedure TSCDeText.Paint(C: TCanvas; X, Y: Integer; Zoom: Double);
var
  I: Integer;
  R: TDoubleRect;
  S: TSCDeShapeBase;
begin
  if ItemCount > 0 then
  begin
    R := TSCDeUtils.Rect(DrawRect);

    for I := 0 to ItemCount-1 do
    begin
      S := Items[I];
      if S.Visible and not S.InDestroy and S.InRect(R) then
        S.Paint(C, X, Y, Zoom);
    end;
  end;
end;

procedure TSCDeText.PaintIn(C: TCanvas; X, Y: Integer; Zoom: Double;
  R: TDoubleRect);
var
  I: Integer;
  S: TSCDeShapeBase;
begin
  if (ItemCount > 0) and not TSCDeUtils.IsRectEmpty(R) then
    for I := 0 to ItemCount-1 do
    begin
      S := Items[I];
      if S.Visible and not S.InDestroy and S.InRect(R) then
        S.PaintIn(C, X, Y, Zoom, R);
    end;
end;

procedure TSCDeText.PenChanged(Sender: TObject);
begin
  inherited PenChanged(Sender);
  UpdateCharacters;
end;

procedure TSCDeText.RecreateCharacters(ACaption: String);
var
  C: Char;
  L: TList;
  Str: String;
  Ch: TSCDeChar;
  TTF: TSCDeTTF;
  ALeft: Double;
  Pt: TSCDePoint;
  P: TDoublePoint;
  S: TSCDeStrokes;
  I, J, K: Integer;
  Poly: TSCDePolygon;
  St: TSCDeFontStroke;
  R, SR, CR: TDoubleRect;
begin
  BeginNotifyLock;
  try
    BeginUpdate;
    try
      SR := GetBounds;

      Clear;

      Str := ACaption;

      if Length(Str) > 0 then
      begin
        TTF := TSCDeTTF.Create;
        try
          Self.Font.AssignTo(TTF.Font);

          ALeft := 0.0;

          L := TList.Create;
          try
            for I := 1 to Length(Str) do
            begin
              C := Str[I];

              S := TTF.GetCharacterGlyphs(Ord(C));
              try
                if S <> nil then
                begin
                  R := S.Bounds;

                  if S.Count > 0 then
                  begin
                    Ch := TSCDeChar.Create(nil, nil);
                    L.Add(Ch);

                    Ch.BeginUpdate;
                    try
                      P := TSCDeUtils.Point(0, 0);

                      Poly := nil;
                      for J := 0 to S.Count-1 do
                      begin
                        St := S.Stroke[J];

                        if (Poly = nil) or ((J > 0) and ((St.P1.x <> P.x) or (St.P1.y <> P.y))) then
                        begin
                          Poly := TSCDePolygon.Create(Self.Surface, Ch);
                          Poly.Add(St.P1.x, St.P1.y);
                        end;

                        P := St.P2;
                        Poly.Add(St.P2.x, St.P2.y);
                      end;

                      Ch.MoveBy(ALeft, 0);

                      Ch.Pen.Assign(Self.Pen);
                      Ch.Brush.Assign(Self.Brush);
                    finally
                      Ch.EndUpdate;
                    end;
                  end;

                  ALeft := ALeft + Abs(R.Left) + R.Right;
                end;
              finally
                if S <> nil then
                  S.Free;
              end;
            end;

            for I := 0 to L.Count-1 do
            begin
              Ch := TSCDeChar(L[I]);
              Self.Add(Ch);
            end;
          finally
            L.Free;
          end;

          RecalculateBounds(True);

          R := GetBounds;

          for I := 0 to ItemCount-1 do
          begin
            Ch := TSCDeChar(Items[I]);

            Ch.BeginUpdate;
            try
              Ch.BeginPointUpdate;
              try
                for J := 0 to Ch.ItemCount-1 do
                begin
                  Poly := TSCDePolygon(Ch.Items[J]);

                  for K := 0 to Poly.PointCount-1 do
                  begin
                    Pt := Poly.Points[K];

                    P := Pt.Point;
                    Pt.SetPosition(P.x, R.Bottom - P.y);
                  end;
                end;
              finally
                Ch.EndPointUpdate;
              end;
            finally
              Ch.EndUpdate;
            end;
          end;
        finally
          TTF.Free;
        end;
      end;
    finally
      EndUpdate;
    end;

    RecalculateBounds(True);

    if not InUpdate and (ItemCount > 0) then
    begin
      R := SR;
      if Self.Owner = nil then
      begin
        CR := GetBounds;

        if R.Bottom - R.Top < 28.0 then R.Bottom := R.Top + 28;
        if CR.Bottom - CR.Top <> 0 then
          R.Right := R.Left + (R.Bottom - R.Top)*((CR.Right - CR.Left) / (CR.Bottom - CR.Top));
      end;

      BeginUpdate;
      try
        SetBounds(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
      finally
        EndUpdate;
      end;
    end;
  finally
    EndNotifyLock;
  end;
end;

procedure TSCDeText.Resize(NewWidth, NewHeight: Double);
var
  Updated: Boolean;
  R, R2: TDoubleRect;
begin
  RecalculateBounds(True);
  R := GetBounds;

  Updated := False;
  if ((R.Right - R.Left <= 0) and (NewWidth > 0)) or
    ((R.Bottom - R.Top <= 0) and (NewHeight > 0)) then
  begin
    Updated := True;
    RecreateCharacters(Self.Caption);
  end;

  if NewWidth < 0.0 then NewWidth := 0.0;
  if NewHeight < 0.0 then NewHeight := 0.0;

  BeginNotifyLock;
  try
    NotifyUndo(Self, scacSizing, '');

    inherited Resize(NewWidth, NewHeight);

    if Updated then
    begin
      RecalculateBounds(True);

      R2 := GetBounds;
      MoveBy(R.Left - R2.Left, R.Top - R2.Top);
    end;
  finally
    EndNotifyLock;
  end;
end;

procedure TSCDeText.SaveToXML(Node: TSCDomElement);
var
  Prop: TSCDomElement;
begin
  inherited SaveToXML(Node);

  SaveBoundsToXML(Node);

  Prop := TSCDomElement.Create(Node);
  Prop.Name := 'brush';
  Node.AddElement(Prop);

  Brush.SaveToXML(Prop);

  Prop := TSCDomElement.Create(Node);
  Prop.Name := 'pen';
  Node.AddElement(Prop);

  Pen.SaveToXML(Prop);

  Prop := TSCDomElement.Create(Node);
  Prop.Name := 'font';
  Node.AddElement(Prop);

  Font.SaveToXML(Prop);

  AddProperty(Node, 'caption', Caption);
end;

procedure TSCDeText.SetBounds(ALeft, ATop, AWidth, AHeight: Double);
var
  R: TDoubleRect;
  Offset: TDoublePoint;
begin
  R := GetBounds;

  if (R.Left <> ALeft) or (R.Top <> ATop) or
    (R.Right - R.Left <> AWidth) or (R.Bottom - R.Top <> AHeight) then
  begin
    BeginUpdate;
    try
      BeginNotifyLock;
      try
        NotifyUndo(Self, scacSizing, '');

        Offset.x := ALeft - R.Left;
        Offset.y := ATop - R.Top;

        if (Offset.x <> 0) or (offset.y <> 0) then
        begin
          MoveBy(Offset.x, Offset.y);
          RecalculateBounds(True);
        end;

        Resize(AWidth, AHeight);
      finally
        EndNotifyLock;
      end;
    finally
      EndUpdate;
      RecalculateBounds(True);
    end;
  end;
end;

procedure TSCDeText.SetCaption(const Value: TCaption);
begin
  if Caption <> Value then
  begin
    NotifyUndo(Self, scacChanging, '');

    BeginNotifyLock;
    try
      inherited SetCaption(Value);
      RecreateCharacters(Self.Caption);
    finally
      EndNotifyLock;
    end;
  end;
end;

procedure TSCDeText.UpdateCharacters;
var
  I: Integer;
  Ch: TSCDeChar;
begin
  BeginUpdate;
  try
    BeginNotifyLock;
    try
      for I := 0 to ItemCount-1 do
        if Items[I] is TSCDeChar then
        begin
          Ch := TSCDeChar(Items[I]);

          Ch.BeginUpdate;
          try
            Ch.Pen.Assign(Self.Pen);
            Ch.Brush.Assign(Self.Brush);
          finally
            Ch.EndUpdate;
          end;
        end;
    finally
      EndNotifyLock;
    end;
  finally
    EndUpdate;
  end;
end;

{ TSCDeCustomPolyBezier }

function TSCDeCustomPolyBezier.BezierPoint(U: Double;
  P0, P1, P2, P3: TDoublePoint): TDoublePoint;
var
  U2, U3: Double;
begin
  U2 := U  * U;
  U3 := U2 * U;

  Result.x := P3.x * U3 + P2.x * U2 + P1.x * U + P0.x;
  Result.y := P3.y * U3 + P2.y * U2 + P1.y * U + P0.y;
end;

procedure TSCDeCustomPolyBezier.CalculateBeziers(Pl: TList; P0, P1, P2,
  P3: TDoublePoint; SqLimit, A, B: Double);
var
  Pp: PDoublePoint;
  PA, PB, PM: TDoublePoint;
  M, SqDistL, SqDistR, SqDistT: Double;
begin
  // M is the midpoint (in U-space)
  M := (A + B) / 2;

  // We calculate the bezier points in A, B and M. This could be further optimized
  // because now we calculate many points more than once, but this is left to the
  // interested reader
  PA := BezierPoint(A, P0, P1, P2, P3);
  PB := BezierPoint(B, P0, P1, P2, P3);
  PM := BezierPoint(M, P0, P1, P2, P3);

  // Calculate squared distances
  SqDistL := Sqr(PM.x - PA.x) + Sqr(PM.y - PA.y); // Left side dist
  SqDistR := Sqr(PB.x - PM.x) + Sqr(PB.y - PM.y); // Right side dist
  SqDistT := Sqr(PB.x - PA.x) + Sqr(PB.y - PA.y); // Total dist

  // I have added this extra check, using 3 distances, to avoid problems
  // where begin = endpoint, and where there are loops in the curve. If you
  // do not need that extra safety, you can remove all SqDistL and SqDistR
  // lines (just preserve SqDistT check).
  if (SqDistL < SqLimit) and (SqDistR < SqLimit) and (SqDistT < SqLimit) then
  begin
    New(Pp);

    Pp^.x := PB.x;
    Pp^.y := PB.y;

    Pl.Add(Pp);
  end else
  begin
    // The distance is bigger than the limit so subdivide
    CalculateBeziers(Pl, P0, P1, P2, P3, SqLimit, A, M);
    CalculateBeziers(Pl, P0, P1, P2, P3, SqLimit, M, B);
  end;
end;

procedure TSCDeCustomPolyBezier.ClearBeziers(Pl: TList);
var
  L: TList;
  I, J: Integer;
  P: PDoublePoint;
  Bh: PBezierHolder;
begin
  if (Pl <> nil) and (Pl.Count > 0) then
  begin
    for I := 0 to Pl.Count-1 do
    begin
      Bh := Pl[I];
      Pl[I] := nil;

      L := Bh^.List;
      Dispose(Bh);

      if L <> nil then
      begin
        for J := 0 to L.Count-1 do
        begin
          P := L[J];
          L[J] := nil;

          Dispose(P);
        end;

        L.Free;
      end;
    end;

    Pl.Clear;
  end;
end;

constructor TSCDeCustomPolyBezier.Create(ASurface: TSCDeSurface;
  AOwner: TSCDeContainer);
begin
  FBeziers := TList.Create;
  inherited Create(ASurface, AOwner);
  RecalculateBounds(True);

  FSegmentLimit := 3;
  SetShapeStyle(ShapeStyle + [scssUsesPointControls]);
  SetIsClosed(False);
end;

destructor TSCDeCustomPolyBezier.Destroy;
begin
  ClearBeziers(FBeziers);
  FreeAndNil(FBeziers);
  inherited Destroy;
end;

procedure TSCDeCustomPolyBezier.DrawSelection(C: TCanvas; X, Y: Double;
  const DragPoint: TSCDeDragPoint; const Data: TSCDeSelectionData;
  Zoom: Double);
var
  L: TList;
  PR: TRect;
  P: TSCDePoint;
  AIsClosed: Boolean;
  P1, P2, P3: TSCDePoint;
  Pa: array[0..3] of TPoint;
  Pd: array[0..3] of TDoublePoint;
  Psa: array[0..6] of TPoint;
  Psd: array[0..6] of TDoublePoint;
  LineCl, OutCl, InCl, Cl: TColor;
  I, J, OX, OY, AX, AY: Integer;
begin
  if PointCount > 0 then
  begin
    L := TList.Create;
    try
      P := Points[0];

      L.Add(P);

      for I := 1 to PointCount-1 do
      begin
        P := Points[I];
        L.Add(P);
      end;

      AIsClosed := False;
      
      if GetIsClosed and (L.Count > 1) then
      begin
        AIsClosed := True;
        L.Add(L[0]);
      end;

      LineCl := Data.LineColor;

      if (LineCl = clNone) and (Data.SelectionType = scdsHottrack) then
      begin
        LineCl := Data.PointOutColor;
        if LineCl = clNone then LineCl := Data.PointInColor;
      end;

      OutCl := Data.PointOutColor;
      InCl  := Data.PointInColor;

      if OutCl = clNone then OutCl := InCl;

      with C do
      begin
        Brush.Style := bsClear;

        Pen.Color := LineCl;
        Pen.Mode  := Data.LineMode;
        Pen.Style := Data.LineStyle;
        Pen.Width := 1;
      end;

      OX := -(Data.PointSize div 2);
      OY := OX;

      if LineCl <> clNone then
      begin
        if (Data.LineStyle = psClear) and (Data.SelectionType = scdsHottrack) then
          C.Pen.Style := psSolid;

        Cl := OutCl;
        if Cl = clNone then
        begin
          Cl := InCl;
          if Cl = clNone then Cl := LineCl;
        end;

        for I := 0 to L.Count-1 do
        begin
          if I < L.Count-1 then
          begin
            P1 := TSCDePoint(L[I]);
            P2 := TSCDePoint(L[I + 1]);

            Pd[0] := P1.Point;
            Pd[3] := P2.Point;

            with DragPoint do
              if (PointType = scptPoint) and
                ((Offset.x <> 0) or (Offset.y <> 0)) then
              begin
                if I = PartIndex then
                begin
                  Pd[0].x := Pd[0].x + Offset.x;
                  Pd[0].y := Pd[0].y + Offset.y;
                end else
                if (PartIndex = I + 1) or (AIsClosed and
                  (PartIndex = 0) and (I = L.Count-2)) then
                begin
                  Pd[3].x := Pd[3].x + Offset.x;
                  Pd[3].y := Pd[3].y + Offset.y;
                end;
              end;

            if (P1.Control_2 = nil) and (P2.Control_1 = nil) then
            begin
              Pa[0].x := TSCDeUtils.Round(X + Zoom*Pd[0].x);
              Pa[0].y := TSCDeUtils.Round(Y + Zoom*Pd[0].y);

              Pa[3].x := TSCDeUtils.Round(X + Zoom*Pd[3].x);
              Pa[3].y := TSCDeUtils.Round(Y + Zoom*Pd[3].y);

              Pa[1] := Pa[0];
              Pa[2] := Pa[3];

              C.PolyBezier(Pa);
            end else
            begin
              Pd[1] := Pd[0];
              Pd[2] := Pd[3];

              // first point control
              if P1.Control_2 <> nil then
              begin
                Pd[1] := P1.Control_2.Point;

                Pd[1].x := Pd[1].x + Pd[0].x;
                Pd[1].y := Pd[1].y + Pd[0].y;

                with DragPoint do
                  if (PointType = scptPointControl) and
                    (SubIndex = 1) and (P1.Index = PartIndex) and
                    ((Offset.x <> 0) or (Offset.y <> 0)) then
                  begin
                    Pd[1].x := Pd[1].x + Offset.x;
                    Pd[1].y := Pd[1].y + Offset.y;
                  end;
              end;

              // second point control
              if P2.Control_1 <> nil then
              begin
                Pd[2] := P2.Control_1.Point;

                Pd[2].x := Pd[2].x + Pd[3].x;
                Pd[2].y := Pd[2].y + Pd[3].y;

                with DragPoint do
                  if (PointType = scptPointControl) and
                    (SubIndex = 0) and (P2.Index = PartIndex) and
                    ((Offset.x <> 0) or (Offset.y <> 0)) then
                  begin
                    Pd[2].x := Pd[2].x + Offset.x;
                    Pd[2].y := Pd[2].y + Offset.y;
                  end;
              end;

              for J := Low(Pd) to High(Pd) do
              begin
                Pa[J].x := TSCDeUtils.Round(X + Zoom*Pd[J].x);
                Pa[J].y := TSCDeUtils.Round(Y + Zoom*Pd[J].y);
              end;

              C.PolyBezier(Pa);

              if DragPoint.PointType = scptPointControl then
                with DragPoint do
                begin
                  if (SubIndex = 1) and (P1.Index = PartIndex) then
                    TSCDeGraphicUtils.Line(C, Pa[1], Pa[0])
                  else
                  if (SubIndex = 0) and (P2.Index = PartIndex) then
                    TSCDeGraphicUtils.Line(C, Pa[2], Pa[3])
                  else
                  if (SubIndex = 1) and (P1.Index = PartIndex) then
                    TSCDeGraphicUtils.Line(C, Pa[1], Pa[0]);
                end;
            end;
          end;

          with DragPoint do
            if (PointType = scptPointControl) and
              (PartIndex = I) and (SubIndex > 2) then
            begin
              P1 := TSCDePoint(L[I]);

              Pd[0] := P1.Point;

              Pd[1] := Pd[0];
              if P1.Control_1 <> nil then
              begin
                Pd[1].x := Pd[1].x + P1.Control_1.x;
                Pd[1].y := Pd[1].y + P1.Control_1.y;
              end;

              Pd[2] := Pd[0];
              if P1.Control_2 <> nil then
              begin
                Pd[2].x := Pd[2].x + P1.Control_2.x;
                Pd[2].y := Pd[2].y + P1.Control_2.y;
              end;

              for J := 0 to 2 do
              begin
                Pa[J].x := TSCDeUtils.Round(X + Zoom*Pd[J].x);
                Pa[J].y := TSCDeUtils.Round(Y + Zoom*Pd[J].y);
              end;

              TSCDeGraphicUtils.Line(C, Pa[1], Pa[0]);
              TSCDeGraphicUtils.Line(C, Pa[2], Pa[0]);
            end;

          if (Data.SelectionType = scdsSelected) and (L.Count > 1) and
            ((I < L.Count-1) or not GetIsClosed or (L.Count < 3)) then
          begin
            P1 := TSCDePoint(L[I]);

            P2 := nil;
            if I < L.Count-1 then
              P2 := TSCDePoint(L[I+1])
            else if AIsClosed and (L.Count > 2) then
              P2 := TSCDePoint(L[0]);

            if I > 0 then
              P3 := TSCDePoint(L[I-1])
            else begin
              P3 := nil;
              if AIsClosed and (L.Count > 2) then
                P3 := TSCDePoint(L[L.Count-2])
              else if L.Count = 2 then
                P3 := TSCDePoint(L[L.Count-1]);
            end;

            if (P3 = P2) and (L.Count = 2) then
            begin
              if I = 0 then
                P3 := nil
              else
                P2 := nil;
            end;

            if IsSelected(P1) then
            begin
              Psd[0] := P1.Point;

              with DragPoint do
                if (PointType = scptPoint) and (I = PartIndex) and
                  ((Offset.x <> 0) or (Offset.y <> 0)) then
                begin
                  Psd[0].x := Psd[0].x + Offset.x;
                  Psd[0].y := Psd[0].y + Offset.y;
                end;

              Psd[1] := Psd[0];
              Psd[2] := Psd[0];

              if (P1.Control_1 <> nil) and (GetIsClosed or (P1 <> L[0])) then
              begin
                Psd[1] := P1.Control_1.Point;

                Psd[1].x := Psd[1].x + Psd[0].x;
                Psd[1].y := Psd[1].y + Psd[0].y;
              end;

              if (P1.Control_2 <> nil) and (GetIsClosed or (P1 <> L[L.Count-1])) then
              begin
                Psd[2] := P1.Control_2.Point;

                Psd[2].x := Psd[2].x + Psd[0].x;
                Psd[2].y := Psd[2].y + Psd[0].y;
              end;

              if P2 = nil then
              begin
                Psd[3] := Psd[0];
                Psd[4] := Psd[0];
              end else
              begin
                Psd[3] := P2.Point;
                Psd[4] := Psd[3];

                if P2.Control_1 <> nil then
                begin
                  Psd[4] := P2.Control_1.Point;

                  Psd[4].x := Psd[4].x + Psd[3].x;
                  Psd[4].y := Psd[4].y + Psd[3].y;
                end;  
              end;

              if P3 = nil then
              begin
                Psd[5] := Psd[0];
                Psd[6] := Psd[0];
              end else
              begin
                Psd[5] := P3.Point;
                Psd[6] := Psd[5];

                if P3.Control_2 <> nil then
                begin
                  Psd[6] := P3.Control_2.Point;

                  Psd[6].x := Psd[6].x + Psd[5].x;
                  Psd[6].y := Psd[6].y + Psd[5].y;
                end;  
              end;

              for J := Low(Psd) to High(Psd) do
              begin
                Psa[J].x := TSCDeUtils.Round(X + Zoom*Psd[J].x);
                Psa[J].y := TSCDeUtils.Round(Y + Zoom*Psd[J].y);
              end;

              if (P1.Control_1 <> nil) and (GetIsClosed or (P1 <> L[0])) then
                TSCDeGraphicUtils.Line(C, Psa[1], Psa[0]);

              if (P1.Control_2 <> nil) and (GetIsClosed or (P1 <> L[L.Count-1])) then
                TSCDeGraphicUtils.Line(C, Psa[2], Psa[0]);

              if (P2 <> nil) and (P2.Control_1 <> nil) then
                TSCDeGraphicUtils.Line(C, Psa[4], Psa[3]);

              if (P3 <> nil) and (P3.Control_2 <> nil) then
                TSCDeGraphicUtils.Line(C, Psa[6], Psa[5]);

              if (DragPoint.PartIndex = -1) and ((InCl <> clNone) or
                (OutCl <> clNone)) and (Data.PointSize > 1) then
              begin
                with C do
                begin
                  Pen.Color := Cl;
                  Pen.Width := 1;
                  Pen.Mode  := pmCopy;
                  Pen.Style := psSolid;

                  Brush.Color := Cl;
                  Brush.Style := bsSolid;
                end;

                if P1.Control_1 <> nil then
                begin
                  PR := Rect(Psa[1].x, Psa[1].y, Psa[1].x + Data.PointSize,
                    Psa[1].y + Data.PointSize);
                  OffsetRect(PR, OX, OY);

                  C.Ellipse(PR.Left, PR.Top, PR.Right, PR.Bottom);
                end;

                if P1.Control_2 <> nil then
                begin
                  PR := Rect(Psa[2].x, Psa[2].y, Psa[2].x + Data.PointSize,
                    Psa[2].y + Data.PointSize);
                  OffsetRect(PR, OX, OY);

                  C.Ellipse(PR.Left, PR.Top, PR.Right, PR.Bottom);
                end;

                if (P2 <> nil) and (P2.Control_1 <> nil) then
                begin
                  PR := Rect(Psa[4].x, Psa[4].y, Psa[4].x + Data.PointSize,
                    Psa[4].y + Data.PointSize);
                  OffsetRect(PR, OX, OY);

                  C.Ellipse(PR.Left, PR.Top, PR.Right, PR.Bottom);
                end;

                if (P3 <> nil) and (P3.Control_1 <> nil) then
                begin
                  PR := Rect(Psa[6].x, Psa[6].y, Psa[6].x + Data.PointSize,
                    Psa[6].y + Data.PointSize);
                  OffsetRect(PR, OX, OY);

                  C.Ellipse(PR.Left, PR.Top, PR.Right, PR.Bottom);
                end;

                with C do
                begin
                  Brush.Style := bsClear;

                  Pen.Color := LineCl;
                  Pen.Mode  := Data.LineMode;
                  Pen.Style := Data.LineStyle;
                  Pen.Width := 1;
                end;
              end;
            end;
          end;
        end;
      end;

      if (DragPoint.PartIndex = -1) and ((InCl <> clNone) or
        (OutCl <> clNone)) and (Data.PointSize > 1) then
      begin
        for I := 0 to PointCount-1 do
        begin
          P := Points[I];

          AX := TSCDeUtils.Round(X + Zoom*P.x);
          AY := TSCDeUtils.Round(Y + Zoom*P.y);

          PR := Rect(AX, AY, AX + Data.PointSize, AY + Data.PointSize);
          OffsetRect(PR, OX, OY);

          if not IsSelected(P) then
            DrawPoint(C, PR, InCl, OutCl)
          else if OutCl <> clNone then
            DrawPoint(C, PR, OutCl, OutCl)
          else
            DrawPoint(C, PR, InCl, InCl);
        end;
      end;
    finally
      L.Free;
    end;
  end;
end;

function TSCDeCustomPolyBezier.GetBounds: TDoubleRect;
begin
  Result := FBounds;
end;

function TSCDeCustomPolyBezier.InRect(const R: TDoubleRect): Boolean;
var
  L: TList;
  I, J: Integer;
  P: PDoublePoint;
  Bh: PBezierHolder;
  P1, P2: TDoublePoint;
begin
  Result := False;

  if not TSCDeUtils.IsRectEmpty(R) and (FBeziers <> nil) and
    (FBeziers.Count > 0) then
  begin
    for I := 0 to FBeziers.Count-1 do
    begin
      Bh := FBeziers[I];
      L := Bh^.List;

      if L <> nil then
        for J := 0 to L.Count-1 do
        begin
          P := L[J];

          P1.x := P^.x;
          P1.y := P^.y;

          Result := TSCDeUtils.PtInRect(R, P1);

          if Result then Exit;

          if J < L.Count-1 then
          begin
            P := L[J + 1];

            P2.x := P^.x;
            P2.y := P^.y;

            Result := TSCDeUtils.DoLineIntersectRect(P1, P2, R);
            if Result then Exit;
          end;
        end;
    end;
  end;
end;

function TSCDeCustomPolyBezier.ApproxInsert(P: TDoublePoint; Fuzz: Double): TSCDePoint;
var
  L: TList;
  I, J: Integer;
  R: TDoubleRect;
  Pp: PDoublePoint;
  Bh: PBezierHolder;
  P1, P2: TDoublePoint;
begin
  Result := nil;
  if Fuzz < 0.0 then Fuzz := 0.0;

  if PointCount = 1 then
  begin
    P1 := Points[0].Point;

    if Fuzz = 0 then
    begin
      if (P1.x = P.x) and (P1.y = P.y) then
      begin
        Result := TSCDePoint.Create(nil);

        Result.x := P.x;
        Result.y := P.y;

        Self.Insert(1, Result);
      end;
    end else
    begin
      R.Left := P1.x - (Fuzz / 2);
      R.Top := P1.y - (Fuzz / 2);

      R.Right := R.Left + Fuzz;
      R.Bottom := R.Top + Fuzz;

      if TSCDeUtils.PtInRect(R, P) then
      begin
        Result := TSCDePoint.Create(nil);

        Result.x := P.x;
        Result.y := P.y;

        Self.Insert(1, Result);
      end;
    end;
  end else
  if (FBeziers <> nil) and (FBeziers.Count > 0) then
  begin
    for I := 0 to FBeziers.Count-1 do
    begin
      Bh := FBeziers[I];
      L := Bh^.List;

      if (L <> nil) and (L.Count > 0) then
      begin
        Pp := L[0];

        P1.x := Pp^.x;
        P1.y := Pp^.y;

        for J := 1 to L.Count-1 do
        begin
          Pp := L[J];

          P2.x := Pp^.x;
          P2.y := Pp^.y;

          if (P1.x = P2.x) and (P1.y = P2.y) then
            Continue;

          if TSCDeUtils.NearLine(P, P1, P2, Fuzz) then
          begin
            Result := TSCDePoint.Create(nil);

            Result.x := P.x;
            Result.y := P.y;

            Self.Insert(I + 1, Result);

            Exit;
          end;

          P1 := P2;
        end;
      end;
    end;
  end;
end;

procedure TSCDeCustomPolyBezier.Paint(C: TCanvas; X, Y: Integer; Zoom: Double);
var
  L: TList;
  P: TPoint;
  I, J: Integer;
  Pt: TDoublePoint;
  P1, P2: TSCDePoint;
  Ps: array[0..2] of TPoint;
  Pd: array[0..2] of TDoublePoint;
begin
  if (PointCount > 0) and (Self.HasPen or (GetIsClosed and Self.HasBrush)) then
  begin
    Pen.AssignTo(C.Pen);
    if Pen.Color = clNone then C.Pen.Style := psClear;

    C.Brush.Style := bsClear;

    if Self.HasBrush then
    begin
      Brush.AssignTo(C.Brush);
      if Brush.Color = clNone then C.Brush.Style := bsClear;
    end;

    L := TList.Create;
    try
      P1 := Points[0];

      L.Add(P1);

      for I := 1 to PointCount-1 do
      begin
        P2 := Points[I];

        if (P1.x <> P2.x) or (P1.y <> P2.y) then
          L.Add(P2);

        P1 := P2;
      end;

      if GetIsClosed and (L.Count > 1) then
        L.Add(L[0]);

      if L.Count = 1 then
      begin
        Pt := Points[0].Point;

        P.x := TSCDeUtils.Round(X + Zoom*Pt.x);
        P.y := TSCDeUtils.Round(Y + Zoom*Pt.y);

        C.MoveTo(P.x, P.y);

        Inc(P.x);
        C.LineTo(P.x, P.y);
      end else
      begin
        if Self.HasBrush then Windows.BeginPath(C.Handle);
        try
          P1 := TSCDePoint(L[0]);

          P.x := TSCDeUtils.Round(X + Zoom*P1.x);
          P.y := TSCDeUtils.Round(Y + Zoom*P1.y);

          C.MoveTo(P.x, P.y);

          for I := 0 to L.Count-2 do
          begin
            P1 := TSCDePoint(L[I]);
            P2 := TSCDePoint(L[I + 1]);

            P.x := TSCDeUtils.Round(Zoom * P1.x);
            P.y := TSCDeUtils.Round(Zoom * P1.y);

            Pd[0] := P1.Point;
            if P1.Control_2 <> nil then
            begin
              Pd[0].x := Pd[0].x + P1.Control_2.x;
              Pd[0].y := Pd[0].y + P1.Control_2.y;
            end;

            Pd[1] := P2.Point;
            if P2.Control_1 <> nil then
            begin
              Pd[1].x := Pd[1].x + P2.Control_1.x;
              Pd[1].y := Pd[1].y + P2.Control_1.y;
            end;

            Pd[2] := P2.Point;
            for J := 0 to 2 do
            begin
              Ps[J].x := TSCDeUtils.Round(X + Zoom*Pd[J].x);
              Ps[J].y := TSCDeUtils.Round(Y + Zoom*Pd[J].y);
            end;

            C.PolyBezierTo(Ps);
          end;
        finally
          if Self.HasBrush then
          begin
            EndPath(C.Handle);
            StrokeAndFillPath(C.Handle);
          end;
        end;
      end;
    finally
      L.Free;
    end;
  end;
end;

function TSCDeCustomPolyBezier.PointControlAt(P: TDoublePoint;
  Fuzz: Double; IsSelected: Boolean; var PointIndex: Integer): Integer;
var
  R: TDoubleRect;
  OX, OY: Double;
  I, Cnt: Integer;
  Ps1, Ps2: TDoublePoint;
  P1, P2, P3: TSCDePoint;
begin
  if Fuzz < 0.0 then Fuzz := 0.0;

  Result := -1;
  PointIndex := -1;

  OX := Fuzz / 2;
  OY := OX;

  Cnt := PointCount;
  for I := 0 to Cnt-1 do
  begin
    P1 := Points[I];

    if Self.IsSelected(P1) then
    begin
      P2 := nil;
      if I < Cnt-1 then
        P2 := Points[I+1]
      else if GetIsClosed and (Cnt > 1) then
        P2 := Points[0];

      if I > 0 then
        P3 := Points[I-1]
      else begin
        P3 := nil;
        if GetIsClosed and (Cnt > 1) then
          P3 := Points[Cnt-1];
      end;

      if (P3 = P2) and (Cnt = 2) then
      begin
        if I = 0 then
          P3 := nil
        else
          P2 := nil;
      end;

      Ps1 := P1.Point;

      if (P1.Control_1 <> nil) and (GetIsClosed or (I <> 0)) then
      begin
        Ps2 := P1.Control_1.Point;

        Ps2.x := Ps2.x + Ps1.x;
        Ps2.y := Ps2.y + Ps1.y;

        R := TSCDeUtils.Rect(Ps2.x, Ps2.y, Ps2.x + Fuzz, Ps2.y + Fuzz);
        TSCDeUtils.OffsetRect(R, -OX, -OY);

        if TSCDeUtils.PtInRect(R, P) then
        begin
          Result := I;
          PointIndex := 0;

          Exit;
        end;
      end;

      if (P1.Control_2 <> nil) and (GetIsClosed or (I <> Cnt-1)) then
      begin
        Ps2 := P1.Control_2.Point;

        Ps2.x := Ps2.x + Ps1.x;
        Ps2.y := Ps2.y + Ps1.y;

        R := TSCDeUtils.Rect(Ps2.x, Ps2.y, Ps2.x + Fuzz, Ps2.y + Fuzz);
        TSCDeUtils.OffsetRect(R, -OX, -OY);

        if TSCDeUtils.PtInRect(R, P) then
        begin
          Result := I;
          PointIndex := 1;

          Exit;
        end;
      end;

      if P2 <> nil then
      begin
        Ps1 := P2.Point;

        if P2.Control_1 <> nil then
        begin
          Ps2 := P2.Control_1.Point;

          Ps2.x := Ps2.x + Ps1.x;
          Ps2.y := Ps2.y + Ps1.y;

          R := TSCDeUtils.Rect(Ps2.x, Ps2.y, Ps2.x + Fuzz, Ps2.y + Fuzz);
          TSCDeUtils.OffsetRect(R, -OX, -OY);

          if TSCDeUtils.PtInRect(R, P) then
          begin
            Result := P2.Index;
            PointIndex := 0;

            Exit;
          end;
        end;
      end;

      if P3 <> nil then
      begin
        Ps1 := P3.Point;

        if P3.Control_2 <> nil then
        begin
          Ps2 := P3.Control_2.Point;

          Ps2.x := Ps2.x + Ps1.x;
          Ps2.y := Ps2.y + Ps1.y;

          R := TSCDeUtils.Rect(Ps2.x, Ps2.y, Ps2.x + Fuzz, Ps2.y + Fuzz);
          TSCDeUtils.OffsetRect(R, -OX, -OY);

          if TSCDeUtils.PtInRect(R, P) then
          begin
            Result := P3.Index;
            PointIndex := 1;

            Exit;
          end;
        end;
      end;
    end;
  end;
end;

function TSCDeCustomPolyBezier.PointOnShape(P: TDoublePoint;
  Fuzz: Double; Editing: Boolean): Boolean;
var
  L: TList;
  R: TDoubleRect;
  Pp: PDoublePoint;
  Bh: PBezierHolder;
  Pts: TDPointArray;
  P1, P2: TDoublePoint;
  I, J, Indicator: Integer;
begin
  if Fuzz < 0.0 then Fuzz := 0.0;

  Result := False;

  if PointCount = 1 then
  begin
    P1 := Points[0].Point;

    if Fuzz = 0 then
      Result := (P1.x = P.x) and (P1.y = P.y)
    else begin
      R.Left := P1.x - (Fuzz / 2);
      R.Top := P1.y - (Fuzz / 2);

      R.Right := R.Left + Fuzz;
      R.Bottom := R.Top + Fuzz;

      Result := TSCDeUtils.PtInRect(R, P);
    end;
  end else
  if (FBeziers <> nil) and (FBeziers.Count > 0) then
  begin
    SetLength(Pts, 0);

    Indicator := 0;
    
    for I := 0 to FBeziers.Count-1 do
    begin
      Bh := FBeziers[I];
      L := Bh^.List;

      if (L <> nil) and (L.Count > 0) then
      begin
        SetLength(Pts, Length(Pts) + L.Count);

        Pp := L[0];

        P1.x := Pp^.x;
        P1.y := Pp^.y;

        Pts[Indicator].x := P1.x;
        Pts[Indicator].y := P1.y;

        Inc(Indicator);

        for J := 1 to L.Count-1 do
        begin
          Pp := L[J];

          P2.x := Pp^.x;
          P2.y := Pp^.y;

          Pts[Indicator].x := P2.x;
          Pts[Indicator].y := P2.y;

          Inc(Indicator);

          if (P1.x = P2.x) and (P1.y = P2.y) then
            Continue;

          Result := TSCDeUtils.NearLine(P, P1, P2, Fuzz);
          if Result then Exit;

          P1 := P2;
        end;
      end;
    end;

    if GetIsClosed and (Brush.Style <> bsClear) and (Brush.Color <> clNone) then
      Result := TSCDeUtils.PtInPolygon(Pts, P);
  end;
end;

procedure TSCDeCustomPolyBezier.RecalculateBounds(Force: Boolean);
var
  L: TList;
  I, J: Integer;
  Pp: PDoublePoint;
  Bh: PBezierHolder;
begin
  if Force or not (InUpdate or InPointUpdate) then
  begin
    FBounds := TSCDeUtils.EmptyRect;

    if (FBeziers <> nil) and (FBeziers.Count > 0) then
    begin
      Bh := FBeziers[0];
      L := Bh^.List;

      Pp := L[0];

      FBounds := TSCDeUtils.Rect(Pp^.x, Pp^.y, Pp^.x, Pp^.y);;

      for I := 0 to FBeziers.Count-1 do
      begin
        Bh := FBeziers[I];
        L := Bh^.List;

        for J := 0 to L.Count-1 do
        begin
          Pp := L[J];

          if Pp^.x < FBounds.Left then FBounds.Left := Pp^.x;
          if Pp^.y < FBounds.Top then FBounds.Top := Pp^.y;
          if Pp^.x > FBounds.Right then FBounds.Right := Pp^.x;
          if Pp^.y > FBounds.Bottom then FBounds.Bottom := Pp^.y;
        end;
      end;
    end;
  end;
end;

function TSCDeCustomPolyBezier.RecreateBeziers: TList;
var
  I: Integer;
  L: TList;
  SqLimit: Double;
  Pp: PDoublePoint;
  P, P2: TSCDePoint;
  Bh: PBezierHolder;
  Pa: array[0..3] of TDoublePoint;
begin
  Result := nil;

  if not InDestroy then
  begin
    Result := TList.Create;

    if PointCount = 1 then
    begin
      New(Bh);
      Result.Add(Bh);

      Bh^.List := TList.Create;
      Bh^.Point := Points[0];

      New(Pp);

      Pp^.x := Points[0].x;
      Pp^.y := Points[0].y;

      Bh^.List.Add(Pp);
    end else
    if PointCount > 1 then
    begin
      L := TList.Create;
      try
        P := Points[0];

        L.Add(P);

        for I := 1 to PointCount-1 do
        begin
          P2 := Points[I];

          if (P.x <> P2.x) or (P.y <> P2.y) then
            L.Add(P2);

          P := P2;
        end;

        if GetIsClosed and (L.Count > 1) then
          L.Add(Points[0]);

        if L.Count > 0 then
        begin
          SqLimit := Sqr(FSegmentLimit);

          for I := 0 to L.Count-2 do
          begin
            P := TSCDePoint(L[I]);
            P2 := TSCDePoint(L[I + 1]);

            New(Bh);
            Result.Add(Bh);

            Bh^.List := TList.Create;
            Bh^.Point := P;

            // add first point
            New(Pp);

            Pp^.x := P.x;
            Pp^.y := P.y;

            Bh^.List.Add(Pp);

            Pa[0] := P.Point;
            Pa[3] := P2.Point;

            Pa[1] := Pa[0];
            Pa[2] := Pa[3];

            if (P.Control_2 <> nil) or (P2.Control_1 <> nil) then
            begin
              // first point control
              if P.Control_2 <> nil then
              begin
                Pa[1] := P.Control_2.Point;

                Pa[1].x := Pa[1].x + Pa[0].x;
                Pa[1].y := Pa[1].y + Pa[0].y;
              end;

              // second point control
              if P2.Control_1 <> nil then
              begin
                Pa[2] := P2.Control_1.Point;

                Pa[2].x := Pa[2].x + Pa[3].x;
                Pa[2].y := Pa[2].y + Pa[3].y;
              end;

              // Bezier factoring
              Pa[3].x := -1 * Pa[0].x +  3 * Pa[1].x + -3 * Pa[2].x + 1 * Pa[3].x;
              Pa[2].x :=  3 * Pa[0].x + -6 * Pa[1].x +  3 * Pa[2].x;
              Pa[1].x := -3 * Pa[0].x +  3 * Pa[1].x;
              Pa[0].x :=  1 * Pa[0].x;

              Pa[3].y := -1 * Pa[0].y +  3 * Pa[1].y + -3 * Pa[2].y + 1 * Pa[3].y;
              Pa[2].y :=  3 * Pa[0].y + -6 * Pa[1].y +  3 * Pa[2].y;
              Pa[1].y := -3 * Pa[0].y +  3 * Pa[1].y;
              Pa[0].y :=  1 * Pa[0].y;

              CalculateBeziers(Bh^.List, Pa[0], Pa[1], Pa[2], Pa[3], SqLimit, 0, 1);
            end;

            // add last point
            New(Pp);

            Pp^.x := P2.x;
            Pp^.y := P2.y;

            Bh^.List.Add(Pp);
          end;
        end;
      finally
        L.Free;
      end;
    end;
  end;
end;

procedure TSCDeCustomPolyBezier.Resize(NewWidth, NewHeight: Double);
var
  I: Integer;
  P: TSCDePoint;
  R: TDoubleRect;
  Pc: TSCDePointControl;
  W, H, X, Y, ZoomX, ZoomY: Double;
begin
  if NewWidth < 0.01 then NewWidth := 0.01;
  if NewHeight < 0.01 then NewHeight := 0.01;

  if not InPointUpdate and (PointCount > 1) then
  begin
    R := GetBounds;

    W := R.Right - R.Left;
    if W < 0.0 then W := 0.0;

    H := R.Bottom - R.Top;
    if H < 0.0 then H := 0.0;

    if (W <> NewWidth) or (H <> NewHeight) then
    begin
      ZoomX := 1.0;
      if W <> 0.0 then ZoomX := NewWidth / W;

      ZoomY := 1.0;
      if H <> 0.0 then ZoomY := NewHeight / H;

      BeginUpdate;
      try
        BeginPointUpdate;
        try
          BeginNotifyLock;
          try
            NotifyUndo(Self, scacSizing, '');

            for I := 0 to PointCount-1 do
            begin
              P := Points[I];

              X := R.Left + (ZoomX * (P.x - R.Left));
              Y := R.Top + (ZoomY * (P.y - R.Top));

              P.SetPosition(X, Y);

              if P.Control_1 <> nil then
              begin
                Pc := P.Control_1;

                X := ZoomX * Pc.x;
                Y := ZoomY * Pc.y;

                Pc.SetPosition(X, Y);
              end;

              if P.Control_2 <> nil then
              begin
                Pc := P.Control_2;

                X := ZoomX * Pc.x;
                Y := ZoomY * Pc.y;

                Pc.SetPosition(X, Y);
              end;
            end;
          finally
            EndNotifyLock;
          end;
        finally
          EndPointUpdate;
        end;
      finally
        EndUpdate;
      end;
    end;
  end;  
end;

procedure TSCDeCustomPolyBezier.SetSegmentLimit(Value: Byte);
begin
  if FSegmentLimit <> Value then
  begin
    FSegmentLimit := Value;
    if PointCount > 1 then
      Changed;
  end;
end;

procedure TSCDeCustomPolyBezier.UpdateRegion;
begin
  inherited UpdateRegion;

  ClearBeziers(FBeziers);
  FreeAndNil(FBeziers);

  FBeziers := RecreateBeziers;
  RecalculateBounds(True);
end;

{ TSCDeFreeLine }

constructor TSCDeFreeLine.Create(ASurface: TSCDeSurface;
  AOwner: TSCDeContainer);
begin
  inherited Create(ASurface, AOwner);
  FPrecision := 3.0;
  SetShapeStyle(Self.ShapeStyle + [scssUsesBrush, scssUsesGradient,
    scssUsesPicture]);

  with Self.Brush do
  begin
    Color := clNone;
    Style := bsClear;
  end;
end;

function TSCDeFreeLine.DefaultClosed: Boolean;
begin
  Result := True;
end;

function TSCDeFreeLine.GetIsClosed: Boolean;
begin
  Result := Self.HasBrush;
end;

function TSCDeFreeLine.GetShapeType: TSCShapeType;
begin
  Result := sctyFreeHand;
end;

function TSCDeFreeLine.IsClosedStored: Boolean;
begin
  Result := False;
end;

procedure TSCDeFreeLine.LoadFromXML(Node: TSCDomElement);
var
  Prop: TSCDomElement;
begin
  Changing;
  BeginUpdate;
  try
    BeginPointUpdate;
    try
      inherited LoadFromXML(Node);

      Prop := Node.ElementByName('brush');
      if Prop <> nil then Brush.LoadFromXML(Prop);

      Prop := Node.ElementByName('pen');
      if Prop <> nil then Pen.LoadFromXML(Prop);

      LoadBoundsFromXML(Node);
    finally
      EndPointUpdate;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TSCDeFreeLine.Refine(Zoom: Double);
var
  L: TList;
  I, Len: Integer;
  P1, P2: TSCDePoint;
  Pd: array of TDoublePoint;
  SimpleList: array of TDoublePoint;
begin
  if PointCount > 0 then
  begin
    BeginUpdate;
    try
      BeginPointUpdate;
      try
        SetLength(Pd, 0);

        L := TList.Create;
        try
          P1 := Points[0];

          L.Add(P1);

          for I := 1 to PointCount-1 do
          begin
            P2 := Points[I];

            if (P1.x <> P2.x) or (P1.y <> P2.y) then
              L.Add(P2);

            P1 := P2;  
          end;

          if (L.Count > 2) and GetIsClosed then
            L.Add(Points[0]);

          SetLength(Pd, L.Count);

          for I := 0 to L.Count - 1 do
          begin
            P1 := TSCDePoint(L[I]);

            Pd[I].x := Zoom*P1.x;
            Pd[I].y := Zoom*P1.y;
          end;
        finally
          L.Free;
        end;

        SetLength(SimpleList, Length(Pd));

        if Length(Pd) = 1 then
          SimpleList[0] := Pd[0]
        else if Length(Pd) > 2 then
        begin
          Len := TSCDeUtils.PolySimplifyInt2D(FPrecision, Pd, SimpleList);
          SetLength(SimpleList, Len);
        end;

        ClearPoints;

        for I := 0 to Length(SimpleList)-1 do
          Add(SimpleList[I].x/Zoom, SimpleList[I].y/Zoom);
      finally
        EndPointUpdate;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSCDeFreeLine.SaveToXML(Node: TSCDomElement);
var
  Prop: TSCDomElement;
begin
  inherited SaveToXML(Node);

  Prop := TSCDomElement.Create(Node);
  Prop.Name := 'brush';
  Node.AddElement(Prop);

  Brush.SaveToXML(Prop);

  Prop := TSCDomElement.Create(Node);
  Prop.Name := 'pen';
  Node.AddElement(Prop);

  Pen.SaveToXML(Prop);

  SaveBoundsToXML(Node);
end;

{ TSCDeRectangle }

constructor TSCDeRectangle.Create(ASurface: TSCDeSurface;
  AOwner: TSCDeContainer);
begin
  inherited Create(ASurface, AOwner);
  SetShapeStyle(ShapeStyle + [scssUsesPicture, scssUsesBrush,
    scssUsesPen, scssUsesGradient]);
end;

procedure TSCDeRectangle.LoadFromXML(Node: TSCDomElement);
var
  Prop: TSCDomElement;
begin
  Changing;
  BeginUpdate;
  try
    inherited LoadFromXML(Node);

    Prop := Node.ElementByName('brush');
    if Prop <> nil then Brush.LoadFromXML(Prop);

    Prop := Node.ElementByName('pen');
    if Prop <> nil then Pen.LoadFromXML(Prop);

    LoadBoundsFromXML(Node);
  finally
    EndUpdate;
  end;
end;

procedure TSCDeRectangle.SaveToXML(Node: TSCDomElement);
var
  Prop: TSCDomElement;
begin
  inherited SaveToXML(Node);

  SaveBoundsToXML(Node);

  Prop := TSCDomElement.Create(Node);
  Prop.Name := 'brush';
  Node.AddElement(Prop);

  Brush.SaveToXML(Prop);

  Prop := TSCDomElement.Create(Node);
  Prop.Name := 'pen';
  Node.AddElement(Prop);

  Pen.SaveToXML(Prop);
end;

{ TSCDeEllipse }

constructor TSCDeEllipse.Create(ASurface: TSCDeSurface;
  AOwner: TSCDeContainer);
begin
  inherited Create(ASurface, AOwner);
  SetShapeStyle(ShapeStyle + [scssUsesPicture, scssUsesBrush, 
    scssUsesPen]);
end;

procedure TSCDeEllipse.LoadFromXML(Node: TSCDomElement);
var
  Prop: TSCDomElement;
begin
  Changing;
  BeginUpdate;
  try
    inherited LoadFromXML(Node);

    Prop := Node.ElementByName('brush');
    if Prop <> nil then Brush.LoadFromXML(Prop);

    Prop := Node.ElementByName('pen');
    if Prop <> nil then Pen.LoadFromXML(Prop);

    LoadBoundsFromXML(Node);
  finally
    EndUpdate;
  end;
end;

procedure TSCDeEllipse.SaveToXML(Node: TSCDomElement);
var
  Prop: TSCDomElement;
begin
  inherited SaveToXML(Node);

  SaveBoundsToXML(Node);

  Prop := TSCDomElement.Create(Node);
  Prop.Name := 'brush';
  Node.AddElement(Prop);

  Brush.SaveToXML(Prop);

  Prop := TSCDomElement.Create(Node);
  Prop.Name := 'pen';
  Node.AddElement(Prop);

  Pen.SaveToXML(Prop);
end;

{ TSCDePolyLine }

procedure TSCDePolyLine.LoadFromXML(Node: TSCDomElement);
var
  S: String;
  Prop: TSCDomElement;
begin
  Changing;
  BeginUpdate;
  try
    BeginPointUpdate;
    try
      inherited LoadFromXML(Node);

      ClearPoints;

      S := GetProperty(Node, 'isclosed');
      SetIsClosed(Boolean(StrToIntDef(S, Integer(False))));

      Prop := Node.ElementByName('pen');
      if Prop <> nil then Pen.LoadFromXML(Prop);

      LoadBoundsFromXML(Node);
    finally
      EndPointUpdate;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TSCDePolyLine.SaveToXML(Node: TSCDomElement);
var
  Prop: TSCDomElement;
begin
  inherited SaveToXML(Node);

  AddProperty(Node, 'isclosed', IntToStr(Integer(GetIsClosed)));

  Prop := TSCDomElement.Create(Node);
  Prop.Name := 'pen';
  Node.AddElement(Prop);

  Pen.SaveToXML(Prop);

  SaveBoundsToXML(Node);
end;

{ TSCDePolyBezier }

procedure TSCDePolyBezier.LoadFromXML(Node: TSCDomElement);
var
  S: String;
  Prop: TSCDomElement;
begin
  Changing;
  BeginUpdate;
  try
    BeginPointUpdate;
    try
      inherited LoadFromXML(Node);

      ClearPoints;

      S := GetProperty(Node, 'isclosed');
      SetIsClosed(Boolean(StrToIntDef(S, Integer(DefaultClosed))));

      Prop := Node.ElementByName('pen');
      if Prop <> nil then Pen.LoadFromXML(Prop);

      Prop := Node.ElementByName('brush');
      if Prop <> nil then Brush.LoadFromXML(Prop);

      LoadBoundsFromXML(Node);
    finally
      EndPointUpdate;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TSCDePolyBezier.SaveToXML(Node: TSCDomElement);
var
  Prop: TSCDomElement;
begin
  inherited SaveToXML(Node);

  if IsClosedStored and (GetIsClosed <> DefaultClosed) then
  begin
    AddProperty(Node, 'isclosed', IntToStr(Integer(GetIsClosed)));
  end;

  Prop := TSCDomElement.Create(Node);
  Prop.Name := 'brush';
  Node.AddElement(Prop);

  Brush.SaveToXML(Prop);

  Prop := TSCDomElement.Create(Node);
  Prop.Name := 'pen';
  Node.AddElement(Prop);

  Pen.SaveToXML(Prop);

  SaveBoundsToXML(Node);
end;

{ TSCDePolyPolygon }

procedure TSCDePolyPolygon.LoadFromXML(Node: TSCDomElement);
var
  S: String;
  Prop: TSCDomElement;
begin
  Changing;
  BeginUpdate;
  try
    inherited LoadFromXML(Node);

    Prop := Node.ElementByName('brush');
    if Prop <> nil then Brush.LoadFromXML(Prop);

    S := GetProperty(Node, 'combined');
    FCombined := Boolean(StrToIntDef(S, Integer(True)));
  finally
    EndUpdate;
  end;
end;

procedure TSCDePolyPolygon.SaveToXML(Node: TSCDomElement);
begin
  inherited SaveToXML(Node);
  AddProperty(Node, 'combined', IntToStr(Integer(Combined)));
end;

{ TSCDeChar }

procedure TSCDeChar.LoadFromXML(Node: TSCDomElement);
begin
  inherited LoadFromXML(Node);
end;

procedure TSCDeChar.SaveToXML(Node: TSCDomElement);
begin
  inherited SaveToXML(Node);
end;

{ TSCDeLabel }

constructor TSCDeLabel.Create(ASurface: TSCDeSurface;
  AOwner: TSCDeContainer);
begin
  inherited Create(ASurface, AOwner);
  SetShapeStyle(ShapeStyle + [scssUsesCaption, scssUsesBrush, scssUsesPen,
    scssUsesFont, scssUsesGradient, scssCanRotate]);
  FAlignment := taCenter;
  FLayout := sctlCenter;
  FWordWrap := False;
end;

procedure TSCDeLabel.DoDrawText(C: TCanvas; var R: TRect;
  Flags: Integer; Zoom: Double);
var
  Text: string;
begin
  Text := Caption;
  Flags := Flags or DT_NOPREFIX;

  Self.Font.AssignTo(C.Font);
  C.Font.Size := TSCDeUtils.Round(Zoom*C.Font.Size);
  
  DrawText(C.Handle, PChar(Text), Length(Text), R, Flags);
end;

procedure TSCDeLabel.LoadFromXML(Node: TSCDomElement);
var
  S: String;
  Prop: TSCDomElement;
begin
  Changing;
  BeginUpdate;
  try
    inherited LoadFromXML(Node);

    S := GetProperty(Node, 'caption');
    SetCaption(S);

    Prop := Node.ElementByName('brush');
    if Prop <> nil then Brush.LoadFromXML(Prop);

    Prop := Node.ElementByName('pen');
    if Prop <> nil then Pen.LoadFromXML(Prop);

    Prop := Node.ElementByName('font');
    if Prop <> nil then Font.LoadFromXML(Prop);

    LoadBoundsFromXML(Node);
  finally
    EndUpdate;
  end;
end;

procedure TSCDeLabel.Paint(C: TCanvas; X, Y: Integer; Zoom: Double);
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  DR: TDoubleRect;
  DrawStyle: Longint;
  SR, R, CalcRect: TRect;
begin
  DR := GetBounds;
  TSCDeUtils.ZoomRect(DR, Zoom);

  R := TSCDeUtils.Rect(DR);

  if (Self.HasPen or Self.HasBrush) and
    ((R.Left <> R.Right) or (R.Top <> R.Bottom)) then
  begin
    OffsetRect(R, X, Y);

    Brush.AssignTo(C.Brush);
    Pen.AssignTo(C.Pen);
    Font.AssignTo(C.Font);

    C.Font.Size := TSCDeUtils.Round(Zoom*C.Font.Size);

    if Brush.Color = clNone then Brush.Style := bsClear;
    if Pen.Color = clNone then Pen.Style := psClear;

    C.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
  end;

  if not IsRectEmpty(R) and Self.HasGradient then
  begin
    Self.Gradient.Paint(C, R);

    if Self.HasPen then
    begin
      Pen.AssignTo(C.Pen);
      if Pen.Color = clNone then Pen.Style := psClear;

      C.Brush.Style := bsClear;

      C.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    end;
  end;

  if not IsRectEmpty(R) and (Caption <> '') then
  begin
    DrawStyle := DT_EXPANDTABS or WordWraps[FWordWrap] or Alignments[FAlignment];

    { Calculate vertical layout }
    if FLayout <> sctlTop then
    begin
      SR := R;
      CalcRect := R;
      OffsetRect(CalcRect, -CalcRect.Left, -CalcRect.Top);

      DoDrawText(C, CalcRect, DrawStyle or DT_CALCRECT, Zoom);
      if FLayout = sctlBottom then
        OffsetRect(R, 0, (R.Bottom - R.Top) - CalcRect.Bottom)
      else
        OffsetRect(R, 0, ((R.Bottom - R.Top) - CalcRect.Bottom) div 2);
    end;

    if IntersectClipRect(C.Handle, SR.Left,
      SR.Top, SR.Right, SR.Bottom) <> NULLREGION then
    begin
      try
        C.Brush.Style := bsClear;
        DoDrawText(C, R, DrawStyle, Zoom);
      finally
        SelectClipRgn(C.Handle, 0);
      end;
    end;
  end;
end;

function TSCDeLabel.PointOnShape(P: TDoublePoint; Fuzz: Double;
  Editing: Boolean): Boolean;
var
  R: TDoubleRect;
begin
  if Fuzz < 0.0 then Fuzz := 0.0;

  R := GetBounds;
  TSCDeUtils.InflateRect(R, Fuzz, Fuzz);

  Result := not TSCDeUtils.IsRectEmpty(R) and TSCDeUtils.PtInRect(R, P);
end;

procedure TSCDeLabel.SaveToXML(Node: TSCDomElement);
var
  Prop: TSCDomElement;
begin
  inherited SaveToXML(Node);

  SaveBoundsToXML(Node);

  Prop := TSCDomElement.Create(Node);
  Prop.Name := 'brush';
  Node.AddElement(Prop);

  Brush.SaveToXML(Prop);

  Prop := TSCDomElement.Create(Node);
  Prop.Name := 'pen';
  Node.AddElement(Prop);

  Pen.SaveToXML(Prop);

  Prop := TSCDomElement.Create(Node);
  Prop.Name := 'font';
  Node.AddElement(Prop);

  Font.SaveToXML(Prop);

  AddProperty(Node, 'caption', Caption);
end;

procedure TSCDeLabel.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    if Caption <> '' then Changed;
  end;
end;

procedure TSCDeLabel.SetLayout(Value: TSCDeTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    if Caption <> '' then Changed;
  end;
end;

procedure TSCDeLabel.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    if Caption <> '' then Changed;
  end;
end;

initialization
  TSCDeRegister.RegisterShape(TSCDeRectangle);
  TSCDeRegister.RegisterShape(TSCDeLabel);
  TSCDeRegister.RegisterShape(TSCDePicture);
  TSCDeRegister.RegisterShape(TSCDeEllipse);
  TSCDeRegister.RegisterShape(TSCDeCircle);
  TSCDeRegister.RegisterShape(TSCDeArc);
  TSCDeRegister.RegisterShape(TSCDeEllipse);
  TSCDeRegister.RegisterShape(TSCDePolyLine);
  TSCDeRegister.RegisterShape(TSCDePolyBezier);
  TSCDeRegister.RegisterShape(TSCDeFreeLine);
  TSCDeRegister.RegisterShape(TSCDePolygon);
  TSCDeRegister.RegisterShape(TSCDePolyPolygon);
  TSCDeRegister.RegisterShape(TSCDeChar);
  TSCDeRegister.RegisterShape(TSCDeText);

end.
