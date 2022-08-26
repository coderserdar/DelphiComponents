unit hhAvSlip;

{ Represent a mechanical Slip Indicator for cockpit instrumentation }
{ Author: Howard Harvey
  Dated:  01/DEC/2000 }

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, 
     Math, StdCtrls;

type
  ThhAvSlip = class(TGraphicControl)
  private
    fSlip         : Real;
    fBallDiam     : Integer;		
    fTubeBend     : Integer;
    fBorderStyle  : TBorderStyle;
    fScaleColor   : TColor;
    fBackColor    : TColor;
    fBallColor    : TColor;
    fTubeColor    : TColor;
    procedure PaintBackground(AnImage: TBitmap);
    procedure PaintSlipImage(AnImage: TBitmap; PaintRect: TRect);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetScaleColor(Colour : TColor);
    procedure SetBackColor(Colour : TColor);
    procedure SetBallColor(Colour : TColor);
    procedure SetTubeColor(Colour : TColor);
    procedure SetSlip(Value : Real);
    procedure SetTubeBend(Value : Integer);
    procedure SetBallDiam(Value : Integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddSlip(Value : Real);
  published
    property Align;
    property Anchors;
    property BorderStyle: TBorderStyle read fBorderStyle write SetBorderStyle default bsNone;
    property BackColor:  TColor read fBackColor write SetBackColor default clBlack;
    property BallColor:  TColor read fBallColor write SetBallColor default clGray;
    property TubeColor:  TColor read fTubeColor write SetTubeColor default clWhite;
    property ScaleColor: TColor read fScaleColor write SetScaleColor default clLime;
    property Slip: Real read fSlip write SetSlip;
    property BallDiameter: Integer read fBallDiam write SetBallDiam default 10;
    property TubeBend: Integer read fTubeBend write SetTubeBend default 30;
    property Constraints;
    property Enabled;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
  end;

procedure Register ;

implementation

uses Consts;

const
  Radian = 180.0 / Pi ;

type
  TBltBitmap = class(TBitmap)
    procedure MakeLike(ATemplate: TBitmap);
  end;

{ -------------------------------------------------------------------- }
{ TBltBitmap }
{ -------------------------------------------------------------------- }

procedure TBltBitmap.MakeLike(ATemplate: TBitmap);
begin
  Width := ATemplate.Width;
  Height := ATemplate.Height;
  Canvas.Brush.Color := clWindowFrame;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(Rect(0, 0, Width, Height));
end;

{ -------------------------------------------------------------------- }
{ ThhAvSlip }
{ -------------------------------------------------------------------- }

constructor ThhAvSlip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csFramed, csOpaque];
  { default values }
  fSlip        := 0.0;
  fBorderStyle := bsNone;
  fScaleColor  := clLime;
  fBackColor   := clBlack;
  fBallColor   := clMaroon;
  fTubeColor   := clWhite;
  fTubeBend := 30 ;
  fBallDiam := 10 ;
  Width  := 200;
  Height := 50;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvSlip.Paint;
var
  TheImage: TBitmap;
  OverlayImage: TBltBitmap;
  PaintRect: TRect;
begin
  with Canvas do
  begin
    TheImage := TBitmap.Create;
    try
      TheImage.Height := Height;
      TheImage.Width := Width;
      PaintBackground(TheImage);
      PaintRect := ClientRect;
      if fBorderStyle = bsSingle then InflateRect(PaintRect, -1, -1);
      OverlayImage := TBltBitmap.Create;
      OverlayImage.Canvas.Font := Font ;
      try
        OverlayImage.MakeLike(TheImage);
        PaintBackground(OverlayImage);
        PaintSlipImage(OverlayImage, PaintRect);
        TheImage.Canvas.CopyMode := cmSrcInvert;
        TheImage.Canvas.Draw(0, 0, OverlayImage);
        TheImage.Canvas.CopyMode := cmSrcCopy;
      finally
        OverlayImage.Free;
      end;
      Canvas.CopyMode := cmSrcCopy;
      Canvas.Draw(0, 0, TheImage);
    finally
      TheImage.Destroy;
    end;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvSlip.PaintBackground(AnImage: TBitmap);
var
  ARect: TRect;
begin
  with AnImage.Canvas do
  begin
    CopyMode := cmBlackness;
    ARect := Rect(0, 0, Width, Height);
    CopyRect(ARect, Animage.Canvas, ARect);
    CopyMode := cmSrcCopy;
  end;
end;

{ -------------------------------------------------------------------- }

procedure XYvalue( var R, X, Y : integer ; Angle : real ) ;
begin
  X :=  Round(R * Sin(Angle/Radian )) ;
  Y :=  Round(R * Cos(Angle/Radian )) ;
end ;

{ -------------------------------------------------------------------- }

procedure ThhAvSlip.PaintSlipImage(AnImage: TBitmap; PaintRect: TRect);

var
  Xctr,Yctr : integer ;
  RadiusM : integer ;
  RadiusB : integer ;
  RadiusI : integer ;
  RadiusO : integer ;
  Xc,Yc   : integer ;

procedure ShowTube ;
var
  Xb,Yb   : integer ;
  X1,Y1   : integer ;
  X2,Y2   : integer ;
begin
  XYvalue(RadiusI,X1,Y1,fTubeBend) ;
  XYvalue(RadiusO,X2,Y2,fTubeBend) ;
  XYvalue(RadiusB,Xb,Yb,fTubeBend) ;
  with AnImage.Canvas do
  begin
    Brush.Color := fBackColor;
    FillRect(PaintRect);
    Pen.Color := fScaleColor ;
    Pen.Width := 2 ;
    Brush.Style := bsSolid ;
    Arc( Xctr-RadiusI,Yctr-RadiusI,
         Xctr+RadiusI,Yctr+RadiusI,
         Xctr-X1,ROUND(Yctr+Y1),Xctr+X1,ROUND(Yctr+Y1)) ;
    Arc( Xctr-RadiusO,Yctr-RadiusO,Xctr+RadiusO,Yctr+RadiusO,
         Xctr-X2,ROUND(Yctr+Y2),Xctr+X2,ROUND(Yctr+Y2)) ;
    Arc( Xctr+Xc-RadiusB,Yctr+Yc-RadiusB,Xctr+Xc+RadiusB,Yctr+Yc+RadiusB,
         Xctr+X2,Yctr+Y2,Xctr+X1,Yctr+Y1) ;
    Arc( Xctr-Xc-RadiusB,Yctr+Yc-RadiusB,Xctr-Xc+RadiusB,Yctr+Yc+RadiusB,
         Xctr-X1,Yctr+Y1,Xctr-X2,Yctr+Y2) ;
    Brush.Color := fTubeColor ;
    Brush.Style := bsSolid ;
    FloodFill(Xctr,Yctr+RadiusM,fScaleColor,fsBorder) ;
  end ;
end ;

procedure ShowBall( Value : Real ) ;
var
  Theta : real ;
  X1,Y1 : integer ;
begin
{ Check ball offset angle and limits }
  Theta := Value ;
  if Theta >  fTubeBend then Theta :=  fTubeBend ;
  if Theta < -fTubeBend then Theta := -fTubeBend ;
  XYvalue(RadiusM,Xc,Yc,Theta) ;
  with AnImage.Canvas do
  begin
{ Draw the ball }
    Pen.Color := fBallColor ;
    Pen.Width := 1 ;
    Brush.Color := fBallColor ;
    Brush.Style := bsSolid ;
    Ellipse( Xctr+Xc+RadiusB-1,Yctr+Yc-1+RadiusB-1,
             Xctr+Xc-RadiusB+1,Yctr+Yc-1-RadiusB+1) ;
{ Draw the overlaid zero-slip reference scribings }
    Theta := ArcSin(RadiusB/RadiusM)*Radian ;
    XYvalue(RadiusM,X1,Y1,Theta) ;
    Pen.Color := fScaleColor ;
    Pen.Width := 3 ;
    MoveTo(Xctr+X1,Yctr+Y1-RadiusB) ;
    LineTo(Xctr+X1,Yctr+Y1+RadiusB-2) ;
    MoveTo(Xctr-X1-1,Yctr+Y1-RadiusB) ;
    LineTo(Xctr-X1-1,Yctr+Y1+RadiusB-2) ;
  end ;
end ;

begin

{ Compute tube shape and position given image size,tube bend and ball size }

{ First compute radius values }
  RadiusB := (fBallDiam+1) DIV 2 ;
  RadiusO := ROUND((AnImage.Width-RadiusB*2)/sin(fTubeBend/Radian)/2.0+0.5) ;
  RadiusM := RadiusO-RadiusB ;
  RadiusI := RadiusM-RadiusB ;
{ Find effective centre of radius for the gauge }
  XYvalue(RadiusM,Xc,Yc,fTubeBend) ;
  Xctr := (AnImage.Width DIV 2) ;
  Yctr := (AnImage.Height+RadiusB-Yc-RadiusO ) DIV 2 ;
{ Show the Tube and the Ball }
  ShowTube ;
  ShowBall( fSlip ) ;
end ;

{ -------------------------------------------------------------------- }

procedure ThhAvSlip.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> fBorderStyle then
  begin
    fBorderStyle := Value;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvSlip.SetScaleColor(Colour : TColor);
begin
  if Colour <> fScaleColor then
  begin
    fScaleColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvSlip.SetBackColor(Colour : TColor);
begin
  if Colour <> fBackColor then
  begin
    fBackColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvSlip.SetBallColor(Colour : TColor);
begin
  if Colour <> fBallColor then
  begin
    fBallColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvSlip.SetTubeColor(Colour : TColor);
begin
  if Colour <> fTubeColor then
  begin
    fTubeColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvSlip.SetTubeBend(Value : Integer);
var
  TempValue: Integer;
begin
  TempValue := fTubeBend ;
    if Value < 5 then Value := 5 ;
  if fTubeBend <> Value then
  begin
    fTubeBend := Value;
    if TempValue <> Value then { only refresh if changed }
      Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvSlip.SetBallDiam(Value : Integer);
var
  TempValue: Integer;
begin
  TempValue := fBallDiam ;
    if Value < 2 then Value := 2 ;
  if fBallDiam <> Value then
  begin
    fBallDiam := Value;
    if TempValue <> Value then { only refresh if changed }
      Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvSlip.SetSlip(Value : Real);
var
  TempValue: Real;
begin
  TempValue := fSlip ;
  if fSlip <> Value then
  begin
    fSlip := Value;
    if TempValue <> Value then { only refresh if changed }
      Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvSlip.AddSlip(Value: Real);
begin
  SetSlip( fSlip + Value ) ;
end;

{ -------------------------------------------------------------------- }

procedure Register ;
begin
  RegisterComponents( 'Howie' , [ThhAvSlip] ) ;
end ;

end.
