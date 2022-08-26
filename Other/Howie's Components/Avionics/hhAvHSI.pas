unit hhAvHSI;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls;

const
  Radian = 180.0 / pi ;

type
  TCourseMode = (cmOff, cmTo, cmFrom, cmNoDir ) ;

  ThhAvHSI = class(TGraphicControl)
  private
    fBearing      : integer ;
    fCompass      : integer ;
    fCourse1      : integer ;
    fCourse2      : integer ;
    fCourseMode1  : TCourseMode ;
    fCourseMode2  : TCourseMode ;
    fDeviation    : integer ;
    fHeading      : integer ;
    fBorderStyle  : TBorderStyle;
    fFullBearing  : boolean ;
    fBackColor    : TColor;
    fBearingColor : TColor;
    fCardinalColor: TColor;
    fCourse1Color : TColor;
    fCourse2Color : TColor;
    fHeadingColor : TColor;
    fLubberColor  : TColor;
    fScaleColor   : TColor;
    procedure PaintBackground(AnImage: TBitmap);
    procedure PaintHSIimage(AnImage: TBitmap; PaintRect: TRect);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetBackColor(Colour : TColor);
    procedure SetBearingColor(Colour : TColor);
    procedure SetCardinalColor(Colour : TColor);
    procedure SetCourse1Color(Colour : TColor);
    procedure SetCourse2Color(Colour : TColor);
    procedure SetHeadingColor(Colour : TColor);
    procedure SetLubberColor(Colour : TColor);
    procedure SetScaleColor(Colour : TColor);
    procedure SetBearing(Value : integer);
    procedure SetFullBearing(Mode : Boolean);
    procedure SetCompass(Value : integer);
    procedure SetCourse1(Value : integer);
    procedure SetCourse2(Value : integer);
    procedure SetCourseMode1(Value : TCourseMode);
    procedure SetCourseMode2(Value : TCourseMode);
    procedure SetDeviation(Value : integer);
    procedure SetHeading(Value : integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property BorderStyle:  TBorderStyle read fBorderStyle write SetBorderStyle default bsNone;
    property BackColor:    TColor read fBackColor write SetBackColor default clBlack;
    property BearingColor: TColor read fBearingColor write SetBearingColor default clRed;
    property CardinalColor:TColor read fCardinalColor write SetCardinalColor default clAqua;
    property Course1Color: TColor read fCourse1Color write SetCourse1Color default clAqua;
    property Course2Color: TColor read fCourse2Color write SetCourse2Color default clFuchsia;
    property HeadingColor: TColor read fHeadingColor write SetHeadingColor default clYellow;
    property LubberColor:  TColor read fLubberColor write SetLubberColor default clWhite;
    property ScaleColor:   TColor read fScaleColor write SetScaleColor default clLime;
    property Constraints;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Bearing: integer read fBearing write SetBearing default 0;
    property Compass: integer read fCompass write SetCompass default 0;
    property Heading: integer read fHeading write SetHeading default 0;
    property FullBearing: Boolean read fFullBearing write SetFullBearing default true;
    property Course1: integer read fCourse1 write SetCourse1 default 0;
    property Course2: integer read fCourse2 write SetCourse2 default 0;
    property CourseMode1: TCourseMode read fCourseMode1 write SetCourseMode1 default cmOff;
    property CourseMode2: TCourseMode read fCourseMode2 write SetCourseMode2 default cmOff;
    property Deviation: integer read fDeviation write SetDeviation default 0;
    property ShowHint;
    property Visible;
  end;

procedure Register ;

implementation

uses Consts;

type
  TBltBitmap = class(TBitmap)
    procedure MakeLike(ATemplate: TBitmap);
  end;

var
  UpdateReq : boolean ;

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
{ ThhAvHSI }
{ -------------------------------------------------------------------- }

constructor ThhAvHSI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csFramed, csOpaque];
  { default values }
  fBearing       := 0;
  fCompass       := 0;
  fCourse1       := 0;
  fCourse2       := 0;
  fDeviation     := 0;
  fHeading       := 0;
  fFullBearing   := true ;
  fCourseMode1   := cmNoDir;
  fCourseMode2   := cmOff;
  fBorderStyle   := bsNone;
  fBackColor     := clBlack;
  fBearingColor  := clRed;
  fCardinalColor := clAqua;
  fCourse1Color  := clAqua;
  fCourse2Color  := clFuchsia;
  fHeadingColor  := clYellow;
  fLubberColor   := clWhite;
  fScaleColor    := clLime;
  Font.Color     := clWhite ;
  Font.Style     := [fsBold] ;
  Font.Size      := 12 ;
  Font.Name      := 'Arial' ;
  Width  := 301;
  Height := 301;
  UpdateReq :=  true ;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvHSI.Paint;
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
        PaintHSIimage(OverlayImage, PaintRect);
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

procedure ThhAvHSI.PaintBackground(AnImage: TBitmap);
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

procedure ThhAvHSI.PaintHSIimage(AnImage: TBitmap; PaintRect: TRect);

{ Draw and annotate a Horizontal Situation Indicator (HSI) }

var
  Radius   : integer ;
  Xmid     : integer ;
  Ymid     : integer ;
  Sector   : boolean ;

procedure DrawCompassRose( Compass : integer ) ;
{ Compass Rose }

var
  Count    : integer ;
  Brg      : integer ;
  Angle    : integer ;
  Cangle   : integer ;
  AngStr   : string ;
  Th       : integer ;
  R2,R3,R4 : real ;
  RX,RY    : real ;
  RX2,RY2  : real ;

procedure AngleTextOut( CV: TCanvas ; const sText : String ; X,Y,ang : integer ) ;
var
  LogFont  : TLogFont ;
  hOldFont : HFont ;
  hNewFont : HFont ;
begin
  while ang > 360 do ang := ang - 360 ;
  while ang < 0 do ang := ang + 360 ;
  GetObject(CV.Font.Handle,sizeof(LogFont),Addr(LogFont)) ;
  with Logfont do
  begin
    lfEscapement := ang*10 ;
    lfPitchAndFamily := FF_DONTCARE;
  end ;
  hNewFont := CreateFontIndirect(LogFont) ;
  SetBkMode(CV.Handle,TRANSPARENT) ;
  hOldFont := SelectObject( CV.Handle , hNewFont ) ;
  CV.TextOut(X,Y,sText) ;
  hNewFont := SelectObject(CV.Handle,hOldFont) ;
  DeleteObject(hNewFont) ;
end ;

procedure TextXY(var RX,RY : real ; Angle,W,H : integer) ;
var
  Ang      : real ;

begin
  while angle > 270 do angle := angle - 360 ;
  while angle < -90 do angle := angle + 360 ;
  Ang := (Angle+90)/Radian ;
  RX := RX + W*Cos(ang)/2 ;
  RY := RY - W*Sin(ang)/2 ;
end ;
 
begin

  with AnImage.Canvas do
  begin

{ Outer circle }

    Pen.Color := fScaleColor ;
    Pen.Width := 2 ;
    Ellipse(Xmid-Radius,Ymid-Radius,Xmid+Radius,Ymid+Radius) ;

{ Inner Circle }

    Pen.Width := 2 ;
    Th := Canvas.TextHeight('00') ;
    R2 := Radius - Th ;
    R3 := Radius - Th / 2 ;
    R4 := Radius - Th * 2 ;
    Ellipse( Xmid - Round(R4),Ymid - Round(R4),
                    Xmid + Round(R4),Ymid + Round(R4)) ;

{ angles and pips }

    Pen.Width := 1 ;
    for count := 0 to 71 do
    begin
      Angle := Count * 5 ;
      Cangle := -(Angle + Compass) ;
      Brg := 450 - Angle ; { reverse the direction for compass }
      while Brg >= 360 do Brg := Brg - 360 ; { reduce to primary compass }

      RY2 := Ymid + R2 * Sin(Cangle/Radian) ;
      if RY2 > AnImage.Height then continue ; { exit if outside image bounds }
      RX2 := Xmid + R2 * Cos(Cangle/Radian) ;

      RX := Xmid + Radius * Cos(Cangle/Radian) ;
      RY := Ymid + Radius * Sin(Cangle/Radian) ;
      MoveTo(Trunc(RX),Trunc(RY)) ;

      if Brg MOD 90 = 0
      then begin
        Font.Size := 12 ;
        Font.Color := fCardinalColor ;
        case Brg DIV 90 of
          0: AngStr := 'N' ;
          1: AngStr := 'E' ;
          2: AngStr := 'S' ;
          3: AngStr := 'W' ;
        end
      end
      else begin
        Font.Color := fLubberColor ;
        if Brg MOD 30 = 0
        then begin
          if fFullBearing
            then AngStr := IntToStr(Brg)
            else AngStr := IntToStr( Brg DIV 10 ) ;
        end
        else begin
          AngStr := ' ' ;
          if Brg Mod 10 = 0
          then begin
            RX2 := RX - Th * Cos(Cangle/Radian) ;
            RY2 := RY - Th * Sin(Cangle/Radian) ;
          end
          else begin
            RX2 := RX - Th / 2 * Cos(Cangle/Radian) ;
            RY2 := RY - Th / 2 * Sin(Cangle/Radian) ;
          end ;
        end ;
      end ;
      LineTo(Trunc(RX2),Trunc(RY2)) ;

      RX := Xmid + R3 * Cos(Cangle/Radian) ;
      RY := Ymid + R3 * Sin(Cangle/Radian) ;
      TextXY( RX, RY, -Cangle , TextWidth(AngStr), TextHeight(AngStr)) ;
      AngleTextOut( AnImage.Canvas,AngStr,Round(RX),Round(RY),-(Cangle+90)) ;
    end ;
  end ;
end ;

procedure DrawPlane ;
{ Orientation Plane symbol }

begin
  with AnImage.Canvas do
  begin
    Pen.Width := 2 ;
    Pen.Color := fLubberColor ;
    MoveTo( Xmid-20,Ymid ) ;
    LineTo( Xmid+20,Ymid ) ;
    MoveTo( Xmid,Ymid-10 ) ;
    LineTo( Xmid,Ymid+25 ) ;
    MoveTo( Xmid-10,Ymid+20 ) ;
    LineTo( Xmid+10,Ymid+20 ) ;
  end ;
end ;

procedure DrawBearingMarker( Bearing : integer ) ;
{ Bearing Marker }

var
  Pointer : array[0..2] of Tpoint ;
  Angle   : integer ;
  RX,RY   : real ;
  RX2,RY2 : real ;
  RX3,RY3 : real ;

begin
  with AnImage.Canvas do
  begin
    Angle := Bearing-fCompass ;
    Pen.Width := 1 ;
    Pen.Color := fBackColor ;
    Brush.Color := fBearingColor ;
    RX := Xmid + (Radius+1) * Cos((Angle-90)/Radian) ;
    RY := Ymid + (Radius+1) * Sin((Angle-90)/Radian) ;
    RX2 := 10 * Cos((Angle+180)/Radian) ;
    RY2 := 10 * Sin((Angle+180)/Radian) ;
    RX3 := Xmid + (Radius-10) * Cos((Angle-90)/Radian) ;
    RY3 := Ymid + (Radius-10) * Sin((Angle-90)/Radian) ;
    Pointer[0] := Point(Round(RX+RX2),Round(RY+RY2)) ;
    Pointer[1] := Point(Round(RX-RX2),Round(RY-RY2)) ;
    Pointer[2] := Point(Round(RX3),Round(RY3)) ;
    Polygon( Pointer ) ;
  end ;
end ;

procedure DrawCourseLine1( Course : integer ; 
                           Cmode : TCourseMode ; Deviation : integer ) ;
{ Course Line }

var
  Count   : integer ;
  Pointer : array[0..2] of Tpoint ;
  Angle   : integer ;
  R4      : integer ;
  RX,RY   : real ;
  RX1,RY1 : real ;
  RX2,RY2 : real ;
  RX3,RY3 : real ;
begin
  if Cmode = cmOff then exit ;
  with AnImage.Canvas do
  begin
    R4 := Radius - Canvas.TextHeight('00')*2 ;
    Angle := Course - fCompass ;
    Pen.Color := fCourse1Color ;
    Brush.Color := fBackColor ;
    RX := R4 * Cos((Angle-90)/Radian) ;
    RY := R4 * Sin((Angle-90)/Radian) ;

{ Deviation circles }

    Pen.Width := 1 ;
    Pen.Color := fLubberColor ;
    Brush.Color := fBackColor ;
    for Count := 1 to 3 do
    begin
      if (Count*50)+5 > R4 then continue ;
      RX2 := Count*50 * Cos((Angle+180)/Radian) ;
      RY2 := Count*50 * Sin((Angle+180)/Radian) ;
      Ellipse( Xmid-Round(RX2)-5 , Ymid-Round(RY2)-5 ,
               Xmid-Round(RX2)+5 , Ymid-Round(RY2)+5 ) ;
      Ellipse( Xmid+Round(RX2)-5 , Ymid+Round(RY2)-5 ,
               Xmid+Round(RX2)+5 , Ymid+Round(RY2)+5 ) ;
    end ;

{ Course Line portions }

    Pen.Color := fCourse1Color ;
    Pen.Width := 2 ;
    Brush.Color := fCourse1Color ;
    MoveTo( Xmid-Round(RX) , Ymid-Round(RY) ) ;
    LineTo( Xmid-Round(RX/2) , Ymid-Round(RY/2) ) ;
    MoveTo( Xmid+Round(RX/2) , Ymid+Round(RY/2) ) ;
    LineTo( Xmid+Round(RX) , Ymid+Round(RY) ) ;
    while Deviation > 180 do Deviation := Deviation - 360 ;
    while Deviation < -180 do Deviation := Deviation + 360 ;

{ Header }

    Pen.Color := fCourse1Color ;
    Pen.Width := 1 ;
    RX2 := 14 * Cos((Angle+120)/Radian) ;
    RY2 := 14 * Sin((Angle+120)/Radian) ;
    RX3 := 14 * Cos((Angle+60)/Radian) ;
    RY3 := 14 * Sin((Angle+60)/Radian) ;
    Pointer[0] := Point( Xmid+Round(RX) , Ymid+Round(RY) ) ;
    Pointer[1] := Point( Xmid+Round(RX+RX2) , Ymid+Round(RY+RY2) ) ;
    Pointer[2] := Point( Xmid+Round(RX+RX3) , Ymid+Round(RY+RY3) ) ;
    Polygon( Pointer ) ;

{ To/From Direction Marker }

    Pen.Color := fCourse1Color ;
    Brush.Color := fBackColor ;
    Pen.Width := 1 ;
    if Sector then RX2 := R4 else RX2 := Radius * 2 / 3 ;
    RX1 := RX2 * Cos((Angle-90)/Radian) ;
    RY1 := RX2 * Sin((Angle-90)/Radian) ;
    RX2 := 14 * Cos((Angle+120)/Radian) ;
    RY2 := 14 * Sin((Angle+120)/Radian) ;
    RX3 := 14 * Cos((Angle+60)/Radian) ;
    RY3 := 14 * Sin((Angle+60)/Radian) ;
    if Cmode = cmTo
    then begin
      Pointer[0] := Point( Xmid+Round(RX1/2) , Ymid+Round(RY1/2) ) ;
      Pointer[1] := Point( Xmid+Round(RX1/2+RX2) , Ymid+Round(RY1/2+RY2) ) ;
      Pointer[2] := Point( Xmid+Round(RX1/2+RX3) , Ymid+Round(RY1/2+RY3) ) ;
      Polygon( Pointer ) ;
    end ;
    if Cmode = cmFrom
    then begin
      Pointer[0] := Point( Xmid-Round(RX1/2) , Ymid-Round(RY1/2) ) ;
      Pointer[1] := Point( Xmid-Round(RX1/2+RX2) , Ymid-Round(RY1/2+RY2) ) ;
      Pointer[2] := Point( Xmid-Round(RX1/2+RX3) , Ymid-Round(RY1/2+RY3) ) ;
      Polygon( Pointer ) ;
    end ;

{ Deviation marker }

    Pen.Width := 2 ;
    Deviation := Deviation*5 ;
    if ABS(Deviation) > 0.86*R4
    then begin
      Deviation := Round(0.86*R4) * Deviation DIV ABS(Deviation) ;
      Pen.Color := clRed ;
    end ;
    RX2 := Deviation * Cos((Angle+180)/Radian) ;
    RY2 := Deviation * Sin((Angle+180)/Radian) ;
    MoveTo( Xmid-Round(RX/2+RX2) , Ymid-Round(RY/2+RY2) ) ;
    LineTo( Xmid+Round(RX/2-RX2) , Ymid+Round(RY/2-RY2) ) ;
  end ;
end ;

procedure DrawCourseLine2( Course : integer ; Cmode : TCourseMode ) ;
{ Course Line }

var
  Pointer : array[0..2] of Tpoint ;
  Angle   : integer ;
  R4      : integer ;
  RX,RY   : real ;
  RX2,RY2 : real ;
  RX3,RY3 : real ;
  RX4,RY4 : real ;
begin
  if Cmode = cmOff then exit ;
  with AnImage.Canvas do
  begin
    R4 := Radius - Canvas.TextHeight('00')*2 ;
    Angle := Course - fCompass ;
    Pen.Color := fCourse2Color ;
    Brush.Color := fBackColor ;
    RX := R4 * Cos((Angle-90)/Radian) ;
    RY := R4 * Sin((Angle-90)/Radian) ;

{ Course Line portions }

    Pen.Color := fCourse2Color ;
    Pen.Width := 2 ;
    Brush.Color := fBackColor ;
    RX4 := RX*3/4 ;
    RY4 := RY*3/4 ;
    MoveTo( Xmid-Round(RX) , Ymid-Round(RY) ) ;
    LineTo( Xmid-Round(RX4) , Ymid-Round(RY4) ) ;
    MoveTo( Xmid+Round(RX4) , Ymid+Round(RY4) ) ;
    LineTo( Xmid+Round(RX) , Ymid+Round(RY) ) ;

{ Header }

    Pen.Color := fCourse2Color ;
    Pen.Width := 1 ;
    RX2 := 14 * Cos((Angle+120)/Radian) ;
    RY2 := 14 * Sin((Angle+120)/Radian) ;
    RX3 := 14 * Cos((Angle+60)/Radian) ;
    RY3 := 14 * Sin((Angle+60)/Radian) ;
    Pointer[0] := Point( Xmid+Round(RX) , Ymid+Round(RY) ) ;
    Pointer[1] := Point( Xmid+Round(RX+RX2) , Ymid+Round(RY+RY2) ) ;
    Pointer[2] := Point( Xmid+Round(RX+RX3) , Ymid+Round(RY+RY3) ) ;
    Polygon( Pointer ) ;

{ To/From Direction Marker }

    Pen.Color := fCourse2Color ;
    Brush.Color := fBackColor ;
    Pen.Width := 1 ;
    RX2 := 14 * Cos((Angle+120)/Radian) ;
    RY2 := 14 * Sin((Angle+120)/Radian) ;
    RX3 := 14 * Cos((Angle+60)/Radian) ;
    RY3 := 14 * Sin((Angle+60)/Radian) ;
    if Cmode = cmTo
    then begin
      Pointer[0] := Point( Xmid+Round(RX4) , Ymid+Round(RY4) ) ;
      Pointer[1] := Point( Xmid+Round(RX4+RX2) , Ymid+Round(RY4+RY2) ) ;
      Pointer[2] := Point( Xmid+Round(RX4+RX3) , Ymid+Round(RY4+RY3) ) ;
      Polygon( Pointer ) ;
    end ;
    if Cmode = cmFrom
    then begin
      Pointer[0] := Point( Xmid-Round(RX4) , Ymid-Round(RY4) ) ;
      Pointer[1] := Point( Xmid-Round(RX4+RX2) , Ymid-Round(RY4+RY2) ) ;
      Pointer[2] := Point( Xmid-Round(RX4+RX3) , Ymid-Round(RY4+RY3) ) ;
      Polygon( Pointer ) ;
    end ;
  end ;
end ;

procedure DrawHeadingMarker( Heading : integer ) ;
{ Heading Marker }

var
  HDGpointer : array[0..4] of TPoint ;
  Angle      : integer ;
  RX,RY      : real ;
  RX2,RY2    : real ;
begin
  with AnImage.Canvas do
  begin
    Angle := Heading-fCompass ;
    Pen.Color := fBackColor ;
    Pen.Width := 1 ;
    Brush.Color := fHeadingColor ;
    Brush.Style := bsSolid ;
    RX := Xmid + (Radius+10) * Cos((Angle-90)/Radian) ;
    RY := Ymid + (Radius+10) * Sin((Angle-90)/Radian) ;
    RX2 := 10 * Cos((Angle+180)/Radian) ;
    RY2 := 10 * Sin((Angle+180)/Radian) ;
    HDGpointer[0] := Point( Round(RX+RX2) , Round(RY+RY2) ) ;
    HDGpointer[2] := Point( Round(RX-RX2) , Round(RY-RY2) ) ;
    RX := Xmid + (Radius) * Cos((Angle-90)/Radian) ;
    RY := Ymid + (Radius) * Sin((Angle-90)/Radian) ;
    HDGpointer[1] := Point( Round(RX) , Round(RY) ) ;
    HDGpointer[3] := Point( Round(RX-RX2) , Round(RY-RY2) ) ;
    HDGpointer[4] := Point( Round(RX+RX2) , Round(RY+RY2) ) ;
    Polygon( HDGPointer ) ;
  end ;
end ;

procedure DrawLubberMarker ;
{ Lubber Marker }

var
  Pointer  : array[0..2] of Tpoint ;

begin
  with AnImage.Canvas do
  begin
    Pen.Width := 1 ;
    Pen.Color := fLubberColor ;
    Pen.Color := fBackColor ;
    Brush.Style := bsSolid ;
    Brush.Color := fLubberColor ;
    Pointer[0] := Point(Xmid-10,Ymid-Radius-10) ;
    Pointer[1] := Point(Xmid+10,Ymid-Radius-10) ;
    Pointer[2] := Point(Xmid,Ymid-Radius) ;
    Polygon( Pointer ) ;
  end ;
end ;

begin
  AnImage.Canvas.Font := Font ;
  if (csDesigning in ComponentState) OR UpdateReq
  then begin
  with AnImage do
    begin
      Canvas.Brush.Color := fBackColor ;
      Canvas.FillRect( PaintRect ) ;
      Xmid := (Width DIV 2) ;
      Radius := Xmid-10 ;
      if Width > Height
      then begin
        Ymid := (Width DIV 2) ;
        Sector := true ;
      end
      else begin
        Ymid := (Height DIV 2) ;
        Sector := false ;
      end ;
      if BorderStyle = bsSingle
      then begin
        INC(Xmid) ;
        INC(Ymid) ;
      end ;
    end ;
    with AnImage.Canvas do
    begin
      with Font do
      begin
        Size := 12 ;
        Color := clWhite ;
      end ;

      DrawCompassRose( fCompass ) ;
      DrawPlane ;
      DrawBearingMarker( fBearing ) ;
      DrawHeadingMarker( fHeading ) ;
      DrawCourseLine1( fCourse1 , fCourseMode1 , fDeviation ) ;
      DrawCourseLine2( fCourse2 , fCourseMode2 ) ;
      DrawLubberMarker ;
    end ;
  end ;
  UpdateReq := false ;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvHSI.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> fBorderStyle then
  begin
    fBorderStyle := Value;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvHSI.SetBackColor(Colour : TColor);
begin
  if Colour <> fBackColor then
  begin
    fBackColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvHSI.SetBearingColor(Colour : TColor);
begin
  if Colour <> fBearingColor then
  begin
    fBearingColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvHSI.SetCardinalColor(Colour : TColor);
begin
  if Colour <> fCardinalColor then
  begin
    fCardinalColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvHSI.SetCourse1Color(Colour : TColor);
begin
  if Colour <> fCourse1Color then
  begin
    fCourse1Color := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvHSI.SetCourse2Color(Colour : TColor);
begin
  if Colour <> fCourse2Color then
  begin
    fCourse2Color := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvHSI.SetHeadingColor(Colour : TColor);
begin
  if Colour <> fHeadingColor then
  begin
    fHeadingColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvHSI.SetLubberColor(Colour : TColor);
begin
  if Colour <> fLubberColor then
  begin
    fLubberColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvHSI.SetScaleColor(Colour : TColor);
begin
  if Colour <> fScaleColor then
  begin
    fScaleColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvHSI.SetBearing(Value : integer);
var
  TempValue: integer;
begin
  TempValue := fBearing ;
  if fBearing <> Value then
  begin
    fBearing := Value;
    while fBearing > 360 do fBearing := fBearing - 360 ;
    while fBearing <   0 do fBearing := fBearing + 360 ;
    if (csDesigning in ComponentState)
      then Refresh
      else if TempValue <> Value then UpdateReq := true ;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvHSI.SetCompass(Value : integer);
var
  TempValue: integer;
begin
  TempValue := fCompass ;
    fCompass := Value;
    while fCompass > 360 do fCompass := fCompass - 360 ;
    while fCompass <   0 do fCompass := fCompass + 360 ;
    if (TempValue <> Value) or (csDesigning in ComponentState) or UpdateReq
      then Refresh ;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvHSI.SetCourse1(Value : integer);
var
  TempValue: integer;
begin
  TempValue := fCourse1 ;
  if fCourse1 <> Value then
  begin
    fCourse1 := Value;
    while fCourse1 > 360 do fCourse1 := fCourse1 - 360 ;
    while fCourse1 <   0 do fCourse1 := fCourse1 + 360 ;
    if (csDesigning in ComponentState)
      then Refresh
      else if TempValue <> Value then UpdateReq := true ;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvHSI.SetCourse2(Value : integer);
var
  TempValue: integer;
begin
  TempValue := fCourse2 ;
  if fCourse2 <> Value then
  begin
    fCourse2 := Value;
    while fCourse2 > 360 do fCourse2 := fCourse2 - 360 ;
    while fCourse2 <   0 do fCourse2 := fCourse2 + 360 ;
    if (csDesigning in ComponentState)
      then Refresh
      else if TempValue <> Value then UpdateReq := true ;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvHSI.SetCourseMode1(Value : TCourseMode);
var
  TempValue: TCourseMode;
begin
  TempValue := fCourseMode1 ;
  if fCourseMode1 <> Value then
  begin
    fCourseMode1 := Value;
    if (csDesigning in ComponentState)
      then Refresh
      else if TempValue <> Value then UpdateReq := true ;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvHSI.SetCourseMode2(Value : TCourseMode);
var
  TempValue: TCourseMode;
begin
  TempValue := fCourseMode2 ;
  if fCourseMode2 <> Value then
  begin
    fCourseMode2 := Value;
    if (csDesigning in ComponentState)
      then Refresh
      else if TempValue <> Value then UpdateReq := true ;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvHSI.SetDeviation(Value : integer);
var
  TempValue: integer;
begin
  TempValue := fDeviation ;
  if fDeviation <> Value then
  begin
    fDeviation := Value;
    while fDeviation > 360 do fDeviation := fDeviation - 360 ;
    while fDeviation <   0 do fDeviation := fDeviation + 360 ;
    if (csDesigning in ComponentState)
      then Refresh
      else if TempValue <> Value then UpdateReq := true ;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvHSI.SetHeading(Value : integer);
var
  TempValue: integer;
begin
  TempValue := fHeading ;
  if fHeading <> Value then
  begin
    fHeading := Value;
    while fHeading > 360 do fHeading := fHeading - 360 ;
    while fHeading <   0 do fHeading := fHeading + 360 ;
    if (csDesigning in ComponentState)
      then Refresh
      else if TempValue <> Value then UpdateReq := true ;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvHSI.SetFullBearing(Mode : boolean);
begin
  if Mode <> fFullBearing then
  begin
    fFullBearing := Mode;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure Register ;
begin
  RegisterComponents( 'Howie' , [ThhAvHSI] ) ;
end ;

end.
