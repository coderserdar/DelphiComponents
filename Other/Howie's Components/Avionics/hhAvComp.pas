unit hhAvComp;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls;

type
  TpsCoursePointer = (psArrow,psDelta,psPlane,psPtrLine) ;

  ThhAvComp = class(TGraphicControl)
  private
    fBearing       : Real ;
    fBorderStyle   : TBorderStyle ;
    fScaleColor    : TColor ;
    fBackColor     : TColor ;
    fPointerColor  : TColor ;
    fCardinalColor : TColor ;
    fCourseColor   : TColor ;
    fMagnetic      : boolean ;
    fShowMagnetic  : boolean ;
    fMagVar        : Real ;
    fFullBearing   : boolean ;
    fCourse        : Real ;
    fCoursePointer : TpsCoursePointer ;
    fShowCourse    : boolean ;
    fShowBearing   : boolean ;
    fToFrom        : boolean ;
    procedure PaintBackground(AnImage: TBitmap);
    procedure PaintCompassImage(AnImage: TBitmap; PaintRect: TRect);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetScaleColor(Colour : TColor);
    procedure SetBackColor(Colour : TColor);
    procedure SetPointerColor(Colour : TColor);
    procedure SetCardinalColor(Colour : TColor);
    procedure SetCourseColor(Colour : TColor);
    procedure SetBearing(Value : Real);
    procedure SetCourse(Value : Real);
    procedure SetMagnetic(Mode : Boolean);
    procedure SetShowMagnetic(Mode : Boolean);
    procedure SetShowBearing(Mode : Boolean);
    procedure SetShowCourse(Mode : Boolean);
    procedure SetFullBearing(Mode : Boolean);
    procedure SetMagVar(Value : Real);
    procedure SetCoursePointer(Value : TpsCoursePointer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddBearing(Value : Real);
    procedure AddCourse(Value : Real);
  published
    property Align;
    property Anchors;
    property BackColor: TColor read fBackColor write SetBackColor default clBlack;
    property BorderStyle: TBorderStyle read fBorderStyle write SetBorderStyle default bsNone;
    property PointerColor: TColor read fPointerColor write SetPointerColor default clWhite;
    property CardinalColor: TColor read fCardinalColor write SetCardinalColor default clLime;
    property CourseColor: TColor read fCourseColor write SetCourseColor default clRed;
    property Constraints;
    property Enabled;
    property Font;
    property ScaleColor: TColor read fScaleColor write SetScaleColor default clLime;
    property Magnetic: Boolean read fMagnetic write SetMagnetic default true;
    property ShowMagnetic: Boolean read fShowMagnetic write SetShowMagnetic default true;
    property ShowBearing: Boolean read fShowBearing write SetShowBearing default true;
    property ShowCourse: Boolean read fShowCourse write SetShowCourse default false;
    property FullBearing: Boolean read fFullBearing write SetFullBearing default true;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Bearing: Real read fBearing write SetBearing;
    property Course: Real read fCourse write SetCourse;
    property CoursePointer: TpsCoursePointer read fCoursePointer write SetCoursePointer default psArrow;
    property MagVar: Real read fMagVar write SetMagVar ;
    property ToFrom: boolean read fToFrom ;
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
{ ThhAvComp }
{ -------------------------------------------------------------------- }

constructor ThhAvComp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csFramed, csOpaque];
  { default values }
  fBearing      := 0.0 ;
  fCourse       := 0.0 ;
  fMagVar       := 0.0 ;
  fMagnetic     := true ;
  fShowMagnetic := true ;
  fShowCourse   := false ;
  fShowBearing  := true ;
  fFullBearing  := true ;
  fToFrom       := true ;
  fBorderStyle  := bsNone ;
  fScaleColor   := clLime ;
  fBackColor    := clBlack ;
  fPointerColor := clWhite ;
  fCardinalColor:= clLime ;
  fCourseColor  := clRed ;
  fCoursePointer:= psArrow ;
  Font.Color    := clWhite ;
  Font.Name     := 'Arial' ;
  Font.Size     := 12 ;
  Width  := 260 ;
  Height := 73 ;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvComp.Paint;
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
        PaintCompassImage(OverlayImage, PaintRect);
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

procedure ThhAvComp.PaintBackground(AnImage: TBitmap);
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

procedure ThhAvComp.PaintCompassImage(AnImage: TBitmap; PaintRect: TRect);

var
  Wide : integer ;
  Xctr : integer ;
  Yctr : integer ;
  Vpos : integer ;
  Heading : integer ;
  rHeading : real ;


procedure DrawVerticalMark( xval,yval,LenVal : integer ; Mark : string ) ;
var
  FontColor : TColor ;
  FontSize  : integer ;

begin
  with AnImage.Canvas do
  begin
    FontSize := Font.Size ;
    FontColor := Font.Color ;
    if (Mark = 'E')
    OR (Mark = 'W')
    OR (Mark = 'N')
    OR (Mark = 'S')
    then Font.Color := fCardinalColor ;
    Pen.Color := fScaleColor ;
    MoveTo(xval,yval) ;
    LineTo( xval , yval-LenVal) ;
    Brush.Color := fBackColor ;
    TextOut( xval-(TextWidth(Mark) DIV 2),yval-LenVal+Font.Height-2,Mark) ;
    Font.Color := FontColor ;
    Font.Size := FontSize ;
  end ;
end ;

procedure DrawSteps( xval,yval : integer ; Dstr : string ) ;
begin
  if (xval < -60) or (xval > AnImage.Width) then exit ;
  DrawVerticalMark( xval ,    yval, 10 , Dstr ) ;
  DrawVerticalMark( xval+10 , yval, 5 , '' ) ;
  DrawVerticalMark( xval+20 , yval, 8 , '' ) ;
  DrawVerticalMark( xval+30 , yval, 5 , '' ) ;
  DrawVerticalMark( xval+40 , yval, 8 , '' ) ;
  DrawVerticalMark( xval+50 , yval, 5 , '' ) ;
end ;

procedure DrawCourse ;
const
  Xa = 10 ;
  Xb = 5 ;
  Ya = 10 ;
  Yb = 5 ;
  Yf = 7 ;
  Yr = 6 ;
  Xf = 10 ;
  Xr = 5 ;
  Xd = 7 ;
  Yd = 7 ;
var
  Pointer : array [0..2] of TPoint ;
  Diff   : Real ;
  Xpos   : integer ;
  Ypos   : integer ;

begin
  Diff := fCourse - fBearing ;
  if Diff > 180.0 then Diff := Diff - 360.0
  else
  if Diff < -180.0 then Diff := Diff + 360.0 ;
  fToFrom := (ABS(Diff) < 90.0) ;
  if Diff > 90.0 then Diff := 180.0-Diff
  else
  if Diff < -90.0 then Diff := -180.0 - Diff ;

  Xpos := ROUND(Diff*2) ;
  if Xpos >= Xctr then Xpos := Xctr 
  else
  if Xpos <= -Xctr then Xpos := -Xctr ;
  with AnImage.Canvas do
  begin
    Ypos := Font.Height-10 ;
    Brush.Color := fCourseColor ;
    Pen.Color   := fBackColor ;
    Pen.Width := 1 ;
    if fToFrom
    then begin
{ "To" direction }
      case fCoursePointer of
      psArrow:
        begin
          Pointer[0] := Point( Xctr+Xpos,    Yctr-Ya ) ;
          Pointer[1] := Point( Xctr+Xpos+Xa, Yctr ) ;
          Pointer[2] := Point( Xctr+Xpos-Xa, Yctr ) ;
          Polygon( Pointer ) ;
          Pen.Color := fCourseColor ;
          Pen.Width := 5 ;
          MoveTo(Xctr+Xpos,Yctr) ;
          LineTo( Xctr+Xpos,Yctr+Yb) ;
        end ;
      psDelta:
        begin
          Pointer[0] := Point( Xctr+Xpos,    Yctr-Ya ) ;
          Pointer[1] := Point( Xctr+Xpos+Xa, Yctr ) ;
          Pointer[2] := Point( Xctr+Xpos-Xa, Yctr ) ;
          Polygon( Pointer ) ;
          Pointer[0] := Point( Xctr+Xpos,    Yctr ) ;
          Pointer[1] := Point( Xctr+Xpos+Xd, Yctr+Yd ) ;
          Pointer[2] := Point( Xctr+Xpos-Xd, Yctr+Yd ) ;
          Polygon( Pointer ) ;
        end ;
      psPlane:
        begin
          Pen.Color := fCourseColor ;
          Pen.Width := 5 ;
          MoveTo(Xctr+Xpos,Yctr-Ya) ;
          LineTo( Xctr+Xpos,Yctr+Ya) ;
          MoveTo(Xctr+Xpos-Xf,Yctr-Yf) ;
          LineTo( Xctr+Xpos+Xf,Yctr-Yf) ;
          MoveTo(Xctr+Xpos-Xr,Yctr+Yr) ;
          LineTo( Xctr+Xpos+Xr,Yctr+Yr) ;
        end ;
      psPtrLine:
        begin
          Pen.Color := fCourseColor ;
          Pen.Width := 3 ;
          MoveTo( Xctr+Xpos-5,Yctr+Ypos+3) ;
          LineTo( Xctr+Xpos+5,Yctr+Ypos+3) ;
          MoveTo(Xctr+Xpos,Yctr) ;
          LineTo( Xctr+Xpos,Yctr+Ypos) ;
        end ;
      end ;
    end
{ "From" direction }
    else begin
      case fCoursePointer of
      psArrow:
        begin
          Pointer[0] := Point( Xctr+Xpos,    Yctr+Ya ) ;
          Pointer[1] := Point( Xctr+Xpos+Xa, Yctr ) ;
          Pointer[2] := Point( Xctr+Xpos-Xa, Yctr ) ;
          Polygon( Pointer ) ;
          Pen.Color := fCourseColor ;
          Pen.Width := 5 ;
          MoveTo(Xctr+Xpos,Yctr) ;
          LineTo( Xctr+Xpos,Yctr-Yb) ;
        end ;
      psDelta:
        begin
          Pointer[0] := Point( Xctr+Xpos,    Yctr+Ya ) ;
          Pointer[1] := Point( Xctr+Xpos+Xa, Yctr ) ;
          Pointer[2] := Point( Xctr+Xpos-Xa, Yctr ) ;
          Polygon( Pointer ) ;
          Pointer[0] := Point( Xctr+Xpos,    Yctr ) ;
          Pointer[1] := Point( Xctr+Xpos+Xd, Yctr-Yd ) ;
          Pointer[2] := Point( Xctr+Xpos-Xd, Yctr-Yd ) ;
          Polygon( Pointer ) ;
        end ;
      psPlane:
        begin
          Pen.Color := fCourseColor ;
          Pen.Width := 5 ;
          MoveTo(Xctr+Xpos,Yctr+Ya) ;
          LineTo( Xctr+Xpos,Yctr-Ya) ;
          MoveTo(Xctr+Xpos-Xf,Yctr+Yr) ;
          LineTo( Xctr+Xpos+Xf,Yctr+Yr) ;
          MoveTo(Xctr+Xpos-Xr,Yctr-Yf) ;
          LineTo( Xctr+Xpos+Xr,Yctr-Yf) ;
        end ;
      psPtrLine:
        begin
          Pen.Color := fCourseColor ;
          Pen.Width := 3 ;
          MoveTo( Xctr+Xpos-5,Yctr-3) ;
          LineTo( Xctr+Xpos+5,Yctr-3) ;
          MoveTo(Xctr+Xpos,Yctr) ;
          LineTo( Xctr+Xpos,Yctr+Ypos) ;
        end ;
      end ;
    end ;
  end ;
end ;

procedure DrawHeadingPointer ;
const
  Xa = 25 ;
  Ya = 10 ;
  Yb = 20 ;
var
  Xtext     : integer ;
  Ytext     : integer ;
  Dstr      : string ;
  HDGMode   : string ;
  FontColor : TColor ;
  Pointer   : array [0..8] of TPoint ;
  W1,W2     : integer ;
  H1,H2     : integer ;
  FontSize  : integer ;

begin
  if fShowMagnetic
  then HDGmode := 'M'
  else HDGmode := 'T' ;
  Ytext := Yctr + 1 ;
  with AnImage.Canvas do
  begin
    FontSize := Font.Size ;
    Brush.Color := fBackColor ;
    Font.Size := 8 ;
    W1 := (TextWidth('X') DIV 2)+5 ;
    H1 := TextHeight('X') ;
    Font.Size := 12 ;
    W2 := (TextWidth('000') DIV 2)+5 ;
    H2 := TextHeight('000') ;
    Pen.Color := fPointerColor ;
    Pen.Width := 2 ;
    Pointer[0] := Point( Xctr,    Ytext ) ;
    Pointer[1] := Point( Xctr+W1, Ytext+W1 ) ;
    Pointer[2] := Point( Xctr+W1, Ytext+W1+H1-2 ) ;
    Pointer[3] := Point( Xctr+W2, Ytext+W1+H1-2 ) ;
    Pointer[4] := Point( Xctr+W2, Ytext+W1+H1+H2+4 ) ;
    Pointer[5] := Point( Xctr-W2, Ytext+W1+H1+H2+4 ) ;
    Pointer[6] := Point( Xctr-W2, Ytext+W1+H1-2 ) ;
    Pointer[7] := Point( Xctr-W1, Ytext+W1+H1-2 ) ;
    Pointer[8] := Point( Xctr-W1, Ytext+W1 ) ;
    Polygon( Pointer ) ;

    Font.Size := 12 ;
    Brush.Color := fBackColor ;
    Dstr := IntToStr(Heading) ;
    while length(Dstr) < 3 do Dstr := '0'+Dstr ;
    Xtext := Xctr - (TextWidth(Dstr) DIV 2) ;
    TextOut( Xtext , Ytext+W1+H1+1, Dstr ) ;
    Font.Size := 8 ;
    FontColor := Font.Color ;
    Font.Color := fCardinalColor ;
    TextOut( Xctr - (TextWidth(HdgMode) DIV 2),Ytext+W1,HdgMode) ;
    Font.Color := FontColor ;
    Pen.Width := 1 ;
    Font.Size := FontSize ;
  end ;
end ;

procedure DrawCourseReference ;
const
  Xa = 10 ;
  Ya = 10 ;

var
  Pointer : array [0..2] of TPoint ;
  Ytext   : integer ;
begin
  with AnImage.Canvas do
  begin
    Ytext := Yctr+1 ;
    Brush.Color := fPointerColor ;
    Pen.Color := fPointerColor ;
    Pen.Width := 1 ;
    Pointer[0] := Point( Xctr,    Ytext ) ;
    Pointer[1] := Point( Xctr+Xa, Ytext+Ya ) ;
    Pointer[2] := Point( Xctr-Xa, Ytext+Ya ) ;
    Polygon( Pointer ) ;
  end ;
end ;

{ Procedure DrawCompass }

var
  HDGStep  : integer ;
  TempHDG  : integer ;
  TempStr  : string ;
  FontSize : integer ;

begin

{ Preset the integer heading value }

  if fShowMagnetic
  then begin
    if fMagnetic
    then rHeading := fBearing
    else rHeading := fBearing + fMagVar ;
  end
  else begin
    if fMagnetic
    then rHeading := fBearing - fMagVar
    else rHeading := fBearing ;
  end ;
  while rHeading >= 360.0 do rHeading := rHeading - 360.0 ;
  while rHeading < 0.0 do rHeading := rHeading + 360.0 ;
  Heading := ROUND(rHeading) ;

{ Clear the image area }

  AnImage.Canvas.Font := Font ;
  with AnImage.Canvas do
  begin
    FontSize := Font.Size ;
    Brush.Color := fBackColor ;
    FillRect( PaintRect ) ;

{ Set up the basic size }

    Font.Size := 12 ;
    Wide := PaintRect.Right - PaintRect.Left ;
    Xctr := (Wide DIV 2) ;
    Yctr := PaintRect.Bottom - PaintRect.Top;
    if fShowBearing
    then Yctr := (Yctr DIV 2) - 10
    else Yctr := (Yctr DIV 2) + 5 ;
    if FBorderStyle = bsSingle
    then begin
      Inc(Xctr);
      Inc(Yctr);
    end ;

    Pen.Color := fScaleColor ;
    Pen.Width := 2 ;
    MoveTo( 0,Yctr ) ;
    LineTo( Wide , Yctr ) ;
    Pen.Width := 1 ;
    Font.Size := FontSize ;
  end ;

{ Insert the scale }

  Vpos := ROUND(rHeading*2)-Xctr ;
  for HDGStep := -8 to 24 do
  begin
    TempHDG := (HDGStep*30+360) MOD 360 ;
    case TempHDG of
        0: TempStr := 'N' ;
       90: TempStr := 'E' ;
      180: TempStr := 'S' ;
      270: TempStr := 'W' ;
      else begin
        if fFullBearing
          then TempStr := IntToStr(TempHDG)
          else TempStr := IntToStr( TempHDG DIV 10 ) ;
      end ;
    end ;
    DrawSteps(HDGStep*60-Vpos , Yctr , TempStr ) ;
  end ;

{ Draw the precise heading Pointer }

  if fShowBearing then DrawHeadingPointer
  else if fShowCourse then DrawCourseReference ;

{ Draw the precise course Pointer }

  if fShowCourse then DrawCourse ;
end ;

{ -------------------------------------------------------------------- }

procedure ThhAvComp.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> fBorderStyle then
  begin
    fBorderStyle := Value;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvComp.SetScaleColor(Colour : TColor);
begin
  if Colour <> fScaleColor then
  begin
    fScaleColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvComp.SetBackColor(Colour : TColor);
begin
  if Colour <> fBackColor then
  begin
    fBackColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvComp.SetPointerColor(Colour : TColor);
begin
  if Colour <> fPointerColor then
  begin
    fPointerColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvComp.SetCardinalColor(Colour : TColor);
begin
  if Colour <> fCardinalColor then
  begin
    fCardinalColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvComp.SetCourseColor(Colour : TColor);
begin
  if Colour <> fCourseColor then
  begin
    fCourseColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvComp.SetBearing(Value : Real);
var
  TempValue: Real;
begin
  TempValue := fBearing ;
  if fBearing <> Value then
  begin
    fBearing := Value;
    while fBearing > 360.0 do fBearing := fBearing - 360.0 ;
    while fBearing <   0.0 do fBearing := fBearing + 360.0 ;
    if TempValue <> Value then { only refresh if changed }
      Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvComp.SetCourse(Value : Real);
var
  TempValue: Real;
begin
  TempValue := fCourse ;
  if fCourse <> Value then
  begin
    fCourse := Value;
    while fCourse > 360.0 do fCourse := fCourse - 360.0 ;
    while fCourse <   0.0 do fCourse := fCourse + 360.0 ;
    if TempValue <> Value then { only refresh if changed }
      Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvComp.AddBearing(Value: Real);
begin
  SetBearing( fBearing + Value ) ;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvComp.AddCourse(Value: Real);
begin
  SetCourse( fCourse + Value ) ;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvComp.SetMagVar(Value : Real);
var
  TempValue: Real;
begin
  TempValue := fMagVar ;
  if fMagVar <> Value then
  begin
    fMagVar := Value;
    while fMagVar > 360.0 do fMagVar := fMagVar - 360.0 ;
    while fMagVar <   0.0 do fMagVar := fMagVar + 360.0 ;
    if TempValue <> Value then { only refresh if changed }
      Refresh;
  end;
end ;

{ -------------------------------------------------------------------- }

procedure ThhAvComp.SetMagnetic(Mode : boolean);
begin
  if Mode <> fMagnetic then
  begin
    fMagnetic := Mode;
    Refresh;
  end;
end ;

{ -------------------------------------------------------------------- }

procedure ThhAvComp.SetShowBearing(Mode : boolean);
begin
  if Mode <> fShowBearing then
  begin
    fShowBearing := Mode;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvComp.SetShowCourse(Mode : boolean);
begin
  if Mode <> fShowCourse then
  begin
    fShowCourse := Mode;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvComp.SetShowMagnetic(Mode : boolean);
begin
  if Mode <> fShowMagnetic then
  begin
    fShowMagnetic := Mode;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvComp.SetFullBearing(Mode : boolean);
begin
  if Mode <> fFullBearing then
  begin
    fFullBearing := Mode;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvComp.SetCoursePointer(Value : TpsCoursePointer);
begin
  if Value <> fCoursePointer then
  begin
    fCoursePointer := Value;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure Register ;
begin
  RegisterComponents( 'Howie' , [ThhAvComp] ) ;
end ;

end.
