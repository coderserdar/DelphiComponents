unit hhAvALT;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls;

type
  ThhAvALT = class(TGraphicControl)
  private
    fMinAlt      : Longint;
    fMaxAlt      : Longint;
    fCurAlt      : Longint;
    fBorderStyle : TBorderStyle;
    fScaleColor  : TColor;
    fBackColor   : TColor;
    fPointerColor: TColor;
    fShutter     : boolean ;
    procedure PaintBackground(AnImage: TBitmap);
    procedure PaintAltImage(AnImage: TBitmap; PaintRect: TRect);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetScaleColor(Colour : TColor);
    procedure SetBackColor(Colour : TColor);
    procedure SetPointerColor(Colour : TColor);
    procedure SetMinAlt(Alt : Longint);
    procedure SetMaxAlt(Alt : Longint);
    procedure SetAltitude(Alt : Longint);
    procedure SetShutter(Mode : boolean);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddAltitude(AddAlt : Longint);
  published
    property Align;
    property Anchors;
    property BackColor: TColor read fBackColor write SetBackColor default clBlack;
    property BorderStyle: TBorderStyle read fBorderStyle write SetBorderStyle default bsNone;
    property PointerColor: TColor read fPointerColor write SetPointerColor default clWhite;
    property Constraints;
    property Enabled;
    property Font;
    property MinAlt: Longint read fMinAlt write SetMinAlt default 0;
    property MaxAlt: Longint read fMaxAlt write SetMaxAlt default 99999;
    property ScaleColor: TColor read fScaleColor write SetScaleColor default clLime;
    property Shutter: Boolean read fShutter write SetShutter default true;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Altitude: Longint read fCurAlt write SetAltitude;
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
{ ThhAvALT }
{ -------------------------------------------------------------------- }

constructor ThhAvALT.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csFramed, csOpaque];
  { default values }
  fMinAlt := 0;
  fMaxAlt := 99999;
  fCurAlt := 0;
  fBorderStyle := bsNone;
  fScaleColor  := clLime;
  fBackColor   := clBlack;
  fPointerColor:= clWhite;
  fShutter     := true ;
  Font.Color   := clWhite ;
  Font.Name    := 'Arial' ;
  Width  := 105;
  Height := 201;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvALT.Paint;
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
        PaintAltImage(OverlayImage, PaintRect);
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

procedure ThhAvALT.PaintBackground(AnImage: TBitmap);
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

procedure ThhAvALT.PaintAltImage(AnImage: TBitmap; PaintRect: TRect);

var
  Vpos : integer ;
  Area : TRect ;
  Xctr : Integer ;
  Yctr : Integer;

procedure DrawHorizontalMark( xval,yval,LenVal : integer ; Mark : string ) ;
begin
  with AnImage.Canvas do
  begin
    Font.Size := 10 ;
    Pen.Color := fScaleColor ;
    MoveTo(xval,yval) ;
    LineTo( xval+LenVal , yval) ;
    Brush.Style := bsClear ;
    TextOut( xval+LenVal+5,yval+(Font.Height DIV 2),Mark) ;
  end ;
end ;

procedure DrawSteps( xval,yval : integer ; Astr : string ) ;
begin
  if (yval < -50) or (yval > AnImage.Height) then exit ;
  DrawHorizontalMark( xval , yval ,   20 , Astr ) ;
  DrawHorizontalMark( xval , yval+10, 10 , '' ) ;
  DrawHorizontalMark( xval , yval+20, 10 , '' ) ;
  DrawHorizontalMark( xval , yval+30, 10 , '' ) ;
  DrawHorizontalMark( xval , yval+40, 10 , '' ) ;
end ;

procedure DrawAltPointer ;
const
  Xa = 10 ;
  Ya = 20 ;
  Yb = 10 ;
var
  XL,XR : integer ;
  Xtext : integer ;
  Astr  : string ;
  Bstr  : string ;
  OPointer  : array[0..7] of Tpoint ;

begin
  with AnImage.Canvas do
  begin
{ Draw the Altitude Pointer outline }
//    Font.Style := [fsBold] ;
    Font.Size := 12 ;
    XR := TextWidth('0000')+5 ;
    Font.Size := 16 ;
    XL := TextWidth('000')+5 ;
    Pen.Width := 2 ;
    Pen.Color := fPointerColor ;
    Brush.Color := clBlack ;
    Brush.Style := bsClear ;
    OPointer[0].X := Xctr-XL ;
    OPointer[0].Y := Yctr-Ya ;
    OPointer[1].X := Xctr ;
    OPointer[1].Y := Yctr-Ya ;
    OPointer[2].X := Xctr+Xa ;
    OPointer[2].Y := Yctr-Yb ;
    OPointer[3].X := Xctr+XR+Xa ;
    OPointer[3].Y := Yctr-Yb ;
    OPointer[4].X := Xctr+XR+Xa ;
    OPointer[4].Y := Yctr+Yb ;
    OPointer[5].X := Xctr+Xa ;
    OPointer[5].Y := Yctr+Yb ;
    OPointer[6].X := Xctr ;
    OPointer[6].Y := Yctr+Ya ;
    OPointer[7].X := Xctr-XL ;
    OPointer[7].Y := Yctr+Ya ;
    PolyGon( OPointer ) ;
    Brush.Style := bsSolid ;
    Pen.Width := 1 ;
{ Thousands of feet }
//    Font.Style := [fsBold] ;
    Font.Size := 16 ;
    Brush.Color := fBackColor ;
    Astr := IntToStr(Altitude DIV 1000) ;
    Bstr := IntToStr(Altitude MOD 1000) ;
    while length(Bstr) < 3 do Bstr := '0'+Bstr ;
    if fShutter AND (Altitude < 1000)
    then begin
{ Bring in the shutter }
      Brush.Color := (NOT clYellow) AND $FFFFFF ;
      Brush.Style := bsBdiagonal ;
      Area := Rect( OPointer[0].X+2,OPointer[0].Y+2,OPointer[6].X-2,OPointer[6].Y-2) ;
      FillRect( Area ) ;
      Brush.Style := bsSolid ;
      Brush.Color := fBackColor ;
    end
    else begin
{ Print the high range Altitude digits }
      Area := Rect(OPointer[0].X+1,OPointer[0].Y+1,OPointer[6].X-1,OPointer[6].Y-1) ;
      Brush.Style := bsSolid ;
      Xtext := Area.Right - TextWidth(Astr)  ;
      TextRect( Area, Xtext , Yctr + (Font.Height DIV 2)-1, Astr ) ;
    end ;
{ Print the low range Altitude digits }
//    Font.Style := [fsBold] ;
    Font.Size := 12 ;
    Area := Rect(OPointer[2].X+5,OPointer[2].Y+1,OPointer[4].X-1,OPointer[4].Y-1) ;
    Brush.Color := fBackColor ;
    Xtext := Area.Right - TextWidth( Bstr) ;
    Area.Left   := Xtext ;
    TextRect( Area, Xtext , Yctr + (Font.Height DIV 2)-1, Bstr ) ;
//    Font.Style := [] ;
  end ;
end ;

{ Procedure DrawALT }

var
  AltStep : integer ;
  TempAlt : integer ;
  TempStr : string ;

begin

{ Clear the image area }

  AnImage.Canvas.Font := Font ;
  with AnImage.Canvas do
  begin
    Brush.Color := fBackColor;
    FillRect(PaintRect);

{ Set up the basic size }

//    Font.Style := [fsBold] ;
    Font.Size := 16 ;
    Xctr := TextWidth('000')+10 ;
    Yctr := PaintRect.Bottom - PaintRect.Top;
    Yctr := Yctr DIV 2 ;
    if FBorderStyle = bsSingle then Inc(Yctr);
    Vpos := Yctr + (Altitude - ((Altitude + 500) DIV 1000)*1000) DIV 2 ;

{ Insert the scale }

    Pen.Color := fScaleColor ;
    Pen.Width := 2 ;
    MoveTo(Xctr,0) ;
    LineTo(Xctr,AnImage.Height) ;
    Pen.Width := 1 ;
    for AltStep := -6 to 20 do
    begin
      TempAlt := 2700 - AltStep*100 ;
      TempStr := IntToStr( TempAlt MOD 1000 ) ;
      if TempStr = '0' then TempStr := '000' ;
      DrawSteps(Xctr , AltStep*50 + vpos -350, TempStr ) ;
    end ;

{ Draw the precise altitude Pointer }

    DrawAltPointer ;
  end ;
end ;

{ -------------------------------------------------------------------- }

procedure ThhAvALT.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> fBorderStyle then
  begin
    fBorderStyle := Value;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvALT.SetScaleColor(Colour : TColor);
begin
  if Colour <> fScaleColor then
  begin
    fScaleColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvALT.SetShutter(Mode : boolean);
begin
  if Mode <> fShutter then
  begin
    fShutter := Mode;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvALT.SetBackColor(Colour : TColor);
begin
  if Colour <> fBackColor then
  begin
    fBackColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvALT.SetPointerColor(Colour : TColor);
begin
  if Colour <> fPointerColor then
  begin
    fPointerColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvALT.SetMinAlt(Alt : Longint);
begin
  if Alt <> FMinAlt then
  begin
    if Alt > FMaxAlt then
      if not (csLoading in ComponentState) then
        raise EInvalidOperation.CreateFmt(SOutOfRange, [-MaxInt, fMaxAlt - 1]);
    if Alt < 0 then Alt := 0 ;
    fMinAlt := Alt;
    if fCurAlt < Alt then fCurAlt := Alt;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvALT.SetMaxAlt(Alt : Longint);
begin
  if Alt <> fMaxAlt then
  begin
    if Alt < fMinAlt then
      if not (csLoading in ComponentState) then
        raise EInvalidOperation.CreateFmt(SOutOfRange, [fMinAlt + 1, MaxInt]);
    fMaxAlt := Alt;
    if fCurAlt > Alt then fCurAlt := Alt;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvALT.SetAltitude(Alt : Longint);
var
  TempValue: Longint;
begin
  TempValue := fCurAlt ;
  if Alt < fMinAlt then Alt := fMinAlt
    else if Alt > fMaxAlt then Alt := fMaxAlt;
  if fCurAlt <> Alt then
  begin
    fCurAlt := Alt;
    if TempValue <> Alt then { only refresh if changed }
      Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvALT.AddAltitude(AddAlt: Longint);
begin
  SetAltitude( fCurAlt + AddAlt ) ;
end;

{ -------------------------------------------------------------------- }

procedure Register ;
begin
  RegisterComponents( 'Howie' , [ThhAvALT] ) ;
end ;

end.

