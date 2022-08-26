unit hhAvGS;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls;

type
  TgsKind = (gsHorizontal,gsVertical,gsILS) ;
  TILSkind = (ILSorthogonal,ILSangular) ;
  ThhAvGS = class(TGraphicControl)
  private
    fAlertValue  : Longint;
    fSlope       : Longint;
    fSlopeHorz   : Longint;
    fSlopeVert   : Longint;
    fSolidPointer: Boolean ;
    fScaleColor  : TColor;
    fBackColor   : TColor;
    fPointerColor: TColor;
    fAlertColor  : TColor;
    fBorderStyle : TBorderStyle;
    fILSkind     : TILSkind;
    fgsKind      : TgsKind;
    procedure PaintBackground(AnImage: TBitmap);
    procedure PaintGSimage(AnImage: TBitmap; PaintRect: TRect);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetScaleColor(Colour : TColor);
    procedure SetBackColor(Colour : TColor);
    procedure SetPointerColor(Colour : TColor);
    procedure SetAlertColor(Colour : TColor);
    procedure SetGSkind(Value : TgsKind);
    procedure SetILSkind(Value : TILSkind);
    procedure SetSlope(Value : Longint);
    procedure SetSlopeHorz(Value : Longint);
    procedure SetSlopeVert(Value : Longint);
    procedure SetAlertValue(Value : Longint);
    procedure SetSolidPointer(Value : Boolean);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddSlope(Value : Longint);
    procedure AddSlopeHorz(Value : Longint);
    procedure AddSlopeVert(Value : Longint);
  published
    property Align;
    property Anchors;
    property BackColor: TColor read fBackColor write SetBackColor default clBlack;
    property BorderStyle: TBorderStyle read fBorderStyle write SetBorderStyle default bsNone;
    property PointerColor: TColor read fPointerColor write SetPointerColor default clAqua;
    property AlertColor: TColor read fAlertColor write SetAlertColor default clRed;
    property gsKind: TgsKind read fgsKind write SetGSkind default gsILS;
    property ILSkind: TILSkind read fILSkind write SetILSkind default ILSorthogonal;
    property Constraints;
    property Enabled;
    property ScaleColor: TColor read fScaleColor write SetScaleColor default clLime;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Slope: Longint read fSlope write SetSlope;
    property SlopeHorz: Longint read fSlopeHorz write SetSlopeHorz;
    property SlopeVert: Longint read fSlopeVert write SetSlopeVert;
    property AlertValue: Longint read fAlertValue write SetAlertValue;
    property SolidPointer: Boolean read fSolidPointer write SetSolidPointer default false;
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
{ ThhAvGS }
{ -------------------------------------------------------------------- }

constructor ThhAvGS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csFramed, csOpaque];
  { default values }
  fSlope       := 0;
  fSlopeVert   := 0;
  fSlopeHorz   := 0;
  fAlertValue  := 60;
  fILSkind     := ILSorthogonal ;
  fBorderStyle := bsNone;
  fBackColor   := clBlack;
  fPointerColor:= clAqua;
  fSolidPointer:= false ;
  fAlertColor  := clRed;
  fGSkind      := gsILS ;
  fScaleColor  := clLime;
  Width  := 200;
  Height := 200;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvGS.Paint;
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
        PaintGSimage(OverlayImage, PaintRect);
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

procedure ThhAvGS.PaintBackground(AnImage: TBitmap);
var
  ARect: TRect;
begin
  with AnImage.Canvas do
  begin
    CopyMode := cmBlackness;
    ARect := Rect(0, 0, Width, Height);
    CopyRect(ARect, AnImage.Canvas, ARect);
    CopyMode := cmSrcCopy;
  end;
end;


{ -------------------------------------------------------------------- }

procedure ThhAvGS.PaintGSimage(AnImage: TBitmap; PaintRect: TRect);
const
  Xa = 7 ;
  Xb = 4 ;
  Ya = 7 ;
  Yb = 4 ;
var
  Pointer  : array[0..3] of TPoint ;
  Xctr : integer ;
  Yctr : integer ;
  Marker : integer ;
  M30    : integer ;
  Radius : integer ;
  Xlim   : integer ;
  Ylim   : integer ;
  Angle  : real ;

begin

{ Clear the image area }
  with AnImage.Canvas do
  begin
    Brush.Color := fBackColor ;
    FillRect( PaintRect ) ;

    Xctr := AnImage.Width DIV 2 ;
    Yctr := AnImage.Height DIV 2 ;
    if FBorderStyle = bsSingle
    then begin
      Inc(Xctr);
      Inc(Yctr);
    end ;
    
    Pen.Color := fScaleColor ;

{ Create the gauge }

    case fGSkind of
      gsHorizontal:
      begin
        RoundRect( Xctr-Xb, Yctr-Ya, Xctr+Xb, Yctr+Ya, 2, 2) ;
        for Marker := 1 to 5 do
        begin
          M30 := Marker * 30 ;
          RoundRect( Xctr-Xb-M30,Yctr-Yb,Xctr+Xb-M30,Yctr+Yb,2,2) ;
          RoundRect( Xctr-Xb+M30,Yctr-Yb,Xctr+Xb+M30,Yctr+Yb,2,2) ;
         end ;
        if ABS(fSlope) >= Xctr
        then begin
          Pen.Color := clYellow ;
          Brush.Color := (NOT clYellow) AND $FFFFFF;
          Brush.Style := bsFDiagonal ;
          FillRect( Rect(0,Yctr-10,Xctr-(Xctr DIV 2),Yctr+10)) ;
          FillRect( Rect(Xctr*2,Yctr-10,Xctr+(Xctr DIV 2),Yctr+10)) ;
        end
        else begin
          if ABS(fSlope) < fAlertValue
          then Pen.Color := fPointerColor
          else Pen.Color := fAlertColor ;
          Pen.Width := 2 ;
          if fSolidPointer then Brush.Color := Pen.Color ;
          Pointer[0] := Point( Xctr + Xa,Yctr-fSlope) ;
          Pointer[0] := Point( Xctr + fSlope,Yctr-Ya) ;
          Pointer[1] := Point( Xctr + fSlope + Xb,Yctr) ;
          Pointer[2] := Point( Xctr + fSlope,Yctr+Ya) ;
          Pointer[3] := Point( Xctr + fSlope - Xb,Yctr) ;
          PolyGon( Pointer ) ;
          Pen.Color := fScaleColor ;
          Pen.Width := 1 ;
        end ;
      end ;
      gsVertical:
      begin
        RoundRect( Xctr-Xa, Yctr-Yb, Xctr+Xa, Yctr+Yb, 2,2) ;
        for Marker := 1 to 5 do
        begin
          M30 := Marker * 30 ;
          RoundRect( Xctr-Xb, Yctr-Yb-M30, Xctr+Xb, Yctr+Yb-M30, 2,2) ;
          RoundRect( Xctr-Xb, Yctr-Yb+M30, Xctr+Xb, Yctr+Yb+M30, 2,2) ;
         end ;
        if ABS(fSlope) >= Yctr
        then begin
          Pen.Color := clYellow ;
          Brush.Color := (NOT clYellow) AND $FFFFFF;
          Brush.Style := bsFDiagonal ;
          FillRect( Rect(Xctr-10,0,Xctr+10,Yctr-(Yctr DIV 2))) ;
          FillRect( Rect(Xctr-10,Yctr*2,Xctr+10,Yctr+(Yctr DIV 2))) ;
        end
        else begin
          if ABS(fSlope) < fAlertValue
          then Pen.Color := fPointerColor
          else Pen.Color := fAlertColor ;
          Pen.Width := 2 ;
          if fSolidPointer then Brush.Color := Pen.Color ;
          Pointer[0] := Point( Xctr + Xa,Yctr-fSlope) ;
          Pointer[1] := Point( Xctr,Yctr-fSlope+Yb) ;
          Pointer[2] := Point( Xctr - Xa,Yctr-fSlope) ;
          Pointer[3] := Point( Xctr ,Yctr-fSlope-Yb) ;
          PolyGon( Pointer ) ;
          Pen.Color := fScaleColor ;
          Pen.Width := 1 ;
        end ;
      end ;
      gsILS:
      begin
        Radius := Xctr ;
        if Radius > Yctr then Radius := Yctr ;
        Pen.Width := 1 ;
        Pen.Color := fScaleColor ;
        Ellipse(Xctr-Radius,Yctr-Radius,Xctr+Radius,Yctr+Radius) ;
        RoundRect( Xctr-Xb, Yctr-Yb, Xctr+Xb, Yctr+Yb, 2,2) ;
        for Marker := 1 to 5 do
        begin
          M30 := Marker * 30 ;
          RoundRect( Xctr-Xb, Yctr-Yb-M30, Xctr+Xb, Yctr+Yb-M30, 2,2) ;
          RoundRect( Xctr-Xb, Yctr-Yb+M30, Xctr+Xb, Yctr+Yb+M30, 2,2) ;
          RoundRect( Xctr-Xb-M30,Yctr-Yb,Xctr+Xb-M30,Yctr+Yb,2,2) ;
          RoundRect( Xctr-Xb+M30,Yctr-Yb,Xctr+Xb+M30,Yctr+Yb,2,2) ;
        end ;
        if ABS(fSlopeVert) >= Radius
        then begin
          Pen.Color := clYellow ;
          Brush.Color := (NOT clYellow) AND $FFFFFF;
          Brush.Style := bsFDiagonal ;
          FillRect( Rect(Xctr-10,Yctr-Radius,Xctr+10,Yctr-(Radius DIV 2))) ;
          FillRect( Rect(Xctr-10,Yctr+Radius,Xctr+10,Yctr+(Radius DIV 2))) ;
        end
        else begin
          if ABS(fSlopeVert) < fAlertValue
          then Pen.Color := fPointerColor
          else Pen.Color := fAlertColor ;
          Pen.Width := 2 ;
          case fILSkind of
            ILSorthogonal:
            begin
              Xlim := ROUND(SQRT(Radius*Radius - fSlopeVert*fSlopeVert)) ;
              MoveTo( Xctr-Xlim,Yctr-fSlopeVert) ;
              LineTo( Xctr+Xlim,Yctr-fSlopeVert) ;
            end ;
            ILSangular:
            begin
              Angle := ArcTan(fSlopeVert/Radius) ;
              Xlim :=  ROUND(Radius*Cos(2*Angle));
              Ylim :=  ROUND(Radius*Sin(2*Angle));
              MoveTo(0,Yctr) ;
              LineTo(Xctr+Xlim,Yctr-Ylim) ;
            end ;
          end ;
        end ;
        if ABS(fSlopeHorz) >= Radius
        then begin
          Pen.Color := clYellow ;
          Brush.Color := (NOT clYellow) AND $FFFFFF;
          Brush.Style := bsFDiagonal ;
          FillRect( Rect(Xctr-Radius,Yctr-10,Xctr-(Radius DIV 2),Yctr+10)) ;
          FillRect( Rect(Xctr+Radius,Yctr-10,Xctr+(Radius DIV 2),Yctr+10)) ;
        end
        else begin
          if ABS(fSlopeHorz) < fAlertValue
          then Pen.Color := fPointerColor
          else Pen.Color := fAlertColor ;
          Pen.Width := 2 ;
          case fILSkind of
            ILSorthogonal:
            begin
              Ylim := ROUND(SQRT(Radius*Radius - fSlopeHorz*fSlopeHorz)) ;
              MoveTo( Xctr+fSlopeHorz,Yctr-Ylim) ;
              LineTo( Xctr+fSlopeHorz,Yctr+Ylim) ;
            end ;
            ILSangular:
            begin
              Angle := ArcTan(fSlopeHorz/Radius) ;
              Xlim :=  ROUND(Radius*Sin(2*Angle));
              Ylim :=  ROUND(Radius*Cos(2*Angle));
              MoveTo(Xctr,Yctr+Radius) ;
              LineTo(Xctr+Xlim,Yctr-Ylim) ;
            end ;
          end ;
        end ;
      end ;
    end ;
  end ;
end ;

{ -------------------------------------------------------------------- }

procedure ThhAvGS.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> fBorderStyle then
  begin
    fBorderStyle := Value;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvGS.SetScaleColor(Colour : TColor);
begin
  if Colour <> fScaleColor then
  begin
    fScaleColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvGS.SetBackColor(Colour : TColor);
begin
  if Colour <> fBackColor then
  begin
    fBackColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvGS.SetPointerColor(Colour : TColor);
begin
  if Colour <> fPointerColor then
  begin
    fPointerColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvGS.SetAlertColor(Colour : TColor);
begin
  if Colour <> fAlertColor then
  begin
    fAlertColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvGS.SetAlertValue(Value : Longint);
var
  TempValue: Longint;
begin
  TempValue := fAlertValue ;
//  if Value < -100 then Value := -100 ;
//  if Value >  100 then Value :=  100 ;
  if fAlertValue <> Value then
  begin
    fAlertValue := Value;
    if TempValue <> Value then { only refresh if changed }
      Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvGS.SetSlope(Value : Longint);
var
  TempValue: Longint;
begin
  case fGSkind of
    gsHorizontal: TempValue := fSlopeHorz ;
    gsVertical:   TempValue := fSlopeVert ;
    else exit ;
  end ;
//  if Value < -100 then Value := -100 ;
//  if Value >  100 then Value :=  100 ;
  if TempValue <> Value then
  begin
    fSlope := Value;
    case fGSkind of
      gsHorizontal: fSlopeHorz := Value ;
      gsVertical:   fSlopeVert := Value ;
    end ;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvGS.AddSlope(Value: Longint);
begin
  case fGSkind of
    gsHorizontal: SetSlope( fSlopeHorz + Value ) ;
    gsVertical:   SetSlope( fSlopeVert + Value ) ;
  end ;

end;

{ -------------------------------------------------------------------- }

procedure ThhAvGS.SetSlopeHorz(Value : Longint);
var
  TempValue: Longint;
begin
  TempValue := fSlopeHorz ;
//  if Value < -100 then Value := -100 ;
//  if Value >  100 then Value :=  100 ;
  if TempValue <> Value then
  begin
    fSlopeHorz := Value;
    fSlope := Value ;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvGS.AddSlopeHorz(Value: Longint);
begin
  SetSlopeHorz( fSlopeHorz + Value ) ;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvGS.SetSlopeVert(Value : Longint);
var
  TempValue: Longint;
begin
  TempValue := fSlopeVert ;
//  if Value < -100 then Value := -100 ;
//  if Value >  100 then Value :=  100 ;
  if TempValue <> Value then
  begin
    fSlopeVert := Value;
    fSlope := Value ;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvGS.AddSlopeVert(Value: Longint);
begin
  SetSlopeVert( fSlopeVert + Value ) ;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvGS.SetGSkind(Value: TgsKind);
begin
  if Value <> fGSkind then
  begin
    fGSkind := Value;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvGS.SetILSkind(Value: TILSkind);
begin
  if Value <> fILSkind then
  begin
    fILSkind := Value;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvGS.SetSolidPointer(Value: Boolean);
begin
  if Value <> fSolidPointer then
  begin
    fSolidPointer := Value;
    Refresh;
  end;
end;


{ -------------------------------------------------------------------- }

procedure Register ;
begin
  RegisterComponents( 'Howie' , [ThhAvGS] ) ;
end ;

end.

