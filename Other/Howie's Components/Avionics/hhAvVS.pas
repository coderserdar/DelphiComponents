unit hhAvVS;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls;

type
  TpointerKind = (pkBoth,pkLeft,pkRight) ;

  ThhAvVS = class(TGraphicControl)
  private
    fMaxSpeed    : Longint;
    fVSpeed      : Longint;
    fBorderStyle : TBorderStyle;
    fScaleColor  : TColor;
    fBackColor   : TColor;
    fPointerColor: TColor;
    fAlertColor  : TColor;
    fAlertSpeed  : Longint;
    fSolidPointer: boolean ;
    fPointerKind : TpointerKind ;
    procedure PaintBackground(AnImage: TBitmap);
    procedure PaintVSimage(AnImage: TBitmap; PaintRect: TRect);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetScaleColor(Colour : TColor);
    procedure SetAlertColor(Colour : TColor);
    procedure SetBackColor(Colour : TColor);
    procedure SetPointerColor(Colour : TColor);
    procedure SetMaxSpeed(Value : Longint);
    procedure SetVSpeed(Value : Longint);
    procedure SetAlertSpeed(Value : Longint);
    procedure SetPointerKind(Value : TpointerKind);
    procedure SetSolidPointer(Value : Boolean);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddSpeed(Value : Longint);
  published
    property Align;
    property Anchors;
    property BackColor: TColor read fBackColor write SetBackColor default clBlack;
    property BorderStyle: TBorderStyle read fBorderStyle write SetBorderStyle default bsNone;
    property AlertColor: TColor read fAlertColor write SetAlertColor default clRed;
    property PointerColor: TColor read fPointerColor write SetPointerColor default clWhite;
    property Constraints;
    property Enabled;
    property Font;
    property MaxSpeed: Longint read fMaxSpeed write SetMaxSpeed default 4000;
    property AlertSpeed: Longint read fAlertSpeed write SetAlertSpeed default 2000;
    property ScaleColor: TColor read fScaleColor write SetScaleColor default clLime;
    property SolidPointer: Boolean read fSolidPointer write SetSolidPointer default false;
    property PointerKind: TpointerKind read fPointerKind write SetPointerKind default pkBoth;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Speed: Longint read fVSpeed write SetVSpeed;
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
{ ThhAvVS }
{ -------------------------------------------------------------------- }

constructor ThhAvVS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csFramed, csOpaque];
  { default values }
  fVSpeed := 0;
  fMaxSpeed := 4000;
  fAlertSpeed  := 2000;
  fBorderStyle := bsNone;
  fAlertColor  := clRed;
  fBackColor   := clBlack;
  fPointerColor:= clWhite;
  fScaleColor  := clLime;
  fPointerKind := pkBoth ;
  Font.Color   := clWhite ;
  fSolidPointer := false ;
  Font.Name    := 'Arial' ;
  Width  := 60;
  Height := 201;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvVS.Paint;
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
        PaintVSimage(OverlayImage, PaintRect);
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

procedure ThhAvVS.PaintBackground(AnImage: TBitmap);
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

procedure ThhAvVS.PaintVSimage(AnImage: TBitmap; PaintRect: TRect);
const
  Xa = 10 ;
  Ya = 7 ;
var
  Xctr  : integer ;
  Yctr  : integer ;

procedure DrawHorizontalMark( xval,yval,LenVal : integer ; Mark : string ) ;
var
  Npoint : TPoint ;
begin
  with AnImage.Canvas do
  begin
    Font.Size := 10 ;
    Npoint.X := xval ;
    Npoint.y := yval ;
    Pen.Color := fScaleColor ;
    PenPos := Npoint ;
    LineTo( xval+LenVal , yval) ;
    Brush.Style := bsClear ;
    TextOut( PenPos.X+5,PenPos.Y+(Font.Height DIV 2),Mark) ;
  end ;
end ;

procedure DrawSteps( xval,yval : integer ; Astr : string ) ;
begin
  if (yval < -50) or (yval > AnImage.Height) then exit ;
  DrawHorizontalMark( xval , yval ,   10 , Astr ) ;
  DrawHorizontalMark( xval , yval+5, 5 , '' ) ;
  DrawHorizontalMark( xval , yval+10, 5 , '' ) ;
  DrawHorizontalMark( xval , yval+15, 5 , '' ) ;
  DrawHorizontalMark( xval , yval+20, 5 , '' ) ;
end ;

var
  VSPsign : integer ;
  Nspeed  : integer ;
  Pointer : array[0..3] of TPoint ;

begin

{ Clear the image area }

  AnImage.Canvas.Font := Font ;
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
    Xctr := Xctr - Xa + 2 ;
    Pen.Width := 2 ;
    Pen.Color := fScaleColor ;
    MoveTo( Xctr , 0 ) ;
    LineTo( Xctr , Yctr * 2) ;
    Pen.Width := 1 ;
    Pen.Color := fScaleColor ;
    Font.Size := 8 ;
    DrawSteps( Xctr,Yctr+150,'-8') ;
    DrawSteps( Xctr,Yctr+125,'-6') ;
    DrawSteps( Xctr,Yctr+100,'-4') ;
    DrawSteps( Xctr,Yctr+75, '-2') ;
    DrawSteps( Xctr,Yctr+50, '-1') ;
    DrawSteps( Xctr,Yctr+25, ''  ) ;
    DrawSteps( Xctr,Yctr,    '0' ) ;
    DrawSteps( Xctr,Yctr-25, ''  ) ;
    DrawSteps( Xctr,Yctr-50, '+1') ;
    DrawSteps( Xctr,Yctr-75, '+2') ;
    DrawSteps( Xctr,Yctr-100,'+4') ;
    DrawSteps( Xctr,Yctr-125,'+6') ;
    DrawSteps( Xctr,Yctr-150,'+8') ;

    Nspeed := fVspeed ;
    if Nspeed < 0 then VSPsign := -1 else VSPsign := 1 ;
    Nspeed := ABS(Nspeed) ;
    if (Nspeed > fAlertSpeed)
    then Pen.Color := fAlertColor
    else Pen.Color := fPointerColor ;
    Pen.Width := 2 ;
    Brush.Color := Pen.Color ;
    if fSolidPointer
      then Brush.Style := bsSolid
      else Brush.Style := bsClear ;

    if Nspeed <= 1000 then Nspeed := Nspeed DIV 20
    else if Nspeed <= 2000 then Nspeed := Nspeed DIV 40 + 25
    else Nspeed := Nspeed DIV 80 + 50 ;
    Nspeed := Nspeed * VSPsign ;
    case PointerKind of
      pkLeft: begin
          Pointer[0] := Point( Xctr   ,Yctr - Nspeed) ;
          Pointer[1] := Point( Xctr-Xa,Yctr - Nspeed + Ya) ;
          Pointer[2] := Point( Xctr-Xa,Yctr - Nspeed - Ya) ;
          Pointer[3] := Point( Xctr   ,Yctr - Nspeed) ;
        end ;
      pkRight: begin
          Pointer[0] := Point( Xctr   ,Yctr - Nspeed) ;
          Pointer[1] := Point( Xctr+Xa,Yctr - Nspeed + Ya) ;
          Pointer[2] := Point( Xctr+Xa,Yctr - Nspeed - Ya) ;
          Pointer[3] := Point( Xctr   ,Yctr - Nspeed) ;
        end ;
      else
        begin
          Pointer[0] := Point( Xctr-Xa,Yctr - Nspeed + Ya) ;
          Pointer[1] := Point( Xctr+Xa,Yctr - Nspeed - Ya) ;
          Pointer[2] := Point( Xctr+Xa,Yctr - Nspeed + Ya) ;
          Pointer[3] := Point( Xctr-Xa,Yctr - Nspeed - Ya) ;
        end ;
    end ;
    Polygon( Pointer ) ;
    Pen.Color := fPointerColor ;
    Pen.Width := 1 ;
  end ;
end ;

{ -------------------------------------------------------------------- }

procedure ThhAvVS.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> fBorderStyle then
  begin
    fBorderStyle := Value;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvVS.SetScaleColor(Colour : TColor);
begin
  if Colour <> fScaleColor then
  begin
    fScaleColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvVS.SetBackColor(Colour : TColor);
begin
  if Colour <> fBackColor then
  begin
    fBackColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvVS.SetPointerColor(Colour : TColor);
begin
  if Colour <> fPointerColor then
  begin
    fPointerColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvVS.SetMaxSpeed(Value : Longint);
begin
  if Value <> fMaxSpeed then
  begin
    if Value <= 0 then
      if not (csLoading in ComponentState) then
        raise EInvalidOperation.CreateFmt(SOutOfRange, [1, MaxInt]);
    fMaxSpeed := Value;
    if fVSpeed > Value then fVSpeed := Value;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvVS.SetVSpeed(Value : Longint);
var
  TempValue: Longint;
begin
  TempValue := fVSpeed ;
    if Value < -fMaxSpeed then Value := -fMaxSpeed
    else if Value > fMaxSpeed then Value := fMaxSpeed;
  if fVSpeed <> Value then
  begin
    fVSpeed := Value;
    if TempValue <> Value then { only refresh if changed }
      Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvVS.AddSpeed(Value: Longint);
begin
  SetVSpeed( fVSpeed + Value ) ;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvVS.SetPointerKind(Value : TpointerKind);
begin
  if Value <> fPointerKind then
  begin
    fPointerKind := Value;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvVS.SetSolidPointer(Value : Boolean);
begin
  if Value <> fSolidPointer then
  begin
    fSolidPointer := Value;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvVS.SetAlertColor(Colour : TColor);
begin
  if Colour <> fAlertColor then
  begin
    fAlertColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvVS.SetAlertSpeed(Value : Longint);
var
  TempValue: Longint;
begin
  TempValue := fAlertSpeed ;
  if Value < -fMaxSpeed then Value := -fMaxSpeed ;
  if Value >  fMaxSpeed then Value :=  fMaxSpeed ;
  if fAlertSpeed <> Value then
  begin
    fAlertSpeed := Value;
    if TempValue <> Value then { only refresh if changed }
      Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure Register ;
begin
  RegisterComponents( 'Howie' , [ThhAvVS] ) ;
end ;

end.

