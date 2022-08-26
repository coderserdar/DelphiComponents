unit hhAvASI;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls;

type
  tPointerKind = (pkLeft,pkRight) ;

  ThhAvASI = class(TGraphicControl)
  private
    fMinSpeed    : Longint;
    fMaxSpeed    : Longint;
    fCurSpeed    : Longint;
    fBorderStyle : TBorderStyle;
    fScaleColor  : TColor;
    fBackColor   : TColor;
    fPointerColor: TColor;
    fPointerKind : tPointerKind;
    procedure PaintBackground(AnImage: TBitmap);
    procedure PaintASIimage(AnImage: TBitmap; PaintRect: TRect);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetScaleColor(Colour : TColor);
    procedure SetBackColor(Colour : TColor);
    procedure SetPointerColor(Colour : TColor);
    procedure SetPointerKind(Value : TPointerKind);
    procedure SetMinSpeed(Speed : Longint);
    procedure SetMaxSpeed(Speed : Longint);
    procedure SetSpeed(Speed : Longint);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddSpeed(AddSpeed : Longint);
  published
    property Align;
    property Anchors;
    property BackColor: TColor read fBackColor write SetBackColor default clBlack;
    property BorderStyle: TBorderStyle read fBorderStyle write SetBorderStyle default bsNone;
    property PointerColor: TColor read fPointerColor write SetPointerColor default clWhite;
    property PointerKind: TPointerKind read fPointerKind write SetPointerKind default pkLeft;
    property Constraints;
    property Enabled;
    property Font;
    property MinSpeed: Longint read fMinSpeed write SetMinSpeed default 0;
    property MaxSpeed: Longint read fMaxSpeed write SetMaxSpeed default 1999;
    property ScaleColor: TColor read fScaleColor write SetScaleColor default clLime;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Speed: Longint read fCurSpeed write SetSpeed;
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
{ ThhAvASI }
{ -------------------------------------------------------------------- }

constructor ThhAvASI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csFramed, csOpaque];
  { default values }
  fMinSpeed := 0;
  fMaxSpeed := 1999;
  fCurSpeed := 0;
  fBorderStyle := bsNone;
  fBackColor   := clBlack;
  fPointerColor    := clWhite;
  fPointerKind     := pkLeft ;
  fScaleColor  := clLime;
  Font.Color   := clWhite ;
  Font.Name    := 'Arial' ;
  Width  := 65;
  Height := 201;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvASI.Paint;
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
        PaintASIimage(OverlayImage, PaintRect);
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

procedure ThhAvASI.PaintBackground(AnImage: TBitmap);
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

procedure ThhAvASI.PaintASIimage(AnImage: TBitmap; PaintRect: TRect);
var
  Xctr : integer ;
  Vpos : integer ;

procedure DrawHorizontalMark( xval,yval,LenVal : integer ; Mark : string ) ;
//var
//  Npoint : TPoint ;
begin
  with AnImage.Canvas do
  begin
    Font.Size := 10 ;
//    Npoint.X := xval ;
//    Npoint.y := yval ;
    Pen.Color := fScaleColor ;
//    PenPos := Npoint ;
    MoveTo(xval,yval) ;
    LineTo( xval-LenVal , yval) ;
    Brush.Style := bsClear ;
    TextOut( Xval -TextWidth(Mark+' ') - LenVal,yval+(Font.Height DIV 2),Mark) ;
  end ;
end ;

procedure DrawSteps( xval,yval : integer ; Astr : string ) ;
begin
  if (yval < -30) or (yval > AnImage.Height) then exit ;
  DrawHorizontalMark( xval , yval ,   10 , Astr ) ;
  DrawHorizontalMark( xval , yval+20, 10 , '' ) ;
end ;

procedure DrawSpeedMarker( YPos : integer ) ;
const
//  Xa = 10 ;
  Xb = 10 ;
//  Ya = 20 ;

var
  Astr  : string ;
  Xtext : integer ;
  Pointer : array[0..4] of TPoint ;
  Yb : integer ;

begin
  with AnImage.Canvas do
  begin
//    Font.Style := [fsBold] ;
    Font.Size := 12 ;
    Yb := TextHeight('X') DIV 2 + 2;
    Brush.Color := fBackColor ;
    if fPointerKind = pkLeft
    then begin
      Brush.Color := fBackColor ;
      Astr := IntToStr(fCurSpeed) ;
      Xtext := TextWidth('0000') ;
//      Area := Rect(Xctr-Xtext-Xb,YPos-Yb,Xctr - Xa,YPos+Yb) ;
      Pen.Width := 2 ;
      Pen.Color := fPointerColor ;
      Pointer[0] := Point( Xctr-2, YPos ) ;
      Pointer[1] := Point( Xctr-Xb,YPos+Yb ) ;
      Pointer[2] := Point( Xctr-Xb-Xtext,YPos+Yb) ;
      Pointer[3] := Point( Xctr-Xb-Xtext,YPos-Yb) ;
      Pointer[4] := Point( Xctr-Xb, YPos-Yb ) ;
      Polygon( Pointer ) ;
      Pen.Width := 1 ;
//      TextRect( Area, Xctr - Xb - TextWidth(Astr) , YPos + (Font.Height DIV 2)-1, Astr ) ;
      TextOut( Xctr - Xb - TextWidth(Astr) , YPos + (Font.Height DIV 2)-1, Astr ) ;
    end
    else begin
      Brush.Color := fBackColor ;
      Astr := IntToStr(fCurSpeed) ;
      Xtext := TextWidth('0000') ;
      Pen.Width := 2 ;
      Pen.Color := fPointerColor ;
(*      MoveTo( Xctr+Xb,YPos-Yb) ;
      LineTo( Xctr+2, YPos ) ;
      LineTo( Xctr+Xb,YPos+Yb ) ;
      LineTo( Xctr+Xb+Xtext,YPos+Yb) ;
      LineTo( Xctr+Xb+Xtext,YPos-Yb) ;
      LineTo( Xctr+Xb,YPos-Yb) ;
*)
      Pointer[0] := Point( Xctr+2, YPos ) ;
      Pointer[1] := Point( Xctr+Xb,YPos+Yb ) ;
      Pointer[2] := Point( Xctr+Xb+Xtext,YPos+Yb) ;
      Pointer[3] := Point( Xctr+Xb+Xtext,YPos-Yb) ;
      Pointer[4] := Point( Xctr+Xb, YPos-Yb ) ;
      Polygon( Pointer ) ;
      Pen.Width := 1 ;
//      Area := Rect(Xctr+Xa,YPos-Yb,Xctr+Xa+Xtext+8,YPos+Yb) ;
//      TextRect( Area, Area.Right - TextWidth(Astr)-10 , YPos + (Font.Height DIV 2)-1, Astr ) ;
      TextOut( Xctr+Xb+Xtext - TextWidth(Astr)-2 , YPos + (Font.Height DIV 2)-1, Astr ) ;
    end ;
//    Font.Style := [] ;
  end ;
end ;

{ Procedure DrawSpeed }

const
  Xb = 10 ;

var
  SpeedStep : integer ;
  TempSpeed : integer ;
  TempStr   : string ;
  PtrLoc    : integer ;

begin

{ Clear the image area }

  AnImage.Canvas.Font := Font ;

{ Set up the basic size }

  with AnImage.Canvas do
  begin
    if fPointerKind = pkLeft
    then begin
//      Font.Style := [fsBold] ;
      Font.Size  := 12 ;
      Xctr := TextWidth( '00000' ) + Xb  ;
    end
    else begin
//      Font.Style := [] ;
      Font.Size  := 10 ;
      Xctr := TextWidth( '00000' ) + Xb  ;
    end ;
    if FBorderStyle = bsSingle
    then begin
      Inc(Xctr);
    end ;
    Brush.Color := fBackColor ;
    FillRect( PaintRect ) ;
    MoveTo( Xctr , 0 ) ;
    Pen.Width := 2 ;
    Pen.Color := fScaleColor ;
    LineTo( Xctr , AnImage.Height ) ;
    Pen.Width := 1 ;

{ Insert the scale }

    Vpos := AnImage.Height ;
    if fCurSpeed < Vpos DIV 4
    then begin
      PtrLoc := fCurSpeed * 2 ;
      Vpos := AnImage.Height ;
    end
    else begin
      PtrLoc := AnImage.Height DIV 2 ;
      Vpos := PtrLoc + fCurSpeed*2 ;
    end ;

{ Insert the speed scale }

    for SpeedStep := 0 to (fMaxSpeed DIV 20)+20 do
    begin
      TempSpeed := SpeedStep*20 ;
      TempStr := IntToStr( TempSpeed ) ;
      DrawSteps(Xctr , Vpos - SpeedStep*40 , TempStr ) ;
    end ;

{ Draw the precise speed Pointer }

    DrawSpeedMarker( AnImage.Height - PtrLoc ) ;
  end ;
end ;

{ -------------------------------------------------------------------- }

procedure ThhAvASI.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> fBorderStyle then
  begin
    fBorderStyle := Value;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvASI.SetScaleColor(Colour : TColor);
begin
  if Colour <> fScaleColor then
  begin
    fScaleColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvASI.SetBackColor(Colour : TColor);
begin
  if Colour <> fBackColor then
  begin
    fBackColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvASI.SetPointerColor(Colour : TColor);
begin
  if Colour <> fPointerColor then
  begin
    fPointerColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvASI.SetMinSpeed(Speed : Longint);
begin
  if Speed <> fMinSpeed then
  begin
    if Speed > fMaxSpeed then
      if not (csLoading in ComponentState) then
        raise EInvalidOperation.CreateFmt(SOutOfRange, [-MaxInt, fMaxSpeed - 1]);
    if Speed < 0 then Speed := 0 ;
    fMinSpeed := Speed;
    if fCurSpeed < Speed then fCurSpeed := Speed;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvASI.SetMaxSpeed(Speed : Longint);
begin
  if Speed <> fMaxSpeed then
  begin
    if Speed < fMinSpeed then
      if not (csLoading in ComponentState) then
        raise EInvalidOperation.CreateFmt(SOutOfRange, [fMinSpeed + 1, MaxInt]);
    fMaxSpeed := Speed;
    if fCurSpeed > Speed then fCurSpeed := Speed;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvASI.SetSpeed(Speed : Longint);
var
  TempValue: Longint;
begin
  TempValue := fCurSpeed ;
  if Speed < fMinSpeed then Speed := fMinSpeed
    else if Speed > fMaxSpeed then Speed := fMaxSpeed;
  if fCurSpeed <> Speed then
  begin
    fCurSpeed := Speed;
    if TempValue <> Speed then { only refresh if changed }
      Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvASI.AddSpeed(AddSpeed: Longint);
begin
  SetSpeed( fCurSpeed + AddSpeed ) ;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvASI.SetPointerKind(Value: tPointerKind);
begin
  if Value <> FPointerKind then
  begin
    FPointerKind := Value;
    Refresh;
  end;
end;


{ -------------------------------------------------------------------- }

procedure Register ;
begin
  RegisterComponents( 'Howie' , [ThhAvASI] ) ;
end ;

end.
