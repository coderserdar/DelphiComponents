unit hhAvADI;

{----
  Attitude Director Indicator (ADI) including:
    Selectable colours for:
        Sky colour
        Ground colour
        Pip colour
        Command colour
        Scale colour
    Roll
    Pitch
    Pip Markers
    Command Bars
----}

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls;

type
  TpkKind = (pkSquare,     pkDiamond,     pkCircle,     pkCross,
             pkSquareCross,pkDiamondCross,pkCircleCross,pkCrossCross,
             pkPending,    pkUnknown,     pkFriend,     pkNeutral,
             pkHostile,    pkAssumed,     pkSuspect,    pkJoker,
             pkFaker ) ;

  TCmdStyle = (cmdOff, cmdRunway, cmdBars, cmdBoth) ;

  ThhAvADI = class(TGraphicControl)
  private
    fMaxPitch    : Longint ;
    fPitch       : Longint ;
    fRoll        : Longint ;
    fBorderStyle : TBorderStyle;
    fBackColor   : TColor;
    fGroundColor : TColor;
    fScaleColor  : TColor;
    fSkyColor    : TColor;
    fCmdColor    : TColor;
    fBarColor    : TColor;
    fPipColor    : TColor;
    fCmdStyle    : TCmdStyle ;
    fCmdBold     : boolean ;
    fCmdPitch    : Longint ;
    fCmdRoll     : Longint ;
    fCmdYaw      : Longint ;
    fSolidCmdBar : boolean ;
    fShowPipMark : boolean ;
    fPipMarkX    : longint ;
    fPipMarkY    : longint ;
    fPipSize     : longint ;
    fPipBold     : boolean ;
    fPipKind     : TpkKind ;
    procedure PaintBackground(AnImage: TBitmap);
    procedure PaintAdiImage(AnImage: TBitmap; PaintRect: TRect);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetScaleColor(Colour : TColor);
    procedure SetBackColor(Colour : TColor);
    procedure SetGroundColor(Colour : TColor);
    procedure SetSkyColor(Colour : TColor);
    procedure SetCmdColor(Colour : TColor);
    procedure SetBarColor(Colour : TColor);
    procedure SetPipColor(Colour : TColor);
    procedure SetMaxPitch(Value : Longint);
    procedure SetPitch(Value : Longint);
    procedure SetRoll(Value : Longint);
    procedure SetCmdStyle(Value : TCmdStyle);
    procedure SetShowPipMark(Value : boolean);
    procedure SetPipBold(Value : boolean);
    procedure SetCmdBold(Value : boolean);
    procedure SetCmdPitch(Value : Longint ) ;
    procedure SetPipMarkX(Value : Longint ) ;
    procedure SetPipMarkY(Value : Longint ) ;
    procedure SetPipSize(Value : Longint ) ;
    procedure SetPipKind(Value : TpkKind ) ;
    procedure SetCmdRoll(Value : Longint ) ;
    procedure SetCmdYaw(Value : Longint ) ;
    procedure SetSolidCmdBar(Value : boolean ) ;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddPitch(Value : Longint);
    procedure AddRoll(Value : Longint);
    procedure AddCMDPitch(Value : Longint);
    procedure AddCMDRoll(Value : Longint);
    procedure AddCMDYaw(Value : Longint);
  published
    property Align;
    property Anchors;
    property BackColor: TColor read fBackColor write SetBackColor default clBlack;
    property BarColor: TColor read fBarColor write SetBarColor;
    property CmdColor: TColor read fCmdColor write SetCmdColor;
    property SkyColor: TColor read fSkyColor write SetSkyColor;
    property GroundColor: TColor read fGroundColor write SetGroundColor;
    property PipColor: TColor read fPipColor write SetPipColor;
    property BorderStyle: TBorderStyle read fBorderStyle write SetBorderStyle default bsNone;
    property Constraints;
    property Enabled;
    property Font;
    property MaxPitch: Longint read fMaxPitch write SetMaxPitch default 90;
    property ScaleColor: TColor read fScaleColor write SetScaleColor default clWhite;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Pitch: Longint read fPitch write SetPitch;
    property Roll: Longint read fRoll write SetRoll;
    property CmdPitch: Longint read fCmdPitch write SetCmdPitch default 0;
    property CmdRoll: Longint read fCmdRoll write SetCmdRoll default 0;
    property CmdYaw: Longint read fCmdYaw write SetCmdYaw default 0;
    property PipMarkX: Longint read fPipMarkX write SetPipMarkX default 0;
    property PipMarkY: Longint read fPipMarkY write SetPipMarkY default 0;
    property PipSize:  Longint read fPipSize write SetPipSize default 10;
    property PipBold: boolean read fPipBold write SetPipBold default false ;
    property CmdBold: boolean read fCmdBold write SetCmdBold default false ;
    property CmdStyle: TCmdStyle read fCmdStyle write SetCmdStyle default cmdOff ;
    property SolidCmdBar: boolean read fSolidCmdBar write SetSolidCmdBar default false ;
    property ShowPipMark: boolean read fShowPipMark write SetShowPipMark default false ;
    property PipKind: TpkKind read fPipKind write SetPipKind default pkSquare;
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
{ ThhAvADI }
{ -------------------------------------------------------------------- }

constructor ThhAvADI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csFramed, csOpaque];
  { default values }
  fMaxPitch    := 90 ;
  fPitch       := 0 ;
  fRoll        := 0 ;
  fCmdPitch    := 0 ;
  fCmdRoll     := 0 ;
  fCmdYaw      := 0 ;
  fPipMarkX    := 0 ;
  fPipMarkY    := 0 ;
  fPipSize     := 10 ;
  fPipKind     := pkSquare ;
  fPipBold     := false ;
  fCmdBold     := false ;
  fShowPipMark := false ;
  fSolidCmdBar := false ;
  fBorderStyle := bsNone;
  fBackColor   := clBlack;
  fGroundColor := TColor($0060A0);
  fScaleColor  := clWhite;
  fSkyColor    := TColor($C08000);
  fBarColor    := TColor($00FFFF);
  fCmdColor    := TColor($FFC0FF);
  fPipColor    := TColor($000000);
  Font.Color   := clWhite ;
  Font.Name    := 'Arial' ;
  Width        := 201;
  Height       := 201;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.Paint;
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
        PaintAdiImage(OverlayImage, PaintRect);
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

procedure ThhAvADI.PaintBackground(AnImage: TBitmap);
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

procedure ThhAvADI.PaintAdiImage(AnImage: TBitmap; PaintRect: TRect);
var
  Ymid   : integer ;
  Xmid   : integer ;

procedure DrawVerticalScale ;

procedure DrawHorizontalMark( xval,yval,LenVal : integer ; Mark : string ) ;
begin
  with AnImage.Canvas do
  begin
    Font.Size := 10 ;
    Pen.Width := 1 ;
    Pen.Color := fScaleColor ;
    MoveTo(xval - (LenVal DIV 2),yval) ;
    LineTo( xval - (LenVal DIV 2)+LenVal , yval) ;
    Brush.Style := bsClear;
    TextOut( xval - (LenVal DIV 2)+5+LenVal,yval+(Font.Height DIV 2),Mark) ;
  end ;
end ;

var
  Marker : integer ;
  M15    : integer ;
  Mstr   : string ;
begin
  with AnImage.Canvas do
  begin
{ Fixed Scale }
    DrawHorizontalMark( Xmid , Ymid    , 120, '' ) ;
    for Marker := 1 to 10 do
    begin
      M15 := Marker * 15 ;
      if ODD(Marker)
      then begin
        DrawHorizontalMark( Xmid , Ymid+M15 , 10 , '' ) ;
        DrawHorizontalMark( Xmid , Ymid-M15 , 10 , '' ) ;
      end
      else begin
        Mstr := IntToStr(Marker*5) ;
        DrawHorizontalMark( Xmid , Ymid+M15 , 20 , Mstr ) ;
        DrawHorizontalMark( Xmid , Ymid-M15 , 20 , Mstr ) ;
      end ;
    end ;
  end ;
end ;

function RollPoint( Xref,Yref,X,Y,angle : integer) : tPoint ;
const
  radian = 180.0 / pi ;
var
  Xres,Yres : real ;
  radius : real ;
  theta  : real ;
begin
  radius := SQRT((X*X)+(Y*Y)) ;
  if X <> 0
  then theta := arctan(Y/X)
  else if Y >= 0
  then theta := pi/2.0
  else theta := -pi/2.0 ;
  Xres := Xref + radius * Cos((angle / radian)+theta) ;
  Yres := Yref + radius * Sin((angle / radian)+theta) ;
  Result := Point(ROUND(Xres),ROUND(Yres)) ;
end ;

procedure DrawCommandBars ;
var
  poly3 : array [0..2] of tPoint ;
  poly6 : array [0..5] of tPoint ;
  CmdW  : integer ;

procedure DrawRunWayRef ;
begin
  with AnImage.Canvas do
  begin

{ Reference Bar }

    pen.Color := clWhite ;
    brush.Style := bsSolid ;
    brush.color := clBlack ;
    pen.width := 1 ;
    poly3[0] := Point(Xmid,Ymid) ;
    poly3[1] := Point(Xmid+40,Ymid+15) ;
    poly3[2] := Point(Xmid+24,Ymid+15) ;
    Polygon( poly3 ) ;
    poly3[0] := Point(Xmid,Ymid) ;
    poly3[1] := Point(Xmid-40,Ymid+15) ;
    poly3[2] := Point(Xmid-24,Ymid+15) ;
    Polygon( poly3 ) ;
  end ;
end ;

procedure DrawRunWay ;
begin
  with AnImage.Canvas do
  begin

{ Command Bar }

    pen.color := fCmdColor ;
    brush.color := pen.color ;
    pen.width := 1 ;
    if fSolidCmdBar
    then brush.style := bsSolid
    else brush.style := bsClear ;
    poly3[0] := RollPoint(Xmid+fCmdYaw,Ymid-fCmdPitch,5,0,fCmdRoll) ;
    poly3[1] := RollPoint(Xmid+fCmdYaw,Ymid-fCmdPitch,55,11,fCmdRoll) ;
    poly3[2] := RollPoint(Xmid+fCmdYaw,Ymid-fCmdPitch,55,19,fCmdRoll) ;
    Polygon( poly3 ) ;
    poly3[0] := RollPoint(Xmid+fCmdYaw,Ymid-fCmdPitch,44,15,fCmdRoll ) ;
    Polygon( poly3 ) ;
    poly3[0] := RollPoint(Xmid+fCmdYaw,Ymid-fCmdPitch,-5,0,180+fCmdRoll) ;
    poly3[1] := RollPoint(Xmid+fCmdYaw,Ymid-fCmdPitch,-55,11,180+fCmdRoll) ;
    poly3[2] := RollPoint(Xmid+fCmdYaw,Ymid-fCmdPitch,-55,19,180+fCmdRoll) ;
    Polygon( poly3 ) ;
    poly3[0] := RollPoint(Xmid+fCmdYaw,Ymid-fCmdPitch,-44,15,180+fCmdRoll ) ;
    Polygon( poly3 ) ;
  end ;
end ;

procedure DrawCmdBarRef ;
begin
  with AnImage.Canvas do
  begin

{ Reference Bar }

    pen.Color := clWhite ;
    pen.width := 1 ;
    brush.Style := bsSolid ;
    brush.color := clBlack ;
    CmdW := (AnImage.Width*5) DIV 12 ;
    poly6[0] := Point(Xmid+33,Ymid-3) ;
    poly6[1] := Point(Xmid+CmdW,Ymid-3) ;
    poly6[2] := Point(Xmid+CmdW,Ymid+3) ;
    poly6[3] := Point(Xmid+39,Ymid+3) ;
    poly6[4] := Point(Xmid+39,Ymid+15) ;
    poly6[5] := Point(Xmid+33,Ymid+15) ;
    Polygon( poly6 ) ;
    poly6[0] := Point(Xmid-33,Ymid-3) ;
    poly6[1] := Point(Xmid-CmdW,Ymid-3) ;
    poly6[2] := Point(Xmid-CmdW,Ymid+3) ;
    poly6[3] := Point(Xmid-39,Ymid+3) ;
    poly6[4] := Point(Xmid-39,Ymid+15) ;
    poly6[5] := Point(Xmid-33,Ymid+15) ;
    Polygon( poly6 ) ;
  end ;
end ;

procedure DrawCmdBar ;
begin
  with AnImage.Canvas do
  begin

{ Command Bar }

    pen.color := fBarColor ;
    brush.color := pen.color ;
    if fCmdBold
    then pen.Width := 3
    else pen.Width := 1 ;
    brush.style := bsSolid ;
    MoveTo(0,Ymid-fCmdPitch) ;
    LineTo(Xmid*2,Ymid-fCmdPitch) ;
    MoveTo(Xmid+fCmdYaw,0) ;
    LineTo(Xmid+fCmdYaw,Ymid*2) ;
  end ;
end ;

  begin
  case fCmdStyle of

    cmdOff: exit ;
    cmdRunWay:
    begin
      DrawRunWayRef ;
      DrawRunWay ;
    end ;
    cmdBars:
    begin
      DrawCmdBarRef ;
      DrawCmdBar ;
    end ;
    cmdBoth:
    begin
      DrawCmdBarRef ;
      DrawRunWayRef ;
      DrawCmdBar ;
      DrawRunWay ;
    end ;
  end ;
end ;

procedure DrawPipMark(X,Y : longint ) ;
var
  poly : array [0..3] of tPoint ;
  Psize07 : longint ;
  Psize10 : longint ;
  Psize11 : longint ;
  Psize12 : longint ;
  Psize14 : longint ;
  OldFont : TFont ;
  Outline : TColor ;
  Filler  : TColor ;

procedure CentreMark ;
begin
  with AnImage.Canvas do
  begin
    Ellipse( X-1,Y-1,X+1,Y+1) ;
  end ;
end ;

procedure PipIndicator( ch : char ; Colour : TColor ) ;
begin
  with AnImage.Canvas do
  begin
    OldFont := TFont.Create ;
    OldFont.Assign( Font ) ;
    Font.Color := Colour ;
    Brush.Style := bsClear ;
    if fPipBold
    then Font.Style := Font.Style + [fsBold]
    else Font.Style := Font.Style - [fsBold] ;
    TextOut(X+Psize11,Y-Psize11-(TextHeight(ch) DIV 2),ch) ;
    Font.Assign( OldFont ) ;
    OldFont.Free ;
  end ;
end ;

begin
  if not fShowPipMark then exit ;
  with AnImage.Canvas do
  begin

{ Pip Mark }

    Psize07 := (fPipSize *  7) DIV 10 ;
    Psize10 := fPipSize ;
    Psize11 := (fPipSize * 11) DIV 10 ;
    Psize12 := (fPipSize * 12) DIV 10 ;
    Psize14 := (fPipSize * 14) DIV 10 ;
    pen.Color := fPipColor ;
    if fPipBold
    then pen.Width := 2
    else pen.Width := 1 ;
    brush.Style := bsClear ;
    case fPipKind of
      pkSquare:
      begin
        MoveTo(X+Psize11,Y+Psize11) ;
        LineTo(X-Psize11,Y+Psize11) ;
        LineTo(X-Psize11,Y-Psize11) ;
        LineTo(X+Psize11,Y-Psize11) ;
        LineTo(X+Psize11,Y+Psize11) ;
        CentreMark ;
      end ;
      pkDiamond:
      begin
        MoveTo(X,Y+Psize14) ;
        LineTo(X-Psize14,Y) ;
        LineTo(X,Y-Psize14) ;
        LineTo(X+Psize14,Y) ;
        LineTo(X,Y+Psize14) ;
        CentreMark ;
      end ;
      pkCircle:
      begin
        Ellipse(X-Psize12,Y-Psize12,X+Psize12,Y+Psize12) ;
        CentreMark ;
      end ;
      pkCross:
      begin
        MoveTo(X-Psize11,Y-Psize11) ;
        LineTo(X+Psize11,Y+Psize11) ;
        MoveTo(X+Psize11,Y-Psize11) ;
        LineTo(X-Psize11,Y+Psize11) ;
      end ;
      pkSquareCross:
      begin
        MoveTo(X,Y+Psize11) ;
        LineTo(X,Y-Psize11) ;
        MoveTo(X+Psize11,Y) ;
        LineTo(X-Psize11,Y) ;
        MoveTo(X+Psize11,Y+Psize11) ;
        LineTo(X-Psize11,Y+Psize11) ;
        LineTo(X-Psize11,Y-Psize11) ;
        LineTo(X+Psize11,Y-Psize11) ;
        LineTo(X+Psize11,Y+Psize11) ;
      end ;
      pkDiamondCross:
      begin
        MoveTo(X,Y+Psize14) ;
        LineTo(X,Y-Psize14) ;
        MoveTo(X+Psize14,Y) ;
        LineTo(X-Psize14,Y) ;
        MoveTo(X,Y+Psize14) ;
        LineTo(X-Psize14,Y) ;
        LineTo(X,Y-Psize14) ;
        LineTo(X+Psize14,Y) ;
        LineTo(X,Y+Psize14) ;
      end ;
      pkCircleCross:
      begin
        MoveTo(X,Y+Psize12) ;
        LineTo(X,Y-Psize12) ;
        MoveTo(X+Psize12,Y) ;
        LineTo(X-Psize12,Y) ;
        Ellipse(X-Psize12,Y-Psize12,X+Psize12,Y+Psize12) ;
      end ;
      pkCrossCross:
      begin
        MoveTo(X-Psize11,Y-Psize11) ;
        LineTo(X+Psize11,Y+Psize11) ;
        MoveTo(X+Psize11,Y-Psize11) ;
        LineTo(X-Psize11,Y+Psize11) ;
        MoveTo(X,Y+Psize11) ;
        LineTo(X,Y-Psize11) ;
        MoveTo(X+Psize11,Y) ;
        LineTo(X-Psize11,Y) ;
      end ;
      pkPending:
      begin
        Outline := TColor($000000) ;
        Filler  := TColor($80FFFF) ;
        Pen.Color := Outline ;
        Brush.Color := Filler ;
        Arc( X-Psize07,Y,X+Psize07,Y-2*Psize07,
             X+Psize07,Y-Psize07,X-Psize07,Y-Psize07) ;
        Arc( X-2*Psize07,Y-Psize07,X,Y+Psize07,
             X-Psize07,Y-Psize07,X-Psize07,Y+Psize07) ;
        Arc( X+2*Psize07,Y-Psize07,X,Y+Psize07,
             X+Psize07-1,Y+Psize07,X+Psize07-1,Y-Psize07) ;
        MoveTo(X-Psize07,Y+Psize07-1) ;
        LineTo(X+Psize07,Y+Psize07-1) ;
        FloodFill(X-1, Y-1, Outline , fsBorder);
        Pen.Color := Brush.Color ;
        MoveTo(X-Psize07,Y+Psize07-1) ;
        LineTo(X+Psize07,Y+Psize07-1) ;
        OldFont := TFont.Create ;
        OldFont.Assign( Font ) ;
        Font.Color := clBlack ;
        if fPipBold
        then Font.Style := Font.Style + [fsBold]
        else Font.Style := Font.Style - [fsBold] ;
        TextOut(X-(TextWidth('?') DIV 2),Y-(TextHeight('?') DIV 2),'?') ;
        Font.Assign( OldFont ) ;
        OldFont.Free ;
      end ;
      pkUnknown:
      begin
        Outline := TColor($000000) ;
        Filler  := TColor($80FFFF) ;
        Pen.Color := Outline ;
        Brush.Color := Filler ;
        Arc( X-Psize07,Y,X+Psize07,Y-2*Psize07,
             X+Psize07,Y-Psize07,X-Psize07,Y-Psize07) ;
        Arc( X-2*Psize07,Y-Psize07,X,Y+Psize07,
             X-Psize07,Y-Psize07,X-Psize07,Y+Psize07) ;
        Arc( X+2*Psize07,Y-Psize07,X,Y+Psize07,
             X+Psize07-1,Y+Psize07,X+Psize07-1,Y-Psize07) ;
        MoveTo(X-Psize07,Y+Psize07-1) ;
        LineTo(X+Psize07,Y+Psize07-1) ;
        FloodFill(X-1, Y-1, Outline , fsBorder);
        Pen.Color := Brush.Color ;
        MoveTo(X-Psize07,Y+Psize07-1) ;
        LineTo(X+Psize07,Y+Psize07-1) ;
        Pen.Color := Outline ;
        CentreMark ;
      end ;
      pkFriend:
      begin
        Outline := TColor($000000) ;
        Filler  := TColor($FFE080) ;
        Pen.Color := Outline ;
        Brush.Color := Filler ;
        Arc( X-Psize11,Y+3*Psize12,X+Psize11,Y-Psize12,
             X+Psize11,Y+Psize12-1,X-Psize11,Y+Psize12) ;
        MoveTo(X-Psize11+1,Y+Psize12-1) ;
        LineTo(X+Psize11-1,Y+Psize12-1) ;
        FloodFill(X-1, Y-1, Outline , fsBorder);
        Pen.Color := Brush.Color ;
        MoveTo(X-Psize11+1,Y+Psize12-1) ;
        LineTo(X+Psize11-2,Y+Psize12-1) ;
        Pen.Color := Outline ;
        CentreMark ;
      end ;
      pkNeutral:
      begin
        Outline := TColor($000000) ;
        Filler  := TColor($AAFFAA) ;
        Pen.Color := Outline ;
        Brush.Color := Filler ;
        MoveTo(X-Psize11,Y+Psize12) ;
        LineTo(X-Psize11,Y-Psize12) ;
        LineTo(X+Psize11,Y-Psize12) ;
        LineTo(X+Psize11,Y+Psize12) ;
        LineTo(X-Psize11,Y+Psize12) ;
        FloodFill(X-1, Y-1, Outline , fsBorder);
        Pen.Color := Brush.Color ;
        MoveTo(X-Psize11+1,Y+Psize12) ;
        LineTo(X+Psize11-1,Y+Psize12) ;
        Pen.Color := Outline ;
        CentreMark ;
      end ;
      pkHostile:
      begin
        Outline := TColor($000000) ;
        Filler  := TColor($8080FF) ;
        Pen.Color := Outline ;
        Brush.Color := Filler ;
        MoveTo(X-Psize11,Y+Psize14 DIV 2) ;
        LineTo(X-Psize11,Y-Psize14 DIV 2) ;
        LineTo(X,Y-Psize14) ;
        LineTo(X+Psize11,Y-Psize14 DIV 2) ;
        LineTo(X+Psize11,Y+Psize14 DIV 2) ;
        LineTo(X-Psize11,Y+Psize14 DIV 2) ;
        FloodFill(X-1, Y-1, Outline , fsBorder);
        Pen.Color := Brush.Color ;
        MoveTo(X-Psize11+1,Y+Psize14 DIV 2) ;
        LineTo(X+Psize11-1,Y+Psize14 DIV 2) ;
        Pen.Color := Outline ;
        CentreMark ;
      end ;
      pkAssumed:
      begin
        Outline := TColor($000000) ;
        Filler  := TColor($FFE080) ;
        Pen.Color := Outline ;
        Brush.Color := Filler ;
        Arc( X-Psize11,Y+3*Psize12,X+Psize11,Y-Psize12,
             X+Psize11,Y+Psize12-1,X-Psize11,Y+Psize12) ;
        MoveTo(X-Psize11+1,Y+Psize12-1) ;
        LineTo(X+Psize11-1,Y+Psize12-1) ;
        FloodFill(X-1, Y-1, Outline , fsBorder);
        Pen.Color := Brush.Color ;
        MoveTo(X-Psize11+1,Y+Psize12-1) ;
        LineTo(X+Psize11-2,Y+Psize12-1) ;
        Pen.Color := Outline ;
        CentreMark ;
        PipIndicator( '?' , Outline ) ;
      end ;
      pkSuspect:
      begin
        Outline := TColor($000000) ;
        Filler  := TColor($8080FF) ;
        Pen.Color := Outline ;
        Brush.Color := Filler ;
        MoveTo(X-Psize11,Y+Psize14 DIV 2) ;
        LineTo(X-Psize11,Y-Psize14 DIV 2) ;
        LineTo(X,Y-Psize14) ;
        LineTo(X+Psize11,Y-Psize14 DIV 2) ;
        LineTo(X+Psize11,Y+Psize14 DIV 2) ;
        LineTo(X-Psize11,Y+Psize14 DIV 2) ;
        FloodFill(X-1, Y-1, Outline , fsBorder);
        Pen.Color := Brush.Color ;
        MoveTo(X-Psize11+1,Y+Psize14 DIV 2) ;
        LineTo(X+Psize11-1,Y+Psize14 DIV 2) ;
        Pen.Color := Outline ;
        CentreMark ;
        Pen.Color := Outline ;
        PipIndicator( '?' , Outline ) ;
      end ;
      pkJoker:
      begin
        Outline := TColor($000000) ;
        Filler  := TColor($8080FF) ;
        Pen.Color := Outline ;
        Brush.Color := Filler ;
        Arc( X-Psize11,Y+3*Psize12,X+Psize11,Y-Psize12,
             X+Psize11,Y+Psize12-1,X-Psize11,Y+Psize12) ;
        MoveTo(X-Psize11+1,Y+Psize12-1) ;
        LineTo(X+Psize11-1,Y+Psize12-1) ;
        FloodFill(X-1, Y-1, Outline , fsBorder);
        Pen.Color := Brush.Color ;
        MoveTo(X-Psize11+1,Y+Psize12-1) ;
        LineTo(X+Psize11-2,Y+Psize12-1) ;
        Pen.Color := Outline ;
        CentreMark ;
        PipIndicator( 'J' , Outline ) ;
      end ;
      pkFaker:
      begin
        Outline := TColor($000000) ;
        Filler  := TColor($8080FF) ;
        Pen.Color := Outline ;
        Brush.Color := Filler ;
        Arc( X-Psize11,Y+3*Psize12,X+Psize11,Y-Psize12,
             X+Psize11,Y+Psize12-1,X-Psize11,Y+Psize12) ;
        MoveTo(X-Psize11+1,Y+Psize12-1) ;
        LineTo(X+Psize11-1,Y+Psize12-1) ;
        FloodFill(X-1, Y-1, Outline , fsBorder);
        Pen.Color := Brush.Color ;
        MoveTo(X-Psize11+1,Y+Psize12-1) ;
        LineTo(X+Psize11-2,Y+Psize12-1) ;
        Pen.Color := Outline ;
        CentreMark ;
        PipIndicator( 'K' , Outline ) ;
      end ;
    end ;
  end ;
end ;

var
  X1,Y1 : integer ;
  X2,Y2 : integer ;
  X3,Y3 : integer ;
  X4,Y4 : integer ;
  Diam  : integer ;
  Wide  : integer ;
  Pitchvalue: integer ;
  RollValue : real ;

begin

  AnImage.Canvas.Font := Font ;
  with AnImage.Canvas do
  begin

{ Clear the image area }

    Brush.Color := fBackColor;
    FillRect(PaintRect);

    Diam := AnImage.Height ;
    Wide := AnImage.Width ;
    Ymid := Diam DIV 2 ;
    Xmid := Wide DIV 2 ;
    if FBorderStyle = bsSingle
    then begin
      Inc(Xmid);
      Inc(Ymid);
    end ;
    Pitchvalue := Ymid+(fPitch * Diam) DIV 100 ;
    RollValue := fRoll * Pi / 180.0 ;
    X1 := Xmid-Ymid ;
    Y1 := 0 ;
    X2 := Diam + X1 ;
    Y2 := Diam ;
    X3 := Xmid + Round(Ymid * COS(RollValue));
    X4 := Xmid - Round(Ymid * COS(RollValue));
    Y3 := Pitchvalue - ROUND(Ymid * SIN(RollValue)) ;
    Y4 := Pitchvalue + ROUND(Ymid * SIN(RollValue)) ;

    Pen.Width := 2 ;
    Brush.Color := fSkyColor ;
    Pen.Color := Brush.Color ;
    Pen.Style := psSolid ;
    Brush.Style := bsSolid ;
    Chord( X1,Y1,X2,Y2,X3,Y3,X4,Y4) ;
    Brush.Color := fGroundColor ;
    Pen.Color := Brush.Color ;
    Chord( X1,Y1,X2,Y2,X4,Y4,X3,Y3) ;
    DrawVerticalScale ;
    DrawCommandBars ;
    DrawPipMark(XMid+fPipMarkX,YMid-fPipMarkY) ;
  end ;
end ;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> fBorderStyle then
  begin
    fBorderStyle := Value;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.SetScaleColor(Colour : TColor);
begin
  if Colour <> fScaleColor then
  begin
    fScaleColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.SetBackColor(Colour : TColor);
begin
  if Colour <> fBackColor then
  begin
    fBackColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.SetGroundColor(Colour : TColor);
begin
  if Colour <> fGroundColor then
  begin
    fGroundColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.SetSkyColor(Colour : TColor);
begin
  if Colour <> fSkyColor then
  begin
    fSkyColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.SetBarColor(Colour : TColor);
begin
  if Colour <> fBarColor then
  begin
    fBarColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.SetCmdColor(Colour : TColor);
begin
  if Colour <> fCmdColor then
  begin
    fCmdColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.SetPipColor(Colour : TColor);
begin
  if Colour <> fPipColor then
  begin
    fPipColor := Colour;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.SetMaxPitch(Value : Longint);
begin
  if Value <> fMaxPitch then
  begin
    if Value < 1 then
      if not (csLoading in ComponentState) then
        raise EInvalidOperation.CreateFmt(SOutOfRange, [1, MaxInt]);
    fMaxPitch := Value;
    if fPitch > Value then fPitch := Value;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.SetPitch(Value : Longint);
var
  TempValue: Longint;
begin
  TempValue := fPitch ;
  if Value < -fMaxPitch then Value := -fMaxPitch
    else if Value > fMaxPitch then Value := fMaxPitch;
  if fPitch <> Value then
  begin
    fPitch := Value;
    if TempValue <> Value then { only refresh if changed }
      Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.AddPitch(Value: Longint);
begin
  SetPitch( fPitch + Value ) ;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.SetRoll(Value : Longint);
var
  TempValue: Longint;
begin
  TempValue := fRoll ;
  if fRoll <> Value then
  begin
    fRoll := Value;
    if TempValue <> Value then { only refresh if changed }
      Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.AddRoll(Value: Longint);
begin
  SetRoll( fRoll + Value ) ;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.SetCmdPitch(Value : Longint);
var
  TempValue: Longint;
begin
  TempValue := fCmdPitch ;
  if Value < -fMaxPitch then Value := -fMaxPitch
    else if Value > fMaxPitch then Value := fMaxPitch;
  if fCmdPitch <> Value then
  begin
    fCmdPitch := Value;
    if TempValue <> Value then { only refresh if changed }
      Refresh;
  end;
end ;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.AddCMDPitch(Value: Longint);
begin
  SetCMDPitch( fCmdPitch + Value ) ;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.SetCmdRoll(Value : Longint);
var
  TempValue: Longint;
begin
  TempValue := fCmdRoll ;
  if fCmdRoll <> Value then
  begin
    fCmdRoll := Value;
    if TempValue <> Value then { only refresh if changed }
      Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.AddCMDRoll(Value: Longint);
begin
  SetCMDRoll( fCmdRoll + Value ) ;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.SetCmdYaw(Value : Longint);
var
  TempValue: Longint;
begin
  TempValue := fCmdYaw ;
  if fCmdYaw <> Value then
  begin
    fCmdYaw := Value;
    if TempValue <> Value then { only refresh if changed }
      Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.AddCMDYaw(Value: Longint);
begin
  SetCMDRoll( fCmdYaw + Value ) ;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.SetCmdStyle(Value : TCmdStyle);
var
  TempValue: TCmdStyle;
begin
  TempValue := fCmdStyle ;
  if fCmdStyle <> Value then
  begin
    fCmdStyle := Value;
    if TempValue <> Value then { only refresh if changed }
      Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.SetSolidCmdBar(Value : boolean ) ;
var
  TempValue: boolean;
begin
  TempValue := fSolidCmdBar ;
  if fSolidCmdBar <> Value then
  begin
    fSolidCmdBar := Value;
    if TempValue <> Value then { only refresh if changed }
      Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.SetPipMarkX(Value : Longint);
var
  TempValue: Longint;
begin
  TempValue := fPipMarkX ;
  if fPipMarkX <> Value then
  begin
    fPipMarkX := Value;
    if TempValue <> Value then { only refresh if changed }
      Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.SetPipMarkY(Value : Longint);
var
  TempValue: Longint;
begin
  TempValue := fPipMarkY ;
  if fPipMarkY <> Value then
  begin
    fPipMarkY := Value;
    if TempValue <> Value then { only refresh if changed }
      Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.SetPipSize(Value : Longint);
var
  TempValue: Longint;
begin
  TempValue := fPipSize ;
  if (fPipSize <> Value) AND (Value > 3) AND (Value < 31) then
  begin
    fPipSize := Value;
    if TempValue <> Value then { only refresh if changed }
      Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.SetShowPipMark(Value : boolean ) ;
var
  TempValue: boolean;
begin
  TempValue := fShowPipMark ;
  if fShowPipMark <> Value then
  begin
    fShowPipMark := Value;
    if TempValue <> Value then { only refresh if changed }
      Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.SetCmdBold(Value : boolean ) ;
var
  TempValue: boolean;
begin
  TempValue := fCmdBold ;
  if fCmdBold <> Value then
  begin
    fCmdBold := Value;
    if TempValue <> Value then { only refresh if changed }
      Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.SetPipBold(Value : boolean ) ;
var
  TempValue: boolean;
begin
  TempValue := fPipBold ;
  if fPipBold <> Value then
  begin
    fPipBold := Value;
    if TempValue <> Value then { only refresh if changed }
      Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhAvADI.SetPipkind(Value: TpkKind);
begin
  if Value <> fPipKind then
  begin
    fPipKind := Value;
    Refresh;
  end;
end;

{ -------------------------------------------------------------------- }

procedure Register ;
begin
  RegisterComponents( 'Howie' , [ThhAvADI] ) ;
end ;

end.

