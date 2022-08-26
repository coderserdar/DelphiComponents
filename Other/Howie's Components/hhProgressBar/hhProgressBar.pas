{
  hhProgressBar v1.40

  Orignal concept (C)2001 Andrew Leigh
  http://www.alphalink.com.au/~leigh/components
  (Andrew no longer seems to be contactable)

  Modifications 2001..2005 by Howard Harvey
  http://homepages.picknowl.com.au/hharvey

  Description:
    TALProgressBar is an enhanced progress bar control. It allows you to tile a
    bitmap on the bar, add various color blending effects and change the
    progress direction.
    Renamed to ThhProgressBar (V1.04+ and has been extended significantly to
    provide a host of extra capabilities.

  History:
    v1.0  06-Nov-1999 Initial release.
    v1.01 30-Dec-2000 Fixed floating point division error when Min and Max
                      values are the same while trying to draw the bitmap.
    v1.02 29-Jan-2001 Added Percentage property for displaying the position text
                      in an absolute percentage value.
    v1.03 29-Sep-2001 Added Caption and ShowCaption properties to replace
                      position value with text.  Added gradient value to allow
                      rate of colour change to be altered.  (HJH)
    v1.04 25-Nov-2001 Renamed to hhProgressBar.  Added redundancy checks in
                      several Setxxx procedures to reduce unnecessary paint.
    v1.10 03-Dec-2001 Added MidPoints, expressed as a percentage (0..100),
                      to set positions at which colour blending is 50:50
                      Added UseBitmap property to allow switching between a
                      bitmap and colour blending after loading the bitmap.
    v1.11 26-Feb-2003 Added paint redundancy checks to more Setxxx functions
    v1.20 17-May-2003 Added variable border width
    v1.30 04-Aug-2003 Changed border to bevel, added bevel style
    v1.31 13-Nov-2003 Fixed % calculation bug
    v1.40 26-Dec-2004 Bitmap (not colour-bar) transparency added
                      Changed logic for position text displays
}

unit hhProgressBar;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls;

const
  Version_Number = '1.40.0' ;

type
  TProgressDirection = ( pdLeftToRight, pdRightToLeft,
                         pdBottomToTop, pdTopToBottom );
  TBarColorStyle = ( cs1Color, cs2Colors, cs3Colors );
  TBevelStyle = ( bvRaised,bvLowered,bvNone ) ;

  ThhProgressBar = class(TGraphicControl)
  private
    MainBitmap    : TBitmap;
    TiledBarBitmap: TBitmap;
    fBarBitmap    : TBitmap;
    fBevelInner   : TBevelStyle ;
    fBevelOuter   : TBevelStyle ;
    fBorderWidth  : integer;
    fBevelWidth   : integer;
    fBackgroundColor: TColor;
    fTransparent  : boolean;
    fPosition     : Integer;
    fGradient     : integer ;
    fMin          : Integer;
    fMax          : Integer;
    fMidPoint1    : integer ;
    fMidPoint2    : integer ;
    fDirection    : TProgressDirection;
    fShowPosText  : Boolean;
    fPosTextSuffix: String;
    fPosTextPrefix: String;
    fCaption      : String ;
    fShowCaption  : boolean ;
    fBarColorStyle: TBarColorStyle;
    fBarColor1: TColor;
    fBarColor2: TColor;
    fBarColor3: TColor;
    fTransparentColor: TColor;
    RegenerateBitmap : Boolean;
    fPercentage: Boolean;
    fUseBitmap : Boolean ;
    fVersion   : string ;
    fIncrement : integer ;
    fIncValue  : integer ;
    procedure PaintBevel;
    procedure PaintPosText;
    procedure SetBevelInner(const Value: TBevelStyle);
    procedure SetBevelOuter(const Value: TBevelStyle);
    procedure SetBevelWidth(const Value: Integer);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetBarBitmap(const Value: TBitmap);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetIncrement(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure SetGradient(const Value: integer);
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetMidPoint1(const Value: Integer);
    procedure SetMidPoint2(const Value: Integer);
    procedure PaintBar(RegenerateBitmap: Boolean);
    procedure TileBitmap(TiledBitmap: TBitmap; var DestBitmap: TBitmap);
    procedure SetDirection(const Value: TProgressDirection);
    procedure SetBarColor1(const Value: TColor);
    procedure SetBarColor2(const Value: TColor);
    procedure SetBarColor3(const Value: TColor);
    procedure SetTransparentColor(const Value: TColor);
    procedure SetShowPosText(const Value: Boolean);
    procedure SetShowCaption(const Value: Boolean);
    procedure SetUseBitmap(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
    procedure SetCaption(const Value: String);
    procedure SetPosTextSuffix(const Value: String);
    procedure CMFontChanged(var Message :TMessage); message CM_FontChanged;
    procedure SetPosTextPrefix(const Value: String);
    function  CalcColorIndex( StartColor, EndColor: TColor;
                              Steps, ColorIndex, Mid: Integer): TColor;
    procedure SetBarColorStyle(const Value: TBarColorStyle);
    procedure DrawColorBlending;
    procedure SetPercentage(const Value: Boolean);
    procedure Dummy(Value: String);
    function  BevelTotal : integer ;
    procedure IncrementValue( var Value : integer ) ;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BackgroundColor: TColor       read fBackgroundColor write SetBackgroundColor default clBlack;
    property BarBitmap: TBitmap            read fBarBitmap       write SetBarBitmap;
    property BarColor1: TColor             read fBarColor1       write SetBarColor1       default clGreen;
    property BarColor2: TColor             read fBarColor2       write SetBarColor2       default clYellow;
    property BarColor3: TColor             read fBarColor3       write SetBarColor3       default clRed;
    property TransparentColor: TColor      read fTransparentColor write SetTransparentColor default clWhite;
    property BarColorStyle: TBarColorStyle read fBarColorStyle   write SetBarColorStyle   default cs3Colors;
    property BevelInner: TBevelStyle       read fBevelInner      write SetBevelInner      default bvNone;
    property BevelOuter: TBevelStyle       read fBevelOuter      write SetBevelOuter      default bvLowered;
    property BevelWidth: integer           read fBevelWidth      write SetBevelWidth      default 1;
    property BorderWidth: integer          read fBorderWidth     write SetBorderWidth     default 0;
    property Direction: TProgressDirection read fDirection       write SetDirection       default pdLeftToRight;
    property Gradient: integer             read fGradient        write SetGradient        default 1;
    property Max: Integer                  read fMax             write SetMax             default 100;
    property Min: Integer                  read fMin             write SetMin             default 0;
    property MidPoint1: Integer            read fMidPoint1       write SetMidPoint1       default 50;
    property MidPoint2: Integer            read fMidPoint2       write SetMidPoint2       default 50;
    property Percentage: Boolean           read fPercentage      write SetPercentage      default False;
    property Increment: Integer            read fIncrement       write SetIncrement       default 1;
    property Position: Integer             read fPosition        write SetPosition        default 0;
    property Caption: String               read fCaption         write SetCaption;
    property PosTextPrefix: String         read fPosTextPrefix   write SetPosTextPrefix;
    property PosTextSuffix: String         read fPosTextSuffix   write SetPosTextSuffix;
    property ShowCaption: Boolean          read fShowCaption     write SetShowCaption     default False;
    property ShowPosText: Boolean          read fShowPosText     write SetShowPosText     default True;
    property UseBitmap: Boolean            read fUseBitmap       write SetUseBitmap       default False;
    property Transparent: Boolean          read fTransparent     write SetTransparent     default False;
    property Version : string              read fVersion         write Dummy              stored False;
    property Align;
    property Font;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Howie', [ThhProgressBar]);
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.Dummy(Value: String);
begin
//  Read only !
end;

{ -------------------------------------------------------------------- }
{ ThhProgressBar }

constructor ThhProgressBar.Create(AOwner: TComponent);

begin
  inherited;

  Width  := 200;
  Height := 20;

  fBevelInner      := bvNone ;
  fBevelOuter      := bvLowered ;
  fBevelWidth      := 1 ;
  fBorderWidth     := 0 ;
  fBackgroundColor := clBtnFace ;
  fBarBitmap       := TBitmap.Create ;
  fBarColor1       := clGreen ;
  fBarColor2       := clYellow ;
  fBarColor3       := clRed ;
  fBarColorStyle   := cs3Colors ;
  fIncrement       := 1 ;
  fIncValue        := 1 ;
  fPosition        := 0 ;
  fMin             := 0 ;
  fMax             := 100 ;
  fMidPoint1       := 50 ;
  fMidPoint2       := 50 ;
  fGradient        := 1 ;
  fShowCaption     := False ;
  fDirection       := pdLeftToRight;
  fShowPosText     := True ;
  fPercentage      := False ;
  fUseBitmap       := false ;
  fTransparent     := false ;
  fTransparentColor:= clWhite ;
  fPosTextSuffix   := '' ;
  fPosTextPrefix   := '' ;
  fCaption         := '' ;
  fVersion         := Version_Number ;
  RegenerateBitmap := True ;

  MainBitmap := TBitmap.Create;
  TiledBarBitmap := TBitmap.Create;
  MainBitMap.Transparent := false ;
  TiledBarBitmap.Transparent := false ;
  TiledBarBitmap.TransparentMode := tmFixed ;
  TiledBarBitmap.TransparentColor := fTransparentColor ;
  fBarBitmap.Transparent := false ;
end;

{ -------------------------------------------------------------------- }

destructor ThhProgressBar.Destroy;
begin
  MainBitmap.Free;
  fBarBitmap.Free;
  TiledBarBitmap.Free;
  inherited;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.Paint;
begin
  inherited;

  MainBitmap.Width  := Width;
  MainBitmap.Height := Height;

  if not(csReading in ComponentState) then PaintBar(RegenerateBitmap);

  PaintBevel;

  PaintPosText;

  Canvas.Draw(0, 0, MainBitmap);
  
  RegenerateBitMap := true ;
end;

{ -------------------------------------------------------------------- }

function ThhProgressBar.BevelTotal : integer ;

{ Compute total bevel width (per side) }

begin
  Result := 0 ;
  if fBevelInner <> bvNone then Result := Result + fBevelWidth ;
  if fBevelOuter <> bvNone then Result := Result + fBevelWidth ;
end ;

{ -------------------------------------------------------------------- }

function ThhProgressBar.CalcColorIndex(StartColor, EndColor: TColor;
                                       Steps, ColorIndex, Mid : Integer): TColor;

{ StartColor: initial colour (when ColorIndex=1)
  EndColor:   ending  colour (when ColorIndex=Steps)
  Steps:      number of pixel steps from Start to End
  ColorIndex: Current position (1..Steps)
  Mid:        Midpoint of change which occurs at rate
              determined by the value of "fGradient" }

var
  BeginRGBValue: Array[0..2] of Byte;
  RGBDifference: Array[0..2] of Integer;
  Red, Green, Blue: Byte;
  NumColors : Integer;
  Ycrossing : integer ;

begin
  if (ColorIndex < 1) or (ColorIndex > Steps)
  then raise
    ERangeError.Create('ColorIndex can''t be less than 1 or greater than '
                             + IntToStr(Steps));
  Ycrossing := (Steps - (Steps*fGradient*2*Mid) DIV 100) ;
  Ycrossing := Ycrossing DIV 2 ;
  ColorIndex := ColorIndex*fGradient + Ycrossing ;
  if (ColorIndex <= 1) then Result := StartColor
  else if (ColorIndex >= Steps) then Result := EndColor
  else begin
    NumColors := Steps;
//    Dec(ColorIndex);
    BeginRGBValue[0] := GetRValue(ColorToRGB(StartColor));
    BeginRGBValue[1] := GetGValue(ColorToRGB(StartColor));
    BeginRGBValue[2] := GetBValue(ColorToRGB(StartColor));
    RGBDifference[0] := GetRValue(ColorToRGB(EndColor)) - BeginRGBValue[0];
    RGBDifference[1] := GetGValue(ColorToRGB(EndColor)) - BeginRGBValue[1];
    RGBDifference[2] := GetBValue(ColorToRGB(EndColor)) - BeginRGBValue[2];

    { Calculate the bands color }

    Red   := BeginRGBValue[0] + MulDiv(ColorIndex,RGBDifference[0],NumColors-1);
    Green := BeginRGBValue[1] + MulDiv(ColorIndex,RGBDifference[1],NumColors-1);
    Blue  := BeginRGBValue[2] + MulDiv(ColorIndex,RGBDifference[2],NumColors-1);
    Result:= RGB(Red, Green, Blue);
  end ;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.IncrementValue( var Value : integer ) ;
begin
  if fIncrement > 0
  then Value := fIncrement
  else begin
    if fUseBitmap
    then begin
      if (fDirection = pdLeftToRight)
      OR (fDirection = pdRightToLeft)
      then Value := fBarBitMap.width
      else Value := fBarBitMap.height ;
    end
    else Value := 1 ;
  end ;
end ;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.PaintBar(RegenerateBitmap: Boolean);

{ Paint the bar }

var
  BarLength        : integer ;
  BarPixelLength   : integer ;
  EmptySpaceLength : integer;
  AreaTop          : integer ;
  AreaBottom       : integer ;
  AreaLeft         : integer ;
  AreaRight        : integer ;
  Border           : Integer ;

begin

{ Preclear the bar area }

  MainBitmap.Canvas.Brush.Color := fBackgroundColor;
  MainBitmap.Canvas.FillRect(Rect(0, 0, Width, Height));

{ Find border size (sum of bevels) }

  Border := BevelTotal ;

  if (fBarBitmap <> nil) and not fBarBitmap.Empty and fUseBitmap
  then begin
    if RegenerateBitmap
    then begin
      TiledBarBitmap.Height := Height - 2*(Border + fBorderWidth);
      TiledBarBitmap.Width := Width - 2*(Border + fBorderWidth);
      TileBitmap(fBarBitmap, TiledBarBitmap);
    end;
    MainBitmap.Canvas.Draw(Border+fBorderWidth, Border+fBorderWidth,
                                                TiledBarBitmap);
  end
  else if fBarColorStyle = cs1Color
  then begin
    MainBitmap.Canvas.Brush.Color := fBarColor1;
    MainBitmap.Canvas.FillRect(Rect(Border+fBorderWidth, Border+fBorderWidth,
                                    Width-Border-fBorderWidth,
                                    Height-Border-fBorderWidth));
  end
  else if fBarColorStyle in [cs2Colors, cs3Colors]
  then begin
    if RegenerateBitmap
    then begin
      TiledBarBitmap.Height := Height - 2 * (Border + fBorderWidth);
      TiledBarBitmap.Width := Width   - 2 *( Border + fBorderWidth);
      DrawColorBlending;
    end;
    MainBitmap.Canvas.Draw( Border+fBorderWidth, Border+fBorderWidth,
                            TiledBarBitmap);
  end;

  if (fDirection = pdLeftToRight) or (fDirection = pdRightToLeft)
  then BarPixelLength := Width  - 2 * (fBorderWidth + Border)
  else BarPixelLength := Height - 2 * (fBorderWidth + Border) ;

  AreaTop    := Border + fBorderWidth;
  AreaLeft   := Border + fBorderWidth;
  AreaBottom := Height - (Border + fBorderWidth);
  AreaRight  := Width  - (Border + fBorderWidth);

  if (fPosition > Min) and (fMax-fMin <> 0)
  then begin
    BarLength := Round(((fPosition-fMin) / Abs(fMax-fMin)) * BarPixelLength) ;
    BarLength := (BarLength DIV fIncValue) * fIncValue ;
  end
  else BarLength := 0;
  EmptySpaceLength := BarPixelLength - BarLength;

  MainBitmap.Canvas.Brush.Color := fBackgroundColor;
  if fDirection = pdLeftToRight
  then MainBitmap.Canvas.FillRect(Rect( AreaRight-EmptySpaceLength, AreaTop,
                                        AreaRight, AreaBottom))
  else if fDirection = pdRightToLeft
  then MainBitmap.Canvas.FillRect(Rect( AreaLeft, AreaTop,
                                        AreaLeft+EmptySpaceLength, AreaBottom))
  else if fDirection = pdTopToBottom
  then MainBitmap.Canvas.FillRect(Rect( AreaLeft, AreaBottom-EmptySpaceLength,
                                        AreaRight, AreaBottom))
  else if fDirection = pdBottomToTop
  then MainBitmap.Canvas.FillRect(Rect( AreaLeft, AreaTop,
                                        AreaRight, AreaTop+EmptySpaceLength));
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.DrawColorBlending;

{ Create the blended colour bitmap }

var
  IndexCount, MaxWidth : integer;
  MaxHeight, StartPoint: Integer;
  FirstColor : TColor ;
  SecondColor: TColor ;

begin
  if fBarColorStyle = cs2Colors
  then begin            { 2-colour version }
    MaxWidth := TiledBarBitmap.Width;
    MaxHeight := TiledBarBitmap.Height;
  end
  else begin            { 3-colour version }
    MaxWidth := TiledBarBitmap.Width div 2;
    MaxHeight := TiledBarBitmap.Height div 2;
  end;

  StartPoint := 1;
  if fDirection in [pdLeftToRight, pdRightToLeft]

{ Horizontal progress bar }

  then begin
    if fDirection = pdLeftToRight
    then begin          { 2 or 3-colour left-to-right }
      FirstColor := fBarColor1;
      SecondColor := fBarColor2;
    end
    else begin
      if fBarColorStyle = cs2Colors
      then begin        { 2-colour version right-to-left }
        FirstColor := fBarColor2;
        SecondColor := fBarColor1;
      end
      else begin        { 3-colour version right-to-left }
        FirstColor := fBarColor3;
        SecondColor := fBarColor2;
      end;
    end;

    for IndexCount := StartPoint to MaxWidth do
    begin
      TiledBarBitmap.Canvas.Pen.Color := CalcColorIndex(FirstColor, SecondColor,
                                         MaxWidth, IndexCount, fMidPoint1);
      TiledBarBitmap.Canvas.MoveTo(IndexCount-1, 0);
      TiledBarBitmap.Canvas.LineTo(IndexCount-1, TiledBarBitmap.Height);
    end;
    if fBarColorStyle = cs3Colors
    then begin
      if fDirection = pdLeftToRight
      then begin        { 3-colour version left-to-right }
        FirstColor := fBarColor2;
        SecondColor := fBarColor3;
      end
      else begin        { 3-colour version right-to-left }
        FirstColor := fBarColor2;
        SecondColor := fBarColor1;
      end;
      for IndexCount := MaxWidth+1 to TiledBarBitmap.Width do
      begin
        TiledBarBitmap.Canvas.Pen.Color := CalcColorIndex(FirstColor, SecondColor,
                                           TiledBarBitmap.Width-MaxWidth,
                                           IndexCount-MaxWidth,
                                           fMidPoint2);
        TiledBarBitmap.Canvas.MoveTo(IndexCount-1, 0);
        TiledBarBitmap.Canvas.LineTo(IndexCount-1, TiledBarBitmap.Height);
      end;
    end;
  end

{ Vertical progress bar }

  else begin {if fDirection in [pdTopToBottom, pdBottomToTop] then}
    if fDirection = pdTopToBottom
    then begin          { 2 or 3-colour Top-to-Bottom }
      FirstColor := fBarColor1;
      SecondColor := fBarColor2;
    end
    else begin          { Bottom-to-Top }
      if fBarColorStyle = cs2Colors
      then begin        { 2-colour Bottom-to-Top }
        FirstColor := fBarColor2;
        SecondColor := fBarColor1;
      end
      else begin        { 3-colour Bottom-to-Top }
        FirstColor := fBarColor3;
        SecondColor := fBarColor2;
      end;
    end;

    for IndexCount := StartPoint to MaxHeight do
    begin
      TiledBarBitmap.Canvas.Pen.Color := CalcColorIndex(FirstColor, SecondColor,
                                         MaxHeight, IndexCount, fMidPoint1);
      TiledBarBitmap.Canvas.MoveTo(0, IndexCount-1);
      TiledBarBitmap.Canvas.LineTo(TiledBarBitmap.Width, IndexCount-1);
    end;

    if fBarColorStyle = cs3Colors
    then begin
      if fDirection = pdTopToBottom
      then begin        { 3-colour Top-to-Bottom }
        FirstColor := fBarColor2;
        SecondColor := fBarColor3;
      end
      else begin        { 3-colour Bottom-to-Top }
        FirstColor := fBarColor2;
        SecondColor := fBarColor1;
      end;
      for IndexCount := MaxHeight+1 to TiledBarBitmap.Height do
      begin
        TiledBarBitmap.Canvas.Pen.Color := CalcColorIndex(FirstColor, SecondColor,
                                          TiledBarBitmap.Height-MaxHeight,
                                          IndexCount-MaxHeight,
                                          fMidPoint2);
        TiledBarBitmap.Canvas.MoveTo(0, IndexCount-1);
        TiledBarBitmap.Canvas.LineTo(TiledBarBitmap.Width, IndexCount-1);
      end;
    end;
  end
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.TileBitmap( TiledBitmap: TBitmap;
                                     var DestBitmap: TBitmap);

var
  NoOfImagesX, NoOfImagesY : integer ;
  ix, iy, XPos, YPos : Integer;

begin
  NoOfImagesX := (Width div TiledBitmap.Width) + 1;
  NoOfImagesY := (Height div TiledBitmap.Height) + 1;
  XPos := 0;
  YPos := 0;
  for iy := 1 to NoOfImagesY do
  begin
    for ix := 1 to NoOfImagesX do
    begin
      DestBitmap.Canvas.Draw(XPos, YPos, TiledBitmap);
      XPos := XPos + TiledBitmap.Width;
    end;
    YPos := YPos + TiledBitmap.Height;
    XPos := 0;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.PaintPosText;
var
  Text     : String;
  TextPosX : integer ;
  TextPosY : Integer ;

begin

{ For fShowCaption will use fCaption }

  if fShowCaption
  then begin
    Text := fCaption ;
  end

{ Otherwise computes value to insert }

  else begin

{ Caption replaced by either value or persentage }

    if fPercentage
    then Text := IntToStr(Round((fPosition-fMin) * 100 / (fMax-fMin)))+'%'
    else Text := IntToStr(fPosition) ;
  end ;

{ Prefix and Suffix text if specified }

  if fShowPosText
  then Text := fPosTextPrefix + Text + fPosTextSuffix ;
  
  TextPosX := (Width div 2)  - (MainBitmap.Canvas.TextWidth(Text) div 2);
  TextPosY := (Height div 2) - (MainBitmap.Canvas.TextHeight(Text) div 2);

  MainBitmap.Canvas.Brush.Style := bsClear;
  MainBitmap.Canvas.TextOut(TextPosX, TextPosY, Text);
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.PaintBevel;

{ Create the bevel around the outer perimeter of the bar }

var
  X1,X2 : integer ;
  Y1,Y2 : integer ;

procedure ProcessBevel( Which : TBevelStyle ) ;

var
  index : integer ;
  C1,C2 : TColor ;

begin
  if Which = bvNone then exit ;
  if Which = bvLowered
  then begin
    C1 := clBtnShadow ;
    C2 := clBtnHighlight ;
  end
  else begin
    C2 := clBtnShadow ;
    C1 := clBtnHighlight ;
  end ;

  with MainBitmap.Canvas do
  begin
    for index := 1 to fBevelWidth do
    begin
      Pen.Color := C1 ;
      Pen.Style := psSolid ;
      MoveTo( X2,Y1 ) ;
      LineTo( X1,Y1 ) ;
      LineTo( X1,Y2 ) ;
      Pen.Color := C2 ;
      LineTo( X2,Y2 ) ;
      LineTo( X2,Y1 ) ;
      INC(X1) ;
      INC(Y1) ;
      DEC(X2) ;
      DEC(Y2) ;
      if (X1 >= X2) OR (Y1 >= Y2) then break ;
    end ;
  end ;
end ;

begin
  X1 := 0 ;
  X2 := Width-1 ;
  Y1 := 0 ;
  Y2 := Height-1 ;
  ProcessBevel( fBevelOuter) ;
  ProcessBevel( fBevelInner) ;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetBarBitmap(const Value: TBitmap);

{ Set bitmap to be used for progress bar }

begin
  fBarBitmap.Assign(Value);
  IncrementValue( fIncValue ) ;
  Paint;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetBackgroundColor(const Value: TColor);

{ Background colour }
{ Default is "clBtnFace" }

begin
  if fBackgroundColor <> Value
  then begin
    fBackgroundColor := Value;
    Paint;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetBevelInner(const Value: TBevelStyle);

{ Inner bevel format }
{ TBevelStyle = ( bvRaised,bvLowered,bvNone ) ; }

begin
  if fBevelInner <>  Value
  then begin
    fBevelInner := Value;
    Paint;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetBevelOuter(const Value: TBevelStyle);

{ Outer Bevel format }
{ TBevelStyle = ( bvRaised,bvLowered,bvNone ) ; }

begin
  if fBevelOuter <>  Value
  then begin
    fBevelOuter := Value;
    Paint;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetBevelWidth(const Value: integer);

{ Bevel width in pixels (for each inner/outer as required }
{ Bevel must be positive }

begin
  if (fBevelWidth <> Value) and (Value > 0)
  then begin
    fBevelWidth := Value;
    Paint;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetBorderWidth(const Value: integer);

{ Set border around progress indicator to the bevel inner extreme }
{ Border can be zero }

begin
  if (fBorderWidth <> Value) and (Value >= 0)
  then begin
    fBorderWidth := Value;
    Paint;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetGradient(const Value: integer);

{ Set gradient of change - must be +1 .. +nnnn }
{ After +1000 not worth bothering, always 1 pixel change }

begin
  if (fGradient <> Value) and (Value > 0)
  then begin
    fGradient := Value;
    Paint;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetPosition(const Value: Integer);

{ Set position of progress bar - must be in legitimate range }

begin
  if (fPosition <> Value) and (Value <= fMax) and (Value >= fMin)
  then begin
    fPosition := Value;
    RegenerateBitMap := false ;
    Paint;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetIncrement(const Value: Integer);

{ This sets the increment (in pixels) that the progress will
  be displayed. }

{ If Value = 0 then uses bitmap size:
    width (horizontal) or
    height (vertical)
  for autoincrements, or 1 if not in bitmap mode }

begin
  if (fIncrement <> Value) and (Value >= 0) and (Value <= fMax)
  then begin
    fIncrement := Value;
    IncrementValue( fIncValue ) ;
    RegenerateBitMap := false ;
    Paint;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetMax(const Value: Integer);

{ Maximum value for progress bar }

begin
  if fMax <> Value
  then begin
    fMax := Value;
    Paint;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetMin(const Value: Integer);

{ Minimum value for progress bar }

begin
  if fMin <> Value
  then begin
    fMin := Value;
    Paint;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetMidPoint1(const Value: Integer);

{ First midpoint (i.e. for colour1..colour2 change }

begin

{ Limit midpoint to 0..100% of range }

  if (Value >= 0) AND (Value <= 100) AND (fMidPoint1 <> Value)
  then begin
    fMidPoint1 := Value;
    Paint;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetMidPoint2(const Value: Integer);

{ Second midpoint (i.e. for colour2..colour3 change }

begin

{ Limit midpoint to 0..100% of range }

  if (Value >= 0) AND (Value <= 100) AND (fMidPoint2 <> Value)
  then begin
    fMidPoint2 := Value;
    Paint;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetDirection(const Value: TProgressDirection);

{ TProgressDirection = ( pdLeftToRight, pdRightToLeft,
                         pdBottomToTop, pdTopToBottom); }

begin
  if fDirection <>  Value
  then begin
    fDirection := Value;
    IncrementValue( fIncValue ) ;
    Paint;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetBarColor1(const Value: TColor);

{ Set first (lower range) colour }

begin
  if fBarColor1 <> Value
  then begin
    fBarColor1 := Value;
    Paint;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetBarColor2(const Value: TColor);

{ Set second (mid range) colour }

begin
  if fBarColor2 <> Value
  then begin
    fBarColor2 := Value;
    Paint;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetBarColor3(const Value: TColor);

{ Set third (high range) colour }

begin
  if fBarColor3 <> Value
  then begin
    fBarColor3 := Value;
    Paint;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetTransparentColor(const Value: TColor);

{ Set transparent colour - this is the colour in a bitmap which
  will force transparency.  Default is clWhite }

begin
  if fTransparentColor <> Value
  then begin
    fTransparentColor := Value;
    TiledBarBitmap.TransparentColor := Value ;
    Paint;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetShowCaption(const Value: Boolean);

{ Show caption flag }

begin
  if fShowCaption <> Value
  then begin
    fShowCaption := Value;
    RegenerateBitMap := false ;
    Paint;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetShowPosText(const Value: Boolean);

{ Show Position Text flag }

begin
  if fShowPosText <> Value
  then begin
    fShowPosText := Value;
    Paint;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetUseBitmap(const Value: Boolean);

{ Use bitmap flag }
{ Transparency can only be used for bitmaps, not standard bar }

begin
  if fUseBitmap <> Value
  then begin
    fUseBitmap := Value;
    TiledBarBitmap.Transparent := Value AND fTransparent ;
    IncrementValue( fIncValue ) ;
    Paint;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetTransparent(const Value: Boolean);

{ Set transparency flag }

begin
  if fTransparent <> Value
  then begin
    fTransparent := Value;
    TiledBarBitmap.Transparent := Value AND fUseBitmap;
    Paint;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetCaption(const Value: String);

{ Set caption }

begin
  if fCaption <> Value
  then begin
    fCaption := Value;
    RegenerateBitMap := false ;
    Paint;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetPosTextSuffix(const Value: String);

{ Text after position caption }

begin
  if fPosTextSuffix <> Value
  then begin
    fPosTextSuffix := Value;
    RegenerateBitMap := false ;
    Paint;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetPosTextPrefix(const Value: String);

{ Text before position caption }

begin
  if fPosTextPrefix <> Value
  then begin
    fPosTextPrefix := Value;
    RegenerateBitMap := false ;
    Paint;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.CMFontChanged(var Message: TMessage);

{ Font definition }

begin
  MainBitmap.Canvas.Font.Assign(Self.Font);
  Paint;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetBarColorStyle(const Value: TBarColorStyle);

{ Standard bar: 1, 2 or 3 colours }
{  TBarColorStyle = (cs1Color, cs2Colors, cs3Colors);  }

begin
  if fBarColorStyle <> Value
  then begin
    fBarColorStyle := Value;
    IncrementValue( fIncValue ) ;
    Paint;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure ThhProgressBar.SetPercentage(const Value: Boolean);

{ Percentage indication flag }

begin
  if fPercentage <> Value
  then begin
    fPercentage := Value;
    Paint;
  end ;
end;

end.
