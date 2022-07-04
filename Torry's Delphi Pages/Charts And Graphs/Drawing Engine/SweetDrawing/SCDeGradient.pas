{------------------------------------------------------------------------------}
{                                                                              }
{  TGradient v2.60                                                             }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

unit SCDeGradient;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SCCommon, SCConsts;

type
  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array[0..1024] of TRGBQuad;

  TSCDeGradientColors = array[0..255] of TRGBQuad;

  TSCDeGradientShift = -100..100;
  TGradientRotation = -100..100;

  TSCDeGradientStyle = (scdgsCustom, scdgsRadialC, scdgsRadialT, scdgsRadialB,
    scdgsRadialL, scdgsRadialR, scdgsRadialTL, scdgsRadialTR, scdgsRadialBL,
    scdgsRadialBR, scdgsLinearH, scdgsLinearV, scdgsReflectedH, scdgsReflectedV,
    scdgsDiagonalLF, scdgsDiagonalLB, scdgsDiagonalRF, scdgsDiagonalRB,
    scdgsArrowL, scdgsArrowR, scdgsArrowU, scdgsArrowD, scdgsDiamond,
    scdgsButterfly);

  TSCDeCustomGradientEvent = procedure(Sender: TObject;
    const Colors: TSCDeGradientColors; Pattern: TBitmap) of object;

  TSCDeShapeGradient = class(TPersistent)
  private
    FColorBegin: TColor;
    FColorEnd: TColor;
    FUseSysColors: Boolean;
    FStyle: TSCDeGradientStyle;
    FShift: TSCDeGradientShift;
    FRotation: TGradientRotation;
    FReverse: Boolean;
    FPattern: TBitmap;
    FOnCustom: TSCDeCustomGradientEvent;
    FUpdateCount: Integer;
    FUpdatePended: Boolean;
    FDirty: Boolean;
    procedure SetColorBegin(Value: TColor);
    procedure SetColorEnd(Value: TColor);
    procedure SetUseSysColors(Value: Boolean);
    procedure SetStyle(Value: TSCDeGradientStyle);
    procedure SetShift(Value: TSCDeGradientShift);
    procedure SetRotation(Value: TGradientRotation);
    procedure SetReverse(Value: Boolean);

    function  IsColorBeginSaved: Boolean;
    function  IsColorEndSaved: Boolean;
  protected
    procedure UpdatePattern; virtual;
    procedure UpdateSysColors; virtual;

    property Pattern: TBitmap read FPattern;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function  CopyPatternTo(ABitmap: TBitmap): Boolean;
    procedure InvalidatePattern;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure Paint(ACanvas: TCanvas; ARect: TRect);
  published
    property ColorBegin: TColor read FColorBegin write SetColorBegin stored IsColorBeginSaved;
    property ColorEnd: TColor read FColorEnd write SetColorEnd stored IsColorEndSaved;
    property Reverse: Boolean read FReverse write SetReverse default False;
    property Rotation: TGradientRotation read FRotation write SetRotation default 0;
    property Shift: TSCDeGradientShift read FShift write SetShift default 0;
    property Style: TSCDeGradientStyle read FStyle write SetStyle default scdgsRadialC;
    property UseSysColors: Boolean read FUseSysColors write SetUseSysColors default False;
  end;

implementation

procedure RadialCentral(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y, rX: Integer;
  pRGB: PRGBQuad;
  Row1, Row2: PRGBQuadArray;
  PreCalcXs: array[0..180] of Integer;
begin
  Pattern.Width := 362;
  Pattern.Height := 362;

  rX := 0;
  for X := 180 downto 0 do
  begin
    PreCalcXs[rX] := X * X;
    Inc(rX);
  end;

  for Y := 180 downto 0 do
  begin
    Row1 := PRGBQuadArray(Pattern.ScanLine[Y]);
    Row2 := PRGBQuadArray(Pattern.ScanLine[361-Y]);
    for X := 180 downto 0 do
    begin
      rX := 361 - X;
      pRGB := @Colors[Round(Sqrt(PreCalcXs[X] + PreCalcXs[Y]))];
      Row1[X] := pRGB^;
      Row1[rX] := pRGB^;
      Row2[X] := pRGB^;
      Row2[rX] := pRGB^;
    end;
  end;
end;

procedure RadialTop(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y, rX, rY: Integer;
  pRGB: PRGBQuad;
  Row: PRGBQuadArray;
  PreCalcY: Integer;
  PreCalcXs: array[0..180] of Integer;
begin
  Pattern.Width := 362;
  Pattern.Height := 181;

  rX := 0;
  for X := 180 downto 0 do
  begin
    PreCalcXs[rX] := X * X;
    Inc(rX);
  end;

  rY := 0;
  for Y := 180 downto 0 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    PreCalcY := PreCalcXs[rY];
    rX := 181;
    for X := 180 downto 0 do
    begin
      pRGB := @Colors[Round(Sqrt(PreCalcXs[X] + PreCalcY))];
      Row[X] := pRGB^;
      Row[rX] := pRGB^;
      Inc(rX);
    end;
    Inc(rY);
  end;
end;

procedure RadialBottom(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y, rX: Integer;
  pRGB: PRGBQuad;
  Row: PRGBQuadArray;
  PreCalcY: Integer;
  PreCalcXs: array[0..180] of Integer;
begin
  Pattern.Width := 362;
  Pattern.Height := 181;

  rX := 0;
  for X := 180 downto 0 do
  begin
    PreCalcXs[rX] := X * X;
    Inc(rX);
  end;

  for Y := 180 downto 0 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    PreCalcY := PreCalcXs[Y];
    rX := 181;
    for X := 180 downto 0 do
    begin
      pRGB := @Colors[Round(Sqrt(PreCalcXs[X] + PreCalcY))];
      Row[X] := pRGB^;
      Row[rX]:= pRGB^;
      Inc(rX);
    end;
  end;
end;

procedure RadialLeft(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y, rY: Integer;
  pRGB: PRGBQuad;
  Row1, Row2: PRGBQuadArray;
  PreCalcY: Integer;
  PreCalcXs: array[0..180] of Integer;
begin
  Pattern.Width := 181;
  Pattern.Height := 362;

  for X := 180 downto 0 do
    PreCalcXs[X] := X * X;

  rY := 180;
  for Y := 0 to 180 do
  begin
    Row1 := PRGBQuadArray(Pattern.ScanLine[Y]);
    Row2 := PRGBQuadArray(Pattern.ScanLine[361-Y]);
    PreCalcY := PreCalcXs[rY];
    for X := 0 to 180 do
    begin
      pRGB := @Colors[Round(Sqrt(PreCalcXs[X] + PreCalcY))];
      Row1[X] := pRGB^;
      Row2[X] := pRGB^;
    end;
    Dec(rY);
  end;
end;

procedure RadialRight(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y, rX: Integer;
  pRGB: PRGBQuad;
  Row1, Row2: PRGBQuadArray;
  PreCalcXs: array[0..180] of Integer;
begin
  Pattern.Width := 181;
  Pattern.Height := 362;

  rX := 0;
  for X := 180 downto 0 do
  begin
    PreCalcXs[rX] := X * X;
    Inc(rX);
  end;

  for Y := 0 to 180 do
  begin
    Row1 := PRGBQuadArray(Pattern.ScanLine[Y]);
    Row2 := PRGBQuadArray(Pattern.ScanLine[361-Y]);
    for X := 0 to 180 do
    begin
      pRGB := @Colors[Round(Sqrt(PreCalcXs[X] + PreCalcXs[Y]))];
      Row1[X] := pRGB^;
      Row2[X] := pRGB^;
    end;
  end;
end;

procedure RadialTopLeft(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y: Integer;
  Row: PRGBQuadArray;
  PreCalcY: Integer;
  PreCalcXs: array[0..180] of Integer;
begin
  Pattern.Width := 181;
  Pattern.Height := 181;

  for X := 180 downto 0 do
    PreCalcXs[X] := X * X;

  for Y := 0 to 180 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    PreCalcY := PreCalcXs[Y];
    for X := 0 to 180 do
      Row[X] := Colors[Round(Sqrt(PreCalcXs[X] + PreCalcY))];
  end;
end;

procedure RadialTopRight(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y, rX, rY: Integer;
  Row: PRGBQuadArray;
  PreCalcY: Integer;
  PreCalcXs: array[0..180] of Integer;
begin
  Pattern.Width := 181;
  Pattern.Height := 181;

  rX :=0;
  for X := 180 downto 0 do
  begin
    PreCalcXs[rX] := X * X;
    Inc(rX);
  end;

  rY := 180;
  for Y := 0 to 180 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    PreCalcY := PreCalcXs[rY];
    for X := 0 to 180 do
      Row[X] := Colors[Round(Sqrt(PreCalcXs[X] + PreCalcY))];
    Dec(rY);
  end;
end;

procedure RadialBottomLeft(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y, rY: Integer;
  Row: PRGBQuadArray;
  PreCalcY: Integer;
  PreCalcXs: array[0..180] of Integer;
begin
  Pattern.Width := 181;
  Pattern.Height := 181;

  for X := 180 downto 0 do
    PreCalcXs[X] := X * X;

  rY := 180;
  for Y := 0 to 180 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    PreCalcY := PreCalcXs[rY];
    for X := 0 to 180 do
      Row[X] := Colors[Round(Sqrt(PreCalcXs[X] + PreCalcY))];
    Dec(rY);
  end;
end;

procedure RadialBottomRight(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y, rX: Integer;
  Row: PRGBQuadArray;
  PreCalcY: Integer;
  PreCalcXs: array[0..180] of Integer;
begin
  Pattern.Width := 181;
  Pattern.Height := 181;

  rX := 0;
  for X := 180 downto 0 do
  begin
    PreCalcXs[rX] := X * X;
    Inc(rX);
  end;

  for Y := 0 to 180 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    PreCalcY := PreCalcXs[Y];
    for X := 0 to 180 do
      Row[X] := Colors[Round(Sqrt(PreCalcXs[X] + PreCalcY))];
  end;
end;

procedure LinearHorizontal(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 256;
  Pattern.Height := 1;
  Row := PRGBQuadArray(Pattern.ScanLine[0]);
  for X := 0 to 255 do
    Row[X] := Colors[X];
end;

procedure LinearVertical(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  Y: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 1;
  Pattern.Height := 256;
  for Y := 0 to 255 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    Row[0] := Colors[Y];
  end;
end;

procedure ReflectedHorizontal(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  Y: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 1;
  Pattern.Height := 512;
  for Y := 0 to 255 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    Row[0] := Colors[255 - Y];
    Row := PRGBQuadArray(Pattern.ScanLine[511 - Y]);
    Row[0] := Colors[255 - Y];
  end;
end;

procedure ReflectedVertical(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 512;
  Pattern.Height := 1;
  Row := PRGBQuadArray(Pattern.ScanLine[0]);
  for X := 0 to 255 do
  begin
    Row[X] := Colors[255 - X];
    Row[511 - X] := Colors[255 - X];
  end;
end;

procedure DiagonalLinearForward(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 128;
  Pattern.Height := 129;
  for Y := 0 to 128 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 127 do
      Row[X] := Colors[X + Y];
  end;
end;

procedure DiagonalLinearBackward(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 128;
  Pattern.Height := 129;
  for Y := 0 to 128 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 127 do
      Row[X] := Colors[127 + (Y - X)];
  end;
end;

procedure DiagonalReflectedForward(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 256;
  Pattern.Height := 256;
  for Y := 0 to 255 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 255 do
      if X + Y < 255 then
        Row[X] := Colors[255 - (X + Y)]
      else
        Row[X] := Colors[(Y + X) - 255];
  end;
end;

procedure DiagonalReflectedBackward(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 256;
  Pattern.Height := 256;
  for Y := 0 to 255 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 255 do
      if X > Y then
        Row[X] := Colors[X - Y]
      else
        Row[X] := Colors[Y - X];
  end;
end;

procedure ArrowLeft(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 129;
  Pattern.Height := 256;
  for Y := 0 to 127 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 128 do
      Row[X] := Colors[255 - (X + Y)];
  end;
  for Y := 128 to 255 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 128 do
      Row[X] := Colors[Y - X];
  end;
end;

procedure ArrowRight(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 129;
  Pattern.Height := 256;
  for Y := 0 to 127 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 128 do
      Row[X] := Colors[(X - Y) + 127];
  end;
  for Y := 128 to 255 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 128 do
      Row[X] := Colors[(X + Y) - 128];
  end;
end;

procedure ArrowUp(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 256;
  Pattern.Height := 129;
  for Y := 0 to 128 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 127 do
      Row[X] := Colors[255 - (X + Y)];
    for X := 128 to 255 do
      Row[X] := Colors[X - Y];
  end;
end;

procedure ArrowDown(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 256;
  Pattern.Height := 129;
  for Y := 0 to 128 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 127 do
      Row[X] := Colors[127 + (Y - X)];
    for X := 128 to 255 do
      Row[X] := Colors[(X + Y) - 128];
  end;
end;

procedure Diamond(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 256;
  Pattern.Height := 256;
  for Y := 0 to 127 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 127 do
      Row[X] := Colors[255 - (X + Y)];
    for X := 128 to 255 do
      Row[X] := Colors[X - Y];
  end;
  for Y := 128 to 255 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 127 do
      Row[X] := Colors[Y - X];
    for X := 128 to 255 do
      Row[X] := Colors[(X + Y) - 255];
  end;
end;

procedure Butterfly(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 256;
  Pattern.Height := 256;
  for Y := 0 to 127 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 127 do
      Row[X] := Colors[(X - Y) + 128];
    for X := 128 to 255 do
      Row[X] := Colors[383 - (X + Y)];
  end;
  for Y := 128 to 255 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 127 do
      Row[X] := Colors[(X + Y) - 128];
    for X := 128 to 255 do
      Row[X] := Colors[128 + (Y - X)];
  end;
end;

{ TSCDeShapeGradient }

type
  TSCDePatternBuilder = procedure(const Colors: TSCDeGradientColors; Pattern: TBitmap);

const
  PatternBuilder: array[TSCDeGradientStyle] of TSCDePatternBuilder = (nil,
    RadialCentral, RadialTop, RadialBottom, RadialLeft, RadialRight,
    RadialTopLeft, RadialTopRight, RadialBottomLeft, RadialBottomRight,
    LinearHorizontal, LinearVertical, ReflectedHorizontal, ReflectedVertical,
    DiagonalLinearForward, DiagonalLinearBackward, DiagonalReflectedForward,
    DiagonalReflectedBackward, ArrowLeft, ArrowRight, ArrowUp, ArrowDown,
    Diamond, Butterfly);

constructor TSCDeShapeGradient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorBegin := clWhite;
  FColorEnd := clBtnFace;
  FStyle := scdgsRadialC;
  FShift := 0;
  FRotation := 0;
  FReverse := False;
  FUseSysColors := False;

  FPattern := TBitmap.Create;
  FPattern.PixelFormat := pf32bit;

  UpdatePattern;
end;

destructor TSCDeShapeGradient.Destroy;
begin
  FreeAndNil(FPattern);
  inherited Destroy;
end;

procedure TSCDeShapeGradient.Paint(ACanvas: TCanvas; ARect: TRect);
begin
 if not FDirty and IsRectEmpty(ARect) then
   ACanvas.StretchDraw(ARect, Pattern);
end;

procedure TSCDeShapeGradient.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TSCDeShapeGradient.EndUpdate;
begin
  Dec(FUpdateCount);
  if (FUpdateCount = 0) and FUpdatePended then
    UpdatePattern;
end;

function TSCDeShapeGradient.CopyPatternTo(ABitmap: TBitmap): Boolean;
begin
  Result := False;
  if not FDirty and (FUpdateCount = 0) and Assigned(ABitmap) then
  begin
    ABitmap.Assign(Self.Pattern);
    Result := True;
  end;
end;

procedure TSCDeShapeGradient.InvalidatePattern;
begin
  UpdatePattern;
end;

procedure TSCDeShapeGradient.SetColorBegin(Value: TColor);
begin
  if FColorBegin <> Value then
  begin
    FColorBegin := Value;
    FUseSysColors := False;
    UpdatePattern;
  end;
end;

procedure TSCDeShapeGradient.SetColorEnd(Value: TColor);
begin
  if FColorEnd <> Value then
  begin
    FColorEnd := Value;
    FUseSysColors := False;
    UpdatePattern;
  end;
end;

procedure TSCDeShapeGradient.SetUseSysColors(Value: Boolean);
begin
  if FUseSysColors <> Value then
  begin
    FUseSysColors := Value;
    if FUseSysColors then
      UpdateSysColors;
  end;
end;

procedure TSCDeShapeGradient.SetStyle(Value: TSCDeGradientStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    UpdatePattern;
  end;
end;

procedure TSCDeShapeGradient.SetShift(Value: TSCDeGradientShift);
begin
  if Value < Low(TSCDeGradientShift) then
    Value := Low(TSCDeGradientShift)
  else if Value > High(TSCDeGradientShift) then
    Value := High(TSCDeGradientShift);

  if FShift <> Value then
  begin
    FShift := Value;
    UpdatePattern;
  end;
end;

procedure TSCDeShapeGradient.SetRotation(Value: TGradientRotation);
begin
  if Value < Low(TGradientRotation) then
    Value := Low(TGradientRotation)
  else if Value > High(TGradientRotation) then
    Value := High(TGradientRotation);

  if FRotation <> Value then
  begin
    FRotation := Value;
    UpdatePattern;
  end;
end;

procedure TSCDeShapeGradient.SetReverse(Value: Boolean);
begin
  if FReverse <> Value then
  begin
    FReverse := Value;
    UpdatePattern;
  end;
end;

function TSCDeShapeGradient.IsColorBeginSaved: Boolean;
begin
  Result := not UseSysColors and (ColorBegin <> clWhite);
end;

function TSCDeShapeGradient.IsColorEndSaved: Boolean;
begin
  Result := not UseSysColors and (ColorBegin <> clBtnFace);
end;

procedure TSCDeShapeGradient.UpdateSysColors;
begin
  BeginUpdate;
  try
    ColorBegin := GetSysColor(COLOR_ACTIVECAPTION);
    try
      ColorEnd := GetSysColor(COLOR_GRADIENTACTIVECAPTION);
      FUseSysColors := True;
    except
      // This windows version doesn't support gradient colors...
      ColorEnd := ColorBegin;
      FUseSysColors := False;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TSCDeShapeGradient.UpdatePattern;
var
  Colors: TSCDeGradientColors;
  dRed, dGreen, dBlue: Integer;
  RGBColor1, RGBColor2: TColor;
  RGB1, RGB2: TRGBQuad;
  UpdatedRect: TRect;
  Index, rIndex: Integer;
  M, rM: Integer;
begin
  FUpdatePended := True;

  if (csLoading in ComponentState) or (FUpdateCount <> 0) then Exit;

  FUpdatePended := False;

  if Reverse then
  begin
    RGBColor1 := ColorToRGB(ColorEnd);
    RGBColor2 := ColorToRGB(ColorBegin);
  end
  else
  begin
    RGBColor1 := ColorToRGB(ColorBegin);
    RGBColor2 := ColorToRGB(ColorEnd);
  end;

  RGB1.rgbRed := GetRValue(RGBColor1);
  RGB1.rgbGreen := GetGValue(RGBColor1);
  RGB1.rgbBlue := GetBValue(RGBColor1);
  RGB1.rgbReserved := 0;

  RGB2.rgbRed := GetRValue(RGBColor2);
  RGB2.rgbGreen := GetGValue(RGBColor2);
  RGB2.rgbBlue := GetBValue(RGBColor2);
  RGB2.rgbReserved := 0;

  if Shift > 0 then
  begin
    RGB1.rgbRed := Byte(RGB1.rgbRed + MulDiv(RGB2.rgbRed - RGB1.rgbRed, Shift, 100));
    RGB1.rgbGreen := Byte(RGB1.rgbGreen + MulDiv(RGB2.rgbGreen - RGB1.rgbGreen, Shift, 100));
    RGB1.rgbBlue := Byte(RGB1.rgbBlue + MulDiv(RGB2.rgbBlue - RGB1.rgbBlue, Shift, 100));
  end
  else if Shift < 0 then
  begin
    RGB2.rgbRed := Byte(RGB2.rgbRed + MulDiv(RGB2.rgbRed - RGB1.rgbRed, Shift, 100));
    RGB2.rgbGreen := Byte(RGB2.rgbGreen + MulDiv(RGB2.rgbGreen - RGB1.rgbGreen, Shift, 100));
    RGB2.rgbBlue := Byte(RGB2.rgbBlue + MulDiv(RGB2.rgbBlue - RGB1.rgbBlue, Shift, 100));
  end;

  dRed := RGB2.rgbRed - RGB1.rgbRed;
  dGreen := RGB2.rgbGreen - RGB1.rgbGreen;
  dBlue := RGB2.rgbBlue - RGB1.rgbBlue;

  M := MulDiv(255, Rotation, 100);
  if M = 0 then
    for Index := 0 to 255 do
      with Colors[Index] do
      begin
        rgbRed := RGB1.rgbRed + (Index * dRed) div 255;
        rgbGreen := RGB1.rgbGreen + (Index * dGreen) div 255;
        rgbBlue := RGB1.rgbBlue + (Index * dBlue) div 255;
      end
  else if M > 0 then
  begin
    M := 255 - M;
    for Index := 0 to M - 1 do
      with Colors[Index] do
      begin
        rgbRed := RGB1.rgbRed + (Index * dRed) div M;
        rgbGreen := RGB1.rgbGreen + (Index * dGreen) div M;
        rgbBlue := RGB1.rgbBlue + (Index * dBlue) div M;
      end;
    for Index := M to 255 do
      with Colors[Index] do
      begin
        rIndex := 255 - Index;
        rM := 255 - M;
        rgbRed := RGB1.rgbRed + ((rIndex) * dRed) div (rM);
        rgbGreen := RGB1.rgbGreen + ((rIndex) * dGreen) div (rM);
        rgbBlue := RGB1.rgbBlue + ((rIndex) * dBlue) div (rM);
      end;
  end
  else if M < 0 then
  begin
    M := -M;
    for Index := 0 to M do
      with Colors[Index] do
      begin
        rgbRed := RGB2.rgbRed - (Index * dRed) div M;
        rgbGreen := RGB2.rgbGreen - (Index * dGreen) div M;
        rgbBlue := RGB2.rgbBlue - (Index * dBlue) div M;
      end;
    for Index := M + 1 to 255 do
      with Colors[Index] do
      begin
        rIndex := 255 - Index;
        rM := 255 - M;
        rgbRed := RGB2.rgbRed - ((rIndex) * dRed) div (rM);
        rgbGreen := RGB2.rgbGreen - ((rIndex) * dGreen) div (rM);
        rgbBlue := RGB2.rgbBlue - ((rIndex) * dBlue) div (rM);
      end;
  end;

  FDirty := True;
  try
    if @PatternBuilder[Style] <> nil then
      PatternBuilder[Style](Colors, Pattern)
    else if Assigned(FOnCustom) then
      FOnCustom(Self, Colors, Pattern)
    else
    begin
      Pattern.Width := 2;
      Pattern.Height := 2;
      Pattern.Canvas.Pixels[0, 0] := RGBColor1;
      Pattern.Canvas.Pixels[0, 1] := RGBColor2;
      Pattern.Canvas.Pixels[1, 0] := RGBColor2;
      Pattern.Canvas.Pixels[1, 1] := RGBColor1;
    end;
  finally
    FDirty := False;
  end;
end;

end.