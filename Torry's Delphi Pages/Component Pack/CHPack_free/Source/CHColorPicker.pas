unit CHColorPicker;

{ ##############################################################################
  TCHColorPicker

  Version   		:   1.1.0
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 21.07.2002    - First Release
  1.0.1 - 09.03.2003    - reorganize "uses" for more performance and less memory needed
  1.1.0 - 19.10.2003    - NEW/BUG: 90 % new code, new functions and procedures

  ############################################################################ }

interface

uses
  Windows, SysUtils, Classes, ExtCtrls, Graphics, Math;

const
  clMoneyGreen = TColor($C0DCC0);   // Color   "8"  RGB:  192 220 192
  clSkyBlue    = TColor($F0CAA6);   // Color   "9"  RGB:  166 202 240
  clCream      = TColor($F0FBFF);   // Color "246"  RGB:  255 251 240
  clMediumGray = TColor($A4A0A0);   // Color "247"  RGB:  160 160 164

  NonDitherColors:  ARRAY[0..19] OF TColor = (clBlack, clMaroon, clGreen, clOlive,
    clNavy, clPurple, clTeal, clSilver, clMoneyGreen, clSkyblue, clCream, clMediumGray,
    clGray, clRed, clGreen, clYellow, clBlue, clFuchsia, clAqua, clWhite);

  MaskBlack       =  0;         {  0   0   0}
  MaskDarkRed     =  1;         {128   0   0}
  MaskDarkGreen   =  2;         {  0 128   0}
  MaskPeaGreen    =  3;         {128 128   0}
  MaskDarkBlue    =  4;         {  0   0 128}
  MaskLavender    =  5;         {128   0 128}
  MaskSlate       =  6;         {  0 128 128}
  MaskLightGray   =  7;         {192 192 192}

type
  TColorSpace = (csRGB, csHSV, csHLS, csCMYK);

type
    EColorError = class(Exception);

type
  TCHColorPicker = class;

  TPickTimer = class(TPersistent)
  private
    FOwner : TCHColorPicker;
    FTimer : TTimer;
  public
    constructor Create(AOwner: TCHColorPicker); virtual;
    destructor Destroy; override;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHColorPicker = class(TComponent)
  private
    FOnTimer : TNotifyEvent;
    FPickTimer : TPickTimer;

    FEnabled : Boolean;
    FInterval : Cardinal;

    procedure GetColor(Sender: TObject);
    function DesktopColor(const x, y: integer): TColor;
    procedure ConvertColor(const Color : TColor);
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);
  public
    FColor : TColor;
    FColorString : string;
    FColorHex : string;
    FColorHTML : string;

    FColorRGB_R : Byte;
    FColorRGB_G : Byte;
    FColorRGB_B : Byte;
    FColorCMYK_C : Byte;
    FColorCMYK_M : Byte;
    FColorCMYK_Y : Byte;
    FColorCMYK_K : Byte;

    FColorHSV_H : Integer;
    FColorHSV_S : Integer;
    FColorHSV_V : Integer;
    FColorHLS_H : Integer;
    FColorHLS_L : Integer;
    FColorHLS_S : Integer;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Color
    function ColorToRGBTriple(const Color: TColor): TRGBTriple;
    function RGBtoRGBTriple(const red, green, blue: Byte): TRGBTriple;
    function ColorToHtml(cColor: TColor): string;

    // HLS
    function  HLSToRGBTriple (const H,L,S : Integer): TRGBTriple;
    procedure RGBTripleToHLS (const RGBTriple: TRGBTriple; var H,L,S: Integer);
    procedure HLStoRGB(const H,L,S: Double; var R,G,B: Double);
    procedure RGBToHLS(const R,G,B: Double; var H,L,S: Double);

    // HSV
    function HSVToRGBTriple (const H,S,V: Integer): TRGBTriple;
    procedure RGBTripleToHSV (const RGBTriple: TRGBTriple; var H,S,V: Integer);
    procedure HSVtoRGB(const H,S,V: Double; var R,G,B: Double);
    procedure RGBToHSV(const R,G,B: Double; var H,S,V: Double);

    // CMY
    function CMYToRGBTriple (const C,M,Y: Integer):  TRGBTriple;
    procedure RGBTripleToCMY(const RGB: TRGBTriple; var C,M,Y: Integer);
    procedure CMYtoRGB(const C,M,Y: Double; var R,G,B: Double);
    procedure RGBToCMY(const R,G,B: Double; var C,M,Y: Double);

    // CMYK
    function CMYKToRGBTriple (const C,M,Y,K: Integer):  TRGBTriple;
    procedure RGBTripleToCMYK(const RGB: TRGBTriple; var C,M,Y,K: Integer);
    procedure CMYKtoRGB(const C,M,Y,K: Double; VAR R,G,B: Double);
    procedure RGBToCMYK(const R,G,B: Double; VAR C,M,Y,K: Double);
  published
    property OnTimer : TNotifyEvent read FOnTimer write FOnTimer;
    property Enabled : Boolean read FEnabled Write SetEnabled;
    property Interval : Cardinal read FInterval Write SetInterval;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHColorPicker]);
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHColorPicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FEnabled := False;
  FInterval := 100;

  FPickTimer := TPickTimer.Create(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHColorPicker.Destroy;
begin
  FPickTimer.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TPickTimer.Create(AOwner: TCHColorPicker);
begin
  inherited Create;
  FOwner := AOwner;

  FTimer := TTimer.Create(AOwner);
  FTimer.Interval := FOwner.FInterval;
  FTimer.Enabled := FOwner.FEnabled;
  FTimer.OnTimer := FOwner.GetColor;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TPickTimer.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHColorPicker.GetColor(Sender: TObject);
var
  CurPos : TPoint;
begin 
  GetCursorPos(CurPos);
  FColor := DesktopColor(CurPos.x, CurPos.y);
  ConvertColor(FColor);

  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHColorPicker.DesktopColor(const x, y: integer): TColor;
var
  hDesktop: THandle;
  DC: HDC;
begin
  hDesktop := GetDesktopWindow;
  DC := GetWindowDC(hDesktop);
  try
    Result := GetPixel(DC, x, y);
  finally
    ReleaseDC(hDesktop, DC);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHColorPicker.ConvertColor(const Color: TColor);
var
  R,G,B : Byte;
  CC,MM,YY,KK : Integer;
  H,S,V,L : Double;
  RGBTriple : TRGBTriple;
begin
  R := GetRValue(Color);
  G := GetGValue(Color);
  B := GetBValue(Color);

  // HTML
  FColorHTML := ColortoHtml(Color);

  // HEX
  FColorHex := IntToHex(B, 2) + IntToHex(G, 2) +IntToHex(R, 2); 

  // STRING
  FColorString := ColorToString(Color);

  // RGB
  FColorRGB_R := R;
  FColorRGB_B := B;
  FColorRGB_G := G;

  // HSV
  RGBToHSV(GetRValue(Color) / 255, GetGValue(Color) / 255, GetBValue(Color) / 255, H, S, V);
  FColorHSV_H := Trunc(255 * H / 360);
  FColorHSV_S := Trunc(255 * S);
  FColorHSV_V := Trunc(255 * V);

  // HLS
  RGBToHLS(GetRValue(Color) / 255, GetGValue(Color) / 255, GetBValue(Color) / 255, H, L, S);
  FColorHLS_H := Trunc(255 * H / 360);
  FColorHLS_L := Trunc(255 * L);
  FColorHLS_S := Trunc(255 * S);

  // CMYK
  RGBTripleToCMYK(RGBTriple, CC,MM,YY,KK);
  FColorCMYK_C := CC;
  FColorCMYK_M := MM;
  FColorCMYK_Y := YY;
  FColorCMYK_K := KK;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHColorPicker.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    FPickTimer.FTimer.Enabled := FEnabled;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHColorPicker.SetInterval(const Value: Cardinal);
begin
  if FInterval <> Value then
  begin
    FInterval := Value;
    if FInterval = 0 then
      FInterval := 1;

    FPickTimer.FTimer.Interval := FInterval;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHColorPicker.ColorToHtml(cColor: TColor): string;
var
  _cColor : Integer;
begin
  _cColor := Graphics.ColorToRGB(cColor);
  Result :=  IntToHex(_cColor and $FF, 2) +
             IntToHex(_cColor shr 8 and $FF, 2) +
             IntToHex(_cColor shr 16 and $FF, 2);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHColorPicker.CMYKToRGBTriple(const C, M, Y,
  K: Integer): TRGBTriple;
begin
  with RESULT do
    begin
      rgbtRed   := 255 - (C + K);
      rgbtGreen := 255 - (M + K);
      rgbtBlue  := 255 - (Y + K)
    end
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHColorPicker.CMYToRGBTriple(const C, M, Y: Integer): TRGBTriple;
begin
  with RESULT do
    begin
      rgbtRed   := 255 - C;
      rgbtGreen := 255 - M;
      rgbtBlue  := 255 - Y
    end
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHColorPicker.HLSToRGBTriple(const H, L, S: Integer): TRGBTriple;
var
  R,G,B: Double;
begin
  HLStoRGB(H, L/255, S/255, R,G,B);
  RESULT := ColorToRGBTriple( RGB(ROUND(255*R), ROUND(255*G), ROUND(255*B)) )

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHColorPicker.HSVToRGBTriple(const H, S, V: Integer): TRGBTriple;
const
  divisor:  INTEGER = 255*60;
var
  f, hTemp, p, q, t, VS :  Integer;
begin
  if S = 0 then
    RESULT := RGBtoRGBTriple(V, V, V)
  else
  begin
    if H = 360 then
      hTemp := 0
    else
      hTemp := H;

    f := hTemp mod 60;
    hTemp := hTemp div 60;

    VS := V * S;
    p := V - VS div 255;
    q := V - (VS*f) div divisor;
    t := V - (VS*(60 - f)) div divisor;

    case hTemp of
      0:   RESULT := RGBtoRGBTriple(V, t, p);
      1:   RESULT := RGBtoRGBTriple(q, V, p);
      2:   RESULT := RGBtoRGBTriple(p, V, t);
      3:   RESULT := RGBtoRGBTriple(p, q, V);
      4:   RESULT := RGBtoRGBTriple(t, p, V);
      5:   RESULT := RGBtoRGBTriple(V, p, q);
      else
        RESULT := RGBtoRGBTriple(0,0,0)
    end
  end
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHColorPicker.RGBTripleToCMY(const RGB: TRGBTriple; var C, M, Y: INTEGER);
begin
  with RGB do
  begin
    C := 255 - rgbtRed;
    M := 255 - rgbtGreen;
    Y := 255 - rgbtBlue
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHColorPicker.RGBTripleToCMYK(const RGB: TRGBTriple; var C, M, Y, K: Integer);
begin
  RGBTripleToCMY(RGB, C,M,Y);
  K := MinIntValue([C, M, Y]);
  C := C - K;
  M := M - K;
  Y := Y - K
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHColorPicker.RGBTripleToHLS(const RGBTriple: TRGBTriple; var H, L, S: INTEGER);
var
  Hue, Lightness, Saturation : Double;
begin
  with RGBTriple do
    RGBToHLS(rgbtRed/255, rgbtGreen/255, rgbtBlue/255, Hue, Lightness, Saturation);

  if IsNan(Hue) then
    H := 0
  else
    H := ROUND(Hue);

  L := ROUND(255*Lightness);
  S := ROUND(255*Saturation);

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHColorPicker.RGBTripleToHSV(const RGBTriple: TRGBTriple; var H, S, V: INTEGER);
var
  Delta, Min : Integer;
begin
  with RGBTriple do
  begin
    Min := MinIntValue( [rgbtRed, rgbtGreen, rgbtBlue] );
    V   := MaxIntValue( [rgbtRed, rgbtGreen, rgbtBlue] )
  end;

  Delta := V - Min;

  if V =  0 then
    S := 0
  else
    S := MulDiv(Delta, 255, V);

  if S  = 0 then
    H := 0
  else
  begin
    with RGBTriple do
    begin
      if rgbtRed = V then
        H := MulDiv(rgbtGreen - rgbtBlue, 60, Delta)
      else if rgbtGreen = V then
        H := 120 + MulDiv(rgbtBlue-rgbtRed, 60, Delta)
      else if rgbtBlue = V then
        H := 240 + MulDiv(rgbtRed-rgbtGreen, 60, Delta);
    end;

    if H < 0 then
      H := H + 360;

  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHColorPicker.CMYKtoRGB(const C, M, Y, K: Double; var R, G, B: Double);
begin
  R := 1.0 - (C + K);
  G := 1.0 - (M + K);
  B := 1.0 - (Y + K)
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHColorPicker.CMYtoRGB(const C, M, Y: Double; var R, G, B: Double);
begin
  R := 1.0 - C;
  G := 1.0 - M;
  B := 1.0 - Y
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHColorPicker.HLStoRGB(const H, L, S: Double; var R, G, B: Double);
var
  m1, m2:  Double;

  function Value(const n1,n2:  Double;  hue:  Double):  Double;
  begin
    if hue > 360.0 then
      hue := hue - 360.0
    else
      if hue < 0.0 then
        hue := hue + 360.0;

    if hue < 60.0 then
      RESULT := n1 + (n2 - n1)*hue / 60.0
    else
      if hue < 180 then
        RESULT := n2
      else if hue < 240.0 then
        RESULT := n1 + (n2-n1)*(240.0 - hue) / 60.0
      else
        RESULT := n1
  end;

begin
  if L <= 0.5 then
    m2 := L * (1 + S)
  else
    m2 := L + S - L*S;

  m1 := 2.0 * L - m2;

  if S = 0.0 then
  begin
    if IsNAN(H) then
    begin
      R := L;
      G := L;
      B := L
    end
    else
      raise EColorError.Create('HLStoRGB:  S = 0 and H has a value');
  end
  else
  begin
    R := Value(m1, m2, H + 120.0);
    G := Value(m1, m2, H);
    B := Value(m1, m2, H - 120.0)
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHColorPicker.HSVtoRGB(const H, S, V: Double; var R, G, B: Double);
var
  f, hTemp, p, q, t : Double;
  i : Integer;
begin
  if S = 0.0 then
  begin
    if IsNaN(H) then
    begin
      R := V;
      G := V;
      B := V
    end
    else
      raise EColorError.Create('HSVtoRGB:  S = 0 and H has a value');
  end
  else
  begin
    if H = 360.0 then
      hTemp := 0.0
    else
      hTemp := H;

    hTemp := hTemp / 60;
    i := TRUNC(hTemp);
    f := hTemp - i;

    p := V * (1.0 - S);
    q := V * (1.0 - (S * f));
    t := V * (1.0 - (S * (1.0 - f)));

    case i of
      0:  begin R := V;  G := t;  B := p  end;
      1:  begin R := q;  G := V;  B := p  end;
      2:  begin R := p;  G := V;  B := t  end;
      3:  begin R := p;  G := q;  B := V  end;
      4:  begin R := t;  G := p;  B := V  end;
      5:  begin R := V;  G := p;  B := q  end;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHColorPicker.RGBToCMY(const R, G, B: Double; var C, M, Y: Double);
begin
  C := 1.0 - R;
  M := 1.0 - G;
  Y := 1.0 - B
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHColorPicker.RGBToCMYK(const R, G, B: Double; var C, M, Y, K: Double);
begin
  RGBtoCMY(R,G,B, C,M,Y);
  K := MinValue([C, M, Y]);
  C := C - K;
  M := M - K;
  Y := Y - K
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHColorPicker.RGBToHLS(const R, G, B: Double; var H, L, S: Double);
var
  Delta, Max, Min:  Double;
begin
  Max := MaxValue( [R, G, B] );
  Min := MinValue( [R, G, B] );

  L := (Max + Min) / 2.0;

  if Max = Min then
  begin
    S := 0.0;
    H := 0;
  end
  else
  begin
    Delta := Max - Min;

    if L <= 0.5 then
      S := Delta / (Max + Min)
    else
      S := Delta / (2.0 - (Max + Min));

    if R = Max then
      H := (60.0*(G - B)) / Delta
    else
      if G = Max then
        H := 120.0 + (60.0*(B - R)) / Delta
      else if B = Max then
        H := 240.0 + (60.0*(R - G)) / Delta;

    if H < 0 then
      H := H + 360.0;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHColorPicker.RGBToHSV(const R, G, B: Double; var H, S, V: Double);
var
  Delta, Min:  Double;
BEGIN
  Min := MinValue( [R, G, B] );
  V   := MaxValue( [R, G, B] );

  Delta := V - Min;

  if V =  0.0 then
    S := 0
  else
    S := Delta / V;

  if S  = 0.0 then
    H := 0
  else
  begin
    if R = V then
      H := 60.0 * (G - B) / Delta
    else if G = V then
      H := 120.0 + 60.0 * (B - R) / Delta
    else if B = V then
      H := 240.0 + 60.0 * (R - G) / Delta;

    if H < 0.0 then
      H := H + 360.0
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHColorPicker.ColorToRGBTriple(const Color: TColor): TRGBTriple;
begin
  with RESULT do
  begin
    rgbtRed   := GetRValue(Color);
    rgbtGreen := GetGValue(Color);
    rgbtBlue  := GetBValue(Color)
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHColorPicker.RGBtoRGBTriple(const red, green, blue: Byte): TRGBTriple;
begin
  with RESULT do
  begin
    rgbtRed   := red;
    rgbtGreen := green;
    rgbtBlue  := blue
  end;
end;

end.
