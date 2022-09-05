{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2020 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnGraphUtils;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�����ͼ����̿ⵥԪ
* ��Ԫ���ߣ�CnPack������
* ��    ע��
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2002.10.20 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Graphics, Math, Controls;


//==============================================================================
// ��չ����ɫ��ʽת������
//==============================================================================

var
  HSLRange: Integer = 240;

//------------------------------------------------------------------------------
// HSL ��ɫ�� RGB ɫת������
//------------------------------------------------------------------------------

function HSLToRGB(H, S, L: Double): TColor;
{* HSL ��ɫת��Ϊ RGB ��ɫ
 |<PRE>
   H, S, L: Double   - �ֱ�Ϊɫ�������Ͷȡ����ȷ�����Ϊ"0"��"1"֮���С��
   Result: TColor    - ����RGB��ɫֵ
 |</PRE>}
function HSLRangeToRGB(H, S, L: Integer): TColor;
{* HSL ��ɫת��Ϊ RGB ��ɫ
 |<PRE>
   H, S, L: Integer  - �ֱ�Ϊɫ�������Ͷȡ����ȷ�����0..240
   Result: TColor    - ����RGB��ɫֵ
 |</PRE>}
procedure RGBToHSL(Color: TColor; out H, S, L: Double);
{* RGB ��ɫת��Ϊ HSL ��ɫ
 |<PRE>
  Color: TColor         - RGB��ɫֵ
  H, S, L: Integer      - ����ֱ�Ϊɫ�������Ͷȡ����ȷ�����Ϊ"0"��"1"֮���С��
 |</PRE>}
procedure RGBToHSLRange(Color: TColor; out H, S, L: Integer);
{* RGB ��ɫת��Ϊ HSL ��ɫ
 |<PRE>
   Color: TColor        - RGB��ɫֵ
   H, S, L: Integer     - ����ֱ�Ϊɫ�������Ͷȡ����ȷ�����0..240
 |</PRE>}

function ChangeHue(Color: TColor; Hue: Double): TColor;
{* �滻��ɫ�е�ɫ��ֵ�������µ���ɫ}
function ChangeSaturation(Color: TColor; Saturation: Double): TColor;
{* �滻��ɫ�еı��Ͷ�ֵ�������µ���ɫ}
function ChangeLighteness(Color: TColor; Lighteness: Double): TColor;
{* �滻��ɫ�е�����ֵ�������µ���ɫ}

function AdjustHue(Color: TColor; Added: Double): TColor;
{* ������ɫ�е�ɫ��ֵ�������µ���ɫ}
function AdjustSaturation(Color: TColor; Added: Double): TColor;
{* ������ɫ�еı��Ͷ�ֵ�������µ���ɫ}
function AdjustLighteness(Color: TColor; Added: Double): TColor;
{* ������ɫ�е�����ֵ�������µ���ɫ}

//------------------------------------------------------------------------------
// CMY ��ɫ�� RGB ɫת������
//------------------------------------------------------------------------------

function CMYToRGB(const C, M, Y: Byte): TColor;
{* CMY��ɫת��ΪRGB��ɫ
 |<PRE>
  C, M, Y: Byte         - �ֱ�ΪCyan�ࡢMagentaƷ�졢Yellow�Ʒ�����0..255
  Result: TColor        - ����RGB��ɫֵ
 |</PRE>}
procedure RGBToCMY(const RGB: TColor; out C, M, Y: Byte);
{* RGB��ɫת��ΪCMY��ɫ
 |<PRE>
 |<BR> Color: TColor������RGB��ɫֵ
 |<BR> C, M, Y: Byte����������ֱ�ΪCyan�ࡢMagentaƷ�졢Yellow�Ʒ�����0..255
 |</PRE>}

//------------------------------------------------------------------------------
// CMYK ��ɫ�� RGB ɫת������
//------------------------------------------------------------------------------

function CMYKToRGB(const C, M, Y, K: Byte): TColor;
{* CMYK��ɫת��ΪRGB��ɫ
 |<PRE>
   C, M, Y, K: Byte     - �ֱ�ΪCyan�ࡢMagentaƷ�졢Yellow�ơ�Black�ڷ�����0..255
   Result: TColor       - ����RGB��ɫֵ
 |</PRE>}
procedure RGBToCMYK(const RGB: TColor; out C, M, Y, K: Byte);
{* RGB��ɫת��ΪCMY��ɫ
 |<PRE>
   Color: TColor        - RGB��ɫֵ
   C, M, Y, K: Byte     - ����ֱ�ΪCyan�ࡢMagentaƷ�졢Yellow�ơ�Black�ڷ�����0..255
 |</PRE>}

//==============================================================================
// ��ǿ����ɫ������
//==============================================================================

function Gray(Intensity: Byte): TColor;
{* ����һ���Ҷ� RGB ��ɫֵ}
function Intensity(Color: TColor): Byte;
{* ����RGB��ɫֵ�ĻҶ�ֵ}
function RandomColor: TColor;
{* ����һ����� RGB ��ɫֵ}
procedure DeRGB(Color: TColor; var r, g, b: Byte);
{* �� Color �ֽ�Ϊ r��g��b ��ɫ����}

//==============================================================================
// ��չ��λͼ������
//==============================================================================

function CreateEmptyBmp24(Width, Height: Integer; Color: TColor): TBitmap;
{* ����һ���� Color Ϊ����ɫ��ָ����С�� 24λλͼ }

function DrawBmpToIcon(Bmp: TBitmap; Icon: TIcon): Boolean;
{* �� Bitmap �����ݷŵ� Icon ��}

implementation

//==============================================================================
// ��չ����ɫ��ʽת������
//==============================================================================

//------------------------------------------------------------------------------
// HSL ��ɫ�� RGB ɫת������
// �㷨��Դ��
// http:/www.r2m.com/win-developer-faq/graphics/8.html
// Grahame Marsh 12 October 1997
//------------------------------------------------------------------------------

// HSL��ɫת��Ϊ RGB ɫ
function HSLToRGB(H, S, L: Double): TColor;
var
  M1, M2: Double;

  procedure CheckInput(var V: Double);
  begin
    if V < 0 then V := 0;
    if V > 1 then V := 1;
  end;

  function HueToColourValue(Hue: Double): Byte;
  var
    V: Double;
  begin
    if Hue < 0 then
      Hue := Hue + 1
    else if Hue > 1 then
      Hue := Hue - 1;
    if 6 * Hue < 1 then
      V := M1 + (M2 - M1) * Hue * 6
    else if 2 * Hue < 1 then
      V := M2
    else if 3 * Hue < 2 then
      V := M1 + (M2 - M1) * (2 / 3 - Hue) * 6
    else
      V := M1;
    Result := Round(255 * V)
  end;
var
  r, g, b: Byte;
begin
  H := H - Floor(H);                   // ��֤ɫ���� 0..1 ֮��
  CheckInput(S);
  CheckInput(L);
  if S = 0 then
  begin
    r := Round(255 * L);
    g := r;
    b := r
  end else
  begin
    if L <= 0.5 then
      M2 := L * (1 + S)
    else
      M2 := L + S - L * S;
    M1 := 2 * L - M2;
    r := HueToColourValue(H + 1 / 3);
    g := HueToColourValue(H);
    b := HueToColourValue(H - 1 / 3)
  end;
  Result := RGB(r, g, b);
end;

// HSL ��ɫ��Χת��Ϊ RGB ɫ
function HSLRangeToRGB(H, S, L: Integer): TColor;
begin
  Assert(HSLRange > 1);
  Result := HSLToRGB(H / (HSLRange - 1), S / HSLRange, L / HSLRange)
end;

// RGB ��ɫתΪ HSL ɫ
procedure RGBToHSL(Color: TColor; out H, S, L: Double);
var
  r, g, b, D, Cmax, Cmin: Double;
begin
  Color := ColorToRGB(Color);
  r := GetRValue(Color) / 255;
  g := GetGValue(Color) / 255;
  b := GetBValue(Color) / 255;
  Cmax := Max(r, Max(g, b));
  Cmin := Min(r, Min(g, b));
  L := (Cmax + Cmin) / 2;
  if Cmax = Cmin then
  begin
    H := 0;
    S := 0
  end else
  begin
    D := Cmax - Cmin;
    if L < 0.5 then
      S := D / (Cmax + Cmin)
    else
      S := D / (2 - Cmax - Cmin);
    if r = Cmax then
      H := (g - b) / D
    else if g = Cmax then
      H := 2 + (b - r) / D
    else
      H := 4 + (r - g) / D;
    H := H / 6;
    if H < 0 then
      H := H + 1
  end
end;

// RGB ��ɫתΪ HSL ɫ��Χ
procedure RGBToHSLRange(Color: TColor; out H, S, L: Integer);
var
  Hd, Sd, Ld: Double;
begin
  RGBToHSL(Color, Hd, Sd, Ld);
  H := Round(Hd * (HSLRange - 1));
  S := Round(Sd * HSLRange);
  L := Round(Ld * HSLRange);
end;

// �滻��ɫ�е�ɫ��ֵ�������µ���ɫ
function ChangeHue(Color: TColor; Hue: Double): TColor;
var
  H, S, L: Double;
begin
  RGBToHSL(Color, H, S, L);
  Result := HSLToRGB(Hue, S, L);
end;

// �滻��ɫ�еı��Ͷ�ֵ�������µ���ɫ
function ChangeSaturation(Color: TColor; Saturation: Double): TColor;
var
  H, S, L: Double;
begin
  RGBToHSL(Color, H, S, L);
  Result := HSLToRGB(H, Saturation, L);
end;

// �滻��ɫ�е�����ֵ�������µ���ɫ
function ChangeLighteness(Color: TColor; Lighteness: Double): TColor;
var
  H, S, L: Double;
begin
  RGBToHSL(Color, H, S, L);
  Result := HSLToRGB(H, S, Lighteness);
end;

// ������ɫ�е�ɫ��ֵ�������µ���ɫ
function AdjustHue(Color: TColor; Added: Double): TColor;
var
  H, S, L: Double;
begin
  RGBToHSL(Color, H, S, L);
  Result := HSLToRGB(H + Added, S, L);
end;

// ������ɫ�еı��Ͷ�ֵ�������µ���ɫ
function AdjustSaturation(Color: TColor; Added: Double): TColor;
var
  H, S, L: Double;
begin
  RGBToHSL(Color, H, S, L);
  Result := HSLToRGB(H, S + Added, L);
end;

// ������ɫ�е�����ֵ�������µ���ɫ
function AdjustLighteness(Color: TColor; Added: Double): TColor;
var
  H, S, L: Double;
begin
  RGBToHSL(Color, H, S, L);
  Result := HSLToRGB(H, S, L + Added);
end;

//------------------------------------------------------------------------------
// CMY ��ɫ�� RGB ɫת������
// �㷨�ṩ��CnPack������ ����
//------------------------------------------------------------------------------

// CMY��ɫת��ΪRGB
function CMYToRGB(const C, M, Y: Byte): TColor;
var
  r, g, b: Byte;
begin
  r := 255 - C;
  g := 255 - M;
  b := 255 - Y;
  Result := RGB(r, g, b);
end;

// RGB��ɫת��ΪCMY
procedure RGBToCMY(const RGB: TColor; out C, M, Y: Byte);
var
  r, g, b: Byte;
begin
  DeRGB(RGB, r, g, b);
  C := 255 - r;
  M := 255 - g;
  Y := 255 - b;
end;

//------------------------------------------------------------------------------
// CMYK ��ɫ�� RGB ɫת������
// �㷨�ṩ��CnPack������ ����
//------------------------------------------------------------------------------

// CMYK ��ɫת��Ϊ RGB
function CMYKtoRGB(const C, M, Y, K: Byte): TColor;
var
  r, g, b: Byte;
begin
  r := 255 - (C + K);
  g := 255 - (M + K);
  b := 255 - (Y + K);
  Result := RGB(r, g, b);
end;

// RGB ��ɫת��Ϊ CMYK
procedure RGBToCMYK(const RGB: TColor; out C, M, Y, K: Byte);
begin
  RGBToCMY(RGB, C, M, Y);
  K := MinIntValue([C, M, Y]);
  C := C - K;
  M := M - K;
  Y := Y - K;
end;

//==============================================================================
// ��ǿ����ɫ������
//==============================================================================

// �����Ҷ���ɫ
function Gray(Intensity: Byte): TColor;
begin
  Result := Intensity shl 16 + Intensity shl 8 + Intensity;
end;

// ������ɫ����ֵ
// �㷨��Դ��Graphic32
// �㷨�޸ģ��ܾ���
function Intensity(Color: TColor): Byte;
asm
// ����:  RGB --> EAX
// ���:  (R * 61 + G * 174 + B * 20) / 256 --> AL
        MOV     ECX,EAX
        AND     EAX,$00FF00FF      // EAX <-   0 B 0 R
        IMUL    EAX,$0014003D
        AND     ECX,$0000FF00      // ECX <-   0 0 G 0
        IMUL    ECX,$0000AE00
        MOV     EDX,EAX
        SHR     ECX,8
        SHR     EDX,16
        ADD     EAX,ECX
        ADD     EAX,EDX
        SHR     EAX,8
end;

// ���������ɫ
function RandomColor: TColor;
begin
  Result := HSLToRGB(Random, 0.75 + Random * 0.25, 0.3 + Random * 0.25);
end;

// ȡ��ɫRGB����
procedure DeRGB(Color: TColor; var r, g, b: Byte);
begin
  Color := ColorToRGB(Color);
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
end;

//==============================================================================
// ��չ��λͼ������
//==============================================================================

// ����һ���� Color Ϊ����ɫ��ָ����С�� 24λλͼ
function CreateEmptyBmp24(Width, Height: Integer; Color: TColor): TBitmap;
type
  TRGBArray = array[0..65535] of TRGBTriple;
var
  r, g, b: Byte;
  x, y: Integer;
  P: ^TRGBArray;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.Width := Width;
  Result.Height := Height;
  DeRGB(Color, r, g, b);
  for y := 0 to Height - 1 do
  begin
    P := Result.ScanLine[y];
    for x := 0 to Width - 1 do
    begin
      with P[x] do
      begin
        rgbtBlue := b;
        rgbtGreen := g;
        rgbtRed := r;
      end;
    end;
  end;
end;

// �� Bitmap �����ݷŵ� Icon ��
function DrawBmpToIcon(Bmp: TBitmap; Icon: TIcon): Boolean;
var
  ImageList: TImageList;
begin
  Result := False;
  if (Bmp = nil) or (Icon = nil) or Bmp.Empty then
    Exit;

  ImageList := TImageList.CreateSize(Bmp.Width, Bmp.Height);
  try
    ImageList.AddMasked(Bmp, Bmp.TransparentColor);
    ImageList.GetIcon(0, Icon);
    Result := True;
  finally
    ImageList.Free;
  end;
end;

end.
