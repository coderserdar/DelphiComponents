unit uImgTools;

interface

uses
  Windows, Graphics, Math, SysUtils, CommCtrl;

procedure Grayscale(const Bitmap: TBitmap);
function GetRGBColor(Value: TColor): DWORD;

implementation

function Lighter(B: Byte): Byte;
begin
  Result := Min(B + 20, 255);
end;

procedure Grayscale(const Bitmap: TBitmap);
type
  PPixel = ^TPixel;
  TPixel = LongWord;
var
  Backup: Integer;
  Y: Integer;
  Pixel: PPixel;
  R, G, B: Byte;
begin
  Bitmap.PixelFormat := pf32Bit;

  for Y := 0 to (Bitmap.Height - 1) do
  begin
    Pixel := Bitmap.ScanLine[Y];
    for Backup := 0 to (Bitmap.Width - 1) do
    begin
      R := Lighter(Pixel^ and $FF0000 shr 16);
      G := Lighter(Pixel^ and $00FF00 shr 8);
      B := Lighter(Pixel^ and $0000FF);

      Pixel^ := ((77 * R + 150 * G + 29 * B) shr 8) * $010101;

      Inc(Pixel);
    end;
  end;
end;

function GetRGBColor(Value: TColor): DWORD;
begin
  Result := ColorToRGB(Value);
  case Result of
    clNone:
      Result := CLR_NONE;
    clDefault:
      Result := CLR_DEFAULT;
  end;
end;

end.
