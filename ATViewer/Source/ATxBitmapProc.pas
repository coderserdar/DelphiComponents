{
This unit was originally named GraphFlip.pas, it's renamed just to avoid
possible name conflicts in project. Originally written by Colin Wilson:
http://www.wilsonc.demon.co.uk
http://www.wilsonc.demon.co.uk/d10lowlevel.htm
}

(*======================================================================*
 | GraphFlip unit                                                       |
 |                                                                      |
 | Simple functions to flip and rotate 24-bit bitmaps                   |
 |                                                                      |
 | Not that these functions *create* copies of the bitmap which must be |
 | freed.                                                               |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2002  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      29/08/2002  CPWW  Original                                  |
 | 10.0     08/03/2006  CPWW  Tidied up for BDS 2006                    |
 *======================================================================*)

unit ATxBitmapProc;

interface

uses Windows, Classes, Sysutils, Graphics;

function RotateBitmap270 (const bitmap : TBitmap) : TBitmap;
function RotateBitmap90 (const bitmap : TBitmap) : TBitmap;
function ConvertToGrayscale (const bitmap : TBitmap; TransparentColor : TColor = clNone) : TBitmap;
function ConvertToNegative (const bitmap : TBitmap) : TBitmap;

implementation

(*----------------------------------------------------------------------*
 | function BytesPerScanLine : LongInt                                  |
 |                                                                      |
 | Returns the bytes required per scanline                              |
 |                                                                      |
 | Parameters:                                                          |
 |   PixelsPerScanline : LongInt     The width of the bitmap in pixels  |
 |   BitsPerPixel                    No. of bits per pixel - eg. 24     |
 |   Alignment                       The bitmap byte alignment - eg. 32 |
 *----------------------------------------------------------------------*)
function BytesPerScanline(PixelsPerScanline, BitsPerPixel, Alignment: Longint): Longint;
begin
  Dec(Alignment);
  Result := ((PixelsPerScanline * BitsPerPixel) + Alignment) and not Alignment;
  Result := Result div 8;
end;

(*----------------------------------------------------------------------*
 | function RotateBitmap270                                             |
 |                                                                      |
 | Rotate a bitmap clockwise through 270 degrees                        |
 |                                                                      |
 | Parameters:                                                          |
 |   bitmap                          The bitmap to rotate               |
 |                                                                      |
 | The function creates a rotated copy of the bitmap.                   |
 *----------------------------------------------------------------------*)
function RotateBitmap270 (const bitmap : TBitmap) : TBitmap;
var
  x, y : Integer;
  ps, ps1, pr, pr1 : PRGBTriple;
  bpss, bpsr : Integer;

begin
  Assert (bitmap.PixelFormat = pf24Bit, 'Invalid pixel format');

  result := TBitmap.Create;
  try
    result.PixelFormat := bitmap.PixelFormat;
    result.Height := bitmap.Width;
    result.Width := bitmap.Height;

    ps1 := bitmap.ScanLine [0];
    pr1 := result.ScanLine [bitmap.Width - 1];

    bpss := BytesPerScanLine (bitmap.Width, 24, 32);
    bpsr := BytesPerScanLine (result.Width, 24, 32);

    for y := 0 to bitmap.Height - 1 do
    begin
      ps := PRGBTriple (PAnsiChar (ps1) - bpss * y);

      for x := 0 to bitmap.Width - 1 do
      begin
        pr := PRGBTriple (PAnsiChar (pr1) + bpsr * x);
        Inc (pr, y);
        pr^ := ps^;
        Inc (ps)
      end
    end;
    GDIFlush
  except
    result.Free;
    raise
  end
end;

(*----------------------------------------------------------------------*
 | function RotateBitmap90                                              |
 |                                                                      |
 | Rotate a bitmap clockwise through 90 degrees                         |
 |                                                                      |
 | Parameters:                                                          |
 |   bitmap                          The bitmap to rotate               |
 |                                                                      |
 | The function creates a rotated copy of the bitmap.                   |
 *----------------------------------------------------------------------*)
function RotateBitmap90 (const bitmap : TBitmap) : TBitmap;
var
  x, y : Integer;
  ps, ps1, pr, pr1 : PRGBTriple;
  bpss, bpsr : Integer;

begin
  Assert (bitmap.PixelFormat = pf24Bit, 'Invalid pixel format');

  result := TBitmap.Create;
  try
    result.PixelFormat := bitmap.PixelFormat;
    result.Height := bitmap.Width;
    result.Width := bitmap.Height;

    ps1 := bitmap.ScanLine [bitmap.Height - 1];
    pr1 := result.ScanLine [0];

    bpss := BytesPerScanLine (bitmap.Width, 24, 32);
    bpsr := BytesPerScanLine (result.Width, 24, 32);

    for y := 0 to bitmap.Height - 1 do
    begin
      ps := PRGBTriple (PAnsiChar (ps1) + bpss * y);

      for x := 0 to Bitmap.Width - 1 do
      begin
        pr := PRGBTriple (PAnsiChar (pr1) - bpsr * x);
        Inc (pr, y);
        pr^ := ps^;
        Inc (ps)
      end
    end;
    GDIFlush
  except
    result.Free;
    raise
  end;
end;

(*----------------------------------------------------------------------*
 | function ConvertToGrayscale                                          |
 |                                                                      |
 | Convert a bitmap to it's greyscale equivalent                        |
 |                                                                      |
 | Parameters:                                                          |
 |   bitmap                          The bitmap to convert              |
 |                                                                      |
 | The function creates a greyscale copy of the bitmap.                 |
 *----------------------------------------------------------------------*)
function ConvertToGrayscale (const bitmap : TBitmap; TransparentColor : TColor) : TBitmap;
var
  x, y : Integer;
  ps, ps1, pr, pr1 : PRGBTriple;
  bps : Integer;
  n : Integer;
  transparent : boolean;
  transparentTriple : TRGBTriple;
  rgb :DWORD;

begin
  Assert (bitmap.PixelFormat = pf24Bit, 'Invalid pixel format');

  transparent := TransparentColor <> clNone;
  if transparent then
  begin
    rgb := ColorToRGB (TransparentColor);
    transparentTriple.rgbtBlue := GetBValue (rgb);
    transparentTriple.rgbtGreen := GetGValue (rgb);
    transparentTriple.rgbtRed := GetRValue (rgb)
  end;

  result := TBitmap.Create;
  try
    result.PixelFormat := bitmap.PixelFormat;
    result.Height := bitmap.Height;
    result.Width := bitmap.Width;

    ps1 := bitmap.ScanLine [0];
    pr1 := result.ScanLine [0];

    bps := BytesPerScanLine (bitmap.Width, 24, 32);

    for y := 0 to bitmap.Height - 1 do
    begin
      ps := PRGBTriple (PAnsiChar (ps1) - bps * y);
      pr := PRGBTriple (PAnsiChar (pr1) - bps * y);

      for x := 0 to Bitmap.Width - 1 do
      begin
        if not Transparent
          or (ps^.rgbtBlue <> transparentTriple.rgbtBlue)
          or (ps^.rgbtGreen <> transparentTriple.rgbtGreen)
          or (ps^.rgbtRed <> transparentTriple.rgbtRed) then
        begin
          n := ((DWORD (ps^.rgbtBlue) * 28) +            // 11% Blue
                (DWORD (ps^.rgbtRed) * 77) +             // 30% Red
                (DWORD (ps^.rgbtGreen) * 151)) div 256;  // 59% Green

          pr^.rgbtBlue := n;
          pr^.rgbtGreen := n;
          pr^.rgbtRed := n;
        end
        else
          pr^ := ps^;

        Inc (pr);
        Inc (ps)
      end
    end;
    GDIFlush
  except
    result.Free;
    raise
  end;
end;

(*----------------------------------------------------------------------*
 | function ConvertToNegative                                           |
 |                                                                      |
 | Convert a bitmap to it's negative equivalent                         |
 |                                                                      |
 | Parameters:                                                          |
 |   bitmap                          The bitmap to convert              |
 |                                                                      |
 | The function creates a negative copy of the bitmap.                  |
 *----------------------------------------------------------------------*)
function ConvertToNegative (const bitmap : TBitmap) : TBitmap;
var
  x, y : Integer;
  ps, ps1, pr, pr1 : PRGBTriple;
  bps : Integer;
begin
  Assert (bitmap.PixelFormat = pf24Bit, 'Invalid pixel format');

  result := TBitmap.Create;
  try
    result.PixelFormat := bitmap.PixelFormat;
    result.Height := bitmap.Height;
    result.Width := bitmap.Width;

    ps1 := bitmap.ScanLine [0];
    pr1 := result.ScanLine [0];

    bps := BytesPerScanLine (bitmap.Width, 24, 32);

    for y := 0 to bitmap.Height - 1 do
    begin
      ps := PRGBTriple (PAnsiChar (ps1) - bps * y);
      pr := PRGBTriple (PAnsiChar (pr1) - bps * y);

      for x := 0 to Bitmap.Width - 1 do
      begin
        pr^.rgbtBlue := 255 - ps^.rgbtBlue;
        pr^.rgbtGreen := 255 - ps^.rgbtGreen;
        pr^.rgbtRed := 255 - ps^.rgbtRed;

        Inc (pr);
        Inc (ps)
      end
    end;
    GDIFlush
  except
    result.Free;
    raise
  end;
end;

end.
