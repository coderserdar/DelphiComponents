{   Unit VCL.cyGraphics

    Description:
    Unit with image functions

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

unit VCL.cyImage;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Windows, Graphics, Classes, SysUtils, Math, Jpeg, {$IFDEF UNICODE} pngimage, {$ENDIF} VCL.cyGraphics;

type
  TRGBQuadArray = Array[0..0] of TRGBQuad;
  pRGBQuadArray = ^TRGBQuadArray;

  function GraphicCreate(GraphicClass: TGraphicClass): TGraphic;
  function BitmapCreate(BmpWidth: Integer; BmpHeight: Integer; BgColor: TColor; PixelFormat: TPixelFormat): TBitmap;

  function BitmapsCompare(Bmp1: TBitmap; Bmp2: TBitmap; FirstCol, LastCol, FirstRow, LastRow: Integer): Integer;
  procedure BitmapSetPercentBrightness(Bmp: TBitmap; IncPercent: Integer; RefreshBmp: Boolean);
  procedure BitmapSetPixelsBrightness(Bmp: TBitmap; IncPixels: Integer; RefreshBmp: Boolean);
  procedure BitmapSetPercentContrast(Bmp: TBitmap; IncPercent: Integer; RefreshBmp: Boolean);
  procedure BitmapSetPixelsContrast(Bmp: TBitmap; IncPixels: Integer; RefreshBmp: Boolean);
  procedure BitmapCrop(Bmp: TBitmap; CropRect: TRect);
  procedure BitmapNegative(Bmp: TBitmap; RefreshBmp: Boolean);
  procedure BitmapModifyRGB(Bmp: TBitmap; IncRed: Integer; IncGreen: Integer; IncBlue: Integer; RefreshBmp: Boolean);

  procedure BitmapReplaceColor(Bmp: TBitmap; OldColor: TColor; NewColor: TColor; RangeRed, RangeGreen, RangeBlue: Word; SingleDestinationColor: Boolean; RefreshBmp: Boolean); overload;
  procedure BitmapReplaceColor(Bmp: TBitmap; OldColor: TColor; NewColor: TColor; PercentRange1Red, PercentRange1Green, PercentRange1Blue: Extended;
                               PercentRange2Red, PercentRange2Green, PercentRange2Blue: Double; SingleDestinationColor: Boolean; RefreshBmp: Boolean); overload;
  procedure BitmapReplaceColors(Bmp: TBitmap; Array_OldPalette, Array_NewPalette: Array of TColor; SingleDestinationColor, RefreshBmp: Boolean);
  procedure BitmapResize(SourceBmp: TBitmap; DestinationBmp: TBitmap; KeepPaletteColor: Boolean; RefreshBmp: Boolean);
  procedure BitmapRotate(Bmp: TBitmap; AngleDegree: Word; AdjustSize: Boolean; BkColor: TColor = clNone);
  procedure BitmapBlur(SourceBmp: TBitmap; DestinationBmp: TBitmap; Pixels: Word; Percent: Integer; RefreshBmp: Boolean);
  procedure GraphicMirror(Source: TGraphic; Destination: TCanvas; Left, Top: Integer; Horizontal, Vertical: Boolean); overload;
  procedure GraphicMirror(Source: TGraphic; Destination: TBitmap; Horizontal: Boolean; Vertical: Boolean); overload;
  procedure BitmapSaveToJpegFile(Bmp: TBitmap; const FileName: String; const QualityPercent: Word);
  procedure ConvertBmpFileToJpegFile(const aBmpFile, aJpegFile: String; const QualityPercent: Word);
  procedure JpegSaveToBitmapFile(JPEG: TJPEGImage; const FileName: String);
  procedure ConvertJpegFileToBmpFile(const aJpegFile, aBmpFile: String);

implementation

function GraphicCreate(GraphicClass: TGraphicClass): TGraphic;
begin
  Result := Nil;

  if GraphicClass = TJPEGImage then
    Result := TJPEGImage.Create
  else
 {$IFDEF UNICODE}
    if GraphicClass = TPngImage then
      Result := TPngImage.Create
    else
 {$ENDIF}
      if GraphicClass = TIcon then
        Result := TIcon.Create
      else
        if GraphicClass = TBitmap then
        Result := TBitmap.Create
      else
        if GraphicClass = TMetafile then
          Result := TMetafile.Create
        else
          raise Exception.Create('Graphic class unknown !');
end;

function BitmapCreate(BmpWidth: Integer; BmpHeight: Integer; BgColor: TColor; PixelFormat: TPixelFormat): TBitmap;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := PixelFormat;
  Result.Width := BmpWidth;
  Result.Height := BmpHeight;
  Result.Canvas.Brush.Color := BgColor;
  Result.Canvas.FillRect(classes.Rect(0, 0, BmpWidth, BmpHeight));
end;

function BitmapsCompare(Bmp1: TBitmap; Bmp2: TBitmap; FirstCol, LastCol, FirstRow, LastRow: Integer): Integer;
var x, y: Integer;
    P1, P2: PByteArray;
begin
  Result := 0;

  if Bmp1.Width - 1 < LastCol then Result := -1;
  if Bmp2.Width - 1 < LastCol then Result := -1;

  if Bmp1.Height - 1 < LastRow then Result := -1;
  if Bmp2.Height - 1 < LastRow then Result := -1;

  if Result = 0 then
    if Bmp1.PixelFormat <> Bmp2.PixelFormat then
      Result := -1;

  if Result <> 0 then Exit;

  for y := FirstRow to LastRow do
  begin
    P1 := Bmp1.ScanLine[y];
    P2 := Bmp2.ScanLine[y];

    case Bmp1.PixelFormat of
      pf4bit :  begin     // 16 cores : 2 pixeis = 1 byte !          // Tested !
                 for x := (FirstCol Div 2) to (LastCol Div 2) do
                   if P1[x] <> P2[x] then
                     Inc(Result);
                end;

      pf8bit :  begin     // 256 cores : 1 pixel = 1 byte !          // Tested !
                 for x := (FirstCol) to (LastCol) do
                   if P1[x] <> P2[x] then
                     Inc(Result);
                end;

      pf16bit : begin     // 16 bit : 1 pixel = 2 bytes !
                 for x := (FirstCol * 2) to (LastCol * 2) do
                   if P1[x] <> P2[x] then
                     Inc(Result);
                end;

      pf24bit : begin     // 24 bit : 1 pixel = 3 bytes !            // Tested !
                 for x := (FirstCol * 3) to (LastCol * 3) do
                   if P1[x] <> P2[x] then
                     Inc(Result);
                end;

      pf32bit : begin     // 32 bit : 1 pixel = 4 bytes !
                 for x := (FirstCol * 4) to (LastCol * 4) do
                   if P1[x] <> P2[x] then
                     Inc(Result);
                end;
    end;
  end;
end;

procedure BitmapSetPixelsBrightness(Bmp: TBitmap; IncPixels: Integer; RefreshBmp: Boolean);
var x, y : integer;
    Row : PRGBQuadArray;
    R,G,B : integer;
    SauvPixelFormat: TPixelFormat;
begin
  SauvPixelFormat := Bmp.PixelFormat;
  if Bmp.PixelFormat <> pf32Bit then Bmp.PixelFormat := pf32Bit;

  for y := 0 to Bmp.height-1 do
  begin
    row := Bmp.scanline[y];

    for x := 0 to Bmp.width-1 do
    begin
      R := Row[x].rgbred   + Round(Row[x].rgbred + IncPixels);
      G := Row[x].rgbgreen + Round(Row[x].rgbgreen + IncPixels);
      B := Row[x].rgbblue  + Round(Row[x].rgbblue + IncPixels);

      if R > 255 then R := 255 else if R < 0 then R := 0;
      if G > 255 then G := 255 else if G < 0 then G := 0;
      if B > 255 then B := 255 else if B < 0 then B := 0;

      row[x].rgbred   := R;
      row[x].rgbgreen := G;
      row[x].rgbblue  := B;
    end;
  end;

  if SauvPixelFormat <> pf32Bit then Bmp.PixelFormat := SauvPixelFormat;
  if RefreshBmp then Bmp.Modified := True;
end;

procedure BitmapSetPercentBrightness(Bmp: TBitmap; IncPercent: Integer; RefreshBmp: Boolean);
var x, y : integer;
    Row : PRGBQuadArray;
    R,G,B : integer;
    SauvPixelFormat: TPixelFormat;
begin
  SauvPixelFormat := Bmp.PixelFormat;
  if Bmp.PixelFormat <> pf32Bit then Bmp.PixelFormat := pf32Bit;

  for y := 0 to Bmp.height-1 do
  begin
    row := Bmp.scanline[y];

    for x := 0 to Bmp.width-1 do
    begin
      R := Row[x].rgbred   + Round(Row[x].rgbred * IncPercent / 100);
      G := Row[x].rgbgreen + Round(Row[x].rgbgreen * IncPercent / 100);
      B := Row[x].rgbblue  + Round(Row[x].rgbblue * IncPercent / 100);

      if R > 255 then R := 255 else if R < 0 then R := 0;
      if G > 255 then G := 255 else if G < 0 then G := 0;
      if B > 255 then B := 255 else if B < 0 then B := 0;

      row[x].rgbred   := R;
      row[x].rgbgreen := G;
      row[x].rgbblue  := B;
    end;
  end;

  if SauvPixelFormat <> pf32Bit then Bmp.PixelFormat := SauvPixelFormat;
  if RefreshBmp then Bmp.Modified := True;
end;

procedure BitmapSetPercentContrast(Bmp: TBitmap; IncPercent: Integer; RefreshBmp: Boolean);
var x, y : integer;
    Row : PRGBQuadArray;
    R,G,B, Average, incValue: integer;

    SauvPixelFormat: TPixelFormat;

        function CalcIncValue(Value: Integer): Integer;
        var Distance: Integer;
        begin
          Distance := Value - Average;
          Result := Round(Distance * IncPercent / 100);
        end;

begin
  if IncPercent > 100 then IncPercent := 100;
  if IncPercent < -100 then IncPercent := -100;

  SauvPixelFormat := Bmp.PixelFormat;
  if Bmp.PixelFormat <> pf32Bit then Bmp.PixelFormat := pf32Bit;

  for y := 0 to Bmp.height-1 do
  begin
    row := Bmp.scanline[y];

    for x := 0 to Bmp.width-1 do
    begin
      R := Row[x].rgbred;
      G := Row[x].rgbgreen;
      B := Row[x].rgbblue;

      Average := (R + G + B) Div 3;

      IncValue := CalcIncValue(R);
      R := R + incValue;

      IncValue := CalcIncValue(G);
      G := G + incValue;

      IncValue := CalcIncValue(B);
      B := B + incValue;

      if R > 255 then R := 255 else if R < 0 then R := 0;
      if G > 255 then G := 255 else if G < 0 then G := 0;
      if B > 255 then B := 255 else if B < 0 then B := 0;

      row[x].rgbred   := R;
      row[x].rgbgreen := G;
      row[x].rgbblue  := B;
    end;
  end;

  if SauvPixelFormat <> pf32Bit then Bmp.PixelFormat := SauvPixelFormat;
  if RefreshBmp then Bmp.Modified := True;
end;

procedure BitmapSetPixelsContrast(Bmp: TBitmap; IncPixels: Integer; RefreshBmp: Boolean);
var x, y : integer;
    Row : PRGBQuadArray;
    R,G,B, Average, incValue: integer;
    SauvPixelFormat: TPixelFormat;

        function CalcIncValue(Value: Integer): Integer;
        begin
          if IncPixels > 0 then
          begin
            // Increase contrast
            if Value > Average
            then Result := IncPixels
            else Result := (-1) * IncPixels;
          end
          else begin
            // Decrease contrast
            if Value > Average then
            begin
              if IncPixels < Average - Value
              then Result := Average - Value
              else Result := IncPixels;
            end
            else begin
              if IncPixels < Average - Value
              then Result := Average - Value
              else Result := IncPixels;
            end;
          end;
        end;

begin
  SauvPixelFormat := Bmp.PixelFormat;
  if Bmp.PixelFormat <> pf32Bit then Bmp.PixelFormat := pf32Bit;

  for y := 0 to Bmp.height-1 do
  begin
    row := Bmp.scanline[y];

    for x := 0 to Bmp.width-1 do
    begin
      R := Row[x].rgbred;
      G := Row[x].rgbgreen;
      B := Row[x].rgbblue;

      Average := (R + G + B) Div 3;

      IncValue := CalcIncValue(R);
      R := R + incValue;

      IncValue := CalcIncValue(G);
      G := G + incValue;

      IncValue := CalcIncValue(B);
      B := B + incValue;

      if R > 255 then R := 255 else if R < 0 then R := 0;
      if G > 255 then G := 255 else if G < 0 then G := 0;
      if B > 255 then B := 255 else if B < 0 then B := 0;

      row[x].rgbred   := R;
      row[x].rgbgreen := G;
      row[x].rgbblue  := B;
    end;
  end;

  if SauvPixelFormat <> pf32Bit then Bmp.PixelFormat := SauvPixelFormat;
  if RefreshBmp then Bmp.Modified := True;
end;

procedure BitmapCrop(Bmp: TBitmap; CropRect: TRect);
begin
  DrawGraphic(Bmp.Canvas, classes.Rect(0, 0, CropRect.Right - CropRect.Left, CropRect.Bottom - CropRect.Top), // Destination
              Bmp, CropRect, clNone);                                                                         // Source

  Bmp.Width := CropRect.Right - CropRect.Left;
  Bmp.Height := CropRect.Bottom - CropRect.Top;
end;

procedure BitmapNegative(Bmp: TBitmap; RefreshBmp: Boolean);
var x, y  : integer;
    Row   : PRGBQuadArray;
    SauvPixelFormat: TPixelFormat;
begin
  SauvPixelFormat := Bmp.PixelFormat;
  if Bmp.PixelFormat <> pf32Bit then Bmp.PixelFormat := pf32Bit;

  for y := 0 to Bmp.height-1 do   // attention au -1
  begin
    row := Bmp.scanline[y];      // scanline

    for x := 0 to Bmp.width-1 do // attention au -1
    begin
      row[x].rgbred   := 255 - row[x].rgbred;
      row[x].rgbgreen := 255 - row[x].rgbgreen;
      row[x].rgbblue  := 255 - row[x].rgbblue;
    end;
  end;

  if SauvPixelFormat <> pf32Bit then Bmp.PixelFormat := SauvPixelFormat;
  if RefreshBmp then Bmp.Modified := True;
end;

procedure BitmapModifyRGB(Bmp: TBitmap; IncRed: Integer; IncGreen: Integer; IncBlue: Integer; RefreshBmp: Boolean);
var x, y  : integer;
    Row   : PRGBQuadArray;
    R,G,B, _R,_G,_B : integer;
    SauvPixelFormat: TPixelFormat;
begin
  SauvPixelFormat := Bmp.PixelFormat;
  if Bmp.PixelFormat <> pf32Bit then Bmp.PixelFormat := pf32Bit;

  for y := 0 to Bmp.height-1 do
  begin
    row := Bmp.scanline[y];

    for x := 0 to Bmp.width-1 do
    begin
      R := Row[x].rgbred;
      G := Row[x].rgbgreen;
      B := Row[x].rgbblue;

      _R := R + IncRed;
      _G := G + IncGreen;
      _B := B + IncBlue;

      if _R > 255 then _R := 255 else if _R < 0 then _R := 0;
      if _G > 255 then _G := 255 else if _G < 0 then _G := 0;
      if _B > 255 then _B := 255 else if _B < 0 then _B := 0;

      row[x].rgbred   := _R;
      row[x].rgbgreen := _G;
      row[x].rgbblue  := _B;
    end;
  end;

  if SauvPixelFormat <> pf32Bit then Bmp.PixelFormat := SauvPixelFormat;
  if RefreshBmp then Bmp.Modified := True;
end;

procedure BitmapReplaceColor(Bmp: TBitmap; OldColor: TColor; NewColor: TColor; RangeRed, RangeGreen, RangeBlue: Word; SingleDestinationColor: Boolean; RefreshBmp: Boolean);
var x, y, Tmp  : integer;
    Row   : PRGBQuadArray;
    R,G,B, _R,_G,_B : integer;
    OldColorR, OldColorG, OldColorB : integer;
    NewColorR, NewColorG, NewColorB : integer;
    SauvPixelFormat: TPixelFormat;
begin
  SauvPixelFormat := Bmp.PixelFormat;
  if Bmp.PixelFormat <> pf32Bit then Bmp.PixelFormat := pf32Bit;

  Tmp := ColorToRGB(OldColor);

  OldColorR := GetRValue(Tmp);
  OldColorG := GetGValue(Tmp);
  OldColorB := GetBValue(Tmp);

  Tmp := ColorToRGB(NewColor);

  NewColorR := GetRValue(Tmp);
  NewColorG := GetGValue(Tmp);
  NewColorB := GetBValue(Tmp);

  for y := 0 to Bmp.height-1 do
  begin
    row := Bmp.scanline[y];

    for x := 0 to Bmp.width-1 do
    begin
      R := Row[x].rgbred;
      G := Row[x].rgbgreen;
      B := Row[x].rgbblue;

      if (R >= OldColorR - RangeRed) and (R <= OldColorR + RangeRed)
        and (G >= OldColorG - RangeGreen) and (G <= OldColorG + RangeGreen)
          and (B >= OldColorB - RangeBlue) and (B <= OldColorB + RangeBlue) then
      begin
        if not SingleDestinationColor then
        begin
          _R := NewColorR + (R - OldColorR);
          _G := NewColorG + (G - OldColorG);
          _B := NewColorB + (B - OldColorB);

          if _R > 255 then _R := 255 else if _R < 0 then _R := 0;
          if _G > 255 then _G := 255 else if _G < 0 then _G := 0;
          if _B > 255 then _B := 255 else if _B < 0 then _B := 0;

          row[x].rgbred   := _R;
          row[x].rgbgreen := _G;
          row[x].rgbblue  := _B;
        end
        else begin
          row[x].rgbred   := NewColorR;
          row[x].rgbgreen := NewColorG;
          row[x].rgbblue  := NewColorB;
        end;
      end;
    end;
  end;

  if SauvPixelFormat <> pf32Bit then Bmp.PixelFormat := SauvPixelFormat;
  if RefreshBmp then Bmp.Modified := True;
end;

procedure BitmapReplaceColor(Bmp: TBitmap; OldColor: TColor; NewColor: TColor; PercentRange1Red, PercentRange1Green, PercentRange1Blue: Extended;
                               PercentRange2Red, PercentRange2Green, PercentRange2Blue: Double; SingleDestinationColor: Boolean; RefreshBmp: Boolean);
var x, y, Tmp  : integer;
    Row   : PRGBQuadArray;
    curR,curG,curB, _R,_G,_B : integer;
    OldColorR, OldColorG, OldColorB: integer;
    NewColorR, NewColorG, NewColorB: integer;
    percR, percG, percB: Double;
    SauvPixelFormat: TPixelFormat;

        procedure ADD_TO_VARS(var Src: Integer; Valor: Integer; ValorFinal: Integer; var Dest1, Dest2: Integer);
        begin
          Dest1 := Dest1 + Valor Div 2;
          Dest2 := Dest2 + Valor Div 2;
          Src := ValorFinal;
        end;

begin
  SauvPixelFormat := Bmp.PixelFormat;
  if Bmp.PixelFormat <> pf32Bit then Bmp.PixelFormat := pf32Bit;

  Tmp := ColorToRGB(OldColor);

  OldColorR := GetRValue(Tmp);
  OldColorG := GetGValue(Tmp);
  OldColorB := GetBValue(Tmp);

  Tmp := ColorToRGB(NewColor);

  NewColorR := GetRValue(Tmp);
  NewColorG := GetGValue(Tmp);
  NewColorB := GetBValue(Tmp);

  for y := 0 to Bmp.height-1 do
  begin
    row := Bmp.scanline[y];

    for x := 0 to Bmp.width-1 do
    begin
      curR := Row[x].rgbred;
      curG := Row[x].rgbgreen;
      curB := Row[x].rgbblue;

      if curR + curG + curB <> 0 then
      begin
        percR := curR / (curR + curG + curB);
        percG := curG / (curR + curG + curB);
        percB := curB / (curR + curG + curB);
      end
      else begin
        percR := 0;
        percG := 0;
        percB := 0;
      end;

      if ( (percR >= PercentRange1Red) and (percR<= PercentRange2Red) )
       and ( (percG >= PercentRange1Green) and (percG<= PercentRange2Green) )
        and ( (percB >= PercentRange1Blue) and (percB<= PercentRange2Blue) ) then
      begin
        if not SingleDestinationColor then
        begin
          _R := NewColorR + (curR - OldColorR);
          _G := NewColorG + (curG - OldColorG);
          _B := NewColorB + (curB - OldColorB);

          for tmp := 1 to 30 do
          begin
            if _R < 0 then ADD_TO_VARS(_R, _R, 0, _G, _B);
            if _G < 0 then ADD_TO_VARS(_G, _G, 0, _R, _B);
            if _B < 0 then ADD_TO_VARS(_B, _B, 0, _R, _G);

            if _R > 255 then ADD_TO_VARS(_R, _R-255, 255, _G, _B);
            if _G > 255 then ADD_TO_VARS(_G, _G-255, 255, _R, _B);
            if _B > 255 then ADD_TO_VARS(_B, _B-255, 255, _R, _G);
          end;

          if _R > 255 then _R := 255 else if _R < 0 then _R := 0;
          if _G > 255 then _G := 255 else if _G < 0 then _G := 0;
          if _B > 255 then _B := 255 else if _B < 0 then _B := 0;

          row[x].rgbred   := _R;
          row[x].rgbgreen := _G;
          row[x].rgbblue  := _B;
        end
        else begin
          row[x].rgbred   := NewColorR;
          row[x].rgbgreen := NewColorG;
          row[x].rgbblue  := NewColorB;
        end;
      end;
    end;
  end;

  if SauvPixelFormat <> pf32Bit then Bmp.PixelFormat := SauvPixelFormat;
  if RefreshBmp then Bmp.Modified := True;
end;

procedure BitmapReplaceColors(Bmp: TBitmap; Array_OldPalette, Array_NewPalette: Array of TColor; SingleDestinationColor, RefreshBmp: Boolean);
var
  a, Ind, Tmp: Integer;

  x, y : Integer;
  pxR, pxG, pxB: Integer;
  Row: PRGBQuadArray;
  SauvPixelFormat: TPixelFormat;

  nbPaletteColors: Integer;
  Arr_OldPalette, Arr_NewPalette: Array of TRGBQuad;
  Arr_Variation: Array of Integer;


        function GET_VARIATION(curPixel: TRGBQuad; OldColor: TRGBQuad): Integer;
        begin
          RESULT := abs(curPixel.rgbRed - OldColor.rgbRed)
                    + abs(curPixel.rgbGreen - OldColor.rgbGreen)
                     + abs(curPixel.rgbBlue - OldColor.rgbBlue);
        end;

begin
  nbPaletteColors := length(Array_OldPalette);
  if nbPaletteColors = 0 then EXIT;

  SauvPixelFormat := Bmp.PixelFormat;
  if Bmp.PixelFormat <> pf32Bit then Bmp.PixelFormat := pf32Bit;

  // Pré-calcul: récupérer valeurs RGB de l' ancienne et de la nouvelle palette :
  SetLength(Arr_Variation,  nbPaletteColors);
  SetLength(Arr_OldPalette, nbPaletteColors);
  SetLength(Arr_NewPalette, nbPaletteColors);

  for a := 0 to nbPaletteColors-1 do
  begin
    Tmp := ColorToRGB(Array_OldPalette[a]);
    Arr_OldPalette[a].rgbRed   := GetRValue(Tmp);
    Arr_OldPalette[a].rgbGreen := GetGValue(Tmp);
    Arr_OldPalette[a].rgbBlue  := GetBValue(Tmp);

    Tmp := ColorToRGB(Array_NewPalette[a]);
    Arr_NewPalette[a].rgbRed   := GetRValue(Tmp);
    Arr_NewPalette[a].rgbGreen := GetGValue(Tmp);
    Arr_NewPalette[a].rgbBlue  := GetBValue(Tmp);
  end;

  for y := 0 to Bmp.height-1 do
  begin
    row := Bmp.scanline[y];

    for x := 0 to Bmp.width-1 do
    begin
      pxR := Row[x].rgbred;
      pxG := Row[x].rgbgreen;
      pxB := Row[x].rgbblue;

      // Calcul des variations pour chaque couleur de la nouvelle palette :
      for a := 0 to nbPaletteColors-1 do
      begin
        Arr_Variation[a] := GET_VARIATION(Row[x], Arr_OldPalette[a]);

        // Recherche de la couleur de la palette qui a la plus petite variation comparé à la couleur du pixel actuel :
        if a = 0
        then
          Ind := 0
        else
          if Arr_Variation[a] < Arr_Variation[Ind]
          then Ind := a;
      end;

      // Remplacer la couleur actuelle para celle qui a la plus petite variation :
      if not SingleDestinationColor then
      begin
        pxR := Arr_NewPalette[Ind].rgbRed + (pxR - Arr_OldPalette[Ind].rgbRed);
        pxG := Arr_NewPalette[Ind].rgbGreen + (pxG - Arr_OldPalette[Ind].rgbGreen);
        pxB := Arr_NewPalette[Ind].rgbBlue + (pxB - Arr_OldPalette[Ind].rgbBlue);

        if pxR > 255 then pxR := 255 else if pxR < 0 then pxR := 0;
        if pxG > 255 then pxG := 255 else if pxG < 0 then pxG := 0;
        if pxB > 255 then pxB := 255 else if pxB < 0 then pxB := 0;

        row[x].rgbred   := pxR;
        row[x].rgbgreen := pxG;
        row[x].rgbblue  := pxB;
      end
      else begin
        row[x].rgbred   := Arr_NewPalette[Ind].rgbRed;
        row[x].rgbgreen := Arr_NewPalette[Ind].rgbGreen;
        row[x].rgbblue  := Arr_NewPalette[Ind].rgbBlue;
      end;
    end;
  end;

  if SauvPixelFormat <> pf32Bit then Bmp.PixelFormat := SauvPixelFormat;
  if RefreshBmp then Bmp.Modified := True;
end;

procedure BitmapResize(SourceBmp: TBitmap; DestinationBmp: TBitmap; KeepPaletteColor: Boolean; RefreshBmp: Boolean);
type
  RColorDef = record
    R: Byte;
    G: Byte;
    B: Byte;
    Count: Word;
  end;

var
  x, y: Integer;
  NbPixeis, R, G, B: Int64;
  Row: PRGBQuadArray;
  BiggerX, BiggerY: Boolean;
  RangeY1, RangeY2, RangeX1, RangeX2: Integer;
  RealRgX, RealRgY, RapportX, RapportY: Extended;
  SauvPixelFormatSrc: TPixelFormat;

                  procedure CalcAverageColor;
                  var
                    _Row: PRGBQuadArray;
                    _x, _y: Integer;
                  begin
                    R := 0;
                    G := 0;
                    B := 0;

                    NbPixeis := 0;

                    for _y := RangeY1 to RangeY2 do
                    begin
                      _Row := SourceBmp.scanline[_y];

                      for _x := RangeX1 to RangeX2 do
                      begin
                        inc(R, _Row[_x].rgbRed);
                        inc(G, _Row[_x].rgbGreen);
                        inc(B, _Row[_x].rgbBlue);
                        Inc(NbPixeis, 1);
                      end;
                    end;

                    if NbPixeis > 0 then
                    begin
                      R := R Div NbPixeis;
                      G := G Div NbPixeis;
                      B := B Div NbPixeis;
                    end;
                  end;


                  procedure GetMostUsedColor;
                  var
                    _Row: PRGBQuadArray;
                    _x, _y, _f, _Count: Integer;

                    Array_palette: Array of RColorDef;
                    idx, LengthArray_palette: Integer;
                  begin
                    R := 0;
                    G := 0;
                    B := 0;
                    LengthArray_palette := 0;

                    for _y := RangeY1 to RangeY2 do
                    begin
                      _Row := SourceBmp.scanline[_y];

                      for _x := RangeX1 to RangeX2 do
                      begin
                        idx := -1;
                        for _f := 0 to LengthArray_palette-1 do
                          if (Array_palette[_f].R = _Row[_x].rgbRed) and (Array_palette[_f].G = _Row[_x].rgbGreen) and (Array_palette[_f].B = _Row[_x].rgbBlue) then
                          begin
                            idx := _f;
                            Break;
                          end;

                        if idx = -1 then
                        begin
                          inc(LengthArray_palette);
                          SetLength(Array_palette, LengthArray_palette);
                          idx := LengthArray_palette - 1;
                          Array_palette[idx].R := _Row[_x].rgbRed;
                          Array_palette[idx].G := _Row[_x].rgbGreen;
                          Array_palette[idx].B := _Row[_x].rgbBlue;
                        end;

                        Array_palette[idx].Count := Array_palette[idx].Count + 1;
                      end;
                    end;

                    // Retrieve color most used :
                    _Count := 0;
                    for _f := 0 to LengthArray_palette-1 do
                      if Array_palette[_f].Count > _Count then
                      begin
                        R := Array_palette[_f].R;
                        G := Array_palette[_f].G;
                        B := Array_palette[_f].B;
                        _Count := Array_palette[_f].Count;
                      end;
                  end;

begin
  if DestinationBmp.Width + DestinationBmp.Height = 0 then Exit;

  SauvPixelFormatSrc  := SourceBmp.PixelFormat;
  if SourceBmp.PixelFormat <> pf32Bit then SourceBmp.PixelFormat := pf32Bit;
  if DestinationBmp.PixelFormat <> pf32Bit then DestinationBmp.PixelFormat := pf32Bit;

  BiggerX := DestinationBmp.Width > SourceBmp.Width;
  BiggerY := DestinationBmp.Height > SourceBmp.Height;

  RapportX := SourceBmp.Width / DestinationBmp.Width;
  RapportY := SourceBmp.Height / DestinationBmp.Height;
  RangeY1 := 0;

  for y := 0 to DestinationBmp.height-1 do
  begin
    row := DestinationBmp.scanline[y];

    RealRgY := y * RapportY + RapportY;
    RangeY2 := Round(RealRgY);

    if RangeY2 > SourceBmp.Height - 1
    then RangeY2 := SourceBmp.Height - 1;

    RangeX1 := 0;

    for x := 0 to DestinationBmp.width-1 do
    begin
      RealRgX := x * RapportX + RapportX;
      RangeX2 := Round(RealRgX);

      if RangeX2 > SourceBmp.Width - 1 then
        RangeX2 := SourceBmp.Width - 1;

      // Calcular media dos pixeis entre RangeX1, RangeY1, RangeX2 e RangeY2 :
      if KeepPaletteColor then
      begin
        GetMostUsedColor;
      end
      else begin
        CalcAverageColor;

        if R < 0 then R := 0; if R > 255 then R := 255;
        if G < 0 then G := 0; if G > 255 then G := 255;
        if B < 0 then B := 0; if B > 255 then B := 255;
      end;

      row[x].rgbred   := R;
      row[x].rgbgreen := G;
      row[x].rgbblue  := B;

      if BiggerX
      then RangeX1 := Round(RealRgX + RapportX)
      else RangeX1 := RangeX2+1;

      if RangeX1 > SourceBmp.Width - 1 then
        RangeX1 := SourceBmp.Width - 1;
    end;

    if BiggerY
    then RangeY1 := Round(RealRgY + RapportY)
    else RangeY1 := RangeY2+1;

    if RangeY1 > SourceBmp.Height - 1
    then RangeY1 := SourceBmp.Height - 1;
  end;

  if SauvPixelFormatSrc <> pf32Bit then SourceBmp.PixelFormat := SauvPixelFormatSrc;
  if RefreshBmp then DestinationBmp.Modified := True;
end;

(* Old
procedure BitmapRotate(SourceBmp: TBitmap; DestinationCanvas: TCanvas; Degree: Integer);
var tXF: tagXForm;
    Angle, AngleDiag: real;
begin
  SetGraphicsMode(DestinationCanvas.Handle, GM_Advanced);
  Angle := Degree * Pi / 180;
  tXF.eM11 :=  cos(Angle);
  tXF.eM12 :=  sin(Angle);
  tXF.eM21 := -sin(Angle);{amusez vous à transformer les valeurs des tXF, par exemple...enlevez le signe "-"}
  tXF.eM22:= cos(Angle);

  AngleDiag := ArcCos(SourceBmp.Width / sqrt(sqr(SourceBmp.Height) + sqr(SourceBmp.Width)));
  Angle := Angle + AngleDiag;

  tXF.eDX:=(-cos(Angle)+cos(AngleDiag)) * sqrt(sqr(SourceBmp.Height) + sqr(SourceBmp.Width))/2;
  tXF.eDY:=(-sin(Angle)+sin(AngleDiag)) * sqrt(sqr(SourceBmp.Height) + sqr(SourceBmp.Width))/2;
  SetWorldTransform(DestinationCanvas.Handle, tXF);
  DestinationCanvas.Draw(0, 0, SourceBmp);
end;  *)

procedure BitmapRotate(Bmp: TBitmap; AngleDegree: Word; AdjustSize: Boolean; BkColor: TColor = clNone);
var
  Rad: Single;
  C: Single;
  S: Single;
  Tmp: TBitmap;
  OffsetX: Single;
  OffsetY: Single;
  Points: array[0..2] of TPoint;
begin
  if (AngleDegree = 0) or (AngleDegree = 360) then Exit;

  Rad := DegToRad(AngleDegree);
  C := Cos(Rad);
  S := Sin(Rad);
  Tmp := TBitmap.Create;

  try
    Tmp.TransparentColor := Bmp.TransparentColor;
    Tmp.TransparentMode := Bmp.TransparentMode;
    Tmp.Transparent := Bmp.Transparent;
    Tmp.Canvas.Brush.Color := BkColor;

    if AdjustSize then
    begin
      if AngleDegree = 180 then
      begin
        Tmp.Width := Bmp.Width;
        Tmp.Height := Bmp.Height;

        // Upper-left corner of source will rotate to lower right corner :
        Points[0].X := Tmp.Width;
        Points[0].Y := Tmp.Height;

        // Upper-right corner will rotate to lower left corner :
        Points[1].X := 0;
        Points[1].Y := Tmp.Height;

        // Lower-left corner will rotate upper right corner :
        Points[2].X := Tmp.Width;
        Points[2].Y := 0;

        PlgBlt(Tmp.Canvas.Handle, Points, Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, 0, 0, 0);
      end
      else
        if AngleDegree = 90 then        // Rotate to right
        begin
          Tmp.Width := Bmp.Height;
          Tmp.Height := Bmp.Width;

          // Upper-left corner of source will rotate to upper right corner :
          Points[0].X := Tmp.Width;
          Points[0].Y := 0;

          // Upper-right corner will rotate to lower right corner :
          Points[1].X := Tmp.Width;
          Points[1].Y := Tmp.Height;

          // Lower-left corner will rotate upper left corner :
          Points[2].X := 0;
          Points[2].Y := 0;

          PlgBlt(Tmp.Canvas.Handle, Points, Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, 0, 0, 0);
        end
        else
          if AngleDegree = 270 then     // Rotate to left
          begin
            Tmp.Width := Bmp.Height;
            Tmp.Height := Bmp.Width;

            // Upper-left corner of source will rotate to lower left corner :
            Points[0].X := 0;
            Points[0].Y := Tmp.Height;

            // Upper-right corner will rotate to upper left corner :
            Points[1].X := 0;
            Points[1].Y := 0;

            // Lower-left corner will rotate lower right corner :
            Points[2].X := Tmp.Width;
            Points[2].Y := Tmp.Height;

            PlgBlt(Tmp.Canvas.Handle, Points, Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, 0, 0, 0);
          end
          else begin
            Tmp.Width := Round(Bmp.Width * Abs(C) + Bmp.Height * Abs(S));
            Tmp.Height := Round(Bmp.Width * Abs(S) + Bmp.Height * Abs(C));
            OffsetX := (Tmp.Width - Bmp.Width * C + Bmp.Height * S) / 2;
            OffsetY := (Tmp.Height - Bmp.Width * S - Bmp.Height * C) / 2;

            Points[0].X := Round(OffsetX);
            Points[0].Y := Round(OffsetY);
            Points[1].X := Round(OffsetX + Bmp.Width * C);
            Points[1].Y := Round(OffsetY + Bmp.Width * S);
            Points[2].X := Round(OffsetX - Bmp.Height * S);
            Points[2].Y := Round(OffsetY + Bmp.Height * C);

            PlgBlt(Tmp.Canvas.Handle, Points, Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, 0, 0, 0);
          end;
    end
    else begin
      Tmp.Width := Bmp.Width;
      Tmp.Height := Bmp.Height;
      OffsetX := (Bmp.Width - Bmp.Width * C + Bmp.Height * S) / 2;
      OffsetY := (Bmp.Height - Bmp.Width * S - Bmp.Height * C) / 2;

      Points[0].X := Round(OffsetX);
      Points[0].Y := Round(OffsetY);
      Points[1].X := Round(OffsetX + Bmp.Width * C);
      Points[1].Y := Round(OffsetY + Bmp.Width * S);
      Points[2].X := Round(OffsetX - Bmp.Height * S);
      Points[2].Y := Round(OffsetY + Bmp.Height * C);

      PlgBlt(Tmp.Canvas.Handle, Points, Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, 0, 0, 0);
    end;

    Bmp.Assign(Tmp);
  finally
    Tmp.Free;
  end;
end;

procedure BitmapBlur(SourceBmp: TBitmap; DestinationBmp: TBitmap; Pixels: Word; Percent: Integer; RefreshBmp: Boolean);
var ImgSrc: TBitmap;
   i, X, Y : Integer;
   R, G, B: Integer;
   _R, _G, _B: Double;
   nbPixels: Extended;
   SauvPixelFormatSrc : TPixelFormat;
   rowDest_Actual, rowSrc_Actual, rowSrc_Anterior, rowSrc_Seguinte: PRGBQuadArray;
begin
  ImgSrc := TBitmap.Create;
  ImgSrc.Assign(SourceBmp);

  SauvPixelFormatSrc  := ImgSrc.PixelFormat;
  if ImgSrc.PixelFormat <> pf32Bit then ImgSrc.PixelFormat := pf32Bit;
  if DestinationBmp.PixelFormat <> pf32Bit then DestinationBmp.PixelFormat := pf32Bit;

  DestinationBmp.Width  := ImgSrc.Width;
  DestinationBmp.Height := ImgSrc.Height;

  for i := 1 to Pixels do
  begin
    for y := 0 to ImgSrc.Height - 1 do
    begin
      rowSrc_Actual   := ImgSrc.scanline[y];

      if y = 0
      then rowSrc_Anterior   := ImgSrc.scanline[y]
      else rowSrc_Anterior   := ImgSrc.scanline[y-1];

      if y = ImgSrc.Height - 1
      then rowSrc_Seguinte   := ImgSrc.scanline[y]
      else rowSrc_Seguinte   := ImgSrc.scanline[y+1];

      rowDest_Actual := DestinationBmp.scanline[y];

      for x := 0 to ImgSrc.Width - 1 do
      begin
        nbPixels := 0;
        _R := 0;
        _G := 0;
        _B := 0;

        // Pixels à esquerda :
        if x > 0 then
        begin
          nbPixels := nbPixels + 1 * Percent / 100;

          _R := _R + (rowSrc_Anterior[x-1].rgbRed * Percent / 100);
          _G := _G + (rowSrc_Anterior[x-1].rgbGreen * Percent / 100);
          _B := _B + (rowSrc_Anterior[x-1].rgbBlue * Percent / 100);

          nbPixels := nbPixels + 1 * Percent / 100;

          _R := _R + (rowSrc_Actual[x-1].rgbRed * Percent / 100);
          _G := _G + (rowSrc_Actual[x-1].rgbGreen * Percent / 100);
          _B := _B + (rowSrc_Actual[x-1].rgbBlue * Percent / 100);

          nbPixels := nbPixels + 1 * Percent / 100;

          _R := _R + (rowSrc_Seguinte[x-1].rgbRed * Percent / 100);
          _G := _G + (rowSrc_Seguinte[x-1].rgbGreen * Percent / 100);
          _B := _B + (rowSrc_Seguinte[x-1].rgbBlue * Percent / 100);
        end;

        // Pixels à direita :
        if x < ImgSrc.Width - 1 then
        begin
          nbPixels := nbPixels + 1 * Percent / 100;

          _R := _R + (rowSrc_Anterior[x+1].rgbRed * Percent / 100);
          _G := _G + (rowSrc_Anterior[x+1].rgbGreen * Percent / 100);
          _B := _B + (rowSrc_Anterior[x+1].rgbBlue * Percent / 100);

          nbPixels := nbPixels + 1 * Percent / 100;

          _R := _R + (rowSrc_Actual[x+1].rgbRed * Percent / 100);
          _G := _G + (rowSrc_Actual[x+1].rgbGreen * Percent / 100);
          _B := _B + (rowSrc_Actual[x+1].rgbBlue * Percent / 100);

          nbPixels := nbPixels + 1 * Percent / 100;

          _R := _R + (rowSrc_Seguinte[x+1].rgbRed * Percent / 100);
          _G := _G + (rowSrc_Seguinte[x+1].rgbGreen * Percent / 100);
          _B := _B + (rowSrc_Seguinte[x+1].rgbBlue * Percent / 100);
        end;

        // Pixels centrais :
        nbPixels := nbPixels + 1;
        _R := _R + rowSrc_Actual[x].rgbRed;
        _G := _G + rowSrc_Actual[x].rgbGreen;
        _B := _B + rowSrc_Actual[x].rgbBlue;

        nbPixels := nbPixels + 1 * Percent / 100;
        _R := _R + (rowSrc_Anterior[x].rgbRed * Percent / 100);
        _G := _G + (rowSrc_Anterior[x].rgbGreen * Percent / 100);
        _B := _B + (rowSrc_Anterior[x].rgbBlue * Percent / 100);

        nbPixels := nbPixels + 1 * Percent / 100;
        _R := _R + (rowSrc_Seguinte[x].rgbRed * Percent / 100);
        _G := _G + (rowSrc_Seguinte[x].rgbGreen * Percent / 100);
        _B := _B + (rowSrc_Seguinte[x].rgbBlue * Percent / 100);

        R := Round(_R / nbPixels);
        G := Round(_G / nbPixels);
        B := Round(_B / nbPixels);

        if R < 0 then R := 0; if R > 255 then R := 255;
        if G < 0 then G := 0; if G > 255 then G := 255;
        if B < 0 then B := 0; if B > 255 then B := 255;

        rowDest_Actual[x].rgbRed   := R;
        rowDest_Actual[x].rgbGreen := G;
        rowDest_Actual[x].rgbBlue  := B;
      end;
    end;

    if i < Pixels then
      ImgSrc.Assign(DestinationBmp);
  end;

  if SauvPixelFormatSrc <> pf32Bit then ImgSrc.PixelFormat := SauvPixelFormatSrc;
  if RefreshBmp then DestinationBmp.Modified := True;
  ImgSrc.Free;
end;

procedure GraphicMirror(Source: TGraphic; Destination: TCanvas; Left, Top: Integer; Horizontal, Vertical: Boolean);
var aRect: TRect;
begin
  if Horizontal and Vertical then
    aRect := Classes.Rect(Left + Source.Width - 1, Top + Source.height - 1, Left + (-1), Top + (-1))
  else
    if Horizontal then
      aRect := Classes.Rect(Left + Source.width - 1, Top, Left + (-1), Top + Source.height)
    else
      if Vertical
      then aRect := Classes.Rect(Left, Top + Source.height - 1, Left + Source.width, Top + (-1))
      else aRect := Classes.Rect(Left, Top, Left + Source.Width, Top + Source.Height);

  Destination.StretchDraw(aRect, Source);
end;

procedure GraphicMirror(Source: TGraphic; Destination: TBitmap; Horizontal, Vertical: Boolean);
begin
  Destination.Width  := Source.Width;
  Destination.Height := Source.Height;
  GraphicMirror(Source, Destination.Canvas, 0, 0, Horizontal, Vertical);
end;

procedure BitmapSaveToJpegFile(Bmp: TBitmap; const FileName: String; const QualityPercent: Word);
var
  _JPEG : TJPEGImage;
begin
  _JPEG := TJPEGImage.Create;

  try
    _JPEG.Assign(Bmp);
    _JPEG.CompressionQuality := QualityPercent;
    _JPEG.SaveToFile(FileName);
  finally
    _JPEG.Free;
  end;
end;

procedure ConvertBmpFileToJpegFile(const aBmpFile, aJpegFile: String; const QualityPercent: Word);
var
  _BMP : TBitmap;
begin
  _BMP := TBitmap.Create;

  try
    _BMP.LoadFromFile(aBmpFile);
    BitmapSaveToJpegFile(_BMP, aJpegFile, QualityPercent);
  finally
    _BMP.Free;
  end;
end;

procedure JpegSaveToBitmapFile(JPEG: TJPEGImage; const FileName: String);
var
  _BMP : TBitmap;
begin
  _BMP := TBitmap.Create;

  try
    _BMP.Assign(JPEG);
    _BMP.SaveToFile(FileName);
  finally
    _BMP.Free;
  end;
end;

procedure ConvertJpegFileToBmpFile(const aJpegFile, aBmpFile: String);
var
  _JPEG : TJPEGImage;
begin
  _JPEG := TJPEGImage.Create;

  try
    _JPEG.LoadFromFile(aJpegFile);
    JpegSaveToBitmapFile(_JPEG, aBmpFile);
  finally
    _JPEG.Free;
  end;
end;

end.
