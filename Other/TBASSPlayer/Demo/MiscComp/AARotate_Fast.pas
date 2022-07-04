// AARotate_Fast v1.2
//
// This unit is based on the VB code written by Lefteris Eleftheriades.
// Please visit "http://www2.cs.ucy.ac.cy/~cs06ee1/", if you want to know more
//  about the background of this unit.
//
//  v1.23 :  3 Apr 2009
//    Fixed floating point exception (occured at Transparent = true)  (ver 1.20)
//    Added a parameter : Scale    (ver 1.20)
//    Added a parameter : ApplySrcAlpha (ver 1.21)
//    Fixed bug at caculating transparency (ver 1.22)
//    Fixed bug at caculating color with alpha (ver 1.23)
//
//  v1.1 :  23 Mar 2009
//    Added a parameter : Transparent
//
//  v1.0 : 17 Mar 2009
//    Initial release
//
//  written by Silhwan Hyun  (hyunsh@hanafos.com)


unit AARotate_Fast;

interface

uses Windows, graphics, Math, SysUtils;


// Parameters & Result
//  SrcBitmap : The source image to be rotated.
//  Rotation :  The degree by which the image should be rotated clockwise.
//  BgColor :   The color of the background where the rotated bitmap does not overlap the
//               destination bitmap.
//  Transparent : Decides if the BgColor is treated as Transparent color.
//            It means BGColor is excluded at adding color elements and Alpha.
//  ApplySrcAlpha : This parameter is valid only if the PixelFormat of source image is
//             pf32bit.
//            Decides whether to apply source image's alpha channel(acually the "rgbReserved"
//             elements of TBitmap's pixel. We can load PNG file preserving alpha channel
//             data by use of GdipLoadImageFromFile and GdipCreateHBITMAPFromBitmap. See demo
//             project.) at calculating the value of destination image's transparency.
//  AutoBlend : This parameter is not effective if Transparent is true. (= considered as
//             false if Transparent is true)
//            Decides if the edges of the rotated image should be blended with the
//             background color defined by BgColor.
//            If false, the rgbReserved byte of each pixel will be set to the appropriate
//             alpha values so that the rotated image can be blended onto another image later
//             without a harsh edge.
//  Scale  : The size(width and height) factor of rotated image.
//            Min : 0.1 ~ Max : 10.0 (ex: 0.5 : 50%, 1.0 : 100%, 2.0 : 200%)
//            note) Limited the scaled image is not lesser than 5 pixel in height or width
//  Result :  The rotated image. Returns nil if there are errors.

function FastAARotatedBitmap(SrcBitmap : TBitmap; Rotation : double; BgColor : integer;
              Transparent, ApplySrcAlpha, AutoBlend : boolean; Scale : double) : TBitmap;


implementation


function aar_roundup(A : Double) : longint;
begin
   if Abs(A - trunc(A + 0.0000000005)) < 0.000000001 then
      aar_roundup := trunc(A + 0.0000000005)
   else
      aar_roundup := trunc(A + 1);
end;

function aar_cos(degrees : double) : double;
var
   off : double;
   idegrees : integer;
begin
   off := (degrees / 30 - round(degrees / 30));
   if (off < 0.0000001) and (off > -0.0000001) then
   begin
      idegrees := round(degrees);
      if (idegrees < 0) then
         idegrees := (360 - (-idegrees mod 360))
      else
         idegrees := (idegrees mod 360);

      case (idegrees) of
              0 : result := 1.0;
             30 : result := 0.866025403784439;
             60 : result := 0.5;
             90 : result := 0.0;
            120 : result := -0.5;
            150 : result := -0.866025403784439;
            180 : result := -1.0;
            210 : result := -0.866025403784439;
            240 : result := -0.5;
            270 : result := 0.0;
            300 : result := 0.5;
            330 : result := 0.866025403784439;
            360 : result := 1.0;
         else
            result := cos(degrees * 3.14159265358979 / 180);  // it shouldn't get here
      end;
   end else
      result := cos(degrees * 3.14159265358979 / 180);
end;

function aar_sin(degrees : double) : double;
begin
   result := aar_cos(degrees + 90.0);
end;

function byterange(a : double) : byte;
var
   b : integer;
begin
   b := round(a);

   if b < 0 then
      b := 0;
   if b > 255 then
      b := 255;

   result := b;
end;

function dorotate(src : HBITMAP; rotation : double; bgcolor : integer; transparent,
                  use_src_alpha, autoblend : boolean; scale : double) : HBITMAP;
const
   mx : array[0..3] of integer = (-1, 1, 1, -1);
   my : array[0..3] of integer = (-1, -1, 1, 1);
var
   indminx, indminy : integer;
   indmaxx, indmaxy : integer;
  // px, py : integer;
   px, py : double;
   pcos, psin : double;
   xres, yres : double;
   width, height : integer;

   srcbmp : Bitmap;
   srcdib : array of TRGBQUAD;
   dstdib : array of TRGBQUAD;
   srcdibbmap : TBITMAPINFO;
   ldc : HDC;
   backcolor : TRGBQUAD;
   TR, TB, TG : byte;

   XX, YY : integer;
   ix, iy : integer;
   cx, cy : integer;
   tx, ty : double;
   dx, dy : double;
   DstIndex, SrcIndex : integer;
   TopL, TopR, BotL, BotR : double;
   TopL_B, TopR_B, BotL_B, BotR_B : boolean;
   Dst_rgbRed, Dst_rgbBlue, Dst_rgbGreen : double;

   screenmode : DEVMODE;
   dstbmp : HBITMAP;
   dstdibmap : TBITMAPINFO;

   autoblend_ : boolean;
   Alpha : byte;
   UAlpha, RAlpha : double;
   AlphaTL, AlphaTR, AlphaBL, AlphaBR : double;
   pcos2, psin2 : double;
   InRegionRatio : double;
   
begin

  //Calculate some index values so that values can easily be looked up
   indminx := trunc(rotation / 90) mod 4;
   indminy := (indminx + 1) mod 4;
   indmaxx := (indminx + 2) mod 4;
   indmaxy := (indminx + 3) mod 4;

  //Load the source bitmaps information
   if (GetObject(src, sizeof(srcbmp), @srcbmp) = 0) then
   begin
      result := 0;
      exit;
   end;

  //Set the rotation axis default values
   px := srcbmp.bmWidth div 2;    // pivot x
   py := srcbmp.bmHeight div 2;   // pivot y
  // px := srcbmp.bmWidth / 2;    // pivot x
  // py := srcbmp.bmHeight / 2;   // pivot y

  //Calculate the cos and sin value
   pcos := aar_cos(Rotation) / abs(scale);
   psin := aar_sin(Rotation) / abs(scale);
   pcos2 := aar_cos(Rotation) * abs(scale);
   psin2 := aar_sin(Rotation) * abs(scale);

  //Calculate the x and y offset of the rotated image (half the width and height of the rotated image)
   xres := mx[indmaxx] * px * pcos2 - my[indmaxx] * py * psin2;
   yres := mx[indmaxy] * px * psin2 + my[indmaxy] * py * pcos2;

  //Get the width and height of the rotated image
   width := aar_roundup(xres * 2);
   height := aar_roundup(yres * 2);
   cx := (width - srcbmp.bmWidth) div 2;
   cy := (height - srcbmp.bmHeight) div 2;

  //Create the source dib array and the destdib array
   SetLength(srcdib, srcbmp.bmWidth * srcbmp.bmHeight);
   SetLength(dstdib, width * height);

  //Load source bits into srcdib
   srcdibbmap.bmiHeader.biSize := sizeof(srcdibbmap.bmiHeader);
   srcdibbmap.bmiHeader.biWidth := srcbmp.bmWidth;
   srcdibbmap.bmiHeader.biHeight := -srcbmp.bmHeight;
   srcdibbmap.bmiHeader.biPlanes := 1;
   srcdibbmap.bmiHeader.biBitCount := 32;
   srcdibbmap.bmiHeader.biCompression := BI_RGB;

   ldc := CreateCompatibleDC(0);
   GetDIBits(ldc, src, 0, srcbmp.bmHeight, srcdib, srcdibbmap, DIB_RGB_COLORS);
   DeleteDC(ldc);

   backcolor.rgbRed := bgcolor and $000000FF;
   backcolor.rgbGreen := (bgcolor and $0000FF00) div $00000100;
   backcolor.rgbBlue := (bgcolor and $00FF0000) div $00010000;
   TR := backcolor.rgbRed;
   TG := backcolor.rgbGreen;
   TB := backcolor.rgbBlue;
 // Surpress AutoBlend option if transparent is true;
   if transparent then autoblend_ := false else autoblend_ := autoblend;

   for XX := -cx to (width - cx - 1) do      // for destination's width
   begin
     for YY := -cy to (height - cy - 1) do   // for destination's height
     begin
      // Get the rotation translation (gives the SourceImage coordinate for each DestImage x,y)
      tx := (XX - px) * PCos - (YY - py) * pSin + px;
      ty := (XX - px) * pSin + (YY - py) * PCos + py;

      // Get nearest to the left pixel
      if (tx > -1) and (tx < 0) then    // Consider just outer border
         ix := -1
      else
         ix := trunc(tx);
      if (ty > -1) and (ty < 0) then    // Consider just outer border
         iy := -1
      else
         iy := trunc(ty);

      // Get the digits after the decimal point
      dx := Abs(tx - ix);
      dy := Abs(ty - iy);

      DstIndex := XX + cx + (YY + cy) * Width;
      SrcIndex := ix + iy * srcbmp.bmWidth;

      if (tx > -1) and (ix + 1 <= srcbmp.bmWidth) and
         (ty > -1) and (iy + 1 <= srcbmp.bmHeight) then
      begin
      // The SourcePixel color maybe a combination of upto four pixels as tx and ty
      //  are not integers.
      // The intersepted (by the current calculated source pixel) area each pixel
      //  involved (see .doc for more info)
        TopL := (1 - dx) * (1 - dy);
        TopR := dx * (1 - dy);
        BotL := (1 - dx) * dy;
        BotR := dx * dy;
      // The sum of (TopL + TopR + BotL + BotR) is always 1

        if (tx >= 0) and (ix + 1 < srcbmp.bmWidth) and
           (ty >= 0) and (iy + 1 < srcbmp.bmHeight) then
        begin
         // All the intersepted areas are placed within srcbmp region
         // Antialiasing: DestColor = SourceTopLeftPixel * TopLeftAreaIntersectedBySourcePixel
         //                         + SourceTopRightPixel * TopRightAreaIntersectedBySourcePixel
         //                         + bottomleft... + bottomrigth...
         if not transparent then
         begin
           if use_src_alpha then begin
             AlphaTL := srcdib[SrcIndex].rgbReserved * TopL;
             AlphaTR := srcdib[SrcIndex+1].rgbReserved * TopR;
             AlphaBL := srcdib[SrcIndex+srcbmp.bmWidth].rgbReserved * BotL;
             AlphaBR := srcdib[SrcIndex+srcbmp.bmWidth+1].rgbReserved * BotR;
           end else begin
             AlphaTL := TopL;
             AlphaTR := TopR;
             AlphaBL := BotL;
             AlphaBR := BotR;
           end;

           dstdib[DstIndex].rgbRed := byterange(srcdib[SrcIndex].rgbRed * AlphaTL
                                     + srcdib[SrcIndex+1].rgbRed * AlphaTR
                                     + srcdib[SrcIndex+srcbmp.bmWidth].rgbRed * AlphaBL
                                     + srcdib[SrcIndex+srcbmp.bmWidth+1].rgbRed * AlphaBR);
           dstdib[DstIndex].rgbBlue := byterange(srcdib[SrcIndex].rgbBlue * AlphaTL
                                     + srcdib[SrcIndex+1].rgbBlue * AlphaTR
                                     + srcdib[SrcIndex+srcbmp.bmWidth].rgbBlue * AlphaBL
                                     + srcdib[SrcIndex+srcbmp.bmWidth+1].rgbBlue * AlphaBR);
           dstdib[DstIndex].rgbGreen := byterange(srcdib[SrcIndex].rgbGreen * AlphaTL
                                     + srcdib[SrcIndex+1].rgbGreen * AlphaTR
                                     + srcdib[SrcIndex+srcbmp.bmWidth].rgbGreen * AlphaBL
                                     + srcdib[SrcIndex+srcbmp.bmWidth+1].rgbGreen * AlphaBR);
           if use_src_alpha then
              dstdib[DstIndex].rgbReserved
                                   := byterange(AlphaTL + AlphaTR + AlphaBL + AlphaBR)
           else
              dstdib[DstIndex].rgbReserved := 255;
         end else
         begin   // if transparent
           RAlpha := 0;
           Dst_rgbRed := 0;
           Dst_rgbBlue := 0;
           Dst_rgbGreen := 0;
           TopL_B := true; TopR_B := true; BotL_B := true; BotR_B := true;

           if (srcdib[SrcIndex].rgbRed <> TR) or (srcdib[SrcIndex].rgbBlue <> TB) or
              (srcdib[SrcIndex].rgbGreen <> TG) then
           begin
              if use_src_alpha then
                 UAlpha := TopL * srcdib[SrcIndex].rgbReserved / 255
              else
                 UAlpha := TopL;
              RAlpha := UAlpha;
              TopL_B := false;
              Dst_rgbRed := srcdib[SrcIndex].rgbRed * TopL{UAlpha};
              Dst_rgbBlue := srcdib[SrcIndex].rgbBlue * TopL{UAlpha};
              Dst_rgbGreen := srcdib[SrcIndex].rgbGreen * TopL{UAlpha};
           end;

           if TopR <> 0 then
           if (srcdib[SrcIndex+1].rgbRed <> TR) or (srcdib[SrcIndex+1].rgbBlue <> TB) or
              (srcdib[SrcIndex+1].rgbGreen <> TG) then
           begin
              if use_src_alpha then
                 UAlpha := TopR * srcdib[SrcIndex+1].rgbReserved / 255
              else
                 UAlpha := TopR;
              RAlpha := RAlpha + UAlpha;
              TopR_B := false;
              Dst_rgbRed := Dst_rgbRed + srcdib[SrcIndex+1].rgbRed * TopR{UAlpha};
              Dst_rgbBlue := Dst_rgbBlue + srcdib[SrcIndex+1].rgbBlue * TopR{UAlpha};
              Dst_rgbGreen := Dst_rgbGreen + srcdib[SrcIndex+1].rgbGreen * TopR{UAlpha};
           end;

           if BotL <> 0 then
           if (srcdib[SrcIndex+srcbmp.bmWidth].rgbRed <> TR) or
              (srcdib[SrcIndex+srcbmp.bmWidth].rgbBlue <> TB) or
              (srcdib[SrcIndex+srcbmp.bmWidth].rgbGreen <> TG) then
           begin
              if use_src_alpha then
                 UAlpha := BotL * srcdib[SrcIndex+srcbmp.bmWidth].rgbReserved / 255
              else
                 UAlpha := BotL;
              RAlpha := RAlpha + UAlpha;
              BotL_B := false;
              Dst_rgbRed := Dst_rgbRed + srcdib[SrcIndex+srcbmp.bmWidth].rgbRed * BotL{UAlpha};
              Dst_rgbBlue := Dst_rgbBlue + srcdib[SrcIndex+srcbmp.bmWidth].rgbBlue * BotL{UAlpha};
              Dst_rgbGreen := Dst_rgbGreen + srcdib[SrcIndex+srcbmp.bmWidth].rgbGreen * BotL{UAlpha};
           end;

           if BotR <> 0 then
           if (srcdib[SrcIndex+srcbmp.bmWidth+1].rgbRed <> TR) or
              (srcdib[SrcIndex+srcbmp.bmWidth+1].rgbBlue <> TB) or
              (srcdib[SrcIndex+srcbmp.bmWidth+1].rgbGreen <> TG) then
           begin
              if use_src_alpha then
                 UAlpha := BotR * srcdib[SrcIndex+srcbmp.bmWidth+1].rgbReserved / 255
              else
                 UAlpha := BotR;
              RAlpha := RAlpha + UAlpha;
              BotR_B := false;
              Dst_rgbRed := Dst_rgbRed + srcdib[SrcIndex+srcbmp.bmWidth+1].rgbRed * BotR{UAlpha};
              Dst_rgbBlue := Dst_rgbBlue + srcdib[SrcIndex+srcbmp.bmWidth+1].rgbBlue * BotR{UAlpha};
              Dst_rgbGreen := Dst_rgbGreen + srcdib[SrcIndex+srcbmp.bmWidth+1].rgbGreen * BotR{UAlpha};
           end;

           if use_src_alpha then
           begin
             dstdib[DstIndex].rgbRed := byterange(Dst_rgbRed);
             dstdib[DstIndex].rgbBlue := byterange(Dst_rgbBlue);
             dstdib[DstIndex].rgbGreen := byterange(Dst_rgbGreen);
           end else begin
           // InRegionRatio gets the ratio of the non-transparent area
           // + 0.0001 : Put to avoid floating point exception by huge number.
             InRegionRatio := 1 - (TopL * ord(TopL_B) + TopR * ord(TopR_B)
                               + BotR * ord(BotR_B) + BotL * ord(BotL_B)) + 0.0001;
             dstdib[DstIndex].rgbRed := byterange(Dst_rgbRed / InRegionRatio);
             dstdib[DstIndex].rgbBlue := byterange(Dst_rgbBlue / InRegionRatio);
             dstdib[DstIndex].rgbGreen := byterange(Dst_rgbGreen / InRegionRatio);
           end;

           dstdib[DstIndex].rgbReserved := byterange(RAlpha * 255);
         end;
        end else
        begin
         // Some elements of the intersepted areas are placed out of srcbmp region.
         // For the intersepted areas which are placed out of srcbmp region,
         //  - Transparent is false AND AutoBlend is true : add background color
         //  - Transparent is true OR AutoBlend is false : do nothing
         // Determine the elements which are placed out of srcbmp region.
           TopL_B := false; TopR_B := false; BotL_B := false; BotR_B := false;
           if tx < 0 then begin TopL_B := true; BotL_B := true; end;
           if ty < 0 then begin TopL_B := true; TopR_B := true; end;
           if ix = (srcbmp.bmWidth - 1) then begin TopR_B := true; BotR_B := true; end;
           if iy = (srcbmp.bmHeight - 1) then begin BotL_B := true; BotR_B := true; end;

           RAlpha := 0;
           Dst_rgbRed := 0;
           Dst_rgbBlue := 0;
           Dst_rgbGreen := 0;
           
           if TopL_B then   // if Top-Left element is out of srcbmp region
           begin
             if autoblend_ then
             begin
               Dst_rgbRed := backcolor.rgbRed * TopL;
               Dst_rgbBlue := backcolor.rgbBlue * TopL;
               Dst_rgbGreen := backcolor.rgbGreen * TopL;
             end;
           end else begin   // Top-Left element is within srcbmp region
             if (not transparent) or
                ((srcdib[SrcIndex].rgbRed <> TR) or (srcdib[SrcIndex].rgbBlue <> TB) or
                 (srcdib[SrcIndex].rgbGreen <> TG)) then
             begin
                if use_src_alpha then
                   UAlpha := TopL * srcdib[SrcIndex].rgbReserved / 255
                else
                   UAlpha := TopL;
                RAlpha := UAlpha;
                Dst_rgbRed := srcdib[SrcIndex].rgbRed * TopL{UAlpha};
                Dst_rgbBlue := srcdib[SrcIndex].rgbBlue * TopL{UAlpha};
                Dst_rgbGreen := srcdib[SrcIndex].rgbGreen * TopL{UAlpha};
             end else
                TopL_B := true;  // treated as out of region element
           end;

           if TopR_B then   // if Top-Right element is out of srcbmp region
           begin
             if autoblend_ then
             begin
               Dst_rgbRed := Dst_rgbRed + backcolor.rgbRed * TopR;
               Dst_rgbBlue := Dst_rgbBlue + backcolor.rgbBlue * TopR;
               Dst_rgbGreen := Dst_rgbGreen + backcolor.rgbGreen * TopR;
             end;
           end else begin   // Top-Right element is within srcbmp region
             if (not transparent) or
                ((srcdib[SrcIndex+1].rgbRed <> TR) or (srcdib[SrcIndex+1].rgbBlue <> TB) or
                 (srcdib[SrcIndex+1].rgbGreen <> TG)) then
             begin
                if use_src_alpha then
                   UAlpha := TopR * srcdib[SrcIndex+1].rgbReserved / 255
                else
                   UAlpha := TopR;
                RAlpha := RAlpha + UAlpha;
                Dst_rgbRed := Dst_rgbRed + srcdib[SrcIndex+1].rgbRed * TopR{UAlpha};
                Dst_rgbBlue := Dst_rgbBlue + srcdib[SrcIndex+1].rgbBlue * TopR{UAlpha};
                Dst_rgbGreen := Dst_rgbGreen + srcdib[SrcIndex+1].rgbGreen * TopR{UAlpha};
             end else
                TopR_B := true;  // treated as out of region element
           end;

           if BotL_B then   // if Bottom-Left element is out of srcbmp region
           begin
             if autoblend_ then
             begin
               Dst_rgbRed := Dst_rgbRed + backcolor.rgbRed * BotL;
               Dst_rgbBlue := Dst_rgbBlue + backcolor.rgbBlue * BotL;
               Dst_rgbGreen := Dst_rgbGreen + backcolor.rgbGreen * BotL;
             end;
           end else begin   // Bottom-Left element is within srcbmp region
             if (not transparent) or
                ((srcdib[SrcIndex+srcbmp.bmWidth].rgbRed <> TR) or
                 (srcdib[SrcIndex+srcbmp.bmWidth].rgbBlue <> TB) or
                 (srcdib[SrcIndex+srcbmp.bmWidth].rgbGreen <> TG)) then
             begin
                if use_src_alpha then
                   UAlpha := BotL * srcdib[SrcIndex+srcbmp.bmWidth].rgbReserved / 255
                else
                   UAlpha := BotL;
                RAlpha := RAlpha + UAlpha;
                Dst_rgbRed := Dst_rgbRed + srcdib[SrcIndex+srcbmp.bmWidth].rgbRed * BotL{UAlpha};
                Dst_rgbBlue := Dst_rgbBlue + srcdib[SrcIndex+srcbmp.bmWidth].rgbBlue * BotL{UAlpha};
                Dst_rgbGreen := Dst_rgbGreen + srcdib[SrcIndex+srcbmp.bmWidth].rgbGreen * BotL{UAlpha};
             end else
                BotL_B := true;   // treated as out of region element
           end;

           if BotR_B then   // if Bottom-Right element is out of srcbmp region
           begin
             if autoblend_ then
             begin
               Dst_rgbRed := Dst_rgbRed + backcolor.rgbRed * BotR;
               Dst_rgbBlue := Dst_rgbBlue + backcolor.rgbBlue * BotR;
               Dst_rgbGreen := Dst_rgbGreen + backcolor.rgbGreen * BotR;
             end;
           end else begin   // Bottom-Right element is within srcbmp region
             if (not transparent) or
                ((srcdib[SrcIndex+srcbmp.bmWidth+1].rgbRed <> TR) or
                 (srcdib[SrcIndex+srcbmp.bmWidth+1].rgbBlue <> TB) or
                 (srcdib[SrcIndex+srcbmp.bmWidth+1].rgbGreen <> TG)) then
             begin
                if use_src_alpha then
                   UAlpha := BotR * srcdib[SrcIndex+srcbmp.bmWidth+1].rgbReserved / 255
                else
                   UAlpha := BotR;
                RAlpha := RAlpha + UAlpha;
                Dst_rgbRed := Dst_rgbRed + srcdib[SrcIndex+srcbmp.bmWidth+1].rgbRed * BotR{UAlpha};
                Dst_rgbBlue := Dst_rgbBlue + srcdib[SrcIndex+srcbmp.bmWidth+1].rgbBlue * BotR{UAlpha};
                Dst_rgbGreen := Dst_rgbGreen + srcdib[SrcIndex+srcbmp.bmWidth+1].rgbGreen * BotR{UAlpha};
             end else
                BotR_B := true;
           end;

           if use_src_alpha then
           begin
              dstdib[DstIndex].rgbRed := byterange(Dst_rgbRed);
              dstdib[DstIndex].rgbBlue := byterange(Dst_rgbBlue);
              dstdib[DstIndex].rgbGreen := byterange(Dst_rgbGreen);
           end else begin
           // InRegionRatio gets the ratio of the area within srcbmp region.
           // + 0.0001 : Put to avoid floating point exception by huge number.
              InRegionRatio := 1 - (TopL * ord(TopL_B) + TopR * ord(TopR_B)
                                  + BotR * ord(BotR_B) + BotL * ord(BotL_B)) + 0.0001;
              dstdib[DstIndex].rgbRed := byterange(Dst_rgbRed / InRegionRatio);
              dstdib[DstIndex].rgbBlue := byterange(Dst_rgbBlue / InRegionRatio);
              dstdib[DstIndex].rgbGreen := byterange(Dst_rgbGreen / InRegionRatio);
           end;

         // Set alpha value for future use
           if autoblend_ then
              dstdib[DstIndex].rgbReserved := 255
           else
              dstdib[DstIndex].rgbReserved := byterange(RAlpha * 255);

          { else if transparent then
           begin
              dstdib[DstIndex].rgbReserved := byterange(RAlpha * 255);
           end else begin
              Alpha := byterange((1 - (TopL * integer(TopL_B)
                                     + TopR * integer(TopR_B)
                                     + BotR * integer(BotR_B)
                                     + BotL * integer(BotL_B))) * 255);
              dstdib[DstIndex].rgbReserved := Alpha;
           end; }
        end;
      end else
      begin
      // for entirely out of srcbmp region
      // Color the destination with the background color, if (not transparent).
        if (not transparent) then
        begin
          dstdib[DstIndex].rgbRed := backcolor.rgbRed;
          dstdib[DstIndex].rgbBlue := backcolor.rgbBlue;
          dstdib[DstIndex].rgbGreen := backcolor.rgbGreen;
        end;
        dstdib[DstIndex].rgbReserved := 0;  // alpha value for future use
      end;

     end;  // for YY
   end;  // for XX

   SetLength(srcdib, 0);

  //Get Current Display Settings
   screenmode.dmSize := sizeof(DEVMODE);
   EnumDisplaySettings(nil, $FFFFFFFF{ENUM_CURRENT_SETTINGS}, screenmode);

  //Create the final bitmap object
   dstbmp := CreateBitmap(width, height, 1, screenmode.dmBitsPerPel, nil);

  //Write the bits into the bitmap and return it
   dstdibmap.bmiHeader.biSize := sizeof(dstdibmap.bmiHeader);
   dstdibmap.bmiHeader.biWidth := width;
   dstdibmap.bmiHeader.biHeight := -height;
   dstdibmap.bmiHeader.biPlanes := 1;
   dstdibmap.bmiHeader.biBitCount := 32;
   dstdibmap.bmiHeader.biCompression := BI_RGB;
   SetDIBits(0, dstbmp, 0, height, dstdib, dstdibmap, DIB_RGB_COLORS);

   SetLength(dstdib, 0);

   result := dstbmp;
end;

function FastAARotatedBitmap(SrcBitmap : TBitmap; Rotation : double; BgColor : integer;
              Transparent, ApplySrcAlpha, AutoBlend : boolean; Scale : double) : TBitmap;
var
   res : HBITMAP;
   mult : integer;
   MinScale : double;
   UseAlphaChannel : boolean;
begin
   if SrcBitmap.Empty then
   begin
      result := nil;
      exit;
   end;

   if SrcBitmap.PixelFormat = pf24bit then
      UseAlphaChannel := false
   else if SrcBitmap.PixelFormat = pf32bit then
      UseAlphaChannel := ApplySrcAlpha
   else begin
      result := nil;
      exit;
   end;

  //Get rotation between (0, 360)
   mult := trunc(Rotation / 360);
   if (Rotation >= 0) then
      Rotation := Rotation - 360.0 * mult
   else
      Rotation := Rotation - 360.0 * (mult - 1);

  //Limit the scaled image is not lesser than 5 pixel in height or width
   if (SrcBitmap.Height <= 5) or (SrcBitmap.Width <= 5) then
      MinScale := 1
   else if SrcBitmap.Height > SrcBitmap.Width then
      MinScale := 5 / SrcBitmap.Width
   else
      MinScale := 5 / SrcBitmap.Height;

  //Get scale between (0.1, 10.0)
   if Scale < MinScale then
      Scale := MinScale;
   if Scale < 0.1 then
      Scale := 0.1
   else if Scale > 10.0 then
      Scale := 10.0;

   res := dorotate(SrcBitmap.Handle, Rotation, BgColor, Transparent, UseAlphaChannel, AutoBlend, Scale);

   if res <> 0 then
   begin
     try
        Result := TBitmap.Create;
        Result.PixelFormat := pf32bit;
        Result.Handle := res;
     except
        Result := nil;
     end;
   end else
      Result := nil;
end;


end.
