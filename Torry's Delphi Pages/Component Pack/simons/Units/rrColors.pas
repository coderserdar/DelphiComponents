{$INCLUDE rr.inc}
(************************************************************************
 Author:	Robert Rossmair

	mailto:Robert.Rossmair@t-online.de
	http://home.t-online.de/home/Robert.Rossmair/

 Module:	rrColors

 Version:       Version 1.8, 27-Jul-2001

 History:
     27-Jul-01:
        ( Simon Reinahrdt - reinhardt@picsoft.de )
        Added GetLuminance function
     28-Nov-00:
        ( Simon Reinahrdt - reinhardt@picsoft.de )
        Deleted functions which require math-unit
     06-Sep-00:
        ( Simon Reinahrdt - reinhardt@picsoft.de )
        Added contrast-factors for highlight and shadow colors to
        the Get3DColors-function
     11-Jul-00:
     	Made Get/SetBitmapColors functions instead of procedures.
        Return value is the number of colors actually retrieved
        respectively set.
     29-May-00:
     	Fixed Bug in MapBitmapColors:
          When (OldColors[I] = NewColors[J]) and (I > J),
          NewColors[I] (instead of NewColors[J]) would replace
          OldColors[J] in the bitmap.
        Thanks to Harry Tarnoff for pointing this issue out to me.
     05-Feb-00: V.1.5
        changed RGB2HLS code so that it does not longer need to
        catch EZeroDivide exceptions.
     23-Jan-00: V.1.4
     	some speed tuning in MapBitmapColors
     12-Jun-99: V.1.3
        added MapBitmapColors
     31-May-99: V.1.2.1
        found and fixed bug in Get/SetBitmapColors procedures.
        The bug caused a messed up color table if StartIndex <> 0.
     24-Mar-99: V.1.2
	re-included my homebrew floating-point RGB/HLS-conversion
	routines RGB2HLS and HLS2RGB.
     23-Mar-99: V.1.1.2
	fixes bugs in RGBtoHLS and HLStoRGB.
     22-Mar-99: V.1.1.1
	eliminated call to rrMath.UMax;
	minor changes.
     28-Jul-98: V.1.1
	Replaced my self-developed RGB to Hue/Luminance/Saturation
	conversion routines by more efficient code ported from
	Microsoft knowledge base.
     20-Nov-97: V.1.0

 Compatibility: Delphi 2-5

 Description:
     A small collection of color utility routines:

	SetBitmapColors: Change entries of the DIB color table
			 (Delphi 3 or above)

	Some color system conversion stuff
	{ RGB <-> Hue, Luminance, Saturation (HLS) }

	How does Win9x/NT 4 compute the	appropriate light and shadow
	colors of 3D-objects if the face color is given?
	I'm not sure, but Get3DColors obviously gives correct results
	thus proving my assumptions to be right.

 Copyright © 1997-2000  Robert Rossmair

 Use, modification and distribution is allowed without limitation,
 warranty, or liability of any kind. If you use this software or portions
 of it in a commercial program, I would appreciate it if you mentioned my
 contribution in the documentation materials accompaning your work.
 ************************************************************************)
unit rrColors;

interface

uses
  SysUtils,
  Windows,
  Classes,
{$IFNDEF VER100_up}
  Controls,
{$ENDIF}
  Graphics;

const
  MoreHighlightFactor = 0.5;

type
  TColorVector = record
    case Integer of
      0: (Coord: array[0..2] of Double);
      1: (R, G, B: Double);
      2: (H, L, S: Double);
  end;

  THLSValue = 0..240;
  THLSVector = record
    Hue:	THLSValue;
    Luminance:	THLSValue;
    Saturation:	THLSValue;
  end;

  EColorConv = class(Exception);

procedure Get3DColors(FaceColor: TColor; var HighLightColor, ShadowColor: TColor; HLFactor, ShFactor: single);
function GetLuminance(AColor: TColor): Double; // Result: 0..1
function HLStoRGB(Hue, Luminance, Saturation: THLSValue): TColorRef;
function RGBtoHLS(RGBColor: TColorRef): THLSVector;
// vector components need to be in [0, 1]
function HLS2RGB(const HLS: TColorVector): TColorVector;
function RGB2HLS(const RGB: TColorVector): TColorVector;
{$IFDEF VER100_up}
function GetBitmapColors(Bmp: TBitmap;var Colors: array of TColor;StartIndex: Integer): Integer;
function GetBmpBitsPerPixel(ABitmap: TBitmap): Integer;
procedure MapBitmapColors(Bmp: TBitmap; OldColors, NewColors: array of TColor);
function SetBitmapColors(Bmp: TBitmap;Colors: array of TColor;StartIndex: Integer): Integer;
{$ENDIF}
implementation

uses Consts;

type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array [Byte] of TRGBTriple;
  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array [Byte] of TRGBQuad;
const
  Invalid = -1;

// OutOfResources, GDIError, GDICheck token from Graphics unit

procedure OutOfResources;
begin
{$IFDEF VER100_up}
  raise EOutOfResources.Create(SOutOfResources);
{$ELSE}
  raise EOutOfResources.CreateRes(SOutOfResources);
{$ENDIF}
end;

procedure GDIError;
var
  ErrorCode: Integer;
  Buf: array [Byte] of Char;
begin
  ErrorCode := GetLastError;
  if (ErrorCode <> 0) and (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil,
    ErrorCode, LOCALE_USER_DEFAULT, Buf, sizeof(Buf), nil) <> 0) then
    raise EOutOfResources.Create(Buf)
  else
    OutOfResources;
end;

function GDICheck(Value: Integer): Integer;
begin
  if Value = 0 then GDIError;
  Result := Value;
end;
{$IFDEF VER100_up}
function GetBitmapColors(
  Bmp: TBitmap;
  var Colors: array of TColor;
  StartIndex: Integer): Integer;
var
  i: Integer;
  ColorTable: PRGBQuadArray;
  Count: Integer;
begin
  Count := High(Colors)-Low(Colors)+1;
  GetMem(ColorTable, Count * SizeOf(TRGBQuad));
  try
    Bmp.HandleType := bmDIB;
    Result := GDICheck(GetDIBColorTable(Bmp.Canvas.Handle, StartIndex, Count, ColorTable^));
    for i := 0 to Result-1 do
      with ColorTable^[i] do
	Colors[i] := RGB(rgbRed, rgbGreen, rgbBlue);
  finally
    FreeMem(ColorTable);
  end;
end;

function SetBitmapColors(
  Bmp: TBitmap;
  Colors: array of TColor;
  StartIndex: Integer): Integer;
var
  i, RGB: Integer;
  ColorTable: PRGBQuadArray;
  Count: Integer;
begin
  Count := High(Colors)-Low(Colors)+1;
  GetMem(ColorTable, Count * SizeOf(TRGBQuad));
  try
    for i := 0 to Count-1 do
      with ColorTable^[i] do
      begin
	RGB := ColorToRGB(Colors[i]);
	rgbBlue		:= GetBValue(RGB);
	rgbGreen	:= GetGValue(RGB);
	rgbRed		:= GetRValue(RGB);
	rgbReserved	:= 0;
      end;
    Bmp.HandleType := bmDIB;
    Result := GDICheck(SetDIBColorTable(Bmp.Canvas.Handle, StartIndex, Count, ColorTable^));
  finally
    FreeMem(ColorTable);
  end;
end;

function GetBmpBitsPerPixel(ABitmap: TBitmap): Integer;
var
  DIB: TDIBSection;
  PixelFormat : TPixelFormat;
begin
  PixelFormat := ABitmap.PixelFormat;
{ we don't know whether TPixelFormat type will change
  in future Delphi versions, thus we shall not use
  PixelFormat as index into an array of constants. }
  case PixelFormat of
    pfDevice: Result := GetDeviceCaps(ABitmap.Canvas.Handle,BITSPIXEL);
    pf1bit:  Result := 1;
    pf4bit:  Result := 4;
    pf8bit:  Result := 8;
    pf15bit: Result := 15;
    pf16bit: Result := 16;
    pf24bit: Result := 24;
    pf32bit: Result := 32;
    pfCustom:
      begin
        Result := 0;
        GetObject(ABitmap.Handle, SizeOf(DIB), @DIB);
        with DIB, dsbmih do
          if biBitCount = 16 then
          if biCompression = BI_BITFIELDS then
          if dsBitFields[1] = $3E0 then
            Result := 15;
      end;
    else
      Result := 0;
  end;
end;
{.$DEFINE PascalCode}
procedure MapBitmapColors(Bmp: TBitmap; OldColors, NewColors: array of TColor);
type
  TColorRec = packed record
    R, G, B, I: Byte;
  end;
const
  Counts: array[pf1bit..pf8bit] of Integer = (2, 16, 256);
var
  I, J, Max: Integer;
  X, Y, W, H: Integer;
  BufSize: Integer;
  Backup: Pointer;
  LineOffset: Integer;
  SrcLine: Pointer;
  DstLine: Pointer;
  SrcFirstLine: Pointer;
  DstFirstLine: Pointer;
  Count: Integer;
  Src24, Dst24: PRGBTriple;
{$IFDEF PascalCode}
  Src32, Dst32: PRGBQuad;
{$ENDIF}
  NewColorT: TRGBTriple;
  OldColorT: TRGBTriple;
  Src16, Dst16: PWord;
  OldColorW: Word;
  NewColorW: Word;
  OldColorQ: TRGBQuad;
  NewColorQ: TRGBQuad;
  Colors: PRGBQuadArray;
  DIB: TDIBSection;
  PixelFormat: TPixelFormat;

  function GetColorWord(Color: TColor; PixelFormat: TPixelFormat): Word;
  var
    C: TColorRec;
  begin
    C := TColorRec(ColorToRGB(Color));
    if PixelFormat = pf15bit then
      Result := (C.B shr 3)
             or (C.G shr 3) shl 5
             or (C.R shr 3) shl 10
    else
      Result := (C.B shr 3)
             or (C.G shr 2) shl 5
             or (C.R shr 3) shl 11;
  end;

  function GetRGBTriple(Color: TColor): TRGBTriple;
  var
    C: TColorRec;
  begin
    C := TColorRec(ColorToRGB(Color));
    Result.rgbtRed   := C.R;
    Result.rgbtGreen := C.G;
    Result.rgbtBlue  := C.B;
  end;

  function GetRGBQuad(Color: TColor): TRGBQuad;
  var
    C: TColorRec;
  begin
    C := TColorRec(ColorToRGB(Color));
    Result.rgbRed   := C.R;
    Result.rgbGreen := C.G;
    Result.rgbBlue  := C.B;
    Result.rgbReserved := 0;
  end;

begin
  Bmp.HandleType := bmDIB;
  PixelFormat := Bmp.PixelFormat;
  if PixelFormat = pfCustom then
  // Hack
  begin
    GetObject(Bmp.Handle, SizeOf(DIB), @DIB);
    with DIB, dsbmih do
      if biBitCount = 16 then
      if biCompression = BI_BITFIELDS then
      if dsBitFields[1] = $3E0 then
        PixelFormat := pf15Bit;
  end;
  Max := High(OldColors);
  if High(NewColors) < Max then Max := High(NewColors);
  case PixelFormat of
    pf1bit,
    pf4bit,
    pf8bit:
      begin
        Count := Counts[PixelFormat];
        Colors := AllocMem(SizeOf(Colors^));
        try
          Count := GetDIBColorTable(Bmp.Canvas.Handle, 0, Count, Colors^);
          for I := 0 to Max do
          begin
            OldColorQ := GetRGBQuad(OldColors[I]);
            NewColorQ := GetRGBQuad(NewColors[I]);
            for J := 0 to Count-1 do
            if Integer(Colors[J]) = Integer(OldColorQ) then
              Colors[J] := NewColorQ;
          end;
          SetDIBColorTable(Bmp.Canvas.Handle, 0, Count, Colors^);
        finally
          FreeMem(Colors);
        end;
      end;
    else
      begin
        W := Bmp.Width;
        H := Bmp.Height;
        LineOffset := Integer(Bmp.ScanLine[1])-Integer(Bmp.ScanLine[0]);
        BufSize := Abs(LineOffset)*H;
        Backup := AllocMem(BufSize);
        try
          DstFirstLine := Bmp.ScanLine[0];
          if LineOffset >= 0 then
          begin
            Move(DstFirstLine^, Backup^, BufSize);
            SrcFirstLine := Backup;
          end
          else
          begin
            Move(Bmp.Scanline[H-1]^, Backup^, BufSize);
            SrcFirstLine := Pointer(Integer(Backup)+BufSize+LineOffset);
          end;
          case PixelFormat of
            pf15bit,
            pf16bit:
              for I := 0 to Max do
              begin
                DstLine := DstFirstLine;
                SrcLine := SrcFirstLine;
                OldColorW := GetColorWord(OldColors[I], PixelFormat);
                NewColorW := GetColorWord(NewColors[I], PixelFormat);
                for Y := 0 to H-1 do
                begin
                  Src16 := PWord(SrcLine);
                  Dst16 := PWord(DstLine);
                  for X := 0 to W-1 do
                  begin
                    if Src16^ = OldColorW then
                      Dst16^ := NewColorW;
                    Inc(Src16);
                    Inc(Dst16);
                  end;
                  Inc(Integer(SrcLine), LineOffset);
                  Inc(Integer(DstLine), LineOffset);
                end;
              end;
            pf24bit:
              for I := 0 to Max do
              begin
                DstLine := DstFirstLine;
                SrcLine := SrcFirstLine;
                OldColorT := GetRGBTriple(OldColors[i]);
                NewColorT := GetRGBTriple(NewColors[i]);
                for Y := 0 to H-1 do
                begin
                  Src24 := PRGBTriple(SrcLine);
                  Dst24 := PRGBTriple(DstLine);
                  for X := 0 to W-1 do
                  begin
                    if Src24^.rgbtBlue = OldColorT.rgbtBlue then
                    if Src24^.rgbtGreen = OldColorT.rgbtGreen then
                    if Src24^.rgbtRed = OldColorT.rgbtRed then
                      Dst24^ := NewColorT;
                    Inc(Src24);
                    Inc(Dst24);
                  end;
                  Inc(Integer(SrcLine), LineOffset);
                  Inc(Integer(DstLine), LineOffset);
                end;
              end;
            pf32bit:
              for I := 0 to Max do
              begin
                OldColorQ := GetRGBQuad(OldColors[I]);
                NewColorQ := GetRGBQuad(NewColors[I]);
                {$IFDEF PascalCode}
                SrcLine := SrcFirstLine;
                DstLine := DstFirstLine;
                for Y := 0 to H-1 do
                begin
                  Src32 := PRGBQuad(SrcLine);
                  Dst32 := PRGBQuad(DstLine);
                  for X := 0 to W-1 do
                  begin
                    if PInteger(Src32)^ = Integer(OldColorQ) then
                      Dst32^ := NewColorQ;
                    Inc(Src32);
                    Inc(Dst32);
                  end;
                  Inc(Integer(SrcLine), LineOffset);
                  Inc(Integer(DstLine), LineOffset);
                end;
                {$ELSE}
                asm
                      mov	eax, W
                      mul	H
                      test	eax, eax
                      jz	@finis

                      push	esi
                      push	edi

                      mov	esi, Backup
                      mov	edi, DstFirstLine
                      add	edi, esi
                      sub	edi, SrcFirstLine

                      mov	ecx, OldColorQ
                      mov	edx, NewColorQ
                @Next:
                      cmp	ecx, [esi]
                      jne	@NE
                      mov	[edi], edx

                @NE:  add	esi, TYPE TRGBQuad
                      add	edi, TYPE TRGBQuad
                      dec	eax
                      jnz	@Next

                      pop	edi
                      pop	esi
                @finis:
                end;
                {$ENDIF}
              end;
          end;
        finally
          FreeMem(Backup);
        end;
      end;
  end;
end;
{$ENDIF}

function HLS2RGB(const HLS: TColorVector): TColorVector;
const
  Hue: array[0..5, 0..2] of Integer = (
	(1, -1, -1),	// red
	(1, 1, -1),	// yellow
	(-1, 1, -1),	// green
	(-1, 1, 1),	// cyan
	(-1, -1, 1),	// blue
	(1, -1, 1));	// magenta
  Components: array[0..2] of string = (
	'Hue',
	'Luminance',
	'Saturation');
var
  i, j, k: Integer;
  x: Double;
begin
  for i := Low(HLS.Coord) to High(HLS.Coord) do
    if (HLS.Coord[i] < 0) or (HLS.Coord[i] > 1) then
      raise EColorConv.CreateFmt('HLS2RGB: 0 <= %s value <= 1 required', [Components[i]]);

  j := Trunc(HLS.H * 6) mod 6;
  k := (j+1) mod 6;
  x := Frac(HLS.H * 6);
  for i := 0 to 2 do
    Result.Coord[i] := Hue[j, i] + x*(Hue[k, i]-Hue[j, i]);
  for i := 0 to 2 do Result.Coord[i] := Result.Coord[i] * HLS.S;
  if HLS.L <= 0.5 then
    for i := 0 to 2 do Result.Coord[i] := HLS.L * (Result.Coord[i]+ 1)
  else
    for i := 0 to 2 do Result.Coord[i] := HLS.L + Result.Coord[i] * (1-HLS.L);

  for i := 0 to 2 do
    if Result.Coord[i] < 0 then Result.Coord[i] := 0 else
    if Result.Coord[i] > 1 then Result.Coord[i] := 1;
end;

function RGB2HLS(const RGB: TColorVector): TColorVector;
const
  Epsilon = 1E-8;
  Components: array[0..2] of string = (
    'Red',
    'Green',
    'Blue');
var
  i, k: Integer;
  x: Double;
  V: TColorVector;
  W: TColorVector absolute Result;
  Hue: Double;
  Sat: Double;
  Lum: Double;

  function GetHue: Double;
  begin
    case k of
      0: if W.G > W.B then Result := 2+(W.B+1)/2
	 else Result := 4-(W.G+1)/2;
      1: if W.B > W.R then Result := 4+(W.R+1)/2
	 else Result := 6-(W.B+1)/2;
      2: if W.R > W.G then Result := (W.G+1)/2
	 else Result := 2-(W.R+1)/2;
      else Result := 0;
    end;
    Result := Result/6;
  end;

begin
  for i := Low(RGB.Coord) to High(RGB.Coord) do
    if (RGB.Coord[i] < 0) or (RGB.Coord[i] > 1) then
      raise EColorConv.CreateFmt('RGB2HLS: 0 <= %s value <= 1 required', [Components[i]]);

  x := 0;
  for i := 0 to 2 do
  begin
    V.Coord[i] := 2*RGB.Coord[i]-1; // [0, 1] -> [-1, 1]
    if Abs(V.Coord[i]) > x then
    begin
      x := Abs(V.Coord[i]);
      k := i;	// index of RGB coordinate most different from 0.5
    end;
  end;
  if x < Epsilon then	// middle grey
  begin
    Result.H := 0;
    Result.L := 0.5; // could be RGB.G or RGB.B as well
    Result.S := 0;
    Exit;
  end
  else x := 1/x;
  for i := 0 to 2 do W.Coord[i] := V.Coord[i] * x;
  x := 0;
  if V.Coord[k] <= 0 then
  begin
    for i := 0 to 2 do if (W.Coord[i]+1) > x then x := W.Coord[i] +1;
    if x < Epsilon then // R = G = B: location on grey axis
    begin
      Result.H := 0;
      Result.L := RGB.R; // could be RGB.G or RGB.B as well
      Result.S := 0;
      Exit;
    end
    else x := 2/x;
    for i := 0 to 2 do W.Coord[i] := x*(W.Coord[i]+1)-1;
    Hue := GetHue;
    // compute saturation
    if Abs(V.G-V.R) > Epsilon then
      Sat := (V.G-V.R)/(W.Coord[1]*(V.R+1)-W.Coord[0]*(V.G+1))
    else if Abs(V.B-V.G) > Epsilon then
      Sat := (V.B-V.G)/(W.Coord[2]*(V.G+1)-W.Coord[1]*(V.B+1))
    else if Abs(V.B-V.R) > Epsilon then
      Sat := (V.B-V.R)/(W.Coord[2]*(V.R+1)-W.Coord[0]*(V.B+1))
    else Sat := 0;
    // compute luminance
    if Abs(W.Coord[1]-W.Coord[0]) > Epsilon then
      Lum := (W.Coord[1]*(V.R+1)-W.Coord[0]*(V.G+1))/(W.Coord[1]-W.Coord[0])
    else if Abs(W.Coord[2]-W.Coord[1]) > Epsilon then
      Lum := (W.Coord[2]*(V.G+1)-W.Coord[1]*(V.B+1))/(W.Coord[2]-W.Coord[1])
    else if Abs(W.Coord[2]-W.Coord[0]) > Epsilon then
      Lum := (W.Coord[2]*(V.R+1)-W.Coord[0]*(V.B+1))/(W.Coord[2]-W.Coord[0])
    else Lum := V.R+1;
    Lum := Lum * 0.5;
  end else
  begin
    for i := 0 to 2 do if (1-W.Coord[i]) > x then x := 1-W.Coord[i];
    if x < Epsilon then // R = G = B: location on grey axis
    begin
      Result.H := 0;
      Result.L := RGB.R; // could be RGB.G or RGB.B as well
      Result.S := 0;
      Exit;
    end
    else x := 2/x;
    for i := 0 to 2 do W.Coord[i] := x*(W.Coord[i]-1)+1;
    x := 1;
    for i := 0 to 2 do
    if W.Coord[i] < x then
    begin
      x := W.Coord[i];
      k := i;
    end;
    Hue := GetHue;
    // compute saturation
    if Abs(V.G-V.R) > Epsilon then
      Sat := (V.G-V.R)/(W.Coord[0]*(V.G-1)-W.Coord[1]*(V.R-1))
    else if Abs(V.B-V.G) > Epsilon then
      Sat := (V.B-V.G)/(W.Coord[1]*(V.B-1)-W.Coord[2]*(V.G-1))
    else if Abs(V.B-V.R) > Epsilon then
      Sat := (V.B-V.R)/(W.Coord[0]*(V.B-1)-W.Coord[2]*(V.R-1))
    else Sat := 0;
    // compute luminance
    if Abs(W.Coord[1]-W.Coord[0]) > Epsilon then
      Lum := (W.Coord[1]*(V.R-1)-W.Coord[0]*(V.G-1))/(W.Coord[1]-W.Coord[0])
    else if Abs(W.Coord[2]-W.Coord[1]) > Epsilon then
      Lum := (W.Coord[2]*(V.G-1)-W.Coord[1]*(V.B-1))/(W.Coord[2]-W.Coord[1])
    else if Abs(W.Coord[2]-W.Coord[0]) > Epsilon then
      Lum := (W.Coord[2]*(V.R-1)-W.Coord[0]*(V.B-1))/(W.Coord[2]-W.Coord[0])
    else Lum := V.R-1;
    Lum := 1 + Lum * 0.5;
  end;
  W.H := Hue;
  W.L := Lum;
  W.S := Sat;

  for i := 0 to 2 do
    if W.Coord[i] < 0 then W.Coord[i] := 0 else
    if W.Coord[i] > 1 then W.Coord[i] := 1;
end;

procedure Get3DColors(FaceColor: TColor; var HighLightColor, ShadowColor: TColor; HLFactor, ShFactor: single);
var
  V,HLS : TColorVector;
  R,G,B : Byte;
begin
  FaceColor := ColorToRGB(FaceColor);
  R := GetRValue(FaceColor);
  G := GetGValue(FaceColor);
  B := GetBValue(FaceColor);
  HighLightColor := RGB(
	255-round((256-R) * HLFactor),
	255-round((256-G) * HLFactor),
	255-round((256-B) * HLFactor));
  V.R := R/255;
  V.G := G/255;
  V.B := B/255;
  HLS := RGB2HLS(V);
  HLS.L := HLS.L * ShFactor;	// Luminance := Luminance * Shadowfactor
  V := HLS2RGB(HLS);
  ShadowColor := RGB(
        Round(V.R*255),
        Round(V.G*255),
        Round(V.B*255));
end;

function GetLuminance(AColor: TColor): Double;  // Result: 0..1
var V,HLS : TColorVector;
    R,G,B : Byte;
begin
  AColor := ColorToRGB(AColor);
  R := GetRValue(AColor);
  G := GetGValue(AColor);
  B := GetBValue(AColor);
  V.R := R/255;
  V.G := G/255;
  V.B := B/255;
  HLS := RGB2HLS(V);
  Result :=  HLS.L;
end;

(************************************************************************
Translated C-code from Microsoft Knowledge Base
-------------------------------------------
Converting Colors Between RGB and HLS (HBS)
Article ID: Q29240
Creation Date: 26-APR-1988
Revision Date: 02-NOV-1995
The information in this article applies to:

Microsoft Windows Software Development Kit (SDK) for Windows versions 3.1 and 3.0
Microsoft Win32 Application Programming Interface (API) included with:

    - Microsoft Windows NT versions 3.5 and 3.51
    - Microsoft Windows 95 version 4.0
SUMMARY


The code fragment below converts colors between RGB (Red, Green, Blue) and HLS/HBS (Hue, Lightness, Saturation/Hue, Brightness, Saturation).


MORE INFORMATION


/* Color Conversion Routines --

RGBtoHLS() takes a DWORD RGB value, translates it to HLS, and stores the results in the global vars H, L, and S. HLStoRGB takes the current values of H, L, and S and returns the equivalent value in an RGB DWORD. The vars H, L, and S are only written to by:


   1. RGBtoHLS (initialization)
   2. The scroll bar handlers
A point of reference for the algorithms is Foley and Van Dam, "Fundamentals of Interactive Computer Graphics," Pages 618-19. Their algorithm is in floating point. CHART implements a less general (hardwired ranges) integral algorithm.
There are potential round-off errors throughout this sample. ((0.5 + x)/y) without floating point is phrased ((x + (y/2))/y), yielding a very small round-off error. This makes many of the following divisions look strange. */

*************************************************************************)
const
  HLSMAX = High(THLSValue);	// H,L, and S vary over 0-HLSMAX
  RGBMAX = 255;			// R,G, and B vary over 0-RGBMAX
				// HLSMAX BEST IF DIVISIBLE BY 6
				// RGBMAX, HLSMAX must each fit in a byte.
{ Hue is undefined if Saturation is 0 (grey-scale)
  This value determines where the Hue scrollbar is
  initially set for achromatic colors }
  UNDEFINED = HLSMAX*2 div 3;

function RGBtoHLS(RGBColor: TColorRef): THLSVector;
var
   R, G, B: Integer;              (* input RGB values *)
   H, L, S: Integer;
   cMax, cMin: Byte;           (* max and min RGB values *)
   Rdelta,Gdelta,Bdelta: Integer; (* intermediate value: % of spread from max*)
begin
   (* get R, G, and B out of DWORD *)
   R := GetRValue(RGBColor);
   G := GetGValue(RGBColor);
   B := GetBValue(RGBColor);

   (* calculate lightness *)
   cMax := R;
   if G > cMax then cMax := G;
   if B > cMax then cMax := B;

   cMin := R;
   if G < cMin then cMin := G;
   if B < cMin then cMin := B;

  L := ( ((cMax+cMin)*HLSMAX) + RGBMAX ) div (2*RGBMAX);

  if (cMax = cMin) then     // r=g=b --> achromatic case
  begin
     S := 0;	// saturation
     H := UNDEFINED;      // hue
  end else
  begin			// chromatic case
     { saturation }
     if L <= (HLSMAX div 2) then
	S := ( ((cMax-cMin)*HLSMAX) + ((cMax+cMin) div 2) )  div  (cMax+cMin)
     else
	S := ( ((cMax-cMin)*HLSMAX) + ((2*RGBMAX-cMax-cMin) div 2) )
	    div  (2*RGBMAX-cMax-cMin);

     (* hue *)
     Rdelta := ( ((cMax-R)*(HLSMAX div 6)) + ((cMax-cMin) div 2) ) div (cMax-cMin);
     Gdelta := ( ((cMax-G)*(HLSMAX div 6)) + ((cMax-cMin) div 2) ) div (cMax-cMin);
     Bdelta := ( ((cMax-B)*(HLSMAX div 6)) + ((cMax-cMin) div 2) ) div (cMax-cMin);

     if R = cMax then
	H := Bdelta - Gdelta
     else if G = cMax then
	H := (HLSMAX div 3) + Rdelta - Bdelta
     else (* B = cMax *)
	H := ((2*HLSMAX) div 3) + Gdelta - Rdelta;

     H := H mod HLSMAX;
     if H < 0 then
	Inc(H, HLSMAX);
  end;
  Result.Hue        := H;
  Result.Luminance  := L;
  Result.Saturation := S;
end;

function HueToRGB(n1,n2,hue: Integer): Integer;
(* utility routine for HLStoRGB *)
begin
   Hue := Hue mod HLSMAX;
   (* range check: note values passed add div subtract thirds of range *)
   if hue < 0 then
      Inc(hue, HLSMAX);

   (* return r,g, or b value from this tridrant *)
   if hue < (HLSMAX div 6) then
      Result := ( n1 + (((n2-n1)*hue+(HLSMAX div 12)) div (HLSMAX div 6)) ) else
   if hue < (HLSMAX div 2) then
      Result := n2 else
   if hue < ((HLSMAX*2) div 3) then
      Result := ( n1 + (((n2-n1)*(((HLSMAX*2) div 3)-hue)+(HLSMAX div 12)) div (HLSMAX div 6)))
   else
      Result := n1;
end;

function HLStoRGB(Hue, Luminance, Saturation: THLSValue): TColorRef;
var
   R, G, B: Integer;              (* RGB component values *)
   Magic1, Magic2: Integer;       (* calculated magic numbers (really!) *)
begin
   if Saturation = 0 then             (* achromatic case *)
   begin
      R :=(Luminance*RGBMAX) div HLSMAX;
      G := R;
      B := R;
      if Hue <> UNDEFINED then
      begin
	 (* ERROR *)
      end
   end else
   begin                    (* chromatic case *)
      (* set up magic numbers *)
      if (Luminance <= (HLSMAX div 2)) then
	 Magic2 := (Luminance*(HLSMAX + Saturation) + (HLSMAX div 2)) div HLSMAX
      else
	 Magic2 := Luminance + Saturation - ((Luminance*Saturation) + (HLSMAX div 2)) div HLSMAX;
      Magic1 := 2*Luminance-Magic2;
      (* get RGB, change units from HLSMAX to RGBMAX *)
      R := (HueToRGB(Magic1,Magic2,Hue+(HLSMAX div 3))*RGBMAX +(HLSMAX div 2)) div HLSMAX;
      G := (HueToRGB(Magic1,Magic2,Hue)               *RGBMAX +(HLSMAX div 2)) div HLSMAX;
      B := (HueToRGB(Magic1,Magic2,Hue-(HLSMAX div 3))*RGBMAX +(HLSMAX div 2)) div HLSMAX;
   end;
   Result :=  RGB(R,G,B);
end;

end.

