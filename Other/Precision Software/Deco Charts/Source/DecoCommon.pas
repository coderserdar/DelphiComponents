{------------------------------------------------------------------------------
  DecoCommon.pas

  DecoCharts for VCL

  written by  Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  Purpose:    Constants and types definition

  The source code is given as is. The author is not responsible
  for any possible damage done due to the use of this code.
  You can freely use this component in your products, if you have purchased
  the license. Except where otherwise noted, the complete source code remains
  property of the author and may not be distributed, published, given or sold
  in any form as such. No parts of the source code can be included in any
  other component or application without written authorization of the author.

  Copyright (c) 2008-2012  Precision software & consulting
  All rights reserved
------------------------------------------------------------------------------}

{ Change log:

  Version 1.2 (11.3.2012)
  - updated: Some color manipulation routines and type declarations have been moved
             into this unit from DecoGDI.pas and DecoGDIP.pas.

  Version 1.1 (26.9.2011)
  - added: Compatibility with Delphi XE2

  Version 1.0 (17.7.2011)
  - The first release
}

{ Common constants and types definition for DecoCharts components. }
unit DecoCommon;

interface

uses
  Windows, Graphics, Classes;

const
  { Mapping of TAlignment to Flags parameter of DrawText function }
  DrawTextAlignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);

type
  { Smoothing mode for GDI+ drawing }
  TRenderSmoothingMode = (rsmNone, rsmDefault, rsmLowQuality, rsmHighQuality, rsmAntiAlias);
  { Text rendering mode for GDI+ drawing }
  TRenderTextMode = (rtmDefault, rtmLowQuality, rtmMiddleQuality, rtmClearType, rtmAntiAlias, rtmAntiAliasHQ);
  { Composition mode for GDI+ drawing }
  TRenderCompositingMode = (rcmDefault, rcmLowQuality, rcmLinear, rcmHighQuality, rcmGammaCorrected);

  {$IF CompilerVersion < 18.0}
  TVerticalAlignment = (taAlignTop, taAlignBottom, taVerticalCenter);
  {$IFEND}

  { A kind of text displayed in progress bars }
  TShowValueInBar = (ibvNone, ibvValue, ibvPercent);
  { Background filling gradient style }
  TPGGradientStyle = (pggNone, pggVertical, pggMirror, pggLightMirror, pggGlass);

  { Common custom draw paint event definition }
  TDecoPaintEvent = procedure(Sender: TObject; TargetCanvas: TCanvas) of object;

  {$IFDEF UNICODE}
  { Draw text function reference definition }
  TDrawTextFunc = function(DC: HDC; const Text: UnicodeString; var TextRect: TRect; TextFlags: Cardinal; TextColor: TColor = clWindowText; GlowSize: Integer = 0): Integer;
  {$ELSE}
  TDrawTextFunc = function(DC: HDC; const Text: string; var TextRect: TRect; TextFlags: Cardinal; TextColor: TColor = clWindowText; GlowSize: Integer = 0): Integer;
  {$ENDIF}

{ Returns darker color }
function DarkColor(Color:TColor; Pct: Integer):TColor;
{ Returns brighter color }
function BrightColor(Color:TColor; Pct: Integer):TColor;
{ Returns a color with modified Hue, Luminance and Saturation percentage }
function ModColorHLS(Color: TColor; const HPct, LPct, SPct: Single): TColor;
{ Returns gray-scaled color }
function GrayScaleColor(Color:TColor):TColor;

{ This function returns a direct reference to the private FItems field of TCollection class.
  It is used for fast sorting and moving in an inherited classes. See also DecoUseSpeedSort variable. }
function GetCollectionList(C: TCollection): TList;

var
  { Allows you to use a non-standard workaround for fast sorting of large amounts of various DecoCharts items (collections).
    It must be set to True at run-time, anywhere in your project, but before the affected collection/component is created
    (ie. in initialization section or in the project main file), if you want to use it. }
  DecoUseSpeedSort:Boolean = False;

implementation

uses
  GraphUtil;

const
  RGBPctIncr = 255 * 0.01;
  HLSPctIncr = 240 * 0.01;

function DarkColor(Color:TColor; Pct:Integer):TColor;
var
  r, g, b: Integer;
  c: Longint;
begin
  c:=ColorToRGB(Color);
  r := Round(Byte(c)-Pct*RGBPctIncr);
  g := Round(Byte(c shr 8)-Pct*RGBPctIncr);
  b := Round(Byte(c shr 16)-Pct*RGBPctIncr);
  if r<0 then r:=0;
  if g<0 then g:=0;
  if b<0 then b:=0;
  Result := RGB(r, g, b);
end;

function BrightColor(Color:TColor; Pct:Integer):TColor;
var
  r, g, b: Integer;
  c: Longint;
begin
  c:=ColorToRGB(Color);
  r := Round(Byte(c)+Pct*RGBPctIncr);
  g := Round(Byte(c shr 8)+Pct*RGBPctIncr);
  b := Round(Byte(c shr 16)+Pct*RGBPctIncr);
  if r>255 then r:=255;
  if g>255 then g:=255;
  if b>255 then b:=255;
  Result := RGB(r, g, b);
end;

function ModColorHLS(Color: TColor; const HPct, LPct, SPct: Single): TColor;
var
  H,L,S:Word;
  Temp:Integer;
begin
  ColorRGBToHLS(ColorToRGB(Color),H,L,S);
  if HPct<>0 then
  begin
    Temp := Round(H + HPct * HLSPctIncr);
    if Temp < 0 then H := 0 else if Temp>240 then H:=240 else H:=Temp;
  end;
  if LPct<>0 then
  begin
    Temp := Round(L + LPct * HLSPctIncr);
    if Temp < 0 then L := 0 else if Temp>240 then L:=240 else L:=Temp;
  end;
  if SPct<>0 then
  begin
    Temp := Round(S + SPct * HLSPctIncr);
    if Temp < 0 then S := 0 else if Temp>240 then S:=240 else S:=Temp;
  end;
  Result:=ColorHLSToRGB(H,L,S);
end;

function GrayScaleColor(Color:TColor):TColor;
begin
  Result:=ModColorHLS(Color,0,0,-100);
end;

function GetCollectionList(C: TCollection): TList;
var
  M: TMethod;
  L: TList absolute M;
  P: Pointer;
begin
  try
    P := @C.ItemClass;
    inc({$IFDEF WIN64} Int64 {$ELSE} Integer {$ENDIF}(P), SizeOf(TCollectionItemClass));
    M := TMethod(P^);
    Result := L;
    if not Result.ClassNameIs('TList') then
      Result := nil;
  except
    Result := nil;
  end;
end;

end.
