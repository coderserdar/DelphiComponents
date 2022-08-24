{------------------------------------------------------------------------------
  DecoGDIP.pas

  DecoCharts for VCL

  written by  Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  Purpose:    Miscellaneous drawing functions and procedures for GDI+

  Notes:      Requires a dynamic loading implementation of GDI+ API library
              ( http://www.progdigy.com, http://themiron.mirandaim.ru )

  The source code is given as is. The author is not responsible
  for any possible damage done due to the use of this code.
  You can freely use this component in your products, if you have purchased
  the license. Except where otherwise noted, the complete source code remains
  property of the author and may not be distributed, published, given or sold
  in any form as such. No parts of the source code can be included in any
  other component or application without written authorization of the author.

  Copyright (c) 2008-2013  Precision software & consulting
  All rights reserved
------------------------------------------------------------------------------}

{ Change log:

  Version 1.3 (2013-11-18)
  - added: Support for Delphi XE3/XE4/XE5
  - improved: Minor improvements and fixes

  Version 1.2 (2012-03-11)
  - added: GDIP_DrawProgressBar function
  - updated: Some types declarations have been moved into DecoCommon.pas unit.

  Version 1.0 (2011-07-17)
  - The first release
}

{ Miscellaneous drawing functions and procedures for GDI+ }
unit DecoGDIP;

interface

uses
  Windows, Classes, Graphics, Themes, DecoCommon,
  {$IF CompilerVersion >= 23}
  Winapi.GDIPAPI, Winapi.GDIPOBJ
  {$ELSE}
  xGDIPAPI, xGDIPOBJ
  {$IFEND}
  ;

const
  brush_mirror_Intensities : array[0..2] of Single = (0.0, 1.0, 0.0);
  brush_mirror_Positions : array[0..2] of Single = (0.0, 0.5, 1.0);
  brush_glass_Intensities : array[0..3] of Single = (0.0, 0.6, 1.0, 0.5);
  brush_glass_Positions : array[0..3] of Single = (0.0, 0.49, 0.5, 1.0);

{ Sets common rendering options for passed graphics objects }
procedure GDIP_SetRenderingOptions(GP:TGPGraphics; aSmoothingMode:TRenderSmoothingMode; aTextMode:TRenderTextMode; aCompositingMode: TRenderCompositingMode);

{ Converts Delphi TColor to GDI+ color along with alpha channel }
function ColorToARGB(Color: TColor; Alpha: Byte=255): ARGB;
{ Returns GDI+ floating-point rectangle (TGPRectF) from Delphi rectangle (TRect) }
function GDIP_RectF(Rect:TRect):TGPRectF;

{ Returns rounded rectangle path based on Delphi rectangle and round-corner factor }
function GDIP_RoundRectanglePath(Rectangle: TRect; CX: Integer): TGPGraphicsPath;
{ Draws rounded rectangle by using Pen and Brush (both can be nil) }
procedure GDIP_RoundRectangle(GP:TGPGraphics; Rectangle: TRect; CX: Integer; Pen:TGPPen; Brush: TGPBrush);

{$IFDEF UNICODE}
{ Helper function for DrawThemeTextEx }
function DoDrawThemeTextEx(DC: HDC; const Text: UnicodeString; var TextRect: TRect; TextFlags: Cardinal; TextColor: TColor = clWindowText; GlowSize: Integer = 0): Integer;
{$ELSE}
function DoDrawThemeTextEx(DC: HDC; const Text: String; var TextRect: TRect; TextFlags: Cardinal; TextColor: TColor = clWindowText; GlowSize: Integer = 0): Integer;
{$ENDIF}

{ Returns GDI+ font style based on passed Delphi font style }
function GDIP_FontStyle(FS:TFontStyles):Integer;
{ Returns GDI+ font family object based on passed font name }
function GDIP_FontFamily(const FontName:string):TGPFontFamily;


{ Draws text using GDI+ DrawString method }
procedure GDIP_DrawText(GP:TGPGraphics; const Text: string; TextRect:TRect; Alignment: TAlignment; VAlignment: TVerticalAlignment;
  const FontName: string; FontSize: Integer; FontStyle: TFontStyles; FontColor:TColor; WordWrap:Boolean=False; AlphaBlend:Byte=255);

procedure GDIP_DrawProgressBar(TargetCanvas:TCanvas; GDIPCanvas: TGPGraphics; cRect:TRect; Value, MinValue, MaxValue: Extended; Gradient: TPGGradientStyle;
  Color, BackColor, FrameColor: TColor; FrameWidth:Integer;
  Rounded:Boolean; RoundCorners:Integer;
  FrameProgress, ShowOverMaxIndicator:Boolean;
  Level1Color: TColor;
  Level1TextColor: TColor;
  Level2Color: TColor;
  Level2Percent: Integer;
  Level2TextColor: TColor;
  Level3Color: TColor;
  Level3Percent: Integer;
  Level3TextColor: TColor;
  ShowValueInBar: TShowValueInBar;
  Enabled, _EMFExporting:Boolean;
  FDrawTextFunc: TDrawTextFunc;
  GlowSize:integer);

{$IF CompilerVersion < 23.0 }
function StyleServices: TThemeServices;
{$IFEND}

implementation

uses
  UxTheme, SysUtils {$IF CompilerVersion >= 24}, System.Types {$IFEND};

{$IF CompilerVersion < 23.0 }
function StyleServices: TThemeServices;
begin
  Result := ThemeServices;
end;
{$IFEND}

procedure GDIP_SetRenderingOptions(GP:TGPGraphics; aSmoothingMode:TRenderSmoothingMode; aTextMode:TRenderTextMode; aCompositingMode: TRenderCompositingMode);
begin
  // set options
  case aSmoothingMode of
    rsmNone:GP.SetSmoothingMode(SmoothingModeNone);
    rsmLowQuality:GP.SetSmoothingMode(SmoothingModeHighSpeed);
    rsmHighQuality:GP.SetSmoothingMode(SmoothingModeHighQuality);
    rsmAntiAlias:GP.SetSmoothingMode(SmoothingModeAntiAlias);
  {else
    GP.SetSmoothingMode(SmoothingModeDefault);}
  end;
  case aTextMode of
    rtmLowQuality:GP.SetTextRenderingHint(TextRenderingHintSingleBitPerPixel);
    rtmMiddleQuality:GP.SetTextRenderingHint(TextRenderingHintSingleBitPerPixelGridFit);
    rtmClearType:GP.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
    rtmAntiAlias:GP.SetTextRenderingHint(TextRenderingHintAntiAlias);
    rtmAntiAliasHQ:GP.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);
  {else
    GP.SetTextRenderingHint(TextRenderingHintSystemDefault);}
  end;
  case aCompositingMode of
    rcmLowQuality:GP.SetCompositingQuality(CompositingQualityHighSpeed);
    rcmLinear:GP.SetCompositingQuality(CompositingQualityAssumeLinear);
    rcmHighQuality:GP.SetCompositingQuality(CompositingQualityHighQuality);
    rcmGammaCorrected:GP.SetCompositingQuality(CompositingQualityGammaCorrected);
  {else
    GP.SetCompositingQuality(CompositingQualityDefault)}
  end;
end;

function ColorToARGB(Color: TColor; Alpha: Byte=255): ARGB;
var
  rgb: COLORREF;
begin
  rgb := ColorToRGB(Color);
  Result := MakeColor(Alpha, GetRValue(rgb), GetGValue(rgb), GetBValue(rgb));
end;

function GDIP_RectF(Rect:TRect):TGPRectF;
begin
  Result.X := Rect.Left; Result.Y := Rect.Top; Result.Width := Rect.Right-Rect.Left; Result.Height:=Rect.Bottom-Rect.Top;
end;

function GDIP_RoundRectanglePath(Rectangle: TRect; CX: Integer): TGPGraphicsPath;
var
  l, t, w, h: Double;
  r:Integer;
begin
  Result := TGPGraphicsPath.Create;
  l := Rectangle.Left;
  t := Rectangle.Top;
  w := Rectangle.Right-Rectangle.Left;
  h := Rectangle.Bottom-Rectangle.Top;
  cx:=CX div 2;
  if CX>h/2 then cx:=Trunc(h/2);
  if CX>w/2 then cx:=Trunc(w/2);
  r := CX * 2;

  Result.AddArc(l, t, r, r, 180, 90); // topleft
  Result.AddLine(l + CX, t, l + w - CX, t); // top
  Result.AddArc(l + w - r, t, r, r, 270, 90); // topright
  Result.AddLine(l + w, t + CX, l + w, t + h - CX); // right
  Result.AddArc(l + w - r, t + h - r, r, r, 0, 90); // bottomright
  Result.AddLine(l + w - CX, t + h, l + CX, t + h); // bottom
  Result.AddArc(l, t + h - r, r, r, 90, 90); // bottomleft
  Result.AddLine(l, t + h - CX, l, t + CX); // left
  Result.CloseFigure();
end;

procedure GDIP_RoundRectangle(GP:TGPGraphics; Rectangle: TRect; CX: Integer; Pen:TGPPen; Brush: TGPBrush);
var
  path : TGPGraphicsPath;
begin
  path := GDIP_RoundRectanglePath(Rectangle, CX);
  if Assigned(Brush) then
    GP.FillPath(brush,path);
  if Assigned(Pen) then
    GP.DrawPath(Pen,path);
  path.Free;
end;

////////////////////////////////////////////// FONTS AND TEXTS ///////////////////////////////////////////////////
{$IFDEF UNICODE}
function DoDrawThemeTextEx(DC: HDC; const Text: UnicodeString; var TextRect: TRect; TextFlags: Cardinal; TextColor: TColor = clWindowText; GlowSize: Integer = 0): Integer;
var
  Options: TDTTOpts;
begin
  FillChar(Options, SizeOf(Options), 0);
  Options.dwSize := SizeOf(Options);
  Options.dwFlags := DTT_TEXTCOLOR or DTT_COMPOSITED or DTT_GLOWSIZE;
  if TextFlags and DT_CALCRECT = DT_CALCRECT then
    Options.dwFlags := Options.dwFlags or DTT_CALCRECT;
  Options.crText := ColorToRGB(TextColor);
  Options.iGlowSize := GlowSize;
  Result:=DrawThemeTextEx(StyleServices.Theme[teWindow], DC, WP_CAPTION, CS_ACTIVE, Text, Length(Text), TextFlags, TextRect, Options);
end;
{$ELSE}
function DoDrawThemeTextEx(DC: HDC; const Text: String; var TextRect: TRect; TextFlags: Cardinal; TextColor: TColor = clWindowText; GlowSize: Integer = 0): Integer;
begin
  Result:=Windows.DrawText(DC, PChar(Text), Length(Text), TextRect, TextFlags);
end;
{$ENDIF}

function GDIP_FontStyle(FS:TFontStyles):Integer;
begin
  Result := FontStyleRegular;
  if fsBold in FS then Result := Result + FontStyleBold;
  if fsItalic in FS then Result := Result + FontStyleItalic;
  if fsUnderline in FS then Result := Result + FontStyleUnderline;
  if fsStrikeOut in FS then Result := Result + FontStyleStrikeout;
end;

function GDIP_FontFamily(const FontName:string):TGPFontFamily;
begin
  Result := TGPFontFamily.Create(FontName);
  if (Result.GetLastStatus in [FontFamilyNotFound, FontStyleNotFound]) then
  begin
    Result.Free;
    Result := TGPFontFamily.Create('Arial');
  end;
end;

procedure GDIP_DrawText(GP:TGPGraphics; const Text: string; TextRect:TRect; Alignment: TAlignment; VAlignment: TVerticalAlignment;
  const FontName: string; FontSize: Integer; FontStyle: TFontStyles; FontColor:TColor; WordWrap:Boolean=False; AlphaBlend:Byte=255);
var
  FontFamily: TGPFontFamily;
  Font: TGPFont;
  SolidBrush: TGPBrush;
  stringFormat: TGPStringFormat;
  fs:Integer;
  RectF:TGPRectF;
begin
  if Length(Text)>0 then
  begin
    FontFamily := TGPFontFamily.Create(FontName);
    if (FontFamily.GetLastStatus in [FontFamilyNotFound, FontStyleNotFound]) then
    begin
      FontFamily.Free;
      FontFamily := TGPFontFamily.Create('Arial');
    end;
    fs := FontStyleRegular;
    if fsBold in FontStyle then fs := fs + FontStyleBold;
    if fsItalic in FontStyle then fs := fs + FontStyleItalic;
    if fsUnderline in FontStyle then fs := fs + FontStyleUnderline;
    if fsStrikeOut in FontStyle then fs := fs + FontStyleStrikeout;

    Font := TGPFont.Create(FontFamily, FontSize, fs, UnitPoint);
    solidBrush := TGPSolidBrush.Create(ColorToARGB(FontColor, AlphaBlend));
    RectF.X := TextRect.Left; RectF.Y := TextRect.Top; RectF.Width := TextRect.Right-TextRect.Left; RectF.Height:=TextRect.Bottom-TextRect.Top;

    if WordWrap then
      stringFormat := TGPStringFormat.Create
    else
      stringFormat := TGPStringFormat.Create(StringFormatFlagsNoWrap);
    case Alignment of
      taCenter:stringFormat.SetAlignment(StringAlignmentCenter);
      taRightJustify: stringFormat.SetAlignment(StringAlignmentFar);
    else
      stringFormat.SetAlignment(StringAlignmentNear);
    end;
    case VAlignment of
      taAlignTop:stringFormat.SetLineAlignment(StringAlignmentNear);
      taAlignBottom:stringFormat.SetLineAlignment(StringAlignmentFar);
    else
      stringFormat.SetLineAlignment(StringAlignmentCenter);
    end;
    stringFormat.SetTrimming(StringTrimmingNone);

    GP.DrawString(Text, Length(Text), Font, RectF, stringFormat, SolidBrush);

    stringFormat.Free;
    solidBrush.Free;
    Font.Free;
    FontFamily.Free;
  end;
end;

procedure GDIP_DrawProgressBar(TargetCanvas:TCanvas; GDIPCanvas: TGPGraphics; cRect:TRect; Value, MinValue, MaxValue: Extended; Gradient: TPGGradientStyle;
  Color, BackColor, FrameColor: TColor; FrameWidth:Integer;
  Rounded:Boolean; RoundCorners:Integer;
  FrameProgress, ShowOverMaxIndicator:Boolean;
  Level1Color: TColor;
  Level1TextColor: TColor;
  Level2Color: TColor;
  Level2Percent: Integer;
  Level2TextColor: TColor;
  Level3Color: TColor;
  Level3Percent: Integer;
  Level3TextColor: TColor;
  ShowValueInBar: TShowValueInBar;
  Enabled, _EMFExporting:Boolean;
  FDrawTextFunc: TDrawTextFunc;
  GlowSize:integer);
var
  j:Integer;
  tmpClr,invClr,frmClr:TColor;
  rcs:Integer;
  vRect,bRect,tmpRect:TRect;
  Flags: Longint;
  percent:Extended;
  tmpStr:string;

  RPath: TGPGraphicsPath;
  RC: TGPRegion;
  gBrush, gbBrush: TGPBrush;
  gPen,sdPen,slPen: TGPPen;
  // oldSM: Integer;
  // bY,fY: Integer;
  alpha: Byte;
  eb:Boolean;

  function _GetRoundedCornersFactor:Integer;
  begin
    if RoundCorners<=0 then
      Result:=(cRect.Bottom - cRect.Top - 1) - (cRect.Bottom - cRect.Top - 1) div 5
    else
      Result:=RoundCorners;
  end;

begin
  if cRect.Left+24>=cRect.Right then
    Exit;

  rcs:=_GetRoundedCornersFactor;
  gPen := TGPPen.Create(ColorToARGB(FrameColor,255));
  sdPen := TGPPen.Create(ColorToARGB(clBlack,24));
  slPen := TGPPen.Create(ColorToARGB(clWhite,72));
  // bY := 0; fY := 0;
  eb:=Enabled;
  if BackColor=clNone then
  begin
    alpha:=24;
    tmpClr:=BrightColor(TargetCanvas.Font.Color,23)
  end
  else
  begin
    alpha:=255;
    tmpClr:=BackColor;
  end;
  if eb then
    frmClr:=FrameColor
  else
  begin
    frmClr:=GrayScaleColor(FrameColor);
    tmpClr:=GrayScaleColor(tmpClr);
  end;

  case Gradient of
    pggNone:
      begin
        gBrush:=TGPSolidBrush.Create(ColorToARGB(Color,255));
        gbBrush:=TGPSolidBrush.Create(ColorToARGB(tmpClr,alpha));
      end;
  else
    begin
      gBrush:=TGPLinearGradientBrush.Create(MakePoint(0,0),MakePoint(0,cRect.Bottom-cRect.Top),ColorToARGB(clWhite),ColorToARGB(clBlack));
      gbBrush:=TGPLinearGradientBrush.Create(MakePoint(0,0),MakePoint(0,cRect.Bottom-cRect.Top),ColorToARGB(BrightColor(tmpClr,15),alpha),ColorToARGB(DarkColor(tmpClr,8),alpha));
      case Gradient of
        pggMirror, pggLightMirror:
          begin
            TGPLinearGradientBrush(gBrush).SetBlend(@brush_mirror_Intensities, @brush_mirror_Positions, 3);
            TGPLinearGradientBrush(gbBrush).SetBlend(@brush_mirror_Intensities, @brush_mirror_Positions, 3);
          end;
        pggGlass:
          begin
            TGPLinearGradientBrush(gBrush).SetBlend(@brush_glass_Intensities, @brush_glass_Positions, 4);
            TGPLinearGradientBrush(gbBrush).SetBlend(@brush_glass_Intensities, @brush_glass_Positions, 4);
          end;
      end;
      case Gradient of
        pggLightMirror:TGPLinearGradientBrush(gbBrush).SetLinearColors(ColorToARGB(BrightColor(tmpClr,29),alpha),ColorToARGB(DarkColor(tmpClr,1),alpha));
        pggGlass:TGPLinearGradientBrush(gbBrush).SetLinearColors(ColorToARGB(BrightColor(tmpClr,21),alpha),ColorToARGB(DarkColor(tmpClr,2),alpha));
      end;
    end;
  end;

  try
    percent:=(Value-MinValue)/(MaxValue-MinValue);
    // setup color by level
    if percent*100>=Level3Percent then
    begin
      tmpClr:=Level3Color;
      invClr:=Level3TextColor;
    end
    else
    if percent*100>=Level2Percent then
    begin
      tmpClr:=Level2Color;
      invClr:=Level2TextColor;
    end
    else
    begin
      tmpClr:=Level1Color;
      invClr:=Level1TextColor;
    end;
    if not Enabled then
    begin
      tmpClr:=GrayScaleColor(tmpClr);
      invClr:=GrayScaleColor(invClr);
    end;

    InflateRect(cRect,-1,-1);
    if Rounded then
    begin
      RPath := GDIP_RoundRectanglePath(cRect, rcs);
      RC := TGPRegion.Create(RPath);
      RPath.Free;
      Dec(cRect.Top);
      RPath := GDIP_RoundRectanglePath(cRect, rcs);
    end
    else
    begin
      Dec(cRect.Top);
      RC:=nil;
      RPath:=nil;
    end;

    vRect:=cRect;
    if percent<0 then
      vRect.Right:=vRect.Left
    else
      vRect.Right:=vRect.Left+Round((vRect.Right-vRect.Left)*percent);
    if vRect.Right>=cRect.Right then
    begin
      if vRect.Right>cRect.Right then
        vRect.Right:=cRect.Right;
      SetRectEmpty(bRect);
    end
    else
    begin
      // background rect
      bRect:=cRect;
      bRect.Left:=vRect.Right;
      if gbBrush is TGPLinearGradientBrush then
      begin
        TGPLinearGradientBrush(gbBrush).TranslateTransform(0,bRect.Top{-bY},MatrixOrderAppend);
        // bY:=bRect.Top;
      end;
      if Rounded then
      begin
        if (vRect.Right>vRect.Left) then
        begin
          tmpRect:=vRect; inflateRect(tmpRect,0,FrameWidth); Dec(tmpRect.Left,FrameWidth);
          GDIPCanvas.ExcludeClip(MakeRect(tmpRect));
        end;
        GDIPCanvas.FillPath(gbBrush,RPath);
        GDIPCanvas.ResetClip;
      end
      else
      begin
        GDIPCanvas.FillRectangle(gbBrush,MakeRect(bRect));
      end;
    end;
    // value rect
    if vRect.Right>vRect.Left then
    begin
      if gBrush is TGPLinearGradientBrush then
      begin
        TGPLinearGradientBrush(gBrush).TranslateTransform(0,vRect.Top{-fY},MatrixOrderAppend);
        //fY:=vRect.Top;
      end;
      case Gradient of
        pggVertical, pggMirror:
          TGPLinearGradientBrush(gBrush).SetLinearColors(ColorToARGB(BrightColor(tmpClr,15)),ColorToARGB(DarkColor(tmpClr,8)));
        pggLightMirror:TGPLinearGradientBrush(gBrush).SetLinearColors(ColorToARGB(BrightColor(tmpClr,29)),ColorToARGB(DarkColor(tmpClr,1)));
        pggGlass:TGPLinearGradientBrush(gBrush).SetLinearColors(ColorToARGB(BrightColor(tmpClr,21)),ColorToARGB(DarkColor(tmpClr,2)));
      else
        TGPSolidBrush(gBrush).SetColor(ColorToARGB(tmpClr));
      end;
      if Rounded then
      begin
        if bRect.Right>bRect.Left then
        begin
          tmpRect:=bRect; inflateRect(tmpRect,-FrameWidth div 2-1,FrameWidth+1); Inc(tmpRect.Right,FrameWidth+2);
          GDIPCanvas.ExcludeClip(MakeRect(tmpRect));
        end;
        GDIPCanvas.FillPath(gBrush,RPath);
        if FrameProgress then
        begin
          gPen.SetColor(ColorToARGB(DarkColor(tmpClr,15)));
          gPen.SetWidth(FrameWidth);
          GDIPCanvas.DrawPath(gPen,RPath);
          GDIPCanvas.SetClip(RC);
          GDIPCanvas.DrawLine(gPen,vRect.Right,vRect.Top-1,vRect.Right,vRect.Bottom+1);
        end;
        GDIPCanvas.ResetClip;
      end
      else
      begin
        GDIPCanvas.FillRectangle(gBrush,MakeRect(vRect));
        if FrameProgress then
        begin
          gPen.SetColor(ColorToARGB(DarkColor(tmpClr,15)));
          gPen.SetWidth(FrameWidth);
          GDIPCanvas.DrawRectangle(gPen,MakeRect(vRect));
        end;
      end;
    end;

    if ShowOverMaxIndicator and ((percent<0) or (percent>1)) then
    begin
      if percent>1 then
        j:=cRect.Right-Round((cRect.Right-cRect.Left)*(percent-1))
      else
        j:=cRect.Left+Round((cRect.Right-cRect.Left)*(-percent));
      if (j>cRect.Left) and (j<cRect.Right) then
      begin
        GDIPCanvas.DrawLine(sdPen,j-1,cRect.Top,j-1,cRect.Bottom);
        GDIPCanvas.DrawLine(slPen,j,cRect.Top,j,cRect.Bottom);
      end;
    end;

    // draw frame
    if (FrameColor<>clNone) and (FrameWidth>0) and ((not FrameProgress) or (vRect.Right<cRect.Right)) then
    begin
      gPen.SetColor(ColorToARGB(frmClr));
      gPen.SetWidth(FrameWidth);
      if FrameProgress and (vRect.Right>vRect.Left) then
      begin
        tmpRect:=vRect; inflateRect(tmpRect,FrameWidth,FrameWidth); Dec(tmpRect.Right,FrameWidth div 2);
        GDIPCanvas.ExcludeClip(MakeRect(tmpRect));
      end;
      if Rounded then
        GDIPCanvas.DrawPath(gPen,RPath)
      else
        GDIPCanvas.DrawRectangle(gPen,MakeRect(cRect));
      GDIPCanvas.ResetClip;
    end;

    if ShowValueInBar<>ibvNone then
    begin
      TargetCanvas.Brush.Style:=bsClear;
      TargetCanvas.Font.Color:=invClr;
      //Dec(cRect.Bottom);
      Flags := DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_CENTER;
      case ShowValueInBar of
        ibvValue:tmpStr:=FloatToStr(Value);
        ibvPercent:tmpStr:=IntToStr(Round(percent*100))+'%';
      else
        tmpStr:='';
      end;
      if Length(tmpStr)>0 then
        FDrawTextFunc(TargetCanvas.Handle, tmpStr, cRect, Flags, TargetCanvas.Font.Color, GlowSize);
    end;


    if Assigned(RPath) then
      RPath.Free;
    if Assigned(RC) then
      RC.Free;
  finally
    sdPen.Free;
    slPen.Free;
    gBrush.Free;
    gbBrush.Free;
    gPen.Free;
  end;
end;

end.
