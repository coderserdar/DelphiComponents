{------------------------------------------------------------------------------
  DecoGDI.pas

  DecoCharts for VCL

  written by  Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  Purpose:    Miscellaneous drawing functions and procedures

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
  - added: DrawProgressBar function
  - updated: Some color manipulation routines have been moved into DecoGDI.pas

  Version 1.0 (2011-07-17)
  - The first release
}

{ Miscellaneous drawing functions and procedures }
unit DecoGDI;

interface

uses
  Windows, Classes, Graphics, DecoCommon;

{ Helper function for filling a gradient }
procedure FillGradient(Canvas:TCanvas; ARect: TRect; StartColor, EndColor: TColor; Horizontal:Boolean=False);

{$IFDEF UNICODE}
{ Helper function for DrawText }
function DoDrawNormalText(DC: HDC; const Text: UnicodeString; var TextRect: TRect; TextFlags: Cardinal; TextColor: TColor = clWindowText; GlowSize: Integer = 0): Integer;
{$ELSE}
function DoDrawNormalText(DC: HDC; const Text: String; var TextRect: TRect; TextFlags: Cardinal; TextColor: TColor = clWindowText; GlowSize: Integer = 0): Integer;
{$ENDIF}

procedure DrawProgressBar(TargetCanvas:TCanvas; cRect:TRect; Value, MinValue, MaxValue: Extended; FGradient: TPGGradientStyle;
  Color, FBackColor, FFrameColor: TColor; FFrameWidth:Integer;
  FRounded:Boolean; FRoundCorners:Integer;
  FFrameProgress, ShowOverMaxIndicator:Boolean;
  FLevel1Color: TColor;
  FLevel1TextColor: TColor;
  FLevel2Color: TColor;
  FLevel2Percent: Integer;
  FLevel2TextColor: TColor;
  FLevel3Color: TColor;
  FLevel3Percent: Integer;
  FLevel3TextColor: TColor;
  FShowValueInBar: TShowValueInBar;
  Enabled, _EMFExporting:boolean);

implementation

uses
  GraphUtil, SysUtils {$IF CompilerVersion >= 24}, System.Types {$IFEND};

{$IF CompilerVersion >= 20.0}
procedure FillGradient(Canvas:TCanvas; ARect: TRect; StartColor, EndColor: TColor; Horizontal:Boolean=False);
begin
  if Horizontal then
    GradientFillCanvas(Canvas,StartColor,EndColor,ARect, gdHorizontal)
  else
    GradientFillCanvas(Canvas,StartColor,EndColor,ARect, gdVertical);
end;
{$ELSE}
procedure FillGradient(Canvas:TCanvas; ARect: TRect; StartColor, EndColor: TColor; Horizontal:Boolean=False);
var
  StartRGB: array [0..2] of Byte;
  RGBKoef: array [0..2] of Double;
  Brush: HBRUSH;
  AreaWidth, AreaHeight, I: Integer;
  ColorRect: TRect;
  RectOffset: Double;
  ColorCount: Integer;
  DC: HDC;
begin
  DC:=Canvas.Handle;
  ColorCount:=ARect.Bottom-ARect.Top;
  if ColorCount<1 then
    ColorCount:=1;
  StartColor := ColorToRGB(StartColor);
  EndColor := ColorToRGB(EndColor);
  StartRGB[0] := GetRValue(StartColor);
  StartRGB[1] := GetGValue(StartColor);
  StartRGB[2] := GetBValue(StartColor);
  RGBKoef[0] := (GetRValue(EndColor) - StartRGB[0]) / ColorCount;
  RGBKoef[1] := (GetGValue(EndColor) - StartRGB[1]) / ColorCount;
  RGBKoef[2] := (GetBValue(EndColor) - StartRGB[2]) / ColorCount;
  AreaWidth := ARect.Right - ARect.Left;
  AreaHeight :=  ARect.Bottom - ARect.Top;
  if Horizontal then
    RectOffset := AreaWidth / ColorCount
  else
    RectOffset := AreaHeight / ColorCount;
  for I := 0 to ColorCount - 1 do
  begin
    Brush := CreateSolidBrush(RGB(
      StartRGB[0] + Round((I + 1) * RGBKoef[0]),
      StartRGB[1] + Round((I + 1) * RGBKoef[1]),
      StartRGB[2] + Round((I + 1) * RGBKoef[2])));
    if Horizontal then
      SetRect(ColorRect, Round(RectOffset * I), 0, Round(RectOffset * (I + 1)), AreaHeight)
    else
      SetRect(ColorRect, 0, Round(RectOffset * I), AreaWidth, Round(RectOffset * (I + 1)));
    OffsetRect(ColorRect, ARect.Left, ARect.Top);
    FillRect(DC, ColorRect, Brush);
    DeleteObject(Brush);
  end;
end;
{$IFEND}

{$IFDEF UNICODE}
function DoDrawNormalText(DC: HDC; const Text: UnicodeString; var TextRect: TRect; TextFlags: Cardinal; TextColor: TColor = clWindowText; GlowSize: Integer = 0): Integer;
begin
  Result:=Windows.DrawTextW(DC, Text, Length(Text), TextRect, TextFlags);
end;
{$ELSE}
function DoDrawNormalText(DC: HDC; const Text: String; var TextRect: TRect; TextFlags: Cardinal; TextColor: TColor = clWindowText; GlowSize: Integer = 0): Integer;
begin
  Result:=Windows.DrawText(DC, PChar(Text), Length(Text), TextRect, TextFlags);
end;
{$ENDIF}

procedure DrawProgressBar(TargetCanvas:TCanvas; cRect:TRect; Value, MinValue, MaxValue: Extended; FGradient: TPGGradientStyle;
  Color, FBackColor, FFrameColor: TColor; FFrameWidth:Integer;
  FRounded:Boolean; FRoundCorners:Integer;
  FFrameProgress, ShowOverMaxIndicator:Boolean;
  FLevel1Color: TColor;
  FLevel1TextColor: TColor;
  FLevel2Color: TColor;
  FLevel2Percent: Integer;
  FLevel2TextColor: TColor;
  FLevel3Color: TColor;
  FLevel3Percent: Integer;
  FLevel3TextColor: TColor;
  FShowValueInBar: TShowValueInBar;
  Enabled, _EMFExporting:boolean);
var
  j:Integer;
  RC:HRGN;
  tmpClr,invClr,bkgClr,frmClr:TColor;
  rcs:Integer;
  vRect,bRect:TRect;
  Flags: Longint;
  percent:Extended;
  tmpStr:string;

  function _GetRoundedCornersFactor:Integer;
  begin
    if FRoundCorners<=0 then
      Result:=(cRect.Bottom - cRect.Top - 1) - (cRect.Bottom - cRect.Top - 1) div 5
    else
      Result:=FRoundCorners;
  end;

begin
  if cRect.Left+24>=cRect.Right then
    Exit;

  rcs:=_GetRoundedCornersFactor;
  if Enabled then
  begin
    bkgClr:=FBackColor;
    frmClr:=FFrameColor;
  end
  else
  begin
    bkgClr:=GrayScaleColor(FBackColor);
    frmClr:=GrayScaleColor(FFrameColor);
  end;
  percent:=(Value-MinValue)/(MaxValue-MinValue);
  // setup color by level
  if percent*100>=FLevel3Percent then
  begin
    tmpClr:=FLevel3Color;
    invClr:=FLevel3TextColor;
  end
  else
  if percent*100>=FLevel2Percent then
  begin
    tmpClr:=FLevel2Color;
    invClr:=FLevel2TextColor;
  end
  else
  begin
    tmpClr:=FLevel1Color;
    invClr:=FLevel1TextColor;
  end;
  if not Enabled then
  begin
    tmpClr:=GrayScaleColor(tmpClr);
    invClr:=GrayScaleColor(invClr);
  end;

  if FRounded then
  begin
    RC := CreateRoundRectRgn(cRect.Left,cRect.Top,cRect.Right+1,cRect.Bottom+1,rcs,rcs);
    SelectClipRgn(TargetCanvas.Handle,RC);
  end
  else
    RC:=0;

  TargetCanvas.Brush.Style:=bsSolid;
  TargetCanvas.Pen.Style:=psSolid;
  TargetCanvas.Pen.Width:=1;
  vRect:=cRect;
  if percent<0 then
    vRect.Right:=vRect.Left
  else
    vRect.Right:=vRect.Left+Round((vRect.Right-vRect.Left)*percent);
  if vRect.Right>=cRect.Right then
  begin
    if vRect.Right>cRect.Right then
      vRect.Right:=cRect.Right;
  end
  else
  begin
    bRect:=cRect;
    bRect.Left:=vRect.Right;
    // background rect
    case FGradient of
      pggVertical:FillGradient(TargetCanvas,bRect,BrightColor(bkgClr,15),DarkColor(bkgClr,8));
      pggMirror:
        begin
          j:=bRect.Top+(bRect.Bottom-bRect.Top) div 2;
          FillGradient(TargetCanvas,Rect(bRect.Left,bRect.Top,bRect.Right,j),BrightColor(bkgClr,15),DarkColor(bkgClr,9));
          FillGradient(TargetCanvas,Rect(bRect.Left,j,bRect.Right,bRect.Bottom),DarkColor(bkgClr,8),BrightColor(bkgClr,15));
        end;
      pggLightMirror:
        begin
          j:=bRect.Top+(bRect.Bottom-bRect.Top) div 2;
          FillGradient(TargetCanvas,Rect(bRect.Left,bRect.Top,bRect.Right,j),BrightColor(bkgClr,30),bkgClr);
          FillGradient(TargetCanvas,Rect(bRect.Left,j,bRect.Right,bRect.Bottom),bkgClr,BrightColor(bkgClr,30));
        end;
      pggGlass:
        begin
          j:=bRect.Top+(bRect.Bottom-bRect.Top) div 2;
          FillGradient(TargetCanvas,Rect(bRect.Left,bRect.Top,bRect.Right,j),BrightColor(bkgClr,20),BrightColor(bkgClr,5));
          FillGradient(TargetCanvas,Rect(bRect.Left,j,bRect.Right,bRect.Bottom),bkgClr,BrightColor(bkgClr,10));
        end;
    else
      begin
        TargetCanvas.Brush.Color:=bkgClr;
        TargetCanvas.Pen.Color:=bkgClr;
        TargetCanvas.Rectangle(bRect);
      end;
    end;
  end;
  // value rect
  if vRect.Right>vRect.Left then
  begin
    case FGradient of
      pggVertical:FillGradient(TargetCanvas,vRect,BrightColor(tmpClr,15),DarkColor(tmpClr,8));
      pggMirror:
        begin
          j:=vRect.Top+(vRect.Bottom-vRect.Top) div 2;
          FillGradient(TargetCanvas,Rect(vRect.Left,vRect.Top,vRect.Right,j),BrightColor(tmpClr,15),DarkColor(tmpClr,9));
          FillGradient(TargetCanvas,Rect(vRect.Left,j,vRect.Right,vRect.Bottom),DarkColor(tmpClr,8),BrightColor(tmpClr,15));
        end;
      pggLightMirror:
        begin
          j:=vRect.Top+(vRect.Bottom-vRect.Top) div 2;
          FillGradient(TargetCanvas,Rect(vRect.Left,vRect.Top,vRect.Right,j),BrightColor(tmpClr,30),tmpClr);
          FillGradient(TargetCanvas,Rect(vRect.Left,j,vRect.Right,vRect.Bottom),tmpClr,BrightColor(tmpClr,30));
        end;
      pggGlass:
        begin
          j:=vRect.Top+(vRect.Bottom-vRect.Top) div 2;
          FillGradient(TargetCanvas,Rect(vRect.Left,vRect.Top,vRect.Right,j),BrightColor(tmpClr,20),BrightColor(tmpClr,5));
          FillGradient(TargetCanvas,Rect(vRect.Left,j,vRect.Right,vRect.Bottom),tmpClr,BrightColor(tmpClr,10));
        end;
    else
      begin
        TargetCanvas.Brush.Color:=tmpClr;
        TargetCanvas.Pen.Color:=tmpClr;
        TargetCanvas.Rectangle(vRect);
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
      case FGradient of
        pggNone:
          begin
            TargetCanvas.Pen.Color:=DarkColor(tmpClr,10);
            TargetCanvas.MoveTo(j-1,cRect.Top); TargetCanvas.LineTo(j-1,cRect.Bottom);
            TargetCanvas.Pen.Color:=BrightColor(tmpClr,10);
            TargetCanvas.MoveTo(j,cRect.Top); TargetCanvas.LineTo(j,cRect.Bottom);
          end;
      else
          begin
            FillGradient(TargetCanvas,Rect(j-1,cRect.Top,j,cRect.Bottom),BrightColor(tmpClr,8),DarkColor(tmpClr,15));
            FillGradient(TargetCanvas,Rect(j,cRect.Top,j+1,cRect.Bottom),BrightColor(tmpClr,38),BrightColor(tmpClr,15));
          end;
      end;
    end;
  end;

  if FRounded then
    SelectClipRgn(TargetCanvas.Handle, HRGN(nil));

  // draw frame
  if _EMFExporting or ((FFrameColor<>clNone) and (FFrameWidth>0)) then
  begin
    TargetCanvas.Brush.Style:=bsClear;
    TargetCanvas.Pen.Style:=psSolid;
    if _EMFExporting then
    begin
      TargetCanvas.Pen.Color:=Color;
      TargetCanvas.Pen.Width:=3;
      if FRounded then
        TargetCanvas.RoundRect(cRect.Left-1,cRect.Top-1,cRect.Right+1,cRect.Bottom+1,rcs,rcs)
      else
        TargetCanvas.Rectangle(cRect.Left-1,cRect.Top-1,cRect.Right+1,cRect.Bottom+1);
    end;
    if (FFrameColor<>clNone) and (FFrameWidth>0) then
    begin
      TargetCanvas.Pen.Color:=frmClr;
      TargetCanvas.Pen.Width:=FFrameWidth;
      if FRounded then
        {$IF CompilerVersion >= 20}
        TargetCanvas.RoundRect(cRect,rcs,rcs)
        {$ELSE}
        TargetCanvas.RoundRect(cRect.Left,cRect.Top,cRect.Right,cRect.Bottom,rcs,rcs)
        {$IFEND}
      else
        TargetCanvas.Rectangle(cRect);
    end;
  end;

  if FFrameProgress and (vRect.Right>vRect.Left) then
  begin
    TargetCanvas.Brush.Style:=bsClear;
    TargetCanvas.Pen.Color:=DarkColor(tmpClr,15);
    TargetCanvas.Pen.Width:=FFrameWidth;
    if FRounded then
    begin
      ExcludeClipRect(TargetCanvas.Handle,vRect.Right+(FFrameWidth div 2),vRect.Top-FFrameWidth,cRect.Right+(FFrameWidth div 2),vRect.Bottom+FFrameWidth);
      {$IF CompilerVersion >= 20}
      TargetCanvas.RoundRect(cRect,rcs,rcs);
      {$ELSE}
      TargetCanvas.RoundRect(cRect.Left,cRect.Top,cRect.Right,cRect.Bottom,rcs,rcs);
      {$IFEND}
      if (vRect.Right>cRect.Left) and (vRect.Right<cRect.Right) then
      begin
        SelectClipRgn(TargetCanvas.Handle,RC);
        TargetCanvas.Polyline([Point(vRect.Right,vRect.Top),Point(vRect.Right,vRect.Bottom-(FFrameWidth div 2)-Byte(_EMFExporting))]);
      end;
      SelectClipRgn(TargetCanvas.Handle, HRGN(nil));
    end
    else
      TargetCanvas.Rectangle(vRect);
  end;

  if RC<>0 then
    DeleteObject(RC);

  if FShowValueInBar<>ibvNone then
  begin
    TargetCanvas.Brush.Style:=bsClear;
    TargetCanvas.Font.Color:=invClr;
    Dec(cRect.Bottom);
    Flags := DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_CENTER;
    case FShowValueInBar of
      ibvValue:tmpStr:=FloatToStr(Value);
      ibvPercent:tmpStr:=IntToStr(Round(percent*100))+'%';
    else
      tmpStr:='';
    end;
    if Length(tmpStr)>0 then
      DoDrawNormalText(TargetCanvas.Handle, tmpStr, cRect, Flags, TargetCanvas.Font.Color);
  end;
end;

end.
