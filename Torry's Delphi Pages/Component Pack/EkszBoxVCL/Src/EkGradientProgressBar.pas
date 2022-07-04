unit EkGradientProgressBar;

//========================
// EkGradientProgressBar
//========================

// Copyright (C) 2007-2008 Kernel Master
// Author: Kernel Master
// kmeksz[At]yahoo.com

//======================================

// Contains code ported from TABExtProgressBar by Aaron Bockover
// Copyright (C) 2001-2002 Aaron Bockover - www.aaronbock.net

// .. which in turn was based on TxGauge by Harold Howe
// Copyright (C) 2000 Harold Howe and bcbdev.com

//==================================================================

// Gradient code based on code found in JVCL
// Copyright (C) Project JEDI

//============================================================

// Bugs
{
 - Round border not working
 - Gradient gets repainted upon each Paint(), not efficient,
 FRepaintIt was set to false after first paint/resize, but didn't work,
 ...Needs looking over again
 - Scroll txt not working
 - Dashed fill style not finished/working
 - Hint follow not finished/working
}

//========================================================================

//{$WARNINGS OFF}
//{$HINTS OFF}
{$O+} // Optimizations

//==============================================================================

interface

uses
	Windows, Classes, Controls, Graphics, Messages, SysUtils, ExtCtrls, Forms,
  EkLabel, EkTypes;

type
	TEkGradientProgressBar = class(TCustomControl)

	private

    FBmpBuf           : TBitmap;
    FBmpBack          : TBitmap;
    FBmpFront         : TBitmap;
    //FBmpFrontDashed   : TBitmap;

    FScrollTimer        : TTimer;
    FMouseTimer         : TTimer;

    FTextAlign          : TAlignment;
  	FBorder             : TEkBorders;
	  FFillStyle          : TEkFillStyles;
    FGradientFrontStyle : TEkGradientStyles;
    FGradientBackStyle  : TEkGradientStyles;
    FTextEllipsis       : TEkTextEllipsis;
    FOnChange           : TNotifyEvent;

    FOnMouseEnter       : TNotifyEvent;
    FOnMouseLeave       : TNotifyEvent;

    FTextForeColor      : TColor;
    FTextBackColor      : TColor;
    FBarForeColorFrom   : TColor;
    FBarForeColorTo     : TColor;
    FBarBackColorFrom   : TColor;
    FBarBackColorTo     : TColor;
    FSingleBorderColor  : TColor;

    FTextLast           : String;
    FText               : String;

    FCursorPos          : TPoint;
    FBarRect            : TRect;
    FEmptyRect          : TRect;

    FPercent            : Double;

    FWidth              : Integer;
    FHeight             : Integer;
    FCurAnimPos         : Integer;
    FCurBarPos          : Integer;
    FTextPos            : Integer;
    FTextLength         : Integer;
    FTextY              : Integer;
    FScrollInterval     : Integer;
    FBlockWidth         : Integer;
    FBlockSpace         : Integer;
    FGradientSteps      : Integer;
    FInnerBorderSize    : Integer;
    FMinValue           : Integer;
    FMaxValue           : Integer;
    FCurValue           : Integer;
    FXPixel             : Integer;
    FStep               : Integer;

    FScrollText         : Boolean;
    FIsMouseDown        : Boolean;
    FIsMouseEnter       : Boolean;
    FAutoUpdate         : Boolean;
    FShowText           : Boolean;
    {FShadowText         : Boolean;
    FShadowTextBlur     : Boolean;}
    FHasDashed          : Boolean;
    FTrackBarMode       : Boolean;
    FRepaintIt          : Boolean;

    procedure WMSize(var pMsg: TWMSize); message WM_SIZE;
    procedure PaintIt;
    procedure PaintFillFront(pic : TBitmap);
    procedure PaintFillBack(pic : TBitmap);
    procedure PaintGradient(typ : Integer; pic : TBitmap;
      frontBack : Integer; colorFrom : TColor; colorTo : TColor);

	  procedure PaintText(pic : TBitmap);
	  procedure PaintBorder(pCanvas : TCanvas);
	  procedure CreateRoundRgn(pCanvas : TCanvas);
    //procedure DashImage();

    procedure CalculatePercentDone();
	  procedure CalculatePixelWidth();

    procedure SetSingleBorderColor(value : TColor);
	  procedure SetInnerBorderSize(value : Integer);
	  procedure SetBorderStyle(value : TEkBorders);
	  procedure SetShowText(value : Boolean);
	  //procedure SetShadowText(value : Boolean);
    procedure SetScrollText(value : Boolean);
    procedure ScrollTimerHandler(Sender: TObject);
    procedure MouseTimerHandler(Sender: TObject);
    procedure SetScrollInterval(value : Integer);
	  procedure SetText(value : String);
	  procedure SetTextAlign(value : TAlignment);
    procedure SetTextY(value : Integer);
	  procedure SetTextForeColor(value : TColor);
	  procedure SetTextBackColor(value : TColor);
    procedure SetBarForeColorFrom(value : TColor);
    procedure SetBarForeColorTo(value : TColor);
	  procedure SetBarBackColorFrom(value : TColor);
    procedure SetBarBackColorTo(value : TColor);
	  procedure SetFillStyle(value : TEkFillStyles);
    procedure SetBlockWidth(value : Integer);
    procedure SetBlockSpace(value : Integer);
    procedure SetFrontGradientStyle(value : TEkGradientStyles);
    procedure SetBackGradientStyle(value : TEkGradientStyles);
    procedure SetGradientSteps(value : Integer);
    procedure SetTextEllipse(value : TEkTextEllipsis);
	  procedure SetMin(value : Integer);
	  procedure SetMax(value : Integer);
	  procedure SetStepValue(value : Integer);
    procedure SetPosition(value : Integer);
    procedure CMEnabledChanged(var pMsg : TMessage); message CM_ENABLEDCHANGED;

  protected
		procedure Paint; override;
    procedure Loaded; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseEnter(var pMsg : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var pMsg : TMessage); message CM_MOUSELEAVE;

    //procedure CreateParams(var pParams: TCreateParams); override;
    procedure WMEraseBkgnd(var pMsg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure Change; dynamic;

  public

		constructor Create(AOwner: TComponent); override;
		destructor  Destroy; override;
    procedure   StepIt();
    procedure   StepBy(value : Integer);

	published

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;

    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnStartDock;
    property OnStartDrag;
    property OnContextPopup;
    property OnEnter;
    property OnExit;
    property DragCursor;
    property DragKind;
    property DragMode;
    property ParentShowHint;
    property Anchors;
    property PopupMenu;
    property Visible;
	  property ShowHint;
	  property ParentFont;
    property Font;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnMouseDown;
    property OnMouseUp;

    property Position : Integer read FCurValue write SetPosition default 50;
    property TrackBarMode : Boolean read FTrackBarMode write FTrackBarMode default false;
    //property TrackBarDblClickZero : Boolean read FDblClickZeroMode write FDblClickZeroMode default false;
	  property ShowText : Boolean read FShowText write SetShowText default true;
    property ScrollText : Boolean read FScrollText write SetScrollText default false;
    property ScrollTextInterval : Integer read FScrollInterval write SetScrollInterval default 30;
    property Style : TEkFillStyles read FFillStyle write SetFillStyle default efSolid;
    property BlockWidth : Integer read FBlockWidth write SetBlockWidth default 12;
    property BlockSpacing : Integer read FBlockSpace write SetBlockSpace default 2;
    property GradientTypeFront : TEkGradientStyles read FGradientFrontStyle write SetFrontGradientStyle default egHorizontal;
    property GradientTypeBack : TEkGradientStyles read FGradientBackStyle write SetBackGradientStyle default egHorizontal;
    property GradientSteps : Integer read FGradientSteps write SetGradientSteps default 200;
    property TextEllipse : TEkTextEllipsis read FTextEllipsis write SetTextEllipse default etlNone;
	  property Text : String read FText write SetText;
    property SingleBorderColor : TColor read FSingleBorderColor write SetSingleBorderColor default clBlack;
    property InnerBorderSize :Integer read FInnerBorderSize write SetInnerBorderSize;
	  property AlignText : TAlignment read FTextAlign write SetTextAlign default taCenter;
    property TextY : Integer read FTextY write SetTextY default 0;
	  property TextForeColor : TColor  read FTextForeColor write SetTextForeColor default clBlack;
	  property TextBackColor : TColor read FTextBackColor write SetTextBackColor default clWhite;
    property BarForeColorFrom : TColor read FBarForeColorFrom  write SetBarForeColorFrom default clHighlight;
    property BarForeColorTo : TColor read FBarForeColorTo  write SetBarForeColorTo default clHighlight;
	  property BarBackColorFrom : TColor read FBarBackColorFrom  write SetBarBackColorFrom default clBtnFace;
    property BarBackColorTo : TColor read FBarBackColorTo  write SetBarBackColorTo default clBtnFace;
	  property Border : TEkBorders read FBorder write SetBorderStyle default ebSingle;
	  property Step : Integer read FStep write SetStepValue default 10;
	  property PercentAt : Double read FPercent;
	  property Min : Integer read FMinValue write SetMin default 0;
	  property Max : Integer read FMaxValue write SetMax default 100;
	end;

procedure Register;

implementation


//==============================================================================

constructor TEkGradientProgressBar.Create(AOwner: TComponent);
const
  clSkyBlue = TColor($F0CAA6);
begin
	inherited Create(AOwner);

ControlStyle := ControlStyle + [csOpaque];

FMinValue := 0;
FMaxValue := 100;
FStep := 10;
FCurValue := 50;
FSingleBorderColor := clBlack;
FShowText := True;
FAutoUpdate := False;
FTrackBarMode := False;
FBorder := ebSingle;
//HintWin := THintWindow.Create(self);
//FHintFollow := False;

FScrollInterval := 30;
FScrollText := False;
FScrollTimer := TTimer.Create(nil);
FScrollTimer.OnTimer := ScrollTimerHandler;
FScrollTimer.Enabled := False;

FMouseTimer := TTimer.Create(nil);
FMouseTimer.Interval := 30;
FMouseTimer.OnTimer := MouseTimerHandler;
FMouseTimer.Enabled := False;

FTextForeColor := clBlack;
FTextBackColor := clWhite;
FBarForeColorFrom := clRed;
FBarForeColorTo := clMaroon;
FBarBackColorFrom := clSkyBlue;
FBarBackColorTo := clNavy;

FFillStyle := efSolid;
FBlockWidth := 12;
FBlockSpace := 2;
FText := '$p%';
FTextPos := 0;
//FTextPos := 0 - FTextLength;  // Need a text length calc procedure
FTextAlign := taCenter;
FTextY := 0;
FGradientSteps := 200;
FTextEllipsis := etlNone;
FRepaintIt := True;

FBmpBuf := TBitmap.Create;
FBmpBack := TBitmap.Create;
FBmpFront := TBitmap.Create;
//FBmpFrontDashed := TBitmap.Create;

FCurBarPos := 0;
FCurAnimPos := 0;
FHasDashed := False;
Width :=  150;
Height := GetSystemMetrics(SM_CYVSCROLL);
FWidth := Width;
FHeight := Height;

end;

//==============================================================================

destructor TEkGradientProgressBar.Destroy;
begin

FBmpBuf.Free;
FBmpBack.Free;
FBmpFront.Free;
//FBmpFrontDashed.Free;
FScrollTimer.Free;
FMouseTimer.Free;

inherited Destroy;

end;

//==============================================================================

procedure TEkGradientProgressBar.Loaded;
begin
  FBmpBuf.Width := Width;
  FBmpBuf.Height := Height;
  if FBorder = ebRound then
    CreateRoundRgn(FBmpBuf.Canvas);
end;

//==============================================================================

procedure TEkGradientProgressBar.WMSize(var pMsg: TWMSize);
begin

FBmpFront.Width := Width;
FBmpFront.Height := Height;
FBmpBack.Width := Width;FBmpBack.Height := Height;
FRepaintIt := True;
Invalidate;

end;

//==============================================================================

procedure TEkGradientProgressBar.Paint;
begin
  PaintIt;
end;

//==============================================================================

procedure TEkGradientProgressBar.PaintIt;
begin

if not Visible then Exit;

CalculatePercentDone();
CalculatePixelWidth();

FBarRect        := ClientRect;
FEmptyRect      := ClientRect;
FBarRect.Right  := FXPixel;
FEmptyRect.Left := FXPixel;


if (FBmpBuf.Height <> ClientHeight) or (FBmpBuf.Width <> ClientWidth)
then begin
    FBmpBuf.Width := Width;
    FBmpBuf.Height := Height;
    FTextLast := ''; // Reposition / Reset text
end;


PaintFillBack(FBmpBack);
PaintFillFront(FBmpFront);

if FShowText then
  PaintText(FBmpBuf);

if FBorder <> ebNone then
  PaintBorder(FBmpBuf.Canvas);

BitBlt(Canvas.Handle, 0, 0,
FBmpBuf.Width, FBmpBuf.Height,
FBmpBuf.Canvas.Handle, 0, 0, SRCCOPY);

end;

//==============================================================================

procedure TEkGradientProgressBar.PaintFillFront(pic: TBitmap);
var  x, adjust : Integer;
begin

if (ClientHeight <> pic.Height) then
  pic.Height := ClientHeight;

if (ClientWidth <> pic.Width) then
  pic.Width := ClientWidth;

if FRepaintIt then
begin  PaintGradient(0, pic, 0, FBarForeColorFrom, FBarForeColorTo);end;

x := 0;

if (FBorder <> ebRound) then // Not round
begin
  adjust := 0;
  //x := 0
end
else // Round
begin
  adjust := 1;
  if (FPercent = 100) then
  begin
    adjust := 2;
  end;
  if (FPercent = 0) then // Glitch fix
  begin
    //x := 0;
  end
  else
    x := 1;
end;

FBmpBuf.Canvas.CopyRect
(
  Rect(x, x, FBarRect.Right -adjust, Height -adjust),
  pic.Canvas,
  Rect(x, x, FBarRect.Right -adjust, Height -adjust)
);

end;

//==============================================================================

procedure TEkGradientProgressBar.PaintFillBack(pic: TBitmap);
begin

if (ClientHeight <> pic.Height) then
  pic.Height := ClientHeight;

if (ClientWidth <> pic.Width) then
  pic.Width := ClientWidth;

if (FBorder = ebRound) then
begin
  {
  PaintGradient(0, FBmpBack, 1, FBarBackColorFrom, FBarBackColorFrom);
  bgBitmap.Canvas.Brush.Color := FBarBackColorFrom;

	bgBitmap.Canvas.FillRect(r);

	pic.Canvas.CopyRect
	(
  Rect(pic.Width, pic.Height, 1, 1), bgBitmap.Canvas,
  Rect(pic.Width, pic.Height, 1, 1)
  );
  }
	Exit;
end;

if FRepaintIt thenbegin  PaintGradient(0, pic, 1, FBarBackColorFrom, FBarBackColorTo);end;FBmpBuf.Canvas.CopyRect(
  Rect(0, 0, Width, Height),
  pic.Canvas,
  Rect(0, 0, Width, Height)
);end;

//==============================================================================

procedure TEkGradientProgressBar.PaintGradient(typ : Integer; pic : TBitmap;
              frontBack : Integer; colorFrom : TColor; colorTo : TColor);
var
  i : Integer;
  j : Real;
  dlta : array [0..2] of Real; // R,G,B
  r : TRect;
  steps : Integer;
begin

steps := FGradientSteps;

// Horizontal
if (frontBack = 0) and (FGradientFrontStyle = egHorizontal)
or (frontBack = 1) and (FGradientBackStyle = egHorizontal) then
begin 

  if steps > Height then
    steps := Height;
  if steps < 1 then
    steps := 1;

  dlta[0] := (GetRValue(colorTo) - GetRValue(colorFrom)) / steps;
  dlta[1] := (GetGValue(colorTo) - GetGValue(colorFrom)) / steps;
  dlta[2] := (GetBValue(colorTo) - GetBValue(colorFrom)) / steps;
  pic.Canvas.Brush.Style := bsSolid;
  j := Height / steps;
  for i := 0 to steps do
  begin
    r.Left := Width;
    r.Right := 0;
    r.Top := Round(i * j);
    r.Bottom := Round((i + 1) * j);
    pic.Canvas.Brush.Color := RGB(
    Round(GetRValue(colorFrom) + i * dlta[0]),
    Round(GetGValue(colorFrom) + i * dlta[1]),
    Round(GetBValue(colorFrom) + i * dlta[2]));
    pic.Canvas.FillRect(r);
  end;

end

// Vertical
else if (frontBack = 0) and (FGradientFrontStyle = egVertical)
or (frontBack = 1) and (FGradientBackStyle = egVertical) then
begin

  if steps > Width then
    steps := Width;
  if steps < 1 then
    steps := 1;

  dlta[0] := (GetRValue(colorTo) - GetRValue(colorFrom)) / steps;
  dlta[1] := (GetGValue(colorTo) - GetGValue(colorFrom)) / steps;
  dlta[2] := (GetBValue(colorTo) - GetBValue(colorFrom)) / steps;
  pic.Canvas.Brush.Style := bsSolid;
  j := Width / steps;
  for i := 0 to steps do
  begin
    r.Left := Round(i * j);
    r.Right := Round((i + 1) * j);
    r.Top := 0;
    r.Bottom := Height;
    pic.Canvas.Brush.Color := RGB(
    Round(GetRValue(colorFrom) + i * dlta[0]),
    Round(GetGValue(colorFrom) + i * dlta[1]),
    Round(GetBValue(colorFrom) + i * dlta[2]));
    pic.Canvas.FillRect(r);
  end;

end;

end;

//==============================================================================

procedure TEkGradientProgressBar.PaintText(pic : TBitmap);
var
  percentTxt : String;
  txtPoint : TPoint;
  path, pathFileName : String;
  i, pathPos : Integer;
  barRect, emptyRect : TRect;
const
  ellipsis : String = '...';
begin

if (FShowText = False) then Exit;

pic.Canvas.Font.Assign(Font);

percentTxt := StringReplace(FText, '$p', IntToStr((Round(FPercent))), [rfReplaceAll]);
txtPoint.y := (Height div 2) - (pic.Canvas.TextHeight(percentTxt) div 2) - (1) + FTextY;

FTextLength := pic.Canvas.TextWidth(percentTxt);

if (FTextLast <> FText) or (FWidth <> Width) or (FHeight <> Height) then
begin

if FTextLength > ClientWidth - 40 then
begin
  if (FTextEllipsis = etlEndEllipsis)then
  begin

    while pic.Canvas.TextWidth(percentTxt) > ClientWidth - 40 do
    begin
      SetLength(percentTxt, FTextLength);
      Dec(FTextLength);
    end;

    percentTxt := percentTxt + ellipsis;
    FTextLength := pic.Canvas.TextWidth(percentTxt);
  end
  else if (FTextEllipsis = etlPathEllipsis) then
  begin

    pathPos := LastDelimiter('\', percentTxt);

    if (pathPos <> 0) then
    begin
      i := -1;
      while pic.Canvas.TextWidth(percentTxt) > ClientWidth -40 do
      begin
        Delete(percentTxt, pathPos +i, 1);
        Dec(FTextLength);
        Dec(i);
      end;

      path := percentTxt;
      SetLength(path, pathPos +i);

      pathFileName := percentTxt;
      pathPos := LastDelimiter('\', pathFileName);
      Delete(pathFileName, 1, pathPos-1);

      if path[Length(path)] = '\' then
        percentTxt := path + ellipsis + pathFileName
      else
        percentTxt := path + '\' + ellipsis + pathFileName;

      FTextLength := pic.Canvas.TextWidth(percentTxt);
    end;
  end;
end;

if not FScrollText then
begin
  if(FTextAlign = taLeftJustify) then
	  FTextPos := 2
  else if(FTextAlign = taCenter) then
	  FTextPos := (ClientWidth - FTextLength) div 2
  else if(FTextAlign = taRightJustify) then
	  FTextPos := (ClientWidth - FTextLength) - (3);
end;

end;

FTextLast := FText;

//// Shadow txt
//if (FShadowText) then
//begin
//	Dec(txtPoint.y);
//	pic.Canvas.Font.Color := clBlack;
//  barRect := FBarRect;
//
//  ExtTextOut(pic.Canvas.Handle, FTextPos +2, txtPoint.y +2, 0,
//  @barRect,
//  PChar(percentTxt), Length(percentTxt), nil);
//end;

//// Blur shadow
//if (FShadowTextBlur and FShadowText) then
//begin
//	BlurBitmap(pic);
//	BlurBitmap(pic);
//end;

pic.Canvas.Font.Color := FTextForeColor;
barRect := FBarRect;
pic.Canvas.Brush.Style := bsClear;


// Bar txt
ExtTextOut(pic.Canvas.Handle, FTextPos, txtPoint.y, ETO_CLIPPED,
@barRect, PChar(percentTxt), Length(percentTxt), nil);

// Bar empty txt
emptyRect := FEmptyRect;
pic.Canvas.Font.Color := FTextBackColor;
ExtTextOut(pic.Canvas.Handle, FTextPos, txtPoint.y, ETO_CLIPPED,
@emptyRect, PChar(percentTxt), Length(percentTxt), nil);

end;

//==============================================================================

procedure TEkGradientProgressBar.PaintBorder(pCanvas : TCanvas);
var
  offset, i : Integer;
begin

offset := 1;
pCanvas.Brush.Style := bsClear;

if FBorder = ebSingle then
begin
  // Just draw a single, black rectangle for a border. Boring!
  pCanvas.Pen.Color := FSingleBorderColor;
  pCanvas.Rectangle(0,0, ClientWidth, ClientHeight);
end

else if FBorder = ebRound then
begin
  pCanvas.Pen.Color := FSingleBorderColor;
  pCanvas.RoundRect(0, 0, ClientWidth, ClientHeight, 4, 4);
end

else if FBorder = ebSunken then
begin
  // This is the default. With bgSunken, the bottom right half of the
  // border is white (clBtnHighlight), and the top left half of the
  // border is black. This gives a sunken look. The border is one pixel
  // thick.
  pCanvas.Pen.Color := clBtnHighlight;
  pCanvas.Rectangle(0,0, ClientWidth, ClientHeight);
  pCanvas.Pen.Color := clBtnShadow;
  pCanvas.MoveTo(0, ClientHeight);
  pCanvas.LineTo(0,0);
  pCanvas.LineTo(ClientWidth, 0);
end

else if FBorder = ebRaised then
begin
  // Opposite of gbSunken. The bottom right half of the border is
  // dark and the top left half is white. This gives a raised.
  pCanvas.Pen.Color := clBtnShadow;
  pCanvas.Rectangle(0,0, ClientWidth, ClientHeight);
  pCanvas.Pen.Color := clBtnHighlight;
  pCanvas.MoveTo(0, ClientHeight);
  pCanvas.LineTo(0,0);
  pCanvas.LineTo(ClientWidth, 0);
end

else if FBorder = ebDeepSunken then
begin
  // gbDeepSunken is similar to gbSunken, except that the border is
  // two pixels thick. The first block of code draws the outermost
  // pixel, the second set of code draws the inner pixel.
  pCanvas.Pen.Color := clBtnHighlight;
  pCanvas.Rectangle(0,0, ClientWidth, ClientHeight);
  pCanvas.Pen.Color := clBtnShadow;
  pCanvas.MoveTo(0, ClientHeight);
  pCanvas.LineTo(0,0);
  pCanvas.LineTo(ClientWidth, 0);

  pCanvas.Pen.Color := clBtnFace;
  pCanvas.Rectangle(1,1,ClientWidth-1, ClientHeight-1);
  pCanvas.Pen.Color := cl3DDkShadow;
  pCanvas.MoveTo(1,ClientHeight-2);
  pCanvas.LineTo(1,1);
  pCanvas.LineTo(ClientWidth-1, 1);

  offset := 2;
end

else if FBorder = ebFatRaised then
begin
  pCanvas.Pen.Color := clBtnShadow;
  pCanvas.Rectangle(0,0,ClientWidth, ClientHeight);
  pCanvas.Pen.Color := clBtnHighlight;
  pCanvas.MoveTo(0,ClientHeight);
  pCanvas.LineTo(0,0);
  pCanvas.LineTo(ClientWidth, 0);

  pCanvas.Pen.Color := cl3DDkShadow;
  pCanvas.Rectangle(1,1,ClientWidth-1, ClientHeight-1);
  pCanvas.Pen.Color := clBtnFace;
  pCanvas.MoveTo(1,ClientHeight-2);
  pCanvas.LineTo(1,1);
  pCanvas.LineTo(ClientWidth-1, 1);

  offset := 2;
end;


if(FInnerBorderSize > 0) then
begin
  pCanvas.Pen.Color := FSingleBorderColor;

  for i := 0 to FInnerBorderSize - 1 do //for(int i=0; i<FInnerBorderSize; i++)
		pCanvas.Rectangle(offset + i,
		offset + i,
		ClientWidth - (offset + i),
		ClientHeight - (offset + i));
end;

end;

//==============================================================================

procedure TEkGradientProgressBar.CreateRoundRgn(pCanvas : TCanvas);
var
  reg : HRGN;
begin

reg := CreateRoundRectRgn(0, 0, ClientWidth+1, ClientHeight+1, 3, 3);
SetWindowRgn(Handle, reg, false);
deleteobject(reg);

end;

//==============================================================================

//procedure TEkGradientProgressBar.DashImage();
//var
//  i : Integer;
//begin
//
//if(FFillStyle = efDashed) then
//  begin
//    //dashWidth := 13;
//    //dashSpace := 3;
//    i := 1;
//
//    while (i < ClientWidth - 1) do
//      begin
//        //CurAnimPos := (i * FBlockWidth) + FBlockSpace;
//        //if(CurAnimPos > FBarRect.right)
//        //  then CurAnimPos := FBarRect.right;
//                                       // inc(Dashspace);
//          //pic.Canvas.FillRect(Rect((i * DashWidth),
//          //FBarRect.top,
//          //CurAnimPos,
//          //FBarRect.bottom));
//        if (not FBmpFrontTile.Empty and FShowImgFront) then
//          begin
//            //FBmpFrontDashed.Assign(FBmpFrontTile);
//            //FBmpFrontDashed.Width := FBmpFrontTile.Width;
//            //FBmpFrontDashed.Height := FBmpFrontTile.Height;
//            //FBmpFrontDashed.Canvas.CopyRect
//            FBmpFrontTile.Canvas.CopyRect
//            //FBmpBuf.Canvas.CopyRect
//		        (
//              Rect(i * FBlockWidth, FBmpBackTile.Height, 10, 10),  //FBlockWidth
//			        FBmpBackTile.Canvas,
//			        Rect(i * FBlockWidth, FBmpBackTile.Height, 10, 10)
//		        )
//          end
//        else
//          begin
//            //FBmpFrontDashed.Assign(FBmpFront);
//          //SetROP2(
//            //FBmpFrontDashed.Canvas.CopyRect
//            FBmpFront.Canvas.CopyRect
//		        (
//              Rect(i, 0, FBlockWidth, FBmpFront.Height),
//			        FBmpBack.Canvas,
//			        Rect(i, 0, FBlockWidth, FBmpFront.Height)
//		        );
//          end;
//
//      Inc(i, FBlockSpace);
//      end;
//  FHasDashed := True;
//  end;
//end;

//==============================================================================

procedure TEkGradientProgressBar.CalculatePercentDone();
var range, progress : Double;
begin

range := FMaxValue - FMinValue;
progress := FCurValue - FMinValue;

if(Range = 0) then
  FPercent := 100
else
  FPercent := (Progress / Range) * 100.0;

end;

//==============================================================================

procedure TEkGradientProgressBar.CalculatePixelWidth();
begin
  FXPixel := Round(ClientWidth * FPercent / 100.0);
end;

//==============================================================================

procedure TEkGradientProgressBar.SetSingleBorderColor(value : TColor);
begin

if(value <> FSingleBorderColor) then
begin
  FSingleBorderColor := value;
  Invalidate;
end;

end;

//==============================================================================

procedure TEkGradientProgressBar.SetInnerBorderSize(value : Integer);
begin

if(value <> FInnerBorderSize) then
begin
  FInnerBorderSize := value;
  Invalidate;
end;

end;

//==============================================================================

procedure TEkGradientProgressBar.SetBorderStyle(value : TEkBorders);
begin

if(value <> FBorder) then
begin
  FBorder := value;
  if FBorder = ebRound then
    CreateRoundRgn(FBmpBuf.Canvas);
  Invalidate;
end;

end;

//==============================================================================

procedure TEkGradientProgressBar.SetShowText(value : Boolean);
begin

if(value <> FShowText) then
begin
  FShowText := value;
	Invalidate;
end;

end;

//==============================================================================

//procedure TEkGradientProgressBar.SetShadowText(value : Boolean);
//begin
//
//if(value <> FShadowText) then
//begin
//		FShadowText := value;
//		Invalidate;
//end;
//
//end;

//==============================================================================

procedure TEkGradientProgressBar.SetScrollText(value : Boolean);
begin

if(value <> FScrollText) then
  FScrollText := value;

FTextLast := ''; // Reset to repaint txt

if value = True then
  FScrollTimer.Enabled := True
else
  FScrollTimer.Enabled := False;

Invalidate;

end;

//==============================================================================

procedure TEkGradientProgressBar.ScrollTimerHandler(Sender: TObject);
begin

FScrollTimer.Enabled := False;

if FTextPos < 0 - FTextLength then
  FTextPos := ClientWidth;

if FScrollText and FAutoUpdate then
begin
  Dec(FTextPos);
  Invalidate;
end;

FScrollTimer.Enabled := True;

end;

//==============================================================================

procedure TEkGradientProgressBar.SetScrollInterval(value : Integer);
begin
  if(value <> FScrollInterval) then
  begin
    FScrollInterval := value;
    FScrollTimer.Interval := FScrollInterval;
  end;
end;

//==============================================================================

procedure TEkGradientProgressBar.SetText(value : String);
begin
  FText := value;
  Perform(CM_TEXTCHANGED, 0, 0);
  Invalidate;
end;

//==============================================================================

procedure TEkGradientProgressBar.SetTextAlign(value : TAlignment);
begin
  FTextAlign := value;
  Invalidate;
end;

//==============================================================================

procedure TEkGradientProgressBar.SetTextY(value : Integer);
begin

if value <> FTextY then
begin
  FTextY := value;
  Invalidate;
end;

end;

//==============================================================================

procedure TEkGradientProgressBar.SetTextForeColor(value : TColor);
begin

if(value <> FTextForeColor) then
begin
		FTextForeColor := value;
		Invalidate;
end;

end;

//==============================================================================

procedure TEkGradientProgressBar.SetTextBackColor(value : TColor);
begin

if(value <> FTextBackColor) then
begin
		FTextBackColor := value;
		Invalidate;
end;

end;

//==============================================================================

procedure TEkGradientProgressBar.SetBarForeColorFrom(value : TColor);
begin

if(value <> FBarForeColorFrom) then
begin
  FBarForeColorFrom := value;
  FBmpFront.Width := ClientWidth;
  FBmpFront.Height := ClientHeight;  PaintGradient(0, FBmpFront, 0, FBarForeColorFrom, FBarForeColorTo);
  Invalidate;
end;

end;

//==============================================================================

procedure TEkGradientProgressBar.SetBarForeColorTo(value : TColor);
begin

if(value <> FBarForeColorTo) then
begin
  FBarForeColorTo := value;
  FBmpFront.Width := ClientWidth;
  FBmpFront.Height := ClientHeight;  PaintGradient(0, FBmpFront, 0, FBarForeColorFrom, FBarForeColorTo);
  Invalidate;
end;

end;

//==============================================================================

procedure TEkGradientProgressBar.SetBarBackColorFrom(value : TColor);
begin

if(value <> FBarBackColorFrom) then
begin
  FBarBackColorFrom := value;
  FBmpBack.Width := ClientWidth;
  FBmpBack.Height := ClientHeight;  PaintGradient(0, FBmpBack, 1, FBarForeColorFrom, FBarForeColorTo);
  Invalidate;
end;

end;

//==============================================================================

procedure TEkGradientProgressBar.SetBarBackColorTo(value : TColor);
begin

if(value <> FBarBackColorTo) then
begin
  FBarBackColorTo := value;
  FBmpBack.Width := ClientWidth;
  FBmpBack.Height := ClientHeight;  PaintGradient(0, FBmpBack, 1, FBarForeColorFrom, FBarForeColorTo);
  Invalidate;
end;

end;

//==============================================================================

procedure TEkGradientProgressBar.SetFrontGradientStyle(value : TEkGradientStyles);
begin
  if value <> FGradientFrontStyle then
  begin
    FGradientFrontStyle := value;
    PaintGradient(0, FBmpFront, 0, FBarForeColorFrom, FBarForeColorTo);
    Invalidate;
  end;
end;

//==============================================================================

procedure TEkGradientProgressBar.SetBackGradientStyle(value : TEkGradientStyles);

begin
  if value <> FGradientBackStyle then
  begin
    FGradientBackStyle := value;
    PaintGradient(0, FBmpBack, 1, FBarForeColorFrom, FBarForeColorTo);
    Invalidate;
  end;
end;

//==============================================================================

procedure TEkGradientProgressBar.SetGradientSteps(value : Integer);
begin

if value <> FGradientSteps then
begin
  FGradientSteps := value;
  Invalidate;
end;

end;

//==============================================================================

procedure TEkGradientProgressBar.SetFillStyle(value : TEkFillStyles);
begin

if(value <> FFillStyle) then
begin
  FFillStyle := value;
  if FFillStyle = efDashed then
    FHasDashed := False;
  Invalidate;
end;

end;

//==============================================================================

procedure TEkGradientProgressBar.SetBlockWidth(value : Integer);
begin
  if value <> FBlockWidth then
    begin
    FBlockWidth := value;
    Invalidate;
  end;
end;

//==============================================================================

procedure TEkGradientProgressBar.SetBlockSpace(value : Integer);
begin
  if value <> FBlockSpace then
    begin
    FBlockSpace := value;
    Invalidate;
  end;
end;

//==============================================================================

procedure TEkGradientProgressBar.SetTextEllipse(value : TEkTextEllipsis);
begin
  if value <> FTextEllipsis then
    begin
      FTextEllipsis := value;
      Invalidate;
    end;
end;

//==============================================================================

procedure TEkGradientProgressBar.SetMin(value : Integer);
begin

if(value <> FMinValue) then
begin
	if(value > FMaxValue) then
		FMinValue := FMaxValue
	else
		FMinValue := value;

	if(FCurValue < FMinValue) then
		FCurValue := FMinValue;

	Invalidate;
end;

end;

//==============================================================================

procedure TEkGradientProgressBar.SetMax(value : Integer);
begin

if(value <> FMaxValue) then
begin
	if(FMaxValue < FMinValue) then
		FMaxValue := FMinValue
	else
		FMaxValue := value;
	if(FCurValue > FMaxValue) then
		FCurValue := FMaxValue;

	Invalidate;
end;

end;

//==============================================================================

procedure TEkGradientProgressBar.SetStepValue(value : Integer);
begin
  FStep := value;
end;

//==============================================================================

procedure TEkGradientProgressBar.SetPosition(value : Integer);
begin

if(value < FMinValue) then
	value := FMinValue
else if(value > FMaxValue) then
	value := FMaxValue;

FCurValue := value;
Invalidate;

end;

//==============================================================================

procedure TEkGradientProgressBar.StepIt();
begin

if((FCurValue + FStep) < FMinValue) then
	FCurValue := FMinValue
else if((FCurValue + FStep) <= FMaxValue) then
  FCurValue := FCurValue + FStep
else
	FCurValue := FMaxValue;

Invalidate;

end;

//==============================================================================

procedure TEkGradientProgressBar.StepBy(value : Integer);
begin

if((FCurValue + value) < FMinValue) then
	FCurValue := FMinValue
else if((FCurValue + value) <= FMaxValue) then
  FCurValue := FCurValue + value
else
	FCurValue := FMaxValue;

Change;
Invalidate;

end;

//==============================================================================

procedure TEkGradientProgressBar.MouseTimerHandler(Sender: TObject);
var
  pos : Single;
begin

if not FIsMouseDown and not FTrackBarMode then
begin;
  FMouseTimer.Enabled := False;
  Exit;
end;

GetCursorPos(FCursorPos);
// Bug in here if FMinValue is value other than 0
pos := ((FCursorPos.x - ClientOrigin.x) * (FMaxValue - FMinValue)) / Width;
//FCursorPos.x := FCursorPos.x - ClientOrigin.x;

if (round(pos)) > FMaxValue then
  pos := FMaxValue
else if (round(pos)) < FMinValue then
  pos := FMinValue;

FCurValue := round(pos);

//if FHintFollow then
//begin
//GetCursorPos(FCursorPos);
//HintWin.ActivateHint(Rect(FCursorPos.x-10,FCursorPos.y-20, FCursorPos.x+10, FCursorPos.y), IntToStr(FCurValue));
//end;

Change;
Invalidate;

end;

//==============================================================================

procedure TEkGradientProgressBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  range, step, pos : Single;
begin
  inherited MouseDown(Button, Shift, X, Y);

if not FTrackBarMode or (not Width > 0) then Exit;
if (Button <> mbLeft) or FIsMouseDown or not FIsMouseEnter then Exit;

range := FMaxValue - FMinValue;
step := range / Width;
pos := X * step;
FCurValue :=  round(pos);

FIsMouseDown := True;
FMouseTimer.Enabled := True;
Invalidate;

end;

//==============================================================================

procedure TEkGradientProgressBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

if not FTrackBarMode then Exit;
if (Button <> mbLeft)then Exit;

FIsMouseDown := False;
FMouseTimer.Enabled := False;
Invalidate;

end;

//==============================================================================

procedure TEkGradientProgressBar.CMMouseEnter(var pMsg: TMessage);
begin
	inherited;

if (csDesigning in ComponentState) then Exit;
if not FTrackBarMode then Exit;
if (FIsMouseEnter) then Exit;

FIsMouseEnter := True;

if (pMsg.LParam = 0) and Assigned(FOnMouseEnter) then
  FOnMouseEnter(self);

end;

//==============================================================================

procedure TEkGradientProgressBar.CMMouseLeave(var pMsg: TMessage);
begin
  inherited;

if (csDesigning in ComponentState) then Exit;
if not FTrackBarMode then Exit;
if (not FIsMouseEnter) then Exit;

FIsMouseEnter := False;

if (pMsg.LParam = 0) and Assigned(FOnMouseLeave) then
  FOnMouseLeave(self);

end;

//==============================================================================

//procedure TEkGradientProgressBar.DblClick;
//begin
//   inherited;
//
//if not FDblClickZeroMode then Exit;
//
//FIsDblClick := True;
//
//if FCurValue = FMinValue then
//FCurValue := FMaxValue
//else
//FCurValue := FMinValue;
//
////showmessage('dbl');
//FIsDblClick := False;
//Invalidate;
//end;

//==============================================================================

//procedure TEkGradientProgressBar.CMHintShow(var Msg: TMessage);
//var
//  HintItem : Integer;
//  //HintInfo : THintInfo;
//begin
//  inherited;
//
//  GetCursorPos(FCursorPos);
//  //pos := (FCursorPos.x - ClientOrigin.x)* (FMaxValue - FMinValue) / Width;
//  //FCursorPos.x := FCursorPos.x - ClientOrigin.x;
//
//
//  with THintInfo(Pointer(Msg.LParam)^) do
//  begin
//    ReshowTimeout := 0; // or any value you findsuitable
//    HintItem := FCursorPos.x;
//    //if HintItem > -1 then
//    //  HintStr := Items[HintItem];
//  end;
//end;

//==============================================================================

procedure TEkGradientProgressBar.WMEraseBkgnd(var pMsg: TWMEraseBkgnd);
begin
  pMsg.Result := 1;
end;

//==============================================================================

procedure TEkGradientProgressBar.CMEnabledChanged(var pMsg: TMessage);
begin
  inherited;
  Invalidate;
end;

//==============================================================================

procedure TEkGradientProgressBar.Change;
begin
  inherited Changed;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//==============================================================================

procedure Register;
begin
	RegisterComponents('EkszBoxVCL', [TEkGradientProgressBar]);
end;

end.
