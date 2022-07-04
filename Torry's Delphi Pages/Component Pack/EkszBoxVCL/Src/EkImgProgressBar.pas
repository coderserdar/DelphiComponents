unit EkImgProgressBar;

//===================
// EkImgProgressBar
//===================

// Copyright (C) 2007-2008 Kernel Master
// Author: Kernel Master
// kmeksz[At]yahoo.com

//======================================

// Contains code ported from TABExtProgressBar by Aaron Bockover
// Copyright (C) 2001-2002 Aaron Bockover - www.aaronbock.net

// .. which in turn was based on TxGauge by Harold Howe
// Copyright (C) 2000 Harold Howe and bcbdev.com

//==================================================================

//{$WARNINGS OFF}
//{$HINTS OFF}
{$O+} // Optimizations

//==============================================================================

interface

uses
	Windows, Graphics, Classes, Controls, Messages, SysUtils, ExtCtrls,
  EkLabel, EkTypes;

type
	TEkImgProgressBar = class(TCustomControl)

	private
    FScrollText       : Boolean;
    FAnimation        : Boolean;
    FMarquee          : Boolean;
    FIsMouseDown      : Boolean;
    FIsMouseEnter     : Boolean;
    //FDblClickZeroMode : Boolean;
    FAutoUpdate       : Boolean;
    FShowText         : Boolean;
    FTrackBarMode     : Boolean;

    FWidth              : Integer;
    FHeight             : Integer;
    CurAnimPos          : Integer;
    CurBarPos           : Integer;
    FTextPos            : Integer;
    FTextLength         : Integer;
    FTextY              : Integer;
    FScrollInterval     : Integer;
    FAnimationInterval  : Integer;
    FMarqueeInterval    : Integer;
    FBlockWidth         : Integer;
    FBlockSpace         : Integer;
    FInnerBorderSize    : Integer;
    FMinValue           : Integer;
    FMaxValue           : Integer;
    FCurValue           : Integer;
    FXPixel             : Integer;
    FStep               : Integer;

    FPercent    : Double;
    FCursorPos  : TPoint;

    FBarRect    : TRect;
    FEmptyRect  : TRect;

    FTextLast   : String;
    FText       : String;

    FTextForeColor      : TColor;
    FTextBackColor      : TColor;
    FSingleBorderColor  : TColor;

    FBmpBuf           : TBitmap;
    FBmpBack          : TBitmap;
    FBmpFront         : TBitmap;
    FBmpBackTile      : TBitmap;
    FBmpFrontTile     : TBitmap;

    FScrollTimer        : TTimer;
    FAnimationTimer     : TTimer;
    FMarqueeTimer       : TTimer;
    FMouseTimer         : TTimer;

    FTextAlign          : TAlignment;
  	FBorder             : TEkBorders;
    FTextEllipsis       : TEkTextEllipsis;
    FDirection          : TEkDirection;
    FAnimateFx          : TEkAnimateFx;
    FOnChange           : TNotifyEvent;

    FOnMouseEnter       : TNotifyEvent;
    FOnMouseLeave       : TNotifyEvent;

    procedure PaintIt;
	  procedure PaintImageTileFront();
	  procedure PaintImageTileBack();
    procedure PaintAnimation();
	  procedure PaintText(pCanvas : TCanvas);
	  procedure PaintBorder(pCanvas : TCanvas);
    procedure CreateRoundRgn(pCanvas : TCanvas);
    procedure CanvasBitmapChanged(Sender : TObject);
    procedure PicChanged(Sender : TObject);

    function  GetPicFront : TBitmap;
	  function  GetPicBack : TBitmap;
	  procedure SetPicFront(pic : TBitmap);
	  procedure SetPicBack(pic : TBitmap);

    procedure CalculatePercentDone();
	  procedure CalculatePixelWidth();

    procedure SetSingleBorderColor(value : TColor);
	  procedure SetInnerBorderSize(value : Integer);
	  procedure SetBorderStyle(value : TEkBorders);
	  procedure SetShowText(value : Boolean);
    procedure SetScrollText(value : Boolean);
    procedure SetAnimation(value : Boolean);
    procedure SetMarqueeMode(value : Boolean);
    procedure ScrollTimerHandler(Sender: TObject);
    procedure AnimationTimerHandler(Sender: TObject);
    procedure MarqueeTimerHandler(Sender: TObject);
    procedure MouseTimerHandler(Sender: TObject);
    procedure SetScrollInterval(value : Integer);
    procedure SetAnimationInterval(value : Integer);
    procedure SetMarqueeInterval(value : Integer);
	  procedure SetText(value : String);
	  procedure SetTextAlign(value : TAlignment);
    procedure SetTextY(value : Integer);
	  procedure SetTextForeColor(value : TColor);
	  procedure SetTextBackColor(value : TColor);
    procedure SetBlockWidth(value : Integer);
    procedure SetBlockSpace(value : Integer);
    procedure SetTextEllipse(value : TEkTextEllipsis);
    procedure SetAnimateDirection(value : TEkDirection);
    procedure SetAnimateFx(value : TEkAnimateFx);
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


    property ImageFront : TBitmap read GetPicFront write SetPicFront;
	  property ImageBack : TBitmap read GetPicBack write SetPicBack;
    property Position : Integer read FCurValue write SetPosition default 50;
    property TrackBarMode : Boolean read FTrackBarMode write FTrackBarMode default false;
    //property TrackBarDblClickZero : Boolean read FDblClickZeroMode write FDblClickZeroMode default false;
	  property ShowText : Boolean read FShowText write SetShowText default true;
    property ScrollText : Boolean read FScrollText write SetScrollText default false;
    property ScrollTextInterval : Integer read FScrollInterval write SetScrollInterval default 30;
    property Animation : Boolean read FAnimation write SetAnimation default false;
    property AnimationInterval : Integer read FAnimationInterval write SetAnimationInterval default 30;
    property MarqueeMode : Boolean read FMarquee write SetMarqueeMode default false;
    property MarqueeInterval : Integer read FMarqueeInterval write SetMarqueeInterval default 20;
    property AutoUpdate : Boolean read FAutoUpdate write FAutoUpdate default false;
    property BlockWidth : Integer read FBlockWidth write SetBlockWidth default 30;
    property BlockSpacing : Integer read FBlockSpace write SetBlockSpace default 2;
    property TextEllipse : TEkTextEllipsis read FTextEllipsis write SetTextEllipse default etlNone;
    property AnimateDirection : TEkDirection read FDirection write SetAnimateDirection default edForward;
    property AnimateFx: TEkAnimateFx read FAnimateFx write SetAnimateFx default efxNone;
	  property Text : String read FText write SetText;
    property SingleBorderColor : TColor read FSingleBorderColor write SetSingleBorderColor default clBlack;
    property InnerBorderSize :Integer read FInnerBorderSize write SetInnerBorderSize;
	  property AlignText : TAlignment read FTextAlign write SetTextAlign default taCenter;
    property TextY : Integer read FTextY write SetTextY default 0;
	  property TextForeColor : TColor  read FTextForeColor write SetTextForeColor default clWindow;
	  property TextBackColor : TColor read FTextBackColor write SetTextBackColor default clNone;
	  property Border : TEkBorders read FBorder write SetBorderStyle default ebRound;
	  property Step : Integer read FStep write SetStepValue default 10;
	  property PercentAt : Double read FPercent;
	  property Min : Integer read FMinValue write SetMin default 0;
	  property Max : Integer read FMaxValue write SetMax default 100;
	end;

procedure Register;

implementation

//==============================================================================

constructor TEkImgProgressBar.Create(AOwner: TComponent);
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
FBorder := ebRound;

FScrollInterval := 30;
FScrollText := False;
FScrollTimer := TTimer.Create(nil);
FScrollTimer.OnTimer := ScrollTimerHandler;
FScrollTimer.Enabled := False;

FAnimationInterval := 30;
FAnimation := False;
FAnimationTimer := TTimer.Create(nil);
FAnimationTimer.OnTimer := AnimationTimerHandler;
FAnimationTimer.Enabled := False;

FMarqueeInterval := 20;
FMarquee := False;
FMarqueeTimer := TTimer.Create(nil);
FMarqueeTimer.OnTimer := MarqueeTimerHandler;
FMarqueeTimer.Enabled := False;

FMouseTimer := TTimer.Create(nil);
FMouseTimer.Interval := 30;
FMouseTimer.OnTimer := MouseTimerHandler;
FMouseTimer.Enabled := False;

FTextForeColor := clWindow;
FTextBackColor := clNone;
FBlockWidth := 30;
FBlockSpace := 2;
FText := '$p%';
FTextPos := 0;
FTextAlign := taCenter;
FTextY := 0;
FTextEllipsis := etlNone;
FDirection :=  edForward;
FAnimateFx := efxNone;

FBmpBuf := TBitmap.Create;
FBmpBack := TBitmap.Create;
FBmpFront := TBitmap.Create;
FBmpBackTile := TBitmap.Create;
FBmpFrontTile := TBitmap.Create;

FBmpBuf.OnChange := CanvasBitmapChanged;
FBmpBack.OnChange := PicChanged;
FBmpFront.OnChange := PicChanged;

CurBarPos := 0;
CurAnimPos := 0;
Width :=  150;
Height := GetSystemMetrics(SM_CYVSCROLL);
FWidth := Width;
FHeight := Height;

end;

//==============================================================================

destructor TEkImgProgressBar.Destroy;
begin

FBmpBuf.Free;
FBmpBack.Free;
FBmpFront.Free;
FBmpFrontTile.Free;
FBmpBackTile.Free;
FScrollTimer.Free;
FAnimationTimer.Free;
FMarqueeTimer.Free;
FMouseTimer.Free;

inherited Destroy;

end;

//==============================================================================

procedure TEkImgProgressBar.Loaded;
begin
  FBmpBuf.Width := Width;
  FBmpBuf.Height := Height;
  if FBorder = ebRound then
    CreateRoundRgn(FBmpBuf.Canvas);
end;

//==============================================================================

procedure TEkImgProgressBar.Paint;
begin
  if not Visible then Exit;
  PaintIt;
end;

//==============================================================================

procedure TEkImgProgressBar.PaintIt;
var
  x, adjust, lftPos, topPos, btmPos: Integer;
begin

CalculatePercentDone();
CalculatePixelWidth();

FBarRect        := ClientRect;
FEmptyRect      := ClientRect;
FBarRect.Right  := FXPixel;
FEmptyRect.Left := FXPixel;

// Design time empty bmp paint
if (FBmpFront.Empty) and (FBmpBack.Empty) then
begin
    Canvas.Brush.Color := clBlue;
    Canvas.Rectangle(0, 0, Width, Height);

    Canvas.Brush.Color := clWhite;
    Canvas.FillRect(FEmptyRect);
    Canvas.Rectangle(FEmptyRect.Left, 0, Width, Height);
    if FShowText then
      PaintText(Canvas);
    Exit;
end;


if (FBmpBuf.Height <> ClientHeight) or (FBmpBuf.Width <> ClientWidth) then
begin
  FBmpBuf.Width := Width;
  FBmpBuf.Height := Height;
  FTextLast := ''; // Reposition / Reset text
end;

//    ImageBack
//-------------------

// No need to call image tile function on every paint
// as back tile image is static

if (FBmpBackTile.Empty and not FBmpBack.Empty) then
	PaintImageTileBack();


if (not FBmpBackTile.Empty) then // Draw back tile
begin
	if (FBorder <> ebRound) then // Not rounded
		FBmpBuf.Canvas.Draw(0, 0, FBmpBackTile)
	else // Rounded
	begin
    BitBlt(FBmpBuf.Canvas.Handle, 1, 1,
    FBmpBuf.Width, FBmpBuf.Height,
    FBmpBackTile.Canvas.Handle, 1, 1, SRCCOPY);
  end;
end;



// ImageFront
//-----------
x := 0;

if (FBorder <> ebRound) then // Not round
begin
  topPos := 0;
  adjust := 0;
  btmPos := Height;
  //x := 0
end
else // Round
begin
  topPos := 1;
  adjust := 1;
  btmPos := Height;
  if (FPercent = 100) then
  begin
    adjust := 2;
    btmPos := Height-1;
  end;
  if (FPercent = 0) then // Glitch fix
  begin
    //x := 0;
    btmPos := Height-2;
  end
  else
    x := 1;
end;

if (FBmpFrontTile.Empty and not FBmpFront.Empty) then
	PaintImageTileFront();

// Draw front tile
if (not FBmpFrontTile.Empty) then
begin

  if FMarquee then  // Marquee mode
  begin
    lftPos := x + CurBarPos;

    BitBlt(FBmpBuf.Canvas.Handle, lftPos, topPos,  //x+CurAnimPos
    FBlockWidth - adjust, btmPos,   //btmPos-2
    FBmpFrontTile.Canvas.Handle, lftPos, topPos, SRCCOPY);
  end
  else
    BitBlt(FBmpBuf.Canvas.Handle, x, topPos,  //x+CurAnimPos
    FBarRect.Right -adjust, btmPos,
    FBmpFrontTile.Canvas.Handle, x, topPos, SRCCOPY);
end;


if FShowText then
  PaintText(FBmpBuf.Canvas);

if FBorder <> ebNone then
  PaintBorder(FBmpBuf.Canvas);

BitBlt(Canvas.Handle, 0, 0,
FBmpBuf.Width, FBmpBuf.Height,
FBmpBuf.Canvas.Handle, 0, 0, SRCCOPY);

end;

//==============================================================================

procedure TEkImgProgressBar.PaintImageTileFront();
var x : Integer;
begin

FBmpFrontTile.Width := ClientWidth;
FBmpFrontTile.Height := FBmpFront.Height;

x := 0;
while x < ClientWidth -1 do
begin
  BitBlt(FBmpFrontTile.Canvas.Handle, x, 0,
  FBmpFrontTile.Width, FBmpFrontTile.Height,
  FBmpFront.Canvas.Handle, 0, 0, SRCCOPY);
  Inc(x, FBmpFront.Width);
end;

CurAnimPos := FBmpFront.Width; // Set for animation

end;

//==============================================================================

procedure TEkImgProgressBar.PaintImageTileBack();
var
  x : Integer;
begin

FBmpBackTile.Width := ClientWidth;
FBmpBackTile.Height := FBmpBack.Height;
Height := FBmpBack.Height;

if (FBmpBack.Width = 1) then // 1 pixel width
begin // Tile image
  for x := 0 to ClientWidth -1 do
    begin
		  FBmpBackTile.Canvas.Draw(x, 0, FBmpBack);
    end;
end
else if (FBmpBack.Width > 1) then // > 1 pixel width
begin // Tile image
  x := 0;
  while x < ClientWidth -1 do
    begin
      BitBlt(FBmpBackTile.Canvas.Handle, x, 0,
      FBmpBack.Width, FBmpBack.Height,
      FBmpBack.Canvas.Handle, 0, 0, SRCCOPY);
      Inc (x, FBmpBack.Width);
    end;
end;

end;

//==============================================================================

procedure TEkImgProgressBar.PaintAnimation();
begin

if FDirection = edForward then
begin

  BitBlt(FBmpFrontTile.Canvas.Handle,
  0,                                        // X
  0,                                        // Y
  1,                                        // Width
  FBmpFront.Height,                  // Height
  FBmpFront.Canvas.Handle,
  FBmpFront.Width - 1 - CurAnimPos,  // X
  0,                                        // Y
  SRCCOPY);

  BitBlt(FBmpFrontTile.Canvas.Handle, 1, 0, FBmpFrontTile.Width,
  FBmpFront.Height,
  FBmpFrontTile.Canvas.Handle, 0, 0, SRCCOPY);

end

else
begin

  // Contains a bug
  // Fix may involve having to re-tile FBmpFrontTile each time
  BitBlt(FBmpFrontTile.Canvas.Handle, 0, 0,
  FBmpFront.Width-1-CurAnimPos,
  FBmpFront.Height,
  FBmpFront.Canvas.Handle, FBmpFront.Width-1, 0,
  SRCCOPY);

  BitBlt(FBmpFrontTile.Canvas.Handle, FBmpFrontTile.Width-1, 0,
  1,
  FBmpFront.Height,
  FBmpFront.Canvas.Handle, FBmpFront.Width -1 - CurAnimPos, 0,
  SRCCOPY);

  BitBlt(FBmpFrontTile.Canvas.Handle, -1, 0,
  FBmpFrontTile.Width,
  FBmpFront.Height,
  FBmpFrontTile.Canvas.Handle, 0, 0,
  SRCCOPY);

end;

if FAnimateFx = efx1 then
  BitBlt(FBmpFrontTile.Canvas.Handle, CurAnimPos, 0, FBmpFrontTile.Width,
  FBmpFront.Height,
  FBmpFrontTile.Canvas.Handle, 0, 0, SRCCOPY)
else if FAnimateFx = efx2 then
  BitBlt(FBmpFrontTile.Canvas.Handle, CurAnimPos+5, 0, FBmpFrontTile.Width,
  FBmpFront.Height,
  FBmpFrontTile.Canvas.Handle, -CurAnimPos*2, 0, SRCCOPY)
else if FAnimateFx = efx3 then
  BitBlt(FBmpFrontTile.Canvas.Handle, CurAnimPos, 0, FBmpFrontTile.Width,
  FBmpFront.Height,
  FBmpFrontTile.Canvas.Handle, 0, 0, SRCCOPY)
else if FAnimateFx = efx4 then
  BitBlt(FBmpFrontTile.Canvas.Handle, CurAnimPos, 0, FBmpFrontTile.Width,
  FBmpFront.Height,
  FBmpFrontTile.Canvas.Handle, 0, 0, PATINVERT)
else if FAnimateFx = efx5 then
  BitBlt(FBmpFrontTile.Canvas.Handle, CurAnimPos * 16 + random(5), 0, FBmpFrontTile.Width,
  FBmpFront.Height,
  FBmpFrontTile.Canvas.Handle, 0, 0, SRCCOPY);


Inc(CurAnimPos);
if CurAnimPos >= FBmpFront.Width then
  CurAnimPos := 0;

end;

//==============================================================================

procedure TEkImgProgressBar.PaintText(pCanvas : TCanvas);
var
  percentTxt    : String;
  path          : String;
  pathFileName  : String;
  i, pathPos    : Integer;
  txtPoint      : TPoint;
  barRect, emptyRect : TRect;
const
  ellipsis : String = '...';
begin

pCanvas.Font.Assign(Font);

percentTxt := StringReplace(FText, '$p', IntToStr((Round(FPercent))), [rfReplaceAll]);
txtPoint.y := (Height div 2) - (pCanvas.TextHeight(percentTxt) div 2) - (1) + FTextY;

FTextLength := pCanvas.TextWidth(percentTxt);

// Only re-calc if text has changed
if (FTextLast <> FText) or (FWidth <> Width) or (FHeight <> Height) then
begin

if FTextLength > ClientWidth - 40 then
begin
  if (FTextEllipsis = etlEndEllipsis)then
  begin

    while pCanvas.TextWidth(percentTxt) > ClientWidth - 40 do
    begin
      SetLength(percentTxt, FTextLength);
      Dec(FTextLength);
    end;

    percentTxt := percentTxt + ellipsis;
    FTextLength := pCanvas.TextWidth(percentTxt);
  end
  else if (FTextEllipsis = etlPathEllipsis) then
  begin

    pathPos := LastDelimiter('\', percentTxt);

    if (pathPos <> 0) then
    begin
      i := -1;
      while pCanvas.TextWidth(percentTxt) > ClientWidth -40 do
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

      FTextLength := pCanvas.TextWidth(percentTxt);
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

pCanvas.Font.Color := FTextForeColor;
if (FMarquee) then
  barRect := Rect(CurBarPos, 0, CurBarPos + FBlockWidth, Height)
else
  barRect := FBarRect;

pCanvas.Brush.Style := bsClear;

if (not FMarquee) then // - Marquee temp solution
  ExtTextOut(pCanvas.Handle, FTextPos, txtPoint.y, ETO_CLIPPED, // Bar txt
  @barRect, PChar(percentTxt), Length(percentTxt), nil);

// Bar empty txt - Marquee temp solution
if (FMarquee) then
  emptyRect := ClientRect
else
  emptyRect := FEmptyRect;

pCanvas.Font.Color := FTextBackColor;
ExtTextOut(pCanvas.Handle, FTextPos, txtPoint.y, ETO_CLIPPED,
@emptyRect, PChar(percentTxt), Length(percentTxt), nil);

end;

//==============================================================================

procedure TEkImgProgressBar.PaintBorder(pCanvas : TCanvas);
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

procedure TEkImgProgressBar.CreateRoundRgn(pCanvas : TCanvas);
var
  reg : HRGN;
begin

reg := CreateRoundRectRgn(0, 0, ClientWidth+1, ClientHeight+1, 3, 3);
SetWindowRgn(Handle, reg, false);
deleteobject(reg);

end;

//==============================================================================

procedure TEkImgProgressBar.CanvasBitmapChanged(Sender: TObject);
begin
  if (FBmpFrontTile.Width <> ClientWidth) or
  (FBmpFrontTile.Height <> ClientHeight) then
  begin
    FBmpFrontTile.Assign(nil); // Clear these fuckers to get a full repaint
    FBmpBackTile.Assign(nil);
  end;
end;

//==============================================================================

procedure TEkImgProgressBar.PicChanged(Sender: TObject);
begin
  if FBorder = ebRound then
    CreateRoundRgn(FBmpBuf.Canvas);
  Invalidate;
end;

//==============================================================================

function TEkImgProgressBar.GetPicFront : TBitmap;
begin

if (FBmpFront.Empty) then
	FBmpFront := TBitmap.Create;

Result := FBmpFront;

end;

//==============================================================================

function TEkImgProgressBar.GetPicBack : TBitmap;
begin

if (FBmpBack.Empty) then
	FBmpBack := TBitmap.Create;

if (not FBmpBack.Empty) then
	Height := FBmpBack.Height;

Result := FBmpBack;

end;

//==============================================================================

procedure TEkImgProgressBar.SetPicFront(pic : TBitmap);
begin

FBmpFront.Assign(pic);

if (FBmpFront.Empty) then
begin
  Invalidate;
  Exit;
end;

FBmpFront.Width := pic.Width;
FBmpFront.Height := pic.Height;

Invalidate;

end;

//==============================================================================

procedure TEkImgProgressBar.SetPicBack(pic : TBitmap);
begin

FBmpBack.Assign(pic);

if (FBmpBack.Empty) then
begin
  Invalidate;
  Exit;
end;

FBmpBack.Width := pic.Width;
FBmpBack.Height := pic.Height;
Height := pic.Height;

Invalidate;

end;

//==============================================================================

procedure TEkImgProgressBar.CalculatePercentDone();
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

procedure TEkImgProgressBar.CalculatePixelWidth();
begin
  FXPixel := Round(ClientWidth * FPercent / 100.0);
end;

//==============================================================================

procedure TEkImgProgressBar.SetSingleBorderColor(value : TColor);
begin

if(value <> FSingleBorderColor) then
begin
  FSingleBorderColor := value;
  Invalidate;
end;

end;

//==============================================================================

procedure TEkImgProgressBar.SetInnerBorderSize(value : Integer);
begin

if(value <> FInnerBorderSize) then
begin
  FInnerBorderSize := value;
  Invalidate;
end;

end;

//==============================================================================

procedure TEkImgProgressBar.SetBorderStyle(value : TEkBorders);
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

procedure TEkImgProgressBar.SetShowText(value : Boolean);
begin

if(value <> FShowText) then
begin
		FShowText := value;
    FTextLast := '';
		Invalidate;
end;

end;

//==============================================================================

procedure TEkImgProgressBar.SetScrollText(value : Boolean);
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

procedure TEkImgProgressBar.SetAnimation(value : Boolean);
begin

if(value <> FAnimation) then
  FAnimation := value;

if value = True then
  FAnimationTimer.Enabled := True
else
  FAnimationTimer.Enabled := False;

Invalidate;

end;

//==============================================================================

procedure TEkImgProgressBar.SetMarqueeMode(value : Boolean);
begin

if(value <> FMarquee) then
  FMarquee := value;

if value = True then
  FMarqueeTimer.Enabled := True
else
  FMarqueeTimer.Enabled := False;

Invalidate;

end;

//==============================================================================

procedure TEkImgProgressBar.ScrollTimerHandler(Sender: TObject);
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

procedure TEkImgProgressBar.AnimationTimerHandler(Sender: TObject);
begin

FAnimationTimer.Enabled := False;

if FAnimation and (FBmpFront.Width > 1) and FAutoUpdate then
begin
  PaintAnimation();
  Invalidate;
end;

FAnimationTimer.Enabled := True;

end;

//==============================================================================

procedure TEkImgProgressBar.MarqueeTimerHandler(Sender: TObject);
begin

FMarqueeTimer.Enabled := False;

Inc(CurBarPos, 2);

if CurBarPos > FBmpBuf.Width then
  CurBarPos := 0 - FBlockWidth;

Invalidate;
FMarqueeTimer.Enabled := True;

end;

//==============================================================================

procedure TEkImgProgressBar.SetScrollInterval(value : Integer);
begin
  if(value <> FScrollInterval) then
  begin
    FScrollInterval := value;
    FScrollTimer.Interval := FScrollInterval;
  end;
end;

//==============================================================================

procedure TEkImgProgressBar.SetAnimationInterval(value : Integer);
begin
  if(value <> FAnimationInterval) then FAnimationInterval := value;
  FAnimationTimer.Interval := FAnimationInterval;
end;

//==============================================================================

procedure TEkImgProgressBar.SetMarqueeInterval(value : Integer);
begin
  if(value <> FMarqueeInterval) then FMarqueeInterval := value;
  FMarqueeTimer.Interval := FMarqueeInterval;
end;

//==============================================================================

procedure TEkImgProgressBar.SetText(value : String);
begin
  FText := value;
  FTextLast := '';
  Invalidate;
end;

//==============================================================================

procedure TEkImgProgressBar.SetTextAlign(value : TAlignment);
begin
  FTextAlign := value;
  Invalidate;
end;

//==============================================================================

procedure TEkImgProgressBar.SetTextY(value : Integer);
begin

if value <> FTextY then
begin
  FTextY := value;
  Invalidate;
end;

end;

//==============================================================================

procedure TEkImgProgressBar.SetTextForeColor(value : TColor);
begin

if(value <> FTextForeColor) then
begin
  FTextForeColor := value;
  Invalidate;
end;

end;

//==============================================================================

procedure TEkImgProgressBar.SetTextBackColor(value : TColor);
begin

if(value <> FTextBackColor) then
begin
  FTextBackColor := value;
  Invalidate;
end;

end;


//==============================================================================

procedure TEkImgProgressBar.SetBlockWidth(value : Integer);
begin

if value <> FBlockWidth then
begin
  FBlockWidth := value;
  Invalidate;
end;

end;

//==============================================================================

procedure TEkImgProgressBar.SetBlockSpace(value : Integer);
begin

if value <> FBlockSpace then
begin
  FBlockSpace := value;
  Invalidate;
end;

end;

//==============================================================================

procedure TEkImgProgressBar.SetTextEllipse(value : TEkTextEllipsis);
begin

if value <> FTextEllipsis then
begin
  FTextEllipsis := value;
  Invalidate;
end;

end;

//==============================================================================

procedure TEkImgProgressBar.SetAnimateDirection(value : TEkDirection);
begin

if value <>FDirection then
  begin
    FDirection := value;
    //PaintImageTileFront();
    Invalidate;
  end;

end;

//==============================================================================

procedure TEkImgProgressBar.SetAnimateFx(value : TEkAnimateFx);
begin

if value <> FAnimateFx then
begin
  if (value = efxNone) or (FAnimateFx = efxNone) then
  begin
    PaintImageTileFront();
    Invalidate;
  end;
  FAnimateFx := value;
end;

end;

//==============================================================================

procedure TEkImgProgressBar.SetMin(value : Integer);
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

procedure TEkImgProgressBar.SetMax(value : Integer);
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

procedure TEkImgProgressBar.SetStepValue(value : Integer);
begin
  FStep := value;
end;

//==============================================================================

procedure TEkImgProgressBar.SetPosition(value : Integer);
begin

if(value < FMinValue) then
	value := FMinValue
else if(value > FMaxValue) then
	value := FMaxValue;

//CurBarPos := value - FCurValue;
FTextLast := '';
FCurValue := value;
Invalidate;

end;

//==============================================================================

procedure TEkImgProgressBar.StepIt();
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

procedure TEkImgProgressBar.StepBy(value : Integer);
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

procedure TEkImgProgressBar.MouseTimerHandler(Sender: TObject);
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
pos := (FCursorPos.x - ClientOrigin.x)* (FMaxValue - FMinValue) / Width;
//FCursorPos.x := FCursorPos.x - ClientOrigin.x;

if (round(pos)) > FMaxValue then
  pos := FMaxValue
else if (round(pos)) < FMinValue then
  pos := FMinValue;

FCurValue :=  round(pos);

Change;
Invalidate;

end;

//==============================================================================

procedure TEkImgProgressBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TEkImgProgressBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

if not FTrackBarMode then Exit;
if (Button <> mbLeft)then Exit;

FIsMouseDown := False;
FMouseTimer.Enabled := False;
Invalidate;

end;

//==============================================================================

procedure TEkImgProgressBar.CMMouseEnter(var pMsg: TMessage);
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

procedure TEkImgProgressBar.CMMouseLeave(var pMsg: TMessage);
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

procedure TEkImgProgressBar.CMEnabledChanged(var pMsg: TMessage);
begin
  inherited;
  Invalidate;
end;

//==============================================================================

procedure TEkImgProgressBar.WMEraseBkgnd(var pMsg: TWMEraseBkgnd);
begin
  pMsg.Result := 1;
end;

//==============================================================================

procedure TEkImgProgressBar.Change;
begin
  inherited Changed;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//==============================================================================

procedure Register;
begin
	RegisterComponents('EkszBoxVCL', [TEkImgProgressBar]);
end;

end.
