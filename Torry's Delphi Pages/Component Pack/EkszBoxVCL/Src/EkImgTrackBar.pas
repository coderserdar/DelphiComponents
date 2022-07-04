unit EkImgTrackBar;

//===================
// EkImgTrackBar
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
	Windows, Graphics, Classes, Controls, Messages, ExtCtrls,
  EkTypes;


//==============================================================================

type                 
  TEkImgTrackBar = class(TCustomControl)

	private
    FAnimation        : Boolean;
    FIsMouseDown      : Boolean;
    FIsMouseEnter     : Boolean;
    FAutoUpdate       : Boolean;

    FWidth              : Integer;
    FHeight             : Integer;
    CurAnimPos          : Integer;
    CurBarPos           : Integer;
    FAnimationInterval  : Integer;
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

    FSingleBorderColor  : TColor;

    FBmpBuf           : TBitmap;
    FBmpBack          : TBitmap;
    FBmpFront         : TBitmap;
    FBmpBackTile      : TBitmap;
    FBmpFrontTile     : TBitmap;

    FAnimationTimer     : TTimer;
    FMouseTimer         : TTimer;

  	FBorder             : TEkBorders;
    FDirection          : TEkDirection;
    FOnChange           : TNotifyEvent;

    FOnMouseEnter       : TNotifyEvent;
    FOnMouseLeave       : TNotifyEvent;

    procedure PaintIt;
	  procedure PaintImageTileFront();
	  procedure PaintImageTileBack();
    procedure PaintAnimation();
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
    procedure SetAnimation(value : Boolean);
    procedure AnimationTimerHandler(Sender: TObject);
    procedure MouseTimerHandler(Sender: TObject);
    procedure SetAnimationInterval(value : Integer);
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
    property Action;
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
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;

    property ImageFront : TBitmap read GetPicFront write SetPicFront;
	  property ImageBack : TBitmap read GetPicBack write SetPicBack;
    property Position : Integer read FCurValue write SetPosition default 50;
    //property TrackBarDblClickZero : Boolean read FDblClickZeroMode write FDblClickZeroMode default false;
    property Animation : Boolean read FAnimation write SetAnimation default false;
    property AnimationInterval : Integer read FAnimationInterval write SetAnimationInterval default 30;
    property AnimateDirection : TEkDirection read FDirection write FDirection default edForward;
    property AutoUpdate : Boolean read FAutoUpdate write FAutoUpdate default false;
    property SingleBorderColor : TColor read FSingleBorderColor write SetSingleBorderColor;
    property InnerBorderSize :Integer read FInnerBorderSize write SetInnerBorderSize;
	  property Border : TEkBorders read FBorder write SetBorderStyle default ebRound;
	  property Step : Integer read FStep write SetStepValue default 0;
	  property PercentAt : Double read FPercent;
	  property Min : Integer read FMinValue write SetMin default 0;
	  property Max : Integer read FMaxValue write SetMax default 100;
	end;

procedure Register;

implementation

//==============================================================================

constructor TEkImgTrackBar.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

ControlStyle := ControlStyle + [csOpaque];

FMinValue := 0;
FMaxValue := 100;
FCurValue := 50;
FSingleBorderColor := clGray;
FAutoUpdate := False;
FBorder := ebRound;

FDirection :=  edForward;
FAnimationInterval := 30;
FAnimation := False;
FAnimationTimer := TTimer.Create(nil);
FAnimationTimer.OnTimer := AnimationTimerHandler;
FAnimationTimer.Enabled := False;

FMouseTimer := TTimer.Create(nil);
FMouseTimer.Interval := 30;
FMouseTimer.OnTimer := MouseTimerHandler;
FMouseTimer.Enabled := False;

FBmpBuf := TBitmap.Create;
FBmpBack := TBitmap.Create;
FBmpFront := TBitmap.Create;
FBmpFrontTile := TBitmap.Create;
FBmpBackTile := TBitmap.Create;

FBmpBack.OnChange := PicChanged;
FBmpFront.OnChange := PicChanged;
FBmpBuf.OnChange := CanvasBitmapChanged;

CurBarPos := 0;
CurAnimPos := 0;
Width :=  100;
Height := 15;
FWidth := Width;
FHeight := Height;

end;

//==============================================================================

destructor TEkImgTrackBar.Destroy;
begin

FBmpBuf.Free;
FBmpBack.Free;
FBmpFront.Free;
FBmpFrontTile.Free;
FBmpBackTile.Free;
FAnimationTimer.Free;
FMouseTimer.Free;

inherited Destroy;

end;

//==============================================================================

procedure TEkImgTrackBar.Loaded;
begin
  FBmpBuf.Width := Width;
  FBmpBuf.Height := Height;
  if FBorder = ebRound then
    CreateRoundRgn(FBmpBuf.Canvas);
end;

//==============================================================================

procedure TEkImgTrackBar.Paint;
begin
  PaintIt;
end;

//==============================================================================

procedure TEkImgTrackBar.PaintIt;
var
  x, adjust, topPos, btmPos: Integer;
begin

if not Visible then Exit;

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
    Exit;
end;

if (FBmpBuf.Height <> ClientHeight) or (FBmpBuf.Width <> ClientWidth)
then begin
    FBmpBuf.Width := Width;
    FBmpBuf.Height := Height;
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
    BitBlt(FBmpBuf.Canvas.Handle, x, topPos,  //x+CurAnimPos
    FBarRect.Right -adjust, btmPos,
    FBmpFrontTile.Canvas.Handle, x, topPos, SRCCOPY);
end;

if FBorder <> ebNone then
  PaintBorder(FBmpBuf.Canvas);

Canvas.Draw(0, 0, FBmpBuf);

end;

//==============================================================================

procedure TEkImgTrackBar.PaintImageTileFront();
var x : Integer;
begin

FBmpFrontTile.Width := ClientWidth;
FBmpFrontTile.Height := FBmpFront.Height;
CurAnimPos := FBmpFront.Width; // Set for animation

x := 0;

while x < ClientWidth -1 do
begin
  BitBlt(FBmpFrontTile.Canvas.Handle, x, 0,
  FBmpFrontTile.Width, FBmpFrontTile.Height,
  FBmpFront.Canvas.Handle, 0, 0, SRCCOPY);
  Inc(x, FBmpFront.Width);
end

end;

//==============================================================================

procedure TEkImgTrackBar.PaintImageTileBack();
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

procedure TEkImgTrackBar.PaintAnimation();
begin

if FDirection = edForward then
begin
  BitBlt(FBmpFrontTile.Canvas.Handle, 0, 0, 1, FBmpFront.Height,
  FBmpFront.Canvas.Handle, FBmpFront.Width - 1 - CurAnimPos, 0,
  SRCCOPY);

  BitBlt(FBmpFrontTile.Canvas.Handle, 1, 0, FBmpFrontTile.Width,
  FBmpFront.Height,
  FBmpFrontTile.Canvas.Handle, 0, 0, SRCCOPY)
end

else
begin
  // Copy all but first pixel
  BitBlt(FBmpFrontTile.Canvas.Handle, 0, 0, FBmpFront.Width-1-CurAnimPos,
  FBmpFront.Height,
  FBmpFront.Canvas.Handle, CurAnimPos, 0,
  SRCCOPY);

  // Copy first pixel to end
  BitBlt(FBmpFrontTile.Canvas.Handle, FBmpFrontTile.Width-1, 0, 1,
  FBmpFront.Height,
  FBmpFront.Canvas.Handle, FBmpFront.Width - 1 - CurAnimPos, 0,
  SRCCOPY);

  BitBlt(FBmpFrontTile.Canvas.Handle, -1, 0,
  FBmpFrontTile.Width,
  FBmpFront.Height,
  FBmpFrontTile.Canvas.Handle, 0, 0, SRCCOPY);

end;

Inc(CurAnimPos);
if CurAnimPos >= FBmpFront.Width then
  CurAnimPos := 0;

end;

//==============================================================================

procedure TEkImgTrackBar.PaintBorder(pCanvas : TCanvas);
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

procedure TEkImgTrackBar.CreateRoundRgn(pCanvas : TCanvas);
var
  reg : HRGN;
begin

reg := CreateRoundRectRgn(0, 0, ClientWidth+1, ClientHeight+1, 3, 3);
SetWindowRgn(Handle, reg, false);
deleteobject(reg);

end;

//==============================================================================

procedure TEkImgTrackBar.CanvasBitmapChanged(Sender: TObject);
begin
  if (FBmpFrontTile.Width <> ClientWidth) or
  (FBmpFrontTile.Height <> ClientHeight)
  then
    begin
      FBmpFrontTile.Assign(nil); // Clear these fuckers to get a full repaint
      FBmpBackTile.Assign(nil);
    end;
end;

//==============================================================================

procedure TEkImgTrackBar.PicChanged(Sender: TObject);
begin
  if FBorder = ebRound then
    CreateRoundRgn(FBmpBuf.Canvas);
  Invalidate;
end;

//==============================================================================

function TEkImgTrackBar.GetPicFront : TBitmap;
begin

if (FBmpFront.Empty) then
	FBmpFront := TBitmap.Create;

Result := FBmpFront;

end;

//==============================================================================

function TEkImgTrackBar.GetPicBack : TBitmap;
begin

if (FBmpBack.Empty) then
	FBmpBack := TBitmap.Create;

if (not FBmpBack.Empty) then
	Height := FBmpBack.Height;

Result := FBmpBack;

end;

//==============================================================================

procedure TEkImgTrackBar.SetPicFront(pic : TBitmap);
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

procedure TEkImgTrackBar.SetPicBack(pic : TBitmap);
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

procedure TEkImgTrackBar.CalculatePercentDone();
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

procedure TEkImgTrackBar.CalculatePixelWidth();
begin
  FXPixel := Round(ClientWidth * FPercent / 100.0);
end;

//==============================================================================

procedure TEkImgTrackBar.SetSingleBorderColor(value : TColor);
begin

if(value <> FSingleBorderColor) then
begin
  FSingleBorderColor := value;
  Invalidate;
end;

end;

//==============================================================================

procedure TEkImgTrackBar.SetInnerBorderSize(value : Integer);
begin

if(value <> FInnerBorderSize) then
begin
  FInnerBorderSize := value;
  Invalidate;
end;

end;

//==============================================================================

procedure TEkImgTrackBar.SetBorderStyle(value : TEkBorders);
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

procedure TEkImgTrackBar.SetAnimation(value : Boolean);
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

procedure TEkImgTrackBar.AnimationTimerHandler(Sender: TObject);
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

procedure TEkImgTrackBar.SetAnimationInterval(value : Integer);
begin
  if(value <> FAnimationInterval) then FAnimationInterval := value;
  FAnimationTimer.Interval := FAnimationInterval;
end;

//==============================================================================

procedure TEkImgTrackBar.SetMin(value : Integer);
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

procedure TEkImgTrackBar.SetMax(value : Integer);
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

procedure TEkImgTrackBar.SetStepValue(value : Integer);
begin
  FStep := value;
end;

//==============================================================================

procedure TEkImgTrackBar.SetPosition(value : Integer);
begin

if(value < FMinValue) then
	value := FMinValue
else if(value > FMaxValue) then
	value := FMaxValue;

FCurValue := value;
Invalidate;

end;

//==============================================================================

procedure TEkImgTrackBar.StepIt();
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

procedure TEkImgTrackBar.StepBy(value : Integer);
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

procedure TEkImgTrackBar.MouseTimerHandler(Sender: TObject);
var pos : Single;
begin

if not FIsMouseDown then
begin;
  FMouseTimer.Enabled := False;
  Exit;
end;

GetCursorPos(FCursorPos);
pos := (FCursorPos.x - ClientOrigin.x)* (FMaxValue - FMinValue) / Width;
FCursorPos.x := FCursorPos.x - ClientOrigin.x;

if (round(pos)) > FMaxValue then
  pos := FMaxValue
else if (round(pos)) < FMinValue then
  pos := FMinValue;

FCurValue :=  round(pos);

Change;
Invalidate;

end;

//==============================================================================

procedure TEkImgTrackBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  range, step, pos : Single;
begin
  inherited MouseDown(Button, Shift, X, Y);

if not Width > 0 then Exit;
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

procedure TEkImgTrackBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

if (Button <> mbLeft)then Exit;

FIsMouseDown := False;
FMouseTimer.Enabled := False;
Invalidate;

end;


//==============================================================================

procedure TEkImgTrackBar.CMMouseEnter(var pMsg: TMessage);
begin
	inherited;

if (csDesigning in ComponentState) then Exit;
if (FIsMouseEnter) then Exit;

FIsMouseEnter := True;

if (pMsg.LParam = 0) and Assigned(FOnMouseEnter) then
  FOnMouseEnter(self);

end;

//==============================================================================

procedure TEkImgTrackBar.CMMouseLeave(var pMsg: TMessage);
begin
  inherited;

if (csDesigning in ComponentState) then Exit;
if (not FIsMouseEnter) then Exit;

FIsMouseEnter := False;

if (pMsg.LParam = 0) and Assigned(FOnMouseLeave) then
  FOnMouseLeave(self);

end;

//==============================================================================

procedure TEkImgTrackBar.CMEnabledChanged(var pMsg: TMessage);
begin
  inherited;
  Invalidate;
end;

//==============================================================================

procedure TEkImgTrackBar.WMEraseBkgnd(var pMsg: TWMEraseBkgnd);
begin
  pMsg.Result := 1;
end;

//==============================================================================

procedure TEkImgTrackBar.Change;
begin
  inherited Changed;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//==============================================================================

procedure Register;
begin
	RegisterComponents('EkszBoxVCL', [TEkImgTrackBar]);
end;

end.
