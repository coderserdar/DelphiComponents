unit EkImgBtn;

//=============
//  EkImgBtn
//=============

// Copyright (C) 2007-2008 Kernel Master
// Author: Kernel Master
// kmeksz[At]yahoo.com

//==============================================================================

//{$WARNINGS OFF}
//{$HINTS OFF}
{$O+} // Optimizations

//==============================================================================

interface

uses Windows, Graphics, Classes, Controls, Messages,
    EkTypes, EkImgFadeBase, EkUtil, Dialogs;

const DEFAULTWIDTH = 75;
      DEFAULTHEIGHT = 25;

type TEkBtnNum = (btnOut, btnOver, btnDown, btnDisabled);

type
  TEkImgBtn = class(TEkImgFadeBase)

    public

		constructor   Create(AOwner: TComponent); override;
		destructor    Destroy; override;

    procedure     LoadBmpSkin(path : String);
    procedure     SetPicImgIcon(pic : TIcon);
    procedure     SetPicBmpIcon(pic : TBitmap);

	private

    PicOut            : TBitmap;
    PicOver           : TBitmap;
    PicDown           : TBitmap;
    PicDisabled       : TBitmap;
    PicSeq            : TBitmap;

    PicBmpIcon        : TBitmap;
    PicImgIcon        : TIcon;

    FMouse            : TEkMouseStates;
    {
    FOnChange         : TNotifyEvent;
    FOnCaptionChanged : TNotifyEvent;
    }

    FTransparentColor : TColor;
    FFontColor        : TColor;

    FCaption          : String;

    IconX             : Integer;
    IconY             : Integer;
    FIconDownXPos     : Integer;
    FIconDownYPos     : Integer;
    FIconOverXPos     : Integer;
    FIconOverYPos     : Integer;
    FWidthMargin      : Integer;
    FHeightMargin     : Integer;
    FOriginalWidth    : Integer;
    FOriginalHeight   : Integer;

    FFadeUponLoad     : Boolean;
    FWordWrap         : Boolean;
    FLockSize         : Boolean;
    FFocusRect        : Boolean;
    FEnableFade       : Boolean;
    FImageSwap        : Boolean;
    FActivateAuto     : Boolean;
    FEnter            : Boolean;

    procedure PaintIt();
    procedure DoResize();
		procedure PaintCaption(pCanvas : TCanvas);
    procedure PaintIcon(pCanvas : TCanvas);
		procedure PicChanged(Sender : TObject);
		procedure SetCaption(text : String);
    procedure SetFocusRect(value : Boolean);
    procedure SetWordWrap(value : Boolean);
		procedure SetProperties(pic : TBitmap);
		procedure SetIconCentered();
    procedure SetPic(pic : TBitmap);

    procedure Lock(const pWidth, pHeight : Integer);
    procedure SetLockSize(state : Boolean);
    procedure SetFadeEnable(state : Boolean);

    function  GetPic : TBitmap;

    function  GetPicImgIcon : TIcon;
		function  GetPicBmpIcon : TBitmap;

  protected
  
    procedure FormActivated(); override;
    procedure FormDeactivated(); override;

    //procedure CaptionChanged; dynamic;
    procedure CreateWnd; override;
    procedure Loaded(); override;
		procedure Paint; override;

    procedure CMMouseEnter(var pMsg : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var pMsg : TMessage); message CM_MOUSELEAVE;
		procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure KeyUp(var Key : Word; Shift : TShiftState); override;

    procedure WMSize(var pMsg: TWMSize); message WM_SIZE;
    procedure CMFontChanged(var pMsg: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var pMsg: TMessage); message CM_ENABLEDCHANGED;
    procedure WMKillFocus(var pMsg : TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var pMsg : TWMSetFocus); message WM_SETFOCUS;

	published
    property Font;
    property OnFormActivate;
    property OnFormDeactivate;

    property ActivateAuto : Boolean read FActivateAuto write FActivateAuto default false;
    property FocusRect : Boolean read FFocusRect write SetFocusRect default false;
    property LockSize : Boolean read FLockSize write SetLockSize default true;
    property MarginWidth : Integer read FWidthMargin write FWidthMargin default 3;
    property MarginHeight : Integer read FHeightMargin write FHeightMargin default 3;
    property WordWrap : Boolean read FWordWrap write SetWordWrap default true;
		property Caption : String  read FCaption write SetCaption;
    property FadeEnable : Boolean read FEnableFade write SetFadeEnable default false;
    property FadeUponLoad : Boolean read FFadeUponLoad write FFadeUponLoad default false;
		property ImageIconIco: TIcon read GetPicImgIcon write SetPicImgIcon;
    property ImageIconBmp : TBitmap read GetPicBmpIcon write SetPicBmpIcon;
    property IconDownXPos : Integer read FIconDownXPos write FIconDownXPos default 0;
    property IconDownYPos : Integer read FIconDownYPos write FIconDownYPos default 1;
    property IconOverXPos : Integer read FIconOverXPos write FIconOverXPos default 0;
    property IconOverYPos : Integer read FIconOverYPos write FIconOverYPos default 0;
    property Skin : TBitmap read GetPic write SetPic;

	end;

procedure Register;

implementation


//==============================================================================

constructor TEkImgBtn.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

ControlStyle := ControlStyle + [csClickEvents];

PicImgIcon :=  TIcon.Create;
PicBmpIcon :=  TBitmap.Create;
PicOut :=  TBitmap.Create;
PicOver :=  TBitmap.Create;
PicDown :=  TBitmap.Create;
PicDisabled :=  TBitmap.Create;
PicSeq :=  TBitmap.Create;

PicOut.OnChange := PicChanged;
PicImgIcon.OnChange := PicChanged;
PicBmpIcon.OnChange := PicChanged;
PicSeq.OnChange := PicChanged;

FActivateAuto := False;
FLockSize := True;

FFadeUponLoad := False;
self.UpdateAuto := False;
FEnableFade := False;
self.FadeStart := False;

FFocusRect := False;

FIconDownYPos := 1;
FWordWrap := true;
FWidthMargin := 3;
FHeightMargin := 3;

SetProperties(PicBmpIcon);

Width := DEFAULTWIDTH;
Height := DEFAULTHEIGHT;

end;

//==============================================================================

destructor TEkImgBtn.Destroy;
begin

PicImgIcon.Free;
PicBmpIcon.Free;
PicOut.Free;
PicOver.Free;
PicDown.Free;
PicDisabled.Free;
PicSeq.Free;

inherited Destroy;
end;

//==============================================================================

procedure TEkImgBtn.CreateWnd;
begin
  inherited;

// Create/apply button region from picture
ShapeImg(PicOut, FTransparentColor, Handle);

end;

//==============================================================================

procedure TEkImgBtn.Loaded();
begin
inherited;

if FFadeUponLoad then
  FMouse := emLeave
else
  FMouse := emDisabled;

SetPic(PicSeq);

if not PicSeq.Empty then
begin
  FOriginalWidth := (PicSeq.Width - 1) div 4;
  FOriginalHeight := PicSeq.Height;
end
else
begin
  FOriginalWidth := Width;
  FOriginalHeight := Height;
end;

FImageSwap := True;

end;

//==============================================================================

procedure TEkImgBtn.FormActivated();
begin
inherited;

if FActivateAuto then SetEnabled(True);

end;

//==============================================================================

procedure TEkImgBtn.FormDeactivated();
begin
inherited;

if FActivateAuto then SetEnabled(False);

end;

//==============================================================================

procedure TEkImgBtn.DoResize();
var offset, ogWidth : Integer;
begin

if Width < 7 then Width := 7; // Min size limit
if Height < 7 then Height := 7;

self.BmpBuf.Width := Width;
self.BmpBuf.Height := Height;
self.BmpFrom.Width := Width;
self.BmpFrom.Height := Height;
self.BmpTo.Width := Width;
self.BmpTo.Height := Height;

if (Width = PicOut.Width) or (Width <= 0)  then
  Exit;

offset := 1;
ogWidth := ((PicSeq.Width -1) div 4);

ResizeImage(PicSeq, PicOut, offset, ogWidth, PicSeq.Height, Width, Height);

Inc(offset, ogWidth);

ResizeImage(PicSeq, PicOver, offset, ogWidth, PicSeq.Height, Width, Height);

Inc(offset, ogWidth);

ResizeImage(PicSeq, PicDown, offset, ogWidth, PicSeq.Height, Width, Height);

Inc(offset, ogWidth);

ResizeImage(PicSeq, PicDisabled, offset, ogWidth, PicSeq.Height, Width, Height);

BmpBuf.Assign(PicOut);
BmpFrom.Assign(PicOut);
BmpTo.Assign(PicOver);

// Create/apply button region from picture
ShapeImg(PicOut, FTransparentColor, Handle);
SetIconCentered();

Invalidate;

end;

//==============================================================================

procedure TEkImgBtn.PaintIt();
begin

if not (csDesigning in ComponentState) and PicSeq.Empty then
  Exit;

// Design time paint
if (csDesigning in ComponentState) then
begin
  if PicSeq.Empty then
  begin
    Canvas.Rectangle(0, 0, Width, Height);
    PaintIcon(Canvas);
    if (Length(FCaption) > 0) then
      PaintCaption(Canvas);
    Exit;
  end;
end;

if FImageSwap and not PicSeq.Empty then
begin

  self.FadeStart := False;
  if FEnableFade then
    self.Offset := 1;

  if (FMouse = emDisabled) and Enabled then
  begin
    self.BmpBuf.Assign(PicOut);
    self.BmpFrom.Assign(PicOut);
  end

  else if not Enabled then
  begin
    self.BmpBuf.Assign(PicDisabled);
    self.BmpFrom.Assign(PicDisabled);
  end

  else if (FMouse = emDown) then
  begin
    self.BmpBuf.Assign(PicDown);
  end
  else if (FMouse = emUp) then
  begin
    self.BmpBuf.Assign(PicOver);
  end

  else if (FMouse = emEnter) then
  begin
    if FEnableFade then // Fade
    begin
      self.BmpBuf.Assign(PicOut);
      self.BmpFrom.Assign(PicOut);
      self.BmpTo.Assign(PicOver);
      self.FadeDirection := edForward;
      self.FadeStart := True;
    end
    else // No fade
    begin
      self.BmpBuf.Assign(PicOver);
      self.BmpFrom.Assign(PicOver);
    end;
  end

  else if (FMouse = emLeave) then
  begin
    if FEnableFade then
    begin
      self.BmpBuf.Assign(PicOver);
      self.BmpFrom.Assign(PicOut);
      self.BmpTo.Assign(PicOver);
      self.FadeDirection := edReverse;
      self.FadeStart := True;
    end
    else
      self.BmpBuf.Assign(PicOut)
  end;

  FImageSwap := False;
end;


if FEnableFade and self.FadeStart then
begin
  self.DoFade();
  self.UpdateAuto := True;
end
else if FEnableFade and (not self.FadeStart) and (self.Offset <> 1)
then
begin
  self.Offset := 1;
  self.UpdateAuto := False;
  // When animation is complete assign appropriate images,
  // req, as otherwise it will be stuck on last frame of animation
  if (Enabled) then
  begin
    if (FMouse = emLeave) then
      BmpBuf.Assign(PicOut)
    else if (FMouse = emEnter) then
      BmpBuf.Assign(PicOver);
  end;
end;

PaintIcon(self.BmpBuf.Canvas);

if (Length(FCaption) > 0) then
  PaintCaption(self.BmpBuf.Canvas);

Canvas.Draw(0, 0, self.BmpBuf);

if FFocusRect and Focus then
  PaintFocusRect(ClientRect, Canvas);

end;

//==============================================================================

procedure TEkImgBtn.Paint;
begin

if not Visible then Exit;
PaintIt();

end;

//==============================================================================

procedure TEkImgBtn.PaintIcon(pCanvas : TCanvas);
begin

// ICON
if (not PicImgIcon.Empty) then
begin
	SetIconCentered();
	if (FMouse = emDown) then
		pCanvas.Draw(IconX + FIconDownXPos, IconY + FIconDownYPos, PicImgIcon)
  else if (FMouse = emEnter) then
		pCanvas.Draw(IconX + FIconOverXPos, IconY + FIconOverYPos, PicImgIcon)
	else
		pCanvas.Draw(IconX, IconY, PicImgIcon);
end
// BMP
else if (not PicBmpIcon.Empty) then 
begin
	SetIconCentered();
	if (FMouse = emDown) then
		pCanvas.Draw(IconX + FIconDownXPos, IconY + FIconDownYPos, PicBmpIcon)
  else if (FMouse = emEnter) then
		pCanvas.Draw(IconX + FIconOverXPos, IconY + FIconOverYPos, PicBmpIcon)
	else
		pCanvas.Draw(IconX, IconY, PicBmpIcon);
end;

end;

//==============================================================================

procedure TEkImgBtn.PaintCaption(pCanvas : TCanvas);
var
	x, y, txtWidth, txtHeight : Integer;
  r : TRect;
  txt : String;
begin

pCanvas.Brush.Style := bsClear;
pCanvas.Font.Assign(Font);
pCanvas.Font.Color := Font.Color;

txt := FCaption;

if (txt[1] = '&') and (txt[2] = #0) then
txt := txt + ' ';

txtWidth := pCanvas.TextWidth(txt);
txtHeight := pCanvas.TextHeight(txt);
x := (ClientWidth div 2) - (txtWidth div 2);
y := (ClientHeight div 2) - (txtHeight div 2);

if FWordWrap and (txtWidth > ClientWidth) then // Wrap text
begin

  r.Left := x;
  r.Top := y;
  r.Right := Width - FWidthMargin;
  r.Bottom := Height - FHeightMargin;

  DrawTextEx(pCanvas.Handle, PChar(FCaption), Length(FCaption),
  r, (DT_END_ELLIPSIS), nil );

end

else
begin
  ExtTextOut(pCanvas.Handle, x, y, ETO_CLIPPED, nil,
  PChar(FCaption), Length(FCaption), nil);

  //r.Left := x;
  //r.Top := y;
  //r.Right := x + txtWidth;
  //r.Bottom := y + txtHeight;

  //DrawText(pCanvas.Handle, PChar(Caption), Length(Caption), &r, DT_NOPREFIX or DT_CALCRECT);
  //DrawText(pCanvas.Handle, PChar(Caption), Length(Caption), r, ETO_CLIPPED);
end;

end;

//==============================================================================

procedure TEkImgBtn.PicChanged(Sender: TObject);
begin

if not (csLoading in ComponentState) then
  Invalidate;

end;

//==============================================================================

procedure TEkImgBtn.SetCaption(text: string);
begin

if (FCaption <> text) then
begin
  FCaption := text;
  Canvas.Font.Assign(Font);
  Canvas.Brush.Style := bsClear;
  Perform(CM_TEXTCHANGED, 0, 0);
  FImageSwap := True;
  Invalidate;
end;

end;

//==============================================================================

procedure TEkImgBtn.SetFocusRect(value: Boolean);
begin
  if value <> FFocusRect then
  begin
    FFocusRect := value;
    Invalidate;
  end;
end;

//==============================================================================

procedure TEkImgBtn.SetWordWrap(value: Boolean);
begin

if (FWordWrap <> value) then
  begin
    FWordWrap := value;
    FImageSwap := True;
    Invalidate;
  end;

end;

//==============================================================================

procedure TEkImgBtn.SetProperties(pic: TBitmap);
begin

pic.Transparent := False;
self.Transparent := False;

SetDefaultBmpType(pic);

if FLockSize and (pic.Width > 0) and (pic.Height > 0) then
begin
  Width := pic.Width;
  Height := pic.Height;
end;

end;

//==============================================================================

procedure TEkImgBtn.SetIconCentered;
begin

if not PicImgIcon.Empty then
begin
  IconX := (ClientWidth div 2) - (PicImgIcon.Width div 2);
  IconY := (ClientHeight div 2) - (PicImgIcon.Height div 2);
end
else if not PicBmpIcon.Empty then
begin
  IconX := (ClientWidth div 2) - (PicBmpIcon.Width div 2);
  IconY := (ClientHeight div 2) - (PicBmpIcon.Height div 2);
end;

end;

//==============================================================================

procedure TEkImgBtn.SetPicImgIcon(pic: TIcon);
begin

//pic.Picture.Graphic is TIcon
PicImgIcon.Assign(pic);

if (not PicImgIcon.Empty) then
begin
  PicImgIcon.Transparent := True;
  SetIconCentered();
end;

FImageSwap := True;
Invalidate;

end;

//==============================================================================

procedure TEkImgBtn.SetPicBmpIcon(pic: TBitmap);
begin

PicBmpIcon.Assign(pic);

if (not PicBmpIcon.Empty) then
begin
  PicBmpIcon.Width := pic.Width;
  PicBmpIcon.Height := pic.Height;
  PicBmpIcon.Transparent := True;
  SetIconCentered();
end;

FImageSwap := True;
Invalidate;

end;

//==============================================================================

procedure TEkImgBtn.LoadBmpSkin(path : String);
var
  bmp : TBitmap;
begin

bmp := TBitmap.Create;
bmp.LoadFromFile(path);

SetPic(bmp);
bmp.Free;

end;

//==============================================================================

procedure TEkImgBtn.SetPic(pic : TBitmap);
begin

PicSeq.Assign(pic);

if (PicSeq.Empty) then
begin
  Invalidate;
  Exit;
end;
                       
SplitBtnBmp(pic, PicOut, PicOver, PicDown, PicDisabled, FTransparentColor);

// Calc new sizes
FOriginalWidth := (PicSeq.Width - 1) div 4;
FOriginalHeight := PicSeq.Height;

// Apply them
if FLockSize then
  Lock(FOriginalWidth, FOriginalHeight);

SetProperties(PicOut);
SetProperties(PicOver);
SetProperties(PicDown);
SetProperties(PicDisabled);

if (FOriginalWidth <> Width) or (FOriginalHeight <> Height) then
  DoResize
else
  // Create/apply button region from picture
  ShapeImg(PicOut, FTransparentColor, Handle);

PicChanged(self);
FImageSwap := True;
Invalidate;

end;

//==============================================================================

function TEkImgBtn.GetPic : TBitmap;
begin

if (PicSeq.Empty) then
  PicSeq := TBitmap.Create;

Result := PicSeq;

end;

//==============================================================================

function TEkImgBtn.GetPicImgIcon;
begin

if (PicImgIcon.Empty) then
  PicImgIcon := TIcon.Create;

PicImgIcon.Transparent := True;
Result := PicImgIcon;

end;

//==============================================================================

function TEkImgBtn.GetPicBmpIcon;
begin

if (PicBmpIcon.Empty) then PicBmpIcon := TBitmap.Create;

PicBmpIcon.Transparent := True;
Result := PicBmpIcon;

end;

//==============================================================================

procedure TEkImgBtn.Lock(const pWidth, pHeight : Integer);
begin

if (Width <> pWidth) and (pWidth > 0) then
  Width := pWidth;
if (Height <> pHeight) and (pHeight > 0) then
  Height := pHeight;

end;

//==============================================================================

procedure TEkImgBtn.SetLockSize(state : Boolean);
begin

if state <> FLockSize then
begin
  FLockSize := state;
  if FLockSize then
    Lock(FOriginalWidth, FOriginalHeight);
  FImageSwap := True;
  Invalidate;
end;

end;

//==============================================================================

procedure TEkImgBtn.SetFadeEnable(state : Boolean);
begin

if state <> FEnableFade then
begin
  FEnableFade := state;
  FImageSwap := True;
  Invalidate;
end;

end;

//==============================================================================

procedure TEkImgBtn.WMSize(var pMsg: TWMSize);
begin

if FLockSize then
  Lock(FOriginalWidth, FOriginalHeight);

DoResize();
PicChanged(self);

end;

//==============================================================================

procedure TEkImgBtn.CMFontChanged(var pMsg : TMessage);
begin

FImageSwap := True;
Invalidate;

end;

//==============================================================================

procedure TEkImgBtn.CMEnabledChanged(var pMsg: TMessage);
begin
  inherited;

if Enabled then
  Font.Color := FFontColor
else
begin
  FFontColor := Font.Color;
  Font.Color := clgray;
end;

if not Enabled then
  FMouse := emDisabled;

FImageSwap := True;
Invalidate;

end;

//==============================================================================

procedure TEkImgBtn.WMKillFocus(var pMsg : TWMKillFocus);
begin
  Invalidate;
inherited;
end;

//==============================================================================

procedure TEkImgBtn.WMSetFocus(var pMsg : TWMSetFocus);
begin
  Invalidate;
inherited;
end;

//==============================================================================

procedure TEkImgBtn.CMMouseEnter(var pMsg : TMessage);
begin
	inherited;
              
FImageSwap := True;
FEnter := True;
FMouse := emEnter;

Invalidate;

end;

//==============================================================================

procedure TEkImgBtn.CMMouseLeave(var pMsg : TMessage);
begin
	inherited;

FImageSwap := True;
FEnter := False;
FMouse := emLeave;

Invalidate;

end;

//==============================================================================

procedure TEkImgBtn.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	inherited MouseDown(Button, Shift, X, Y);

if (Button <> mbLeft) or (FMouse = emDown) then Exit;

FImageSwap := True;
FMouse := emDown;

if not (csDesigning in ComponentState) and not (Focus) then
  SetFocus();

Invalidate;

end;

//==============================================================================

procedure TEkImgBtn.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y : Integer);
begin
	inherited MouseUp(Button, Shift, X, Y);

if (Button <> mbLeft) or (FMouse <> emDown) then Exit;

FImageSwap := True;
FMouse := emUp;

Invalidate;

end;

//==============================================================================

procedure TEkImgBtn.KeyDown(var Key : Word; Shift : TShiftState);
begin

if ((Shift = []) and ((Key = 0032)) or (Key = 0013)) then // Space or return
begin
  self.FadeStart := False;
  if not (csDesigning in ComponentState) and not (Focus) then
    SetFocus();
  FImageSwap := True;
  FMouse := emDown;
  Invalidate;
end;

inherited KeyDown(Key, Shift);
end;

//==============================================================================

procedure TEkImgBtn.KeyUp(var Key : Word; Shift : TShiftState);
begin

if ((Shift = []) and (Key = 0032)) or (Key = 0013) then
begin
  self.FadeStart := False;
  FImageSwap := True;
  if FEnter then
    FMouse := emUp
  else
    FMouse := emLeave;
  Invalidate;

  if Assigned(OnClick) then
    OnClick(nil);
end;

inherited KeyUp(Key,Shift);
end;

//==============================================================================

procedure Register;
begin
	RegisterComponents('EkszBoxVCL', [TEkImgBtn]);
end;

end.
