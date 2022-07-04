unit EkImgFade;

//================
//  EkImgFade
//================

// Copyright (C) 2007-2008 Kernel Master
// Author: Kernel Master
// kmeksz[At]yahoo.com

// Portions of tweening code taken from / inspired by 'Inside Delphi 2006'

//==============================================================================

{$WARNINGS OFF}
//{$HINTS OFF}
{$O+} // Optimizations

//==============================================================================

interface

uses
  Windows, Graphics, Classes, Controls, Messages, ExtCtrls,
  EkTypes, EkUtil;


type
  TEkImgFade = class(TCustomControl)

 public

    constructor Create(AOwner: TComponent); override;
		destructor  Destroy; override;

    procedure LoadImgFrom(path : String);
    procedure LoadImgTo(path : String);

  private

    FBmpBuf         : TBitmap;
    FBmpFrom        : TBitmap;
    FBmpTo          : TBitmap;

    FDirection      : TEkDirection;
    FFadeFx         : TEkFadeFx;

    FOnFadeComplete : TNotifyEvent;
    FFadeStartStop  : TNotifyEvent;
    FOnMouseEnter   : TNotifyEvent;
    FOnMouseLeave   : TNotifyEvent;

    FFadeStart      : Boolean;
    FUpdateAuto     : Boolean;
    FTransparent    : Boolean;
    FLockSize       : Boolean;
    FLoop           : Boolean;
    FAutoReverse    : Boolean;
    FCenter         : Boolean;
    FIsPainting     : Boolean;
//    FAlphaBlend     : Boolean;
//    FAlphaBlendValue: Byte;

    FOffset         : Integer;
    FStates         : Integer;
    FUpdateInterval : Integer;
    FTimer          : TTimer;

    procedure TimerHandler(pSender : TObject);
    procedure SetUpdateInterval(pValue : Integer);
    procedure SetFadeStart(pValue : Boolean);
    procedure SetLoop(pValue : Boolean);
    procedure PicChanged(pSender : TObject);
    procedure SetProperties(pPic : TBitmap);
    procedure SetAutoReverse(pValue : Boolean);
    procedure SetDirection(pValue : TEkDirection);
    procedure SetFadeFx(pValue : TEkFadeFx);
    procedure CalcDirection();
    procedure SetTransparent(pValue : Boolean);
    //procedure SetAlphaBlendValue(pValue : Byte);
    procedure SetAutoUpdate(pValue : Boolean);
    procedure SetCenter(pValue: Boolean);

    function  GetImgFrom : TPicture;
	  procedure SetImgFrom(pPic : TPicture);
    function  GetImgTo : TPicture;
	  procedure SetImgTo(pPic : TPicture);

    //procedure SetFinished(state : TNotifyEvent);

  protected

		procedure Paint; override;
    procedure DoFade();
    procedure WMEraseBkgnd(var pMsg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CMMouseEnter(var pMsg : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var pMsg : TMessage); message CM_MOUSELEAVE;

  published

    property OnFadeStartStop : TNotifyEvent read FFadeStartStop write FFadeStartStop;
    property OnFadeComplete : TNotifyEvent read FOnFadeComplete write FOnFadeComplete;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property Visible;
    property Align;
    property Anchors;
    property AutoSize;
    property Center: Boolean read FCenter write SetCenter default False;

    property LockSize : Boolean read FLockSize write FLockSize default true;
    property Fade : Boolean read FFadeStart write SetFadeStart default false;
    property FadeFx : TEkFadeFx read FFadeFx write SetFadeFx default efxfTween;
    property UpdateInterval : Integer read FUpdateInterval write SetUpdateInterval default 40;
    property Loop : Boolean read FLoop write SetLoop default false;
    property AutoReverse : Boolean read FAutoReverse write SetAutoReverse default false;
    property Direction : TEkDirection read FDirection write SetDirection default edForward;
    property Transparent : Boolean read FTransparent write SetTransparent default false;
//    property AlphaBlendValue: Byte read mAlphaBlendValue write SetAlphaBlendValue default 255;
//    property AlphaBlend: Boolean read mAlphaBlend write mAlphaBlend default False;
    property States : Integer read FStates write FStates default 40;
    property StateOffset : Integer read FOffset write FOffset;
    property UpdateAuto : Boolean read FUpdateAuto write SetAutoUpdate default false;

    property ImgFrom : TPicture read GetImgFrom write SetImgFrom;
    property ImgTo : TPicture read GetImgTo write SetImgTo;

  end;

procedure Register;

implementation


//==============================================================================

procedure TEkImgFade.LoadImgFrom(path : String);
var img : TPicture;
begin

img := TPicture.Create;
img.LoadFromFile(path);
SetImgFrom(img);
img.Free;

end;

//==============================================================================

procedure TEkImgFade.LoadImgTo(path : String);
var img : TPicture;
begin

img := TPicture.Create;
img.LoadFromFile(path);
SetImgTo(img);
img.Free;

end;

//==============================================================================

constructor TEkImgFade.Create(AOwner: TComponent);
begin
inherited Create(AOwner);

ControlStyle := ControlStyle + [csOpaque];

FBmpBuf := TBitmap.Create;
FBmpFrom := TBitmap.Create;
FBmpTo := TBitmap.Create;
SetProperties(FBmpBuf);
SetProperties(FBmpFrom);
SetProperties(FBmpTo);

FBmpFrom.OnChange := PicChanged;
FBmpTo.OnChange := PicChanged;

//mAlphaBlendValue := 255;
//mAlphaBlend := False;
FFadeFx := efxfTween;
FDirection := edForward;
FLoop := False;
FTransparent := False;
FAutoReverse := False;
FLockSize := True;
FUpdateAuto := False;
Fade := False;
FOffset := 1;
FUpdateInterval := 40;
FStates := 40;

FTimer := TTimer.Create(nil);
FTimer.Interval := FUpdateInterval;
FTimer.OnTimer := TimerHandler;
FTimer.Enabled := True;

end;

//==============================================================================

destructor TEkImgFade.Destroy;
begin

FBmpBuf.Destroy;
FBmpFrom.Destroy;
FBmpTo.Destroy;
FTimer.Destroy;

Inherited Destroy;
end;

//==============================================================================

procedure TEkImgFade.Paint;
begin

if not Visible then Exit;

FIsPainting := True;
//SetBkMode(Canvas.Handle, 1); // Transparent

if (csDesigning in ComponentState) then //Design-time paint
  if FBmpFrom.Empty and FBmpTo.Empty then
  begin
    with Canvas do
    begin
      //Pen.Style := psDash;
      //Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;
    Exit;
  end;

if FBmpBuf.Empty then
begin
  if (FDirection = edForward) then
    FBmpBuf.Assign(FBmpFrom)
  else
    FBmpBuf.Assign(FBmpTo);
end;

if (FLockSize) then
begin
  if (not(Width = FBmpFrom.Width) and not(Height = FBmpFrom.Height)
  and FBmpFrom.Empty = False) then
    begin
      Width := FBmpFrom.Width;
      Height := FBmpFrom.Height;
    end
  else if (not(Width = FBmpTo.Width) and not(Height = FBmpTo.Height)
  and FBmpTo.Empty = False) then
    begin
      Width := FBmpTo.Width;
      Height := FBmpTo.Height;
    end
end;


DoFade();
Canvas.Draw(0, 0, FBmpBuf);

FIsPainting := False;
//TransparentBlt(Canvas.Handle, 0, 0, Width, Height,
//FBmpBuf.Handle, 0, 0, Width, Height, clFuchsia);

end;

//==============================================================================

procedure TEkImgFade.TimerHandler(pSender : TObject);
begin
if FUpdateAuto and FFadeStart and not FIsPainting then
  Invalidate;
end;

//==============================================================================

procedure TEkImgFade.PicChanged(pSender : TObject);
begin
  //Invalidate;
end;

//==============================================================================

procedure TEkImgFade.CalcDirection();
begin

if FAutoReverse and (FOffset > FStates) and FFadeStart then
begin
  if FDirection = edReverse then
    FDirection := edForward
  else
    FDirection := edReverse;
end;

end;

//==============================================================================

procedure TEkImgFade.SetProperties(pPic: TBitmap);
begin

if (FTransparent = True) then
	pPic.Transparent := True;

if FLockSize then
begin
  if Width <> pPic.Width then
    Width := pPic.Width;
  if Height <> pPic.Height then
    Height := pPic.Height;
end;

if FBmpBuf.Width <> Width then
  FBmpBuf.Width := Width;
if FBmpBuf.Height <> Height then
  FBmpBuf.Height := Height;

SetDefaultBmpType(FBmpBuf);
SetDefaultBmpType(FBmpFrom);
SetDefaultBmpType(FBmpTo);
SetDefaultBmpType(pPic);

end;

//==============================================================================

procedure TEkImgFade.SetTransparent(pValue: Boolean);
begin

FTransparent := pValue;

if (FTransparent = True) then
begin
  FBmpBuf.Transparent := True;
  FBmpFrom.Transparent := True;
  FBmpTo.Transparent := True;
end
else
begin
  FBmpBuf.Transparent := False;
  FBmpFrom.Transparent := False;
  FBmpTo.Transparent := False;
end;

Invalidate;

end;

//==============================================================================

//procedure TEkImgFade.SetAlphaBlendValue(pValue : Byte);
//var
//  SetLayeredWindowAttributes: TSetLayeredWindowAttributes;
//begin
//
//mAlphaBlendValue := pValue;
//
//if mAlphaBlend then
//begin
//  if not (csDesigning in ComponentState) then
//    SetLayeredWindowAttributes :=
//    GetProcAddress(GetModulehandle(user32), 'SetLayeredWindowAttributes');
//    SetLayeredWindowAttributes(Handle, 0, mAlphaBlendValue, LWA_ALPHA);
//end;
//Invalidate;
//
//end;

//==============================================================================

procedure TEkImgFade.SetAutoUpdate(pValue : Boolean);
begin
if FUpdateAuto <> pValue then
  begin
    FUpdateAuto := pValue;
    Invalidate;
  end;
end;

//==============================================================================

procedure TEkImgFade.SetCenter(pValue: Boolean);
begin
  if FCenter <> pValue then
  begin
    FCenter := pValue;
    //PictureChanged(self);
  end;
end;

//==============================================================================

procedure TEkImgFade.SetAutoReverse(pValue : Boolean);
begin
  if FAutoReverse <> pValue then
  begin
    FAutoReverse := pValue;
    Invalidate;
  end;
end;

//==============================================================================

procedure TEkImgFade.SetDirection(pValue : TEkDirection);
begin
  if FDirection <> pValue then
  begin
    FDirection := pValue;
    Invalidate;
  end;
end;

//==============================================================================

procedure TEkImgFade.SetFadeFx(pValue : TEkFadeFx);
begin
  if FFadeFx <> pValue then
  begin
    FFadeFx := pValue;
    Invalidate;
  end;
end;

//==============================================================================

procedure TEkImgFade.DoFade();
var
  pos : Integer;
  y : Integer;
  x : Integer;
  grey, strength, invert, mixB, mixG, mixR :Integer;
  b, g ,r  : Byte;
  pixCur, pixSrc, pixDest : PRGBTriple;
begin

if (FBmpFrom.Empty) then
  Exit
else if (FBmpTo.Empty) then
  FBmpTo.Assign(FBmpFrom);

if not Fade then // Return only after bmp has been assigned ^
  Exit;

Inc(FOffset);
pos := FOffset;
CalcDirection();

              
if (pos = FStates) then // Fire OnFinish event
begin
  if Assigned(FOnFadeComplete) then
    FOnFadeComplete(self);
end;

// Reset at end
if (pos > FStates) and FLoop = True then
begin
  FOffset := 1;
end
else if (not FLoop) and (pos > FStates) then
begin
  if Assigned(FFadeStartStop) then FFadeStartStop(self); // Fire event
  Fade := False;
  //mOffset := 1;
end;

//if (h > Height) or (h < 0) then
//  h := 0;
//
//if (pos <= FStates) then
//try
//begin
//  pixCur := FBmpBuf.Scanline[h];
//  if FDirection = edForward then
//  begin
//    pixSrc := FBmpFrom.scanline[h];
//    pixDest := FBmpTo.scanline[h];
//  end
//  else
//  begin
//    pixSrc := FBmpTo.scanline[h];
//    pixDest := FBmpFrom.scanline[h];
//  end;
//  for x := 0 to Pred(FBmpBuf.Width) do
//  begin
//    pixCur.rgbtRed := pixDest.rgbtRed;
//    pixCur.rgbtGreen := pixDest.rgbtGreen;
//    //pixCur.rgbtGreen := ((FStates - pos) * pixSrc.rgbtGreen + pos * pixDest.rgbtGreen) div FStates;
//    pixCur.rgbtBlue := pixDest.rgbtBlue;
//    Inc(pixCur);
//    Inc(pixDest);
//    Inc(pixSrc);
//  end;
//end;
//except
//end;
//h := h + 1;
//Exit;

try
  if (pos <= FStates) then
      for y := 0 to Pred(FBmpBuf.Height) do
      begin
        pixCur := FBmpBuf.Scanline[y];
        if FDirection = edForward then
          begin
            pixSrc := FBmpFrom.scanline[y];
            pixDest := FBmpTo.scanline[y];
          end
          else
          begin
            pixSrc := FBmpTo.scanline[y];
            pixDest := FBmpFrom.scanline[y];
          end;
        for x := 0 to Pred(FBmpBuf.Width) do
        begin
          if FFadeFx = efxfTween then
          begin
            pixCur.rgbtBlue := ((FStates - pos) * pixSrc.rgbtBlue + pos * pixDest.rgbtBlue) div FStates;
            pixCur.rgbtGreen := ((FStates - pos) * pixSrc.rgbtGreen + pos * pixDest.rgbtGreen) div FStates;
            pixCur.rgbtRed := ((FStates - pos) * pixSrc.rgbtRed + pos * pixDest.rgbtRed) div FStates;
          end
          else if FFadeFx = efxfTweenFade then
          begin
            b := ((FStates - pos) * (pixDest.rgbtBlue shr 7)  + pos * pixDest.rgbtBlue) div FStates;
            g := ((FStates - pos) * (pixDest.rgbtGreen shr 7)  + pos * pixDest.rgbtGreen) div FStates;
            r := ((FStates - pos) * (pixDest.rgbtRed shr 7) + pos * pixDest.rgbtRed) div FStates;
            pixCur.rgbtBlue := ((FStates - pos) * pixSrc.rgbtBlue + pos * Clip(b)) div FStates;
            pixCur.rgbtGreen := ((FStates - pos) * pixSrc.rgbtGreen + pos * Clip(g)) div FStates;
            pixCur.rgbtRed := ((FStates - pos) * pixSrc.rgbtRed + pos * Clip(r)) div FStates;
          end
          else if FFadeFx = efxfFade1 then
          begin
            strength := 100 div pos;
            b := ((FStates - pos) * (pixSrc.rgbtBlue shr 7)  + pos * pixSrc.rgbtBlue) div FStates;
            g := ((FStates - pos) * (pixSrc.rgbtGreen shr 7)  + pos * pixSrc.rgbtGreen) div FStates;
            r := ((FStates - pos) * (pixSrc.rgbtRed shr 7) + pos * pixSrc.rgbtRed) div FStates;
            pixCur.rgbtBlue := ((FStates - pos) * Clip(pixSrc.rgbtBlue + strength) + pos * Clip(b)) div FStates;
            pixCur.rgbtGreen := ((FStates - pos) * Clip(pixSrc.rgbtGreen + strength) + pos * Clip(g)) div FStates;
            pixCur.rgbtRed := ((FStates - pos) * Clip(pixSrc.rgbtRed + strength) + pos * Clip(r)) div FStates;
          end
          else if FFadeFx = efxfFade2 then begin
            b := ((FStates - pos) * (pixSrc.rgbtBlue shr 8) shl 7 + pos * pixSrc.rgbtBlue) div FStates;
            g := ((FStates - pos) * (pixSrc.rgbtGreen shr 8) shl 7 + pos * pixSrc.rgbtGreen) div FStates;
            r := ((FStates - pos) * (pixSrc.rgbtRed shr 8) shl 7 + pos * pixSrc.rgbtRed) div FStates;
            pixCur.rgbtBlue := Clip(b);
            pixCur.rgbtGreen := Clip(g);
            pixCur.rgbtRed := Clip(r);
          end
          else if FFadeFx = efxfInvert then
          begin
            strength := 255 div (FStates - pos);
            b := not (((FStates - pos) * pos * Clip(strength - pixDest.rgbtBlue)) div FStates);
            g := not (((FStates - pos) * pos * Clip(strength - pixDest.rgbtGreen)) div FStates);
            r := not (((FStates - pos) * pos * Clip( strength - pixDest.rgbtRed)) div FStates);
            pixCur.rgbtBlue := ((FStates - pos) * pixSrc.rgbtBlue + pos * b) div FStates;
            pixCur.rgbtGreen := ((FStates - pos) * pixSrc.rgbtGreen + pos * g) div FStates;
            pixCur.rgbtRed := ((FStates - pos) * pixSrc.rgbtRed + pos * r) div FStates;
          end
          else if FFadeFx = efxfGreyscale then
          begin
            grey := (pixDest.rgbtRed * 3 + pixDest.rgbtBlue * 4 + pixDest.rgbtGreen * 2) div 9;
            b := ((FStates - pos) * grey + pos * pixDest.rgbtBlue) div FStates;
            g := ((FStates - pos) * grey + pos * pixDest.rgbtGreen) div FStates;
            r := ((FStates - pos) * grey + pos * pixDest.rgbtRed) div FStates;
            pixCur.rgbtBlue := ((FStates - pos) * pixSrc.rgbtBlue + pos * b) div FStates;
            pixCur.rgbtGreen := ((FStates - pos) * pixSrc.rgbtGreen + pos * g) div FStates;
            pixCur.rgbtRed := ((FStates - pos) * pixSrc.rgbtRed + pos * r) div FStates;
          end
          else if FFadeFx = efxfMess then
          begin
            b := (FStates - pos) * pixDest.rgbtBlue - pixDest.rgbtBlue div FStates;
            g := (FStates - pos) * pixDest.rgbtGreen - pixDest.rgbtGreen div FStates;
            pixCur.rgbtBlue := (FStates - pos) * (pixSrc.rgbtBlue + pos * b) div FStates;
            pixCur.rgbtGreen := (FStates - pos) * (pixSrc.rgbtGreen + pos * g) div FStates;
            pixCur.rgbtRed := (FStates - pos) * (pixSrc.rgbtRed + pos + pixDest.rgbtRed * pixSrc.rgbtRed) div FStates;
          end
          else if FFadeFx = efxfFlash then
          begin
            strength := (255 * (100 div pos)) div 100;
            b := Clip(pixDest.rgbtBlue - strength);
            g := Clip(pixDest.rgbtGreen - strength);
            r := Clip(pixDest.rgbtRed - strength);
            pixCur.rgbtBlue := ((FStates - pos) * Clip(pixSrc.rgbtBlue + strength) + pos * b) div FStates;
            pixCur.rgbtGreen := ((FStates - pos) * Clip(pixSrc.rgbtGreen + strength) + pos * g) div FStates;
            pixCur.rgbtRed := ((FStates - pos) * Clip(pixSrc.rgbtRed + strength) + pos * r) div FStates;
          end
          else if FFadeFx = efxfBuild then
          begin
            b := ((FStates - pos) * (pixSrc.rgbtBlue shr 8) shl 7 + pos * pixSrc.rgbtBlue) div FStates;
            g := ((FStates - pos) * (pixSrc.rgbtGreen shr 8) shl 7 + pos * pixSrc.rgbtGreen) div FStates;
            r := ((FStates - pos) * (pixSrc.rgbtRed shr 8) shl 7 + pos * pixSrc.rgbtRed) div FStates;
            pixCur.rgbtBlue := Clip(b);
            pixCur.rgbtGreen := Clip(g);
            pixCur.rgbtRed := Clip(r);
          end
          else if FFadeFx = efxfColour1 then
          begin
            strength := (255 * (100 div pos)) div 100;
            b := Clip(pixDest.rgbtRed + pixSrc.rgbtRed - strength);
            g := Clip(pixSrc.rgbtGreen + strength);
            r := Clip(pixDest.rgbtRed - strength);
            pixCur.rgbtBlue := b;
            pixCur.rgbtGreen := g;
            pixCur.rgbtRed := r;
          end
          else if FFadeFx = efxfColour2 then
          begin
            strength := (255 * (100 div pos)) div 100;
            b := Clip(pixSrc.rgbtBlue - strength);
            g := Clip(pixDest.rgbtGreen + strength);
            r := Clip(pixDest.rgbtRed - strength);
            pixCur.rgbtBlue := b;
            pixCur.rgbtGreen := g;
            pixCur.rgbtRed := r;
          end;
          Inc(pixCur);
          Inc(pixSrc);
          Inc(pixDest);
        end;
    end;
except
end;

end;

//==============================================================================

procedure TEkImgFade.SetUpdateInterval(pValue : Integer);
begin

if FUpdateInterval <> pValue then
begin
  FUpdateInterval := pValue;
  FTimer.Interval := pValue;
  Invalidate;
end;

end;

//==============================================================================

procedure TEkImgFade.SetFadeStart(pValue : Boolean);
begin

if pValue <> FFadeStart then
  begin
    FFadeStart := pValue;
    FOffset := 1;
    if Assigned(FFadeStartStop) then
      FFadeStartStop(self); // Fire event
    Invalidate;
  end;

end;

//==============================================================================

procedure TEkImgFade.SetLoop(pValue : Boolean);
begin

if pValue <> FLoop then
begin
  FLoop := pValue;
  //mOffset := 1;
end;

end;

//==============================================================================

function  TEkImgFade.GetImgFrom : TPicture;
begin

if (FBmpFrom.Empty) then FBmpFrom := TBitmap.Create;
if (FTransparent) then FBmpFrom.Transparent := True;

try
  Result := TPicture(FBmpFrom);
except
end;

end;

//==============================================================================

procedure TEkImgFade.SetImgFrom(pPic : TPicture);
begin

FBmpFrom.Assign(pPic.Graphic);
FBmpBuf.Assign(FBmpFrom);

if (FBmpFrom.Empty) then
begin
  Invalidate;
  Exit;
end;

SetProperties(FBmpFrom);

FOffset := 1;
Invalidate;

end;

//==============================================================================

function  TEkImgFade.GetImgTo : TPicture;
begin


if (FBmpTo.Empty) then FBmpTo := TBitmap.Create;
if (FTransparent) then FBmpTo.Transparent := True;

try
  Result := TPicture(FBmpTo);
except
end;

end;

//==============================================================================

procedure TEkImgFade.SetImgTo(pPic : TPicture);
begin

FBmpTo.Assign(pPic.Graphic);

if FBmpFrom.Empty then // Show only if bmpTo is empty
  FBmpBuf.Assign(FBmpTo);

//FBmpBuf.Assign(FBmpFrom); // ?

if (FBmpTo.Empty) then
begin
  Invalidate;
  Exit;
end;

SetProperties(FBmpTo);

FOffset := 1;
Invalidate;

end;

//==============================================================================

//procedure TEkImgFade.SetFinished(state : TNotifyEvent);
//begin
//
////if (moffset > FStates) then
////begin
//  mOnFinish := state;
////end;
//
//end;

////==============================================================================
//
//procedure TEkImgFade.CreateParams(var pParams: TCreateParams);
//begin
//  inherited CreateParams(pParams);
//
//pParams.ExStyle := pParams.ExStyle or WS_EX_TRANSPARENT;
//
//end;

//==============================================================================

procedure TEkImgFade.WMEraseBkgnd(var pMsg: TWMEraseBkgnd);
begin
  if not (csDesigning in ComponentState) then Exit;
  pMsg.Result := 1;
end;

//==============================================================================

procedure TEkImgFade.CMMouseEnter(var pMsg : TMessage);
begin
  inherited;

if (csDesigning in ComponentState) then Exit;

if (pMsg.LParam = 0) and Assigned(FOnMouseEnter) then
  FOnMouseEnter(self);

end;

//==============================================================================

procedure TEkImgFade.CMMouseLeave(var pMsg : TMessage);
begin

if (csDesigning in ComponentState) then Exit;

if (pMsg.LParam = 0) and Assigned(FOnMouseLeave) then
  FOnMouseLeave(self);

inherited;
end;

//==============================================================================

procedure Register;
begin
  RegisterComponents('EkszBoxVCL', [TEkImgFade]);
end;

end.
