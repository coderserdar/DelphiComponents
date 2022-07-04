unit EkImgFadeBase;

//===================
//  EkImgFadeBase
//===================

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
  Windows, Graphics, Classes, Controls, Messages, ExtCtrls, SysUtils, Forms,
  EkTypes, EkUtil, dialogs;

type
  TEkImgFadeBase = class(TCustomControl)

 public

    constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;

  private

    FBmpBuf         : TBitmap;
    FBmpFrom        : TBitmap;
    FBmpTo          : TBitmap;

    FTimer          : TTimer;

    FDirection      : TEkDirection;
    FFadeFx         : TEkFadeFx;

    FOnClick        : TNotifyEvent;
    FOnMouseEnter   : TNotifyEvent;
    FOnMouseLeave   : TNotifyEvent;
    FOnActivate     : TNotifyEvent;
    FOnDeactivate   : TNotifyEvent;

    FOffset         : Integer;
    FStates         : Integer;
    FUpdateInterval : Integer;

    FUpdateAuto     : Boolean;
    FFadeStart      : Boolean;
    FTransparent    : Boolean;
    FLockSize       : Boolean;
    FLoop           : Boolean;
    FAutoReverse    : Boolean;
    FIsPainting     : Boolean;
    FIsClicked      : Boolean;
    FFocus          : Boolean;

    FOldAppProc     : Pointer;
    FNewAppProc     : Pointer;
    FOldWndProc     : Pointer;
    FNewWndProc     : Pointer;
    FAppHandle      : HWnd;
    FFormHandle     : HWnd;

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

    {procedure SetTransparent(pValue : Boolean);
    procedure SetAutoUpdate(pValue : Boolean);}

    procedure HookApp;
    procedure UnhookApp;

    procedure HookForm;
    procedure UnhookForm;

    procedure SetClicked(state : TNotifyEvent);

  protected

    procedure FormActivated(); virtual;
    procedure FormDeactivated(); virtual;
    procedure FormRestored(); virtual;
    procedure FormMinimized(); virtual;

    procedure WndProcApp(var pMsg: TMessage); virtual;
    procedure WndProcForm(var pMsg: TMessage); virtual;

    procedure CreateWnd(); override;
    procedure DoFade();
    procedure Paint; override;
    procedure CreateParams(var pParams: TCreateParams); override;
    procedure WMEraseBkgnd(var pMsg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CMCtl3DChanged(var pMsg: TMessage); message CM_CTL3DCHANGED;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure WMKillFocus(var pMsg : TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var pMsg : TWMSetFocus); message WM_SETFOCUS;
    //procedure WndProc(var Message : TMessage); override;

    procedure CMMouseEnter(var pMsg : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var pMsg : TMessage); message CM_MOUSELEAVE;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y : Integer); override;

    property BmpBuf : TBitmap read FBmpBuf write FBmpBuf;
    property BmpFrom : TBitmap read FBmpFrom write FBmpBuf;
    property BmpTo : TBitmap read FBmpTo write FBmpBuf;
    property Offset : Integer read FOffset write FOffset;
    property UpdateAuto : Boolean read FUpdateAuto write FUpdateAuto;
    property FadeStart : Boolean read FFadeStart write SetFadeStart; 
    property FadeDirection : TEkDirection read FDirection write SetDirection default edForward;
    property Transparent : Boolean read FTransparent write FTransparent;
    property Focus : Boolean read FFocus write FFocus;

  published
    property Enabled;
    property Constraints;
    property Align;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property Action;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Anchors;
    property BiDiMode;
    property Visible;
	  property ShowHint;
	  property ParentFont;
    property PopupMenu;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnStartDock;
    property OnStartDrag;
    //property OnClick;
    property OnClick : TNotifyEvent read FOnClick write SetClicked;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnEnter;
    property OnExit;
    property OnContextPopup;

    property OnFormActivate : TNotifyEvent read FOnActivate write FOnActivate;
    property OnFormDeactivate : TNotifyEvent read FOnDeactivate write FOnDeactivate;
    {
    property Color;
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
    }

    property FadeFx : TEkFadeFx read FFadeFx write SetFadeFx default efxfTween;
    property FadeUpdateInterval : Integer read FUpdateInterval write SetUpdateInterval default 40;
    property FadeLoop : Boolean read FLoop write SetLoop default false;
    property FadeAutoReverse : Boolean read FAutoReverse write SetAutoReverse default false;
    property FadeStates : Integer read FStates write FStates default 10;

  end;


implementation

//==============================================================================

constructor TEkImgFadeBase.Create(AOwner: TComponent);
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

FDirection := edForward;
FLoop := False;
FTransparent := False;
FAutoReverse := False;
FLockSize := True;
FUpdateAuto := False;
FFadeStart := False;
FFocus := False;
FOffset := 1;
FUpdateInterval := 40;
FStates := 10;

TabStop := True;

FTimer := TTimer.Create(self);
FTimer.Interval := FUpdateInterval;
FTimer.OnTimer := TimerHandler;
FTimer.Enabled := True;

if Owner is TForm then
if not (csDesigning in ComponentState) then
begin
  HookApp;
  HookForm;
end;

end;

//==============================================================================

destructor TEkImgFadeBase.Destroy;
begin

FTimer.Free;
FBmpBuf.Free;
FBmpFrom.Free;
FBmpTo.Free;

if not (csDesigning in ComponentState) then
begin
  UnhookApp;
  UnhookForm;
end;

inherited Destroy;
end;

//==============================================================================

procedure TEkImgFadeBase.CreateWnd();
begin
  inherited;

if FTransparent then
SetWindowLong( parent.handle, GWL_STYLE,
GetWindowLong( parent.handle, GWL_STYLE ) and not WS_CLIPCHILDREN );

end;

//==============================================================================

procedure TEkImgFadeBase.HookApp;
begin

if (Application.Handle <> 0) then
  FAppHandle := Application.Handle
else
  raise Exception.Create('Error: Couldn''t get Application.Handle');

FOldAppProc := Pointer(GetWindowLong(FAppHandle, GWL_WNDPROC));
FNewAppProc := MakeObjectInstance(WndProcApp);

if (FNewAppProc = nil) then
  raise EOutOfResources.Create('Error: WndProc pointer = nil');

SetWindowLong(FAppHandle, GWL_WNDPROC, LongInt(FNewAppProc));

end;

//==============================================================================

procedure TEkImgFadeBase.UnhookApp;
begin

if (FOldAppProc <> nil) then
  SetWindowLong(FAppHandle, GWL_WNDPROC, LongInt(FOldAppProc));
if FNewAppProc <> nil then
  FreeObjectInstance(FNewAppProc);

FOldAppProc := nil;
FNewAppProc := nil;

end;

//==============================================================================

procedure TEkImgFadeBase.HookForm;
begin

if ((Owner as TForm).FormStyle = fsMDIForm) then
  FFormHandle := (Owner as TForm).ClientHandle
else
  FFormHandle := (Owner as TForm).Handle;

FOldWndProc := Pointer(GetWindowLong(FFormHandle, GWL_WNDPROC));
FNewWndProc := MakeObjectInstance(WndProcForm);

if (FNewWndProc = nil) then
  raise EOutOfResources.Create('Error: WndProc pointer = nil');

SetWindowLong(FFormHandle, GWL_WNDPROC, LongInt(FNewWndProc));

end;

//==============================================================================

procedure TEkImgFadeBase.UnhookForm;
begin

if (FOldWndProc <> nil) then
  SetWindowLong(FFormHandle, GWL_WNDPROC, LongInt(FOldWndProc));
if FNewWndProc <> nil then
  FreeObjectInstance(FNewWndProc);

FOldWndProc := nil;
FNewWndProc := nil;

end;

//==============================================================================

procedure TEkImgFadeBase.WndProcApp(var pMsg: TMessage);
begin

case pMsg.Msg of

  WM_SIZE:
    begin
      if (pMsg.wParam = SIZE_MINIMIZED) then
        FormMinimized()
      else if (pMsg.wParam = SIZE_RESTORED) then
        FormRestored();
    end;

end;

pMsg.Result := CallWindowProc(FOldAppProc, FAppHandle,
pMsg.Msg, pMsg.wParam, pMsg.lParam);

end;

//==============================================================================

procedure TEkImgFadeBase.WndProcForm(var pMsg: TMessage);
begin

case pMsg.Msg of

    WM_ACTIVATE:
      begin
        if Assigned(Screen.ActiveControl) //and Assigned(Screen.ActiveControl.Parent)
        then
          if (pMsg.WParamLo <> WA_INACTIVE) then
            FormActivated()
          else
            FormDeactivated();
      end;

end;

pMsg.Result := CallWindowProc(FOldWndProc, FFormHandle,
pMsg.Msg, pMsg.wParam, pMsg.lParam);

end;

//==============================================================================

procedure TEkImgFadeBase.FormActivated();
begin

if Assigned(FOnActivate) then FOnActivate(self);

//Invalidate;

end;

//==============================================================================

procedure TEkImgFadeBase.FormDeactivated();
begin

if Assigned(FOnDeactivate) then FOnDeactivate(self);
//Invalidate;

end;

//==============================================================================

procedure TEkImgFadeBase.FormRestored();
begin

FormActivated();
//

end;

//==============================================================================

procedure TEkImgFadeBase.FormMinimized();
begin

//

end;

//==============================================================================

procedure TEkImgFadeBase.Paint;
begin

if not Visible then Exit;

FIsPainting := True;

if FBmpBuf.Empty then
begin
  if (FDirection = edForward) then
    FBmpBuf.Assign(FBmpFrom)
  else
    FBmpBuf.Assign(FBmpTo);
end;

FIsPainting := False;

end;

//==============================================================================

procedure TEkImgFadeBase.TimerHandler(pSender : TObject);
begin
if FUpdateAuto and FFadeStart and not FIsPainting then
  Invalidate;
end;

//==============================================================================

procedure TEkImgFadeBase.PicChanged(pSender : TObject);
begin
end;

//==============================================================================

procedure TEkImgFadeBase.CalcDirection();
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

procedure TEkImgFadeBase.SetProperties(pPic: TBitmap);
begin

//if (FTransparent = True) then
//  pPic.Transparent := True;
Width := pPic.Width;
Height := pPic.Height;
FBmpBuf.Width := Width;
FBmpBuf.Height := Height;
SetDefaultBmpType(pPic);

end;

//==============================================================================

procedure TEkImgFadeBase.SetAutoReverse(pValue : Boolean);
begin
  if FAutoReverse <> pValue then
  begin
    FAutoReverse := pValue;
    Invalidate;
  end;
end;

//==============================================================================

procedure TEkImgFadeBase.SetDirection(pValue : TEkDirection);
begin
  if FDirection <> pValue then
  begin
    FDirection := pValue;
    Invalidate;
  end;
end;

//==============================================================================

procedure TEkImgFadeBase.SetFadeFx(pValue : TEkFadeFx);
begin
  if FFadeFx <> pValue then
  begin
    FFadeFx := pValue;
    Invalidate;
  end;
end;

//==============================================================================

procedure TEkImgFadeBase.DoFade();
var
  pos : Integer;
  y : Integer;
  x : Integer;
  grey, strength, b, g, r : Byte;
  pixCur, pixSrc, pixDest : PRGBTriple;
begin

if (FBmpFrom.Empty or FBmpTo.Empty) then
  Exit;

if not FFadeStart then // Return only after bmp has been assigned ^
  Exit;

Inc(FOffset);
pos := FOffset;
//if not FadeStart then
CalcDirection();

if (pos > FStates) and FLoop = True then
  FOffset := 1
else if (not FLoop) and (pos > FStates) then
begin             
  FadeStart := False;
  //FOffset := 1;
end;

//SetDefaultBmpType(FBmpBuf);
//SetDefaultBmpType(FBmpFrom);
//SetDefaultBmpType(FBmpTo);

try
  if (pos <= FStates) then
      for y := 0 to FBmpBuf.Height-1 do
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
        for x := 0 to FBmpBuf.Width-1 do
        begin
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
              r := (FStates - pos) * pixDest.rgbtRed - pixDest.rgbtRed ;
              pixCur.rgbtBlue := ((FStates - pos) * pixSrc.rgbtBlue + pos * b) div FStates;
              pixCur.rgbtGreen := ((FStates - pos) * pixSrc.rgbtGreen + pos * g) div FStates;
              pixCur.rgbtRed := ((FStates - pos) * pixSrc.rgbtRed + pos * r) div FStates;
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
    end;
except
end;

end;

//==============================================================================

procedure TEkImgFadeBase.SetUpdateInterval(pValue : Integer);
begin
  if FUpdateInterval <> pValue then
  begin
    FUpdateInterval := pValue;
    FTimer.Interval := pValue;
    Invalidate;
  end;
end;

//==============================================================================

procedure TEkImgFadeBase.SetFadeStart(pValue : Boolean);
begin
  //if pValue <> FFadeStart then
  begin
    FFadeStart := pValue;
    //FOffset := 1;
    Invalidate;
  end;
end;

//==============================================================================

procedure TEkImgFadeBase.SetLoop(pValue : Boolean);
begin
  if pValue <> FLoop then
    begin
      FLoop := pValue;
      FOffset := 1;
      Invalidate;
    end;
end;

//==============================================================================

procedure TEkImgFadeBase.CreateParams(var pParams: TCreateParams);
begin
  inherited CreateParams(pParams);
  if FTransparent then
    pParams.ExStyle := pParams.ExStyle or WS_EX_TRANSPARENT
  else
    inherited;
end;

//==============================================================================

procedure TEkImgFadeBase.WMEraseBkgnd(var pMsg: TWMEraseBkgnd);
begin
  pMsg.Result := 1;
end;

//==============================================================================

procedure TEkImgFadeBase.CMCtl3DChanged(var pMsg: TMessage);
begin
inherited;
  Invalidate;
end;

//==============================================================================

procedure TEkImgFadeBase.CMFocusChanged(var Message: TCMFocusChanged);
begin
  Invalidate;
end;

//==============================================================================

procedure TEkImgFadeBase.WMKillFocus(var pMsg : TWMKillFocus);
begin
  FFocus := False;
inherited;
end;

//==============================================================================

procedure TEkImgFadeBase.WMSetFocus(var pMsg : TWMSetFocus);
begin
  FFocus := True;
inherited;
end;

//==============================================================================

procedure TEkImgFadeBase.CMMouseEnter(var pMsg : TMessage);
begin
	inherited;

if (pMsg.LParam = 0) and Assigned(FOnMouseEnter) then
  FOnMouseEnter(self);

end;

//==============================================================================

procedure TEkImgFadeBase.CMMouseLeave(var pMsg : TMessage);
begin
	inherited;

if (pMsg.LParam = 0) and Assigned(FOnMouseLeave) then
  FOnMouseLeave(self);

end;

//==============================================================================

procedure TEkImgFadeBase.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y : Integer);
begin
	inherited MouseUp(Button, Shift, X, Y);

if FIsClicked then
if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
  OnClick(nil);

end;

//==============================================================================

procedure TEkImgFadeBase.SetClicked(state : TNotifyEvent);
begin

FOnClick := state;
FIsClicked := not FIsClicked;

end;

//==============================================================================

//procedure TEkImgFadeBase.WndProc(var Message : TMessage);
//begin
//
//inherited WndProc(Message);
//end;


//==============================================================================

end.
