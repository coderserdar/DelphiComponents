unit EkImgForm;

//=============
//  EkImgForm
//=============

// Copyright (C) 2007-2008 Kernel Master
// Author: Kernel Master
// kmeksz[At]yahoo.com

// Windproc subclass code based on code by Rick Rogers (TeamB)

//==============================================================================

{$WARNINGS OFF}
//{$HINTS OFF}
{$O+} // Optimizations
{$Include EkVCL.inc}

//==============================================================================

interface

uses Windows, Graphics, Classes, Controls, Messages, SysUtils, Forms,
    EkTypes, EkImgFadeBase, EkUtil;

const DEFAULTWIDTH = 271;
      DEFAULTHEIGHT = 188;
const ICONSPACE = 2;

type
  TEkImgForm = class(TGraphicControl)
  //TEkImgForm = class(TCustomControl)

    public

		constructor   Create(AOwner: TComponent); override;
		destructor    Destroy; override;
    procedure     LoadNewImage(const path : string);

	private

    FForm             : TForm;

    FBmpBuf           : TBitmap;
    FPic              : TBitmap;

    FPicIcon          : TIcon;

    FMouse            : TEkMouseStates;
    FOnClick          : TNotifyEvent;
    FOnMouseEnter     : TNotifyEvent;
    FOnMouseLeave     : TNotifyEvent;
    FOnActivate       : TNotifyEvent;
    FOnDeactivate     : TNotifyEvent;
    FOnMinimize       : TNotifyEvent;
    FOnRestore        : TNotifyEvent;

    FTransparentColor : TColor;
    FFontColor        : TColor;

		FCaption          : String;

    FCaptionX         : Integer;
    FCaptionY         : Integer;
    FIconSize         : Integer;

    FIsClicked        : Boolean;
    FWordWrap         : Boolean;
    FFocus            : Boolean;
    FCoverForm        : Boolean;
    FActive           : Boolean;

    FOldAppProc     : Pointer;
    FNewAppProc     : Pointer;
    FOldWndProc     : Pointer;
    FNewWndProc     : Pointer;
    FAppHandle      : HWnd;
    FFormHandle     : HWnd;

    procedure DrawToBuffer();
    procedure DoResize();
		procedure PaintCaption(pCanvas : TCanvas);
    procedure PaintIcon(pCanvas : TCanvas);
    //procedure SetIconPosition();

		procedure PicChanged(Sender : TObject);
		procedure SetCaption(text : String);
    procedure SetCaptionX(x : Integer);
    procedure SetCaptionY(y : Integer);
    procedure SetIconSize(x : Integer);
    procedure SetWordWrap(value : Boolean);
		procedure SetProperties(pic : TBitmap);
    procedure SetTransparentColor(color : TColor);

    procedure SetCoverForm(value : Boolean);
    procedure SetActive(state : Boolean);
    procedure SetClicked(state : TNotifyEvent);
    procedure SetPic(pic : TBitmap);
		function  GetPic : TBitmap;
    procedure SetPicIcon(pic : TIcon);
    function  GetPicIcon : TIcon;

  protected

    procedure Activate(Sender : TObject);
    procedure Deactivate(Sender : TObject);
    procedure Minimized(Sender : TObject);
    procedure Restored(Sender : TObject);
    procedure PaintIt();

    procedure HookApp;
    procedure UnhookApp;
    procedure WndProcApp(var pMsg: TMessage);

    procedure HookForm;
    procedure UnhookForm;
    procedure WndProcForm(var pMsg: TMessage);

    procedure Loaded(); override;
		procedure Paint; override;
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y: Integer); override;
    procedure CMMouseEnter(var pMsg : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var pMsg : TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkgnd(var pMsg: TWMEraseBkgnd); message WM_ERASEBKGND;
    //procedure WMSize(var pMsg: TWMSize); message WM_SIZE;

	published

    property Canvas;

    property Constraints;
    property Align;
    property Tag;
    property Action;
    Property DragCursor;
    Property DragKind;
    Property DragMode;
    property Anchors;
    property BiDiMode;
    property Visible;
	  property ShowHint;
	  property ParentFont;
    property PopupMenu;

    property Font;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;

    property OnClick : TNotifyEvent read FOnClick write SetClicked;
    property OnActivate : TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate : TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnMinimize : TNotifyEvent read FOnMinimize write FOnMinimize;
    property OnRestore : TNotifyEvent read FOnRestore write FOnRestore;

    property Active : Boolean read FActive write SetActive default true;

    property DesignTimeApply : Boolean read FCoverForm write SetCoverForm default false;

    property WordWrap : Boolean read FWordWrap write SetWordWrap default true;
		property Caption : String  read FCaption write SetCaption;
    property CaptionX : Integer read FCaptionX write SetCaptionX default 7;
    property CaptionY : Integer read FCaptionY write SetCaptionY default 4;

    property Icon : TIcon read GetPicIcon write SetPicIcon;
    property IconSize : Integer read FIconSize write SetIconSize default 16;

    property Skin : TBitmap read GetPic write SetPic;
    property TransparentColor : TColor read FTransparentColor write SetTransparentColor default clFuchsia;

	end;

procedure Register;

implementation

//==============================================================================

constructor TEkImgForm.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

ControlStyle := ControlStyle + [csOpaque];

FTransparentColor := clFuchsia;

FBmpBuf :=  TBitmap.Create;
FPic :=  TBitmap.Create;
FPicIcon := TIcon.Create;
//FPicIcon.OnChange := PicChanged;

FPic.OnChange := PicChanged;
SetProperties(FPic);

FActive := True;
FFocus := False;
FWordWrap := True;

FIconSize := 16;
FCaptionX := 7;
FCaptionY := 4;

Width := DEFAULTWIDTH;
Height := DEFAULTHEIGHT;

FCoverForm := False;

try
  FForm := (Owner as TForm);
except
  if Owner is TCustomForm then
    FForm := TForm(Owner)
  else
    exception.Create('Error: FForm could not be created');
end;

FForm.BorderIcons := [];
FForm.BorderStyle := bsNone;
FForm.AutoSize := False;
FForm.AutoScroll := False;
FForm.Position := poDesktopCenter;
{$ifdef COMPILER7_UP}
FForm.ScreenSnap := True;
{$endif}
SendToBack();
     
if Owner is TForm then
if not (csDesigning in ComponentState) then
begin
  HookApp;
  HookForm;
end;

end;

//==============================================================================

destructor TEkImgForm.Destroy;
begin

FBmpBuf.Free;
FPic.Free;
FPicIcon.Free;

if not (csDesigning in ComponentState) then
begin
  UnhookApp;
  UnhookForm;
end;

inherited Destroy;
end;

//==============================================================================

procedure TEkImgForm.Loaded();
begin
inherited;

FMouse := emDisabled;
if not FPic.Empty then
begin
  Left := 0;
  ShapeForm(FForm, FPic, FTransparentColor);
  DrawToBuffer();
end;

end;

//==============================================================================

procedure TEkImgForm.HookApp;
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

procedure TEkImgForm.UnhookApp;
begin

if (FOldAppProc <> nil) then
  SetWindowLong(FAppHandle, GWL_WNDPROC, LongInt(FOldAppProc));
if FNewAppProc <> nil then
  FreeObjectInstance(FNewAppProc);

FOldAppProc := nil;
FNewAppProc := nil;

end;

//==============================================================================

procedure TEkImgForm.HookForm;
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

procedure TEkImgForm.UnhookForm;
begin

if (FOldWndProc <> nil) then
  SetWindowLong(FFormHandle, GWL_WNDPROC, LongInt(FOldWndProc));
if FNewWndProc <> nil then
  FreeObjectInstance(FNewWndProc);

FOldWndProc := nil;
FNewWndProc := nil;

end;

//==============================================================================

procedure TEkImgForm.WndProcApp(var pMsg: TMessage);
begin

case pMsg.Msg of

  WM_SIZE:
  begin
    if (pMsg.wParam = SIZE_MINIMIZED) then
      Minimized(self)
    else if (pMsg.wParam = SIZE_RESTORED) then
      Restored(self);
  end;

end;

pMsg.Result := CallWindowProc(FOldAppProc, FAppHandle,
pMsg.Msg, pMsg.wParam, pMsg.lParam);

end;

//==============================================================================

procedure TEkImgForm.WndProcForm(var pMsg: TMessage);
begin

case pMsg.Msg of

  WM_SIZE:
    DoResize();

  WM_ACTIVATE:

    if Assigned(Screen.ActiveControl) //and Assigned(Screen.ActiveControl.Parent)
    then
      if (pMsg.WParamLo <> WA_INACTIVE) then
      begin
        SetActive(True);
        Activate(self);
      end
      else
      begin
        SetActive(False);
        Deactivate(self);
      end;

end;

pMsg.Result := CallWindowProc(FOldWndProc, FFormHandle,
pMsg.Msg, pMsg.wParam, pMsg.lParam);

end;

//==============================================================================

procedure TEkImgForm.Activate(Sender : TObject);
begin

if Assigned(FOnActivate) then FOnActivate(self);

end;

//==============================================================================

procedure TEkImgForm.Deactivate(Sender : TObject);
begin

if Assigned(FOnDeactivate) then FOnDeactivate(self);

end;

//==============================================================================

procedure TEkImgForm.Minimized(Sender : TObject);
begin

if Assigned(FOnMinimize) then FOnMinimize(self);

end;

//==============================================================================

procedure TEkImgForm.Restored(Sender : TObject);
begin

if Assigned(FOnRestore) then FOnRestore(self);

end;

//==============================================================================

procedure TEkImgForm.DrawToBuffer();
begin

FBmpBuf.Assign(FPic);

end;

//==============================================================================

procedure TEkImgForm.DoResize();
var
  ogWidth, ogHeight : Integer;
  temp : TBitmap;
begin

Exit; // Unfinished function!!
                       
if (Width <> FPic.Width) and (Height <> FPic.Height)
  then Exit;

//showmessage(inttostr(width));
//showmessage(inttostr(FPic.Width));

ogWidth := FPic.Width;
ogHeight := FPic.Height;

temp := TBitmap.Create;
temp.Width := Width;
temp.Height := Height;

SetProperties(temp);

ResizeImage(FPic, temp, 0, ogWidth, ogHeight, Width+10, Height);

FPic.Width := Width;
FPic.Height := Height;
FBmpBuf.Width := Width;
FBmpBuf.Height := Height;

FPic.Assign(temp);
temp.Free;

ShapeForm(FForm, FPic, FTransparentColor);

DrawToBuffer();
Invalidate;

end;

//==============================================================================

procedure TEkImgForm.PaintIt();
begin
           
if not Visible then Exit;

if (csDesigning in ComponentState) then
if not FCoverForm then
begin
  Width := 25;
  Height := 25;
  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Style := bsClear; // bsSolid
  Canvas.Rectangle(0, 0, Width, Height);
  Exit;
end;

// Locksize
if (FPic.Empty = False) then
begin
  if Width <> FPic.Width then
    Width := FPic.Width;
  if Height <> FPic.Height then
    Height := FPic.Height;
end;

if not FPic.Empty then
  if (Width > FBmpBuf.Width) or (Width > FBmpBuf.Width) or (Width > FBmpBuf.Width) then
    DoResize();

PaintCaption(FBmpBuf.Canvas);
PaintIcon(FBmpBuf.Canvas);
Canvas.Draw(0, 0, FBmpBuf);

end;

//==============================================================================

procedure TEkImgForm.Paint;
begin

PaintIt();

end;

//==============================================================================

procedure TEkImgForm.PaintCaption(pCanvas : TCanvas);
var
	x, y, txtWidth : Integer;
  r : TRect;
begin

pCanvas.Brush.Style := bsClear;
pCanvas.Font.Assign(Font);
pCanvas.Font.Color := Font.Color;

txtWidth := pCanvas.TextWidth(FCaption);
if not FPicIcon.Empty then
  x := FCaptionX + FIconSize + ICONSPACE
else
  x := FCaptionX;

y := FCaptionY;

if FWordWrap and (txtWidth > ClientWidth) then // Wrap text
begin
  r.Left := 6;
  r.Top := y;
  r.Right := Width;
  r.Bottom := Height;

  DrawTextEx(pCanvas.Handle, PChar(FCaption), Length(FCaption),
  r, (DT_END_ELLIPSIS), nil );
end
else
begin
  ExtTextOut(pCanvas.Handle, x, y, ETO_CLIPPED, nil,
  PChar(FCaption), Length(FCaption), nil);
end;

end;

//==============================================================================

procedure TEkImgForm.PaintIcon(pCanvas : TCanvas);
begin

if (not FPicIcon.Empty) then
begin
  pCanvas.Draw(FCaptionX, FCaptionY, FPicIcon);
end;

end;

//==============================================================================

procedure TEkImgForm.PicChanged(Sender: TObject);
begin

if not(csLoading In ComponentState) then
begin
  SetProperties(FPic);
  Invalidate;
end;

end;

//==============================================================================

procedure TEkImgForm.SetCaption(text: string);
begin

  if (FCaption <> text) then
  begin
    FCaption := text;
    Canvas.Font.Assign(Font);
    Canvas.Brush.Style := bsClear;
    Perform(CM_TEXTCHANGED, 0, 0);
    DrawToBuffer();
    Invalidate;
  end;

end;

//==============================================================================

procedure TEkImgForm.SetCaptionX(x : Integer);
begin
if x <> FCaptionX then
begin
  FCaptionX := x;
  DrawToBuffer();
  Invalidate;
end;
end;

//==============================================================================

procedure TEkImgForm.SetCaptionY(y : Integer);
begin
if y <> FCaptionY then
begin
  FCaptionY := y;
  DrawToBuffer();
  Invalidate;
end;
end;

//==============================================================================

procedure TEkImgForm.SetIconSize(x : Integer);
begin
if x <> FIconSize then
begin
  FIconSize := x;
  DrawToBuffer();
  Invalidate;
end;
end;

//==============================================================================

procedure TEkImgForm.SetWordWrap(value: Boolean);
begin

if (FWordWrap <> value) then
begin
  FWordWrap := value;
  DrawToBuffer();
  Invalidate;
end;

end;

//==============================================================================

procedure TEkImgForm.SetProperties(pic: TBitmap);
begin

if pic.Empty = false then
begin

  SetDefaultBmpType(pic);

  if (pic.Width > 0) and (pic.Width <> Width) then
    Width := pic.Width;

  if (pic.Height > 0) and (pic.Height <> Height) then
    Height := pic.Height;

end;

end;

//==============================================================================

procedure TEkImgForm.SetTransparentColor(color : TColor);
begin

if color <> FTransparentColor then
begin
  FTransparentColor := color;
  ShapeForm(FForm, FPic, FTransparentColor);
  Invalidate;
end;

end;

//==============================================================================

procedure TEkImgForm.SetPic(pic: TBitmap);
begin

FPic.Assign(pic);
DrawToBuffer();
SendToBack();

if (FPic.Empty) then
begin
  Invalidate;
  Exit;
end;

FPic.Width := pic.Width;
FPic.Height := pic.Height;
Left := 0;
Top := 0;
FForm.Width := pic.Width;
FForm.Height := pic.Height;
SetProperties(FPic);

Invalidate;

end;

//==============================================================================

function TEkImgForm.GetPic : TBitmap;
begin

if (FPic.Empty) then FPic := TBitmap.Create;

Result := FPic;

end;

//==============================================================================

procedure TEkImgForm.SetPicIcon(pic: TIcon);
begin

FPicIcon.Assign(pic);

if (not FPicIcon.Empty) then
begin
  FPicIcon.Transparent := True;
  FIconSize := FPicIcon.Width;
end;

DrawToBuffer();
Invalidate;

end;

//==============================================================================

function TEkImgForm.GetPicIcon : TIcon;
begin

if (FPicIcon.Empty) then
  FPicIcon := TIcon.Create;

FPicIcon.Transparent := True;
Result := FPicIcon;
Invalidate;

end;

//==============================================================================

procedure TEkImgForm.SetCoverForm(value : Boolean);
begin

if value <> FCoverForm then
begin
  FCoverForm := value;
  Invalidate;
end;

end;

//==============================================================================

procedure TEkImgForm.SetActive(state : Boolean);
var
  bmp : TBitmap;
begin

//if state <> FActive then
begin
  FActive := state;
  if (FActive) then
  begin
    FBmpBuf.Assign(FPic);
    Font.Color := FFontColor;
  end
  else
  begin
    bmp := TBitmap.Create;
    bmp.Assign(FPic);
    FBmpBuf.Assign(ChangeBrightness(bmp, -40));
    bmp.Free;
    FFontColor := Font.Color;
    Font.Color := clGray;
  end;

  Invalidate;
end;

end;

//==============================================================================

procedure TEkImgForm.SetClicked(state : TNotifyEvent);
begin

if FMouse <> emDown then
begin
  FOnClick := state;
  FIsClicked := not FIsClicked;
end;

end;

//==============================================================================

procedure TEkImgForm.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	inherited MouseDown(Button, Shift, X, Y);

if (Button <> mbLeft) or (FMouse = emDown) then Exit;

FMouse := emDown;
FFocus := True;

// Easy drag form
if (Button = mbLeft) then
begin
  if Assigned(FForm) then
  begin
    ReleaseCapture;
    FForm.Perform(WM_LBUTTONUP, 0, 0);
    FForm.Perform(WM_SYSCOMMAND, $F012, 8);
  end;
end;

end;

//==============================================================================

procedure TEkImgForm.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y : Integer);
begin
	inherited MouseUp(Button, Shift, X, Y);

if (Button <> mbLeft) or (FMouse <> emDown) then Exit;

FMouse := emUp;

if FIsClicked then
if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
  OnClick(nil);

end;

//==============================================================================

procedure TEkImgForm.CMMouseEnter(var pMsg : TMessage);
begin
  inherited;

if (csDesigning in ComponentState) then Exit;

if (pMsg.LParam = 0) and Assigned(FOnMouseEnter) then
  FOnMouseEnter(self);

end;

//==============================================================================

procedure TEkImgForm.CMMouseLeave(var pMsg : TMessage);
begin
inherited;

if (csDesigning in ComponentState) then Exit;

if (pMsg.LParam = 0) and Assigned(FOnMouseLeave) then
  FOnMouseLeave(self);

end;

//==============================================================================

procedure TEkImgForm.WMEraseBkgnd(var pMsg: TWMEraseBkgnd);
begin
  pMsg.Result := 1;
end;

//==============================================================================

procedure TEkImgForm.LoadNewImage(const path : string);
var
bmp : TBitmap;
begin

bmp := TBitmap.Create;
bmp.LoadFromFile(path);

//FForm.DoubleBuffered := True;
SetPic(bmp);
ShapeForm(FForm, FPic, FTransparentColor);
//FForm.DoubleBuffered := False;

bmp.Free;

end;

//==============================================================================

procedure Register;
begin
	RegisterComponents('EkszBoxVCL', [TEkImgForm]);
end;

end.
