unit EkImgCheckBox;

//=================
//  EkImgCheckBox
//=================

// Copyright (C) 2007-2008 Kernel Master
// Author: Kernel Master
// kmeksz[At]yahoo.com

//==============================================================================

//{$WARNINGS OFF}
//{$HINTS OFF}
{$O+} // Optimizations

//==============================================================================

interface

uses
	Windows, Graphics, Classes, Controls, Messages,
  EkTypes, EkImgFadeBase, EkUtil;

const
  DEFAULT_WIDTH = 90;
  DEFAULT_HEIGHT = 17;


type
  TEkImgCheckBox = class(TEkImgFadeBase)

	private

		PicChk            : TBitmap;
    PicChkOver        : TBitmap;
    PicChkDown        : TBitmap;
    PicChkDisabled    : TBitmap;
    PicUnChk          : TBitmap;
    PicUnChkOver      : TBitmap;
    PicUnChkDown      : TBitmap;
    PicUnChkDisabled  : TBitmap;
    PicSeq            : TBitmap;

    FCheck            : TEkCheckStates;
    FMouse            : TEkMouseStates;
    FOnClick          : TNotifyEvent;
    FOnMouseEnter     : TNotifyEvent;
    FOnMouseLeave     : TNotifyEvent;

    FTextOverColor    : TColor;
    FTransparentColor : TColor;
    FFontColor        : TColor;

    FCaption          : String;

    FTextWidth        : Integer;
    FTextHeight       : Integer;
    FTextSpacing      : Integer;
    FPicWidth         : Integer;
    FPicHeight        : Integer;
    FYImage           : Integer;

    FFadeUponLoad     : Boolean;
    FIsClicked        : Boolean;
    FIsChecked        : Boolean;
    FEnter            : Boolean;
    FDefaultName      : Boolean;
    FEnableFade       : Boolean;
    FFocusRect        : Boolean;
    FImageSwap        : Boolean;

    procedure ShapeIt();
    procedure PaintIt();
    procedure DrawTxt(const x : Integer; pCanvas : TCanvas);
		procedure PicChanged(Sender : TObject);
    procedure SetCaption(text : String);
    //procedure SetFocusRect(value : Boolean);

		procedure SetProperties(pic : TBitmap);
    procedure SetChecked(checked : Boolean);

    procedure SetPic(pic : TBitmap);
  	function GetPic : TBitmap;
    procedure SetTextSpacing(value : Integer);
    procedure SetTextOverColor(value : TColor);
    procedure SetYImage(value : Integer);

  protected

    procedure CreateWnd; override;
    procedure Loaded(); override;
		procedure Paint; override;

    procedure CMMouseEnter(var pMsg : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var pMsg : TMessage); message CM_MOUSELEAVE;
		procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y: Integer); override;
    procedure SetClicked(state : TNotifyEvent);
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure KeyUp(var Key : Word; Shift : TShiftState); override;

    procedure WMSize(var pMsg: TWMSize); message WM_SIZE;
    procedure CMFontChanged(var pMsg: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var pMsg: TMessage); message CM_ENABLEDCHANGED;
    procedure WMKillFocus(var pMsg : TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var pMsg : TWMSetFocus); message WM_SETFOCUS;

  public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;

	published
    property Font;

    //property FocusRect : Boolean read FFocusRect write SetFocusRect default false;
    property OnClick : TNotifyEvent read FOnClick write SetClicked;
    property FadeEnable : Boolean read FEnableFade write FEnableFade default false;
    property FadeUponLoad : Boolean read FFadeUponLoad write FFadeUponLoad default false;
    property Checked : Boolean read FIsChecked write SetChecked default True;
    property Caption : String  read FCaption write SetCaption;
    property TextSpacing : Integer read FTextSpacing write SetTextSpacing default 6;
    property TextOverColor : TColor  read FTextOverColor write SetTextOverColor default clRed;
  	property Skin : TBitmap read GetPic write SetPic;
    property ImgY : Integer read FYImage write SetYImage default 0;

	end;

procedure Register;

implementation

//==============================================================================

constructor TEkImgCheckBox.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

ControlStyle := ControlStyle + [csClickEvents];

FFadeUponLoad := False;
self.UpdateAuto := False;
FEnableFade := False;
self.FadeStart := False;
self.Transparent := True;

FFocusRect := False;
FDefaultName := True;
FTextOverColor := clRed;

FIsChecked := True;

PicChk := TBitmap.Create;
PicChkOver := TBitmap.Create;
PicChkDown := TBitmap.Create;
PicChkDisabled := TBitmap.Create;
PicUnChk := TBitmap.Create;
PicUnChkOver := TBitmap.Create;
PicUnChkDown := TBitmap.Create;
PicUnChkDisabled := TBitmap.Create;
PicSeq := TBitmap.Create;

PicSeq.OnChange := PicChanged;

FTextSpacing := 6;
FYImage := 0;

Width := DEFAULT_WIDTH;
Height := DEFAULT_HEIGHT;

end;

//==============================================================================

destructor TEkImgCheckBox.Destroy;
begin

PicChk.Free;
PicChkOver.Free;
PicChkDown.Free;
PicChkDisabled.Free;
PicUnChk.Free;
PicUnChkOver.Free;
PicUnChkDown.Free;
PicUnChkDisabled.Free;
PicSeq.Free;

inherited Destroy;
end;

//==============================================================================

procedure TEkImgCheckBox.CreateWnd;
begin
  inherited;

ShapeIt();

end;


//==============================================================================

procedure TEkImgCheckBox.Loaded();
begin
inherited;

if FFadeUponLoad then
  FMouse := emLeave
else
  FMouse := emDisabled;

SetPic(PicSeq);
FImageSwap := True;

end;

//==============================================================================

procedure TEkImgCheckBox.PaintIt();
var
  x, y : Integer;
begin

// Set default component name
If (Self.Text <> '') and (FDefaultName) then
  SetCaption(Self.Text);


// Design-time paint
if (csDesigning in ComponentState) then
begin

  if PicSeq.Empty then
  begin
    if (Caption <> '') then
    begin
      Canvas.Pen.Style := psSolid;
      Brush.Style := bsClear;
      Canvas.Rectangle(0, 0, Height-1, Height);
      DrawTxt(Height-2 + FTextSpacing, Canvas);
    end
    else
    begin
      Canvas.Pen.Style := psSolid;
      Brush.Style := bsClear;
      Canvas.Rectangle(0, 0, Width, Height);
    end;
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
    if FIsChecked then
      FCheck := ecCheck
    else
      FCheck := ecUnCheck;
    if (FIsChecked) then
      self.BmpBuf.Assign(PicChk)
    else if (not FIsChecked) then
      self.BmpBuf.Assign(PicUnChk);
  end

  else if not Enabled then
  begin
    if not FIsChecked then
      self.BmpBuf.Assign(PicUnChkDisabled)
    else 
      self.BmpBuf.Assign(PicChkDisabled);
  end

  else if (FCheck = ecUnCheckDown) then
  begin // Down
      self.BmpBuf.Assign(PicUnChkDown)
  end

  else if (FCheck = ecCheckDown) then
  begin // Down
      self.BmpBuf.Assign(PicChkDown)
  end

  else if (FCheck = ecUnCheck) then
  begin // UnCheckOut
    if FEnableFade then // Fade
    begin
        self.BmpBuf.Assign(PicUnChkOver);
        self.BmpFrom.Assign(PicUnChkOver);
        self.BmpTo.Assign(PicUnChk);
        self.FadeDirection := edForward;
        self.FadeStart := True;
    end
    else // No fade
    begin
      self.BmpBuf.Assign(PicUnChk);
      self.BmpFrom.Assign(PicUnChk);
    end;
  end

  else if (FCheck = ecCheck) then
  begin // CheckOut
    if FEnableFade then // Fade
    begin
      self.BmpBuf.Assign(PicChkOver);
      self.BmpFrom.Assign(PicChkOver);
      self.BmpTo.Assign(PicChk);
      self.FadeDirection := edForward;
      self.FadeStart := True;
    end
    else // No fade
    begin
      self.BmpBuf.Assign(PicChk);
      self.BmpFrom.Assign(PicChk);
    end;
  end

  else if ((FCheck = ecUnCheckOver) and not PicUnChkOver.Empty) then
  begin // UnCheckOver
    if FEnableFade and ((FMouse <> emDown) and (FMouse <> emUp)) then // Fade
    begin
      self.BmpBuf.Assign(PicUnChk);
      self.BmpFrom.Assign(PicUnChk);
      self.BmpTo.Assign(PicUnChkOver);
      self.FadeDirection := edForward;
      self.FadeStart := True;
    end
    else // No fade
    begin
      self.BmpBuf.Assign(PicUnChkOver);
      self.BmpFrom.Assign(PicUnChkOver);
    end;
  end

  else if (FCheck = ecCheckOver) then
  begin // CheckOver
    if FEnableFade and ((FMouse <> emDown) and (FMouse <> emUp)) then // Fade
    begin
      self.BmpBuf.Assign(PicChk);
      self.BmpFrom.Assign(PicChk);
      self.BmpTo.Assign(PicChkOver);
      self.FadeDirection := edForward;
      self.FadeStart := True;
    end
    else // No fade
      begin
        self.BmpBuf.Assign(PicChkOver);
        self.BmpFrom.Assign(PicChkOver);
      end;
  end;

  FImageSwap := False;
end;


if FEnableFade and self.FadeStart then
begin
  self.DoFade();
  self.UpdateAuto := True;
end
else if FEnableFade and (not self.FadeStart) and (self.Offset <> 1) then
begin // Reset animation to first frame
  if (Enabled) then
  begin
    if (FCheck = ecCheck) then
      self.BmpBuf.Assign(PicChk)
    else if (FCheck = ecUnCheck) then
      self.BmpBuf.Assign(PicUnChk)
    else if (FCheck = ecCheckOver) then
      self.BmpBuf.Assign(PicChkOver)
    else if (FCheck = ecUnCheckOver) then
      self.BmpBuf.Assign(PicUnChkOver);
  end;
  self.Offset := 1;
  self.UpdateAuto := False;
end;


if (Caption <> '') then
  DrawTxt(0, Canvas);

// Buf image placement calc
if (Caption = '') then
  y := 0
else
begin
  FTextHeight := Canvas.TextHeight(Caption);
  if FTextHeight > FPicHeight then
    y := (FTextHeight div 2) - (FPicHeight div 2) + FYImage
  else
    y := 0 + FYImage;
end;

x := 0;

Canvas.Draw(x, y, self.BmpBuf);

end;

//==============================================================================

procedure TEkImgCheckBox.Paint;
begin

if not Visible then Exit;
PaintIt();

end;

//==============================================================================

procedure TEkImgCheckBox.ShapeIt();
var bmp : TBitmap;
begin

if PicSeq.Empty then Exit;

// Create/apply button region from picture
bmp := TBitmap.Create;
bmp.PixelFormat := pf24Bit;
DrawTxt(0, bmp.Canvas); // Just to calc/set size
bmp.Width := Width;
bmp.Height := Height;

bmp.Canvas.Pen.Color := clBlack;
bmp.Canvas.Pen.Style := psSolid;
bmp.Canvas.Brush.Style := bsSolid;
bmp.Canvas.Brush.Color := clBlack;
bmp.Canvas.Rectangle(0, 0, Width, Height);

BitBlt(bmp.Canvas.Handle, 0, 0,
bmp.Width, bmp.Height,
PicUnChk.Canvas.Handle, 0, 0, SRCCOPY);

ShapeImg(bmp, FTransparentColor, Handle);
bmp.Free;

end;

//==============================================================================

procedure TEkImgCheckBox.DrawTxt(const x : Integer; pCanvas : TCanvas);
var
  r : TRect;
begin

// Component width
if Caption = '' then
begin
  if FPicWidth = 0 then
    Width := 13
  else
    Width := FPicWidth + FTextSpacing;
  Exit;
end;

FTextWidth := pCanvas.TextWidth(Caption);

// Font setup
pCanvas.Font.Assign(Font);

// Calc
if FPicWidth <> 0 then
  r.Left := x + FPicWidth + FTextSpacing
else
  r.Left := x + 2;


if not PicUnChk.Empty then
  r.Top := (PicUnChk.Height div 2) - (pCanvas.TextHeight(Caption) div 2)
else
  r.Top := (Height div 2) - (pCanvas.TextHeight(Caption) div 2);

r.Right := r.Left + FTextWidth;
r.Bottom := r.Top + pCanvas.TextHeight(Caption);


if FPicWidth <> 0 then
  Width := r.Right
else
  Width := x + FTextWidth +3;

if FPicHeight <> 0 then
  begin
  if pCanvas.TextHeight(Caption) > FPicHeight then
    Height := pCanvas.TextHeight(Caption)
  else
    Height := FPicHeight;
  end
else
  Height := pCanvas.TextHeight(Caption);


if (FMouse = emLeave) or (FMouse = emDisabled) then
  pCanvas.Font.Color := Font.Color
else
  pCanvas.Font.Color := FTextOverColor;

DrawText(pCanvas.Handle, PChar(Caption), Length(Caption), r, 0);

if FFocusRect and Focus then
begin
  r.Left := FPicWidth+3;
  r.Bottom := Height;
  InflateRect(r, 2, 2);
  //PaintFocusRect(r, pCanvas); // Buggy
end;

end;

//==============================================================================

procedure TEkImgCheckBox.PicChanged(Sender: TObject);
begin
  Invalidate;
end;

//==============================================================================

procedure TEkImgCheckBox.SetCaption(text: string);
begin

if (FCaption <> text) then
begin
  FCaption := text;
  Canvas.Font.Assign(Font);
  Canvas.Brush.Style := bsClear;

  FDefaultName := False;
  if (FCaption = '') and (FPicWidth <> 0) then
    Width := FPicWidth
  else if (FCaption = '') then
    Width := Height-1;

  Perform(CM_TEXTCHANGED, 0, 0);
  FImageSwap := True;
  Invalidate;
end;

end;

////==============================================================================
//
//procedure TEkImgCheckBox.SetFocusRect(value: Boolean);
//begin
//  if value <> FFocusRect then
//  begin
//    FFocusRect := value;
//    Invalidate;
//  end;
//end;

//==============================================================================

procedure TEkImgCheckBox.SetProperties(pic: TBitmap);
begin

pic.Transparent := False;

if (pic.Width > 0) then
begin
  if Caption <> '' then
    Width := pic.Width + FTextSpacing + FTextWidth
  else
    Width := pic.Width;
end;

SetDefaultBmpType(pic);

end;

//==============================================================================

procedure TEkImgCheckBox.SetChecked(checked: Boolean);
begin

FIsChecked := checked;

if FIsChecked then
  FCheck := ecCheck
else
  FCheck := ecUnCheck;

FImageSwap := True;
Invalidate;

end;

//==============================================================================

procedure TEkImgCheckBox.SetPic(pic: TBitmap);
begin

PicSeq.Assign(pic);

if (PicSeq.Empty) then
begin
  Invalidate;
  Exit;
end;

SplitCheckboxBmp(pic, PicUnChk, PicUnChkOver, PicUnChkDown,
PicUnChkDisabled, PicChk, PicChkOver, PicChkDown, PicChkDisabled,
FTransparentColor);

FPicWidth := PicUnChk.Width;
FPicHeight := PicUnChk.Height+1;//???????????????????

Height := PicUnChk.Height;

SetProperties(PicUnChk);
SetProperties(PicUnChkOver);
SetProperties(PicUnChkDown);
SetProperties(PicUnChkDisabled);
SetProperties(PicChk);
SetProperties(PicChkOver);
SetProperties(PicChkDown);
SetProperties(PicChkDisabled);

ShapeIt();

PicChanged(self);
FImageSwap := True;
Invalidate;

end;

//==============================================================================

function TEkImgCheckBox.GetPic;
begin

if (PicSeq.Empty) then
  PicSeq := TBitmap.Create;

Result := PicSeq;

end;

//==============================================================================

procedure TEkImgCheckBox.SetTextSpacing(value : Integer);
begin
if value <> FTextSpacing then
begin
  FTextSpacing := value;
  Invalidate;
end;
end;

//==============================================================================

procedure TEkImgCheckBox.SetTextOverColor(value : TColor);
begin
  if value <> FTextOverColor then
  begin
    FTextOverColor := value;
    Invalidate;
  end;
end;

//==============================================================================

procedure TEkImgCheckBox.SetYImage(value : Integer);
begin
if value <> FYImage then
begin
  FYImage := value;
  Invalidate;
end;
end;

//==============================================================================

procedure TEkImgCheckBox.WMSize(var pMsg: TWMSize);
begin

//DoResize();
Invalidate;

end;

//==============================================================================

procedure TEkImgCheckBox.CMFontChanged(var pMsg: TMessage);
begin

FImageSwap := True;
Invalidate;

end;

//==============================================================================

procedure TEkImgCheckBox.CMEnabledChanged(var pMsg: TMessage);
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

procedure TEkImgCheckBox.WMKillFocus(var pMsg : TWMKillFocus);
begin
  Invalidate;
inherited;
end;

//==============================================================================

procedure TEkImgCheckBox.WMSetFocus(var pMsg : TWMSetFocus);
begin
  Invalidate;
inherited;
end;

//==============================================================================

procedure TEkImgCheckBox.CMMouseEnter(var pMsg : TMessage);
begin
  inherited;

if FIsChecked then
  FCheck := ecCheckOver
else
  FCheck := ecUnCheckOver;

FImageSwap := True;
FEnter := True;
FMouse := emEnter;
Invalidate;

if (pMsg.LParam = 0) and Assigned(FOnMouseEnter) then
  FOnMouseEnter(self);


end;

//==============================================================================

procedure TEkImgCheckBox.CMMouseLeave(var pMsg : TMessage);
begin
  inherited;

if FIsChecked then
  FCheck := ecCheck
else
  FCheck := ecUnCheck;

FImageSwap := True;
FEnter := False;
FMouse := emLeave;
Invalidate;

if (pMsg.LParam = 0) and Assigned(FOnMouseLeave) then
  FOnMouseLeave(self);

end;

//==============================================================================

procedure TEkImgCheckBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

if (Button <> mbLeft) or ((FCheck = ecCheckDown) or (FCheck = ecUnCheckDown))
then Exit;

FMouse := emDown;
if FIsChecked then
  FCheck := ecCheckDown
else
  FCheck := ecUnCheckDown;

FImageSwap := True;

if not (csDesigning in ComponentState) and not (Focus) then
  SetFocus();

Invalidate;

inherited;
end;

//==============================================================================

procedure TEkImgCheckBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
inherited MouseUp(Button, Shift, X, Y);

if (Button <> mbLeft) or ((FCheck <> ecCheckDown) and (FCheck <> ecUnCheckDown))
  then Exit;

FIsChecked := not FIsChecked;
FMouse := emUp;

if FIsChecked then
  FCheck := ecCheckOver
else
  FCheck := ecUnCheckOver;

FImageSwap := True;
Invalidate;

if FIsClicked then
if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
  OnClick(nil);

end;

//==============================================================================

procedure TEkImgCheckBox.SetClicked(state : TNotifyEvent);
begin
if (FCheck <> ecUnCheckDown) and (FCheck <> ecCheckDown) then
  begin
    FOnClick := state;
    FIsClicked := not FIsClicked;
  end;
end;

//==============================================================================

procedure TEkImgCheckBox.KeyDown(var Key : Word; Shift : TShiftState);
begin

if (Shift = []) and (Key = 0032) then
begin
  if not FEnter then
    FMouse := emLeave;

  if (FIsChecked) then
    FCheck := ecCheckDown
  else
    FCheck := ecUnCheckDown;

  FImageSwap := True;
  if not (csDesigning in ComponentState) and not (Focus) then
    SetFocus();
  Invalidate;
end;

inherited KeyDown(Key, Shift);
end;

//==============================================================================

Procedure TEkImgCheckBox.KeyUp(var Key : Word; Shift : TShiftState);
begin

if (Shift = []) and (Key = 0032) then
begin
  FIsChecked := not FIsChecked;

  if FEnter then
  begin
    if FIsChecked then
      FCheck := ecCheckOver
    else
      FCheck := ecUnCheckOver;
  end
  else
  begin
    if FIsChecked then
      FCheck := ecCheck
    else
      FCheck := ecUnCheck;
  end;

  
  FImageSwap := False;
  self.Offset := 0; // Kludge
  Invalidate;

  if Assigned(OnClick) then
    OnClick(self);
end;

inherited KeyUp(Key,Shift);
end;

//==============================================================================

procedure Register;
begin
	RegisterComponents('EkszBoxVCL', [TEkImgCheckBox]);
end;

end.
