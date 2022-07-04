unit EkImgLED;

//=============
//  EkImgLED
//=============

// Copyright (C) 2007-2008 Kernel Master
// Author: Kernel Master
// kmeksz[At]yahoo.com

// Skinned 7 segment LED

//==============================================================================

//{$WARNINGS OFF}
//{$HINTS OFF}
{$O+} // Optimizations

//==============================================================================

interface

uses Windows, Graphics, Classes, Controls, Messages,
    EkTypes, EkUtil;

const DEFAULTWIDTH = 80;
      DEFAULTHEIGHT = 40;

type
  TEkImgLED = class(TCustomControl)

    public

		constructor Create(AOwner: TComponent); override;
		destructor  Destroy; override;

    procedure   LoadBmpSkin(path : String);

	private

    FPicBuf           : TBitmap;
    FPicLED           : array [0..10] of TBitmap;
    FPicSeq           : TBitmap;

    FOnMouseEnter     : TNotifyEvent;
    FOnMouseLeave     : TNotifyEvent;
    FLedType          : TEkLEDTypes;
    {
      mLedValClip: How mValue is clipped if it's higher/lower than
      what the LED can display
    }
    FLedValClip       : TEkLEDValClip;

    FTransparentColor : TColor;

    FDigitWidth       : Integer;
    FValue            : Integer;
    FMaxValue         : Integer;

    procedure PaintIt();
    procedure SetDimensions();
    procedure SingleLEDPaint(const x : Integer; src : TCanvas);
    procedure ResetLEDPaint();
    procedure LEDPaint();
		procedure PicChanged(Sender : TObject);
		procedure SetProperties(pic : TBitmap);

    function  GetPic : TBitmap;
    procedure SetPic(pic : TBitmap);

    function  GetValue : Integer;
    procedure SetValue(value : Integer);

    procedure SetLEDType(value : TEkLEDTypes);
    procedure SetLEDValClip(value : TEkLEDValClip);

  protected
            
    procedure Loaded(); override;
		procedure Paint; override;
    procedure CMMouseEnter(var Msg : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg : TMessage); message CM_MOUSELEAVE;
    //procedure WMEraseBkgnd(var pMsg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CMEnabledChanged(var pMsg: TMessage); message CM_ENABLEDCHANGED;

	published

    property Constraints;
    property Align;
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

    property Font;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnStartDock;
    property OnStartDrag;

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

    property Enabled;
    property LEDType : TEkLEDTypes read FLedType write SetLedType default eltQuad;
    property LEDClipType : TEkLEDValClip read FLedValClip write SetLEDValClip default elcNone;
    property Skin : TBitmap read GetPic write SetPic;
    property Value : Integer read GetValue write SetValue default 0;

	end;

procedure Register;

implementation

//==============================================================================

constructor TEkImgLED.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

ControlStyle := ControlStyle + [csOpaque];

FPicBuf :=  TBitmap.Create;
FPicLED[0] := TBitmap.Create;
FPicLED[1] := TBitmap.Create;
FPicLED[2] := TBitmap.Create;
FPicLED[3] := TBitmap.Create;
FPicLED[4] := TBitmap.Create;
FPicLED[5] := TBitmap.Create;
FPicLED[6] := TBitmap.Create;
FPicLED[7] := TBitmap.Create;
FPicLED[8] := TBitmap.Create;
FPicLED[9] := TBitmap.Create;
FPicLED[10] := TBitmap.Create;
FPicSeq :=  TBitmap.Create;

FPicSeq.OnChange := PicChanged;

FLedType := eltQuad;
FLedValClip := elcNone;
FValue := 0;
TabStop := True;

Width := DEFAULTWIDTH;
Height := DEFAULTHEIGHT;

end;

//==============================================================================

destructor TEkImgLED.Destroy;
begin

FPicSeq.Free;
FPicBuf.Free;
FPicLED[0].Free;
FPicLED[1].Free;
FPicLED[2].Free;
FPicLED[3].Free;
FPicLED[4].Free;
FPicLED[5].Free;
FPicLED[6].Free;
FPicLED[7].Free;
FPicLED[8].Free;
FPicLED[9].Free;
FPicLED[10].Free;

inherited Destroy;
end;

//==============================================================================

procedure TEkImgLED.Loaded();
begin
inherited;

SetPic(FPicSeq);

end;

//==============================================================================

procedure TEkImgLED.PaintIt();
begin

if not (csDesigning in ComponentState) and FPicSeq.Empty then
  Exit;

if (csDesigning in ComponentState) then // Design time paint
begin
  if FPicSeq.Empty then
  begin
    Canvas.Rectangle(0, 0, Width, Height);
    Exit;
  end;
end;

Canvas.Draw(0, 0, FPicBuf);

end;

//==============================================================================

procedure TEkImgLED.Paint;
begin

if not Visible then Exit;
PaintIt();

end;

//==============================================================================

procedure TEkImgLED.SetDimensions();
begin

if FLedType = eltSingle then
  Width := FDigitWidth
else if FLedType = eltDual then
  Width := FDigitWidth * 2
else if FLedType = eltTriple then
  Width := FDigitWidth * 3
else if FLedType = eltQuad then
  Width := FDigitWidth * 4;

Height := FPicLED[0].Height;

end;

//==============================================================================

procedure TEkImgLED.SingleLEDPaint(const x : Integer; src : TCanvas);
begin

BitBlt(FPicBuf.Canvas.Handle, x, 0,
FDigitWidth, Height,
src.Handle, 0, 0, SRCCOPY);

end;

//==============================================================================

procedure TEkImgLED.ResetLEDPaint();
begin

if FLedType = eltSingle then
  SingleLEDPaint(0, FPicLED[0].Canvas)
else if FLedType = eltDual then
begin
  SingleLEDPaint(0, FPicLED[0].Canvas);
  SingleLEDPaint(FDigitWidth, FPicLED[0].Canvas);
end
else if FLedType = eltTriple then
begin
  SingleLEDPaint(0, FPicLED[0].Canvas);
  SingleLEDPaint(FDigitWidth, FPicLED[0].Canvas);
  SingleLEDPaint(FDigitWidth * 2, FPicLED[0].Canvas);
end
else if FLedType = eltQuad then
begin
  SingleLEDPaint(0, FPicLED[0].Canvas);
  SingleLEDPaint(FDigitWidth, FPicLED[0].Canvas);
  SingleLEDPaint(FDigitWidth * 2, FPicLED[0].Canvas);
  SingleLEDPaint(FDigitWidth * 3, FPicLED[0].Canvas);
end;

end;

//==============================================================================

procedure TEkImgLED.LEDPaint();
var
  num1, num2, num3, num4 : Integer;
begin

num1 := 0;
num2 := 0;
num3 := 0;
num4 := 0;

if value >= 1000 then
  num1 := (value mod 10000) div 1000;

if value >= 100 then
  num2 := (value mod 1000) div 100;

if value >= 10 then
  num3 := (value mod 100) div 10;

if value >= 1 then
  num4 := value mod 10;

if FLedType = eltSingle then
  SingleLEDPaint(0, FPicLED[num4 +1].Canvas)
else if FLedType = eltDual then
begin
  SingleLEDPaint(0, FPicLED[num3 +1].Canvas);
  SingleLEDPaint(FDigitWidth, FPicLED[num4 +1].Canvas);
end
else if FLedType = eltTriple then
begin
  SingleLEDPaint(0, FPicLED[num2 +1].Canvas);
  SingleLEDPaint(FDigitWidth, FPicLED[num3 +1].Canvas);
  SingleLEDPaint(FDigitWidth * 2, FPicLED[num4 +1].Canvas);
end
else if FLedType = eltQuad then
begin
  SingleLEDPaint(0, FPicLED[num1 +1].Canvas);
  SingleLEDPaint(FDigitWidth, FPicLED[num2 +1].Canvas);
  SingleLEDPaint(FDigitWidth * 2, FPicLED[num3 +1].Canvas);
  SingleLEDPaint(FDigitWidth * 3, FPicLED[num4 +1].Canvas);
end;

end;

//==============================================================================

procedure TEkImgLED.PicChanged(Sender: TObject);
begin

if Enabled then
  LEDPaint()
else
  ResetLEDPaint();

SetDimensions;

if not (csLoading In ComponentState) then
  Invalidate;

end;

//==============================================================================

procedure TEkImgLED.SetProperties(pic : TBitmap);
begin

pic.Transparent := False;
pic.HandleType := bmDIB;
pic.PixelFormat := pf24bit;

end;


//==============================================================================

procedure TEkImgLED.LoadBmpSkin(path : String);
var
  bmp : TBitmap;
begin

bmp := TBitmap.Create;
bmp.LoadFromFile(path);

SetPic(bmp);
bmp.Free;

end;

//==============================================================================

function TEkImgLED.GetPic : TBitmap;
begin

if (FPicSeq.Empty) then
  FPicSeq := TBitmap.Create;

Result := FPicSeq;

end;

//==============================================================================

procedure TEkImgLED.SetPic(pic : TBitmap);
begin

FPicSeq.Assign(pic);

if (FPicSeq.Empty) then
begin
  Invalidate;
  Exit;
end;

SplitStdBmp(FPicSeq, FPicLED, 11, FTransparentColor);

FDigitWidth := FPicLED[0].Width;

SetDimensions();
FPicBuf.Width := Width;
FPicBuf.Height := Height;

SetProperties(FPicBuf);
SetProperties(FPicLED[0]);
SetProperties(FPicLED[1]);
SetProperties(FPicLED[2]);
SetProperties(FPicLED[3]);
SetProperties(FPicLED[4]);
SetProperties(FPicLED[5]);
SetProperties(FPicLED[6]);
SetProperties(FPicLED[7]);
SetProperties(FPicLED[8]);
SetProperties(FPicLED[9]);
SetProperties(FPicLED[10]);

PicChanged(self);
Invalidate;

end;

//==============================================================================

function TEkImgLED.GetValue : Integer;
begin

if FLedType = eltSingle then
  FMaxValue := 9
else if FLedType = eltDual then
  FMaxValue := 99
else if FLedType = eltTriple then
  FMaxValue := 999
else if FLedType = eltQuad then
  FMaxValue := 9999;

if (FValue > FMaxValue) or (FValue < 0) then
begin
  if FLedValClip = elcZero then
    FValue := 0
  else if FLedValClip = elcMax then
    FValue := FMaxValue
end;

Result := FValue;

end;

//==============================================================================

procedure TEkImgLED.SetValue(value : Integer);
begin

if value <> FValue then
begin
  FValue := value;
  PicChanged(self);
  Invalidate;
end;

end;

//==============================================================================

procedure TEkImgLED.SetLEDType(value : TEkLEDTypes);
begin

if value <> FLedType then
begin
  FLedType := value;
  SetDimensions();
  PicChanged(self);
  Invalidate;
end;

end;

//==============================================================================

procedure TEkImgLED.SetLEDValClip(value : TEkLEDValClip);
begin

if value <> FLedValClip then
begin
  FLedValClip := value;
  Invalidate;
end;

end;

//==============================================================================

procedure TEkImgLED.CMMouseEnter(var Msg : TMessage);
begin
  inherited;

if (csDesigning in ComponentState) then Exit;

if (Msg.LParam = 0) and Assigned(FOnMouseEnter) then
  FOnMouseEnter(self);

end;

//==============================================================================

procedure TEkImgLED.CMMouseLeave(var Msg : TMessage);
begin

if (csDesigning in ComponentState) then Exit;

if (Msg.LParam = 0) and Assigned(FOnMouseLeave) then
  FOnMouseLeave(self);

inherited;
end;

//==============================================================================

procedure TEkImgLED.CMEnabledChanged(var pMsg: TMessage);
begin
  inherited;

PicChanged(self);
Invalidate;

end;

//==============================================================================

//procedure TEkImgLED.WMEraseBkgnd(var pMsg: TWMEraseBkgnd);
//begin
//  pMsg.Result := 1;
//end;

//==============================================================================

procedure Register;
begin
	RegisterComponents('EkszBoxVCL', [TEkImgLED]);
end;

end.
