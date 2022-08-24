{------------------------------------------------------------------------------
  DecoProgressBar.pas

  DecoCharts for VCL

  written by  Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  Purpose:    Standalone progress bar component, that mimics the visual style
              of progress bars used in other complex DecoCharts controls.

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
  - added: PrintTo method
  - improved: Minor improvements and fixes

  Version 1.2 (2012-03-11)
  - The first release
}

{ This unit contains TCustomDecoProgressBar and TDecoProgressBar components declaration and implementation. }
unit DecoProgressBar;

interface

uses
  Classes, Windows, Messages, Graphics, Controls, SysUtils, DecoCommon;

type
  { A base class for all DecoProgressBar visual component implementations. See also TDecoProgressBarPlus. }
  TCustomDecoProgressBar = class(TCustomControl)
  private
    FMinValue: Extended;
    FMaxValue: Extended;
    FPosition: Extended;
    FGlowSize: Integer;
    FParentBackgroundSet: Boolean;
    FOnAfterPaint: TDecoPaintEvent;
    FOnBeforePaint: TDecoPaintEvent;
    FShowFocusRect: Boolean;

    FBackColor: TColor;
    FFrameColor: TColor;
    FFrameProgress: Boolean;
    FFrameWidth: Integer;
    FGradient: TPGGradientStyle;
    FLevel1Color: TColor;
    FLevel1TextColor: TColor;
    FLevel2Color: TColor;
    FLevel2Percent: Integer;
    FLevel2TextColor: TColor;
    FLevel3Color: TColor;
    FLevel3Percent: Integer;
    FLevel3TextColor: TColor;
    FRoundCorners: Integer;
    FRounded: Boolean;
    FBarSize: Integer;
    FBarMargin: Integer;
    FShowValueInBar: TShowValueInBar;
    FShowOverMaxIndicator: Boolean;

    FAlignment: TAlignment;

    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMWantSpecialKey(var Message: TWMKey); message CM_WANTSPECIALKEY;
    procedure SetOnAfterPaint(Value: TDecoPaintEvent);
    procedure SetOnBeforePaint(Value: TDecoPaintEvent);
    procedure SetMinValue(Value: Extended); virtual;
    procedure SetMaxValue(Value: Extended); virtual;
    procedure SetPosition(Value: Extended); virtual;
    procedure SetColorVal(Index: Integer; Value: TColor);
    procedure SetIntVal(Index: Integer; Value: Integer);
    procedure SetBoolVal(Index: Integer; Value: Boolean);
    procedure SetGradient(Value: TPGGradientStyle);
    procedure SetShowValueInBar(Value: TShowValueInBar);
    procedure SetAlignment(Value: TAlignment);
  protected
    // Temporary variable, that indicates a drawing onto the vector based canvas (ie. TMetafileCanvas). It is used in PrintTo method.
    _EMFExporting:Boolean;
    // A temporary variable that indicates if the component has a focus
    FActive:Boolean;
    // Reference to the helper function for drawing the texts
    FDrawTextFunc:TDrawTextFunc;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetParentBackground(Value: Boolean); override;
    function GetFocusRect:TRect; virtual;
    procedure DrawBar(TargetCanvas:TCanvas; Dest:TRect); virtual;
    procedure DrawFocus(TargetCanvas:TCanvas; aRect:TRect); virtual;
    { Set this property to True to draw the component's background as transparent.
      It is not published in components that use a standard GDI drawing methods, but in the descendants (such as TDecoProgressBarPlus) it is. }
    property ParentBackground stored FParentBackgroundSet default False;
    // Base method for drawing the component, that calls other methods to draw the bars, texts, etc.
    procedure Paint; override;
    { A glow size attribut for drawing texts (title, captions, values, etc.).
      It is not published in components that use a standard GDI drawing methods, but in the descendants (such as TDecoProgressBarPlus) it is. }
    property GlowSize: Integer Index 5 read FGlowSize write SetIntVal default 0;
    { An event handler that allows you to perform a custom drawing on the component surface after the default drawing is finished. }
    property OnAfterPaint: TDecoPaintEvent read FOnAfterPaint write SetOnAfterPaint;
    { An event handler that allows you to perform a custom drawing on the component surface before the default drawing will start. }
    property OnBeforePaint: TDecoPaintEvent read FOnBeforePaint write SetOnBeforePaint;
    // If True, a focus rectangle will be drawn.
    property ShowFocusRect: Boolean Index 2 read FShowFocusRect write SetBoolVal default True;
    { A minimum value of the range for the progress bar. }
    property MinValue: Extended read FMinValue write SetMinValue;
    { A maximum value of the range for the progress bar. }
    property MaxValue: Extended read FMaxValue write SetMaxValue;

    // By this property you can set the gradient style of progress bar.
    property Gradient: TPGGradientStyle read FGradient write SetGradient default pggGlass;
    { This property defines a kind of text displayed inside progress bar.
      It can be a current value, percentage or no text. If Caption property is not empty, it will be always displayed inside the progress bar}
    property ShowValueInBar: TShowValueInBar read FShowValueInBar write SetShowValueInBar default ibvPercent;
    // Background color of progress bar. You can set the value to clNone, to make the progress bars transparent.
    property BackColor: TColor Index 3 read FBackColor write SetColorVal default clBtnHighlight;
    // Frame color of progress bar. You can set the value to clNone, to remove the frame. See also FrameProgress, FrameWidth.
    property FrameColor: TColor Index 4 read FFrameColor write SetColorVal default clBtnFace;
    // Width of progress bar frame. See also FrameColor, FrameProgress.
    property FrameWidth: Integer Index 4 read FFrameWidth write SetIntVal default 1;
    // If True, then progress bar with percentage greater than 0, will be painted with a frame of a slightly darker color. See also FrameColor, FrameWidth.
    property FrameProgress: Boolean Index 3 read FFrameProgress write SetBoolVal default False;
    // Color of the progress bar, when the Value is lower than the Level2Percent limit.
    property Level1Color: TColor Index 5 read FLevel1Color write SetColorVal default $3437D9;
    // Color of the text inside the progress bar, when the Value is lower than the Level2Percent limit. See also ShowValueInBar property.
    property Level1TextColor: TColor Index 6 read FLevel1TextColor write SetColorVal default clBlack;
    // Color of the progress bar, when the Value is in the range from Level2Percent to Level3Percent.
    property Level2Color: TColor Index 7 read FLevel2Color write SetColorVal default $00BFFF;
    // A limit (in percent) for displaying the progress bar in colors defined by Level1Color and Level1TextColor properties.
    property Level2Percent: Integer Index 7 read FLevel2Percent write SetIntVal default 30;
    // Color of the text inside the progress bar, when the Value is in the range from Level2Percent to Level3Percent. See also ShowValueInBar property.
    property Level2TextColor: TColor Index 8 read FLevel2TextColor write SetColorVal default clBlack;
    // Color of the progress bar, when the Value is greater than Level3Percent limit.
    property Level3Color: TColor Index 9 read FLevel3Color write SetColorVal default $24CA8B;
    // A limit (in percent) for displaying the progress bar in colors defined by Level2Color and Level2TextColor properties.
    property Level3Percent: Integer Index 8 read FLevel3Percent write SetIntVal default 70;
    // Color of the text inside the progress bar, when the Value is greater than Level3Percent limit. See also ShowValueInBar property.
    property Level3TextColor: TColor Index 10 read FLevel3TextColor write SetColorVal default clBlack;
    { Setup this property to non-zero, if you want to use your own rounded corners factor.
      If you set the Rounded property to True and the RoundCorners property is 0, an internal rounded corners factor will be used, based on the RowHeight property.
      See also Rounded property. }
    property RoundCorners: Integer Index 9 read FRoundCorners write SetIntVal default 0;
    // If False, the bars will be displayed as standard rectangles. If True, the bars will be displayed with rounded corners. See also RoundCorners property.
    property Rounded: Boolean Index 4 read FRounded write SetBoolVal default True;
    { A height of the progress bar. Progress bar will be centered inside the control's client area. }
    property BarSize: Integer Index 10 read FBarSize write SetIntVal default 16;
    { A margin between the control's client area and the progress bar. }
    property BarMargin: Integer Index 6 read FBarMargin write SetIntVal default 4;
    { If True, a thin dividing line will be displayed in all progress bars (criteria values with cgsProgressBar style), that have the Value greater than MaxValue, or lower than MinValue.
      This line indicates the percentage of exceeding the maximum (or minimum) value. }
    property ShowOverMaxIndicator: Boolean Index 5 read FShowOverMaxIndicator write SetBoolVal default False;
    { Alignment of progress bar status text. }
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;

  public
    // Constructor of the component. You can call it directly, when creating the component in run-time.
    constructor Create(AOwner: TComponent); override;
    // Component destructor. You don't have to call it directly, unless you want to destroy the component at run-time.
    destructor Destroy; override;
    // Temporary property, that indicates if the component has a focus
    property Active: Boolean read FActive;
    { Allows you to paint the component to the passed TargetCanvas. Dest is a destination rectangle, where the component will be drawn.
      Set AsEMF paramenter to True, when the TargetCanvas belongs to a vector based device (ie TMetafileCanvas).
      Refer to the main DecoBar demo project, to see how to use this method to save/export the grid into a graphic formats. }
    procedure PrintTo(TargetCanvas:TCanvas; Dest:TRect; AsEMF:Boolean=False); virtual;
  published
    // Background color of the control. See also ParentBackground.
    property Color default clBtnFace;
    // Takes the same background color as its parent control (when not using the ParentBackground mode)
    property ParentColor default False;
    { Current progress bar position style. }
    property Position: Extended read FPosition write SetPosition;
  end;


  { TDecoProgressBar is a standalone progress bar component, that mimics the visual style of progress bars
    used in other complex DecoCharts controls. }
  TDecoProgressBar = class(TCustomDecoProgressBar)
  published
    // Setups the align in the parent control
    property Align;
    { Alignment of progress bar status text. }
    property Alignment;
    // Component anchors
    property Anchors;
    property BevelEdges;
    property BevelKind;
    // Bidirectional text drawing mode
    property BiDiMode;
    { Status label for the progress bar. See also ShowCaption and CaptionFont. }
    property Caption;
    // Background color of the control. See also ParentBackground.
    property Color;
    { Component size constraints }
    property Constraints;
    property Ctl3D;
    { Controls double-buffering of the component. TDecoProgressBar component is always painted as double-buffered, but this property is important
      for other descendants (such as TDecoProgressBarPlus), that use a composited rendering. }
    property DoubleBuffered;
    // Enabling or disabling the component
    property Enabled;
    // Font for texts (values, captions, etc.). See also CaptionFont.
    property Font;
    { A glow size attribut for drawing texts (title, captions, values, etc.).
      It is not published in components that use a standard GDI drawing methods, but in the descendants (such as TDecoProgressBarPlus) it is. }
    property GlowSize;
    { A minimum value of the range for the progress bar. }
    property MinValue;
    { A maximum value of the range for the progress bar. }
    property MaxValue;
    { An event handler that allows you to perform a custom drawing on the component surface after the default drawing is finished. }
    property OnAfterPaint;
    { An event handler that allows you to perform a custom drawing on the component surface before the default drawing will start. }
    property OnBeforePaint;
    {$IF CompilerVersion >= 20.0}
    property OnAlignInsertBefore;
    property OnAlignPosition;
    {$IFEND}
    { OnCanResize event }
    property OnCanResize;
    { OnClick event }
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IF CompilerVersion >= 18.0}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    {$IFEND}
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property ParentBiDiMode;
    // Takes the same background color as its parent control (when not using the ParentBackground mode)
    property ParentColor;
    property ParentCtl3D;
    {$IF CompilerVersion >= 20.0}
    { ParentDoubleBuffered property. See also DoubleBuffered property. }
    property ParentDoubleBuffered;
    {$IFEND}
    // Takes the font of its parent control
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    // If True, a focus rectangle will be drawn.
    property ShowFocusRect;
    { If True, the hints will be shown when the mouse is above the control }
    property ShowHint;
    property TabOrder;
    property TabStop;
    { Controls visibility of the control }
    property Visible;

    // By this property you can set the gradient style of criteria values with cgsProgressBar style.
    property Gradient;
    { This property defines a kind of text displayed inside progress bars (criteria values with cgsProgressBar style).
      It can be a current value, percentage or no text. }
    property ShowValueInBar;
    // Background color of progress bars (criteria values with cgsProgressBar style). You can set the value to clNone, to make the progress bars transparent.
    property BackColor;
    // Frame color for progress bars (criteria values with cgsProgressBar style). You can set the value to clNone, to remove the frame. See also FrameProgress.
    property FrameColor;
    // If True, then progress bars (criteria values with cgsProgressBar style) with percentage greater than 0, will be painted with a frame of a slightly darker color. See also FrameColor.
    property FrameProgress;
    // Width of progress bar frame. See also FrameColor, FrameProgress.
    property FrameWidth;
    // Color of the progress bar, when the Value is lower than the Level2Percent limit.
    property Level1Color;
    // Color of the text inside the progress bar, when the Value is lower than the Level2Percent limit. See also ShowValueInBar property.
    property Level1TextColor;
    // Color of the progress bar, when the Value is in the range from Level2Percent to Level3Percent.
    property Level2Color;
    // A limit (in percent) for displaying the progress bar in colors defined by Level1Color and Level1TextColor properties.
    property Level2Percent;
    // Color of the text inside the progress bar, when the Value is in the range from Level2Percent to Level3Percent. See also ShowValueInBar property.
    property Level2TextColor;
    // Color of the progress bar, when the Value is greater than Level3Percent limit.
    property Level3Color;
    // A limit (in percent) for displaying the progress bar in colors defined by Level2Color and Level2TextColor properties.
    property Level3Percent;
    // Color of the text inside the progress bar, when the Value is greater than Level3Percent limit. See also ShowValueInBar property.
    property Level3TextColor;
    { Setup this property to non-zero, if you want to use your own rounded corners factor.
      If you set the Rounded property to True and the RoundCorners property is 0, an internal rounded corners factor will be used, based on the RowHeight property.
      See also Rounded property. }
    property RoundCorners;
    // If False, the bars will be displayed as standard rectangles. If True, the bars will be displayed with rounded corners. See also RoundCorners property.
    property Rounded;
    { A height of the progress bar. Progress bar will be centered inside the control's client area. }
    property BarSize;
    { A margin between the control's client area and the progress bar. }
    property BarMargin;
    { If True, a thin dividing line will be displayed in all progress bars (criteria values with cgsProgressBar style), that have the Value greater than MaxValue, or lower than MinValue.
      This line indicates the percentage of exceeding the maximum (or minimum) value. }
    property ShowOverMaxIndicator;
  end;

implementation

uses
  DecoGDI;

//////////////////////////////////////////// TCustomDecoProgressBar ////////////////////////////////////////////
constructor TCustomDecoProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csOpaque, csDoubleClicks, csReplicatable,
    csSetCaption {$IF CompilerVersion >= 20.0}, csPannable {$IFEND} ];
  FDrawTextFunc := DoDrawNormalText;
  FActive:=False;
  FGlowSize := 0;
  FShowFocusRect := True;
  FMinValue := 0;
  FMaxValue := 100;
  FPosition := 0;
  FAlignment := taCenter;

  FBarMargin := 4;
  FBackColor:=clBtnHighlight;
  FFrameColor:=clBtnFace;
  FFrameProgress:=False;
  FFrameWidth:=1;
  FGradient:=pggGlass;
  FLevel1Color := $24CA8B;
  FLevel2Color := $00BFFF;
  FLevel3Color := $3437D9;
  FLevel1TextColor := clBlack;
  FLevel2TextColor := clBlack;
  FLevel3TextColor := clWhite;
  FLevel2Percent := 101;
  FLevel3Percent := 128;
  FRoundCorners:=0;
  FRounded:=True;
  FBarSize:=16;
  FShowValueInBar := ibvPercent;
  FShowOverMaxIndicator := False;
  _EMFExporting := False;

  Width := 200;
  Height := 24;
  Color := clBtnFace;
  ParentBackground := False;
end;

destructor TCustomDecoProgressBar.Destroy;
begin
  inherited;
end;

procedure TCustomDecoProgressBar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    WindowClass.style := WindowClass.style  and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TCustomDecoProgressBar.Loaded;
begin
  inherited;
end;

procedure TCustomDecoProgressBar.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  Invalidate;
  inherited;
end;

procedure TCustomDecoProgressBar.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TCustomDecoProgressBar.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  FActive:=False;
  Invalidate;
end;

procedure TCustomDecoProgressBar.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  FActive:=True;
  Invalidate;
end;

procedure TCustomDecoProgressBar.CMWantSpecialKey(var Message: TWMKey);
begin
  inherited;
  if (Message.CharCode in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_ESCAPE, VK_HOME, VK_END, VK_NEXT, VK_PRIOR]) then
    Message.Result := 1
  else
  if (Message.CharCode = VK_TAB) and FActive then
    Invalidate;
end;

procedure TCustomDecoProgressBar.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomDecoProgressBar.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomDecoProgressBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (not FActive) and HandleAllocated and Visible and (not (ssDouble in Shift)) then
    SetFocus;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomDecoProgressBar.SetParentBackground(Value: Boolean);
begin
  if Value then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
  FParentBackgroundSet := True;
  inherited;
end;

procedure TCustomDecoProgressBar.SetPosition(Value: Extended);
begin
  if Value<>FPosition then
  begin
    FPosition:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressBar.SetMaxValue(Value: Extended);
begin
  if Value<>FMaxValue then
  begin
    FMaxValue:=Value;
    if FMaxValue<=FMinValue then
      FMinValue:=FMaxValue-1;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressBar.SetMinValue(Value: Extended);
begin
  if Value<>FMinValue then
  begin
    FMinValue:=Value;
    if FMinValue>=FMaxValue then
      FMaxValue:=FMinValue+1;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressBar.SetOnBeforePaint(Value: TDecoPaintEvent);
begin
  FOnBeforePaint:=Value;
  Invalidate;
end;

procedure TCustomDecoProgressBar.SetOnAfterPaint(Value: TDecoPaintEvent);
begin
  FOnAfterPaint:=Value;
  Invalidate;
end;

procedure TCustomDecoProgressBar.SetColorVal(Index: Integer; Value: TColor);
begin
  case Index of
    3:if Value<>FBackColor then begin FBackColor := Value; Invalidate; end;
    4:if Value<>FFrameColor then begin FFrameColor := Value; Invalidate; end;
    5:if Value<>FLevel1Color then begin FLevel1Color := Value; Invalidate; end;
    6:if Value<>FLevel1TextColor then begin FLevel1TextColor := Value; Invalidate; end;
    7:if Value<>FLevel2Color then begin FLevel2Color := Value; Invalidate; end;
    8:if Value<>FLevel2TextColor then begin FLevel2TextColor := Value; Invalidate; end;
    9:if Value<>FLevel3Color then begin FLevel3Color := Value; Invalidate; end;
   10:if Value<>FLevel3TextColor then begin FLevel3TextColor := Value; Invalidate; end;
  end;
end;

procedure TCustomDecoProgressBar.SetIntVal(Index: Integer; Value: Integer);
begin
  case Index of
    4:if Value<>FFrameWidth then begin FFrameWidth := Value; Invalidate; end;
    5:if Value<>FGlowSize then begin FGlowSize := Value; Invalidate; end;
    6:if Value<>FBarMargin then begin FBarMargin := Value; Invalidate; end;
    7:if Value<>FLevel2Percent then begin FLevel2Percent := Value; Invalidate; end;
    8:if Value<>FLevel3Percent then begin FLevel3Percent := Value; Invalidate; end;
    9:if Value<>FRoundCorners then begin FRoundCorners := Value; Invalidate; end;
   10:if Value<>FBarSize then begin FBarSize := Value; Invalidate; end;
  end;
end;

procedure TCustomDecoProgressBar.SetBoolVal(Index: Integer; Value: Boolean);
begin
  case Index of
    2:if Value<>FShowFocusRect then begin FShowFocusRect:=Value; Invalidate; end;
    3:if Value<>FFrameProgress then begin FFrameProgress := Value; Invalidate; end;
    4:if Value<>FRounded then begin FRounded := Value; Invalidate; end;
    5:if Value<>FShowOverMaxIndicator then begin FShowOverMaxIndicator := Value; Invalidate; end;
  end;
end;

procedure TCustomDecoProgressBar.SetGradient(Value: TPGGradientStyle);
begin
  if Value<>FGradient then
  begin
    FGradient:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressBar.SetShowValueInBar(Value: TShowValueInBar);
begin
  if Value<>FShowValueInBar then
  begin
    FShowValueInBar:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressBar.SetAlignment(Value: TAlignment);
begin
  if Value<>FAlignment then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

function TCustomDecoProgressBar.GetFocusRect:TRect;
begin
  Result := ClientRect;
end;

procedure TCustomDecoProgressBar.DrawBar(TargetCanvas:TCanvas; Dest:TRect);
var
  RR:TRect;
  percent:Extended;
  invClr:TColor;
  tmpStr:string;
  Flags: Longint;
begin
  RR:=Dest;
  if (FBarSize>0) and (FBarSize<=RR.Bottom-RR.Top) then
  begin
    RR.Top:=RR.Top+(RR.Bottom-RR.Top-FBarSize) div 2;
    RR.Bottom:=RR.Top+FBarSize;
    inflateRect(RR,-FBarMargin,0);
  end
  else
  begin
    InflateRect(RR,-FBarMargin,-FBarMargin);
    Dec(RR.Bottom);
  end;

  DrawProgressBar(TargetCanvas, RR, FPosition, FMinValue, FMaxValue, FGradient, Color, FBackColor, FFrameColor,
    FFrameWidth, FRounded, FRoundCorners, FFrameProgress, FShowOverMaxIndicator, FLevel1Color, FLevel1TextColor,
    FLevel2Color, FLevel2Percent, FLevel2TextColor, FLevel3Color, FLevel3Percent, FLevel3TextColor,
    ibvNone, Enabled, _EMFExporting);

  if (FShowValueInBar<>ibvNone) or (Length(Caption)>0) then
  begin
    percent:=(FPosition-MinValue)/(MaxValue-MinValue);
    // setup color by level
    if percent*100>=FLevel3Percent then
      invClr:=FLevel3TextColor
    else
    if percent*100>=FLevel2Percent then
      invClr:=FLevel2TextColor
    else
      invClr:=FLevel1TextColor;
    if not Enabled then
      invClr:=GrayScaleColor(invClr);

    TargetCanvas.Brush.Style:=bsClear;
    TargetCanvas.Font.Color:=invClr;
    Dec(RR.Bottom);
    if FAlignment<>taCenter then
    begin
      if FBarSize<=FBarMargin then inflateRect(RR,-FBarSize,0)
      else if FBarMargin<=16 then inflateRect(RR,-FBarMargin,0)
      else inflateRect(RR,-3,0);
    end;
    Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DrawTextAlignments[FAlignment] or DT_END_ELLIPSIS or DT_VCENTER);

    case FShowValueInBar of
      ibvValue:tmpStr:=FloatToStr(FPosition);
      ibvPercent:tmpStr:=IntToStr(Round(percent*100))+'%';
    else
      tmpStr:='';
    end;
    if Length(Caption)>0 then
    begin
      if UseRightToLeftAlignment then
        tmpStr:=tmpStr+Caption
      else
        tmpStr:=Caption+tmpStr
    end;
    if Length(tmpStr)>0 then
      FDrawTextFunc(TargetCanvas.Handle, tmpStr, RR, Flags, TargetCanvas.Font.Color);
  end;

end;

procedure TCustomDecoProgressBar.DrawFocus(TargetCanvas:TCanvas; aRect:TRect);
begin
  TargetCanvas.Font.Color:=Font.Color;
  windows.DrawFocusRect(TargetCanvas.Handle,aRect);
end;

procedure TCustomDecoProgressBar.Paint;
var
  aRect,cRect: TRect;
  Buffer : TBitmap;
  tmpCanvas : TCanvas;
begin
  aRect := ClientRect;
  cRect := Canvas.ClipRect;
  Buffer := TBitmap.Create;
  try
    Buffer.Width := aRect.Right-aRect.Left;
    Buffer.Height := aRect.Bottom-aRect.Top;
    tmpCanvas := Buffer.Canvas;
    IntersectClipRect(tmpCanvas.Handle,cRect.Left,cRect.Top,cRect.Right,cRect.Bottom);

    tmpCanvas.Brush.Style := bsSolid;
    tmpCanvas.Brush.Color := Color;
    tmpCanvas.FillRect(cRect);

    tmpCanvas.Font := Font;
    if not Enabled then
      tmpCanvas.Font.Color:=clGrayText;

    if Assigned(OnBeforePaint) then
      OnBeforePaint(Self,tmpCanvas);

    DrawBar(tmpCanvas, aRect);

    IntersectClipRect(tmpCanvas.Handle,cRect.Left,cRect.Top,cRect.Right,cRect.Bottom);
    if Assigned(OnAfterPaint) then
      OnAfterPaint(Self,tmpCanvas);

    Canvas.CopyRect(cRect, tmpCanvas, cRect);

  finally
    Buffer.Free;
  end;

  if FActive and FShowFocusRect then
    DrawFocus(Canvas, GetFocusRect);
end;

procedure TCustomDecoProgressBar.PrintTo(TargetCanvas:TCanvas; Dest:TRect; AsEMF:Boolean=False);
begin
  _EMFExporting:=AsEMF;
  try
    TargetCanvas.Brush.Style := bsSolid;
    TargetCanvas.Brush.Color := Color;
    TargetCanvas.FillRect(Dest);

    TargetCanvas.Font := Font;
    if not Enabled then
      TargetCanvas.Font.Color:=clGrayText;

    if Assigned(OnBeforePaint) then
      OnBeforePaint(Self,TargetCanvas);

    DrawBar(TargetCanvas, Dest);

    if Assigned(OnAfterPaint) then
      OnAfterPaint(Self,TargetCanvas);

  finally
    _EMFExporting := False;
  end;
end;

end.
