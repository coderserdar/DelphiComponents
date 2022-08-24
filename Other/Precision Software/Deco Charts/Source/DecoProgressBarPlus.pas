{------------------------------------------------------------------------------
  DecoProgressBarPlus.pas

  DecoCharts for VCL

  written by  Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  Purpose:    TDecoProgressBarPlus is a descendant of the standard
              TDecoProgressBar component, but it uses the GDI+ library
              for antialiased and composited drawing, that enhances the look
              and the feel of resulting progress bar.

  Notes:      Requires a dynamic loading implementation of GDI+ API library
              ( http://www.progdigy.com, http://themiron.mirandaim.ru )

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
  - added: AntiAliasing property
  - added: PrintTo method
  - added: Properties for setting the rendering mode (RenderCompositingMode, RenderSmoothingMode, RenderTextMode) have been declared as published
  - improved: Minor improvements and fixes

  Version 1.2 (2012-03-11)
  - The first release
}

{ This unit contains TDecoProgressBarPlus component declaration and implementation. }
unit DecoProgressBarPlus;

interface

uses
  Classes, Windows, Messages, Graphics, Controls, SysUtils,
  {$IF CompilerVersion >= 23}
  Winapi.GDIPAPI, Winapi.GDIPOBJ, Winapi.GDIPUTIL,
  {$ELSE}
  xGDIPAPI, xGDIPOBJ, xGDIPUTIL,
  {$IFEND}
  DecoCommon, DecoGDIP, DecoProgressBar;

type
  { A descendant of the standard TDecoProgressBar component, but it uses the GDI+ library
    for antialiased and composited drawing, that enhances the look and the feel of resulting progress bar. }
  TDecoProgressBarPlus = class(TCustomDecoProgressBar)
  private
    FRenderCompositingMode: TRenderCompositingMode;
    FRenderSmoothingMode: TRenderSmoothingMode;
    FRenderTextMode: TRenderTextMode;
    FAntiAliasing: boolean;
    {$IF CompilerVersion >= 19.0}
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    {$IFEND}
  protected
    // Main drawing method - paints the progress bar
    procedure DrawBar(TargetCanvas:TCanvas; Dest:TRect); override;
    // Base method for drawing the component, but overrided to draw using the GDI+ library
    procedure Paint; override;
    procedure SetRenderCompositingMode(Value: TRenderCompositingMode); virtual;
    procedure SetRenderSmoothingMode(Value: TRenderSmoothingMode); virtual;
    procedure SetRenderTextMode(Value: TRenderTextMode); virtual;
    procedure SetAntiAliasing(Value: boolean); virtual;
  public
    { GDIPCanvas variable refers to the GDI+ drawing object (a canvas), that is used to render the component using GDI+ library.
      It is valid only during the painting process. You can refer to it inside your custom drawing handlers, such as OnBeforePaint, OnAfterPaint, OnDrawCell, etc. }
    GDIPCanvas: TGPGraphics;
    // Component constructor
    constructor Create(AOwner: TComponent); override;
    { Allows you to paint the component to the passed TargetCanvas. Dest is a destionation rectangle, where the component will be drawn.
      Set AsEMF paramenter to True, when the TargetCanvas belongs to a vector based device (ie TMetafileCanvas). }
    procedure PrintTo(TargetCanvas:TCanvas; Dest:TRect; AsEMF:Boolean=False); override;
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
    { Controls double-buffering of the component. }
    property DoubleBuffered;
    // Enabling or disabling the component
    property Enabled;
    // Font for texts (values, captions, etc.). See also CaptionFont.
    property Font;
    { A glow size attribut for drawing texts (title, captions, values, etc.). }
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
    { See @inherited }
    property ParentBackground;
    { Composition mode for GDI+ drawing }
    property RenderCompositingMode: TRenderCompositingMode read FRenderCompositingMode write SetRenderCompositingMode default rcmDefault;
    { Smoothing mode for GDI+ drawing }
    property RenderSmoothingMode: TRenderSmoothingMode read FRenderSmoothingMode write SetRenderSmoothingMode default rsmAntiAlias;
    { Text rendering mode for GDI+ drawing }
    property RenderTextMode: TRenderTextMode read FRenderTextMode write SetRenderTextMode default rtmDefault;
    { If False, the GDI+ library is not used for drawing even if it is available }
    property AntiAliasing: boolean read FAntiAliasing write SetAntiAliasing default True;
  end;

implementation

uses
  Themes, UxTheme, {$IF CompilerVersion >= 19.0} DwmApi, {$IFEND} Forms
  {$IF CompilerVersion >= 24}, System.Types {$IFEND};

{ TDecoProgressBarPlus }

constructor TDecoProgressBarPlus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  GDIPCanvas := nil;
  FRenderSmoothingMode:=rsmAntiAlias;
  FRenderTextMode:=rtmDefault;
  FRenderCompositingMode:=rcmDefault;
  FAntiAliasing := True;
  if {$IF CompilerVersion >= 23} StyleServices.Enabled {$ELSE} StyleServices.ThemesEnabled {$IFEND} then
  begin
    if ParentBackground then
      ControlStyle := ControlStyle + [csParentBackground] - [csOpaque];
    if (Win32MajorVersion >= 6) then
      FDrawTextFunc := DoDrawThemeTextEx
  end;
end;

procedure TDecoProgressBarPlus.SetRenderSmoothingMode(Value: TRenderSmoothingMode);
begin
  if Value<>FRenderSmoothingMode then
  begin
    FRenderSmoothingMode:=Value;
    Invalidate;
  end;
end;

procedure TDecoProgressBarPlus.SetRenderTextMode(Value: TRenderTextMode);
begin
  if Value<>FRenderTextMode then
  begin
    FRenderTextMode:=Value;
    Invalidate;
  end;
end;

procedure TDecoProgressBarPlus.SetRenderCompositingMode(Value: TRenderCompositingMode);
begin
  if Value<>FRenderCompositingMode then
  begin
    FRenderCompositingMode:=Value;
    Invalidate;
  end;
end;

procedure TDecoProgressBarPlus.SetAntiAliasing(Value: boolean);
begin
  if Value<>FAntiAliasing then
  begin
    FAntiAliasing:=Value;
    Invalidate;
  end;
end;

{$IF CompilerVersion >= 19.0}
procedure TDecoProgressBarPlus.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  PaintBuffer: HPAINTBUFFER;
begin
  if not FDoubleBuffered or (Message.DC <> 0) then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(Message);
  end
  else
  begin
    if DwmCompositionEnabled then
    begin
      DC := BeginPaint(Handle, PS);
      try
        PaintBuffer := BeginBufferedPaint(DC, PS.rcPaint, BPBF_COMPOSITED, nil, MemDC);
        if PaintBuffer <> 0 then
          try
            Perform(WM_ERASEBKGND, MemDC, MemDC);
            Perform(WM_PRINTCLIENT, MemDC, PRF_CLIENT);
            if not ParentBackground then
              BufferedPaintMakeOpaque(PaintBuffer, PS.rcPaint);
          finally
            EndBufferedPaint(PaintBuffer, True);
          end;
      finally
        EndPaint(Handle, PS);
      end;
    end
    else
    begin
      DC := BeginPaint(Handle, PS);
      MemBitmap := CreateCompatibleBitmap(DC, Width, Height);
      try
        MemDC := CreateCompatibleDC(DC);
        OldBitmap := SelectObject(MemDC, MemBitmap);
        try
          IntersectClipRect(MemDC, PS.rcPaint.Left, PS.rcPaint.Top, PS.rcPaint.Right, PS.rcPaint.Bottom);
          Perform(WM_ERASEBKGND, MemDC, MemDC);
          Message.DC := MemDC;
          WMPaint(Message);
          Message.DC := 0;
          BitBlt(DC, PS.rcPaint.Left, PS.rcPaint.Top,
            PS.rcPaint.Right - PS.rcPaint.Left,
            PS.rcPaint.Bottom - PS.rcPaint.Top,
            MemDC,
            PS.rcPaint.Left, PS.rcPaint.Top,
            SRCCOPY);
        finally
          SelectObject(MemDC, OldBitmap);
        end;
      finally
        EndPaint(Handle, PS);
        DeleteDC(MemDC);
        DeleteObject(MemBitmap);
      end;
    end;
  end;
end;
{$IFEND}

procedure TDecoProgressBarPlus.DrawBar(TargetCanvas:TCanvas; Dest:TRect);
var
  RR:TRect;
  percent:Extended;
  invClr:TColor;
  tmpStr:string;
  Flags: Longint;
begin
  if GDIPCanvas=nil then
  begin
    inherited;
    Exit;
  end;

  RR:=Dest;
  if (BarSize>0) and (BarSize<=RR.Bottom-RR.Top) then
  begin
    RR.Top:=RR.Top+(RR.Bottom-RR.Top-BarSize) div 2;
    RR.Bottom:=RR.Top+BarSize;
    inflateRect(RR,-BarMargin,0);
  end
  else
  begin
    InflateRect(RR,-BarMargin,-BarMargin);
    Dec(RR.Bottom);
  end;

  GDIP_DrawProgressBar(TargetCanvas, GDIPCanvas, RR, Position, MinValue, MaxValue, Gradient, Color, BackColor, FrameColor,
    FrameWidth, Rounded, RoundCorners, FrameProgress, ShowOverMaxIndicator, Level1Color, Level1TextColor,
    Level2Color, Level2Percent, Level2TextColor, Level3Color, Level3Percent, Level3TextColor,
    ibvNone, Enabled, False, nil, GlowSize);

  if (ShowValueInBar<>ibvNone) or (Length(Caption)>0) then
  begin
    percent:=(Position-MinValue)/(MaxValue-MinValue);
    // setup color by level
    if percent*100>=Level3Percent then
      invClr:=Level3TextColor
    else
    if percent*100>=Level2Percent then
      invClr:=Level2TextColor
    else
      invClr:=Level1TextColor;
    if not Enabled then
      invClr:=GrayScaleColor(invClr);

    TargetCanvas.Brush.Style:=bsClear;
    TargetCanvas.Font.Color:=invClr;
    Dec(RR.Bottom);
    if Alignment<>taCenter then
    begin
      if BarSize<=BarMargin then inflateRect(RR,-BarSize,0)
      else if BarMargin<=16 then inflateRect(RR,-BarMargin,0)
      else inflateRect(RR,-3,0);
    end;
    Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DrawTextAlignments[Alignment] or DT_END_ELLIPSIS or DT_VCENTER);

    case ShowValueInBar of
      ibvValue:tmpStr:=FloatToStr(Position);
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
      FDrawTextFunc(TargetCanvas.Handle, tmpStr, RR, Flags, TargetCanvas.Font.Color, GlowSize);
  end;
end;

procedure TDecoProgressBarPlus.Paint;
var
  aRect,cRect: TRect;
  gClipRect: TGPRect;
begin
  if FAntiAliasing {$IF CompilerVersion < 23} and UseGDIP {$IFEND} then
  begin
    aRect := ClientRect;
    cRect := Canvas.ClipRect;

    if ParentBackground then
    begin
      if {$IF CompilerVersion >= 23} StyleServices.Enabled {$ELSE} StyleServices.ThemesEnabled {$IFEND} then
        {$IF CompilerVersion >= 20.0}
        StyleServices.DrawParentBackground(Handle, Canvas.Handle, nil, False, cRect)
        {$ELSE}
        StyleServices.DrawParentBackground(Handle, Canvas.Handle, nil, False, @cRect)
        {$IFEND}
      else
      if Owner is TCustomForm then
      begin
        Canvas.Brush.Color := TCustomForm(Owner).Color;
        Canvas.FillRect(cRect);
      end;
    end
    else
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(cRect);
    end;

    GDIPCanvas := TGPGraphics.Create(Canvas.Handle);
    try
      gClipRect:=MakeRect(cRect);
      GDIPCanvas.SetClip(gClipRect);
      GDIP_SetRenderingOptions(GDIPCanvas, FRenderSmoothingMode, FRenderTextMode, FRenderCompositingMode);

      Canvas.Font := Font;
      if not Enabled then
        Canvas.Font.Color:=clGrayText;

      if Assigned(OnBeforePaint) then
        OnBeforePaint(Self,Canvas);

      DrawBar(Canvas, aRect);

      GDIPCanvas.SetClip(gClipRect);
      if Assigned(OnAfterPaint) then
        OnAfterPaint(Self,Canvas);

    finally
      GDIPCanvas.Free;
      GDIPCanvas:=nil;
    end;

    if FActive and ShowFocusRect then
      DrawFocus(Canvas, GetFocusRect);

  end
  else
    inherited;
end;

procedure TDecoProgressBarPlus.PrintTo(TargetCanvas:TCanvas; Dest:TRect; AsEMF:Boolean=False);
var
  gBrush:TGPSolidBrush;
begin
  if AsEMF or (not FAntiAliasing) {$IF CompilerVersion < 23} or (not UseGDIP) {$IFEND} then
    inherited PrintTo(TargetCanvas, Dest, AsEMF)
  else
  begin
    GDIPCanvas:=TGPGraphics.Create(TargetCanvas.Handle);
    try
      if not ParentBackground then
      begin
        gBrush:=TGPSolidBrush.Create(ColorToARGB(Color));
        GDIPCanvas.SetSmoothingMode(SmoothingModeNone);
        GDIPCanvas.FillRectangle(gBrush,MakeRect(Dest));
        gBrush.Free;
      end;
      GDIP_SetRenderingOptions(GDIPCanvas, FRenderSmoothingMode, FRenderTextMode, FRenderCompositingMode);

      TargetCanvas.Brush.Style := bsSolid;
      TargetCanvas.Font := Font;
      if not Enabled then
        TargetCanvas.Font.Color:=clGrayText;

      if Assigned(OnBeforePaint) then
        OnBeforePaint(Self,TargetCanvas);

      DrawBar(TargetCanvas, Dest);

      if Assigned(OnAfterPaint) then
        OnAfterPaint(Self,TargetCanvas);

    finally
      GDIPCanvas.Free;
      GDIPCanvas:=nil;
    end;
  end;
end;

end.
