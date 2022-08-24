{------------------------------------------------------------------------------
  DecoRatingStarsPlus.pas

  DecoCharts for VCL

  written by  Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  Purpose:    TDecoRatingStarsPlus is a descendant of the standard
              TCustomDecoRatingStars component, but it uses the GDI+ library
              for antialiased and composited drawing, that enhances the look
              and the feel of your rating control.

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

{ This unit contains TDecoRatingStarsPlus component declaration and implementation. }
unit DecoRatingStarsPlus;

interface

uses
  Classes, Windows, Messages, Graphics, Controls, SysUtils,
  {$IF CompilerVersion >= 23}
  Winapi.GDIPAPI, Winapi.GDIPOBJ, Winapi.GDIPUTIL,
  {$ELSE}
  xGDIPAPI, xGDIPOBJ, xGDIPUTIL,
  {$IFEND}
  DecoCommon, DecoRatingStars;

type
  { A descendant of the standard TCustomDecoRatingStars component, but it uses the GDI+ library
    for antialiased and composited drawing, that enhances the look and the feel of your rating control. }
  TDecoRatingStarsPlus = class(TCustomDecoRatingStars)
  private
    FRenderCompositingMode: TRenderCompositingMode;
    FRenderSmoothingMode: TRenderSmoothingMode;
    FRenderTextMode: TRenderTextMode;
    FAntiAliasing: boolean;
    {$IF CompilerVersion >= 19.0}
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    {$IFEND}
  protected
    // Main drawing method - paints the rating stars
    procedure DrawRating(TargetCanvas:TCanvas; Dest: TRect); override;
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
    { Currently hovered rating (if Interactive property is True). }
    property HoveredRating: Extended read FHoveredRating;
  published
    // Setups the align in the parent control
    property Align;
    // Component anchors
    property Anchors;
    property BevelEdges;
    property BevelKind;
    // Background color of the control. See also ParentBackground.
    property Color;
    { Component size constraints }
    property Constraints;
    { Controls double-buffering of the component. Component is always painted as double-buffered, but this property is important
      for other descendants (such as TDecoRatingStarsPlus), that use a composited rendering. }
    property DoubleBuffered;
    // Enabling or disabling the component
    property Enabled;
    // Font for texts (values, captions, etc.). See also CaptionFont.
    property Font;
    { A minimum rating value. }
    property MinValue;
    { A maximum rating value. }
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
    // Takes the same background color as its parent control (when not using the ParentBackground mode)
    property ParentColor;
    {$IF CompilerVersion >= 20.0}
    { ParentDoubleBuffered property. See also DoubleBuffered property. }
    property ParentDoubleBuffered;
    {$IFEND}
    property ParentShowHint;
    property PopupMenu;
    { ImageList that represents rating stars.
      ImageIndex 0 corresponds to "full" star, 1 to "half" star and 2 to "empty" star. You can ommit "half" star image,
      so the image list can contain only two images (for full and empty stars). }
    property Images;
    // If True, a focus rectangle will be drawn.
    property ShowFocusRect;
    { If True, the hints will be shown when the mouse is above the control }
    property ShowHint;
    property TabOrder;
    property TabStop;
    { Controls visibility of the control }
    property Visible;

    // Background color of empty star. You can set the value to clNone, to make the star transparent. Ignored when using Images property.
    property BackColor;
    // Frame color for empty star You can set the value to clNone, to remove the frame. Ignored when using Images property.
    property FrameColor;
    // Width of the rating star frame. See also FrameColor. Ignored when using Images property.
    property FrameWidth;
    // Color of the full rating star. Ignored when using Images property.
    property FillColor;
    // Color of the full rating star when the control is hovered. Ignored when using Images property.
    property HoverColor;
    { Number of stars in the presented rating control. }
    property StarCount;
    { A size of rating star. Rating stars will be centered inside the control's client area. Ignored when using Images property. }
    property StarSize;
    { A margin between the stars. }
    property StarMargin;
    // If True, the control will accept mouse clicks on stars to accumulate user ratings. See also Rating and RatingsCount
    property Interactive;
    { Number of performed ratings. Not actively used until Interactive property is True. }
    property RatingsCount;
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
  DecoGDIP, Themes, UxTheme, {$IF CompilerVersion >= 19.0} DwmApi, {$IFEND} Forms
  {$IF CompilerVersion >= 24}, System.Types {$IFEND};

{ TDecoRatingStarsPlus }

constructor TDecoRatingStarsPlus.Create(AOwner: TComponent);
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
  end;
end;

procedure TDecoRatingStarsPlus.SetRenderSmoothingMode(Value: TRenderSmoothingMode);
begin
  if Value<>FRenderSmoothingMode then
  begin
    FRenderSmoothingMode:=Value;
    Invalidate;
  end;
end;

procedure TDecoRatingStarsPlus.SetRenderTextMode(Value: TRenderTextMode);
begin
  if Value<>FRenderTextMode then
  begin
    FRenderTextMode:=Value;
    Invalidate;
  end;
end;

procedure TDecoRatingStarsPlus.SetRenderCompositingMode(Value: TRenderCompositingMode);
begin
  if Value<>FRenderCompositingMode then
  begin
    FRenderCompositingMode:=Value;
    Invalidate;
  end;
end;

procedure TDecoRatingStarsPlus.SetAntiAliasing(Value: boolean);
begin
  if Value<>FAntiAliasing then
  begin
    FAntiAliasing:=Value;
    Invalidate;
  end;
end;

{$IF CompilerVersion >= 19.0}
procedure TDecoRatingStarsPlus.WMPaint(var Message: TWMPaint);
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

type
  TStarPoints = array[0..9] of TGPPointF;

procedure _StarPoints(Rect:TRect; var p:TStarPoints);
var
  i: Integer;
  t, r, sv, sw, sx, sy: Single;
begin
  sx := (Rect.Right-Rect.Left)*0.48;
  sy := (Rect.Bottom-Rect.Top)*0.5;
  if sx > sy then
    sx := sy
  else
    sy := sx;
  sv := Rect.Left + (Rect.Right - Rect.Left) / 2;
  sw := Rect.Top + sy;
  for i := 0 To 9 do
  begin
    if ((i and 1) <> 0) then
      r := 1
    else
      r := 0.384;
    t := i * 2 * (PI/10);
    P[i].x := sv+sx*r*sin(t);
    P[i].Y := sw+sy*r*cos(t);
  end;
end;

procedure TDecoRatingStarsPlus.DrawRating(TargetCanvas:TCanvas; Dest: TRect);
var
  RR:TRect;
  drawImg:Boolean;
  imH,imW,rX,sCnt:Integer;
  z,zi:Integer;
  zs:Extended;
  p:TStarPoints;
  fClr:TColor;
  fHovering:Boolean;
  rcs:Integer;

  gBrush: TGPSolidBrush;
  gPen: TGPPen;
  RPath:TGPGraphicsPath;
  RS:TGPRect;
begin
  if GDIPCanvas=nil then
  begin
    inherited;
    Exit;
  end;

  // prepare GDI objects
  gBrush:=TGPSolidBrush.Create(ColorToARGB(BackColor));
  gPen := TGPPen.Create(ColorToARGB(FrameColor));
  try
    RR:=Dest;
    inflateRect(RR,-StarMargin,0);

    drawImg:=Assigned(Images) and (Images.Count>1);
    if drawImg then
    begin
      imW:=Images.Width;
      sCnt:=Images.Count;
    end
    else
    begin
      gPen.SetWidth(FrameWidth);
      imW:=StarSize;
      sCnt:=3;
    end;
    if imW>=RR.Bottom-RR.Top then
      imW:=RR.Bottom-RR.Top-1
    else
    if (StarShape=stsCircle) and (imW<4) then
      imW:=4;
    imH:=imW;
    rcs:=imW div 3;

    RR.Left := RR.Left + (RR.Right-RR.Left - StarCount*(imW+StarMargin)) div 2;
    RR.Right:=RR.Left+imW;
    RR.Top := RR.Top + (RR.Bottom-RR.Top - imH) div 2;
    RR.Bottom := RR.Top + imH;
    offsetRect(RR,StarMargin div 2,0);

    fHovering:=Interactive and (not (csDesigning in ComponentState)) and (FMouseX>=0);
    FHoveredRating:=MinValue;

    zi:=0;
    z:=1;
    if sCnt in [2,4] then
    begin
      zs:=Rating/((MaxValue-MinValue)/StarCount)+0.5;
      while z<=StarCount do
      begin
        if fHovering then
        begin
          if FMouseX>=RR.Left then
          begin
            FHoveredRating:=z+MinValue;
            if sCnt=4 then
              Images.Draw(TargetCanvas, RR.Left, RR.Top, 2)
            else
              Images.Draw(TargetCanvas, RR.Left, RR.Top, 0);
          end
          else
          if sCnt=4 then
            Images.Draw(TargetCanvas, RR.Left, RR.Top, 3)
          else
            Images.Draw(TargetCanvas, RR.Left, RR.Top, 1);
        end
        else
        begin
          if (zi=0) and (zs<z) then zi:=1;
          Images.Draw(TargetCanvas, RR.Left, RR.Top, zi);
        end;
        offsetRect(RR,imW,0);
        Inc(z);
      end;
    end
    else
    begin
      zs:=round((Rating/((MaxValue-MinValue)/StarCount)+0.2)*10)/10;
      while z<=StarCount do
      begin
        fClr:=FillColor;
        if fHovering then
        begin
          if FMouseX>=RR.Left then
          begin
            FHoveredRating:=z+MinValue;
            if HoverColor<>clNone then
              fClr:=HoverColor;
          end;
        end;

        if zi=0 then
        begin
          if zs<z then
          begin
            zi:=1;
            if zs<=z-0.6 then
              zi:=2;
          end;
        end;
        if drawImg then
        begin
          if fHovering then
          begin
            if FMouseX>=RR.Left then
            begin
              case sCnt of
                5:Images.Draw(TargetCanvas, RR.Left, RR.Top, 3);
              else
                Images.Draw(TargetCanvas, RR.Left, RR.Top, 0);
              end;
            end
            else
              case sCnt of
                5:Images.Draw(TargetCanvas, RR.Left, RR.Top, 4);
              else
                Images.Draw(TargetCanvas, RR.Left, RR.Top, 2);
              end;
          end
          else
            Images.Draw(TargetCanvas, RR.Left, RR.Top, zi)
        end
        else
        begin
          RS:=MakeRect(RR);
          if (zi=0) or (zi=sCnt-1) or fHovering then
          begin
            if fHovering then
            begin
              if FMouseX>=RR.Left then
              begin
                gPen.SetColor(ColorToARGB(fClr));
                gBrush.SetColor(ColorToARGB(BrightColor(fClr,10)));
              end
              else
              begin
                gPen.SetColor(ColorToARGB(FrameColor));
                gBrush.SetColor(ColorToARGB(BackColor));
              end;
            end
            else
            if (zi=0) then
            begin
              gPen.SetColor(ColorToARGB(fClr));
              gBrush.SetColor(ColorToARGB(BrightColor(fClr,10)));
            end
            else
            begin
              gPen.SetColor(ColorToARGB(FrameColor));
              gBrush.SetColor(ColorToARGB(BackColor));
            end;
            case StarShape of
              stsSquare:begin GDIPCanvas.FillRectangle(gBrush,RS); GDIPCanvas.DrawRectangle(gPen,RS); end;
              stsRoundSquare:
                begin
                  RPath := GDIP_RoundRectanglePath(RR, rcs);
                  GDIPCanvas.FillPath(gBrush,RPath);
                  GDIPCanvas.DrawPath(gPen,RPath);
                  RPath.Free;
                end;
              stsCircle:begin GDIPCanvas.FillEllipse(gBrush,RS); GDIPCanvas.DrawEllipse(gPen,RS); end;
            else
              begin
                _StarPoints(RR,p);
                GDIPCanvas.FillPolygon(gBrush,PGPPointF(@p[0]),10);
                GDIPCanvas.DrawPolygon(gPen,PGPPointF(@p[0]),10);
              end;
            end;
          end
          else
          begin
            rX:=RR.Left+imW div 2;
            gPen.SetColor(ColorToARGB(FrameColor));
            gBrush.SetColor(ColorToARGB(BackColor));
            case StarShape of
              stsSquare:
                begin
                  GDIPCanvas.FillRectangle(gBrush,rx,RR.Top,RR.Right-rX,imH);
                  GDIPCanvas.DrawRectangle(gPen,rx,RR.Top,RR.Right-rX,imH);
                  gPen.SetColor(ColorToARGB(fClr));
                  gBrush.SetColor(ColorToARGB(BrightColor(fClr,10)));
                  GDIPCanvas.FillRectangle(gBrush,RR.Left,RR.Top,imW div 2,imH);
                  GDIPCanvas.DrawRectangle(gPen,RR.Left,RR.Top,imW div 2,imH);
                end;
              stsRoundSquare:
                begin
                  gPen.SetColor(ColorToARGB(fClr));
                  gBrush.SetColor(ColorToARGB(BrightColor(fClr,10)));
                  RPath := GDIP_RoundRectanglePath(rect(RR.Left,RR.Top,RR.Right-1,RR.Bottom), rcs);
                  GDIPCanvas.FillPath(gBrush,RPath);
                  GDIPCanvas.DrawPath(gPen,RPath);
                  RPath.Free;
                  RPath := GDIP_RoundRectanglePath(RR, rcs);
                  GDIPCanvas.ExcludeClip(MakeRect(RR.Left-1,RR.Top-1,imW div 2+1,imH+2));
                  gPen.SetColor(ColorToARGB(FrameColor));
                  gBrush.SetColor(ColorToARGB(BackColor));
                  GDIPCanvas.FillPath(gBrush,RPath);
                  GDIPCanvas.DrawPath(gPen,RPath);
                  gPen.SetColor(ColorToARGB(fClr));
                  GDIPCanvas.DrawLine(gPen,rX,RR.Top,rX,RR.Top+imH);
                  RPath.Free;
                end;
              stsCircle:
                begin
                  GDIPCanvas.FillPie(gBrush,RS,90,-180);
                  GDIPCanvas.DrawPie(gPen,RS,90,-180);
                  gPen.SetColor(ColorToARGB(fClr));
                  gBrush.SetColor(ColorToARGB(BrightColor(fClr,10)));
                  GDIPCanvas.FillPie(gBrush,RS,90,180);
                  GDIPCanvas.DrawPie(gPen,RS,90,180);
                end;
            else
              begin
                _StarPoints(RR,p);
                GDIPCanvas.FillPolygon(gBrush,PGPPointF(@p[0]),10);
                GDIPCanvas.DrawPolygon(gPen,PGPPointF(@p[0]),10);
                gPen.SetColor(ColorToARGB(fClr));
                gBrush.SetColor(ColorToARGB(BrightColor(fClr,10)));
                GDIPCanvas.ExcludeClip(MakeRect(rX+1,RR.Top,imW div 2,imH));
                GDIPCanvas.FillPolygon(gBrush,PGPPointF(@p[0]),10);
                GDIPCanvas.DrawPolygon(gPen,PGPPointF(@p[0]),10);
              end;
            end;
          end;
        end;
        offsetRect(RR,imW+StarMargin,0);
        if zi=1 then
          zi:=2;
        Inc(z);
      end;
    end;

  finally
    gPen.Free;
    gBrush.Free;
  end;
end;

procedure TDecoRatingStarsPlus.Paint;
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

      DrawRating(Canvas, aRect);

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

procedure TDecoRatingStarsPlus.PrintTo(TargetCanvas:TCanvas; Dest:TRect; AsEMF:Boolean=False);
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

      DrawRating(TargetCanvas, Dest);

      if Assigned(OnAfterPaint) then
        OnAfterPaint(Self,TargetCanvas);

    finally
      GDIPCanvas.Free;
      GDIPCanvas:=nil;
    end;
  end;
end;

end.
