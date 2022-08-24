{------------------------------------------------------------------------------
  DecoCompareGridPlus.pas

  DecoCharts for VCL

  written by  Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  Purpose:    TDecoCompareGridPlus is a descendant of the standard
              TCustomDecoCompareGrid component, but it uses the GDI+ library
              for antialiased and composited drawing, that enhances the look
              and the feel of resulting grid.

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
  - added: Properties for setting the rendering mode (RenderCompositingMode, RenderSmoothingMode, RenderTextMode) have been declared as published
  - improved: Minor improvements and fixes

  Version 1.0 (2012-03-11)
  - The first release
}

{ This unit contains TDecoCompareGridPlus component declaration and implementation. }
unit DecoCompareGridPlus;

interface

uses
  Classes, Windows, Messages, Graphics, Controls, SysUtils,
  {$IF CompilerVersion >= 23}
  Winapi.GDIPAPI, Winapi.GDIPOBJ, Winapi.GDIPUTIL,
  {$ELSE}
  xGDIPAPI, xGDIPOBJ, xGDIPUTIL,
  {$IFEND}
  DecoCommon, DecoGDIP, DecoCompareGrid;

type
  { A descendant of the standard TCustomDecoCompareGrid component, but it uses the GDI+ library
    for antialiased and composited drawing, that enhances the look and the feel of resulting grid. }
  TDecoCompareGridPlus = class(TCustomDecoCompareGrid)
  private
    FRenderCompositingMode: TRenderCompositingMode;
    FRenderSmoothingMode: TRenderSmoothingMode;
    FRenderTextMode: TRenderTextMode;
    FAntiAliasing: boolean;
    {$IF CompilerVersion >= 19.0}
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    {$IFEND}
  protected
    // Main drawing method - paints the whole comparison table (criterias, items, values, frame, grid, etc.)
    procedure DrawChart(TargetCanvas:TCanvas); override;
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
  Themes, UxTheme, {$IF CompilerVersion >= 19.0} DwmApi, {$IFEND} DecoGDI,
  Forms {$IF CompilerVersion >= 24}, System.Types, System.UITypes{$IFEND};

{ TDecoCompareGridPlus }

constructor TDecoCompareGridPlus.Create(AOwner: TComponent);
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

procedure TDecoCompareGridPlus.SetRenderSmoothingMode(Value: TRenderSmoothingMode);
begin
  if Value<>FRenderSmoothingMode then
  begin
    FRenderSmoothingMode:=Value;
    Invalidate;
  end;
end;

procedure TDecoCompareGridPlus.SetRenderTextMode(Value: TRenderTextMode);
begin
  if Value<>FRenderTextMode then
  begin
    FRenderTextMode:=Value;
    Invalidate;
  end;
end;

procedure TDecoCompareGridPlus.SetRenderCompositingMode(Value: TRenderCompositingMode);
begin
  if Value<>FRenderCompositingMode then
  begin
    FRenderCompositingMode:=Value;
    Invalidate;
  end;
end;

procedure TDecoCompareGridPlus.SetAntiAliasing(Value: boolean);
begin
  if Value<>FAntiAliasing then
  begin
    FAntiAliasing:=Value;
    Invalidate;
  end;
end;

{$IF CompilerVersion >= 19.0}
procedure TDecoCompareGridPlus.WMPaint(var Message: TWMPaint);
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

procedure TDecoCompareGridPlus.Paint;
var
  aRect,cRect: TRect;
  gClipRect: TGPRect;
begin
  if FAntiAliasing {$IF CompilerVersion < 23} and UseGDIP {$IFEND} then
  begin
    if UpdateCount>0 then
      Exit;
    aRect := ClientRect;
    if FRebuildNeeded then
      Rebuild(aRect);
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

      //Canvas.Brush.Style := bsSolid;
      Canvas.Font := Font;
      if not Enabled then
        Canvas.Font.Color:=clGrayText;

      if Assigned(OnBeforePaint) then
        OnBeforePaint(Self,Canvas);

      DrawChart(Canvas);

      GDIPCanvas.SetClip(gClipRect);
      if Assigned(OnAfterPaint) then
        OnAfterPaint(Self,Canvas);

    finally
      GDIPCanvas.Free;
      GDIPCanvas:=nil;
    end;

    if FActive and ShowFocusRect and IsAnyItemSelected then
      DrawFocus(Canvas, GetFocusRect);

  end
  else
    inherited;
end;

procedure TDecoCompareGridPlus.PrintTo(TargetCanvas:TCanvas; Dest:TRect; AsEMF:Boolean=False);
var
  gBrush:TGPSolidBrush;
begin
  if AsEMF or (not FAntiAliasing) {$IF CompilerVersion < 23} or (not UseGDIP) {$IFEND} then
    inherited PrintTo(TargetCanvas, Dest, AsEMF)
  else
  begin
    GDIPCanvas:=TGPGraphics.Create(TargetCanvas.Handle);
    try
      FRebuildNeeded:=True;
      Rebuild(Dest,TargetCanvas);
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

      DrawChart(TargetCanvas);

      if Assigned(OnAfterPaint) then
        OnAfterPaint(Self,TargetCanvas);

    finally
      GDIPCanvas.Free;
      GDIPCanvas:=nil;
      FRebuildNeeded:=True;
      Rebuild(ClientRect);
    end;
  end;
end;

procedure TDecoCompareGridPlus.DrawChart(TargetCanvas:TCanvas);
var
  i,j,x,z,zi:Integer; // rows, cols
  ve,zs:Extended;
  imH,imW,rM:Integer;
  R,R2,RR:TRect;
  Flags,cFlags:Longint;
  C:TDecoCompareGridCriterium;
  A:TDecoCompareGridItem;
  V:TDecoCompareGridValue;
  tmp:string;
  drawImg:Boolean;
  hdrLn:Boolean;
  iiL:TComparedItemImageLayout;
  stripe:Integer;

  gBrush: TGPSolidBrush;
  gPen,glPen: TGPPen;
  {$IF CompilerVersion >= 23}
  oldSM: SmoothingMode;
  {$ELSE}
  oldSM: Integer;
  {$IFEND}
  glHM:Integer;
  RPath:TGPGraphicsPath;
  RS:TGPRect;
begin
  if GDIPCanvas=nil then
  begin
    inherited;
    Exit;
  end;

  // prepare GDI objects
  gBrush:=TGPSolidBrush.Create(ColorToARGB(Color));
  gPen := TGPPen.Create(ColorToARGB(FrameColor));
  glPen := TGPPen.Create(ColorToARGB(GridLineColor));
  oldSM:=GDIPCanvas.GetSmoothingMode;
  try
    if (GridLineStyle=psSolid) and (GridLineWidth<=1) then glHM:=1 else glHM:=0;
    glPen.SetDashStyle(DashStyle(GridLineStyle));
    glPen.SetWidth(GridLineWidth);
    // draw fixed areas, caption, guides
    R:=CaptionRect;
    if (FixedColor<>clNone) and (not StripeColumns) and (not StripeRows) then
    begin
      GDIPCanvas.SetSmoothingMode(SmoothingModeNone);
      gBrush.SetColor(ColorToARGB(FixedColor));
      R2:=R;
      R2.Right:=GridRect.Right;
      GDIPCanvas.FillRectangle(gBrush,MakeRect(R2));

      R2.Right:=R.Right;
      if ShowGuideFooter then
        R2.Bottom:=GuideFooter.Top
      else
        R2.Bottom:=GridRect.Bottom;
      R2.Top:=R.Bottom;
      GDIPCanvas.FillRectangle(gBrush,MakeRect(R2));

      if ShowGuideHeader then
        GDIPCanvas.FillRectangle(gBrush,MakeRect(GuideHeader));
      if ShowGuideFooter then
        GDIPCanvas.FillRectangle(gBrush,MakeRect(GuideFooter));
      GDIPCanvas.SetSmoothingMode(oldSM);
    end;

    tmp := Caption;
    TargetCanvas.Font := CaptionFont;
    if not Enabled then
      TargetCanvas.Font.Color:=clGrayText;
    TargetCanvas.Brush.Style := bsClear;
    if Assigned(OnDrawCell) then
      OnDrawCell(Self, TargetCanvas, nil, nil, nil, R, False, tmp);
    if ShowCaption and (Length(tmp)>0) then
    begin
      InflateRect(R,-2*FSpacing,0);
      Flags := DrawTextBiDiModeFlags(DT_WORDBREAK or DT_TOP or DT_CENTER);
      R2:=R;
      FDrawTextFunc(TargetCanvas.Handle, tmp, R2, Flags or DT_CALCRECT, TargetCanvas.Font.Color, GlowSize);
      i:=R2.Bottom-R2.Top;
      if R.Top+i+FSpacing>R.Bottom then
      begin
        Inc(R.Top,FSpacing);
        FDrawTextFunc(TargetCanvas.Handle, tmp, R, DrawTextBiDiModeFlags(DT_SINGLELINE or DT_TOP or DT_CENTER or DT_END_ELLIPSIS), TargetCanvas.Font.Color, GlowSize)
      end
      else
      begin
        Inc(R.Top,(R.Bottom-R.Top-i) div 2);
        FDrawTextFunc(TargetCanvas.Handle, tmp, R, Flags, TargetCanvas.Font.Color, GlowSize);
      end;
      InflateRect(R,2*FSpacing,0);
    end;
    // draw guide header
    if ShowGuideHeader then
    begin
      R2:=GuideHeader; R2.Left:=R.Left; R2.Right:=R.Right;
      TargetCanvas.Font := Font;
      if not Enabled then
        TargetCanvas.Font.Color:=clGrayText;
      TargetCanvas.Brush.Style := bsClear;
      if Assigned(OnDrawCell) then
        OnDrawCell(Self, TargetCanvas, nil, nil, nil, R2, True, tmp);
    end;
    // draw guide footer
    if ShowGuideFooter then
    begin
      R2:=GuideFooter; R2.Left:=R.Left; R2.Right:=R.Right;
      TargetCanvas.Font := Font;
      if not Enabled then
        TargetCanvas.Font.Color:=clGrayText;
      TargetCanvas.Brush.Style := bsClear;
      if Assigned(OnDrawCell) then
        OnDrawCell(Self, TargetCanvas, nil, nil, nil, R2, True, tmp);
    end;

    // draw items
    if Assigned(Images) then
    begin
      imH := Images.Height+2*FSpacing;
      imW := Images.Width+4*FSpacing;
    end
    else
    begin
      imH := 0;
      imW := 0;
    end;
    iiL:=ItemImageLayout;
    if UseRightToLeftAlignment then
      case iiL of
        iilRight:iiL:=iilLeft;
        iilLeft:iiL:=iilRight;
      end;
    Flags := DT_WORDBREAK or DT_TOP or DT_END_ELLIPSIS;
    case iiL of
      iilLeft:Flags:=Flags or DT_LEFT;
      iilRight:Flags:=Flags or DT_RIGHT;
    else
      Flags:=Flags or DT_CENTER;
    end;
    Flags := DrawTextBiDiModeFlags(Flags);
    hdrLn := (HeadGridLine and (FrameWidth>0) and (FrameColor<>clNone));

    stripe := 0;
    for j:=0 To Items.Count-1 do
    begin
      A:=TDecoCompareGridItem(Items.Items[j]);
      if not A.Visible then
        continue;
      R:=A.ItemRect;

      if StripeColumns and (FixedColor<>clNone) and (stripe mod 2 = 0) then
      begin
        R2:=R;
        if ShowGuideHeader then R2.Top:=GuideHeader.Top;
        if ShowGuideFooter then R2.Bottom:=GuideFooter.Bottom else R2.Bottom:=GridRect.Bottom;
        GDIPCanvas.SetSmoothingMode(SmoothingModeNone);
        gBrush.SetColor(ColorToARGB(FixedColor));
        GDIPCanvas.FillRectangle(gBrush,MakeRect(R2));
        GDIPCanvas.SetSmoothingMode(oldSM);
      end;
      Inc(stripe);

      // draw guide header
      if ShowGuideHeader then
      begin
        R2:=GuideHeader; R2.Left:=R.Left; R2.Right:=R.Right;
        TargetCanvas.Font := Font;
        if not Enabled then
          TargetCanvas.Font.Color:=clGrayText;
        TargetCanvas.Brush.Style := bsClear;
        tmp:=IntToStr(j+1);
        if Assigned(OnDrawCell) then
          OnDrawCell(Self, TargetCanvas, A, nil, nil, R2, True, tmp);
        if Length(tmp)>0 then
          FDrawTextFunc(TargetCanvas.Handle, tmp, R2, DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_CENTER or DT_END_ELLIPSIS), TargetCanvas.Font.Color, GlowSize);
      end;
      // draw guide footer
      if ShowGuideFooter then
      begin
        R2:=GuideFooter; R2.Left:=R.Left; R2.Right:=R.Right;
        TargetCanvas.Font := Font;
        if not Enabled then
          TargetCanvas.Font.Color:=clGrayText;
        TargetCanvas.Brush.Style := bsClear;
        tmp:=IntToStr(j+1);
        if Assigned(OnDrawCell) then
          OnDrawCell(Self, TargetCanvas, A, nil, nil, R2, True, tmp);
        if Length(tmp)>0 then
        begin
          Dec(R2.Bottom);
          FDrawTextFunc(TargetCanvas.Handle, tmp, R2, DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_CENTER or DT_END_ELLIPSIS), TargetCanvas.Font.Color, GlowSize);
        end;
      end;

      TargetCanvas.Font := ItemFont;
      if not Enabled then
        TargetCanvas.Font.Color:=clGrayText;
      TargetCanvas.Brush.Style := bsClear;
      if A=HoverCell then
        TargetCanvas.Font.Style:=TargetCanvas.Font.Style+HoverStyle;

      tmp:=A.Caption;
      if Assigned(OnDrawCell) then
        OnDrawCell(Self, TargetCanvas, A, nil, nil, R, False, tmp);

      InflateRect(R,-2*FSpacing,0);
      if Assigned(Images) and (R.Bottom-R.Top>=imH) and (R.Right - R.Left>imW) then
      begin
        if A.ImageIndex>=0 then
          case iiL of
            iilBottom:
              begin
                Images.Draw(TargetCanvas, R.Left + (R.Right - R.Left - Images.Width) div 2, R.Bottom - imH + FSpacing, A.ImageIndex);
                Dec(R.Bottom,imH);
              end;
            iilTop:
              begin
                Images.Draw(TargetCanvas, R.Left + (R.Right - R.Left - Images.Width) div 2, R.Top + FSpacing, A.ImageIndex);
                Inc(R.Top,imH);
              end;
            iilLeft:
              begin
                Images.Draw(TargetCanvas, R.Left + 2*FSpacing, R.Top + (R.Bottom - R.Top - Images.Height) div 2, A.ImageIndex);
                Inc(R.Left,imW+2*FSpacing);
              end;
            iilRight:
              begin
                Images.Draw(TargetCanvas, R.Right - 2*FSpacing - Images.Width, R.Top + (R.Bottom - R.Top - Images.Height) div 2, A.ImageIndex);
                Dec(R.Right,imW+2*FSpacing);
              end;
          end;
      end;

      if Length(tmp)>0 then
      begin
        R2:=R;
        FDrawTextFunc(TargetCanvas.Handle, A.Caption, R2, Flags or DT_CALCRECT, TargetCanvas.Font.Color, GlowSize);
        i:=R2.Bottom-R2.Top;
        if (i>R.Bottom-R.Top-FSpacing) or (R2.Left<R.Left) or (R2.Right>R.Right) then
        begin
          R2:=R;
          FDrawTextFunc(TargetCanvas.Handle, A.Caption, R2, DT_SINGLELINE or Flags or DT_CALCRECT and (not DT_WORDBREAK), TargetCanvas.Font.Color, GlowSize);
          i:=R2.Bottom-R2.Top;
          if i<=R.Bottom-R.Top-FSpacing then
          begin
            case ItemTextLayout of
              taAlignTop:if iiL<>iilTop then Inc(R.Top,FSpacing);
              taAlignBottom:begin R.Top:=R.Bottom-i; if (iiL<>iilBottom) or (not Assigned(Images)) then Dec(R.Top,FSpacing); end;
            else
              Inc(R.Top,(R.Bottom-R.Top-i) div 2);
            end;
            FDrawTextFunc(TargetCanvas.Handle, tmp, R, DT_SINGLELINE or Flags and (not DT_WORDBREAK), TargetCanvas.Font.Color, GlowSize)
          end;
        end
        else
        begin
          case ItemTextLayout of
            taAlignTop:if iiL<>iilTop then Inc(R.Top,FSpacing);
            taAlignBottom:begin R.Top:=R.Bottom-i; if (iiL<>iilBottom) or (not Assigned(Images)) then Dec(R.Top,FSpacing); end;
          else
            Inc(R.Top,(R.Bottom-R.Top-i) div 2);
          end;

          FDrawTextFunc(TargetCanvas.Handle, tmp, R, Flags, TargetCanvas.Font.Color, GlowSize);
        end;
      end;

    end;

    // draw values
    Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_CENTER or DT_END_ELLIPSIS or DT_VCENTER);
    for j:=0 To Items.Count-1 do
    begin
      A:=TDecoCompareGridItem(Items.Items[j]);
      if not A.Visible then
        continue;
      stripe:=0;
      x:=A.Values.Count;
      if x>Criterias.Count then
        x:=Criterias.Count;
      for i:=0 To x-1 do
      begin
        V:=TDecoCompareGridValue(A.Values.Items[i]);
        C:=TDecoCompareGridCriterium(Criterias.Items[i]);
        R2:=V.ItemRect;

        if StripeRows and (FixedColor<>clNone) and (stripe mod 2 = 0) then
        begin
          GDIPCanvas.SetSmoothingMode(SmoothingModeNone);
          gBrush.SetColor(ColorToARGB(FixedColor));
          GDIPCanvas.FillRectangle(gBrush,MakeRect(R2));
          GDIPCanvas.SetSmoothingMode(oldSM);
        end;
        Inc(stripe);

        if not V.Visible then
          continue;

        TargetCanvas.Font := Font;
        if not Enabled then
          TargetCanvas.Font.Color:=clGrayText;
        TargetCanvas.Brush.Style := bsClear;
        if V=HoverCell then
          TargetCanvas.Font.Style:=TargetCanvas.Font.Style+HoverStyle;

        tmp:=GetValueFormatted(C,V);
        if Assigned(OnDrawCell) then
          OnDrawCell(Self, TargetCanvas, A, C, V, R2, False, tmp);
        case C.Style of
          cgsCheck:
              if Assigned(CheckImages) then
              begin
                CheckImages.Draw(TargetCanvas, R2.Left + (R2.Right - R2.Left - CheckImages.Width) div 2,
                  R2.Top + (R2.Bottom - R2.Top - CheckImages.Height) div 2, Integer(V.AsBoolean));
                tmp := '';
              end;
          cgsRating:
              begin
                RR:=R2;
                drawImg:=Assigned(RatingImages) and (RatingImages.Count>1);
                if drawImg then
                begin
                  imW:=RatingImages.Width;
                  cFlags:=RatingImages.Count;
                  rM:=0;
                  offsetRect(RR,0,1);
                end
                else
                begin
                  gPen.SetWidth(1);
                  imW:=TargetCanvas.Font.Size;
                  imW:=imW+(1-imW mod 2);
                  if imW>RR.Bottom-RR.Top then imW:=RR.Bottom-RR.Top
                  else if imW<4 then imW:=4;
                  cFlags:=3;
                  rM:=(imW div 8)+2;
                  Dec(imW);
                end;
                imH:=imW;

                RR.Left := RR.Left + (RR.Right-RR.Left - 5*(imW+rM)) div 2;
                RR.Right:=RR.Left+imW;
                RR.Top := RR.Top + (RR.Bottom-RR.Top - imH) div 2;
                offsetRect(RR,rM div 2,-1);

                if (RR.Left-2*FSpacing>=R2.Left) and (RR.Right+2*FSpacing<=R2.Right) then
                begin
                  ve:=V.AsExtended;
                  zi:=0;
                  z:=1;
                  if cFlags=3 then
                  begin
                    zs:=round((ve/((C.MaxValue-C.MinValue)/5)+0.2)*10)/10;
                    while z<=5 do
                    begin
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
                        RatingImages.Draw(TargetCanvas, RR.Left, RR.Top, zi)
                      else
                      begin
                        RS:=MakeRect(RR.Left,RR.Top,imW,imH);
                        if zi=0 then
                        begin
                          gPen.SetColor(ColorToARGB(ProgBarLevel2Color));
                          gBrush.SetColor(ColorToARGB(BrightColor(ProgBarLevel2Color,10)));
                          GDIPCanvas.FillEllipse(gBrush,RS);
                          GDIPCanvas.DrawEllipse(gPen,RS);
                        end
                        else
                        if zi=cFlags-1 then
                        begin
                          gPen.SetColor(ColorToARGB(ProgBarFrameColor));
                          gBrush.SetColor(ColorToARGB(ProgBarBackColor));
                          GDIPCanvas.FillEllipse(gBrush,RS);
                          GDIPCanvas.DrawEllipse(gPen,RS);
                        end
                        else
                        begin
                          gPen.SetColor(ColorToARGB(ProgBarLevel2Color));
                          gBrush.SetColor(ColorToARGB(BrightColor(ProgBarLevel2Color,10)));
                          GDIPCanvas.FillPie(gBrush,RS,90,180);
                          GDIPCanvas.DrawPie(gPen,RS,90,180);
                          gPen.SetColor(ColorToARGB(ProgBarFrameColor));
                          gBrush.SetColor(ColorToARGB(ProgBarBackColor));
                          GDIPCanvas.FillPie(gBrush,RS,90,-180);
                          GDIPCanvas.DrawPie(gPen,RS,90,-180);
                        end;
                      end;
                      offsetRect(RR,imW+rM,0);
                      if zi=1 then
                        zi:=2;
                      Inc(z);
                    end;
                  end
                  else
                  begin
                    zs:=ve/((C.MaxValue-C.MinValue)/5)+0.5;
                    while z<=5 do
                    begin
                      if (zi=0) and (zs<z) then zi:=1;
                      RatingImages.Draw(TargetCanvas, RR.Left, RR.Top, zi);
                      offsetRect(RR,imW+rM,0);
                      Inc(z);
                    end;
                  end;
                  tmp := '';
                end;
              end;
          cgsProgressBar:
            begin
              RR:=R2;
              InflateRect(RR,-4*FSpacing,0);
              if RR.Left+FSpacing*12<RR.Right then
              begin
                if (ProgBarHeight>0) and (ProgBarHeight<=R2.Bottom-R2.Top - 2*FSpacing) then
                begin
                  RR.Top:=RR.Top+(RR.Bottom-RR.Top-ProgBarHeight) div 2;
                  RR.Bottom:=RR.Top+ProgBarHeight;
                end
                else
                begin
                  InflateRect(RR,0,-FSpacing);
                  Dec(RR.Bottom);
                end;
                TargetCanvas.Font.Style:=[];
                GDIP_DrawProgressBar(TargetCanvas, GDIPCanvas, RR, V.AsExtended, C.MinValue, C.MaxValue, ProgBarGradient, Color, ProgBarBackColor, ProgBarFrameColor,
                  1, ProgBarRounded, ProgBarRoundCorners, ProgBarFrameProgress, ProgBarShowOverMaxIndicator, ProgBarLevel1Color, ProgBarLevel1TextColor,
                  ProgBarLevel2Color, ProgBarLevel2Percent, ProgBarLevel2TextColor, ProgBarLevel3Color, ProgBarLevel3Percent, ProgBarLevel3TextColor,
                  ProgBarShowValueInBar, Enabled, _EMFExporting,FDrawTextFunc,GlowSize);
                tmp:='';
                TargetCanvas.Font := Font;
                if not Enabled then
                  TargetCanvas.Font.Color:=clGrayText;
              end;
            end;
        else
            begin
              // cgsCustom, cgsText - handled bellow
            end;
        end;
        if Length(tmp)>0 then
        begin
          InflateRect(R2,-2*FSpacing,0);
          FDrawTextFunc(TargetCanvas.Handle, tmp, R2, Flags, TargetCanvas.Font.Color, GlowSize);
        end;

      end;
    end;

    // draw criteria
    Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DrawTextAlignments[CriteriaAlignment] or DT_END_ELLIPSIS or DT_VCENTER);
    cFlags := DrawTextBiDiModeFlags(DT_SINGLELINE or DrawTextAlignments[CategoryAlignment] or DT_END_ELLIPSIS);
    for i:=0 To Criterias.Count-1 do
    begin
      C:=TDecoCompareGridCriterium(Criterias.Items[i]);

      if ShowCategories and C.IsFirstInCategory then
      begin
        R2:=C.CategoryRect;
        R2.Right:=GridRect.Right;
        if CategoryColor=clNone then
          gBrush.SetColor(ColorToARGB(Color))
        else
          gBrush.SetColor(ColorToARGB(CategoryColor));
        GDIPCanvas.SetSmoothingMode(SmoothingModeNone);
        GDIPCanvas.FillRectangle(gBrush,MakeRect(R2));
        GDIPCanvas.SetSmoothingMode(oldSM);

        TargetCanvas.Font := CaptionFont;
        if not Enabled then
          TargetCanvas.Font.Color:=clGrayText;
        TargetCanvas.Brush.Style := bsClear;
        tmp:=C.Category;
        if Assigned(OnDrawCell) then
          OnDrawCell(Self, TargetCanvas, nil, C, nil, R2, True, tmp);
        InflateRect(R2,-2*FSpacing,0);
        if Length(tmp)>0 then
        begin
          if C.CategoryRect.Bottom-C.CategoryRect.Top>C.ItemRect.Bottom-C.ItemRect.Top+FSpacing then
          begin
            Dec(R2.Bottom,FSpacing);
            FDrawTextFunc(TargetCanvas.Handle, tmp, R2, cFlags or DT_BOTTOM, TargetCanvas.Font.Color, GlowSize)
          end
          else
            FDrawTextFunc(TargetCanvas.Handle, tmp, R2, cFlags or DT_VCENTER, TargetCanvas.Font.Color, GlowSize);
        end;

        if hdrLn or (ShowVertGrid and ((GridLineWidth>0) and (GridLineColor<>clNone))) then
        begin
          j:=((FrameWidth+1) div 2);
          if hdrLn then
          begin
            z:=(FrameWidth) div 2;
            SetRect(R2,C.CategoryRect.Left+j+1,C.CategoryRect.Top+z-1+(FrameWidth mod 2),GridRect.Right-j-1,C.CategoryRect.Bottom-z-1)
          end
          else
          if ShowHorzGrid then
          begin
            z:=(GridLineWidth) div 2;
            SetRect(R2,C.CategoryRect.Left+j+1,C.CategoryRect.Top+z-1+(GridLineWidth mod 2),GridRect.Right-j-1,C.CategoryRect.Bottom-z-1)
          end;
          if hdrLn or (not ShowHorzGrid) then
          begin
            R2:=C.CategoryRect;
            R2.Right:=GridRect.Right;
            GDIPCanvas.SetSmoothingMode(SmoothingModeNone);
            if hdrLn then
            begin
              gPen.SetWidth(FrameWidth);
              gPen.SetColor(ColorToARGB(FrameColor));
              GDIPCanvas.DrawLine(gPen,R2.Left,R2.Top-1,R2.Right-1,R2.Top-1);
              GDIPCanvas.DrawLine(gPen,R2.Left,R2.Bottom-1,R2.Right-1,R2.Bottom-1);
            end
            else
            begin
              GDIPCanvas.DrawLine(glPen,R2.Left,R2.Top-1,R2.Right-glHM,R2.Top-1);
              GDIPCanvas.DrawLine(glPen,R2.Left,R2.Bottom-1,R2.Right-glHM,R2.Bottom-1);
            end;
            GDIPCanvas.SetSmoothingMode(oldSM);
            SetRect(R2,C.CategoryRect.Left+j+1,C.CategoryRect.Top,GridRect.Right-j-1,C.CategoryRect.Bottom);
          end;
          GDIPCanvas.ExcludeClip(MakeRect(R2));
        end;
      end;

      R:=C.ItemRect;
      if StripeRows and (FixedColor<>clNone) and (i mod 2 = 0) then
      begin
        R2:=R;
        GDIPCanvas.SetSmoothingMode(SmoothingModeNone);
        gBrush.SetColor(ColorToARGB(FixedColor));
        GDIPCanvas.FillRectangle(gBrush,MakeRect(R2));
        GDIPCanvas.SetSmoothingMode(oldSM);
      end;

      TargetCanvas.Font := CriteriaFont;
      if not Enabled then
        TargetCanvas.Font.Color:=clGrayText;
      TargetCanvas.Brush.Style := bsClear;
      if C=HoverCell then
        TargetCanvas.Font.Style:=TargetCanvas.Font.Style+HoverStyle;
      tmp:=C.Caption;
      if Assigned(OnDrawCell) then
        OnDrawCell(Self, TargetCanvas, nil, C, nil, R, False, tmp);
      InflateRect(R,-2*FSpacing,0);
      if Length(tmp)>0 then
      begin
        if ShowCategories and (Length(C.Category)>0) and (CategoryAlignment=taLeftJustify) then
          Inc(R.Left,CategoryIndent);
        FDrawTextFunc(TargetCanvas.Handle, tmp, R, Flags, TargetCanvas.Font.Color, GlowSize);
      end
    end;

    // draw grid lines
    if ((GridLineWidth>0) and (GridLineColor<>clNone)) and (ShowVertGrid or ShowHorzGrid) then
    begin
      GDIPCanvas.SetSmoothingMode(SmoothingModeNone);
      if ShowVertGrid and (Items.Count>0) then
        for j:=0 To Items.Count-1 do
        begin
          A:=TDecoCompareGridItem(Items.Items[j]);
          GDIPCanvas.DrawLine(glPen,A.ItemRect.Left-1,GridRect.Top,A.ItemRect.Left-1,GridRect.Bottom-glHM);
        end;
      if ShowHorzGrid and (Criterias.Count>0) then
        for i:=0 To Criterias.Count-1 do
        begin
          C:=TDecoCompareGridCriterium(Criterias.Items[i]);
          if ShowCategories and C.IsFirstInCategory and (not hdrLn) then
            GDIPCanvas.DrawLine(glPen,C.CategoryRect.Left,C.CategoryRect.Top-1,GridRect.Right-glHM,C.CategoryRect.Top-1);
          GDIPCanvas.DrawLine(glPen,C.ItemRect.Left,C.ItemRect.Top-1,GridRect.Right-glHM,C.ItemRect.Top-1);
        end;
      GDIPCanvas.SetSmoothingMode(oldSM);
    end;

    // draw frame around
    if (FrameWidth>0) and (FrameColor<>clNone) then
    begin
      TargetCanvas.Brush.Style := bsClear;
      GDIPCanvas.SetSmoothingMode(SmoothingModeNone);
      gPen.SetWidth(FrameWidth);
      gPen.SetColor(ColorToARGB(FrameColor));

      if ShowGuideHeader then
        GDIPCanvas.DrawLine(gPen,GuideHeader.Left,GuideHeader.Bottom,GuideHeader.Right-1,GuideHeader.Bottom);

      if ShowGuideFooter then
        GDIPCanvas.DrawLine(gPen,GuideFooter.Left,GuideFooter.Top,GuideFooter.Right-1,GuideFooter.Top);

      if HeadGridLine then
      begin
        if Items.Count>0 then
          GDIPCanvas.DrawLine(gPen,CaptionRect.Right-1,GridRect.Top,CaptionRect.Right-1,GridRect.Bottom-1);
        if Criterias.Count>0 then
          GDIPCanvas.DrawLine(gPen,GridRect.Left,CaptionRect.Bottom-1,GridRect.Right-1,CaptionRect.Bottom-1);
      end;

      RR:=GridRect;
      Dec(RR.Right); Dec(RR.Bottom);
      if FrameRoundCorners>0 then
      begin
        GDIPCanvas.SetSmoothingMode(oldSM);
        RPath := GDIP_RoundRectanglePath(RR, FrameRoundCorners);
        GDIPCanvas.DrawPath(gPen,RPath);
        RPath.Free;
      end
      else
        GDIPCanvas.DrawRectangle(gPen,MakeRect(RR));
    end;

  finally
    glPen.Free;
    gPen.Free;
    gBrush.Free;
    GDIPCanvas.SetSmoothingMode(oldSM);
  end;
end;

end.
