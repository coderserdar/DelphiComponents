{------------------------------------------------------------------------------
  DecoProgressGridPlus.pas

  DecoCharts for VCL

  written by  Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  Purpose:    TDecoProgressGridPlus is a descendant of the standard
              TCustomDecoProgressGrid component, but it uses the GDI+ library
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
  - improved: Minor improvements and fixes

  Version 1.0 (2011-07-17)
  - The first release
}

{ This unit contains TDecoProgressGridPlus component declaration and implementation. }
unit DecoProgressGridPlus;

interface

uses
  Classes, Windows, Messages, Graphics, Controls, Forms, SysUtils,
  {$IF CompilerVersion >= 23}
  Winapi.GDIPAPI, Winapi.GDIPOBJ, Winapi.GDIPUTIL,
  {$ELSE}
  xGDIPAPI, xGDIPOBJ, xGDIPUTIL,
  {$IFEND}
  DecoCommon, DecoGDIP, DecoProgressGrid;

type
  { A descendant of the standard TCustomDecoProgressGrid component, but it uses the GDI+ library
    for antialiased and composited drawing, that enhances the look and the feel of resulting grid. }
  TDecoProgressGridPlus = class(TCustomDecoProgressGrid)
  private
    FRenderCompositingMode: TRenderCompositingMode;
    FRenderSmoothingMode: TRenderSmoothingMode;
    FRenderTextMode: TRenderTextMode;
    FAntiAliasing: boolean;
    {$IF CompilerVersion >= 19}
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    {$IFEND}
  protected
    // This method implements the drawing of grid lines using the GDI+ library. See also @inherited.
    procedure DrawGrid(TargetCanvas:TCanvas); override;
    // This method implements the drawing of data rows (progress bars and text cells) using the GDI+ library. See also @inherited.
    procedure DrawItems(TargetCanvas:TCanvas); override;
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
    property Align;
    { See @inherited }
    property Alignment;
    { See @inherited }
    property Anchors;
    { See @inherited }
    property AutoSize;
    { See @inherited }
    property BackColor;
    { See @inherited }
    property BarPosition;
    { See @inherited }
    property BevelEdges;
    { See @inherited }
    property BevelKind;
    { See @inherited }
    property BiDiMode;
    { See @inherited }
    property BorderStyle;
    { See @inherited }
    property Caption;
    { See @inherited }
    property CaptionFont;
    { See @inherited }
    property Color;
    { See @inherited }
    property Columns;
    { See @inherited }
    property Constraints;
    { See @inherited }
    property Ctl3D;
    { See @inherited }
    property DoubleBuffered;
    { See @inherited }
    property DragCursor;
    { See @inherited }
    property DragKind;
    { See @inherited }
    property DragMode;
    { See @inherited }
    property Enabled;
    { See @inherited }
    property Font;
    { See @inherited }
    property FrameColor;
    { See @inherited }
    property FrameProgress;
    { See @inherited }
    property FrameWidth;
    { See @inherited }
    property GlowSize;
    { See @inherited }
    property Gradient;
    { See @inherited }
    property GridLineColor;
    { See @inherited }
    property GridLineStyle;
    { See @inherited }
    property HoverCursor;
    { See @inherited }
    property Items;
    { See @inherited }
    property Level1Color;
    { See @inherited }
    property Level1TextColor;
    { See @inherited }
    property Level2Color;
    { See @inherited }
    property Level2Percent;
    { See @inherited }
    property Level2TextColor;
    { See @inherited }
    property Level3Color;
    { See @inherited }
    property Level3Percent;
    { See @inherited }
    property Level3TextColor;
    { See @inherited }
    property OnAfterPaint;
    {$IF CompilerVersion >= 20}
    { See @inherited }
    property OnAlignInsertBefore;
    { See @inherited }
    property OnAlignPosition;
    {$IFEND}
    { See @inherited }
    property OnBeforePaint;
    { See @inherited }
    property OnCanResize;
    { See @inherited }
    property OnClick;
    { See @inherited }
    property OnCompareItems;
    { See @inherited }
    property OnConstrainedResize;
    { See @inherited }
    property OnContextPopup;
    { See @inherited }
    property OnDblClick;
    { See @inherited }
    property OnDragDrop;
    { See @inherited }
    property OnDragOver;
    { See @inherited }
    property OnDrawCell;
    { See @inherited }
    property OnEndDrag;
    { See @inherited }
    property OnEnter;
    { See @inherited }
    property OnExit;
    { See @inherited }
    property OnHoverChange;
    { See @inherited }
    property OnItemClick;
    { See @inherited }
    property OnKeyDown;
    { See @inherited }
    property OnKeyPress;
    { See @inherited }
    property OnKeyUp;
    {$IF CompilerVersion >= 18}
    { See @inherited }
    property OnMouseActivate;
    { See @inherited }
    property OnMouseEnter;
    { See @inherited }
    property OnMouseLeave;
    {$IFEND}
    { See @inherited }
    property OnMouseDown;
    { See @inherited }
    property OnMouseMove;
    { See @inherited }
    property OnMouseUp;
    { See @inherited }
    property OnResize;
    { See @inherited }
    property OnSelectItem;
    { See @inherited }
    property OnStartDrag;
    {$IF CompilerVersion >= 20}
    { See @inherited }
    property Padding;
    {$IFEND}
    { See @inherited }
    property ParentBackground;
    { See @inherited }
    property ParentBiDiMode;
    { See @inherited }
    property ParentColor;
    { See @inherited }
    property ParentCtl3D;
    {$IF CompilerVersion >= 20}
    { See @inherited }
    property ParentDoubleBuffered;
    {$IFEND}
    { See @inherited }
    property ParentFont;
    { See @inherited }
    property ParentShowHint;
    { See @inherited }
    property PopupMenu;
    { Composition mode for GDI+ drawing }
    property RenderCompositingMode: TRenderCompositingMode read FRenderCompositingMode write SetRenderCompositingMode default rcmDefault;
    { Smoothing mode for GDI+ drawing }
    property RenderSmoothingMode: TRenderSmoothingMode read FRenderSmoothingMode write SetRenderSmoothingMode default rsmAntiAlias;
    { Text rendering mode for GDI+ drawing }
    property RenderTextMode: TRenderTextMode read FRenderTextMode write SetRenderTextMode default rtmDefault;
    { See @inherited }
    property RoundCorners;
    { See @inherited }
    property Rounded;
    { See @inherited }
    property RowHeight;
    { See @inherited }
    property ShowCaption;
    { See @inherited }
    property ShowColumnsHeader;
    { See @inherited }
    property ShowFirstGridLine;
    { See @inherited }
    property ShowFocusRect;
    { See @inherited }
    property ShowHint;
    { See @inherited }
    property ShowHorzGrid;
    { See @inherited }
    property ShowLastGridLine;
    { See @inherited }
    property ShowOverMaxIndicator;
    { See @inherited }
    property ShowValueInBar;
    { See @inherited }
    property ShowVertGrid;
    { See @inherited }
    property Sorted;
    { See @inherited }
    property TabOrder;
    { See @inherited }
    property TabStop;
    { See @inherited }
    property TextMargin;
    { See @inherited }
    property ValueTextFormat;
    { See @inherited }
    property Visible;
    { If False, the GDI+ library is not used for drawing even if it is available }
    property AntiAliasing: boolean read FAntiAliasing write SetAntiAliasing default True;
  end;

implementation

uses
  Themes, UxTheme, {$IF CompilerVersion >= 19} DwmApi, {$IFEND} DecoGDI
  {$IF CompilerVersion >= 24}, System.Types {$IFEND};

{ TDecoPreogressGridPlus }

constructor TDecoProgressGridPlus.Create(AOwner: TComponent);
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

procedure TDecoProgressGridPlus.SetRenderSmoothingMode(Value: TRenderSmoothingMode);
begin
  if Value<>FRenderSmoothingMode then
  begin
    FRenderSmoothingMode:=Value;
    Invalidate;
  end;
end;

procedure TDecoProgressGridPlus.SetRenderTextMode(Value: TRenderTextMode);
begin
  if Value<>FRenderTextMode then
  begin
    FRenderTextMode:=Value;
    Invalidate;
  end;
end;

procedure TDecoProgressGridPlus.SetRenderCompositingMode(Value: TRenderCompositingMode);
begin
  if Value<>FRenderCompositingMode then
  begin
    FRenderCompositingMode:=Value;
    Invalidate;
  end;
end;

procedure TDecoProgressGridPlus.SetAntiAliasing(Value: boolean);
begin
  if Value<>FAntiAliasing then
  begin
    FAntiAliasing:=Value;
    Invalidate;
  end;
end;

{$IF CompilerVersion >= 19}
procedure TDecoProgressGridPlus.WMPaint(var Message: TWMPaint);
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

procedure TDecoProgressGridPlus.DrawGrid(TargetCanvas:TCanvas);
var
  i,j:Integer;
  Col:TDecoProgressGridColumn;
  Item:TDecoProgressGridItem;
  gPen:TGPPen;
  {$IF CompilerVersion >= 23}
  oldSM: SmoothingMode;
  {$ELSE}
  oldSM: Integer;
  {$IFEND}
  df:Integer;
begin
  if GDIPCanvas=nil then
    inherited
  else
  if ShowHorzGrid or (ShowVertGrid and (BarPosition=pgbInline)) then
  begin
    oldSM:=GDIPCanvas.GetSmoothingMode;
    if Enabled then
      gPen := TGPPen.Create(ColorToARGB(GridLineColor,255))
    else
      gPen := TGPPen.Create(ColorToARGB(GrayScaleColor(GridLineColor),255));
    try
      GDIPCanvas.SetSmoothingMode(SmoothingModeNone);
      gPen.SetDashStyle(DashStyle(GridLineStyle));
      gPen.SetWidth(1);
      if GridLineStyle=psSolid then df:=1 else df:=0;
      if ShowHorzGrid then
      begin
        Item:=nil;
        j:=0;
        for i:=0 To Items.Count-1 do
          if Items[i].Visible then
          begin
            Item:=Items[i];
            Inc(j);
            if (j=1) and (not ShowFirstGridLine) then
              Continue;
            GDIPCanvas.DrawLine(gPen,Item.ItemRect.Left,Item.ItemRect.Top,Item.ItemRect.Right-df,Item.ItemRect.Top);
          end;
        if ShowLastGridLine and Assigned(Item) then
          GDIPCanvas.DrawLine(gPen,Item.ItemRect.Left,Item.ItemRect.Bottom,Item.ItemRect.Right-df,Item.ItemRect.Bottom);
      end;
      if ShowVertGrid and (BarPosition=pgbInline) then
      begin
        Col:=nil;
        j:=0;
        for i:=0 To Columns.Count-1 do
          if Columns[i].Visible then
          begin
            Col:=Columns[i];
            Inc(j);
            if (j=1) and (not ShowFirstGridLine) then
              Continue;
            GDIPCanvas.DrawLine(gPen,Col.ColRect.Left,FBarsRect.Top,Col.ColRect.Left,FBarsRect.Bottom);
          end;
        if ShowLastGridLine and Assigned(Col) then
          GDIPCanvas.DrawLine(gPen,Col.ColRect.Right-1,FBarsRect.Top,Col.ColRect.Right-1,FBarsRect.Bottom);
      end;
    finally
      gPen.Free;
      GDIPCanvas.SetSmoothingMode(oldSM);
    end;
  end;
end;

procedure TDecoProgressGridPlus.DrawItems(TargetCanvas:TCanvas);
var
  c,i,j:Integer;
  Item:TDecoProgressGridItem;
  tmpClr,invClr,frmClr:TColor;
  rcs:Integer;
  cRect,cellRect,vRect,bRect,tmpRect:TRect;
  Col:TDecoProgressGridColumn;
  Flags: Longint;
  percent:Extended;
  tmpStr:string;

  RPath: TGPGraphicsPath;
  RC: TGPRegion;
  gBrush, gbBrush: TGPBrush;
  gPen,sdPen,slPen: TGPPen;
  bY,fY: Integer;
  alpha: Byte;
  eb:Boolean;
begin
  // draw items
  if GDIPCanvas=nil then
    inherited
  else
  if Items.Count>0 then
  begin
    gPen := TGPPen.Create(ColorToARGB(FrameColor,255));
    sdPen := TGPPen.Create(ColorToARGB(clBlack,24));
    slPen := TGPPen.Create(ColorToARGB(clWhite,72));
    bY := 0; fY := 0;
    eb:=Enabled;
    if BackColor=clNone then
    begin
      alpha:=24;
      tmpClr:=BrightColor(Font.Color,23)
    end
    else
    begin
      alpha:=255;
      tmpClr:=BackColor;
    end;
    if eb then
      frmClr:=FrameColor
    else
    begin
      frmClr:=GrayScaleColor(FrameColor);
      tmpClr:=GrayScaleColor(tmpClr);
    end;
    case Gradient of
      pggNone:
        begin
          gBrush:=TGPSolidBrush.Create(ColorToARGB(Color,255));
          gbBrush:=TGPSolidBrush.Create(ColorToARGB(tmpClr,alpha));
        end;
    else
      begin
        gBrush:=TGPLinearGradientBrush.Create(MakePoint(0,0),MakePoint(0,RowHeight),ColorToARGB(clWhite),ColorToARGB(clBlack));
        gbBrush:=TGPLinearGradientBrush.Create(MakePoint(0,0),MakePoint(0,RowHeight),ColorToARGB(BrightColor(tmpClr,15),alpha),ColorToARGB(DarkColor(tmpClr,8),alpha));
        case Gradient of
          pggMirror, pggLightMirror:
            begin
              TGPLinearGradientBrush(gBrush).SetBlend(@brush_mirror_Intensities, @brush_mirror_Positions, 3);
              TGPLinearGradientBrush(gbBrush).SetBlend(@brush_mirror_Intensities, @brush_mirror_Positions, 3);
            end;
          pggGlass:
            begin
              TGPLinearGradientBrush(gBrush).SetBlend(@brush_glass_Intensities, @brush_glass_Positions, 4);
              TGPLinearGradientBrush(gbBrush).SetBlend(@brush_glass_Intensities, @brush_glass_Positions, 4);
            end;
        end;
        case Gradient of
          pggLightMirror:TGPLinearGradientBrush(gbBrush).SetLinearColors(ColorToARGB(BrightColor(tmpClr,29),alpha),ColorToARGB(DarkColor(tmpClr,1),alpha));
          pggGlass:TGPLinearGradientBrush(gbBrush).SetLinearColors(ColorToARGB(BrightColor(tmpClr,21),alpha),ColorToARGB(DarkColor(tmpClr,2),alpha));
        end;
      end;
    end;

    try
      TargetCanvas.Font:=Font;
      rcs:=_GetRoundedCornersFactor;
      for i:=0 To Items.Count-1 do
      begin
        Item:=Items[i];
        if Item.Visible then
        begin
          percent:=(Item.Value-Item.MinValue)/(Item.MaxValue-Item.MinValue);
          // setup color by level
          if percent*100>=Level3Percent then
          begin
            tmpClr:=Level3Color;
            invClr:=Level3TextColor;
          end
          else
          if percent*100>=Level2Percent then
          begin
            tmpClr:=Level2Color;
            invClr:=Level2TextColor;
          end
          else
          begin
            tmpClr:=Level1Color;
            invClr:=Level1TextColor;
          end;
          if not eb then
          begin
            tmpClr:=GrayScaleColor(tmpClr);
            invClr:=GrayScaleColor(invClr);
          end;
          for c:=0 To Columns.Count-1 do
          begin
            Col:=Columns[c];
            if Col.Visible then
            begin
              cellRect:=Col.ColRect;
              if cellRect.Left+FSpacing*8>=cellRect.Right then
                continue;
              OffsetRect(cellRect,0,Item.ItemRect.Top);
              cRect:=cellRect;
              InflateRect(cRect,-FSpacing,0);
              if Col.Kind=pgcProgressBar then
              begin
                InflateRect(cRect,-1,-1);
                if Rounded then
                begin
                  RPath := GDIP_RoundRectanglePath(cRect, rcs);
                  RC := TGPRegion.Create(RPath);
                  RPath.Free;
                  Dec(cRect.Top);
                  RPath := GDIP_RoundRectanglePath(cRect, rcs);
                end
                else
                begin
                  Dec(cRect.Top);
                  RC:=nil;
                  RPath:=nil;
                end;

                vRect:=cRect;
                if percent<0 then
                  vRect.Right:=vRect.Left
                else
                  vRect.Right:=vRect.Left+Round((vRect.Right-vRect.Left)*percent);
                if vRect.Right>=cRect.Right then
                begin
                  if vRect.Right>cRect.Right then
                    vRect.Right:=cRect.Right;
                  SetRectEmpty(bRect);
                end
                else
                begin
                  // background rect
                  bRect:=cRect;
                  bRect.Left:=vRect.Right;
                  if gbBrush is TGPLinearGradientBrush then
                  begin
                    TGPLinearGradientBrush(gbBrush).TranslateTransform(0,bRect.Top-bY,MatrixOrderAppend);
                    bY:=bRect.Top;
                  end;
                  if Rounded then
                  begin
                    if (vRect.Right>vRect.Left) then
                    begin
                      tmpRect:=vRect; inflateRect(tmpRect,0,FrameWidth); Dec(tmpRect.Left,FrameWidth);
                      GDIPCanvas.ExcludeClip(MakeRect(tmpRect));
                    end;
                    GDIPCanvas.FillPath(gbBrush,RPath);
                    GDIPCanvas.ResetClip;
                  end
                  else
                  begin
                    GDIPCanvas.FillRectangle(gbBrush,MakeRect(bRect));
                  end;
                end;
                // value rect
                if vRect.Right>vRect.Left then
                begin
                  if gBrush is TGPLinearGradientBrush then
                  begin
                    TGPLinearGradientBrush(gBrush).TranslateTransform(0,vRect.Top-fY,MatrixOrderAppend);
                    fY:=vRect.Top;
                  end;
                  case Gradient of
                    pggVertical, pggMirror:
                      TGPLinearGradientBrush(gBrush).SetLinearColors(ColorToARGB(BrightColor(tmpClr,15)),ColorToARGB(DarkColor(tmpClr,8)));
                    pggLightMirror:TGPLinearGradientBrush(gBrush).SetLinearColors(ColorToARGB(BrightColor(tmpClr,29)),ColorToARGB(DarkColor(tmpClr,1)));
                    pggGlass:TGPLinearGradientBrush(gBrush).SetLinearColors(ColorToARGB(BrightColor(tmpClr,21)),ColorToARGB(DarkColor(tmpClr,2)));
                  else
                    TGPSolidBrush(gBrush).SetColor(ColorToARGB(tmpClr));
                  end;
                  if Rounded then
                  begin
                    if bRect.Right>bRect.Left then
                    begin
                      tmpRect:=bRect; inflateRect(tmpRect,-FrameWidth div 2-1,FrameWidth+1); Inc(tmpRect.Right,FrameWidth+2);
                      GDIPCanvas.ExcludeClip(MakeRect(tmpRect));
                    end;
                    GDIPCanvas.FillPath(gBrush,RPath);
                    if FrameProgress then
                    begin
                      gPen.SetColor(ColorToARGB(DarkColor(tmpClr,15)));
                      gPen.SetWidth(FrameWidth);
                      GDIPCanvas.DrawPath(gPen,RPath);
                      GDIPCanvas.SetClip(RC);
                      GDIPCanvas.DrawLine(gPen,vRect.Right,vRect.Top-1,vRect.Right,vRect.Bottom+1);
                    end;
                    GDIPCanvas.ResetClip;
                  end
                  else
                  begin
                    GDIPCanvas.FillRectangle(gBrush,MakeRect(vRect));
                    if FrameProgress then
                    begin
                      gPen.SetColor(ColorToARGB(DarkColor(tmpClr,15)));
                      gPen.SetWidth(FrameWidth);
                      GDIPCanvas.DrawRectangle(gPen,MakeRect(vRect));
                    end;
                  end;
                end;

                if ShowOverMaxIndicator and ((percent<0) or (percent>1)) then
                begin
                  if percent>1 then
                    j:=cRect.Right-Round((cRect.Right-cRect.Left)*(percent-1))
                  else
                    j:=cRect.Left+Round((cRect.Right-cRect.Left)*(-percent));
                  if (j>cRect.Left) and (j<cRect.Right) then
                  begin
                    GDIPCanvas.DrawLine(sdPen,j-1,cRect.Top,j-1,cRect.Bottom);
                    GDIPCanvas.DrawLine(slPen,j,cRect.Top,j,cRect.Bottom);
                  end;
                end;

                // draw frame
                if (FrameColor<>clNone) and (FrameWidth>0) and ((not FrameProgress) or (vRect.Right<cRect.Right)) then
                begin
                  gPen.SetColor(ColorToARGB(frmClr));
                  gPen.SetWidth(FrameWidth);
                  if FrameProgress and (vRect.Right>vRect.Left) then
                  begin
                    tmpRect:=vRect; inflateRect(tmpRect,FrameWidth,FrameWidth); Dec(tmpRect.Right,FrameWidth div 2);
                    GDIPCanvas.ExcludeClip(MakeRect(tmpRect));
                  end;
                  if Rounded then
                    GDIPCanvas.DrawPath(gPen,RPath)
                  else
                    GDIPCanvas.DrawRectangle(gPen,MakeRect(cRect));
                  GDIPCanvas.ResetClip;
                end;

                if ShowValueInBar<>ibvNone then
                begin
                  TargetCanvas.Brush.Style:=bsClear;
                  TargetCanvas.Font:=Font;
                  TargetCanvas.Font.Color:=invClr;
                  inflateRect(cRect,-4*FSpacing,0);
                  Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DrawTextAlignments[Col.Alignment]);
                  case ShowValueInBar of
                    ibvValue:tmpStr:=GetValueFormatted(Item.Value);
                    ibvPercent:tmpStr:=IntToStr(Round(percent*100))+'%';
                  end;
                  if Assigned(OnDrawCell) then
                    OnDrawCell(Self, TargetCanvas, Item, c, cellRect, tmpStr);
                  if Length(tmpStr)>0 then
                    FDrawTextFunc(TargetCanvas.Handle, tmpStr, cRect, Flags, TargetCanvas.Font.Color, GlowSize);
                end;

                if Assigned(RPath) then
                  RPath.Free;
                if Assigned(RC) then
                  RC.Free;
              end
              else
              begin
                InflateRect(cRect,-FSpacing,0);
                TargetCanvas.Brush.Style:=bsClear;
                TargetCanvas.Font:=Font;
                if not eb then
                  TargetCanvas.Font.Color:=GrayScaleColor(Font.Color);
                Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DrawTextAlignments[Col.Alignment]);
                case Col.Kind of
                  pgcCaption:tmpStr:=Item.Caption;
                  pgcDifference:tmpStr:=GetValueFormatted(Item.MaxValue-Item.Value);
                  pgcMinValue:tmpStr:=GetValueFormatted(Item.MinValue);
                  pgcMaxValue:tmpStr:=GetValueFormatted(Item.MaxValue);
                  pgcValue:tmpStr:=GetValueFormatted(Item.Value);
                  pgcPercent:tmpStr:=IntToStr(Round(percent*100))+'%';
                else
                  tmpStr:='';
                end;
                if Assigned(OnDrawCell) then
                  OnDrawCell(Self, TargetCanvas, Item, c, cellRect, tmpStr);
                if Length(tmpStr)>0 then
                  FDrawTextFunc(TargetCanvas.Handle, tmpStr, cRect, Flags, TargetCanvas.Font.Color, GlowSize);
              end;
            end;
          end;
        end;
      end;
    finally
      sdPen.Free;
      slPen.Free;
      gBrush.Free;
      gbBrush.Free;
      gPen.Free;
    end;
  end;
end;

procedure TDecoProgressGridPlus.Paint;
var
  aRect,cRect: TRect;
  gClipRect: TGPRect;
begin
  if FAntiAliasing {$IF CompilerVersion < 23} and UseGDIP {$IFEND} then
  begin
    aRect := ClientRect;
    if FRebuildNeeded then
      Rebuild(aRect);
    cRect := Canvas.ClipRect;

    if ParentBackground then
    begin
      if {$IF CompilerVersion >= 23} StyleServices.Enabled {$ELSE} StyleServices.ThemesEnabled {$IFEND} then
        {$IF CompilerVersion >= 20}
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

      if Assigned(OnBeforePaint) then
        OnBeforePaint(Self,Canvas);

      if ShowCaption then
        DrawCaption(Canvas);

      if ShowColumnsHeader then
        DrawColumnsHeader(Canvas);

      DrawGrid(Canvas);

      DrawItems(Canvas);

      GDIPCanvas.SetClip(gClipRect);
      if Assigned(OnAfterPaint) then
        OnAfterPaint(Self,Canvas);

    finally
      GDIPCanvas.Free;
      GDIPCanvas:=nil;
    end;

    if FActive and ShowFocusRect and Assigned(SelectedItem) then
    begin
      aRect:=SelectedItem.ItemRect;
      Inc(aRect.Bottom);
      Canvas.Brush.Style:=bsSolid;
      Canvas.Pen.Style:=psSolid;
      windows.DrawFocusRect(Canvas.Handle,aRect);
    end;

  end
  else
    inherited;
end;

procedure TDecoProgressGridPlus.PrintTo(TargetCanvas:TCanvas; Dest:TRect; AsEMF:Boolean=False);
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

      if Assigned(OnBeforePaint) then
        OnBeforePaint(Self,TargetCanvas);

      if ShowCaption then
        DrawCaption(TargetCanvas);

      if ShowColumnsHeader then
        DrawColumnsHeader(TargetCanvas);

      DrawGrid(TargetCanvas);

      DrawItems(TargetCanvas);

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

end.
