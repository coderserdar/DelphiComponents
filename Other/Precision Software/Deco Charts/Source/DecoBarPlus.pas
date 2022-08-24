{------------------------------------------------------------------------------
  DecoBarPlus.pas

  DecoCharts for VCL

  written by  Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  Purpose:    TDecoBarPlus is a descendant of standard TCustomDecoBar component,
              but it uses the GDI+ library for antialiased and composited drawing,
              that enhances the look and the feel of resulting chart.

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

{ This unit contains TDecoBarPlus component declaration and implementation. }
unit DecoBarPlus;

interface

uses
  Classes, Windows, Messages, Graphics, Controls, Forms, SysUtils,
  {$IF CompilerVersion >= 23}
  Winapi.GDIPAPI, Winapi.GDIPOBJ, Winapi.GDIPUTIL,
  {$ELSE}
  xGDIPAPI, xGDIPOBJ, xGDIPUTIL,
  {$IFEND}
  DecoCommon, DecoGDIP, DecoBar;

type
  { A descendant of standard TCustomDecoBar component, but it uses the GDI+ library
    for antialiased and composited drawing, that enhances the look and the feel of resulting chart. }
  TDecoBarPlus = class(TCustomDecoBar)
  private
    FRenderCompositingMode: TRenderCompositingMode;
    FRenderSmoothingMode: TRenderSmoothingMode;
    FRenderTextMode: TRenderTextMode;
    FAntiAliasing: boolean;
    {$IF CompilerVersion >= 19}
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    {$IFEND}
  protected
    // This method implements the drawing of texts and tree-lines using the GDI+ library.
    procedure DrawItemText(TargetCanvas:TCanvas; Item:TDecoBarItem); override;
    // This method implements the drawing of bars and items (sections) using the GDI+ library.
    procedure DrawItems(TargetCanvas:TCanvas; L:TDecoBarItems); override;
    // This method implements the drawing of the chart legend using the GDI+ library.
    procedure DrawLegend(TargetCanvas:TCanvas; L:TDecoBarItems); override;
    // Base method for drawing the component, but overrided to draw using the GDI+ library
    procedure Paint; override;
    procedure SetRenderCompositingMode(Value: TRenderCompositingMode); virtual;
    procedure SetRenderSmoothingMode(Value: TRenderSmoothingMode); virtual;
    procedure SetRenderTextMode(Value: TRenderTextMode); virtual;
    procedure SetAntiAliasing(Value: boolean); virtual;
  public
    { GDIPCanvas variable refers to the GDI+ drawing object (a canvas), that is used to render the component using GDI+ library.
      It is valid only during the painting process. You can refer to it inside your custom drawing handlers, such as OnBeforePaint, OnAfterPaint, etc. }
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
    property AllowExpandingNodes;
    { See @inherited }
    property Anchors;
    { See @inherited }
    property AutoCalcValues;
    { See @inherited }
    property AutoColors;
    { See @inherited }
    property AutoSize;
    { See @inherited }
    property BarSize;
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
    property CaptionFormat;
    { See @inherited }
    property Color;
    { See @inherited }
    property Constraints;
    { See @inherited }
    property Ctl3D;
    { See @inherited }
    property CutExpandedBar;
    { See @inherited }
    property DarkenCollapsedBars;
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
    property FrameWidth;
    { See @inherited }
    property GlowSize;
    { See @inherited }
    property Gradient;
    { See @inherited }
    property HoverCursor;
    { See @inherited }
    property Items;
    { See @inherited }
    property ItemTextFormat;
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
    property OnCollapsed;
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
    property OnEndDrag;
    { See @inherited }
    property OnEnter;
    { See @inherited }
    property OnExit;
    { See @inherited }
    property OnExpanded;
    { See @inherited }
    property OnExpanding;
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
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    { See @inherited }
    property OnSelectItem;
    property OnStartDrag;
    {$IF CompilerVersion >= 20}
    // A padding for the component
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
    property ShowCaption;
    { See @inherited }
    property ShowHint;
    { See @inherited }
    property ShowLegend;
    { See @inherited }
    property StripSize;
    { See @inherited }
    property TabOrder;
    { See @inherited }
    property TabStop;
    { See @inherited }
    property TreeLinesColor;
    { See @inherited }
    property TreeLinesStyle;
    { See @inherited }
    property Visible;
    { If False, the GDI+ library is not used for drawing even if it is available }
    property AntiAliasing: boolean read FAntiAliasing write SetAntiAliasing default True;
  end;

implementation

uses
  Themes, UxTheme, {$IF CompilerVersion >= 19} DwmApi, {$IFEND} DecoGDI
  {$IF CompilerVersion >= 24}, System.Types, System.UITypes {$IFEND};

{ TDecoBarPlus }

constructor TDecoBarPlus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  GDIPCanvas := nil;
  FRenderSmoothingMode := rsmAntiAlias;
  FRenderTextMode := rtmDefault;
  FRenderCompositingMode := rcmDefault;
  FAntiAliasing := True;
  if {$IF CompilerVersion >= 23} StyleServices.Enabled {$ELSE} StyleServices.ThemesEnabled {$IFEND} then
  begin
    if ParentBackground then
      ControlStyle := ControlStyle + [csParentBackground] - [csOpaque];
    if (Win32MajorVersion >= 6) then
      FDrawTextFunc := DoDrawThemeTextEx
  end;
end;

procedure TDecoBarPlus.SetRenderSmoothingMode(Value: TRenderSmoothingMode);
begin
  if Value<>FRenderSmoothingMode then
  begin
    FRenderSmoothingMode:=Value;
    Invalidate;
  end;
end;

procedure TDecoBarPlus.SetRenderTextMode(Value: TRenderTextMode);
begin
  if Value<>FRenderTextMode then
  begin
    FRenderTextMode:=Value;
    Invalidate;
  end;
end;

procedure TDecoBarPlus.SetRenderCompositingMode(Value: TRenderCompositingMode);
begin
  if Value<>FRenderCompositingMode then
  begin
    FRenderCompositingMode:=Value;
    Invalidate;
  end;
end;

procedure TDecoBarPlus.SetAntiAliasing(Value: boolean);
begin
  if Value<>FAntiAliasing then
  begin
    FAntiAliasing:=Value;
    Invalidate;
  end;
end;

{$IF CompilerVersion >= 19}
procedure TDecoBarPlus.WMPaint(var Message: TWMPaint);
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

procedure TDecoBarPlus.DrawItemText(TargetCanvas:TCanvas; Item:TDecoBarItem);
var
  txtRect,tmpRect:TRect;
  tmpStr:string;
  Flags:Longint;
  rcs,scs:Integer;
  pl,pr:array[0..3] of TGPPoint;
  gPen:TGPPen;
begin
  if GDIPCanvas=nil then
    inherited
  else
  if Assigned(Item) then
  begin
    txtRect:=Item.ItemRect;
    txtRect.Top:=txtRect.Bottom+FSpacing;
    txtRect.Bottom:=Item.Items.BarsRect.Top-FSpacing div 2;
    TargetCanvas.Brush.Style:=bsClear;
    if Enabled then
      TargetCanvas.Font.Color:=Font.Color
    else
      TargetCanvas.Font.Color:=GrayScaleColor(Font.Color);
    TargetCanvas.Font.Style:=Font.Style;
    Flags := DT_SINGLELINE or DT_VCENTER or DT_CENTER;
    tmpStr:=GetItemTextFormatted(Item);
    tmpRect:=txtRect;
    DoDrawNormalText(TargetCanvas.Handle, tmpStr, tmpRect, Flags or DT_NOCLIP or DT_CALCRECT); // Do not use FDrawTextFunc here, due to a correct measuring
    txtRect.Right:=tmpRect.Right; txtRect.Left:=tmpRect.Left;
    offsetRect(txtRect,-((txtRect.Right-txtRect.Left)-(Item.ItemRect.Right-Item.ItemRect.Left)) div 2,0);
    if txtRect.Right>FBarsRect.Right-FSpacing then offsetRect(txtRect,-(txtRect.Right-(FBarsRect.Right-FSpacing)),0);
    if txtRect.Left<FBarsRect.Left+FSpacing then
    begin
      offsetRect(txtRect,(FBarsRect.Left+FSpacing)-txtRect.Left,0);
      if txtRect.Right>FBarsRect.Right-FSpacing then
        txtRect.Right:=FBarsRect.Right-FSpacing;
    end;

    if Item.Expanded and (TreeLinesColor<>clNone) and (TreeLinesStyle<>tlsNone) then
    begin
      if Enabled then
        gPen:=TGPPen.Create(ColorToARGB(TreeLinesColor))
      else
        gPen:=TGPPen.Create(ColorToARGB(GrayScaleColor(TreeLinesColor)));
      if FrameWidth=0 then
        gPen.SetWidth(1)
      else
        gPen.SetWidth(FrameWidth);
      pl[0].x:=Item.ItemRect.Left; if pl[0].x>Item.Items.BarsRect.Left then Dec(pl[0].x);
      pl[0].y:=Item.ItemRect.Bottom+2; if CutExpandedBar and (pl[0].x<=Item.Items.BarsRect.Left) then Dec(pl[0].y,BarSize div 3);

      pl[1].x:=pl[0].x; pl[1].y:=Item.ItemRect.Bottom+FSpacing-1;
      pl[2].x:=Item.Items.BarsRect.Left; pl[2].y:=pl[1].y;
      pl[3].x:=pl[2].x; pl[3].y:=Item.Items.BarsRect.Top-2;

      pr[0].x:=Item.ItemRect.Right; if pr[0].x>Item.Items.BarsRect.Right-1 then pr[0].x:=Item.Items.BarsRect.Right-1;
      pr[0].y:=Item.ItemRect.Bottom+2; if CutExpandedBar and (pr[0].x>=Item.Items.BarsRect.Right-1) then Dec(pr[0].y,BarSize div 3);

      pr[1].x:=pr[0].x; pr[1].y:=pl[1].y;
      pr[2].x:=Item.Items.BarsRect.Right-1; pr[2].y:=pl[2].y;
      pr[3].x:=pr[2].x; pr[3].y:=pl[3].y;

      case TreeLinesStyle of
        tlsRectangular:
          begin
            GDIPCanvas.DrawLines(gPen,PGPPoint(@pl[0]),4);
            GDIPCanvas.DrawLines(gPen,PGPPoint(@pr[0]),4);
          end;
        tlsRoundedRect:
          begin
            if pl[0].x<FBarsRect.Left+1 then
              GDIPCanvas.DrawLines(gPen,PGPPoint(@pl[0]),4)
            else
            begin
              rcs:=_GetRoundedCornersFactor;
              scs:=FSpacing;
              if pl[1].x-scs-rcs<pl[2].x then
              begin
                scs:=(pl[1].x-pl[2].x) div 4;
                rcs:=scs*3;
              end;
              GDIPCanvas.DrawBezier(gPen,pl[0],pl[1],pl[1],MakePoint(pl[1].x-scs,pl[1].y));
              GDIPCanvas.DrawLine(gPen,pl[1].x-scs,pl[1].y,pl[2].x+rcs,pl[2].y);
              GDIPCanvas.DrawBezier(gPen,MakePoint(pl[2].x+rcs,pl[2].y),pl[2],pl[2],MakePoint(pl[2].x,pl[2].y+rcs));
              GDIPCanvas.DrawLine(gPen,MakePoint(pl[2].x,pl[2].y+rcs),pl[3]);
            end;
            if pr[0].x>FBarsRect.Right-4 then
              GDIPCanvas.DrawLines(gPen,PGPPoint(@pr[0]),4)
            else
            begin
              rcs:=_GetRoundedCornersFactor;
              scs:=FSpacing;
              if pr[1].x+scs+rcs>pr[2].x then
              begin
                scs:=(pr[2].x-pr[1].x) div 4;
                rcs:=scs*3;
              end;
              GDIPCanvas.DrawBezier(gPen,pr[0],pr[1],pr[1],MakePoint(pr[1].x+scs,pr[1].y));
              GDIPCanvas.DrawLine(gPen,pr[1].x+scs,pr[1].y,pr[2].x-rcs,pr[2].y);
              GDIPCanvas.DrawBezier(gPen,MakePoint(pr[2].x-rcs,pr[2].y),pr[2],pr[2],MakePoint(pr[2].x,pr[2].y+rcs));
              GDIPCanvas.DrawLine(gPen,MakePoint(pr[2].x,pr[2].y+rcs),pr[3]);
            end;
          end;
        tlsCurved:
          begin
            if txtRect.Left-FSpacing<Item.ItemRect.Left then pl[1].x:=txtRect.Left-FSpacing;
            if txtRect.Right+FSpacing>Item.ItemRect.Right then
            begin
              pr[1].x:=txtRect.Right+FSpacing; if pr[1].x>FBarsRect.Right-1 then pr[1].x:=FBarsRect.Right-1;
            end;
            pl[2].y:=Item.ItemRect.Bottom; pr[2].y:=pl[2].y;
            GDIPCanvas.DrawBezier(gPen,pl[0],pl[1],pl[2],pl[3]);
            GDIPCanvas.DrawBezier(gPen,pr[0],pr[1],pr[2],pr[3]);
          end;
        tlsStrightLines:
          begin
            if pl[0].x>=FBarsRect.Left+1 then
            begin
              pl[2].x:=txtRect.Left-1;
              if pl[2].x>pl[1].x then pl[2].x:=pl[1].x;
            end;
            if pr[0].x<=FBarsRect.Right-4 then
            begin
              pr[2].x:=txtRect.Right+1;
              if pr[2].x<pr[1].x then pr[2].x:=pr[1].x;
            end;
            GDIPCanvas.DrawLines(gPen,PGPPoint(@pl[0]),4);
            GDIPCanvas.DrawLines(gPen,PGPPoint(@pr[0]),4);
          end;
      end;
    end;

    FDrawTextFunc(TargetCanvas.Handle, tmpStr, txtRect, Flags or DT_END_ELLIPSIS, TargetCanvas.Font.Color, GlowSize)
  end;
end;

procedure TDecoBarPlus.DrawItems(TargetCanvas:TCanvas; L:TDecoBarItems);
var
  i,j:Integer;
  Item,expItem:TDecoBarItem;
  tmpClr:TColor;
  rcs:Integer;
  clIdx:Integer;
  tmpRect:TRect;

  RPath: TGPGraphicsPath;
  RC: TGPRegion;
  gBrush: TGPBrush;
  gPen,sdPen,slPen: TGPPen;
  {$IF CompilerVersion >= 23}
  oldSM: SmoothingMode;
  {$ELSE}
  oldSM: Integer;
  {$IFEND}
  alpha: Byte;
begin
  if GDIPCanvas=nil then
    inherited
  else
  begin
    if L.Count>0 then
    begin
      expItem:=L.ExpandedItem;
      rcs:=_GetRoundedCornersFactor;
      j:=L.BarsRect.Left+StripSize;
      clIdx:=GetAutoColorIndex(L);

      if Rounded then
      begin
        tmpRect:=L.BarsRect;
        InflateRect(tmpRect,-1,-1);
        RPath := GDIP_RoundRectanglePath(tmpRect, rcs);
        RC := TGPRegion.Create(RPath);
        RPath.Free;
        Dec(tmpRect.Top); Dec(tmpRect.Left);
        RPath := GDIP_RoundRectanglePath(tmpRect, rcs);
        gBrush := TGPSolidBrush.Create(ColorToARGB(Color,255));
        gPen := TGPPen.Create(ColorToARGB(FrameColor,255));
        sdPen := TGPPen.Create(ColorToARGB(clBlack,56));
        slPen := TGPPen.Create(ColorToARGB(clWhite,32));
        try
          for i:=0 To L.Count-1 do
          begin
            Item:=L.Items[i];
            if Item.Color=clNone then
            begin
              alpha:=24;
              tmpClr:=BrightColor(Font.Color,23);
            end
            else
            begin
              alpha:=255;
              if clIdx>=0 then
              begin
                if clIdx>=Length(AutoColorPalette) then
                  clIdx:=0;
                if Item.Expanded or (expItem=nil) or (not DarkenCollapsedBars) then
                  tmpClr:=AutoColorPalette[clIdx]
                else
                  tmpClr:=ModColorHLS(AutoColorPalette[clIdx],0,-30,-10);
                Inc(clIdx);
              end
              else
              if Item.Expanded or (expItem=nil) or (not DarkenCollapsedBars) then
                tmpClr:=Item.Color
              else
                tmpClr:=ModColorHLS(Item.Color,0,-30,-10);
            end;
            if not Enabled then
              tmpClr:=GrayScaleColor(tmpClr);

            tmpRect:=Item.ItemRect;
            InflateRect(tmpRect,0,2);
            GDIPCanvas.SetClip(MakeRect(tmpRect));
            if CutExpandedBar and (Item=expItem) then
              GDIPCanvas.ExcludeClip(MakeRect(expItem.ItemRect.Left,expItem.ItemRect.Bottom-BarSize div 3,
                expItem.ItemRect.Right-expItem.ItemRect.Left,BarSize div 3+FrameWidth+1));

            if Gradient then
            begin
              gBrush.Free;
              gBrush := TGPLinearGradientBrush.Create(GDIP_RectF(Item.ItemRect),
                ColorToARGB(BrightColor(tmpClr,15),alpha),ColorToARGB(DarkColor(tmpClr,8),alpha),LinearGradientModeVertical);
            end
            else
              TGPSolidBrush(gBrush).SetColor(ColorToARGB(tmpClr,alpha));
            GDIPCanvas.FillPath(gBrush,RPath);
            if StripSize>0 then
            begin
              GDIPCanvas.SetClip(RC);
              if CutExpandedBar and (Item=expItem) then
                GDIPCanvas.ExcludeClip(MakeRect(expItem.ItemRect.Left,expItem.ItemRect.Bottom-BarSize div 3,
                  expItem.ItemRect.Right-expItem.ItemRect.Left,BarSize div 3+FrameWidth+1));
              while j<=Item.ItemRect.Right do
              begin
                GDIPCanvas.DrawLine(sdPen,j-1,Item.ItemRect.Top+1,j-1,Item.ItemRect.Bottom-2);
                GDIPCanvas.DrawLine(slPen,j,Item.ItemRect.Top+1,j,Item.ItemRect.Bottom-2);
                Inc(j,StripSize);
              end;
            end;

          end;
          GDIPCanvas.ResetClip();
          // draw frame
          if (FrameColor<>clNone) and (FrameWidth>0) then
          begin
            if CutExpandedBar and Assigned(expItem) then
              GDIPCanvas.ExcludeClip(MakeRect(expItem.ItemRect.Left,expItem.ItemRect.Bottom-BarSize div 3,
                expItem.ItemRect.Right-expItem.ItemRect.Left,BarSize div 3+1));
            if Enabled then
              gPen.SetColor(ColorToARGB(FrameColor))
            else
              gPen.SetColor(ColorToARGB(GrayScaleColor(FrameColor)));
            gPen.SetWidth(FrameWidth);
            GDIPCanvas.DrawPath(gPen,RPath);
            if CutExpandedBar and Assigned(expItem) then
              GDIPCanvas.ResetClip();
          end;
        finally
          RPath.Free;
          RC.Free;
          gBrush.Free;
          gPen.Free;
          sdPen.Free;
          slPen.Free;
        end;
      end
      else
      begin
        if CutExpandedBar and Assigned(expItem) then
          GDIPCanvas.ExcludeClip(MakeRect(expItem.ItemRect.Left,expItem.ItemRect.Bottom-BarSize div 3,
            expItem.ItemRect.Right-expItem.ItemRect.Left,BarSize div 3+1));
        gBrush := TGPSolidBrush.Create(ColorToARGB(Color,255));
        gPen := TGPPen.Create(ColorToARGB(FrameColor,255));
        sdPen := TGPPen.Create(ColorToARGB(clBlack,56));
        slPen := TGPPen.Create(ColorToARGB(clWhite,32));
        oldSM:=GDIPCanvas.GetSmoothingMode;
        GDIPCanvas.SetSmoothingMode(SmoothingModeNone);
        try
          for i:=0 To L.Count-1 do
          begin
            Item:=L.Items[i];
            if Item.Color=clNone then
            begin
              alpha:=24;
              tmpClr:=BrightColor(Font.Color,23);
            end
            else
            begin
              alpha:=255;
              if clIdx>=0 then
              begin
                if clIdx>=Length(AutoColorPalette) then
                  clIdx:=0;
                if Item.Expanded or (expItem=nil) or (not DarkenCollapsedBars) then
                  tmpClr:=AutoColorPalette[clIdx]
                else
                  tmpClr:=ModColorHLS(AutoColorPalette[clIdx],0,-30,-10);
                Inc(clIdx);
              end
              else
              if Item.Expanded or (expItem=nil) or (not DarkenCollapsedBars) then
                tmpClr:=Item.Color
              else
                tmpClr:=ModColorHLS(Item.Color,0,-30,-10);
            end;
            if not Enabled then
              tmpClr:=GrayScaleColor(tmpClr);

            if Gradient then
            begin
              gBrush.Free;
              gBrush := TGPLinearGradientBrush.Create(GDIP_RectF(Item.ItemRect),
                ColorToARGB(BrightColor(tmpClr,15),alpha),ColorToARGB(DarkColor(tmpClr,8),alpha),LinearGradientModeVertical);
            end
            else
              TGPSolidBrush(gBrush).SetColor(ColorToARGB(tmpClr,alpha));
            GDIPCanvas.FillRectangle(gBrush,MakeRect(Item.ItemRect));
            if StripSize>0 then
            begin
              while j<=Item.ItemRect.Right do
              begin
                GDIPCanvas.DrawLine(sdPen,j-1,Item.ItemRect.Top+1,j-1,Item.ItemRect.Bottom-2);
                GDIPCanvas.DrawLine(slPen,j,Item.ItemRect.Top+1,j,Item.ItemRect.Bottom-2);
                Inc(j,StripSize);
              end;
            end;
          end;
          // draw frame
          if (FrameColor<>clNone) and (FrameWidth>0) then
          begin
            if Enabled then
              gPen.SetColor(ColorToARGB(FrameColor))
            else
              gPen.SetColor(ColorToARGB(GrayScaleColor(FrameColor)));
            gPen.SetWidth(FrameWidth);
            tmpRect:=L.BarsRect; Dec(tmpRect.Right); Dec(tmpRect.Bottom);
            GDIPCanvas.DrawRectangle(gPen,MakeRect(tmpRect));
          end

        finally
          GDIPCanvas.SetSmoothingMode(oldSM);
          gBrush.Free;
          gPen.Free;
          if CutExpandedBar and Assigned(expItem) then
            GDIPCanvas.ResetClip();
        end;
      end;

      if expItem<>nil then
      begin
        // draw expanded item text and lines
        DrawItemText(TargetCanvas,expItem);
        // go into the next level
        DrawItems(TargetCanvas,expItem.Items);
      end;
    end;
  end;
end;

procedure TDecoBarPlus.DrawLegend(TargetCanvas:TCanvas; L:TDecoBarItems);
var
  i,ls:Integer;
  Item:TDecoBarItem;
  tmpClr:TColor;
  mRect,txtRect:TRect;
  Flags: Longint;
  rcs,clIdx:Integer;

  gBrush: TGPBrush;
  gPen: TGPPen;
  alpha: Byte;
begin
  if GDIPCanvas=nil then
    inherited
  else
  begin
    gBrush := TGPSolidBrush.Create(ColorToARGB(Color,255));
    gPen := TGPPen.Create(ColorToARGB(FrameColor,255));
    try
      TargetCanvas.Font := Font;
      if not Enabled then
        TargetCanvas.Font.Color:=GrayScaleColor(Font.Color);
      Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_LEFT);
      rcs := _GetRoundedCornersFactor;
      clIdx:=GetAutoColorIndex(L);

      for i:=0 To L.Count-1 do
      begin
        Item:=L.Items[i];
        txtRect:=Item.ItemLegRect;
        mRect:=Item.ItemLegRect;
        ls:=txtRect.Bottom-txtRect.Top;
        if Flags and DT_RIGHT = DT_RIGHT then
        begin
          Dec(txtRect.Right,ls + FSpacing div 2);
          mRect.Left:=mRect.Right-ls;
          inflateRect(mRect,-ls div 6,-ls div 6);
          Inc(mRect.Left);
        end
        else
        begin
          Inc(txtRect.Left,ls + FSpacing div 2);
          mRect.Right:=mRect.Left+ls;
          inflateRect(mRect,-ls div 6,-ls div 6);
          Dec(mRect.Right);
        end;
        Dec(mRect.Bottom);

        if Item.Color=clNone then
        begin
          alpha:=24;
          tmpClr:=BrightColor(Font.Color,23);
        end
        else
        begin
          alpha:=255;
          if clIdx>=0 then
          begin
            if clIdx>=Length(AutoColorPalette) then
              clIdx:=0;
            tmpClr:=AutoColorPalette[clIdx];
            Inc(clIdx);
          end
          else
            tmpClr:=Item.Color;
        end;
        if not Enabled then
          tmpClr:=GrayScaleColor(tmpClr);

        if Gradient then
        begin
          gBrush.Free;
          gBrush := TGPLinearGradientBrush.Create(GDIP_RectF(Item.ItemLegRect),
            ColorToARGB(BrightColor(tmpClr,15),alpha),ColorToARGB(DarkColor(tmpClr,8),alpha),LinearGradientModeVertical);
        end
        else
          TGPSolidBrush(gBrush).SetColor(ColorToARGB(tmpClr,alpha));
        tmpClr:=DarkColor(tmpClr,23);
        gPen.SetColor(ColorToARGB(tmpClr));
        gPen.SetWidth(FrameWidth);

        if Rounded then
          GDIP_RoundRectangle(GDIPCanvas, mRect, rcs div 2, gPen, gBrush)
        else
        begin
          GDIPCanvas.FillRectangle(gBrush,MakeRect(mRect));
          GDIPCanvas.DrawRectangle(gPen,MakeRect(mRect));
        end;

        TargetCanvas.Brush.Style:=bsClear;
        TargetCanvas.Font.Color:=tmpClr;
        if (Item=HoverItem) and (FIsLegendHovered) then
        begin
          TargetCanvas.Font.Style:=Font.Style+[fsUnderline];
          FDrawTextFunc(TargetCanvas.Handle, GetItemTextFormatted(Item), txtRect, Flags, tmpClr, GlowSize);
          TargetCanvas.Font.Style:=Font.Style;
        end
        else
          FDrawTextFunc(TargetCanvas.Handle, GetItemTextFormatted(Item), txtRect, Flags, tmpClr, GlowSize)
      end;
    finally
      gPen.Free;
      gBrush.Free;
    end;
  end;
end;

procedure TDecoBarPlus.Paint;
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

      Canvas.Font := Font;
      DrawItems(Canvas, Items);

      if ShowLegend then
        DrawLegend(Canvas, FExpandedItems);

      // draw hover or selected item text at bottom of the last expanded bar
      if Assigned(HoverItem) and (not FIsLegendHovered) and (not HoverItem.Expanded) and (HoverItem.Collection=FExpandedItems) then
        DrawItemText(Canvas,HoverItem)
      else
      if FShowingFocusRect and FActive and Assigned(SelectedItem) and (not SelectedItem.Expanded) and (SelectedItem.Collection=FExpandedItems) then
        DrawItemText(Canvas,SelectedItem);

      GDIPCanvas.SetClip(gClipRect);
      if Assigned(OnAfterPaint) then
        OnAfterPaint(Self,Canvas);

    finally
      GDIPCanvas.Free;
      GDIPCanvas:=nil;
    end;

    if FActive and FShowingFocusRect and Assigned(SelectedItem) then
    begin
      aRect:=SelectedItem.ItemRect;
      inflateRect(aRect,0,2);
      Canvas.Brush.Style:=bsSolid;
      Canvas.Pen.Style:=psSolid;
      windows.DrawFocusRect(Canvas.Handle,aRect);
    end;

  end
  else
    inherited;
end;

procedure TDecoBarPlus.PrintTo(TargetCanvas:TCanvas; Dest:TRect; AsEMF:Boolean=False);
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

      TargetCanvas.Font := Font;
      DrawItems(TargetCanvas,Items);

      if ShowLegend then
        DrawLegend(TargetCanvas,FExpandedItems);

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
