{------------------------------------------------------------------------------
  DecoRatingStars.pas

  DecoCharts for VCL

  written by  Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  Purpose:    TDecoRatingStars is a simple interactive rating stars component.

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

{ This unit contains TCustomDecoRatingStars and TDecoRatingStars components declaration and implementation. }
unit DecoRatingStars;

interface

uses
  Classes, Windows, Messages, Graphics, Controls, SysUtils, ImgList, DecoCommon;

type
  TRatingStarShape = (stsStar, stsCircle, stsSquare, stsRoundSquare);

  { A base class for all DecoRatingStars visual component implementations. See also TDecoRatingStarsPlus. }
  TCustomDecoRatingStars = class(TCustomControl)
  private
    FMinValue: Extended;
    FMaxValue: Extended;
    FRating: Extended;
    FParentBackgroundSet: Boolean;
    FOnAfterPaint: TDecoPaintEvent;
    FOnBeforePaint: TDecoPaintEvent;
    FShowFocusRect: Boolean;
    FImagesChangeLink: TChangeLink;
    FImages: TCustomImageList;

    FBackColor: TColor;
    FFrameColor: TColor;
    FFrameWidth: Integer;
    FFillColor: TColor;
    FHoverColor: TColor;
    FStarCount: Integer;
    FStarSize: Integer;
    FStarMargin: Integer;
    FStarShape: TRatingStarShape;
    FRatingsCount: Integer;
    FInteractive:Boolean;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMWantSpecialKey(var Message: TWMKey); message CM_WANTSPECIALKEY;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetOnAfterPaint(Value: TDecoPaintEvent);
    procedure SetOnBeforePaint(Value: TDecoPaintEvent);
    procedure SetMinValue(Value: Extended); virtual;
    procedure SetMaxValue(Value: Extended); virtual;
    procedure SetRating(Value: Extended); virtual;
    procedure SetColorVal(Index: Integer; Value: TColor);
    procedure SetIntVal(Index: Integer; Value: Integer);
    procedure SetBoolVal(Index: Integer; Value: Boolean);
    procedure SetRatingImages(Value: TCustomImageList);
    procedure ImageListChange(Sender: TObject);
    procedure SetShape(Value: TRatingStarShape);
  protected
    // A temporary variable that indicates if the component has a focus
    FActive:Boolean;
    FHoveredRating: Extended;
    FMouseX:Integer;
    // Temporary variable, that indicates a drawing onto the vector based canvas (ie. TMetafileCanvas). It is used in PrintTo method.
    _EMFExporting:Boolean;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetParentBackground(Value: Boolean); override;
    function GetFocusRect:TRect; virtual;
    procedure DrawRating(TargetCanvas:TCanvas; Dest: TRect); virtual;
    procedure DrawFocus(TargetCanvas:TCanvas; aRect:TRect); virtual;
    { Set this property to True to draw the component's background as transparent.
      It is not published in components that use a standard GDI drawing methods, but in the descendants (such as TDecoRatingStarsPlus) it is. }
    property ParentBackground stored FParentBackgroundSet default False;
    // Base method for drawing the component, that calls other methods to draw the rating stars.
    procedure Paint; override;
    { An event handler that allows you to perform a custom drawing on the component surface after the default drawing is finished. }
    property OnAfterPaint: TDecoPaintEvent read FOnAfterPaint write SetOnAfterPaint;
    { An event handler that allows you to perform a custom drawing on the component surface before the default drawing will start. }
    property OnBeforePaint: TDecoPaintEvent read FOnBeforePaint write SetOnBeforePaint;
    // If True, a focus rectangle will be drawn.
    property ShowFocusRect: Boolean Index 2 read FShowFocusRect write SetBoolVal default True;
    // If True, the control will accept mouse clicks on stars to accumulate user ratings. See also Rating and RatingsCount
    property Interactive: Boolean Index 1 read FInteractive write SetBoolVal default False;
    { Number of performed ratings. Not actively used until Interactive property is True. }
    property RatingsCount: Integer Index 2 read FRatingsCount write SetIntVal default 0;

    { A minimum rating value }
    property MinValue: Extended read FMinValue write SetMinValue;
    { A maximum rating value }
    property MaxValue: Extended read FMaxValue write SetMaxValue;
    { ImageList that represents rating stars.
      ImageIndex 0 corresponds to "full" star, 1 to "half" star and 2 to "empty" star. You can ommit "half" star image,
      so the image list can contain only two images (for full and empty stars). }
    property Images: TCustomImageList read FImages write SetRatingImages;

    // Background color of empty rating star. You can set the value to clNone, to make the star transparent. Ignored when using Images property.
    property BackColor: TColor Index 3 read FBackColor write SetColorVal default clBtnHighlight;
    // Frame color of empty rating star. You can set the value to clNone, to remove the frame. Ignored when using Images property.
    property FrameColor: TColor Index 4 read FFrameColor write SetColorVal default clBtnFace;
    // Width of the rating star frame. See also FrameColor. Ignored when using Images property.
    property FrameWidth: Integer Index 4 read FFrameWidth write SetIntVal default 1;
    // Color of the full rating star. Ignored when using Images property.
    property FillColor: TColor Index 7 read FFillColor write SetColorVal default $00BFFF;
    // Color of the full rating star when the control is hovered. Ignored when using Images property. See also Interactive property.
    property HoverColor: TColor Index 5 read FHoverColor write SetColorVal default $00EFBF;
    { A size of rating star. Rating stars will be centered inside the control's client area. Ignored when using Images property. }
    property StarSize: Integer Index 10 read FStarSize write SetIntVal default 9;
    { Number of stars in the presented rating control. }
    property StarCount: Integer Index 3 read FStarCount write SetIntVal default 5;
    { A margin between the stars. }
    property StarMargin: Integer Index 6 read FStarMargin write SetIntVal default 1;
    { Currently hovered rating (if Interactive property is True). }
    property HoveredRating: Extended read FHoveredRating;
  public
    // Constructor of the component. You can call it directly, when creating the component in run-time.
    constructor Create(AOwner: TComponent); override;
    // Component destructor. You don't have to call it directly, unless you want to destroy the component at run-time.
    destructor Destroy; override;
    { Allows you to paint the component to the passed TargetCanvas. Dest is a destination rectangle, where the component will be drawn.
      Set AsEMF paramenter to True, when the TargetCanvas belongs to a vector based device (ie TMetafileCanvas).
      Refer to the main DecoBar demo project, to see how to use this method to save/export the grid into a graphic formats. }
    procedure PrintTo(TargetCanvas:TCanvas; Dest:TRect; AsEMF:Boolean=False); virtual;
    // Temporary property, that indicates if the component has a focus
    property Active: Boolean read FActive;
  published
    // Background color of the control. See also ParentBackground.
    property Color default clWhite;
    // Takes the same background color as its parent control (when not using the ParentBackground mode)
    property ParentColor default False;
    { Current rating value. }
    property Rating: Extended read FRating write SetRating;
    // A shape of the rating star. Ignored when using Images property.
    property StarShape: TRatingStarShape read FStarShape write SetShape default stsStar;
  end;


  { TDecoRatingStars is a simple interactive rating stars component. }
  TDecoRatingStars = class(TCustomDecoRatingStars)
  public
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

  end;

implementation

{$IF CompilerVersion >= 24}
uses
  System.Types;
{$IFEND}

//////////////////////////////////////////// TCustomDecoRatingStars ////////////////////////////////////////////
constructor TCustomDecoRatingStars.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csOpaque, csDoubleClicks, csReplicatable {$IF CompilerVersion >= 20.0}, csPannable {$IFEND} ];
  FActive:=False;
  FShowFocusRect := True;
  FMinValue := 0;
  FMaxValue := 5;
  FRating := 0;
  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := ImageListChange;

  FStarMargin := 1;
  FBackColor:=clBtnHighlight;
  FFrameColor:=clBtnFace;
  FFrameWidth:=1;
  FFillColor := $00BFFF;
  FHoverColor:=$008FEF;
  FStarCount := 5;

  FStarSize:=9;
  FStarShape:=stsStar;
  FMouseX:=-1;
  FInteractive:=False;
  FRatingsCount:=0;
  FHoveredRating:=0;
  _EMFExporting := False;

  Width := 100;
  Height := 24;
  Color := clWhite;
  ParentBackground := False;
end;

destructor TCustomDecoRatingStars.Destroy;
begin
  FImagesChangeLink.Free;
  inherited;
end;

procedure TCustomDecoRatingStars.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    WindowClass.style := WindowClass.style  and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TCustomDecoRatingStars.Loaded;
begin
  inherited;
end;

procedure TCustomDecoRatingStars.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  Invalidate;
  inherited;
end;

procedure TCustomDecoRatingStars.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TCustomDecoRatingStars.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  FActive:=False;
  Invalidate;
end;

procedure TCustomDecoRatingStars.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  FActive:=True;
  Invalidate;
end;

procedure TCustomDecoRatingStars.CMWantSpecialKey(var Message: TWMKey);
begin
  inherited;
  if (Message.CharCode in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_ESCAPE, VK_HOME, VK_END, VK_NEXT, VK_PRIOR]) then
    Message.Result := 1
  else
  if (Message.CharCode = VK_TAB) and FActive then
    Invalidate;
end;

procedure TCustomDecoRatingStars.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomDecoRatingStars.ImageListChange(Sender: TObject);
begin
  if Sender = Images then
    Invalidate;
end;

procedure TCustomDecoRatingStars.SetRatingImages(Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    if FImages <> nil then
      FImages.UnRegisterChanges(FImagesChangeLink);
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.RegisterChanges(FImagesChangeLink);
      FImages.FreeNotification(Self);
    end;
    Invalidate;
  end;
end;

procedure TCustomDecoRatingStars.SetShape(Value: TRatingStarShape);
begin
  if FStarShape <> Value then
  begin
    FStarShape := Value;
    Invalidate;
  end;
end;

procedure TCustomDecoRatingStars.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Images then
      Images := nil;
  end;
end;

procedure TCustomDecoRatingStars.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FInteractive and (not (csDesigning in ComponentState)) then
  begin
    FMouseX:=X;
    Invalidate;
  end;
  inherited;
end;

procedure TCustomDecoRatingStars.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (csDesigning in ComponentState) then
  begin
    if (not FActive) and HandleAllocated and Visible and (not (ssDouble in Shift)) then
      SetFocus;
    if FInteractive then
    begin
      if FRatingsCount<0 then
        FRatingsCount:=1
      else
        Inc(FRatingsCount);
      Rating:=(Rating*(FRatingsCount-1)+FHoveredRating)/FRatingsCount;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomDecoRatingStars.CMMouseLeave(var Message: TMessage);
begin
  if FInteractive then
  begin
    FMouseX:=-1;
    Invalidate;
  end;
  inherited;
end;

procedure TCustomDecoRatingStars.SetParentBackground(Value: Boolean);
begin
  if Value then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
  FParentBackgroundSet := True;
  inherited;
end;

procedure TCustomDecoRatingStars.SetRating(Value: Extended);
begin
  if Value<>FRating then
  begin
    FRating:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoRatingStars.SetMaxValue(Value: Extended);
begin
  if Value<>FMaxValue then
  begin
    FMaxValue:=Value;
    if FMaxValue<=FMinValue then
      FMinValue:=FMaxValue-1;
    Invalidate;
  end;
end;

procedure TCustomDecoRatingStars.SetMinValue(Value: Extended);
begin
  if Value<>FMinValue then
  begin
    FMinValue:=Value;
    if FMinValue>=FMaxValue then
      FMaxValue:=FMinValue+1;
    Invalidate;
  end;
end;

procedure TCustomDecoRatingStars.SetOnBeforePaint(Value: TDecoPaintEvent);
begin
  FOnBeforePaint:=Value;
  Invalidate;
end;

procedure TCustomDecoRatingStars.SetOnAfterPaint(Value: TDecoPaintEvent);
begin
  FOnAfterPaint:=Value;
  Invalidate;
end;

procedure TCustomDecoRatingStars.SetColorVal(Index: Integer; Value: TColor);
begin
  case Index of
    3:if Value<>FBackColor then begin FBackColor := Value; Invalidate; end;
    4:if Value<>FFrameColor then begin FFrameColor := Value; Invalidate; end;
    5:if Value<>FHoverColor then begin FHoverColor := Value; Invalidate; end;
    7:if Value<>FFillColor then begin FFillColor := Value; Invalidate; end;
  end;
end;

procedure TCustomDecoRatingStars.SetIntVal(Index: Integer; Value: Integer);
begin
  case Index of
    2:if (Value<>FRatingsCount) and (Value>=0) then FRatingsCount := Value;
    3:if Value<>FStarCount then begin FStarCount := Value; Invalidate; end;
    4:if Value<>FFrameWidth then begin FFrameWidth := Value; Invalidate; end;
    6:if Value<>FStarMargin then begin FStarMargin := Value; Invalidate; end;
   10:if Value<>FStarSize then begin FStarSize := Value; Invalidate; end;
  end;
end;

procedure TCustomDecoRatingStars.SetBoolVal(Index: Integer; Value: Boolean);
begin
  case Index of
    1:if Value<>FInteractive then FInteractive:=Value;
    2:if Value<>FShowFocusRect then begin FShowFocusRect:=Value; Invalidate; end;
  end;
end;

function TCustomDecoRatingStars.GetFocusRect:TRect;
begin
  Result := ClientRect;
end;

type
  TStarPoints = array[0..9] of TPoint;

procedure _StarPoints(Rect:TRect; var p:TStarPoints);
var
  i: Integer;
  t, r, sv, sw, sx, sy: Real;
begin
  sx := (Rect.Right-Rect.Left)*0.48;
  sy := (Rect.Bottom-Rect.Top)*0.5;
  if sx > sy then
    sx := sy
  else
    sy := sx;
  sv := Rect.Left + (Rect.Right - Rect.Left) / 2;
  sw := round(Rect.Top + sy);
  for i := 0 To 9 do
  begin
    if ((i and 1) <> 0) then
      r := 1
    else
      r := 0.384;
    t := i * 2 * (PI/10);
    P[i].x := Trunc(sv+sx*r*sin(t));
    P[i].Y := Trunc(sw+sy*r*cos(t));
  end;
end;

procedure TCustomDecoRatingStars.DrawRating(TargetCanvas:TCanvas; Dest: TRect);
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
begin
  RR:=Dest;
  inflateRect(RR,-FStarMargin,0);

  drawImg:=Assigned(FImages) and (FImages.Count>1);
  if drawImg then
  begin
    imW:=FImages.Width;
    sCnt:=FImages.Count;
  end
  else
  begin
    TargetCanvas.Brush.Style:=bsSolid;
    TargetCanvas.Pen.Style:=psSolid;
    TargetCanvas.Pen.Width:=FrameWidth;
    imW:=FStarSize;
    sCnt:=3;
  end;
  if imW>RR.Bottom-RR.Top then
    imW:=RR.Bottom-RR.Top
  else
  if (FStarShape=stsCircle) and (imW<4) then
    imW:=4;
  imH:=imW;
  rcs:=imW div 3;

  RR.Left := RR.Left + (RR.Right-RR.Left - FStarCount*(imW+FStarMargin)) div 2;
  RR.Right:=RR.Left+imW;
  RR.Top := RR.Top + (RR.Bottom-RR.Top - imH) div 2;
  RR.Bottom := RR.Top + imH;
  offsetRect(RR,FStarMargin div 2,0);

  fHovering:=FInteractive and (not (csDesigning in ComponentState)) and (FMouseX>=0);
  FHoveredRating:=FMinValue;

  zi:=0;
  z:=1;
  if sCnt in [2,4] then
  begin
    zs:=Rating/((MaxValue-MinValue)/FStarCount)+0.5;
    while z<=FStarCount do
    begin
      if fHovering then
      begin
        if FMouseX>=RR.Left then
        begin
          FHoveredRating:=z+FMinValue;
          if sCnt=4 then
            FImages.Draw(TargetCanvas, RR.Left, RR.Top, 2)
          else
            FImages.Draw(TargetCanvas, RR.Left, RR.Top, 0);
        end
        else
        if sCnt=4 then
          FImages.Draw(TargetCanvas, RR.Left, RR.Top, 3)
        else
          FImages.Draw(TargetCanvas, RR.Left, RR.Top, 1);
      end
      else
      begin
        if (zi=0) and (zs<z) then zi:=1;
        FImages.Draw(TargetCanvas, RR.Left, RR.Top, zi);
      end;
      offsetRect(RR,imW,0);
      Inc(z);
    end;
  end
  else
  begin
    zs:=round((Rating/((MaxValue-MinValue)/FStarCount)+0.2)*10)/10;
    while z<=FStarCount do
    begin
      fClr:=FFillColor;
      if fHovering then
      begin
        if FMouseX>=RR.Left then
        begin
          FHoveredRating:=z+FMinValue;
          if FHoverColor<>clNone then
            fClr:=FHoverColor;
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
              5:FImages.Draw(TargetCanvas, RR.Left, RR.Top, 3);
            else
              FImages.Draw(TargetCanvas, RR.Left, RR.Top, 0);
            end;
          end
          else
            case sCnt of
              5:FImages.Draw(TargetCanvas, RR.Left, RR.Top, 4);
            else
              FImages.Draw(TargetCanvas, RR.Left, RR.Top, 2);
            end;
        end
        else
          FImages.Draw(TargetCanvas, RR.Left, RR.Top, zi)
      end
      else
      begin
        if (zi=0) or (zi=sCnt-1) or fHovering then
        begin
          if fHovering then
          begin
            if FMouseX>=RR.Left then
            begin
              TargetCanvas.Pen.Color:=fClr;
              TargetCanvas.Brush.Color:=BrightColor(fClr,10);
            end
            else
            begin
              TargetCanvas.Pen.Color:=FFrameColor;
              TargetCanvas.Brush.Color:=FBackColor;
            end;
          end
          else
          if (zi=0) then
          begin
            TargetCanvas.Pen.Color:=fClr;
            TargetCanvas.Brush.Color:=BrightColor(fClr,10);
          end
          else
          begin
            TargetCanvas.Pen.Color:=FFrameColor;
            TargetCanvas.Brush.Color:=FBackColor;
          end;
          case FStarShape of
            stsSquare:TargetCanvas.Rectangle(RR.Left,RR.Top,RR.Left+imW,RR.Top+imH);
            stsRoundSquare:TargetCanvas.RoundRect(RR.Left,RR.Top,RR.Left+imW,RR.Top+imH, rcs, rcs);
            stsCircle:TargetCanvas.Ellipse(RR.Left,RR.Top,RR.Left+imW,RR.Top+imH);
          else
            begin
              _StarPoints(RR,p);
              TargetCanvas.Polygon(p);
            end;
          end;
        end
        else
        begin
          rX:=RR.Left+imW div 2;
          TargetCanvas.Pen.Color:=FFrameColor;
          TargetCanvas.Brush.Color:=FBackColor;
          case FStarShape of
            stsSquare:
              begin
                TargetCanvas.Rectangle(rx-1,RR.Top,RR.Left+imW,RR.Top+imH);
                TargetCanvas.Pen.Color:=fClr;
                TargetCanvas.Brush.Color:=BrightColor(fClr,10);
                TargetCanvas.Rectangle(RR.Left,RR.Top,rX+1,RR.Top+imH);
              end;
            stsRoundSquare:
              begin
                TargetCanvas.RoundRect(RR.Left,RR.Top,RR.Left+imW,RR.Top+imH,rcs,rcs);
                TargetCanvas.Pen.Color:=fClr;
                TargetCanvas.Brush.Color:=BrightColor(fClr,10);
                TargetCanvas.Polyline([point(rX,RR.Top), point(rx,RR.Top+imH)]);
                ExcludeClipRect(TargetCanvas.Handle,rX,RR.Top,RR.Left+imW, RR.Top+imH);
                TargetCanvas.RoundRect(RR.Left,RR.Top,RR.Left+imW,RR.Top+imH,rcs,rcs);
              end;
            stsCircle:
              begin
                TargetCanvas.Pen.Color:=FFrameColor;
                TargetCanvas.Brush.Color:=FBackColor;
                TargetCanvas.Ellipse(RR.Left,RR.Top,RR.Left+imW,RR.Top+imH);
                TargetCanvas.Pen.Color:=fClr;
                TargetCanvas.Polyline([point(rX,RR.Top+1), point(rx,RR.Top+imH-1)]);
                TargetCanvas.Arc(RR.Left,RR.Top,RR.Left+imW,RR.Top+imH,rX+1,RR.Top,rX+1,RR.Top+imH);
                TargetCanvas.Brush.Color:=BrightColor(fClr,10);
                TargetCanvas.FloodFill(rx-FrameWidth,RR.Top+FrameWidth+1,fClr,fsBorder);
              end;
          else
            begin
              _StarPoints(RR,p);
              TargetCanvas.Polygon(p);
              TargetCanvas.Pen.Color:=fClr;
              TargetCanvas.Brush.Color:=BrightColor(fClr,10);
              ExcludeClipRect(TargetCanvas.Handle,rX+1,RR.Top,RR.Left+imW, RR.Top+imH);
              TargetCanvas.Polygon(p);
            end;
          end;
        end;
      end;
      offsetRect(RR,imW+FStarMargin,0);
      if zi=1 then
        zi:=2;
      Inc(z);
    end;
  end;
end;

procedure TCustomDecoRatingStars.DrawFocus(TargetCanvas:TCanvas; aRect:TRect);
begin
  TargetCanvas.Font.Color:=Font.Color;
  windows.DrawFocusRect(TargetCanvas.Handle,aRect);
end;

procedure TCustomDecoRatingStars.Paint;
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

    DrawRating(tmpCanvas, aRect);

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

procedure TCustomDecoRatingStars.PrintTo(TargetCanvas:TCanvas; Dest:TRect; AsEMF:Boolean=False);
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

    DrawRating(TargetCanvas, Dest);

    if Assigned(OnAfterPaint) then
      OnAfterPaint(Self,TargetCanvas);

  finally
    _EMFExporting := False;
  end;
end;

end.
