unit CHButton;

{ ##############################################################################
  TCHButton

  Version   		:   1.4.0
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 21.07.2002    - First Release
  1.0.1 - 29.08.2002    - Code reorganized
                        - NEW: Array for Gradient
  1.1.0 - 12.11.2002    - NEW: EnableColor (CaptionLayout)
                        - NEW: Speedbutton compatibility (Style)
                        - NEW: Autocolor, AutoPercent (Border)
                        - CHANGE: Draw Caption work some smarter
                        - NEW: MouseWheel, MouseWheelDown, MouseWheelUp
  1.1.1 - 30.11.2002    - BUG: repair some memory leaks
  1.2.0 - 28.12.2002    - NEW: "ModalResult" supported
                        - NEW: Captioneffects (multigradient and texture - font)
                        - BUG: some fixes if "Speedbutton" mode
  1.2.1 - 09.03.2003    - reorganize "uses" for more performance and less memory needed
  1.3.0 - 19.08.2003    - BUG: Return execute the OnClick event now
                        - NEW: ReturnAsTab change the focus to the next control
  1.4.0 - 04.07.3004    - NEW: Glphy can enabled

  ############################################################################ }


interface

uses
  Windows, Forms, Messages, Classes, Controls, Graphics, Math, Types,
  _CHClassProperty, _CHClassFunction;

type
  TPercent = 1..100;

  TCHCustomButton = class;

  TCHButtonStyle = class(TPersistent)
  private
    FOwner : TCHCustomButton;
    FButtonMode : TButtonMode;
    FAllowAllUp: Boolean;
    FGroupIndex: Integer;
    procedure SetButtonMode(const Value: TButtonMode);
    procedure SetGroupIndex(const Value: Integer);
  protected

  public
    constructor Create(AOwner : TCHCustomButton); virtual;
  published
    property Mode : TButtonMode read FButtonMode Write SetButtonMode;
    property GroupIndex : Integer read FGroupIndex Write SetGroupIndex;
    property AllowAllUp : Boolean read FAllowAllUp Write FAllowAllUp;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHCustomButton = class(TCustomControl)
  private
    FCaptionLayout : TCHCaptionLayout;
    FBorder : TCHBorder;
    FGlyph : TCHGlyph;
    FBitmap : TCHBitmap;
    FGradient : TCHGradient;
    FFill : TCHFill;
    FFocus : TCHFocus;
    FButtonStyle: TCHButtonStyle;
    FCaptionEffect: TCHCaptionEffect;

    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnClick : TNotifyEvent;
    FOnMouseWheel: TMouseWheelEvent;
    FOnMouseWheelUp: TMouseWheelUpDownEvent;
    FOnMouseWheelDown: TMouseWheelUpDownEvent;

    FModalResult: TModalResult;
    FColor : TColor;
    FWheelAccumulator: Integer;
    FDown : Boolean;
    FDefault : Boolean;
    FActive : Boolean;
    FClicksDisabled : Boolean;
    FEffect : Boolean;
    FShowAccelChar : Boolean;
    FClientRect : TRect;
    FWorkRect : TRect;
    FCaptionRect : TRect;
    FBackgroundBmp : TBitmap;
    FEffectBitmap : TBitmap;
    FReturnAsTab: Boolean;

    procedure UpdateChanges(Sender: TObject);
    procedure GetRectSize;
    procedure MakeBorder(ButtonState : Boolean);
    procedure MakeForeground;
    procedure DrawFocus;
    procedure DoButtonMode;
    procedure DoAutoColor;

    procedure CMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message : TMessage); message CM_MOUSELEAVE;
    procedure CMMouseWheel(var Message : TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure CMFontChanged(var Message : TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Msg : TMessage); message CM_TEXTCHANGED;
    procedure CMDialogChar(var Message : TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Message :TMessage); message CM_ENABLEDCHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_EraseBkgnd;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure SetColor(const Value: TColor);
    procedure SetDown(const Value : Boolean);
    procedure SetTextStyle(const Value: TTextStyle);
    //procedure SetDefault(const Value: Boolean);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;MousePos: TPoint): Boolean; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure WndProc(var Message: TMessage); override;
    procedure Click; override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property Caption;
    function GetHighlightColor(cColor: TColor; Percent : TPercent): TColor;
    function GetShadowColor(cColor: TColor; Percent : TPercent): TColor;
  published
    property OnMouseEnter : TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave : TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
    property OnMouseWheelDown: TMouseWheelUpDownEvent read FOnMouseWheelDown write FOnMouseWheelDown;
    property OnMouseWheelUp: TMouseWheelUpDownEvent read FOnMouseWheelUp write FOnMouseWheelUp;

    property Bitmap : TCHBitmap read FBitmap Write FBitmap;
    property Border : TCHBorder read FBorder write FBorder;
    property CaptionEffect : TCHCaptionEffect read FCaptionEffect write FCaptionEffect;
    property CaptionLayout : TCHCaptionLayout read FCaptionLayout write FCaptionLayout;
    property Color: TColor read FColor write SetColor;
    property Down : Boolean read FDown write SetDown;
    //property Default: Boolean read FDefault write SetDefault default False;
    property Fill : TCHFill read FFill write FFill;
    property Focus : TCHFocus read FFocus write FFocus;
    property Glyph : TCHGlyph read FGlyph Write FGlyph;
    property Gradient : TCHGradient read FGradient Write FGradient;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
    property Style : TCHButtonStyle read FButtonStyle Write FButtonStyle;
    property ReturnAsTab : Boolean read FReturnAsTab Write FReturnAsTab;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }

  TCHButton = class(TCHCustomButton)
  published
    property Action;
    property Align;
    property Anchors;
    property BiDiMode;
    property Bitmap;
    property Border;
    property Caption;
    property CaptionEffect;
    property CaptionLayout;
    property Constraints;
    property Color;
    property Down;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Fill;
    property Focus;
    property Font;
    property Glyph;
    property Gradient;
    property ModalResult;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style;
    property TabStop default True;
    property TabOrder;
    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHButton]);
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHCustomButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FGlyph := TCHGlyph.Create;
  FGlyph.OnChange := UpdateChanges;
  FBitmap := TCHBitmap.Create;
  FBitmap.OnChange := UpdateChanges;
  FCaptionLayout := TCHCaptionLayout.Create;
  FCaptionLayout.OnChange := UpdateChanges;
  FCaptionEffect := TCHCaptionEffect.Create;
  FCaptionEffect.OnChange := UpdateChanges;
  FBorder := TCHBorder.Create;
  FBorder.OnChange := UpdateChanges;
  FGradient := TCHGradient.Create;
  FGradient.OnChange := UpdateChanges;
  FFill := TCHFill.Create;
  FFill.OnChange := UpdateChanges;
  FFocus := TCHFocus.Create;
  FFocus.OnChange := UpdateChanges;
  FButtonStyle := TCHButtonStyle.Create(Self);

  Width := 75;
  Height := 25;
  FColor := clBtnFace;
  FDown := False;
  FDefault := False;
  TabStop := True;
  FShowAccelChar := True;
  FEffectBitmap := TBitmap.Create;
  FBackgroundBmp := TBitmap.Create;

  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHCustomButton.Destroy;
begin
  FGlyph.Free;
  FBitmap.Free;
  FCaptionLayout.Free;
  FCaptionEffect.Free;
  FBorder.Free;
  FGradient.Free;
  FFill.Free;
  FFocus.Free;
  FEffectBitmap.Free;
  FBackgroundBmp.Free;
  FButtonStyle.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.UpdateChanges(Sender: TObject);
begin
  if csLoading in ComponentState then
    Exit;

  if FBorder.AutoColor then
    DoAutoColor;
  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.Paint;
var
  nX_BackgroundBmp, nY_BackgroundBmp : Integer;
begin
  inherited Paint;
  Canvas.Font.Assign(Self.Font);
  FEffect := False;

  // get size of ClientRect, WorkRect and TextRect
  GetRectSize;

  // ++++ SET TEXTSTYLE ++++
  if (FFill.Style = fsNormal) or (FFill.Style = fsGradient) or (FFill.Style = fsEffect) then
  begin
    SetTextStyle(FCaptionLayout.TextStyle);
  end;

  // ++++ DRAW NORMAL BACKGROUND
  if not FFill.Transparent then
    DrawNormal(Self.Canvas, FWorkRect, Self.Color)
  // ++++ DRAW TRANSPARENT BACKGROUND
  else
    DrawTransparent(Self, Self.Canvas, FWorkRect);

  // ++++ DRAW GRADIENT BACKGROUND ++++
  if (FFill.Style = fsGradient) then
  begin
    // to draw gradient, min. two colors must ACTIVE
    if Length(FGradient.FGradientColorArray) > 1 then
    begin
      DrawGradient(Canvas, FClientRect, FGradient.FGradientColorArray,
        FGradient.Style, FGradient.Rotation);
    end
    else
    begin
      // only one color is ACTICE
      if (Length(FGradient.FGradientColorArray) = 1) then
        Canvas.Brush.Color := FGradient.FGradientColorArray[0]
      // no color is ACTIVE
      else
        Canvas.Brush.Color := Color;

      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(FClientRect);
    end;
  end
  // ++++ DRAW CAPTION-EFFECT ++++
  else if (FFill.Style = fsEffect) then
  begin
    FEffect := True;

    // draw caption-gradinet
    if (FCaptionEffect.Effect = ceGradient) then
    begin
      FEffectBitmap.Width := Self.Width;
      FEffectBitmap.Height := Self.Height;

      // to draw gradient, min. two colors must ACTIVE
      if Length(FGradient.FGradientColorArray) > 1 then
      begin
        DrawGradient(FEffectBitmap.Canvas, FClientRect, FGradient.FGradientColorArray,
          FGradient.Style, FGradient.Rotation);
      end
      else
      begin
        // only one color is ACTICE
        if (Length(FGradient.FGradientColorArray) = 1) then
          FEffectBitmap.Canvas.Brush.Color := FGradient.FGradientColorArray[0]
        // no color is ACTIVE
        else
          FEffectBitmap.Canvas.Brush.Color := Color;

        FEffectBitmap.Canvas.Brush.Style := bsSolid;
        FEffectBitmap.Canvas.FillRect(FClientRect);
      end;
    end
    // set fontbitmap; bitmap will not scale
    else if (FCaptionEffect.Effect = ceBitmap) then
    begin
      FEffectBitmap.Width := Self.Width;
      FEffectBitmap.Height := Self.Height;
      FEffectBitmap.Assign(FCaptionEffect.FontBitmap);
    end;
  end
  // ++++ DRAW BACKGROUND BITMAP ++++
  else if (FFill.Style = fsBitmap) then
  begin
    if not FBitmap.Bitmap.Empty then
    begin
      // build bitmap
      MakeForeground;

      // draw bitmap onto the canvas
      with Canvas do
      begin
        // Normal
        if FBitmap.Mode = bmNormal then
        begin
          FBackgroundBmp.Canvas.Draw(0, 0, FBitmap.Bitmap);
          nX_BackgroundBmp := FWorkRect.Left;
          nY_BackgroundBmp := FWorkRect.Top;
          Draw(nX_BackgroundBmp, nY_BackgroundBmp, FBackgroundBmp);
        end
        // Stretch
        else if FBitmap.Mode = bmStretch then
        begin
          FBackgroundBmp.Canvas.Draw(0, 0, FBitmap.Bitmap);
          StretchDraw(FWorkRect, FBackgroundBmp);
        end
        // Tile
        else if FBitmap.Mode = bmTile then
        begin
          FBackgroundBmp.Canvas.Draw(0, 0, FBitmap.Bitmap);
          Brush.Bitmap := FBackgroundBmp;
          FillRect(FWorkRect);
        end
        // Center
        else if FBitmap.Mode = bmCenter then
        begin
          // Center X
          if (FWorkRect.Right - FWorkRect.Left) > FBitmap.Bitmap.Width then
            nX_BackgroundBmp := ((FWorkRect.Right - FWorkRect.Left) div 2) -
              (FBitmap.Bitmap.Width div 2)
          else
            nX_BackgroundBmp := -(FBitmap.Bitmap.Width - (FWorkRect.Right - FWorkRect.Left)) div 2;

          // Center Y
          if (FWorkRect.Bottom - FWorkRect.Top) > FBitmap.Bitmap.Height then
            nY_BackgroundBmp := ((FWorkRect.Bottom - FWorkRect.Top) div 2) -
              (FBitmap.Bitmap.Height div 2)
          else
            nY_BackgroundBmp := -(FBitmap.Bitmap.Height - (FWorkRect.Bottom - FWorkRect.Top)) div 2;

          FBackgroundBmp.Canvas.Draw(nX_BackgroundBmp, nY_BackgroundBmp, FBitmap.Bitmap);

          Draw(0, 0, FBackgroundBmp);
        end;
      end;
    end;
  end;

  // draw caption with setting parameters
  if (FFill.Style = fsNormal) or (FFill.Style = fsGradient) or (FFill.Style = fsEffect) then
  begin
    DrawCaption(
      Self,
      Self.Canvas,
      FCaptionRect, FWorkRect, FClientRect,
      FCaptionLayout.Angle,
      FCaptionLayout.HighlightDepth, FCaptionLayout.ShadowDepth,
      FCaptionLayout.HighlightDirection, FCaptionLayout.ShadowDirection,
      FCaptionLayout.HighlightColor, FCaptionLayout.ShadowColor, Self.Color,
      FCaptionLayout.DisableColor, Self.Font.Color, FGlyph.TransparentColor,
      Self.Caption,
      FEffectBitmap,
      FCaptionLayout.TextStyle,
      FCaptionLayout.Alignment,
      FFill.Style,
      FGlyph.TransparentMode,
      FGlyph.AlignMode,
      FGlyph.Alignment,
      FGlyph.EnabledMode,
      FGlyph.Glyph,
      FGlyph.PosX, FGlyph.PosY, FGlyph.Space, FCaptionLayout.PosX, FCaptionLayout.PosY,
      False, Self.Down, FCaptionLayout.Antialiasing, FShowAccelChar, FEffect,
      ctButton);
  end;

  // ++++ DRAW BORDER ++++
  MakeBorder(FDown);

  // ++++ DRAW FOCUS ++++
  if FActive or FFocus.Show then
    DrawFocus;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.GetRectSize;
var
  nSingle, nBorderWidth : Integer;
begin
  nSingle := 0;
  nBorderWidth := 0;

  if (FBorder.Style = bsExtended) or (FBorder.Style = bsFlat) then
    nSingle := FBorder.SingleWidth;

  if (FBorder.Style = bsExtended) or (FBorder.Style = bsNormal) then
    nBorderWidth := FBorder.Width;

  // ClientRect
  FClientRect := GetClientRect;

  // WorkRect
  FWorkRect.Left := FClientRect.Left + (nBorderWidth + nSingle);
  FWorkRect.Top := FClientRect.Top + (nBorderWidth + nSingle);
  FWorkRect.Right := FClientRect.Right - (nBorderWidth + nSingle);
  FWorkRect.Bottom := FClientRect.Bottom - (nBorderWidth + nSingle);

  // TextRect
  FCaptionRect.Left := 0;
  FCaptionRect.Top := 0;
  FCaptionRect.Right := 0;
  FCaptionRect.Bottom := 0;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.MakeBorder(ButtonState : Boolean);
var
  nRow, nSingle, I : Integer;
begin
  nSingle := 0;

  with Canvas do
  begin
    Pen.Width := 1;
    Pen.Style := psSolid;
    Brush.Style := bsSolid;

    // Single-Border
    if (FBorder.Style = bsExtended) or (FBorder.Style = bsFlat) then
    begin
      Pen.Color := FBorder.Color;
      for I := 0 to FBorder.SingleWidth - 1 do
      begin
        MoveTo(I, I);
        LineTo(Width - 1 - I, I);
        LineTo(Width - 1 - I, Height - 1 - I);
        LineTo(I, Height - 1 - I);
        LineTo(I, I);
      end;
      nSingle := FBorder.SingleWidth;
    end;

    // Button-Border
    if (FBorder.Style = bsExtended) or (FBorder.Style = bsNormal) then
    begin
      // top + left
      if (ButtonState = True) then
        Pen.Color := FBorder.ShadowColor
      else
        Pen.Color := FBorder.HighlightColor;

      for nRow := 0 to FBorder.Width - 1 do
      begin
        MoveTo(Width - nRow - 1 - nSingle, nRow + nSingle);
        LineTo(nRow + nSingle, nRow + nSingle);
        LineTo(nRow + nSingle, Height - nRow - 1 - nSingle);
      end;

      // right + bottom
      if (ButtonState = True) then
        Pen.Color := FBorder.HighlightColor
      else
        Pen.Color := FBorder.ShadowColor;

      for nRow := 0 to FBorder.Width - 1 do
      begin
        MoveTo(Width - nRow - 1 - nSingle, nRow + nSingle);
        LineTo(Width - nRow - 1 - nSingle, Height - nRow - 1 - nSingle);
        LineTo(nRow + nSingle - 1, Height - nRow - 1 - nSingle);
      end;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.MakeForeground;
var
  BRect : TRect;
begin
  BRect := Bounds(0, 0, Self.Width, Self.Height);
  FBackgroundBmp.Canvas.Brush.Color := clWhite;
  FBackgroundBmp.Canvas.Brush.Style := bsSolid;
  FBackgroundBmp.Canvas.FillRect(BRect);

  if (FBitmap.Mode = bmNormal) or (FBitmap.Mode = bmCenter) then
  begin
    FBackgroundBmp.Width := (FWorkRect.Right - FWorkRect.Left);
    FBackgroundBmp.Height := (FWorkRect.Bottom - FWorkRect.Top);
  end
  else if (FBitmap.Mode = bmStretch) or (FBitmap.Mode = bmTile) then
  begin
    FBackgroundBmp.Width := FBitmap.Bitmap.Width;
    FBackgroundBmp.Height := FBitmap.Bitmap.Height;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.DrawFocus;
var
  nWidth, X, Y, PixStep, PosLeft, PosRight, PosTop, PosBottom : Integer;
begin
  // Step
  PixStep := FFocus.Step;

  // Style
  if FFocus.Style = csSolid then
    PixStep := 1;

  // Mode + Pos
  if FFocus.Mode = cmAuto then
  begin
    PosLeft := FWorkRect.Left + 4;
    PosRight := FWorkRect.Right - 4;
    PosTop := FWorkRect.Top + 4;
    PosBottom := FWorkRect.Bottom - 4;
  end
  else
  begin
    PosLeft := FFocus.PosLeft;
    PosRight := FFocus.PosRight;
    PosTop := FFocus.PosTop;
    PosBottom := FFocus.PosBottom;
  end;

  with Self.Canvas do
  begin
    for nWidth := 0 to FFocus.Width - 1 do
    begin
      // top
      X := PosLeft;
      while X <= PosRight do
      begin
        Pixels[X, PosTop + nWidth] := FFocus.Color;
        Inc(X, PixStep);
      end;
      // bottom
      X := PosLeft;
      while X <= PosRight do
      begin
        Pixels[X, PosBottom - nWidth] := FFocus.Color;
        Inc(X, PixStep);
      end;
      // left
      Y := PosTop;
      while Y <= PosBottom do
      begin
        Pixels[PosLeft + nWidth, Y] := FFocus.Color;
        Inc(Y, PixStep);
      end;
      // right
      Y := PosTop;
      while Y <= PosBottom do
      begin
        Pixels[PosRight - nWidth, Y] := FFocus.Color;
        Inc(Y, PixStep);
      end;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.DoButtonMode;
var
  nCtl : Integer;
  SelfButton : TCHButton;
begin
  if FButtonStyle.FButtonMode = bmSpeedbutton then
  begin
    if Parent <> nil then
    begin
      for nCtl := 0 to Parent.ControlCount - 1 do
      begin
        if Parent.Controls[nCtl] is TCHButton then
        begin
          SelfButton := TCHButton(Parent.Controls[nCtl]);
          if (SelfButton <> Self) and (SelfButton.Style.GroupIndex = Style.GroupIndex) then
            SelfButton.Down := False;
        end;
      end;
    end;
    if (FButtonStyle.FAllowAllUp) and (Self.Down) then
      Self.Down := False
    else
      Self.Down := True;

    if csDesigning in ComponentState then
    begin
      if (GetParentForm(self) <> nil) and (GetParentForm(self).Designer <> nil) then
        GetParentForm(self).Designer.Modified;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.CMMouseEnter(var Message: TMessage);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.CMMouseLeave(var Message: TMessage);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.CMMouseWheel(var Message: TCMMouseWheel);
begin
  with Message do
  begin
    Result := 0;
    if DoMouseWheel(ShiftState, WheelDelta, SmallPointToPoint(Pos)) then
      Message.Result := 1;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomButton.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  IsNeg: Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheel) then
    FOnMouseWheel(Self, Shift, WheelDelta, MousePos, Result);
  if not Result then
  begin
    Inc(FWheelAccumulator, WheelDelta);
    while Abs(FWheelAccumulator) >= WHEEL_DELTA do
    begin
      IsNeg := FWheelAccumulator < 0;
      FWheelAccumulator := Abs(FWheelAccumulator) - WHEEL_DELTA;
      if IsNeg then
      begin
        if FWheelAccumulator <> 0 then FWheelAccumulator := -FWheelAccumulator;
        Result := DoMouseWheelDown(Shift, MousePos);
      end
      else
        Result := DoMouseWheelUp(Shift, MousePos);
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomButton.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheelDown) then
    FOnMouseWheelDown(Self, Shift, MousePos, Result);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomButton.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheelUp) then
    FOnMouseWheelUp(Self, Shift, MousePos, Result);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.Resize;
begin
  inherited Resize;
  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.SetDown(const Value : Boolean);
begin
  if FDown <> Value then
  begin
    FDown := Value;
    Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Invalidate;
    if FBorder.AutoColor then
      DoAutoColor;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
  begin
    if IsAccel(CharCode, Caption) then
    begin
      if Enabled and Assigned(FOnClick) then
        FOnClick(Self);
      Result := 1;
    end
    else
      inherited;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FGlyph.Glyph.Assign(FGlyph.FRestoreBmp);
  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.CMTextChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Text-Style von Caption setzen }
procedure TCHCustomButton.SetTextStyle(const Value: TTextStyle);
begin
  with FCaptionLayout do
  begin
    TextStyle := Value;
    { ssNone }
    if Value = tsNone then
    begin
      ShadowDirection := drNone;
      ShadowDepth := 0;
      ShadowColor := clBtnShadow;

      HighlightDirection := drNone;
      HighlightDepth := 0;
      HighlightColor := clBtnHighlight;
    end
    { ssRaised }
    else if Value = tsRaised then
    begin
      ShadowDirection := drNone;
      ShadowDepth := 0;
      ShadowColor := clBtnShadow;

      HighlightDirection := drUpLeft;
      HighlightDepth := 1;
      HighlightColor := clBtnHighlight;
    end
    { ssRaisedColor }
    else if Value = tsRaisedColor then
    begin
      ShadowDirection := drDownRight;
      ShadowDepth := 1;
      ShadowColor := clBtnShadow;
      Self.Font.Color := clBtnFace;

      HighlightDirection := drUpLeft;
      HighlightDepth := 1;
      HighlightColor := clBtnHighlight;
    end
    { ssRaisedShadow }
    else if Value = tsRaisedShadow then
    begin
      ShadowDirection := drDownRight;
      ShadowDepth := 1;
      ShadowColor := clBtnShadow;

      HighlightDirection := drUpLeft;
      HighlightDepth := 1;
      HighlightColor := clBtnHighlight;
    end
    { ssRecessed }
    else if Value = tsRecessed then
    begin
      ShadowDirection := drNone;
      ShadowDepth := 0;
      ShadowColor := clBtnShadow;

      HighlightDirection := drDownRight;
      HighlightDepth := 1;
      HighlightColor := clBtnHighlight;
    end
    { ssRecessedColor }
    else if Value = tsRecessedColor then
    begin
      ShadowDirection := drUpLeft;
      ShadowDepth := 1;
      ShadowColor := clBtnShadow;
      Self.Font.Color := clBtnFace;

      HighlightDirection := drDownRight;
      HighlightDepth := 1;
      HighlightColor := clBtnHighlight;
    end
    { ssRecessedShadow }
    else if Value = tsRecessedShadow then
    begin
      ShadowDirection := drUpLeft;
      ShadowDepth := 1;
      ShadowColor := clBtnShadow;

      HighlightDirection := drDownRight;
      HighlightDepth := 1;
      HighlightColor := clBtnHighlight;
    end
    { ssShadow }
    else if Value = tsShadow then
    begin
      ShadowDirection := drDownRight;
      ShadowDepth := 2;
      ShadowColor := clBtnShadow;

      HighlightDirection := drNone;
      HighlightDepth := 0;
      HighlightColor := clBtnHighlight;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DoButtonMode;

  inherited MouseDown(Button, Shift, X, Y);
  if Enabled and (Button = mbLeft) and (FButtonStyle.FButtonMode <> bmSpeedbutton) then
  begin
    if not FDown then
      FDown := True;

    Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Enabled and (Button = mbLeft) and (FButtonStyle.FButtonMode <> bmSpeedbutton) then
  begin
    if FDown then
      FDown := False;

    Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.CMFocusChanged(var Message: TCMFocusChanged);
begin
  with Message do
  begin
    if (Sender is TCHButton) and (FFocus.Active = True) then
    begin
      FActive := Sender = Self;
      Invalidate;
    end
    else
      FActive := FDefault;
  end;

  inherited;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_LBUTTONDBLCLK:
	    if not (csDesigning in ComponentState) and not Focused then
      begin
        FClicksDisabled := True;
        Windows.SetFocus(Handle);
        FClicksDisabled := False;
        if not Focused then
          Exit;
      end;
    CN_COMMAND:
      if FClicksDisabled then
        Exit;
  end;

  inherited WndProc(Message);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.CMExit(var Message: TCMExit);
begin
  with Message do
  begin
    FActive := False;
    Invalidate;
  end;

  inherited;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.Click;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Form <> nil then
    Form.ModalResult := ModalResult;


  Invalidate;
  inherited;

  if Assigned(FOnClick) then
    FOnClick(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.DoAutoColor;
begin
  FBorder.HighlightColor := GetHighlightColor(Self.Color, FBorder.AutoPercent);
  FBorder.ShadowColor := GetShadowColor(Self.Color, FBorder.AutoPercent);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
//procedure TCHCustomButton.SetDefault(const Value: Boolean);
//var
//  Form: TCustomForm;
//begin
//  FDefault := Value;
//  if HandleAllocated then
//  begin
//    Form := GetParentForm(Self);
//    if Form <> nil then
//      Form.Perform(CM_FOCUSCHANGED, 0, Longint(Form.ActiveControl));
//  end;
//end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomButton.KeyPress(var Key: Char);
begin
  if Key = #13 then
  begin
    if FReturnAsTab then
    begin
      Key := #0;
      PostMessage(self.Handle,WM_KEYDOWN,VK_TAB,0);
    end
    else
    begin
      OnClick(Self);
    end;
  end;

  inherited;
end;



{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ TCHButtonStyle }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHButtonStyle.Create(AOwner: TCHCustomButton);
begin
  inherited Create;
  FOwner := AOwner;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHButtonStyle.SetButtonMode(const Value: TButtonMode);
begin
  if FButtonMode <> Value then
    FButtonMode := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHButtonStyle.SetGroupIndex(const Value: Integer);
begin
  if FGroupIndex <> Value then
    FGroupIndex := Value;
end;







function TCHCustomButton.GetHighlightColor(cColor: TColor;
  Percent: TPercent): TColor;
begin
  Result := RGB(Min(GetRValue(ColorToRGB(cColor)) + (Percent * 2), 255),
    Min(GetGValue(ColorToRGB(cColor)) + (Percent * 2), 255),
    Min(GetBValue(ColorToRGB(cColor)) + (Percent * 2), 255));
end;

function TCHCustomButton.GetShadowColor(cColor: TColor;
  Percent: TPercent): TColor;
begin
  Result := RGB(Max(GetRValue(ColorToRGB(cColor)) - (Percent * 2), 0),
    Max(GetGValue(ColorToRGB(cColor)) - (Percent * 2), 0),
    Max(GetBValue(ColorToRGB(cColor)) - (Percent * 2), 0));
end;

end.
