unit CHCheckBox;

{ ##############################################################################
  TCHCheckBox

  Version   		:   1.4.0
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 21.07.2002    - First Release
  1.0.1 - 31.08.2002    - BUG: repair memoryleck
  1.1.0 - 21.10.2002    - NEW: Autocolor, AutoPercent (Border)
                        - CHANGE: draw Caption work some smarter
                        - NEW: MouseWheel, MouseWheelDown, MouseWheelUp
  1.1.1 - 15.12.2002    - BUG: repair some memory leaks
  1.2.0 - 27.12.2002    - NEW: Captioneffects (multigradient and texture - font)
                        - NEW: Gradient background supported
                        - NEW: Autocolor, AutoPercent (Border)
                        - NEW: Bitmap supported
                        - BUG: "Transparent" work now correctly at Designtime
  1.2.1 - 09.03.2003    - reorganize "uses" for more performance and less memory needed
  1.3.0 - 20.08.2003    - NEW: Groupindex
  1.4.0 - 04.07.2004    - NEW: Glphy can enabled
  1.4.1 - 02.01.2005    - BUG: Fix error in Event handler for Onclick, OnMouseDown and OnMouseUp

  ############################################################################ }

interface

uses
  Forms, Windows, Messages, Classes, Controls, StdCtrls, Graphics, Math, SysUtils,
  Types, _CHClassProperty, _CHClassFunction;

type
  TPercent = 1..100;

  TCHCustomCheckBox = class(TCustomControl)
  private
    FCaptionLayout : TCHCaptionLayout;
    FBox : TCHBoxCB;
    FGlyph : TCHGlyph;
    FBorder : TCHBorder;
    FFill : TCHFill;
    FFocus : TCHFocus;
    FCaptionEffect: TCHCaptionEffect;
    FGradient: TCHGradient;
    FBitmap: TCHBitmap;

    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnClick : TNotifyEvent;
    FOnMouseWheel: TMouseWheelEvent;
    FOnMouseWheelUp: TMouseWheelUpDownEvent;
    FOnMouseWheelDown: TMouseWheelUpDownEvent;

    FColor: TColor;
    FTmpColor : TBoxFillColor;
    FState : TCheckBoxState;
    FWheelAccumulator: Integer;
    FClientRect : TRect;
    FWorkRect : TRect;
    FCaptionRect : TRect;
    FHook : Boolean;
    FCross : Boolean;
    FSolid : Boolean;
    FNormal : Boolean;
    FFlat : Boolean;
    FChecked: Boolean;
    FDefault : Boolean;
    FActive : Boolean;
    FClicksDisabled : Boolean;
    FAllowGrayed: Boolean;
    FShowAccelChar : Boolean;
    FEffect : Boolean;
    FStatus : string;
    FEffectBitmap : TBitmap;
    FBackgroundBmp : TBitmap;
    FGroupIndex: Integer;

    procedure SetTextStyle(const Value : TTextStyle);
    procedure SetColor(const Value : TColor);
    procedure SetChecked(const Value : Boolean);
    procedure SetAllowGrayed(const Value : Boolean);
    procedure SetState(const Value : TCheckBoxState);

    procedure GetRectSize;
    procedure MakeBorder(ButtonState : Boolean);
    procedure MakeForeground;
    procedure GetBoxBmp(Style : TBoxStyle; Fill : TCheckBoxFill; Color : TBoxFillColor);
    procedure DoState;
    procedure DrawFocus;
    procedure DoAutoColor;
    procedure DoGroup;
    function IsUnderlineChar(const Str : string; CC : Integer) : Boolean;
    function GetHighlightColor(cColor: TColor; Percent : TPercent): TColor;
    function GetShadowColor(cColor: TColor; Percent : TPercent): TColor;

    procedure CMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message : TMessage); message CM_MOUSELEAVE;
    procedure CMMouseWheel(var Message : TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure CMFontChanged(var Message : TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Msg : TMessage); message CM_TEXTCHANGED;
    procedure CMDialogChar(var Message : TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Message :TMessage); message CM_ENABLEDCHANGED;
    procedure WMEraseBkgnd(var Message : TWMEraseBkgnd); message WM_EraseBkgnd;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure UpdateChanges(Sender : TObject);
  protected
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
  published
    property OnMouseEnter : TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave : TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
    property OnMouseWheelDown: TMouseWheelUpDownEvent read FOnMouseWheelDown write FOnMouseWheelDown;
    property OnMouseWheelUp: TMouseWheelUpDownEvent read FOnMouseWheelUp write FOnMouseWheelUp;
    property OnClick : TNotifyEvent read FOnClick write FOnClick;

    property AllowGrayed : Boolean read FAllowGrayed Write SetAllowGrayed;
    property Bitmap : TCHBitmap read FBitmap Write FBitmap;
    property Box : TCHBoxCB read FBox Write FBox;
    property Border : TCHBorder read FBorder write FBorder;
    property CaptionEffect : TCHCaptionEffect read FCaptionEffect write FCaptionEffect;
    property CaptionLayout : TCHCaptionLayout read FCaptionLayout write FCaptionLayout;
    property Checked : Boolean read FChecked Write SetChecked;
    property Color: TColor read FColor write SetColor;
    property Fill : TCHFill read FFill write FFill;
    property Focus : TCHFocus read FFocus write FFocus;
    property Glyph : TCHGlyph read FGlyph Write FGlyph;
    property Gradient : TCHGradient read FGradient Write FGradient;
    property GroupIndex: Integer read FGroupIndex write FGroupIndex;
    property State : TCheckBoxState read FState write SetState;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHCheckBox = class(TCHCustomCheckBox)
  published
    property Action;
    property Anchors;
    property AllowGrayed;
    property BiDiMode;
    property Bitmap;
    property Box;
    property Border;
    property Caption;
    property CaptionEffect;
    property CaptionLayout;
    property Checked;
    property Color;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Fill;
    property Focus;
    property Font;
    property Glyph;
    property Gradient;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property State;
    property ShowHint;
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

{$R *.res}

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHCheckBox]);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHCustomCheckBox.Create(aOwner: TComponent);
begin
  inherited Create(AOwner);

  FBox := TCHBoxCB.Create;
  FBox.OnChange := UpdateChanges;
  FBorder := TCHBorder.Create;
  FBorder.OnChange := UpdateChanges;
  FGlyph := TCHGlyph.Create;
  FGlyph.OnChange := UpdateChanges;
  FCaptionLayout := TCHCaptionLayout.Create;
  FCaptionLayout.OnChange := UpdateChanges;
  FCaptionEffect := TCHCaptionEffect.Create;
  FCaptionEffect.OnChange := UpdateChanges;
  FFill := TCHFill.Create;
  FFill.OnChange := UpdateChanges;
  FFocus := TCHFocus.Create;
  FFocus.OnChange := UpdateChanges;
  FGradient := TCHGradient.Create;
  FGradient.OnChange := UpdateChanges;
  FBitmap := TCHBitmap.Create;
  FBitmap.OnChange := UpdateChanges;

  Width := 110;
  Height := 20;
  FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_N_BLANK');
  FGlyph.AlignMode := gmControl;
  FCaptionLayout.Alignment := tgAuto;
  FBorder.HighlightColor := clWhite;
  FBorder.ShadowColor := clGray;
  FBorder.Width := 0;
  FBorder.SingleWidth := 1;
  FColor := clBtnFace;
  FStatus := 'none';
  FChecked := False;
  FAllowGrayed := False;
  FTmpColor := bcBlack;
  TabStop := True;
  FEffectBitmap := TBitmap.Create;
  FBackgroundBmp := TBitmap.Create;
  FShowAccelChar := True;

  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHCustomCheckBox.Destroy;
begin
  FBox.Free;
  FCaptionLayout.Free;
  DeleteObject(FGlyph.Glyph.Handle);
  FGlyph.Free;
  FBorder.Free;
  FFill.Free;
  FFocus.Free;
  FEffectBitmap.Free;
  FCaptionEffect.Free;
  FGradient.Free;
  FBitmap.Free;
  FBackgroundBmp.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomCheckBox.Paint;
var
  nX_BackgroundBmp, nY_BackgroundBmp : Integer;
begin
  inherited Paint;
  Canvas.Font.Assign(Self.Font);
  FEffect := False;

  // ++++ CHECKBOX - BMP ++++
  GetBoxBmp(FBox.Style, FBox.Fill, FBox.Color);

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
      False, False, FCaptionLayout.Antialiasing, FShowAccelChar, FEffect,
      ctCheckbox);
  end;

  // draw Border
  MakeBorder(False);

  // draw Focus
  if FActive or FFocus.Show then
    DrawFocus;

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomCheckBox.GetRectSize;
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
procedure TCHCustomCheckBox.MakeBorder(ButtonState : Boolean);
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
procedure TCHCustomCheckBox.DoState;
begin
  if FStatus = 'none' then
  begin
    FStatus := 'checked';
    FChecked := True;
    FState := cbChecked;
  end
  else if FStatus = 'checked' then
  begin
    if FAllowGrayed then
    begin
      FStatus := 'disable';
      FChecked := True;
      FState := cbGrayed;
    end
    else
    begin
      FStatus := 'none';
      FChecked := False;
      FState := cbUnchecked;
    end;
  end
  else if FStatus = 'disable' then
  begin
    FStatus := 'none';
    FChecked := False;
    FState := cbUnchecked;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomCheckBox.DrawFocus;
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
    PosLeft := FCaptionRect.Left - 2;
    PosRight := FCaptionRect.Right + 1;
    PosTop := FCaptionRect.Top - 2;
    PosBottom := FCaptionRect.Bottom;
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
procedure TCHCustomCheckBox.GetBoxBmp(Style: TBoxStyle; Fill: TCheckBoxFill;
  Color: TBoxFillColor);
begin
  FNormal := False;
  FFlat := False;
  FHook := False;
  FCross := False;
  FSolid := False;

  FTmpColor := Color;

  if FStatus = 'disable' then
  begin
    FTmpColor := bcGray;
  end
  else if FStatus = 'none' then
  begin
    FTmpColor := bcNone;
  end;

  // NORMAL
  if Style = bxNormal then
  begin
    FNormal := True;
    if FTmpColor = bcNone then
      FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_N_BlANK')
    else
    begin
      if Fill = cfHook then
      begin
        FHook := True;
        if FTmpColor = bcBlack then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_N_HOOK_BLACK')
        else if FTmpColor = bcBlue then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_N_HOOK_BLUE')
        else if FTmpColor = bcGreen then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_N_HOOK_GREEN')
        else if FTmpColor = bcLime then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_N_HOOK_LIME')
        else if FTmpColor = bcRed then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_N_HOOK_RED')
        else if FTmpColor = bcYellow then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_N_HOOK_YELLOW')
        else if FTmpColor = bcGray then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_N_HOOK_GRAY');
      end
      else if Fill = cfCross then
      begin
        FCross := True;
        if FTmpColor = bcBlack then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_N_CROSS_BLACK')
        else if FTmpColor = bcBlue then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_N_CROSS_BLUE')
        else if FTmpColor = bcGreen then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_N_CROSS_GREEN')
        else if FTmpColor = bcLime then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_N_CROSS_LIME')
        else if FTmpColor = bcRed then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_N_CROSS_RED')
        else if FTmpColor = bcYellow then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_N_CROSS_YELLOW')
        else if FTmpColor = bcGray then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_N_CROSS_GRAY');
      end
      else if Fill = cfSolid then
      begin
        FSolid := True;
        if FTmpColor = bcBlack then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_N_FILL_BLACK')
        else if FTmpColor = bcBlue then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_N_FILL_BLUE')
        else if FTmpColor = bcGreen then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_N_FILL_GREEN')
        else if FTmpColor = bcLime then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_N_FILL_LIME')
        else if FTmpColor = bcRed then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_N_FILL_RED')
        else if FTmpColor = bcYellow then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_N_FILL_YELLOW')
        else if FTmpColor = bcGray then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_N_FILL_GRAY');
      end;
    end;
  end
  // FLAT
  else if Style = bxFlat then
  begin
    FFlat := True;
    if FTmpColor = bcNone then
      FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_F_BlANK')
    else
    begin
      if Fill = cfHook then
      begin
        FHook := True;
        if FTmpColor = bcBlack then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_F_HOOK_BLACK')
        else if FTmpColor = bcBlue then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_F_HOOK_BLUE')
        else if FTmpColor = bcGreen then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_F_HOOK_GREEN')
        else if FTmpColor = bcLime then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_F_HOOK_LIME')
        else if FTmpColor = bcRed then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_F_HOOK_RED')
        else if FTmpColor = bcYellow then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_F_HOOK_YELLOW')
        else if FTmpColor = bcGray then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_F_HOOK_GRAY');
      end
      else if Fill = cfCross then
      begin
        FCross := True;
        if FTmpColor = bcBlack then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_F_CROSS_BLACK')
        else if FTmpColor = bcBlue then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_F_CROSS_BLUE')
        else if FTmpColor = bcGreen then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_F_CROSS_GREEN')
        else if FTmpColor = bcLime then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_F_CROSS_LIME')
        else if FTmpColor = bcRed then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_F_CROSS_RED')
        else if FTmpColor = bcYellow then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_F_CROSS_YELLOW')
        else if FTmpColor = bcGray then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_F_CROSS_GRAY');
      end
      else if Fill = cfSolid then
      begin
        FSolid := True;
        if FTmpColor = bcBlack then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_F_FILL_BLACK')
        else if FTmpColor = bcBlue then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_F_FILL_BLUE')
        else if FTmpColor = bcGreen then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_F_FILL_GREEN')
        else if FTmpColor = bcLime then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_F_FILL_LIME')
        else if FTmpColor = bcRed then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_F_FILL_RED')
        else if FTmpColor = bcYellow then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_F_FILL_YELLOW')
        else if FTmpColor = bcGray then
          FGlyph.Glyph.Handle := LoadBitmap(hInstance, 'CB_F_FILL_GRAY');
      end;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomCheckBox.Click;
begin
  inherited;
  DoState;

  if Assigned(FOnClick) then
    FOnClick(Self);

  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomCheckBox.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    DoState;
    Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomCheckBox.SetState(const Value: TCheckBoxState);
begin
  if FState <> Value then
  begin
    FState := Value;
    DoState;
    Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomCheckBox.UpdateChanges(Sender: TObject);
begin
  if csLoading in ComponentState then
    Exit;

  if FBorder.AutoColor then
    DoAutoColor;

  // ++++ DRAW TRANSPARENT BACKGOUND ++++
  // transparents must set befor do invalitate !!!
  if FFill.Transparent  then
  	ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];

  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomCheckBox.CMMouseEnter(var Message: TMessage);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomCheckBox.CMMouseLeave(var Message: TMessage);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomCheckBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
  begin
    if IsUnderlineChar(Caption, CharCode) then
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
procedure TCHCustomCheckBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomCheckBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomCheckBox.CMTextChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomCheckBox.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomCheckBox.SetColor(const Value: TColor);
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
procedure TCHCustomCheckBox.SetAllowGrayed(const Value: Boolean);
begin
  if FAllowGrayed <> Value then
  begin
    FAllowGrayed := Value;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomCheckBox.SetTextStyle(const Value: TTextStyle);
begin
  with FCaptionLayout do
  begin
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
procedure TCHCustomCheckBox.CMFocusChanged(var Message: TCMFocusChanged);
begin
  with Message do
  begin
    if (Sender is TCHCheckBox) then
    begin
      if (FFocus.Active = True) then
      begin
        FActive := Sender = Self;
        Invalidate;
      end
      else
        FActive := FDefault;
    end;
  end;

  inherited;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomCheckBox.CMExit(var Message: TCMExit);
begin
  with Message do
  begin
    FActive := False;
    Invalidate;
  end;

  inherited;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomCheckBox.WndProc(var Message: TMessage);
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
procedure TCHCustomCheckBox.DoAutoColor;
begin
  FBorder.HighlightColor := GetHighlightColor(Self.Color, FBorder.AutoPercent);
  FBorder.ShadowColor := GetShadowColor(Self.Color, FBorder.AutoPercent);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomCheckBox.CMMouseWheel(var Message: TCMMouseWheel);
begin
  with Message do
  begin
    Result := 0;
    if DoMouseWheel(ShiftState, WheelDelta, SmallPointToPoint(Pos)) then
      Message.Result := 1;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomCheckBox.DoMouseWheel(Shift: TShiftState;
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
function TCHCustomCheckBox.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheelDown) then
    FOnMouseWheelDown(Self, Shift, MousePos, Result);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomCheckBox.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheelUp) then
    FOnMouseWheelUp(Self, Shift, MousePos, Result);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomCheckBox.MakeForeground;
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
procedure TCHCustomCheckBox.Loaded;
begin
  inherited Loaded;
  UpdateChanges(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomCheckBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FChecked := True;
  DoGroup;

  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomCheckBox.DoGroup;
var
  nCtl : Integer;
  SelfCHK : TCHCheckBox;
begin
  if FChecked and (FGroupIndex > 0) then
  begin
    if Parent <> nil then
    begin
      for nCtl := 0 to Parent.ControlCount - 1 do
      begin
        if Parent.Controls[nCtl] is TCHCheckBox then
        begin
          SelfCHK := TCHCheckBox(Parent.Controls[nCtl]);
          if (SelfCHK <> Self) and (SelfCHK.GroupIndex = GroupIndex) then
            SelfCHK.Checked := False;
        end;
      end;
    end;
    if csDesigning in ComponentState then
    begin
      if (GetParentForm(self) <> nil) and (GetParentForm(self).Designer <> nil) then
        GetParentForm(self).Designer.Modified;
    end;
  end;

end;

function TCHCustomCheckBox.IsUnderlineChar(const Str: string;CC: Integer): Boolean;
var
  UnderlinePos : Integer;
  AKey, UnderlineText : string;
  bUnderline : Boolean;
const
  KU = '&';
begin
  try
    bUnderline := False;
    UnderlinePos := Pos(KU, Str);
    AKey := AnsiUpperCase(Char(CC));

    if UnderlinePos < Length(Str) then
    begin
      UnderlineText := AnsiUpperCase(Str[UnderlinePos + 1]);
      bUnderline := (UnderlinePos > 0) and
        (UnderlinePos < Length(Str)) and
        (AKey = UnderlineText);
    end;
  except
    bUnderline := False;
  end;
  Result := bUnderline;

end;

function TCHCustomCheckBox.GetHighlightColor(cColor: TColor;
  Percent: TPercent): TColor;
begin
  Result := RGB(Min(GetRValue(ColorToRGB(cColor)) + (Percent * 2), 255),
    Min(GetGValue(ColorToRGB(cColor)) + (Percent * 2), 255),
    Min(GetBValue(ColorToRGB(cColor)) + (Percent * 2), 255));
end;

function TCHCustomCheckBox.GetShadowColor(cColor: TColor;
  Percent: TPercent): TColor;
begin
  Result := RGB(Max(GetRValue(ColorToRGB(cColor)) - (Percent * 2), 0),
    Max(GetGValue(ColorToRGB(cColor)) - (Percent * 2), 0),
    Max(GetBValue(ColorToRGB(cColor)) - (Percent * 2), 0));
end;

end.
