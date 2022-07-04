
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit BtnCtrls;

interface

{$I STD.INC}

uses
  Classes, ImgList, Controls, Windows, Menus, Forms, Messages, SysUtils,
  StdCtrls, Graphics, GraphTools, StrTools, PopCtrls, ScrollCtrls,
  Dialogs, GraphThemes;

{ TCustomImageButton class }

type
  TDrawEvent = procedure(Control: TWinControl; Rect: TRect;
    State: TOwnerDrawState) of object;

  TButtonKind = (bkButton, bkMenuButton);
  TButtonStyle = (bsFramed, bsBeveled, bsFlat);
  TCaptionPosition = (cpLeft, cpRight, cpHide);
  TPressedState = (psNone, psButtonDown, psMenuButtonDown);

  TCustomImageButton = class(TCustomControl)
  private
    FAutoPopup: Boolean;
    FCaptionPosition: TCaptionPosition;
    FFocusedRect: Boolean;
    FImageDisabledIndex: Integer;
    FImageIndex: Integer;
    FImages: TCustomImageList;
    FKind: TButtonKind;
    FLocked: Boolean;
    FMouseInControl: Boolean;
    FOwnerDrawn: Boolean;
    FPressedState: TPressedState;
    FSharedImages: Boolean;
    FStyle: TButtonStyle;
    FOnMenuButtonClick: TNotifyEvent;
    FOnCustomDraw: TDrawEvent;
    function GetCaptionWidth: Integer;
		function GetImageList: TCustomImageList;
    function GetImageWidth: Integer;
    procedure SetCaptionPosition(Value: TCaptionPosition);
    procedure SetFocusedRect(Value: Boolean);
    procedure SetImageDisabledIndex(Value: Integer);
    procedure SetImageIndex(Value: Integer);
    procedure SetImages(Value: TCustomImageList);
    procedure SetKind(Value: TButtonKind);
    procedure SetOwnerDrawn(Value: Boolean);
    procedure SetSharedImages(Value: Boolean);
    procedure SetStyle(Value: TButtonStyle);
    procedure CMLostFocus(var Message: TCMLostFocus); message CM_LOSTFOCUS;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
  protected
    procedure AdjustBounds;
    procedure AdjustSize; override;
    procedure CustomDraw(Rect: TRect; State: TOwnerDrawState); virtual;
		procedure KeyDown(var Key: Word; Shift: TShiftState); override;
		procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MenuButtonClick; dynamic;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    property AutoPopup: Boolean read FAutoPopup write FAutoPopup;
    property CaptionPosition: TCaptionPosition read FCaptionPosition write
      SetCaptionPosition;
    property CaptionWidth: Integer read GetCaptionWidth;
    property FocusedRect: Boolean read FFocusedRect write SetFocusedRect default True;
    property ImageDisabledIndex: Integer read FImageDisabledIndex write SetImageDisabledIndex;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property ImageList: TCustomImageList read GetImageList;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageWidth: Integer read GetImageWidth;
    property Kind: TButtonKind read FKind write SetKind;
    property Locked: Boolean read FLocked write FLocked;
    property OwnerDrawn: Boolean read FOwnerDrawn write SetOwnerDrawn;
    property SharedImages: Boolean read FSharedImages write
      SetSharedImages;
    property Style: TButtonStyle read FStyle write SetStyle;
    property OnCustomDraw: TDrawEvent read FOnCustomDraw write FOnCustomDraw;
    property OnMenuButtonClick: TNotifyEvent read FOnMenuButtonClick write FOnMenuButtonClick;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    procedure Popup; dynamic;
  end;

{ TImageButton }

  TImageButton = class(TCustomImageButton)
  public
    property Canvas;
  published
    property Anchors;
    property ActionLink;
    property AutoSize;
    property AutoPopup;
    property Caption;
    property CaptionPosition;
    property Color;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusedRect;
    property Font;
    property ImageDisabledIndex;
    property ImageIndex;
    property Images;
    property Kind;
    property Locked;
    property OwnerDrawn;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property SharedImages;
    property Style;
    property TabOrder;
    property TabStop;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnCustomDraw;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMenuButtonClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TPopupButton }

  TPopupButton = class(TCustomImageButton)
  private
    FOnChange: TNotifyEvent;
    FPopupForm: TCustomPopupForm;
    procedure PopupFormSelect(Sender: TObject);
  protected
    procedure Change; virtual;
    function CreatePopup: TCustomPopupForm; virtual; abstract;
    procedure MenuButtonClick; override;
    property PopupForm: TCustomPopupForm read FPopupForm;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Popup; override;
    procedure Click; override;
  published
    property Anchors;
    property ActionLink;
    property AutoSize;
    property AutoPopup;
    property Color;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusedRect;
    property Font;
    property ImageIndex;
    property Images;
    property Kind;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Style;
    property TabOrder;
    property TabStop;
    property OnCanResize;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TColorGridButton }

  TColorGridButton = class(TPopupButton)
  private
   function GetActiveColor: TColor;
   procedure SetActiveColor(Value: TColor);
  protected
    function CreatePopup: TCustomPopupForm; override;
    procedure CustomDraw(Rect: TRect; State: TOwnerDrawState); override;
  published
    property ActiveColor: TColor read GetActiveColor write SetActiveColor;
  end;

{ TBrushButton }

  TBrushButton = class(TPopupButton)
  private
   function GetBrushStyle: TBrushStyle;
   procedure SetBrushStyle(Value: TBrushStyle);
   function GetMode: TListMode;
   procedure SetMode(Value: TListMode);
  protected
    function CreatePopup: TCustomPopupForm; override;
    procedure CustomDraw(Rect: TRect; State: TOwnerDrawState); override;
  published
    property BrushStyle: TBrushStyle read GetBrushStyle write SetBrushStyle;
    property Mode: TListMode read GetMode write SetMode;
  end;

{ TPenButton }

  TPenButton = class(TPopupButton)
  private
   function GetPenStyle: TPenStyle;
   procedure SetPenStyle(Value: TPenStyle);
   function GetMode: TListMode;
   procedure SetMode(Value: TListMode);
  protected
    function CreatePopup: TCustomPopupForm; override;
    procedure CustomDraw(Rect: TRect; State: TOwnerDrawState); override;
  published
    property PenStyle: TPenStyle read GetPenStyle write SetPenStyle;
    property Mode: TListMode read GetMode write SetMode;
  end;

{ TThemeGlyphButton }

	TThemeGlyphKind = (tgToolClose, tgClose, tgPin);

  TThemeGlyphButton = class(TGraphicControl)
  private
    FDown: Boolean;
    FKind: TThemeGlyphKind;
    FStyle: TButtonStyle;
    FMouseInControl: Boolean;
    procedure SetKind(Value: TThemeGlyphKind);
    procedure SetStyle(Value: TButtonStyle);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
	published
    property Align;
    property Color;
    property Kind: TThemeGlyphKind read FKind write SetKind;
    property Style: TButtonStyle read FStyle write SetStyle;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

{ The SharedImageList variable is a simple way to share a single image list with
  many controls. }

var
  SharedImageList: TCustomImageList;

implementation

{ TCustomImageButton }

const
  FLAT_SPACE     = 8;
  FLAT_MENUWIDTH = 12;

constructor TCustomImageButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentFont := True;
  ControlStyle := ControlStyle - [csClickEvents];
  Canvas.Brush.Color := clBtnFace;
  TabStop := True;
  Height := 22;
  Width := 75;
  FAutoPopup := True;
  FFocusedRect := True;
  FImageDisabledIndex := -1;
  FImageIndex := -1;
  FSharedImages := False;
end;

procedure TCustomImageButton.AdjustBounds;
var
  CaptionSize: Integer;
  ImageSize: Integer;
  ButtonWidth: Integer;
  ButtonHeight: Integer;
begin
  Invalidate;
  if (not (csLoading in ComponentState)) and AutoSize then
  begin
    CaptionSize := CaptionWidth;
    ImageSize := ImageWidth;
    ButtonWidth := CaptionSize + ImageSize + FLAT_SPACE * 2;
    if (CaptionSize > 0) and (ImageSize > 0) then
      Inc(ButtonWidth, FLAT_SPACE);
    if FKind = bkMenuButton then
      Inc(ButtonWidth, FLAT_MENUWIDTH);
    ButtonHeight := Height;
    if (ImageSize > 0) and (ButtonHeight < ImageList.Height + FLAT_SPACE) then
      ButtonHeight := ImageList.Height + FLAT_SPACE;
    SetBounds(Left, Top, ButtonWidth, ButtonHeight);
  end;
end;

procedure TCustomImageButton.AdjustSize;
begin
  if AutoSize then
    AdjustBounds
  else
    inherited AdjustSize;
end;

procedure TCustomImageButton.Click;
begin
  if AutoPopup then
    Popup;
  inherited Click;
end;

procedure TCustomImageButton.CustomDraw(Rect: TRect; State: TOwnerDrawState);
begin
  if Assigned(FOnCustomDraw) then
    FOnCustomDraw(Self, Rect, State);
end;

procedure TCustomImageButton.MenuButtonClick;
begin
  Popup;
  if Assigned(FOnMenuButtonClick) then
    FOnMenuButtonClick(Self);
end;

procedure TCustomImageButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
	inherited KeyDown(Key, Shift);
  if Key = VK_RETURN then
  begin
    Click;
    SetCapture(0);
    Key := 0;
  end
  else if (Key = VK_SPACE) and (FPressedState <> psButtonDown) then
	begin
		SetCapture(0);
    FPressedState := psButtonDown;
    Invalidate;
  end
  else if Key = VK_DOWN then
  	Popup;
end;

procedure TCustomImageButton.KeyUp(var Key: Word; Shift: TShiftState);
begin
	inherited KeyUp(Key, Shift);
  if (Key = VK_SPACE) or (Key = VK_RETURN) then
    if FPressedState = psButtonDown then
    begin
      MouseCapture := False;
      FPressedState := psNone;
      Invalidate;
      Click;
    end;
end;

procedure TCustomImageButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Rect: TRect;
begin
  if not (csDesigning in ComponentState) and
    (CanFocus or (GetParentForm(Self) = nil)) then
  begin
    SetFocus;
    SetCapture(Handle);
    if Button = mbLeft then
    begin
      Rect := ClientRect;
      if FKind = bkMenuButton then
      begin
        with Rect do
          Right := Right - FLAT_MENUWIDTH -1;
        if PtInRect(Rect, Point(X, Y)) then
          FPressedState := psButtonDown
        else
          FPressedState := psMenuButtonDown;
      end
        else
          FPressedState := psButtonDown;
      Invalidate;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomImageButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  PriorState: TPressedState;
begin
  if Button = mbLeft then
  begin
    PriorState := FPressedState;
    FPressedState := psNone;
    Invalidate;
    if PtInRect(ClientRect, Point(X, Y)) then
      case PriorState of
        psButtonDown:
          Click;
        psMenuButtonDown:
          MenuButtonClick;
      end;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomImageButton.Paint;

  function Pressed(State: TPressedState): Boolean;
  begin
    Result := (State = FPressedState) or ((FPressedState <> psNone) and FLocked);
  end;

  function GenImageIndex: Integer;
  begin
		Result := FImageIndex;
    if (not Enabled) and (FImageDisabledIndex > -1) then
    	Result := FImageDisabledIndex;
  end;

  function GenEnabled: Boolean;
  begin
		Result := Enabled or (FImageDisabledIndex > -1);
  end;

  procedure DrawFrameButton(Rect: TRect);
  begin
    InflateRect(Rect, -FLAT_SPACE, 0);
    if ImageWidth <> 0 then
      with Rect do
        if (FCaptionPosition = cpLeft) and (Caption <> '') and
          (FCaptionPosition <> cpHide) then
          begin
            Images.Draw(Canvas, Right -  ImageWidth, Top + (Bottom -
              Top - ImageList.Height) div 2, GenImageIndex, GenEnabled);
            Dec(Right, ImageWidth + FLAT_SPACE);
          end
          else
          begin
            Images.Draw(Canvas, Left , Top + (Bottom -
              Top - ImageList.Height) div 2, GenImageIndex, GenEnabled);
            Inc(Left, ImageWidth + FLAT_SPACE);
          end;
    OffsetRect(Rect, 0, -1);
    if (Caption <> '') and (FCaptionPosition <> cpHide) then ;
			DrawCaption(Canvas.Handle, Caption, Rect, drFill, Enabled);
    OffsetRect(Rect, 0, 1);
    { Draw focused rect }
    if Focused and FocusedRect and (Caption <> '') and
      (FCaptionPosition <> cpHide) then
    begin
      Rect := CalculateCaptionRect(Canvas.Handle, Caption, Rect, drFill);
      InflateRect(Rect, 1, 1);
      Canvas.DrawFocusRect(Rect);
    end;
  end;

  procedure DrawFrameMenu(Rect: TRect; DrawState: TDrawFrameState);
  var
    BevelRect: TRect;
  begin
    DrawFrame(Canvas.Handle, Rect, DrawState);
    if Pressed(psButtonDown) then
      OffsetRect(Rect, 1, 1);
    InflateRect(Rect, -2, 0);
    OffsetRect(Rect, 0, -1);
    if DrawState = dfPushed then
      OffsetRect(Rect, 1, 1);
    DrawArrow(Canvas.Handle, Rect, drDown, $000000, $FFFFFF, 15, Enabled);
    InflateRect(Rect, 2, 0);
    OffsetRect(Rect, 0, 1);
    if (FPressedState <> psMenuButtonDown) or FLocked then
    begin
      BevelRect := Rect;
      InflateRect(BevelRect, -3, -3);
      Dec(BevelRect.Left, 4);
      BevelRect.Right := BevelRect.Left + 2;
      DrawFrame(Canvas.Handle, BevelRect, dfLowered);
    end;
  end;

var
  Rect: TRect;
  ButtonRect: TRect;
  MenuRect: TRect;
  DrawState: TDrawState;
  OwnerDrawState: TOwnerDrawState;
begin
  Rect := ClientRect;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect);
  DrawState := [];
  if not Enabled then
    DrawState := DrawState + [dsDisabled];
  if (FPressedState = psButtonDown) or ((FPressedState = psMenuButtonDown) and FLocked) then
    DrawState := DrawState + [dsPressed];
  if FStyle = bsBeveled then
    DrawState := DrawState + [dsThin]
 	else if FStyle = bsFlat then
    DrawState := DrawState + [dsFlat];
  if Focused or FMouseInControl then
    DrawState := DrawState + [dsHot];
  if DrawState - [dsDisabled] = [dsFlat] then
    //
  else
    DrawThemeButton(Canvas.Handle, Rect, DrawState);
  ButtonRect := Rect;
  if FKind = bkMenuButton then
    Dec(ButtonRect.Right, FLAT_MENUWIDTH);
  if (FPressedState = psButtonDown) or ((FPressedState = psMenuButtonDown) and FLocked) then
    OffsetRect(ButtonRect, 1, 1);
  if OwnerDrawn then
  begin
    OwnerDrawState := [];
    if not Enabled then
      Include(OwnerDrawState, odDisabled);
    if Focused then
      OwnerDrawState := OwnerDrawState + [odSelected, odFocused];
    CustomDraw(ButtonRect, OwnerDrawState);
  end
  else
    DrawFrameButton(ButtonRect);
  if FKind = bkMenuButton then
  begin
    MenuRect := Rect;
    with MenuRect do
      Left := Right - FLAT_MENUWIDTH;
    if (FPressedState = psMenuButtonDown) and (not FLocked) then
      DrawFrameMenu(MenuRect, dfPushed)
    else
      DrawFrameMenu(MenuRect, dfFlat)
  end;
end;

procedure TCustomImageButton.Popup;
var
	Handled: Boolean;
begin
  if PopupMenu <> nil then
  begin
  	Handled := False;
    if Assigned(OnContextPopup) then
    	OnContextPopup(Self, Mouse.CursorPos, Handled);
    if not Handled then
    	with ClientToScreen(Classes.Point(-1, Height + 1)) do
      	PopupMenu.Popup(X, Y);
	end;
end;

procedure TCustomImageButton.SetCaptionPosition(Value: TCaptionPosition);
begin
  if Value <> FCaptionPosition then
  begin
    FCaptionPosition := Value;
    AdjustBounds;
  end;
end;

function TCustomImageButton.GetCaptionWidth: Integer;
begin
  Result := 0;
  if (Caption <> '') and (FCaptionPosition <> cpHide) then
    Result := Canvas.TextWidth(Caption);
end;

procedure TCustomImageButton.SetOwnerDrawn(Value: Boolean);
begin
  if Value <> FOwnerDrawn then
  begin
    FOwnerDrawn := Value;
    Invalidate;
  end;
end;

procedure TCustomImageButton.SetFocusedRect(Value: Boolean);
begin
  if Value <> FFocusedRect then
  begin
    FFocusedRect := Value;
    Invalidate;
  end;
end;

procedure TCustomImageButton.SetImageDisabledIndex(Value: Integer);
begin
  if Value <> FImageDisabledIndex then
  begin
    FImageDisabledIndex := Value;
    Invalidate;
  end;
end;

procedure TCustomImageButton.SetImageIndex(Value: Integer);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TCustomImageButton.SetImages(Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    FImages := Value;
    AdjustBounds;
  end;
end;

function TCustomImageButton.GetImageList: TCustomImageList;
begin
  if FSharedImages then
    Result := SharedImageList
  else
    Result := FImages;
end;

function TCustomImageButton.GetImageWidth: Integer;
begin
  Result := 0;
  if ImageList <> nil then
    Result := ImageList.Width;
end;

procedure TCustomImageButton.SetKind(Value: TButtonKind);
begin
  if Value <> FKind then
  begin
    FKind := Value;
    AdjustBounds;
  end;
end;

procedure TCustomImageButton.SetSharedImages(Value: Boolean);
begin
  if Value <> FSharedImages then
  begin
    FSharedImages := Value;
    AdjustBounds;
  end;
end;

procedure TCustomImageButton.SetStyle(Value: TButtonStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    AdjustBounds;
  end;
end;

procedure TCustomImageButton.CMLostFocus(var Message: TCMLostFocus);
begin
  ShowMessage('CMLostFocus');
end;

procedure TCustomImageButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and Enabled and Visible and
      (Parent <> nil) and Parent.Showing then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

procedure TCustomImageButton.CMEnabledChanged(var Message: TMessage);
begin
  Invalidate;
  inherited;
end;

procedure TCustomImageButton.CMFocusChanged(var Message: TCMFocusChanged);
begin
  FMouseInControl := False;
  FPressedState := psNone;
  Invalidate;
  inherited;
end;

procedure TCustomImageButton.CMFontChanged(var Message: TMessage);
begin
  Canvas.Font := Font;
  Invalidate;
  AdjustBounds;
  inherited;
end;

procedure TCustomImageButton.CMMouseEnter(var Message: TMessage);
begin
  FMouseInControl := True;
  if (Style = bsFlat) or ThemesLoaded then
    Invalidate;
  inherited;
end;

procedure TCustomImageButton.CMMouseLeave(var Message: TMessage);
begin
  FMouseInControl := False;
  if (Style = bsFlat) or ThemesLoaded then
    Invalidate;
  inherited;
end;

procedure TCustomImageButton.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
  AdjustBounds;
  inherited;
end;

procedure TCustomImageButton.WMKillFocus(var Message: TWMKillFocus);
begin
  MouseCapture := False;
  FPressedState := psNone;
  Invalidate;
  inherited;
end;

procedure TCustomImageButton.WMSetFocus(var Message: TWMSetFocus);
begin
  MouseCapture := False;
  FPressedState := psNone;
  Invalidate;
  inherited;
end;

{ TPopupButton }

constructor TPopupButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CaptionPosition := cpHide;
  Kind := bkMenuButton;
  Locked := True;
  OwnerDrawn := True;
  FPopupForm := CreatePopup;
  with FPopupForm do
  begin
    Associate := Self;
    OnSelect := PopupFormSelect;
  end;
end;

procedure TPopupButton.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TPopupButton.Click;
begin
  inherited Click;
  Popup;
end;

procedure TPopupButton.MenuButtonClick;
begin
  Click;
end;

procedure TPopupButton.Popup;
begin
  FPopupForm.Popup;
end;

procedure TPopupButton.PopupFormSelect(Sender: TObject);
begin
  Invalidate;
  Change;
end;

{ TColorGridButton }

function TColorGridButton.CreatePopup: TCustomPopupForm;
begin
  Result := TPopupColorGridForm.Create(Self);
end;

procedure TColorGridButton.CustomDraw(Rect: TRect; State: TOwnerDrawState);
var
  DC: HDC;
  Pen: HPEN;
  Brush: HBRUSH;
begin
  DC := Canvas.Handle;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  InflateRect(Rect, -3, -3);
  if FocusedRect and (odFocused in State) then
    DrawFocusRect(DC, Rect);
  InflateRect(Rect, -2, -2);
  Brush := Windows.CreateSolidBrush(Graphics.ColorToRGB(ActiveColor));
  FillRect(DC, Rect, Brush);
  DeleteObject(Brush);
  Pen := SelectObject(DC, GetStockObject(BLACK_PEN));
  with Rect do
  begin
    MoveToEx(DC, Left, Top, nil);
    LineTo(DC, Right - 1, Top);
    LineTo(DC, Right - 1, Bottom - 1);
    LineTo(DC, Left, Bottom - 1);
    LineTo(DC, Left, Top);
  end;
  SelectObject(DC, Pen);
end;

function TColorGridButton.GetActiveColor: TColor;
begin
  Result := (PopupForm as TPopupColorGridForm).ActiveColor;
end;

procedure TColorGridButton.SetActiveColor(Value: TColor);
begin
  (PopupForm as TPopupColorGridForm).ActiveColor := Value;
end;

{ TBrushButton }

function TBrushButton.CreatePopup: TCustomPopupForm;
begin
  Result := TPopupBrushForm.Create(Self);
end;

procedure TBrushButton.CustomDraw(Rect: TRect; State: TOwnerDrawState);
var
  DC: HDC;
  DrawPen: HPEN;
  DrawBrush: HBRUSH;
begin
  DC := Canvas.Handle;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  InflateRect(Rect, -3, -3);
  if FocusedRect and (odFocused in State) then
    DrawFocusRect(DC, Rect);
  InflateRect(Rect, -2, -2);
  DrawBrush := GetStockObject(NULL_BRUSH);
  case (PopupForm as TPopupBrushForm).BrushStyle of
    bsSolid: DrawBrush := CreateSolidBrush(0);
    bsClear: DrawBrush := GetStockObject(NULL_BRUSH);
    bsHorizontal: DrawBrush := CreateHatchBrush(HS_HORIZONTAL, 0);
    bsVertical: DrawBrush := CreateHatchBrush(HS_VERTICAL, 0);
    bsFDiagonal: DrawBrush := CreateHatchBrush(HS_FDIAGONAL, 0);
    bsBDiagonal: DrawBrush := CreateHatchBrush(HS_BDIAGONAL, 0);
    bsCross: DrawBrush := CreateHatchBrush(HS_CROSS, 0);
    bsDiagCross: DrawBrush := CreateHatchBrush(HS_DIAGCROSS, 0);
  end;
  FillRect(DC, Rect, DrawBrush);
  if (PopupForm as TPopupBrushForm).BrushStyle <> bsClear then
    DeleteObject(DrawBrush);
  DrawPen := SelectObject(DC, GetStockObject(BLACK_PEN));
  with Rect do
  begin
    MoveToEx(DC, Left, Top, nil);
    LineTo(DC, Right - 1, Top);
    LineTo(DC, Right - 1, Bottom - 1);
    LineTo(DC, Left, Bottom - 1);
    LineTo(DC, Left, Top);
  end;
  SelectObject(DC, DrawPen);
end;

function TBrushButton.GetBrushStyle: TBrushStyle;
begin
  Result := (PopupForm as TPopupBrushForm).BrushStyle;
end;

procedure TBrushButton.SetBrushStyle(Value: TBrushStyle);
begin
  (PopupForm as TPopupBrushForm).BrushStyle := Value;
end;

function TBrushButton.GetMode: TListMode;
begin
  Result := (PopupForm as TPopupBrushForm).Mode;
end;

procedure TBrushButton.SetMode(Value: TListMode);
begin
  (PopupForm as TPopupBrushForm).Mode := Value;
end;

{ TPenButton }

function TPenButton.CreatePopup: TCustomPopupForm;
begin
  Result := TPopupPenForm.Create(Self);
end;

procedure TPenButton.CustomDraw(Rect: TRect; State: TOwnerDrawState);
var
  DC: HDC;
  DrawPen: HPEN;
begin
  DC := Canvas.Handle;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  InflateRect(Rect, -3, -3);
  if FocusedRect and (odFocused in State) then
    DrawFocusRect(DC, Rect);
  InflateRect(Rect, -2, -2);
  DrawPen := 0;
  case (PopupForm as TPopupPenForm).PenStyle of
    psSolid: DrawPen := CreatePen(PS_SOLID, 1, 0);
    psDash: DrawPen := CreatePen(PS_DASH, 1, 0);
    psDot: DrawPen := CreatePen(PS_DOT, 1, 0);
    psDashDot: DrawPen := CreatePen(PS_DASHDOT, 1, 0);
    psDashDotDot: DrawPen := CreatePen(PS_DASHDOTDOT, 1, 0);
    psClear: DrawPen := CreatePen(PS_NULL, 1, 0);
    psInsideFrame: DrawPen := CreatePen(PS_INSIDEFRAME, 1, 0);
  end;
  DrawPen := SelectObject(DC, DrawPen);
  with Rect do
  begin
    MoveToEx(DC, Left, Top + (Bottom - Top) div 2 - 1, nil);
    LineTo(DC, Left, Top + (Bottom - Top) div 2 - 1);
    MoveToEx(DC, Left, Top + (Bottom - Top) div 2, nil);
    LineTo(DC, Right, Top + (Bottom - Top) div 2);
    MoveToEx(DC, Left, Top + (Bottom - Top) div 2 + 1, nil);
    LineTo(DC, Right, Top + (Bottom - Top) div 2 + 1);
  end;
  OverwriteObject(DC, DrawPen);
end;

function TPenButton.GetPenStyle: TPenStyle;
begin
  Result := (PopupForm as TPopupPenForm).PenStyle;
end;

procedure TPenButton.SetPenStyle(Value: TPenStyle);
begin
  (PopupForm as TPopupPenForm).PenStyle := Value;
end;

function TPenButton.GetMode: TListMode;
begin
  Result := (PopupForm as TPopupPenForm).Mode;
end;

procedure TPenButton.SetMode(Value: TListMode);
begin
  (PopupForm as TPopupPenForm).Mode := Value;
end;

{ TThemeGlyphButton }

constructor TThemeGlyphButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
	ControlStyle := [csCaptureMouse, csClickEvents, csFixedWidth, csFixedHeight];
  Color := clWindowFrame;
  Width := 22;
  Height := 22;
end;

destructor TThemeGlyphButton.Destroy;
begin
  inherited Destroy;
end;

procedure TThemeGlyphButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
	if Button = mbLeft then
  begin
  	FDown := True;
    Invalidate;
  end;
end;

procedure TThemeGlyphButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
	if Button = mbLeft then
  begin
  	FDown := False;
    Invalidate;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TThemeGlyphButton.Paint;
var
	DC: HDC;
	Rect: TRect;
  State: TDrawState;
begin
  inherited Paint;
  DC := Canvas.Handle;
	Rect := ClientRect;
  if not Enabled then
  	State := [dsDisabled]
  else if FDown and FMouseInControl then
  	State := [dsPressed]
	else if FMouseInControl then
		State := [dsHot]
	else
  	State := [];
  if FStyle = bsBeveled then
  	State := State + [dsThin]
  else if FStyle = bsFlat then
  	State := State + [dsFlat];
	case FKind of
  	tgToolClose: DrawThemeToolClose(DC, Rect, State, Color);
    tgClose: DrawThemeClose(DC, Rect, State, Color);
    tgPin: DrawThemePin(DC, Rect, State, Color);
	end;
end;

procedure TThemeGlyphButton.SetKind(Value: TThemeGlyphKind);
begin
	if Value <> FKind then
  begin
  	FKind := Value;
    Invalidate;
  end;
end;

procedure TThemeGlyphButton.SetStyle(Value: TButtonStyle);
begin
	if Value <> FStyle then
  begin
  	FStyle := Value;
    Invalidate;
  end;
end;

procedure TThemeGlyphButton.CMEnabledChanged(var Message: TMessage);
begin
	inherited;
  FDown := False;
  FMouseInControl := False;
  Invalidate;
end;

procedure TThemeGlyphButton.CMMouseEnter(var Message: TMessage);
begin
	inherited;
  FMouseInControl := True;
  Invalidate;
end;

procedure TThemeGlyphButton.CMMouseLeave(var Message: TMessage);
begin
	inherited;
  FMouseInControl := False;
  Invalidate;
end;

procedure TThemeGlyphButton.CMSysColorChange(var Message: TMessage);
begin
	inherited;
  Invalidate;
end;

end.
