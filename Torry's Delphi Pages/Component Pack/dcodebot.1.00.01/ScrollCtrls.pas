
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit ScrollCtrls;

interface

{$I STD.INC}

uses
  Classes, Controls, Forms, Graphics, Messages, SysUtils, Windows, StdCtrls,
  ImgList, GraphTools;

{ TControlHintWindow }

type
  TControlHintWindow = class(THintWindow)
  private
    FActive: Boolean;
    FControl: TControl;
    FPoint: TPoint;
    procedure SetActive(Value: Boolean);
    procedure SetControl(Value: TControl);
    procedure SetPoint(const Value: TPoint);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
    property Active: Boolean read FActive write SetActive;
    property Control: TControl read FControl write SetControl;
    property Point: TPoint read FPoint write SetPoint;
  end;

{ TScrollList }

  TDrawStateEvent = procedure(Control: TWinControl; Index: Integer;
    Rect: TRect; State: TDrawState) of object;

  TScrollDir = (sdNone, sdUp, sdDown);

  TScrollList = class(TCustomControl)
  private
    FBorderStyle: TBorderStyle;
    FCount: Integer;
    FHintWindow: TControlHintWindow;
    FItemHeight: Integer;
    FItemIndex: Integer;
    FTopIndex: Integer;
    FScrolling: Boolean;
    FOnSelectItem: TNotifyEvent;
    procedure UpdateScrollRange;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetCount(Value: Integer);
    procedure SetItemHeight(Value: Integer);
    procedure SetScrollIndex(Value: Integer);
    procedure SetItemIndex(Value: Integer);
    procedure SetScrolling(Value: Boolean);
    procedure SetTopIndex(Value: Integer);
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMCaptureChanged(var Message: TWMNoParams); message WM_CAPTURECHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
		procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    procedure WMVScroll(var Message: TWMScroll); message WM_VSCROLL;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
  protected
    procedure CreateHandle; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CaptureMove(X, Y: Integer); virtual;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure DrawItem(Index: Integer; var Rect: TRect; State: TDrawState); virtual;
    procedure InvalidateItem(Item: Integer);
    function ItemRect(Item: Integer): TRect;
    procedure Scroll(Delta: Integer); virtual;
    procedure ScrollMove(Distance: Integer; Direction: TScrollDir); virtual;
    procedure SelectItem(PriorIndex: Integer; NewIndex: Integer; var CanSelect: Boolean); virtual;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Count: Integer read FCount write SetCount;
    property HintWindow: TControlHintWindow read FHintWindow;
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 16;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property Scrolling: Boolean read FScrolling write SetScrolling;
    property TopIndex: Integer read FTopIndex write SetTopIndex;
    property OnSelectItem: TNotifyEvent read FOnSelectItem write FOnSelectItem;
  public
    constructor Create(AOwner: TComponent); override;
    function ItemAtPos(const Pos: TPoint; Existing: Boolean = False): Integer;
    procedure ScrollToSelection;
  end;

{ TCustomDrawList }

  TCustomDrawList = class(TScrollList)
  private
    FAutoScroll: Boolean;
    FOnDrawBackground: TNotifyEvent;
    FOnDrawItem: TDrawStateEvent;
    procedure SetAutoScroll(Value: Boolean);
  protected
    procedure Paint; override;
    procedure DrawBackground; virtual;
    procedure DrawItem(Index: Integer; var Rect: TRect;
      State: TDrawState); override;
    procedure Scroll(Delta: Integer); override;
    property AutoScroll: Boolean read FAutoScroll write SetAutoScroll;
    property OnDrawBackground: TNotifyEvent read FOnDrawBackground write FOnDrawBackground;
    property OnDrawItem: TDrawStateEvent read FOnDrawItem write FOnDrawItem;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TDrawList }

  TDrawList = class(TCustomDrawList)
  public
    property Canvas;
    property Count;
    property TopIndex;
    property ItemIndex;
  published
    property Align;
    property Anchors;
    property AutoScroll;
    property BorderStyle;
    property Color;
    property DesktopFont;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ItemHeight;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawBackground;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectItem;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TCustomBubbleList }

  TCustomBubbleList = class(TCustomDrawList)
  private
    FClicked: Boolean;
    FKeyDown: Boolean;
    FDownIndex: Integer;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
  protected
    procedure Click; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DrawBackground; override;
    procedure DrawItem(Index: Integer; var Rect: TRect;
      State: TDrawState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure SelectItem(PriorIndex, NewIndex: Integer;
      var CanSelect: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TImageDrawList }

  { TBubbleList }

  TBubbleList = class(TCustomBubbleList)
  public
    property Canvas;
    property Count;
    property TopIndex;
    property ItemIndex;
  published
    property Align;
    property Anchors;
    property BorderStyle;
    property Color;
    property DesktopFont;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ItemHeight;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property OnCanResize;
    property OnDblClick;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectItem;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TImageDrawList }

  TImageDrawList = class(TCustomDrawList)
  private
		FChangeLink: TChangeLink;
    FDisplayEmpty: Boolean;
  	FImages: TCustomImageList;
		procedure ImagesChange(Sender: TObject);
    procedure UpdateImages(var InternalImages: TCustomImageList;
      ExternalImages: TCustomImageList);
    procedure SetDisplayEmpty(Value: Boolean);
    function GetLineHeight: Integer;
    procedure SetImages(Value: TCustomImageList);
    function GetImageIndex: Integer;
    procedure SetImageIndex(Value: Integer);
	protected
    procedure DrawItem(Index: Integer; var Rect: TRect;
      State: TDrawState); override;
		procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
  	constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  	property ImageIndex: Integer read GetImageIndex write SetImageIndex;
    property LineHeight: Integer read GetLineHeight;
  published
  	property BorderStyle;
    property DisplayEmpty: Boolean read FDisplayEmpty write SetDisplayEmpty;
  	property Images: TCustomImageList read FImages write SetImages;
    property TabOrder;
  	property TabStop;
    property OnClick;
    property OnSelectItem;
	end;

implementation

{ TControlHintWindow }

constructor TControlHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clWindow;
end;

procedure TControlHintWindow.ActivateHint(Rect: TRect; const AHint: string);
begin
  FActive := True;
  if IsRectEmpty(Rect) then
  begin
    Rect := CalcHintRect(High(Integer), AHint, nil);
    if FControl <> nil then
      with FControl.ClientToScreen(FPoint) do
        OffsetRect(Rect, x - 4, y - 3)
    else
      OffsetRect(Rect, FPoint.x, FPoint.y);
  end;
  inherited ActivateHint(Rect, AHint);
end;

procedure TControlHintWindow.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    if FActive then
      ActivateHint(Rect(0, 0, 0, 0), Caption)
    else
      ShowWindow(Handle, SW_HIDE);
  end;
end;

procedure TControlHintWindow.SetControl(Value: TControl);
begin
  FControl := Value;
  Active := False;
end;

procedure TControlHintWindow.SetPoint(const Value: TPoint);
begin
  if (Value.x <> FPoint.x) or (Value.y <> FPoint.y) then
  begin
    FPoint := Value;
    Active := False;
  end;
end;

{ TScrollList }

constructor TScrollList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDoubleClicks];
  Color := clBtnFace;
  ParentColor := False;
  Width := 100;
  Height := 200;
  FBorderStyle := bsSingle;
  FHintWindow := TControlHintWindow.Create(Self);
  FHintWindow.Control := Self;
  FItemHeight := 15;
  FItemIndex := -1;
end;

procedure TScrollList.CreateHandle;
begin
  inherited CreateHandle;
  UpdateScrollRange;
end;

procedure TScrollList.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := (Style or WS_VSCROLL) and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end
    else
      Style := Style or WS_VSCROLL or BorderStyles[FBorderStyle];
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TScrollList.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then RecreateWnd;
  inherited;
end;

procedure TScrollList.CMMouseLeave(var Message: TMessage);
begin
  FHintWindow.Active := False;
  inherited;
end;

procedure TScrollList.WMCaptureChanged(var Message: TWMNoParams);
begin
  inherited;
  FScrolling := GetCapture = Handle;
end;

procedure TScrollList.WMMouseWheel(var Message: TWMMouseWheel);
var
  Delta: Integer;
begin
  Delta := -Message.WheelDelta div 120;
  if ItemIndex > -1 then
    if ItemIndex + Delta < 0 then
      ItemIndex := 0
    else if ItemIndex + Delta > Count - 1 then
      ItemIndex := Count - 1
    else
      ItemIndex := ItemIndex + Delta;
  inherited;
end;

procedure TScrollList.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TScrollList.WMNCPaint(var Message: TMessage);
var
  DC: HDC;
  Rect: TRect;
begin
  inherited;
  if FBorderStyle = bsSingle then
  begin
    DC := GetWindowDC(Handle);
    Rect := Classes.Rect(0, 0, Width, Height);
		ExcludeClipRect(DC, Rect.Left + 2, Rect.Top + 2, Rect.Right - 2, Rect.Bottom - 2);
    DrawThemeBorder(DC, Color, Rect, []);
    ReleaseDC(Handle, DC);
    Message.Result := 0;
  end;
end;

procedure TScrollList.WMSize(var Message: TWMSize);
begin
  inherited;
  UpdateScrollRange;
end;

procedure TScrollList.WMTimer(var Message: TWMTimer);
var
  Point: TPoint;
  ScrollDir: TScrollDir;
  Distance: Integer;
begin
  if FScrolling then
  begin
    GetCursorPos(Point);
    Windows.ScreenToClient(Handle, Point);
    ScrollDir := sdNone;
    Distance := 0;
    with Point do
      if Y < 0 then
      begin
        Distance := -Y div FItemHeight + 1;
        ScrollDir := sdUp;
      end
      else if Y > ClientHeight then
      begin
        Distance := (Y - ClientHeight) div FItemHeight + 1;
        ScrollDir := sdDown;
      end;
    if ScrollDir <> sdNone then
      ScrollMove(Distance, ScrollDir)
    else
    begin
      FScrolling := False;
      KillTimer(Handle, Message.TimerID);
    end;
  end
  else
    KillTimer(Handle, Message.TimerID);
end;

procedure TScrollList.WMVScroll(var Message: TWMScroll);
begin
  with Message do
    case ScrollCode of
      SB_BOTTOM: SetTopIndex(FCount - 1);
      SB_LINEDOWN: SetTopIndex(FTopIndex + 1);
      SB_LINEUP: SetTopIndex(FTopIndex - 1);
      SB_PAGEDOWN: SetTopIndex(FTopIndex + ClientHeight div FItemHeight);
      SB_PAGEUP: SetTopIndex(FTopIndex - ClientHeight div FItemHeight);
      SB_THUMBTRACK:
        begin
          {if Pos < 0 then
            Pos := Count;}
          SetTopIndex(Pos);
        end;
      SB_TOP: SetTopIndex(0);
    end;
end;

procedure TScrollList.WMKillFocus(var Message: TWMKillFocus);
begin
end;

procedure TScrollList.CaptureMove(X, Y: Integer);
begin
  if MouseCapture then
    if (Y < 0) or (Y > ClientHeight) then
      Scrolling := True
    else
      SetScrollIndex(ItemAtPos(Point(X, Y)));
end;

procedure TScrollList.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  ScrollToSelection;
end;

procedure TScrollList.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_NEXT: SetScrollIndex(ItemIndex + ClientHeight div FItemHeight);
    VK_PRIOR: SetScrollIndex(ItemIndex - ClientHeight div FItemHeight);
    VK_UP: SetScrollIndex(ItemIndex - 1);
    VK_DOWN: SetScrollIndex(ItemIndex + 1);
  end;
end;

procedure TScrollList.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FHintWindow.Active := False;
  if Button = mbLeft then
  begin
    SetFocus;
    CaptureMove(X, Y);
  end;
end;

procedure TScrollList.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if MouseCapture then
    CaptureMove(X, Y);
end;

procedure TScrollList.Paint;
var
  UpdateRect: TRect;
  DrawState: TDrawState;
  I: Integer;
begin
  UpdateRect := Canvas.ClipRect;
  with UpdateRect do
  begin
    Left := 0;
    Right := ClientWidth;
    Top := (Top div FItemHeight) * FItemHeight;
    if Bottom mod FItemHeight > 0 then
      Bottom := Bottom + FItemHeight;
    Bottom := (Bottom div FItemHeight) * FItemHeight;
    for I := Top div FItemHeight to Bottom div FItemHeight do
    begin
      if I + FTopIndex > FCount - 1 then
        Break;
      Top := I * FItemHeight;
      Bottom := Top + FItemHeight;
      DrawState := [];
      if FTopIndex + I = ItemIndex then
      begin
        Include(DrawState, dsHot);
        if Focused then
          Include(DrawState, dsFocused);
      end;
      DrawItem(FTopIndex + I, UpdateRect, DrawState);
    end;
    Top := Bottom;
  end;
end;

procedure TScrollList.DrawItem(Index: Integer; var Rect: TRect;
  State: TDrawState);
begin
end;

procedure TScrollList.InvalidateItem(Item: Integer);
var
  Rect: TRect;
begin
  if HandleAllocated then
  begin
    Rect := ItemRect(Item);
    InvalidateRect(Handle, @Rect, True);
  end;
end;

function TScrollList.ItemRect(Item: Integer): TRect;
begin
  Result := Classes.Rect(0, (Item - FTopIndex) * FItemHeight, ClientWidth,
    (Item - FTopIndex + 1) * FItemHeight);
end;

procedure TScrollList.Scroll(Delta: Integer);
begin
  ScrollBy(0, Delta);
end;

procedure TScrollList.ScrollMove(Distance: Integer; Direction: TScrollDir);
const
  Movement: array[TScrollDir] of Integer = (0, -1, 1);
begin
  if Distance > 0 then
    SetScrollIndex(ItemIndex + Distance * Movement[Direction]);
end;

procedure TScrollList.SelectItem(PriorIndex: Integer; NewIndex: Integer;
  var CanSelect: Boolean);
begin
  if CanSelect then
  begin
    FItemIndex := NewIndex;
    if Assigned(FOnSelectItem) then
      FOnSelectItem(Self);
  end;
end;

procedure TScrollList.ScrollToSelection;
begin
  if FItemIndex < FTopIndex then
    SetTopIndex(FItemIndex)
  else if FItemIndex >= FTopIndex + (ClientHeight + 1) div FItemHeight  then
    SetTopIndex(FItemIndex - (ClientHeight - 1) div FItemHeight);
  InvalidateItem(FItemIndex);
end;

function TScrollList.ItemAtPos(const Pos: TPoint;
  Existing: Boolean = False): Integer;
begin
  Result := FTopIndex + Pos.y div FItemHeight;
  if Result > FCount - 1 then
    if Existing then Result := -1 else Result := FCount - 1;
end;

procedure TScrollList.UpdateScrollRange;
var
  ScrollInfo: TScrollInfo;
begin
  if Parent <> nil then HandleNeeded;
  if HandleAllocated then
    with ScrollInfo do
    begin
      cbSize := SizeOf(TScrollInfo);
      fMask := SIF_PAGE or SIF_POS or SIF_RANGE;
      nMin := 0;
      nMax := FCount - 1;
      nPage := ClientHeight div FItemHeight;
      nPos := FTopIndex;
      SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
      if FCount - FTopIndex < Integer(nPage) then SetTopIndex(FCount -
        Integer(nPage));
    end;
end;

procedure TScrollList.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TScrollList.SetCount(Value: Integer);
begin
  if Value <> FCount then
  begin
    if Value < 0 then
      Value := 0;
    FCount := Value;
    FItemIndex := -1;
    if FCount > 0 then
      ItemIndex := 0;
    UpdateScrollRange;
    Invalidate;
  end;
end;

procedure TScrollList.SetItemHeight(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  if Value <> FItemHeight then
  begin
    FItemHeight := Value;
    UpdateScrollRange;
    Invalidate;
  end;
end;

procedure TScrollList.SetScrollIndex(Value: Integer);
begin
  { TODO: Scroll window to bottom }
  if Count = 0 then
    SetItemIndex(-1)
  else if Value > Count - 1 then
  begin
    SetItemIndex(Count - 1);
    SetTopIndex(FTopIndex + 1);
  end
  else if Value < 0 then
    SetItemIndex(0)
  else
    SetItemIndex(Value);
end;

procedure TScrollList.SetItemIndex(Value: Integer);
var
  PriorIndex: Integer;
  CanSelect: Boolean;
begin
  if Value > Count - 1 then
    Value := Count - 1;
  if Value <> FItemIndex then
  begin
    PriorIndex := FItemIndex;
    if not HandleAllocated then Exit;
    CanSelect := True;
    SelectItem(FItemIndex, Value, CanSelect);
    if CanSelect then
    begin
      if PriorIndex > -1 then
        InvalidateItem(PriorIndex);
      FItemIndex := Value;
      ScrollToSelection;
    end;
  end;
end;

procedure TScrollList.SetScrolling(Value: Boolean);
begin
  if Value <> FScrolling then
  begin
    FScrolling := Value;
    if FScrolling then
      SetTimer(Handle, 0, 60, nil);
  end;
end;

procedure TScrollList.SetTopIndex(Value: Integer);
var
  ScrollInfo: TScrollInfo;
  Delta: Integer;
begin
  if Value > FCount - ClientHeight div FItemHeight then
    Value := FCount - ClientHeight div FItemHeight;
  if Value < 0 then
    Value := 0;
  if Value <> FTopIndex then
  begin
    Delta := (FTopIndex - Value) * FItemHeight;
    FTopIndex := Value;
    ScrollInfo.cbSize := Sizeof(TScrollInfo);
    ScrollInfo.fMask := SIF_POS;
    ScrollInfo.nPos := FTopIndex;
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
    Scroll(Delta);
  end;
end;

{ TCustomDrawList }

constructor TCustomDrawList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoScroll := True;
end;

procedure TCustomDrawList.Paint;
begin
  DrawBackground;
  inherited Paint;
end;

procedure TCustomDrawList.DrawBackground;
begin
  if Assigned(FOnDrawBackground) then
    FOnDrawBackground(Self);
end;

procedure TCustomDrawList.DrawItem(Index: Integer; var Rect: TRect; 
  State: TDrawState);
begin
  if Assigned(FOnDrawItem) then
    FOnDrawItem(Self, Index, Rect, State);
end;

procedure TCustomDrawList.Scroll(Delta: Integer);
var
  Rect: TRect;
begin
  if FAutoScroll then
    inherited Scroll(Delta)
  else
  begin
    Rect := ClientRect;
    InvalidateRect(Handle, @Rect, False);
  end;
end;

procedure TCustomDrawList.SetAutoScroll(Value: Boolean);
begin
  if Value <> FAutoScroll then
  begin
    FAutoScroll := Value;
    DoubleBuffered := not Value;
  end;
end;

{ TCustomBubbleList }

constructor TCustomBubbleList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csDoubleClicks];
  FDownIndex := -1;
end;

procedure TCustomBubbleList.Click;
begin
  if FClicked then inherited Click;
end;

procedure TCustomBubbleList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style or CS_HREDRAW;
end;

procedure TCustomBubbleList.DrawBackground;
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
end;

procedure TCustomBubbleList.DrawItem(Index: Integer; var Rect: TRect;
  State: TDrawState);
var
  Brush: HBRUSH;
  DC: HDC;
begin
  DC := Canvas.Handle;
  Brush := GetBrush(Color);
  FillRect(DC, Rect, Brush);
  DeleteObject(Brush);
  State := State + [dsFlat];
  if not Enabled then
    State := State + [dsDisabled];
  if Index = FDownIndex then
    State := State + [dsPressed];
  DrawThemeThinButton(DC, Rect, State);
  if dsPressed in State then
    OffsetRect(Rect, 1, 1);
  inherited DrawItem(Index, Rect, State);
end;

procedure TCustomBubbleList.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if ItemIndex > -1 then
    case Key of
      VK_RETURN:
        try
          FClicked := True;
          Click;
        finally
          FClicked := False;
        end;
      VK_SPACE:
        if not FKeyDown then
        begin
          FKeyDown := True;
          FDownIndex := ItemIndex;
          if FDownIndex > -1 then
            InvalidateItem(ItemIndex);
        end;
    end;
end;

procedure TCustomBubbleList.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if ItemIndex > -1 then
    case Key of
      VK_SPACE:
        if FKeyDown then
        try
          FKeyDown := False;
          FDownIndex := -1;
          ReleaseCapture;
          InvalidateItem(ItemIndex);
          FClicked := True;
          Click;
        finally
          FClicked := False;
        end;
    end;
end;

procedure TCustomBubbleList.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FDownIndex := ItemAtPos(Point(X, Y), True);
    if FDownIndex > -1 then
      InvalidateItem(FDownIndex);
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomBubbleList.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  PriorIndex: Integer;
begin
  if (Button = mbLeft) and (FDownIndex > -1) then
  begin
    PriorIndex := FDownIndex;
    FDownIndex := -1;
    InvalidateItem(PriorIndex);
    if PriorIndex = ItemIndex then
    try
      FClicked := True;
      Click;
    finally
      FClicked := False;
    end;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomBubbleList.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
end;

procedure TCustomBubbleList.SelectItem(PriorIndex: Integer; NewIndex: Integer;
  var CanSelect: Boolean);
begin
  inherited SelectItem(PriorIndex, NewIndex, CanSelect);
end;

procedure TCustomBubbleList.WMLButtonUp(var Message: TWMLButtonUp);
var
  Point: TPoint;
begin
  Point := SmallPointToPoint(Message.Pos);
  inherited;
end;

procedure TCustomBubbleList.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

{ TImageDrawList }

constructor TImageDrawList.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  Color := clWindow;
  Width := 100;
  Height := 200;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := ImagesChange;
  FDisplayEmpty := True;
  ImagesChange(nil);
  ItemIndex := 0;
end;

destructor TImageDrawList.Destroy;
begin
  UpdateImages(FImages, nil);
 	FChangeLink.Free;
  inherited Destroy;
end;

procedure TImageDrawList.DrawItem(Index: Integer; var Rect: TRect;
  State: TDrawState);
var
  H, W: Integer;
begin
  inherited DrawItem(Index, Rect, State);
  if dsHot in State then
  	FillRect(Canvas.Handle, Rect, COLOR_HIGHLIGHT + 1)
	else
  	FillRect(Canvas.Handle, Rect, COLOR_WINDOW + 1);
  H := HeightOf(Rect);
  W := WidthOf(Rect);
  if FDisplayEmpty then
		Dec(Index);
	if Index > -1 then
	  FImages.Draw(Canvas, Rect.Left + (W - FImages.Width) div 2,
  		Rect.Top + (H - FImages.Height) div 2, Index);
end;

procedure TImageDrawList.ImagesChange(Sender: TObject);
var
  Images: TCustomImageList absolute Sender;
begin
  if Sender is TCustomImageList then
  begin
  	if FDisplayEmpty then
    	Count := Images.Count + 1
		else
    	Count := Images.Count;
  	ItemHeight := Images.Height + 16;
  end
  else
  begin
  	if FDisplayEmpty then
	  	Count := 1
    else
	  	Count := 0;
    ItemHeight := 32;
  end;
  Invalidate;
end;

procedure TImageDrawList.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FImages <> nil) then
    if AComponent = FImages then
      UpdateImages(FImages, nil)
end;

procedure TImageDrawList.UpdateImages(var InternalImages: TCustomImageList;
	ExternalImages: TCustomImageList);
begin
  if InternalImages <> nil then
  begin
    InternalImages.UnRegisterChanges(FChangeLink);
    InternalImages.RemoveFreeNotification(Self);
  end;
  InternalImages := ExternalImages;
  if InternalImages <> nil then
  begin
    InternalImages.RegisterChanges(FChangeLink);
    InternalImages.FreeNotification(Self);
  end;
	ImagesChange(InternalImages);
end;

procedure TImageDrawList.SetDisplayEmpty(Value: Boolean);
begin
	if Value <> FDisplayEmpty then
  begin
  	FDisplayEmpty := Value;
  	if Value then
    	Count := Count + 1
		else
    	Count := Count - 1;
  end;
end;

function TImageDrawList.GetLineHeight: Integer;
begin
  Result := ItemHeight;
end;

procedure TImageDrawList.SetImages(Value: TCustomImageList);
begin
  if Value <> FImages then
    UpdateImages(FImages, Value);
end;

function TImageDrawList.GetImageIndex: Integer;
begin
  Result := ItemIndex;
  if FDisplayEmpty then
  	Dec(Result);
  if Result < 0 then
  	Result := -1;
end;

procedure TImageDrawList.SetImageIndex(Value: Integer);
begin
  if FDisplayEmpty then
  	ItemIndex := Value + 1
	else
  	ItemIndex := Value;
end;

end.
