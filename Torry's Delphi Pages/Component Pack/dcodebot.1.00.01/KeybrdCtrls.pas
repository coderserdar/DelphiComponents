
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit KeybrdCtrls;

interface

{$I STD.INC}

uses
  Classes, Controls, Forms, Graphics, Windows, Messages, GraphTools, MathTools,
  WinTools, StretchCtrls, SysUtils;

{ Virtual key codes }

const
  VK_ALT = $12;
  VK_START = $5B;
  VK_CONTEXT = $5D;
  VK_0 = Ord('0');
  VK_1 = Ord('1');
  VK_2 = Ord('2');
  VK_3 = Ord('3');
  VK_4 = Ord('4');
  VK_5 = Ord('5');
  VK_6 = Ord('6');
  VK_7 = Ord('7');
  VK_8 = Ord('8');
  VK_9 = Ord('9');
  VK_A = Ord('A');
  VK_B = Ord('B');
  VK_C = Ord('C');
  VK_D = Ord('D');
  VK_E = Ord('E');
  VK_F = Ord('F');
  VK_G = Ord('G');
  VK_H = Ord('H');
  VK_I = Ord('I');
  VK_J = Ord('J');
  VK_K = Ord('K');
  VK_L = Ord('L');
  VK_M = Ord('M');
  VK_N = Ord('N');
  VK_O = Ord('O');
  VK_P = Ord('P');
  VK_Q = Ord('Q');
  VK_R = Ord('R');
  VK_S = Ord('S');
  VK_T = Ord('T');
  VK_U = Ord('U');
  VK_V = Ord('V');
  VK_W = Ord('W');
  VK_X = Ord('X');
  VK_Y = Ord('Y');
  VK_Z = Ord('Z');
  VK_TILDA = $C0;
  VK_MINUS = $BD;
  VK_EQUALS = $BB;
  VK_LBRACKET = $DB;
  VK_RBRACKET = $DD;
  VK_BACKSLASH = $DC;
  VK_SEMICOLON = $BA;
  VK_QUOTE = $DE;
  VK_COMMA = $BC;
  VK_PERIOD = $BE;
  VK_FORWARDSLASH = $BF;

{ TFloatingKeyboard }

type
  TFloatingKey = record
    Rect: TRect;
    KeyCode: Cardinal;
    LowerCaption: string;
    UpperCaption: string;
    Down: Boolean;
  end;
  PFloatingKey = ^TFloatingKey;

  TFloatingKeyOutline = array [0..7] of TPoint;

{ The TFloatingKeyboard class represents a keyboard

  TODO: Provide a point to virtual key method and set window position to
  topmost as the window is shown. }

  TFloatingKeyboard = class(TStretchWindow)
  private
    FCapsLock: Boolean;
    FCapsIndex: Integer;
    FKeys: array[0..72] of TFloatingKey;
    FKeyDownIndex: Integer;
    FKeyHoverIndex: Integer;
    FHookRef: Integer;
    FLocked: Boolean;
    FShift: Boolean;
    FSize: Integer;
    FLockRect: TRect;
    FLockDown: Boolean;
    FCloseRect: TRect;
    FCloseDown: Boolean;
    FTopRow: Boolean;
    procedure ClickKey(KeyIndex: Integer);
    procedure KeyboardHook(Key: Word; State: Cardinal; var Remove: Boolean);
    procedure SetCapsLock(Value: Boolean);
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
    procedure SetLocked(Value: Boolean);
    procedure SetShift(Value: Boolean);
    procedure SetSize(Value: Integer);
    function GetKeyIndex(Key: Word): Integer; overload;
    function GetKeyIndex(const Point: TPoint): Integer; overload;
    function GetKeyOutline(Index: Integer): TFloatingKeyOutline;
    procedure SetTopRow(Value: Boolean);
    procedure WMEnable(var Message: TWMEnable); message WM_ENABLE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSettingChange(var Message: TMessage); message WM_SETTINGCHANGE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    procedure DockButtonsChange(PriorState: TDockButtons); override;
    procedure DoShow; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure PaintButton(DC: HDC; Button: TDockButton);
    procedure PaintItem(DC: HDC; Index: Integer);
    function QueryMouseHide(Wnd: HWND): Boolean; override;
    procedure ReleaseKey;
    procedure StretchShow; override;
    procedure StretchHide; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read GetActive write SetActive;
    property DockButtons;
    property TopRow: Boolean read FTopRow write SetTopRow;
    property Locked: Boolean read FLocked write SetLocked;
    property CapsLock: Boolean read FCapsLock write SetCapsLock;
    property Shift: Boolean read FShift write SetShift;
    property Size: Integer read FSize write SetSize;
  end;

implementation

{ TFloatingKeyboard }

constructor TFloatingKeyboard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  Font.Name := 'Arial';
  Font.Style := [fsBold];
  Mode := smBottom;
  Interval := 0;
  Size := 75;
end;

destructor TFloatingKeyboard.Destroy;
begin
  if FHookRef > 0 then
    UnhookKeyboard(KeyboardHook);
  inherited Destroy;
end;

procedure TFloatingKeyboard.ClickKey(KeyIndex: Integer);
var
  Wnd: HWND;
  Key: Cardinal;
begin
  Wnd := GetFocus;
  SendMessage(Wnd, WM_KEYDOWN, FKeys[KeyIndex].KeyCode, 0);
  if Length(FKeys[KeyIndex].LowerCaption) = 1 then
  begin
    if Shift then
      Key := Ord(FKeys[KeyIndex].UpperCaption[1])
    else if CapsLock then
      Key := Ord(UpCase(FKeys[KeyIndex].LowerCaption[1]))
    else
      Key := Ord(FKeys[KeyIndex].LowerCaption[1]);
    SendMessage(Wnd, WM_CHAR, Key, 0);
  end
  else case FKeys[KeyIndex].KeyCode of
    VK_TAB:
      if SendMessage(Wnd, WM_GETDLGCODE, 0, 0) and DLGC_WANTTAB = DLGC_WANTTAB then
      begin
        PostMessage(Wnd, WM_KEYDOWN, VK_TAB, 0);
        PostMessage(Wnd, WM_KEYUP, VK_TAB, Integer($80000000));
        // PostMessage(Wnd, WM_CHAR, 9, 0);
      end
      else
        PostMessage(GetDialogParent(Wnd), WM_NEXTDLGCTL, 0, 0);
      else
    begin
      PostMessage(Wnd, WM_KEYDOWN, FKeys[KeyIndex].KeyCode, 0);
      PostMessage(Wnd, WM_KEYUP, FKeys[KeyIndex].KeyCode, Integer($80000000));
    end;
  end;
end;

procedure TFloatingKeyboard.DockButtonsChange(PriorState: TDockButtons);
var
  PriorSize: Integer;
begin
  Active := False;
  PriorSize := Size;
  FSize := 0;
  Size := PriorSize;
end;

procedure TFloatingKeyboard.DoShow;
begin
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or
    SWP_NOACTIVATE);
  UpdateWindow(Handle);
  inherited DoShow;
end;

procedure TFloatingKeyboard.KeyboardHook(Key: Word; State: Cardinal;
  var Remove: Boolean);
var
  KeyPressed: Boolean;
  I: Integer;
begin
  KeyPressed := State shr $1F = 0;
  case Key of
    VK_CAPITAL:
      if GetKeyState(VK_CAPITAL) and 1 = 1 <> CapsLock then
        CapsLock := not CapsLock;
    VK_SHIFT:
      if Shift <> KeyPressed then
        Shift := not Shift
      else
  else for I := Low(FKeys) to High(FKeys) do
    if (FKeys[I].KeyCode = Key) and (FKeys[I].Down <> KeyPressed) then
    begin
      FKeys[I].Down := not FKeys[I].Down;
      PaintItem(Canvas.Handle, I);
    end;
  end;
end;

procedure TFloatingKeyboard.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Point: TPoint;
  DC: HDC;
  Index: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Visible then
  begin
    Point := GetPoint(X, Y);
    if SendMessage(Handle, WM_NCHITTEST, Integer(PointToSmallPoint(
      ClientToScreen(Point))), 0) <> HTCLIENT then
      SetCapture(Handle);
    if PtInRect(FLockRect, Point) or PtInRect(FCloseRect, Point) then
    begin
      DC := Canvas.Handle;
      Index := FKeyHoverIndex;
      FKeyHoverIndex := -1;
      if Index > -1 then
        PaintItem(DC, Index);
      Index := FKeyDownIndex;
      FKeyDownIndex := -1;
      if Index > -1 then
      begin
        FKeys[Index].Down := False;
        PaintItem(DC, Index);
      end;
      if PtInRect(FLockRect, Point) then
      begin
        FLockDown := True;
        PaintButton(DC, dbLock);
      end
      else
      begin
        FCloseDown := True;
        PaintButton(DC, dbClose);
      end;
    end
    else
    begin
      Index := FKeyDownIndex;
      if (Index > -1) and (Index <> FCapsIndex) then
      begin
        FKeyDownIndex := -1;
        FKeys[Index].Down := False;
        PaintItem(Canvas.Handle, Index);
      end;
      FKeyDownIndex := GetKeyIndex(Point);
      if FKeyDownIndex = FCapsIndex then
      begin
        FCapsLock := not FKeys[FKeyDownIndex].Down;
        FKeys[FKeyDownIndex].Down := FCapsLock;
        Invalidate;
      end
      else if FKeyDownIndex > -1 then
      begin
        FKeys[FKeyDownIndex].Down := True;
        PaintItem(Canvas.Handle, FKeyDownIndex);
      end;
    end;
  end;
end;

procedure TFloatingKeyboard.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  HoverIndex: Integer;
  PriorIndex: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  SetCursor(Screen.Cursors[crDefault]);
  if not (FLockDown or FCloseDown) then
  begin
    HoverIndex := GetKeyIndex(GetPoint(X, Y));
    if (HoverIndex > -1) and (HoverIndex <> FKeyHoverIndex) then
    begin
      PriorIndex := FKeyHoverIndex;
      FKeyHoverIndex := HoverIndex;
      if PriorIndex > -1 then
        PaintItem(Canvas.Handle, PriorIndex);
      PaintItem(Canvas.Handle, FKeyHoverIndex);
    end;
  end;
end;

procedure TFloatingKeyboard.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  UpIndex: Integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Visible and (FKeyDownIndex <> FCapsIndex) then
  begin
    //ReleaseCapture;
    if FLockDown then
    begin
      FLockDown := False;
      if PtInRect(FLockRect, GetPoint(X, Y)) then
        FLocked := not FLocked;
      PaintButton(Canvas.Handle, dbLock);
    end
    else if FCloseDown then
    begin
      FCloseDown := False;
      if PtInRect(FCloseRect, GetPoint(X, Y)) then
        Cancel
      else
        PaintButton(Canvas.Handle, dbClose);
    end
    else
    begin
      if FKeyDownIndex > -1 then
      begin
        FKeys[FKeyDownIndex].Down := False;
        PaintItem(Canvas.Handle, FKeyDownIndex);
      end;
      UpIndex := GetKeyIndex(GetPoint(X, Y));
      if (FKeyDownIndex = UpIndex) and (UpIndex > -1) then
      begin
        ClickKey(UpIndex);
        if UpperCase(FKeys[FKeyDownIndex].LowerCaption) = 'SHIFT' then
        begin
          FShift := not FShift;
          Invalidate;
        end
        else if FShift then
        begin
          FShift := False;
          Invalidate;
        end;
        FKeyDownIndex := -1;
      end
      else
        FKeyDownIndex := -1;
    end;
  end;
end;

procedure TFloatingKeyboard.Paint;
var
  DC: HDC;
  Button: TDockButton;
  I: Integer;
begin
  DC := Canvas.Handle;
  for Button := Low(TDockButton) to High(TDockButton) do
    PaintButton(DC, Button);
  for I := Low(FKeys) to High(FKeys) do
    PaintItem(DC, I);
end;

procedure TFloatingKeyboard.PaintButton(DC: HDC; Button: TDockButton);
const
  LockStyles: array[Boolean] of Cardinal = (DFCS_CAPTIONMAX, DFCS_CAPTIONRESTORE);
  PushStyles: array[Boolean] of Cardinal = (0, DFCS_PUSHED);
var
  Rect: TRect;
  Style: Cardinal;
begin
  if Button in DockButtons then
  begin
    Style := 0;
    case Button of
      dbLock:
        begin
          Rect := FLockRect;
          Style := DFCS_CAPTIONCLOSE or LockStyles[FLocked] or
            PushStyles[FLockDown]
        end;
      dbClose:
        begin
          Rect := FCloseRect;
          Style := DFCS_CAPTIONCLOSE or PushStyles[FCloseDown]
        end;
    end;
    if not IsRectEmpty(Rect) then
      DrawFrameControl(DC, Rect, DFC_CAPTION, Style);
  end;
end;

procedure TFloatingKeyboard.PaintItem(DC: HDC; Index: Integer);
var
  Brush: HBRUSH;
  Outline: TFloatingKeyOutline;
  Rgn: HRGN;
  Rect: TRect;
  S: string;
begin
  Rect := FKeys[Index].Rect;
  if (Index = FKeyHoverIndex) and (not FKeys[Index].Down) then
    Brush := CreateSolidBrush(Delta(GetSysColor(COLOR_BTNFACE), 20))
  else
    Brush := GetSysColorBrush(COLOR_BTNFACE);
  FillRect(DC, Rect, Brush);
  DeleteObject(Brush);
  if (Index = FKeyHoverIndex) and (not FKeys[Index].Down) then
    Brush := GetSysColorBrush(COLOR_BTNSHADOW)
  else
    Brush := GetStockObject(BLACK_BRUSH);
  Outline := GetKeyOutline(Index);
  Rgn := CreatePolygonRgn(Outline, 8, WINDING);
  FrameRgn(DC, Rgn, Brush, 1, 1);
  if FKeys[Index].Down then
  begin
    OffsetRgn(Rgn, 1, 1);
    FrameRgn(DC, Rgn, Brush, 1, 1);
    OffsetRgn(Rgn, -1, -1);
  end
  else
  begin
    OffsetRgn(Rgn, -1, -1);
    FrameRgn(DC, Rgn, Brush, 1, 1);
    OffsetRgn(Rgn, -1, -1);
    FrameRgn(DC, Rgn, Brush, 1, 1);
    OffsetRect(Rect, -2, -2);
  end;
  DeleteObject(Rgn);
  SetBkMode(DC, TRANSPARENT);
  if FShift then
    S := FKeys[Index].UpperCaption
  else
  begin
    S := FKeys[Index].LowerCaption;
    if FCapsLock and (Length(S) = 1) then
      S := UpperCase(S);
  end;
  DrawText(DC, PChar(S), -1, Rect, DT_CENTER or DT_VCENTER or DT_NOCLIP or
    DT_SINGLELINE or DT_NOPREFIX);
  if (Index = FKeyHoverIndex) and (not FKeys[Index].Down) then
    DeleteObject(Brush);
end;

function TFloatingKeyboard.QueryMouseHide(Wnd: HWND): Boolean;
begin
  Result := (Wnd <> Handle) and (not FLocked);
end;

procedure TFloatingKeyboard.ReleaseKey;
begin
  if FKeyDownIndex > -1 then
  begin
    FKeys[FKeyDownIndex].Down := False;
    PaintItem(Canvas.Handle, FKeyDownIndex);
  end;
end;

procedure TFloatingKeyboard.StretchShow;
begin
  FCapsLock := GetKeyState(VK_CAPITAL) and 1 = 1;
  FKeys[FCapsIndex].Down := FCapsLock;
  Font.Size := Trunc(14 * Size / 100 * Screen.Width / 1024);
  Canvas.Font := Font;
  Inc(FHookRef);
  if FHookRef = 1 then
    HookKeyboard(KeyboardHook);
  inherited StretchShow;
end;

procedure TFloatingKeyboard.StretchHide;
begin
  if FHookRef > 0 then
  begin
    Dec(FHookRef);
    if FHookRef = 0 then
      UnhookKeyboard(KeyboardHook);
  end;
  inherited StretchHide;
end;

function TFloatingKeyboard.GetActive: Boolean;
begin
  Result := Visible;
end;

procedure TFloatingKeyboard.SetActive(Value: Boolean);
begin
  if Active <> Value then
    if Value then
      Popup(Left, Top)
    else
      StretchHide;
end;

procedure TFloatingKeyboard.SetCapsLock(Value: Boolean);
begin
  if Value <> FCapsLock then
  begin
    FCapsLock := Value;
    FKeys[FCapsIndex].Down := FCapsLock;
    Invalidate;
  end;
end;

procedure TFloatingKeyboard.SetLocked(Value: Boolean);
begin
  if Value <> FLocked then
  begin
    FLocked := Value;
    PaintButton(Canvas.Handle, dbLock);
  end;
end;

procedure TFloatingKeyboard.SetShift(Value: Boolean);
var
  I: Integer;
begin
  if Value <> FShift then
  begin
    FShift := Value;
    for I := Low(FKeys) to High(FKeys) do
      if FKeys[I].KeyCode = VK_SHIFT then
        FKeys[I].Down := FShift;
    Invalidate;
  end;
end;

procedure TFloatingKeyboard.SetSize(Value: Integer);
var
  ButtonOffset: Integer;
  KeyIndex: Integer;
  XScale: Double;
  YScale: Double;
  MaxHeight: Integer;
  MaxWidth: Integer;

  procedure CreateKey(A, B, C, D: Integer; const Lower, Upper: string; Code: Integer);
  begin
    if not FTopRow then
      if B = 0 then
        Exit
      else
        Dec(B, 48);
    with FKeys[KeyIndex] do
    begin
      C := C - 3;
      D := D - 3;
      Rect.Left := Trunc(A * XScale);
      Rect.Top := Trunc(B * YScale);
      Rect.Right := Trunc((A + C) * XScale);
      Rect.Bottom := Trunc((B + D) * YScale);
      OffsetRect(Rect, 0, ButtonOffset);
      if Rect.Right > MaxWidth then
        MaxWidth := Rect.Right;
      if Rect.Bottom > MaxHeight then
        MaxHeight := Rect.Bottom;
      LowerCaption := Lower;
      UpperCaption := Upper;
      KeyCode := Code;
      Down := False;
    end;
    Inc(KeyIndex);
  end;

  procedure CreateRegion;
  var
    Outline: TFloatingKeyOutline;
    DestRgn: HRGN;
    SourceRgn: HRGN;
    Rect: TRect;
    I: Integer;
  begin
    DestRgn := CreateRectRgn(0, 0, 0, 0);
    for I := Low(FKeys) to High(FKeys) do
    begin
      if IsRectEmpty(FKeys[I].Rect) then
        Continue;
      Outline := GetKeyOutline(I);
      SourceRgn := CreatePolygonRgn(Outline, 8, WINDING);
      CombineRgn(DestRgn, SourceRgn, DestRgn, RGN_OR);
      DeleteObject(SourceRgn);
    end;
    Rect := GetRect(Width - GetSystemMetrics(SM_CXSIZE), 0, Width,
      GetSystemMetrics(SM_CYSIZE));
    if dbClose in DockButtons then
    begin
      FCloseRect := Rect;
      with Rect do
        SourceRgn := CreateRectRgn(Left, Top, Right, Bottom);
      CombineRgn(DestRgn, SourceRgn, DestRgn, RGN_OR);
      DeleteObject(SourceRgn);
      OffsetRect(Rect, -WidthOf(Rect) - 2, 0);
    end
    else
      SetRectEmpty(FCloseRect);
    if dbLock in DockButtons then
    begin
      FLockRect := Rect;
      with Rect do
        SourceRgn := CreateRectRgn(Left, Top, Right, Bottom);
      CombineRgn(DestRgn, SourceRgn, DestRgn, RGN_OR);
      DeleteObject(SourceRgn);
    end
    else
      SetRectEmpty(FLockRect);
    SetWindowRgn(Handle, DestRgn, True);
  end;

begin
  StretchHide;
  if Value < 1 then Value := 1;
  if Value = FSize then Exit;
  FillChar(FKeys, SizeOf(FKeys), #0);
  FSize := Value;
  if DockButtons <> [] then
    ButtonOffset := GetSystemMetrics(SM_CYCAPTION) +
      GetSystemMetrics(SM_CYEDGE) * 2
  else
    ButtonOffset := 0;
  KeyIndex := 0;
  XScale := (Screen.Width / 800) * FSize / 100;
  YScale := (Screen.Height / 600) * FSize / 100;
  MaxWidth := 0;
  MaxHeight := 0;
  CreateKey(560, 48, 55, 38, '0', ')', VK_0);
  CreateKey(56, 48, 55, 38, '1', '!', VK_1);
  CreateKey(112, 48, 55, 38, '2', '@', VK_2);
  CreateKey(168, 48, 55, 38, '3', '#', VK_3);
  CreateKey(224, 48, 55, 38, '4', '$', VK_4);
  CreateKey(280, 48, 55, 38, '5', '%', VK_5);
  CreateKey(336, 48, 55, 38, '6', '^', VK_6);
  CreateKey(392, 48, 55, 38, '7', '&', VK_7);
  CreateKey(448, 48, 55, 38, '8', '*', VK_8);
  CreateKey(504, 48, 55, 38, '9', '(', VK_9);
  CreateKey(80, 128, 55, 38, 'a', 'A', VK_A);
  CreateKey(112, 208, 55, 38, 'Alt', 'Alt', VK_ALT);
  CreateKey(560, 208, 55, 38, 'Alt', 'Alt', VK_ALT);
  CreateKey(320, 168, 55, 38, 'b', 'B', VK_B);
  CreateKey(728, 48, 57, 38, 'Back', 'Back', VK_BACK);
  CreateKey(672, 48, 55, 38, '=', '+', VK_EQUALS);
  CreateKey(632, 88, 55, 38, '[', '{', VK_LBRACKET);
  CreateKey(688, 88, 55, 38, ']', '}', VK_RBRACKET);
  CreateKey(208, 168, 55, 38, 'c', 'C', VK_C);
  FCapsIndex := KeyIndex;
  CreateKey(0, 128, 78, 38, 'Caps', 'Caps', VK_CAPITAL);
  CreateKey(544, 168, 55, 38, '.',  '>', VK_PERIOD);
  CreateKey(0, 208, 55, 38, 'Ctrl', 'Ctrl', VK_CONTROL);
  CreateKey(728, 208, 55, 38, 'Ctrl', 'Ctrl', VK_CONTROL);
  CreateKey(192, 128, 55, 38, 'd', 'D', VK_D);
  CreateKey(184, 88, 55, 38, 'e', 'E', VK_E);
  CreateKey(696, 128, 89, 38, 'Enter', 'Enter', VK_RETURN);
  CreateKey(0, 0, 55, 38, 'Esc', 'Esc', VK_ESCAPE);
  CreateKey(248, 128, 55, 38, 'f', 'F', VK_F);
  CreateKey(88, 0, 55, 38, 'F1', 'F1', VK_F1);
  CreateKey(616, 0, 55, 38, 'F10', 'F10', VK_F10);
  CreateKey(672, 0, 55, 38, 'F11', 'F11', VK_F11);
  CreateKey(728, 0, 57, 38, 'F12', 'F12', VK_F12);
  CreateKey(144, 0, 55, 38, 'F2', 'F2', VK_F2);
  CreateKey(200, 0, 55, 38, 'F3', 'F3', VK_F3);
  CreateKey(256, 0, 55, 38, 'F4', 'F4', VK_F4);
  CreateKey(320, 0, 55, 38, 'F5', 'F5', VK_F5);
  CreateKey(376, 0, 55, 38, 'F6', 'F6', VK_F6);
  CreateKey(432, 0, 55, 38, 'F7', 'F7', VK_F7);
  CreateKey(488, 0, 55, 38, 'F8', 'F8', VK_F8);
  CreateKey(560, 0, 55, 38, 'F9', 'F9', VK_F9);
  CreateKey(304, 128, 55, 38, 'g', 'G', VK_G);
  CreateKey(360, 128, 55, 38, 'h', 'H', VK_H);
  CreateKey(616, 48, 55, 38, '-', '_', VK_MINUS);
  CreateKey(464, 88, 55, 38, 'i', 'I', VK_I);
  CreateKey(416, 128, 55, 38, 'j', 'J', VK_J);
  CreateKey(472, 128, 55, 38, 'k', 'K', VK_K);
  CreateKey(528, 128, 55, 38, 'l', 'L', VK_L);
  CreateKey(432, 168, 55, 38, 'm', 'M', VK_M);
  CreateKey(672, 208, 55, 38, 'Menu', 'Menu', VK_CONTEXT);
  CreateKey(376, 168, 55, 38, 'n', 'N', VK_N);
  CreateKey(520, 88, 55, 38, 'o', 'O', VK_O);
  CreateKey(576, 88, 55, 38, 'p', 'P', VK_P);
  CreateKey(488, 168, 55, 38, ',', '<', VK_COMMA);
  CreateKey(72, 88, 55, 38, 'q', 'Q', VK_Q);
  CreateKey(640, 128, 55, 38, '''', '"', VK_QUOTE);
  CreateKey(240, 88, 55, 38, 'r', 'R', VK_R);
  CreateKey(136, 128, 55, 38, 's', 'S', VK_S);
  CreateKey(584, 128, 55, 38, ';', ':', VK_SEMICOLON);
  CreateKey(0, 168, 94, 38, 'Shift', 'Shift', VK_SHIFT);
  CreateKey(656, 168, 129, 38, 'Shift', 'Shift', VK_SHIFT);
  CreateKey(600, 168, 55, 38, '/',  '?', VK_FORWARDSLASH);
  CreateKey(168, 208, 390, 38, 'Space', 'Space', VK_SPACE);
  CreateKey(56, 208, 55, 38, 'Start', 'Start', VK_START);
  CreateKey(616, 208, 55, 38, 'Start', 'Start', VK_START);
  CreateKey(296, 88, 55, 38, 't', 'T', VK_T);
  CreateKey(0, 88, 70, 38, 'Tab', 'Tab', VK_TAB);
  CreateKey(0, 48, 55, 38, '`',  '~', VK_TILDA);
  CreateKey(408, 88, 55, 38, 'u', 'U', VK_U);
  CreateKey(264, 168, 55, 38, 'v', 'V', VK_V);
  CreateKey(128, 88, 55, 38, 'w', 'W', VK_W);
  CreateKey(152, 168, 55, 38, 'x', 'X', VK_X);
  CreateKey(352, 88, 55, 38, 'y', 'Y', VK_Y);
  CreateKey(96, 168, 55, 38, 'z', 'Z', VK_Z);
  Width := MaxWidth + 1;
  Height := MaxHeight + 1;
  Left := (Screen.Width - Width) div 2;
  Top := Screen.Height - Height - 32;
  CreateRegion;
  FKeyHoverIndex := -1;
end;

function TFloatingKeyboard.GetKeyIndex(Key: Word): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(FKeys) to High(FKeys) do
    if FKeys[I].KeyCode = Key then
    begin
      Result := I;
      //
      Break;
    end;
end;

function TFloatingKeyboard.GetKeyIndex(const Point: TPoint): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(FKeys) to High(FKeys) do
    if PtInRect(FKeys[I].Rect, Point) then
    begin
      Result := I;
      Break;
    end;
end;

function TFloatingKeyboard.GetKeyOutline(Index: Integer): TFloatingKeyOutline;
const
  BevelSize = 8;
var
  Bevel: Integer;
begin
  Bevel := Trunc(BevelSize * FSize / 100);
  with FKeys[Index].Rect do
  begin
    Result[0] := Point(Left, Top + Bevel);
    Result[1] := Point(Left + Bevel, Top);
    Result[2] := Point(Right - Bevel, Top);
    Result[3] := Point(Right, Top + Bevel);
    Result[4] := Point(Right, Bottom - Bevel);
    Result[5] := Point(Right - Bevel, Bottom);
    Result[6] := Point(Left + Bevel, Bottom);
    Result[7] := Point(Left, Bottom - Bevel);
  end;
end;

procedure TFloatingKeyboard.SetTopRow(Value: Boolean);
var
  PriorSize: Integer;
begin
  if Value <> FTopRow then
  begin
    PriorSize := Size;
    FSize := 0;
    FTopRow := Value;
    Size := PriorSize;
  end;
end;


procedure TFloatingKeyboard.WMEnable(var Message: TWMEnable);
begin
  //inherited;
  //StretchHide; ignore
end;

procedure TFloatingKeyboard.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TFloatingKeyboard.WMNCHitTest(var Message: TWMNCHitTest);
begin
  Message.Result := HTNOWHERE;
end;

procedure TFloatingKeyboard.WMSettingChange(var Message: TMessage);
begin
  Size := Size + 1;
  Size := Size - 1;
  inherited;
end;

{  procedure TFloatingKeyboard.WMNCHitText(var Message: TWMNCHitTest);
var
  Wnd: HWND;
begin
  Wnd := GetForegroundWindow;
  if (Wnd <> 0) and (Wnd <> Handle) then
    FActiveWnd := Wnd;
  inherited;
end;

  GetKeyboardState(State);
  State[VK_SHIFT] := $80;
  SetKeyboardState(State);
  PostMessage(Edit1.Handle, WM_KEYDOWN, VK_1, 0); }

end.
