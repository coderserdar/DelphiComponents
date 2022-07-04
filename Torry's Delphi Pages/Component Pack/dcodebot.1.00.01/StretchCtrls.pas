
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit StretchCtrls;

interface

{$I STD.INC}

uses
  Windows, Classes, Controls, Forms, Graphics, Messages, StdCtrls, SysUtils,
  GraphTools, MathTools, WinTools;

{ TStretcher }

type
  TStretcher = class(TComponent)
  private
    FControl: TWinControl;
    FInterval: Integer;
    FThread: TThread;
    FOnStretch: TNotifyEvent;
    procedure SetControl(Value: TWinControl);
    procedure SetInterval(Value: Integer);
  protected
    procedure DoTerminate(Sender: TObject);
    procedure KillThread;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
    procedure Stretch(ALeft, ATop, AWidth, AHeight: Integer); overload;
    procedure Stretch(const Pos: TWindowPosition); overload;
    property Control: TWinControl read FControl write SetControl;
    property Interval: Integer read FInterval write SetInterval;
    property Thread: TThread read FThread;
    property OnStretch: TNotifyEvent read FOnStretch write FOnStretch;
  end;

{ TStretchWindow }

const
  WM_STRETCHHIDE = WM_USER + $FA;

type
  TDockButton = (dbLock, dbClose);
  TDockButtons = set of TDockButton;
  TStretchMode = (smNone, smLeft, smTop, smRight, smBottom, smExplode, smZoom);

  TStretchWindow = class(TCustomForm)
  private
    FCaptured: Boolean;
    FClicking: Boolean;
    FDockButtons: TDockButtons;
    FMode: TStretchMode;
    FPosition: TWindowPosition;
    FStretcher: TStretcher;
    FOnCancel: TNotifyEvent;
    procedure StretcherStretch(Sender: TObject);
    procedure SetDockButtons(Value: TDockButtons);
    function GetInterval: Integer;
    procedure SetInterval(Value: Integer);
    procedure SetMode(Value: TStretchMode);
    procedure WMActivateApp(var Message: TWMActivateApp); message WM_ACTIVATEAPP;
    procedure WMStretchHide(var Message: TMessage); message WM_STRETCHHIDE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DockButtonsChange(PriorState: TDockButtons); dynamic;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseHook(Msg: Cardinal; const HookStruct: TMouseHookStruct;
      var Remove: Boolean); virtual;
    function QueryMouseHide(Wnd: HWND): Boolean; virtual;
    procedure Stretch; dynamic;
    procedure StretchShow; dynamic;
    procedure StretchHide; dynamic;
    procedure WndProc(var Message: TMessage); override;
    property Captured: Boolean read FCaptured write FCaptured;
    property DockButtons: TDockButtons read FDockButtons write SetDockButtons;
    property Interval: Integer read GetInterval write SetInterval;
    property Mode: TStretchMode read FMode write SetMode;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Cancel;
    procedure Popup(X, Y: Integer);
    procedure Select;
  end;

{ TCustomStretchList }

  TDrawCaptionEvent = procedure(Control: TWinControl; Rect: TRect) of object;

  TCustomStretchList = class(TStretchWindow)
  private
    FCount: Integer;
    FDragging: Boolean;
    FDragPoint: TPoint;
    FDownButtonPressed: Boolean;
    FDownButtonVisible: Boolean;
    FHookRef: Integer;
    FHitTest: TPoint;
    FItemHeight: Integer;
    FItemIndex: Integer;
    FTopIndex: Integer;
    FStretching: Boolean;
    FUpButtonPressed: Boolean;
    FUpButtonVisible: Boolean;
    FOnChange: TNotifyEvent;
    FOnDrawBackground: TNotifyEvent;
    FOnDrawCaption: TDrawCaptionEvent;
    FOnDrawItem: TDrawItemEvent;
    procedure KeyboardHook(Key: Word; State: Cardinal; var Remove: Boolean);
    procedure UpdateButtons;
    procedure SetCount(Value: Integer);
    function GetDisplayCount: Integer;
    procedure SetDisplayCount(Value: Integer);
    procedure SetItemHeight(Value: Integer);
    procedure SetItemIndex(Value: Integer);
    procedure SetTopIndex(Value: Integer);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
  protected
    procedure AdjustScrollRect(var Rect: TRect); virtual;
    function CanDrawBackground: Boolean; virtual;
    procedure Click; override;
    procedure DoShow; override;
    procedure DrawCaption(Rect: TRect); virtual;
    procedure DrawBackground; virtual;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); virtual;
    function GetClippingPath: TPolygon;
    function GetCaptionRect: TRect; virtual;
    function GetUpRect: TRect;
    function GetDownRect: TRect;
    function GetListRect: TRect;
    function HitTestCaption(X, Y: Integer): Boolean; dynamic;
    procedure InvalidateItem(Index: Integer);
    function ItemFromPoint(const Point: TPoint): Integer;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure Stretch; override;
    procedure StretchShow; override;
    procedure StretchHide; override;
    property DisplayCount: Integer read GetDisplayCount write SetDisplayCount;
    property Count: Integer read FCount write SetCount;
    property HitTest: TPoint read FHitTest;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property ItemHeight: Integer read FItemHeight write SetItemHeight;
    property ListRect: TRect read GetListRect;
    property TopIndex: Integer read FTopIndex write SetTopIndex;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDrawBackground: TNotifyEvent read FOnDrawBackground write FOnDrawBackground;
    property OnDrawCaption: TDrawCaptionEvent read FOnDrawCaption write FOnDrawCaption;
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TStretchList }

  TStretchList = class(TCustomStretchList)
  public
    property Canvas;
    property Count;
    property DisplayCount;
    property ItemIndex;
    property ListRect;
    property TopIndex;
  published
    property Interval;
    property ItemHeight;
    property Mode;
    property OnDrawBackground;
    property OnDrawCaption;
    property OnDrawItem;
    property OnCancel;
    property OnChange;
    property OnClick;
  end;

{ TStretchMenu }

  TStretchMenu = class(TCustomStretchList)
  private
    FItems: TStrings;
    FMenuLeft: Integer;
    FTitle: string;
    FTitleFont: TFont;
    FTitleBase: TColor;
    FTitleLead: TColor;
    FExtraLine: Boolean;
    FDrawTitle: Boolean;
    procedure ItemsChange(Sender: TObject);
    procedure SetItems(Value: TStrings);
    procedure SetTitle(const Value: string);
    procedure SetExtraLine(const Value: Boolean);
  protected
    procedure AdjustScrollRect(var Rect: TRect); override;
    function CanDrawBackground: Boolean; override;
    procedure DrawBackground; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    property ExtraLine: Boolean read FExtraLine write SetExtraLine;
    property MenuLeft: Integer read FMenuLeft;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DisplayCount;
    property DrawTitle: Boolean read FDrawTitle write FDrawTitle;
    property ItemIndex;
    property ListRect;
    property TopIndex;
    property TitleBase: TColor read FTitleBase write FTitleBase;
    property TitleLead: TColor read FTitleLead write FTitleLead;
    property Interval;
    property ItemHeight;
    property Items: TStrings read FItems write SetItems;
    property Mode;
    property Title: string read FTitle write SetTitle;
    property OnCancel;
    property OnChange;
    property OnClick;
    property OnDrawBackground;
    property OnDrawCaption;
    property OnDrawItem;
  end;

{ TGridMenu }

  TCalculateCellEvent = procedure(Sender: TObject; Column: Integer;
    var Cell: string) of object;
  TSortColumnsEvent = function(Sender: TObject; Column: Integer;
    Left, Right: Integer): Integer of object;

  TGridMenu = class(TStretchMenu)
  private
    FAutomaticSize: Boolean;
    FColumns: TStrings;
    FColumnCount: Integer;
    FColWidths: TIntegers;
    FDragColumn: Integer;
    FStatusText: string;
    FSortColumn: Integer;
    FSortDirection: TDirection;
    FOnCalculateCell: TCalculateCellEvent;
    FOnSortColumns: TSortColumnsEvent;
    procedure ColumnsChange(Sender: TObject);
    procedure SetAutomaticSize(Value: Boolean);
    procedure SetColumns(Value: TStrings);
    function GetColWidths(Index: Integer): Integer;
    procedure SetColWidths(Index: Integer; Value: Integer);
    procedure SetStatusText(Value: string);
    procedure SetOnSortColumns(Value: TSortColumnsEvent);
  protected
    procedure DoCalculateCell(Column: Integer; var Cell: string);
    procedure DrawCaption(Rect: TRect); override;
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    function GetCaptionRect: TRect; override;
    function GetCellRect(const Bounds: TRect; Index: Integer): TRect; virtual;
    function HitTestCaption(X, Y: Integer): Boolean; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
   procedure StretchHide; override;
 public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Sort(Column: Integer; Direction: TDirection);
    property AutomaticSize: Boolean read FAutomaticSize write SetAutomaticSize;
    property Columns: TStrings read FColumns write SetColumns;
    property ColWidths[Index: Integer]: Integer read GetColWidths write SetColWidths;
    property StatusText: string read FStatusText write SetStatusText;
    property OnCalculateCell: TCalculateCellEvent read FOnCalculateCell
      write FOnCalculateCell;
    property OnSortColumns: TSortColumnsEvent read FOnSortColumns write
      SetOnSortColumns;
  end;

implementation

{ TStretcherThread class }

type
  TStretcherThreadParams = record
    Wnd: HWND;
    Pos: TWindowPosition;
    Interval: Integer;
    OnTerminate: TNotifyEvent;
  end;

  TStretcherThread = class(TThread)
  private
    FWnd: HWND;
    FOldPos: TWindowPosition;
    FNewPos: TWindowPosition;
    FUpdatePos: TWindowPosition;
    FInterval: Integer;
    procedure UpdateWnd;
  protected
    procedure Execute; override;
  public
    constructor Create(const Params: TStretcherThreadParams);
  end;

constructor TStretcherThread.Create(const Params: TStretcherThreadParams);
begin
  FWnd := Params.Wnd;
  GetWindowPosition(FWnd, FOldPos);
  FNewPos := Params.Pos;
  FInterval := Params.Interval;
  OnTerminate := Params.OnTerminate;
  inherited Create(False);
end;

procedure TStretcherThread.Execute;
const
  Rate = 20;
var
  Start: Integer;
  Frame: Cardinal;
  Ratio: Double;
begin
  Start := GetTickCount;
  Frame := 0;
  repeat
    while Frame = GetTickCount div Rate do
      Sleep(0);
    Frame := GetTickCount div Rate;
    Ratio :=  (Integer(GetTickCount) - Start) / FInterval;
    if Ratio > 1 then Ratio := 1;
    with FOldPos do
    begin
      FUpdatePos.Left := Trunc(Left + ((FNewPos.Left - Left) * Ratio));
      FUpdatePos.Top := Trunc(Top + ((FNewPos.Top - Top) * Ratio));
      FUpdatePos.Width := Trunc(Width + ((FNewPos.Width - Width) * Ratio));
      FUpdatePos.Height := Trunc(Height + ((FNewPos.Height - Height) * Ratio));
    end;
    if not Terminated then
      Synchronize(UpdateWnd);
  until Terminated or (Ratio = 1);
end;

procedure TStretcherThread.UpdateWnd;
var
  Rect: TRect;
begin
  SetWindowPosition(FWnd, FUpdatePos);
  SetWindowPos(FWnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOACTIVATE or
    SWP_NOSIZE);
  GetClientRect(FWnd, Rect);
  InvalidateRect(FWnd, @Rect, True);
end;

{ TStretcher }

destructor TStretcher.Destroy;
begin
  KillThread;
  inherited Destroy;
end;

procedure TStretcher.DoTerminate(Sender: TObject);
begin
  if Assigned(FOnStretch) then FOnStretch(Self);
end;

procedure TStretcher.KillThread;
var
  Handle: THandle;
  ExitCode: DWORD;
begin
  if FThread <> nil then
  begin
    Handle := FThread.Handle;
    FThread.OnTerminate := nil;
    FThread.Terminate;
    repeat
      Sleep(0);
      GetExitCodeThread(Handle, ExitCode);
    until ExitCode <> STILL_ACTIVE;
    FThread.Free;
    FThread := nil;
  end;
end;

procedure TStretcher.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FControl) and (Operation = opRemove) then
    FControl := nil;
end;

procedure TStretcher.Stretch(ALeft, ATop, AWidth, AHeight: Integer);
var
  Pos: TWindowPosition;
begin
  with Pos do
  begin
    Left := ALeft;
    Top := ATop;
    Width := AWidth;
    Height := AHeight;
  end;
  Stretch(Pos);
end;

procedure TStretcher.Stretch(const Pos: TWindowPosition);
var
  Params: TStretcherThreadParams;
begin
  KillThread;
  if FControl <> nil then
  begin
    Params.Wnd := FControl.Handle;
    Params.Pos := Pos;
    Params.Interval := FInterval;
    Params.OnTerminate := DoTerminate;
    FThread := TStretcherThread.Create(Params);
  end;
end;

procedure TStretcher.SetControl(Value: TWinControl);
begin
  if Value <> FControl then
  begin
    KillThread;
    if FControl <> nil then
      FControl.RemoveFreeNotification(Self);
    FControl := Value;
    if FControl <> nil then
      FControl.FreeNotification(Self);
  end;
end;

procedure TStretcher.SetInterval(Value: Integer);
begin
  if Value <> FInterval then
  begin
    KillThread;
    FInterval := Value;
  end;
end;

{ TStretchWindow }

constructor TStretchWindow.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  DesktopFont := True;
  Height := 200;
  Width := 100;
  FStretcher := TStretcher.Create(Self);
  FStretcher.Control := Self;
  FStretcher.Interval := 200;
  FStretcher.OnStretch := StretcherStretch;
  FMode := smZoom;
  HookMouse(MouseHook);
end;

destructor TStretchWindow.Destroy;
begin
  FStretcher.Control := nil;
  UnhookMouse(MouseHook);
  inherited Destroy;
end;

procedure TStretchWindow.SetDockButtons(Value: TDockButtons);
var
  PriorState: TDockButtons;
begin
  if Value <> FDockButtons then
  begin
    PriorState := FDockButtons;
    FDockButtons := Value;
    DockButtonsChange(PriorState);
  end;
end;

procedure TStretchWindow.CreateParams(var Params: TCreateParams);
const
  ExStyle = WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
  Style = WS_CHILD or WS_CLIPSIBLINGS;
begin
  inherited CreateParams(Params);
  Params.ExStyle := ExStyle;
  Params.Style := Style;
  Params.WndParent := GetDesktopWindow;
end;

procedure TStretchWindow.Cancel;
begin
  FClicking := False;
  StretchHide;
end;

procedure TStretchWindow.DockButtonsChange(PriorState: TDockButtons);
begin
end;

procedure TStretchWindow.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or
    SWP_NOACTIVATE);
end;

procedure TStretchWindow.MouseHook(Msg: Cardinal;
  const HookStruct: TMouseHookStruct; var Remove: Boolean);
var
  Wnd: HWND;
  Point: TPoint;
begin
  if not Visible then Exit;
  Wnd := WindowFromPoint(HookStruct.pt);
  if Wnd = Handle then
  begin
    Point := ScreenToClient(HookStruct.pt);
    case Msg of
      WM_NCMOUSEMOVE:
        MouseMove([], Point.x, Point.y);
      WM_NCLBUTTONDOWN:
        MouseDown(mbLeft, [], Point.X, Point.Y);
      WM_NCLBUTTONUP:
        MouseUp(mbLeft, [], Point.X, Point.Y);
    end;
  end
  else
  case Msg of
    WM_NCACTIVATE, WM_NCLBUTTONDOWN, WM_NCRBUTTONDOWN, WM_NCMBUTTONDOWN,
    WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_MBUTTONDOWN:
      if QueryMouseHide(Wnd) then
        PostMessage(Handle, WM_STRETCHHIDE, 0, 0);
  end;
end;

procedure TStretchWindow.Popup(X, Y: Integer);
begin
  if not Visible then
  begin
    StretchHide;
    Left := X;
    Top := Y;
    StretchShow;
  end;
end;

procedure TStretchWindow.Select;
begin
  Click;
end;

procedure TStretchWindow.Stretch;
begin
end;

function TStretchWindow.QueryMouseHide(Wnd: HWND): Boolean;
begin
  Result := Wnd <> Handle;
end;

procedure TStretchWindow.StretchShow;
var
  CanStretch: Boolean;
  StartPosition: TWindowPosition;
begin
  if not Visible then
  begin
    CanStretch := True;
    GetWindowPosition(Handle, FPosition);
    StartPosition := FPosition;
    with StartPosition do
      case FMode of
        smNone:
          CanStretch := False;
        smLeft:
            Width := 0;
        smTop:
            Height := 0;
        smRight:
          begin
            Left := Left + Width;
            Width := 0;
          end;
        smBottom:
          begin
            Top := Top + Height;
            Height := 0;
          end;
        smExplode:
          begin
            Width := 0;
            Height := 0;
          end;
        smZoom:
          begin
            Left := Left + Width div 2;
            Top := Top + Height div 2;
            Width := 0;
            Height := 0;
          end;
      end;
    CanStretch := CanStretch and (FStretcher.Interval > 0);
    if CanStretch then
    begin
      SetWindowPosition(Handle, StartPosition);
      FStretcher.Stretch(FPosition);
    end;
    FCaptured := False;
    Show;
  end;
end;

procedure TStretchWindow.StretchHide;
begin
  if Visible then
  begin
    FStretcher.KillThread;
    Hide;
    SetWindowPosition(Handle, FPosition);
    Application.ProcessMessages;
    if (not FClicking) and Assigned(FOnCancel) then
      FOnCancel(Self);
    FClicking := False;
  end;
end;

procedure TStretchWindow.WndProc(var Message: TMessage);
const
  WM_POPUPBASE     = WM_USER + $B800;
  WM_POPUPKEYFIRST = WM_POPUPBASE + WM_KEYFIRST;
  WM_POPUPKEYLAST  = WM_POPUPKEYFIRST + 2;
begin
  with Message do
    case Msg of
      WM_POPUPKEYFIRST..WM_POPUPKEYLAST:
        Perform(Msg - WM_POPUPBASE, WParam, LParam);
    end;
  inherited WndProc(Message);
end;

function TStretchWindow.GetInterval: Integer;
begin
  Result := FStretcher.Interval;
end;

procedure TStretchWindow.SetInterval(Value: Integer);
begin
  StretchHide;
  FStretcher.Interval := Value;
end;

procedure TStretchWindow.SetMode(Value: TStretchMode);
begin
  if Value <> FMode then
  begin
    StretchHide;
    FMode := Value;
  end;
end;

procedure TStretchWindow.WMActivateApp(var Message: TWMActivateApp);
begin
  inherited;
  StretchHide;
end;

procedure TStretchWindow.WMStretchHide(var Message: TMessage);
begin
  StretchHide;
end;

procedure TStretchWindow.StretcherStretch(Sender: TObject);
begin
  Stretch;
end;

{ TCustomStretchList }

constructor TCustomStretchList.Create(AOwner: TComponent);
var
  NonClientMetrics: TNonClientMetrics;
begin
  inherited Create(AOwner);
  NonClientMetrics.cbSize := SizeOf(TNonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
  begin
    FItemHeight := NonClientMetrics.iMenuHeight;
    Font.Handle := CreateFontIndirect(NonClientMetrics.lfMenuFont);
  end
  else
    FItemHeight := 18;
  FTopIndex := 0;
end;

destructor TCustomStretchList.Destroy;
begin
  if FHookRef > 0 then
    UnhookKeyboard(KeyboardHook);
  inherited Destroy;
end;

procedure TCustomStretchList.AdjustScrollRect(var Rect: TRect);
begin
end;

function TCustomStretchList.CanDrawBackground: Boolean;
begin
  Result := not Assigned(FOnDrawBackground);
end;

procedure TCustomStretchList.Click;
begin
  if (not (FDragging or FStretching)) and (FItemIndex > -1) then
  begin
    FClicking := True;
    StretchHide;
    inherited Click;
  end;
end;

procedure TCustomStretchList.DoShow;
begin
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or
    SWP_NOACTIVATE);
  UpdateWindow(Handle);
  inherited DoShow;
end;

procedure TCustomStretchList.DrawCaption(Rect: TRect);
begin
  if Assigned(FOnDrawCaption) then
    FOnDrawCaption(Self, Rect)
  else
  begin
    InflateRect(Rect, -4, 0);
    GraphTools.DrawCaption(Canvas.Handle, Caption, Rect, drLeft);
    InflateRect(Rect, 4, 0);
    OffSetRect(Rect, 0, HeightOf(Rect) -2);
    DrawDivider(Canvas.Handle, Rect, ddVert);
  end;
end;

procedure TCustomStretchList.DrawBackground;
begin
  if Assigned(FOnDrawBackground) then
    FOnDrawBackground(Self);
end;

procedure TCustomStretchList.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  if Assigned(FOnDrawItem) then
    FOnDrawItem(Self, Index, Rect, State)
  else
    Canvas.FillRect(Rect);
end;

procedure TCustomStretchList.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Index: Integer;
  Pos: TWindowPosition;
  Grip: TPolygon;
  Rect: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);
  Grip := nil;
  if Visible then
  begin
    FDragging := PtInRect(GetCaptionRect, GetPoint(X, Y));
    if FDragging then
    begin
      if HitTestCaption(X, Y) then
        FDragPoint := Point(X, Y)
      else
        FDragging := False;
      Exit;
    end;
    if FCaptured then Exit;
    GetWindowPosition(Handle, Pos);
    with Pos do
      Grip := GetPolygon([Width, Height, Width -
        GetSystemMetrics(SM_CXVSCROLL), Height, Width, Height -
        GetSystemMetrics(SM_CYHSCROLL)]);
    if PointInPolygon(Point(X, Y), Grip) then
    begin
      FStretching := True;
      SetCursor(Screen.Cursors[crSizeNWSE]);
      Exit;
    end;
    for Index := 1 to 2 do
    begin
      case Index of
        1: Rect := GetUpRect;
        2: Rect := GetDownRect;
      end;
      if PtInRect(Rect, Point(X, Y)) then
      begin
        case Index of
          1: FUpButtonPressed := True;
          2: FDownButtonPressed := True;
        end;
        Captured := True;
        InvalidateRect(Handle, @Rect, False);
        SetTimer(Handle, Index, 50, nil);
        Exit;
      end;
    end;
    Index := ItemFromPoint(Point(X, Y));
    if Index > -1 then
    begin
      ItemIndex := Index;
      Exit;
    end;
  end;
end;

procedure TCustomStretchList.MouseMove(Shift: TShiftState; X, Y: Integer);

  procedure Drag;
  begin
    FPosition.Left := FPosition.Left + X - FDragPoint.X;
    FPosition.Top :=  FPosition.Top + Y - FDragPoint.Y;
    SetWindowPosition(Handle, FPosition);
  end;

  procedure Stretch;
  var
    XGrip: Integer;
    YGrip: Integer;
    XSize: Integer;
    YSize: Integer;
    Rect: TRect;
  begin
    XGrip := X + GetSystemMetrics(SM_CXVSCROLL) div 3;
    YGrip := Y + GetSystemMetrics(SM_CYHSCROLL) div 3;
    XSize := 75;
    YSize := HeightOf(GetCaptionRect) + GetSystemMetrics(SM_CYHSCROLL) * 2 + 6;
    if (XGrip > XSize) or (YGrip > YSize) or (FPosition.Width <> XSize) or
      (FPosition.Height <> YSize) then
    begin
      if XGrip > XSize then
        FPosition.Width := XGrip
      else
        FPosition.Width := XSize;
      if Y > YSize then
        FPosition.Height := YGrip
      else
        FPosition.Height := YSize;
      SetWindowPosition(Handle, FPosition);
      UpdateButtons;
      Rect := ClientRect;
      Invalidate;
    end;
  end;

begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then
    Drag
  else if FStretching then
    Stretch
  else if not Captured then
  begin
    SetCursor(Screen.Cursors[crDefault]);
    ItemIndex := ItemFromPoint(Point(X, Y));
  end;
end;

procedure TCustomStretchList.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Rect: TRect;
  Index: Integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  FDragging := False;
  FStretching := False;
  SetCursor(Screen.Cursors[crDefault]);
  if FUpButtonPressed then
  begin
    Rect := GetUpRect;
    InvalidateRect(Handle, @Rect, False);
  end
  else if FDownButtonPressed then
  begin
    Rect := GetDownRect;
    InvalidateRect(Handle, @Rect, False);
  end;
  FUpButtonPressed := False;
  FDownButtonPressed := False;
  if not Captured then
  begin
    Index := ItemFromPoint(Point(X, Y));
    ItemIndex := Index;
  end;
  Captured := False;
end;

function TCustomStretchList.GetClippingPath: TPolygon;
begin
  with ListRect do
    if FDownButtonVisible then
      Result := GetPolygon([Left, Top, Right - 1, Top, Right - 1,
        Bottom - 1, Left, Bottom - 1 ])
    else
      Result := GetPolygon([Left, Top, Right - 1, Top, Right - 1,
        Bottom - GetSystemMetrics(SM_CYHSCROLL) - 1, Right -
        GetSystemMetrics(SM_CXHSCROLL) - 1, Bottom -1, Left, Bottom - 1 ]);
end;

function TCustomStretchList.GetCaptionRect: TRect;
begin
  Result := ClientRect;
  InflateRect(Result, -1, -1);
  Result.Bottom := Result.Top + FItemHeight + 5;
end;

function TCustomStretchList.GetUpRect: TRect;
begin
  SetRectEmpty(Result);
  if FUpButtonVisible then
  begin
    Result := ClientRect;
    InflateRect(Result, -2, -1);
    OffsetRect(Result, 0, HeightOf(GetCaptionRect));
    Result.Bottom := Result.Top + GetSystemMetrics(SM_CXVSCROLL) + 1;
  end;
end;

function TCustomStretchList.GetDownRect: TRect;
begin
  SetRectEmpty(Result);
  if FDownButtonVisible then
  begin
    Result := ClientRect;
    InflateRect(Result, -2, -2);
    Result.Top := Result.Bottom - GetSystemMetrics(SM_CXVSCROLL) - 1;
  end;
end;

function TCustomStretchList.HitTestCaption(X, Y: Integer): Boolean;
begin
  Result := True;
end;

procedure TCustomStretchList.InvalidateItem(Index: Integer);
var
  Rect: TRect;
  ItemRect: TRect;
begin
  if (Index > Count - 1) or (Index < FTopIndex) then
    Exit;
  Rect := ListRect;
  ItemRect := Rect;
  ItemRect.Bottom := ItemRect.Top + FItemHeight + 1;
  OffsetRect(ItemRect, 0, Index * FItemHeight - FTopIndex * FItemHeight);
  if (ItemRect.Bottom - 2 < Rect.Top) or (ItemRect.Top > Rect.Bottom - 2) then
    Exit;
  AdjustScrollRect(ItemRect);
  InvalidateRect(Handle, @ItemRect, True);
end;

function TCustomStretchList.ItemFromPoint(const Point: TPoint): Integer;
var
  Rect: TRect;
begin
  Result := -1;
  Rect := ListRect;
  if PtInRect(Rect, Point) then
  begin
    Result := FTopIndex + (Point.Y - Rect.Top) div FItemHeight;
    if Result > FCount - 1 then
      Result := -1;
  end;
end;

procedure TCustomStretchList.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_UP:
      if ItemIndex > -1 then
        ItemIndex := ItemIndex - 1;
    VK_DOWN:
      if ItemIndex < Count - 1 then
        ItemIndex := ItemIndex + 1;
    VK_ESCAPE:
      Cancel;
    VK_RETURN:
      Click;
  end;
end;

procedure TCustomStretchList.Paint;

  procedure ClipBorder(DC: HDC; const Rect: TRect);
  var
    Path: TPolygon;
  begin
    with Rect do
    begin
      BeginPath(DC);
      Path := GetPolygon([Left, Top, Right - 1, Top, Right - 1,
        Bottom - GetSystemMetrics(SM_CYHSCROLL) - 1, Right -
        GetSystemMetrics(SM_CXHSCROLL) - 1, Bottom -1, Left, Bottom - 1 ]);
      Polygon(DC, Pointer(Path)^, Length(Path));
      EndPath(DC);
      SelectClipPath(DC, RGN_AND);
    end;
  end;

  procedure DrawListItems(DC: HDC; Rect: TRect);
  var
    Path: TPolygon;
    DrawRect: TRect;
    ClipRegion: HRGN;
    Brush: HBRUSH;
    I: Integer;
  begin
    with Rect do
    begin
      ClipRegion := CreateRectRgn(Left, Top, Right, Bottom);
      GetClipRgn(DC, ClipRegion);
      BeginPath(DC);
      Path := GetClippingPath;
      Polygon(DC, Pointer(Path)^, Length(Path));
      EndPath(DC);
      SelectClipPath(DC, RGN_AND);
    end;
    DrawBackground;
    DrawRect := Rect;
    DrawRect.Bottom := DrawRect.Top + FItemHeight + 1;
    for I := FTopIndex to FCount - 1 do
    begin
      if DrawRect.Top > Rect.Bottom then
        Break;
      if I = FItemIndex then
        DrawItem(I, DrawRect, [odSelected])
      else
        DrawItem(I, DrawRect, []);
      OffsetRect(DrawRect, 0, FItemHeight);
    end;
    Rect.Top := DrawRect.Top;
    Brush := GetSysColorBrush(COLOR_BTNFACE);
    if CanDrawBackground then
      FillRect(DC, Rect, Brush);
    DeleteObject(Brush);
    SelectClipRgn(DC, ClipRegion);
    DeleteObject(ClipRegion);
  end;

const
  PushStyles: array[Boolean] of TDrawState = ([dsThin], [dsPressed]);
var
  DC: HDC;
  Rect: TRect;
  DrawRect: TRect;
  ClipRegion: HRGN;
begin
  inherited Paint;
  DC := Canvas.Handle;
  Rect := ClientRect;
  DrawFrame(DC, Rect, dfRaised);
  InflateRect(Rect, 0, -1);
  with Rect do
    ClipRegion := CreateRectRgn(Left, Top, Right, Bottom);
  GetClipRgn(DC, ClipRegion);
  ClipBorder(DC, Rect);
  DrawCaption(GetCaptionRect);
  DrawRect := GetUpRect;
  DrawThemeScroll(DC, drUp, DrawRect, PushStyles[FUpButtonPressed]);
  SelectClipRect(DC, DrawRect, RGN_DIFF);
  DrawRect := GetDownRect;
  DrawThemeScroll(DC, drDown, DrawRect, PushStyles[FDownButtonPressed]);
  SelectClipRect(DC, DrawRect, RGN_DIFF);
  DrawRect := ListRect;
  DrawListItems(DC, DrawRect);
  SelectClipRgn(DC, ClipRegion);
  DeleteObject(ClipRegion);
  DrawThemeGrip(DC, Rect);
end;

procedure TCustomStretchList.Stretch;
begin
  UpdateButtons;
  Invalidate;
end;

procedure TCustomStretchList.StretchShow;
begin
  if FCount > 0 then
    TopIndex := 0;
  Inc(FHookRef);
  if FHookRef = 1 then
    HookKeyboard(KeyboardHook);
  inherited StretchShow;
end;

procedure TCustomStretchList.StretchHide;
begin
  if FHookRef > 0 then
  begin
    Dec(FHookRef);
    if FHookRef = 0 then
      UnhookKeyboard(KeyboardHook);
  end;
  inherited StretchHide;
end;

procedure TCustomStretchList.KeyboardHook(Key: Word; State: Cardinal;
  var Remove: Boolean);
begin
  if State shr $1F = 0 then
    KeyDown(Key, []);
  Remove := True;
end;

procedure TCustomStretchList.UpdateButtons;
var
  Rect: TRect;
begin
  if FTopIndex > 0 then
    FUpButtonVisible := True
  else
  begin
    if FUpButtonPressed then
    begin
      Rect := GetUpRect;
      InvalidateRect(Handle, @Rect, False);
      FUpButtonPressed := False;
    end;
    FUpButtonVisible := False;
  end;
  Rect := ListRect;
  if FUpButtonVisible then
    Inc(Rect.Top, GetSystemMetrics(SM_CYHSCROLL));
  if (Count - FTopIndex) * FItemHeight > Rect.Bottom - Rect.Top - 1 then
    FDownButtonVisible := True
  else
  begin
    if FDownButtonPressed then
    begin
      Rect := GetUpRect;
      InvalidateRect(Handle, @Rect, False);
      FDownButtonPressed := False;
    end;
    FDownButtonVisible := False;
  end;
end;

procedure TCustomStretchList.SetCount(Value: Integer);
begin
  if Value <> FCount then
  begin
    if Value < 0 then Value := 0;
    FCount := Value;
    FItemIndex := -1;
    ItemIndex := 0;
    Invalidate;
  end;
end;

function TCustomStretchList.GetDisplayCount: Integer;
begin
  with ListRect do
    Result := (Bottom - Top) div ItemHeight;
end;

procedure TCustomStretchList.SetDisplayCount(Value: Integer);
var
  YSize: Integer;
begin
  if Value < 0 then Value := 0;
  YSize := GetSystemMetrics(SM_CYHSCROLL) * 3 + 12;
  FPosition.Height := FItemHeight * (Value + 1) + 8;
  if FPosition.Height < YSize then
    FPosition.Height := YSize;
  PInteger(@Height)^ := 0;
  Height := FPosition.Height;
end;

procedure TCustomStretchList.SetItemHeight(Value: Integer);
begin
  if Value <> FItemHeight then
  begin
    if Value < 0 then Value := 0;
    FItemHeight := Value;
    if HandleAllocated then
    begin
      UpdateButtons;
      Invalidate;
    end;
  end;
end;

procedure TCustomStretchList.SetItemIndex(Value: Integer);
begin
  if Value > Count -1 then
    Value := Count - 1;
  if Value <> FItemIndex then
  begin
    InvalidateItem(FItemIndex);
    FItemIndex := Value;
    InvalidateItem(FItemIndex);
    if FItemIndex < 0 then
      FItemIndex := -1
    else if FItemIndex < TopIndex then
      TopIndex := FItemIndex
    else if FItemIndex > TopIndex + DisplayCount then
      if FItemIndex = Count - 1 then
      begin
        TopIndex := FItemIndex - DisplayCount + 1;
        FDownButtonVisible := False;
      end
      else
        TopIndex := FItemIndex - DisplayCount;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

function TCustomStretchList.GetListRect: TRect;
begin
  Result := ClientRect;
  InflateRect(Result, -1, -1);
  with Result do
  begin
    Inc(Left);
    Inc(Top, HeightOf(GetCaptionRect));
    if FUpButtonVisible then
      Inc(Top, GetSystemMetrics(SM_CYHSCROLL) + 2);
    if FDownButtonVisible then
      Dec(Bottom, GetSystemMetrics(SM_CYHSCROLL) + 2);
  end;
end;

procedure TCustomStretchList.SetTopIndex(Value: Integer);
var
  UpButton, DownButton: Boolean;
  Rect, DirtyRect: TRect;
  PriorIndex: Integer;
begin
  if Value < 0 then
    Value := 0;
  if Value <> FTopIndex then
  begin
    UpButton := FUpButtonVisible;
    DownButton := FDownButtonVisible;
    Rect := ListRect;
    PriorIndex := FTopIndex;
    FTopIndex := Value;
    UpdateButtons;
    if (UpButton <> FUpButtonVisible) or (DownButton <> FDownButtonVisible) then
      Invalidate
    else
    begin
      AdjustScrollRect(Rect);
      DirtyRect := Rect;
      DirtyRect.Bottom := DirtyRect.Top + (HeightOf(DirtyRect) div ItemHeight)
        * ItemHeight;
      ScrollWindowEx(Handle, 0, (PriorIndex - FTopIndex) * ItemHeight, @DirtyRect,
        @DirtyRect, 0, nil, SW_ERASE or SW_INVALIDATE);
      if DirtyRect.Bottom < Rect.Bottom then
      begin
        Rect.Top := DirtyRect.Bottom;
        InvalidateRect(Handle, @Rect, True);
      end;
    end;
  end;
end;

procedure TCustomStretchList.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if HandleAllocated then
  begin
    ItemHeight := Canvas.TextHeight('Wg');
    Invalidate;
  end
end;

procedure TCustomStretchList.CMTextChanged(var Message: TMessage);
var
  Rect: TRect;
begin
  inherited;
  if HandleAllocated then
  begin
    Rect := GetCaptionRect;
    InvalidateRect(Handle, @Rect, True);
  end;
end;

procedure TCustomStretchList.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  Rect: TRect;
  Brush: HBRUSH;
  ClipRegion: HRGN;
  Path: TPolygon;
begin
  Rect := ListRect;
  with ClientRect do
    ClipRegion := CreateRectRgn(Left, Top, Right, Bottom);
  with Message, Rect do
  begin
    if Bottom - Top > FItemHeight * (FCount - FTopIndex) + 1 then
      Bottom := Top + FItemHeight * (FCount - FTopIndex) + 1;
    GetClipRgn(DC, ClipRegion);
    BeginPath(DC);
    Path := GetClippingPath;
    Polygon(DC, Pointer(Path)^, Length(Path));
    EndPath(DC);
    SelectClipPath(DC, RGN_XOR);
    Brush := GetSysColorBrush(COLOR_BTNFACE);
    FillRect(DC, ClientRect, Brush);
    DeleteObject(Brush);
    SelectClipRgn(DC, ClipRegion);
    DeleteObject(ClipRegion);
    Result := 1;
  end;
end;

procedure TCustomStretchList.WMNCHitTest(var Message: TWMNCHitTest);
begin
  inherited;
  FHitTest := ScreenToClient(SmallPointToPoint(Message.Pos));
end;

procedure TCustomStretchList.WMTimer(var Message: TWMTimer);
begin
  case Message.TimerID of
    1: if FUpButtonPressed then TopIndex := TopIndex - 1 else
        KillTimer(Handle, 1);
    2: if FDownButtonPressed then TopIndex := TopIndex + 1 else
        KillTimer(Handle, 2);
  end;
end;

{ TStretchMenu }

constructor TStretchMenu.Create(AOwner: TComponent);
var
  StringList: TStringList;
begin
  inherited Create(AOwner);
  StringList := TStringList.Create;
  StringList.OnChange := ItemsChange;
  FItems := StringList;
  FTitleFont := TFont.Create;
  with FTitleFont do
  begin
    Name := 'Arial Black';
    Color := clWhite;
    Size := 13;
  end;
  FTitleBase := clBackground;
  FTitleLead := clBlack;
  FDrawTitle := True;
end;

destructor TStretchMenu.Destroy;
begin
  inherited Destroy;
  FItems.Free;
  FTitleFont.Free;
end;

procedure TStretchMenu.AdjustScrollRect(var Rect: TRect);
begin
  Rect.Left := MenuLeft;
end;

function TStretchMenu.CanDrawBackground: Boolean;
begin
  Result := False;
end;

procedure TStretchMenu.DrawBackground;
var
  Rect: TRect;
  DrawRect: TRect;
  I: Integer;
begin
  with Canvas, DrawRect do
  begin
    Rect := ClientRect;
    DrawRect := Rect;
    if FDrawTitle then
    begin
      Left := 4;
      SwapFont(Font, FTitleFont);
      FMenuLeft := TextHeight('Wg');
      Right := FMenuLeft;
      I := Bottom - 5;
      Top := Bottom - 255;
      DrawGradient(Handle, DrawRect, FTitleBase, FTitleLead, 255, drUp);
      Brush.Color := FTitleLead;
      Bottom := Top;
      Top := 0;
      FillRect(DrawRect);
      if FDownButtonVisible then
        Dec(I, GetSystemMetrics(SM_CYHSCROLL));
      DrawAngledText(Handle, FTitle, 270, Point(1, I));
      DrawRect := Rect;
      Brush.Color := clBtnFace;
      Left := 0;
      Right := 4;
      FillRect(DrawRect);
      DrawRect := Rect;
      Left := FMenuLeft;
      SwapFont(Font, FTitleFont);
    end
    else
    begin
      FMenuLeft := 0;
      Brush.Color := clBtnFace;
    end;
    FillRect(DrawRect);
  end;
end;

procedure TStretchMenu.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with Canvas do
  begin
    if odSelected in State then
    begin
      Font.Color := clWindow;
      Brush.Color := clHighlight;
    end
    else
    begin
      Font.Color := clWindowText;
      Brush.Color := clBtnFace;
    end;
    Rect.Left := FMenuLeft;
    FillRect(Rect);
    InflateRect(Rect, -4, 0);
    if (Index < Count - 1) or (not FExtraLine) then
      GraphTools.DrawCaption(Handle, FItems[Index], Rect, drLeft);
    Font.Color := clWindowText;
    Brush.Color := clBtnFace;
  end;
end;

procedure TStretchMenu.ItemsChange(Sender: TObject);
var
  I: Integer;
begin
  I := FItems.Count;
  if FExtraLine then
    Inc(I);
  Count := I;
  Invalidate;
end;

procedure TStretchMenu.SetExtraLine(const Value: Boolean);
begin
  if Value <> FExtraLine then
  begin
    FExtraLine := Value;
    ItemsChange(nil);
  end;
end;

procedure TStretchMenu.SetItems(Value: TStrings);
begin
  FItems.Assign(Value);
  TopIndex := 0;
end;

procedure TStretchMenu.SetTitle(const Value: string);
begin
  if Value <> FTitle then
  begin
    FTitle := Value;
    Invalidate;
  end;
end;

{ TGridMenu }

constructor TGridMenu.Create(AOwner: TComponent);
var
  StringList: TStringList;
begin
  inherited Create(AOwner);
  StringList := TStringList.Create;
  StringList.OnChange := ColumnsChange;
  FColumns := StringList;
  FDragColumn := -1;
  FAutomaticSize := True;
end;

destructor TGridMenu.Destroy;
begin
  FColumns.Free;
  inherited Destroy;
end;

procedure TGridMenu.ColumnsChange(Sender: TObject);
begin
  FColumnCount := FColumns.Count;
  SetLength(FColWidths, FColumns.Count); // remove later
  FSortDirection := drDown;
  FSortColumn := 0;
  Invalidate;
end;

{procedure TGridMenu.BuildColumns;
var
  I: Integer;
begin
  SetLength(FColumnWidths, FColumns.Count);
  for I := 0 to FColumns.Count - 1 do
  begin
    FColumnWidths
  end;
end;}

procedure TGridMenu.SetAutomaticSize(Value: Boolean);
begin
  if Value <> FAutomaticSize then
  begin
    FAutomaticSize := Value;
    Invalidate;
  end;
end;

procedure TGridMenu.DoCalculateCell(Column: Integer; var Cell: string);
begin
  if Assigned(FOnCalculateCell) then
    FOnCalculateCell(Self, Column, Cell);
end;

{

    Rect.Left := MenuLeft;
    Inc(Rect.Right);
    InflateRect(Rect, -4, 0);
    BoxRect := GetCellRect(Rect);
    Rect.Right := Rect.Left + WidthOf(Rect) div FColumnCount;
    for I := 0 to FColumnCount - 1 do
    begin
      Inc(Rect.Left);
      BoxRect := Rect;
      if I = 0 then
        BoxRect.Left := 2;
      if I = FColumnCount - 1 then
        BoxRect.Right := Width - 2
      else if (not AutomaticSize) and  (FColWidths[I] > 0)  then
        BoxRect.Right := BoxRect.Left + Trunc(WidthOf(Rect) / FColWidths[I] / 100);
      DrawBox(DC, clBtnShadow, BoxRect);
      InflateRect(BoxRect, -4, 0);
      GraphTools.DrawCaption(DC, FColumns[I], BoxRect, drLeft);
      if Assigned(FOnSortColumns) and (I = FSortColumn) then
      begin
        Inc(BoxRect.Left, CalculateCaptionSize(DC, FColumns[I]).cx);
        if BoxRect.Right > BoxRect.Left + 12 then
        begin
          BoxRect.Right := BoxRect.Left + 12;
          DrawSortArrow(DC, BoxRect, FSortDirection);
        end;
      end;
      Dec(Rect.Left);
      Slide(Rect, drRight);
    end;
  end

}

function TGridMenu.GetCellRect(const Bounds: TRect; Index: Integer): TRect;
begin
  Result := Bounds;
  if Index > 0 then
    Result.Left := GetCellRect(Bounds, Index - 1).Right + 2;
  if (Index = FColumnCount - 1) or (FColumnCount = 0) then
    Result.Right := Bounds.Right
  else if AutomaticSize then
    Result.Right := Result.Left + Trunc(WidthOf(Bounds) / FColumnCount)
  else if FColWidths[Index] > 0 then
    Result.Right := Result.Left + Trunc(WidthOf(Bounds) * FColWidths[Index] / 100)
  else
    Result.Right := Result.Left + 1;
end;

procedure TGridMenu.DrawCaption(Rect: TRect);
var
  DC: HDC;
  BoxRect: TRect;
  I: Integer;
begin
  Rect.Bottom := Rect.Top + HeightOf(Rect) div 2;
  InflateRect(Rect, -4, 0);
  DC := Canvas.Handle;
  GraphTools.DrawCaption(DC, Caption, Rect, drLeft);
  InflateRect(Rect, 4, 0);
  OffSetRect(Rect, 0, HeightOf(Rect) - 2);
  DrawDivider(Canvas.Handle, Rect, ddVert);
  OffSetRect(Rect, 0, 2);
  InflateRect(Rect, -1, -1);
  if FColumnCount > 0 then
    for I := 0 to FColumnCount - 1 do
    begin
      BoxRect := GetCellRect(Rect, I);
      if ThemePainter.Enabled then
        DrawThemeThinButton(DC, BoxRect, [dsHot])
      else
        DrawBox(DC, clBtnShadow, BoxRect);
      InflateRect(BoxRect, -4, 0);
      GraphTools.DrawCaption(DC, FColumns[I], BoxRect, drLeft);
      if Assigned(FOnSortColumns) and (I = FSortColumn) then
      begin
        Inc(BoxRect.Left, CalculateCaptionSize(DC, FColumns[I]).cx);
        if BoxRect.Right > BoxRect.Left + 12 then
        begin
          BoxRect.Right := BoxRect.Left + 6;
          DrawSortArrow(DC, BoxRect, FSortDirection);
        end;
      end;
    end;
end;

procedure TGridMenu.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  DrawRect: TRect;
  Row, Cell: string;
  I: Integer;
begin
  with Canvas do
  begin
    if odSelected in State then
    begin
      Font.Color := clWindow;
      Brush.Color := clHighlight;
    end
    else
    begin
      Font.Color := clWindowText;
      Brush.Color := clBtnFace;
    end;
    Rect.Left := MenuLeft;
    FillRect(Rect);
    InflateRect(Rect, -4, 0);
    DrawRect := Rect;
    if (Index = Count - 1) and (ExtraLine) then
    begin
      InflateRect(DrawRect, 2, 0);
      if not (odSelected in State) then
        DrawThemeThinButton(Canvas.Handle, DrawRect, [dsHot]);
      InflateRect(DrawRect, -5, 0);
      GraphTools.DrawCaption(Handle, StatusText, DrawRect,  drLeft);
    end
    else if FColumnCount > 0 then
    begin
      Row := Items[Index];
      for I := 0 to FColumnCount - 1 do
      begin
        Cell := Row;
        DoCalculateCell(I, Cell);
        DrawRect := Rect;
        DrawRect.Left := 1;
        Inc(DrawRect.Right, 2);
        DrawRect := GetCellRect(DrawRect, I);
        if DrawRect.Left < MenuLeft then
          DrawRect.Left := MenuLeft;
        InflateRect(DrawRect, -4, 0);
        GraphTools.DrawCaption(Handle, Cell, DrawRect, drLeft);
        InflateRect(DrawRect, 4, 0);
        Slide(DrawRect, drRight, 1);
        if (DrawRect.Left > MenuLeft) and (I < FColumnCount - 1) then
          DrawDivider(Handle, DrawRect, ddHorz);
      end;
      if Index = Count - 1 then
      begin
        InflateRect(Rect, 4, 0);
        OffsetRect(Rect, 0, HeightOf(Rect));
          DrawDivider(Canvas.Handle, Rect, ddVert);
      end;
    end;
    Font.Color := clWindowText;
    Brush.Color := clBtnFace;
  end;
end;

function TGridMenu.GetCaptionRect: TRect;
begin
  Result := inherited GetCaptionRect;
  Result.Bottom := Result.Top + HeightOf(Result) * 2;
end;

function TGridMenu.HitTestCaption(X, Y: Integer): Boolean;
var
  Rect: TRect;
  Point: TPoint;
  I: Integer;
begin
  Rect := GetCaptionRect;
  Rect.Bottom := Rect.Top + HeightOf(Rect) div 2;
  Point := Classes.Point(X, Y);
  Result := PtInRect(Rect, Point);
  if Assigned(FOnSortColumns) then
  begin
    Slide(Rect);
    for I := 0 to FColumnCount - 1 do
      if PtInRect(GetCellRect(Rect, I), Point) then
      begin
        if I <> FSortColumn then
          Sort(I, drDown)
        else if FSortDirection = drUp then
          Sort(I, drDown)
        else
          Sort(I, drUp);
        Break;
      end;
  end;
end;

procedure TGridMenu.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  { TODO: Allow column sizing }
end;

procedure TGridMenu.MouseUp(Button: TMouseButton; Shift: TShiftState;
 X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
    FDragColumn := -1;
end;

procedure TGridMenu.StretchHide;
begin
  inherited StretchHide;
end;

var
  SortInstance: TGridMenu;

function GridMenuSort(List: TStringList; Index1, Index2: Integer): Integer;
begin
  if Index1 = Index2 then
    Result := 0
  else with SortInstance do
  begin
    Result := FOnSortColumns(SortInstance, FSortColumn, Index1, Index2);
    if Result = 0 then
      if Index1 < Index2 then
        Result := 1
      else
        Result := -1;
    if FSortDirection = drUp then
      Result := -Result;
  end;
end;

procedure TGridMenu.Sort(Column: Integer; Direction: TDirection);
begin
  FSortColumn := Column;
  if Direction = drDown then
    FSortDirection := drDown
  else
    FSortDirection := drUp;
  if Assigned(FOnSortColumns) then
  begin
    SortInstance := Self;
    (Items as TStringList).CustomSort(GridMenuSort);
  end;
  Invalidate;
end;

procedure TGridMenu.SetColumns(Value: TStrings);
begin
  FColumns.Assign(Value);
end;

function TGridMenu.GetColWidths(Index: Integer): Integer;
begin
  Result := FColWidths[Index];
end;

procedure TGridMenu.SetColWidths(Index: Integer; Value: Integer);
var
//  Delta: Integer;
  Rect: TRect;
  I: Integer;
begin
  if (not AutomaticSize) and HandleAllocated and Visible then
  begin
    Rect := ListRect;
    for I := 0 to Index do
    //Rect.
  end;
  FColWidths[Index] := Value;
  //ScrollWindow()
end;

procedure TGridMenu.SetStatusText(Value: string);
begin
  Cancel;
  FStatusText := Value;
  ExtraLine := FStatusText <> '';
end;

procedure TGridMenu.SetOnSortColumns(Value: TSortColumnsEvent);
begin
  FOnSortColumns := Value;
  FSortDirection := drDown;
  FSortColumn := 0;
  Invalidate;
end;

end.
