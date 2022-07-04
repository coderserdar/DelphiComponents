
(********************************************************)
(*                                                      *)
(*  Bare Object Library @ www.codebot.org/delphi        *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit BareForms;

interface

{$I BARE.INC}

uses
	{$IFNDEF BARE}Classes, SysUtils, FileTools, StrTools,{$ENDIF}
  BareUtils, Windows, Messages;

{ Modal result values }

const
  mrNone = TModalResult(0);
  mrOk = TModalResult(IDOK);
  mrCancel = TModalResult(IDCANCEL);
  mrAbort = TModalResult(IDABORT);
  mrRetry = TModalResult(IDRETRY);
  mrIgnore = TModalResult(IDIGNORE);
  mrYes = TModalResult(IDYES);
  mrNo = TModalResult(IDNO);

{  Key codes not defined in windows }

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
  VK_TILDE = $C0;
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

{ Window management routines }

type
  TWindowPosition = record
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
  end;

procedure GetWindowPosition(Wnd: HWND; var Pos: TWindowPosition);
procedure SetWindowPosition(Wnd: HWND; const Pos: TWindowPosition);

function CreateWindowPosition(Left, Top, Width, Height: Integer): TWindowPosition;

{ TWindow class }

type
  TCreateParams = record
    ClassStyle: Integer;
    Brush: HBrush;
    Icon: HIcon;
    Name: string;
    Style: Cardinal;
    ExStyle: Cardinal;
    Text: string;
    Position: TWindowPosition;
  end;

  TNotifyEvent = procedure(Sender: TObject) of object;

  TWindow = class(TObject)
  private
    FDestroying: Boolean;
    FHandle: THandle;
    FParent: HWND;
    procedure CreateHandle;
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function GetClientHeight: Integer;
    procedure SetClientHeight(Value: Integer);
    function GetClientRect: TRect;
    function GetClientWidth: Integer;
    procedure SetClientWidth(Value: Integer);
    procedure SetDim(Index: Integer; Value: Integer);
    function GetDim(Index: Integer): Integer;
    function GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    function GetHandle: THandle;
    function GetParent: HWND;
    procedure SetParent(Wnd: HWND);
    function GetPosition: TWindowPosition;
    procedure SetPosition(const Value: TWindowPosition);
    function GetVisible: Boolean;
    procedure SetVisible(Value: Boolean);
    procedure WMNCDestroy(var Message: TWMNCDestroy); message WM_NCDESTROY;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    procedure AllocateHandle;
    procedure CreateParams(var Params: TCreateParams); virtual;
    procedure Resize(Width, Height: Integer); virtual;
    function WindowProc(Msg: LongWord; wParam, lParam: LongInt): LongInt; virtual;
    {property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnLeave: TNotifyEvent read FOnLeave write FOnLeave;}
  public
    constructor Create(Parent: HWND = 0); virtual;
    destructor Destroy; override;
    function ClientToScreen(const Point: TPoint): TPoint;
    procedure DefaultHandler(var Message); override;
    procedure Hide;
    function ScreenToClient(const Point: TPoint): TPoint;
    procedure Show;
    procedure Update;
    property Caption: string read GetCaption write SetCaption;
    property ClientHeight: Integer read GetClientHeight write SetClientHeight;
    property ClientRect: TRect read GetClientRect;
    property ClientWidth: Integer read GetClientWidth write SetClientWidth;
    property Handle: THandle read GetHandle;
    property Height: Integer index 3 read GetDim write SetDim;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Left: Integer index 0 read GetDim write SetDim;
    property Parent: HWND read GetParent write SetParent;
    property Position: TWindowPosition read GetPosition write SetPosition;
    property Top: Integer index 1 read GetDim write SetDim;
    property Visible: Boolean read GetVisible write SetVisible;
    property Width: Integer index 2 read GetDim write SetDim;
  end;

  TWindowClass = class of TWindow;

{ TInterfacedWindow class }

	TInterfacedWindow = class(TWindow)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

{ TOutputWindow class }

  TOutputWindow = class(TWindow)
  private
    FOutputText: string;
    FOutputChanged: Boolean;
    procedure UpdateDisplay(DC: HDC);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    procedure Write(const S: string);
  end;

{ TDialog class }

  TDialog = class(TWindow)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

{ TForm class }

  TForm = class(TDialog)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Maximize;
    procedure Minimize;
    procedure Restore;
  end;

{ TInputDevice class }

  TInputDevice = class(TObject)
  protected
    procedure Reset; virtual; abstract;
  end;

{ TKeyboard class }

const
  KeyActive = $80;

type
  TKeyBuffer = array[Low(Byte)..High(Byte)] of Byte;
  PKeyBuffer = ^TKeyBuffer;

  TKeyboard = class(TInputDevice)
  private
    FKeys: TKeyBuffer;
    FKeyBuffer: PKeyBuffer;
    FToggles: TKeyBuffer;
    function GetKey(Index: Byte): Boolean;
    procedure SetKey(Index: Byte; Value: Boolean);
    function GetPressed(Index: Byte): Boolean;
    procedure SetPressed(Index: Byte; Value: Boolean);
    function GetToggled(Index: Byte): Boolean;
    procedure SetToggled(Index: Byte; Value: Boolean);
  protected
    procedure Reset; override;
  public
    constructor Create;
    property KeyBuffer: PKeyBuffer read FKeyBuffer;
    property Key[Index: Byte]: Boolean read GetKey write SetKey;
    property Pressed[Index: Byte]: Boolean read GetPressed write SetPressed;
    property Toggled[Index: Byte]: Boolean read GetToggled write SetToggled;
  end;

{ TMouse class }

  TMouse = class(TInputDevice)
  private
    FLeftButton: Boolean;
    FRightButton: Boolean;
    function GetX: Integer;
    function GetY: Integer;
    function GetPosition: TPoint;
  protected
    procedure Reset; override;
  public
    property LeftButton: Boolean read FLeftButton;
    property Position: TPoint read GetPosition;
    property RightButton: Boolean read FRightButton;
    property X: Integer read GetX;
    property Y: Integer read GetY;
  end;

{ TApplication class }

  TColorDepth = (cdDefault, cdBlackWhite, cdSafe, cdBasic, cdHigh, cdReal,
    cdTrue);

  TDisplayInfo = record
    Exclusive: Boolean;
    Width: Integer;
    Height: Integer;
    ColorDepth: TColorDepth;
    Prompt: Boolean;
    Caption: ShortString;
  end;

  TApplication = class(TObject)
  private
    FActive: Boolean;
    FColorDepth: TColorDepth;
    FExclusive: Boolean;
    FGreedy: Boolean;
    FHeight: Integer;
    FKeyboard: TKeyboard;
    FMouse: TMouse;
    FTerminated: Boolean;
    FWidth: Integer;
    FWindow: TWindow;
    FOutputWindow: TOutputWindow;
    procedure DisplayChanged;
    procedure ResetDisplay;
    procedure SetActive(Value: Boolean);
    procedure SetExclusive(Value: Boolean);
  public
    constructor Create(WindowClass: TWindowClass = nil); virtual;
    destructor Destroy; override;
    destructor Run; virtual;
    procedure Quit;
    procedure Write(const S: string);
    property Active: Boolean read FActive write SetActive;
    property ColorDepth: TColorDepth read FColorDepth write FColorDepth;
    property Exclusive: Boolean read FExclusive write SetExclusive;
    property Greedy: Boolean read FGreedy write FGreedy;
    property Height: Integer read FHeight write FHeight;
    property Keyboard: TKeyboard read FKeyboard;
    property Mouse: TMouse read FMouse;
    property Terminated: Boolean read FTerminated write FTerminated;
    property Width: Integer read FWidth write FWidth;
    property Window: TWindow read FWindow write FWindow;
  end;

  TApplicationClass = class of TApplication;

var
  Application: TApplication = nil;
  UniqueInstance: string = '';

function BeginUniqueInstance: THandle;
procedure EndUniqueInstance(Handle: THandle);

{ Display intialization rountines }

procedure InitDisplay(Exclusive: Boolean; Width, Height: Integer;
  ColorDepth: TColorDepth; Prompt: Boolean = False); overload;

procedure InitDisplay(const DisplayInfo: TDisplayInfo); overload;

implementation

{ General window helper routines }

procedure GetWindowPosition(Wnd: HWND; var Pos: TWindowPosition);
begin
  GetWindowRect(Wnd, TRect(Pos));
  with Pos do
  begin
    Dec(Width, Left);
    Dec(Height, Top);
  end;
  MapWindowPoints(GetDesktopWindow, GetParent(Wnd), TRect(Pos).TopLeft, 1);
end;

procedure SetWindowPosition(Wnd: HWND; const Pos: TWindowPosition);
begin
  with Pos do
    MoveWindow(Wnd, Left, Top, Width, Height, True);
end;

function CreateWindowPosition(Left, Top, Width, Height: Integer): TWindowPosition;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Width := Width;
  Result.Height := Height;
end;

function CreateRect(Left, Top, Right, Bottom: Integer): TRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

function CreatePoint(X, Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function HeightOf(const Rect: TRect): Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;

function WidthOf(const Rect: TRect): Integer;
begin
  Result := Rect.Right - Rect.Left;
end;

{ Main window procedure }

function WndProc(Wnd: HWND; uMsg, wParam, lParam: LongInt): LongInt; stdcall;
var
  Window: TWindow;
  Msg: TMessage;
begin
  Window := TWindow(GetWindowLong(Wnd, GWL_USERDATA));
  if Window = nil then
    case uMsg of
      WM_CREATE, WM_NCCREATE:
        begin
          Window := PCreateStruct(lParam).lpCreateParams;
          Window.FHandle := Wnd;
          SetWindowLong(Wnd, GWL_USERDATA, Integer(Window));
        end;
    end;
  Msg.Msg := uMsg;
  Msg.wParam := wParam;
  Msg.lParam := lParam;
  Msg.Result := 0;
  try
    if Window <> nil then
      Window.Dispatch(Msg)
    else
      Msg.Result := DefWindowProc(Wnd, uMsg, wParam, lParam);
  except
    on E: Exception do
      if (Application <> nil) and (Application.Window <> nil) then
        MessageBox(Application.Window.Handle, PChar(E.Message), 'Error', MB_OK or
          MB_ICONERROR or MB_DEFBUTTON1)
      else
        MessageBox(0, PChar(E.Message), 'Error', MB_OK or MB_ICONERROR or MB_DEFBUTTON1);
  end;
  Result := Msg.Result;
end;

{ TWindow }

constructor TWindow.Create(Parent: HWND = 0);
begin
  inherited Create;
  FParent := Parent;
  AllocateHandle;
end;

destructor TWindow.Destroy;
begin
  FDestroying := True;
  if FHandle <> 0 then
    DestroyWindow(FHandle);
  inherited Destroy;
end;

procedure TWindow.AllocateHandle;
begin
  if FHandle = 0 then CreateHandle;
end;

procedure TWindow.CreateParams(var Params: TCreateParams);
begin
  with Params do
  begin
    Brush := COLOR_BTNFACE + 1;
    Name := ClassName;
  end;
end;

procedure TWindow.CreateHandle;
var
  Params: TCreateParams;
  WndClassEx: TWndClassEx;
begin
  FillChar(Params, SizeOf(Params), #0);
  CreateParams(Params);
  FillMemory(@WndClassEx, SizeOf(WndClassEx), 0);
  WndClassEx.cbSize := SizeOf(WndClassEx);
  with Params do
    if not GetClassInfoEx(MainInstance, PChar(Name), WndClassEx) then
    begin
      WndClassEx.Style := ClassStyle;
      WndClassEx.lpfnWndProc := @WndProc;
      WndClassEx.hInstance := MainInstance;
      WndClassEx.lpszClassName := PChar(Name);
      WndClassEx.hbrBackground := Brush;
      WndClassEx.hIcon := Icon;
      if RegisterClassEx(WndClassEx) = 0 then
        raise Exception.Create('Could not register class');
    end;
  with Params, Position do
    FHandle := CreateWindowEx(ExStyle, PChar(Name), PChar(Text), Style,
      Left, Top, Width, Height, FParent, 0, 0, Self);
  if FHandle = 0 then
    raise Exception.Create('Could not create window.');
  SendMessage(FHandle, WM_SETFONT, GetStockObject(ANSI_VAR_FONT), 0);
end;

function TWindow.ClientToScreen(const Point: TPoint): TPoint;
begin
  Result := Point;
  Windows.ClientToScreen(Handle, Result);
end;

procedure TWindow.DefaultHandler(var Message);
var
  Msg: TMessage absolute Message;
begin
  with Msg do
    Result := WindowProc(Msg, wParam, lParam)
end;

procedure TWindow.Hide;
begin
  Visible := False;
end;

procedure TWindow.Resize(Width, Height: Integer);
begin
end;

procedure TWindow.Update;
begin
  UpdateWindow(Handle);
end;

function IsForeignWindow(Wnd: HWND): Boolean;
var
  Process: THandle;
begin
  if IsWindow(Wnd) then
  begin
    GetWindowThreadProcessId(Wnd, @Process);
    Result := Process <> GetCurrentProcessID;
  end
  else
    Result := True;
end;

function TWindow.WindowProc(Msg: LongWord; wParam, lParam: LongInt): LongInt;
var
  Keyboard: TKeyboard;
  Key: Byte;
begin
  if Application <> nil then
    case Msg of
      WM_ACTIVATEAPP:
      	Application.Active := wParam <> 0;
      WM_ACTIVATE:
        if IsForeignWindow(lParam) then
          Application.Active := LongRec(wParam).Lo <> WA_INACTIVE;
    	WM_ENDSESSION:
      	if wParam <> 0 then
      	begin
        	Application.Terminated := True;
          Result := 0;
          Exit;
        end;
      WM_KEYUP, WM_KEYDOWN:
        begin
          Keyboard := Application.Keyboard;
          Key := wParam;
          Keyboard.Key[Key] := Msg = WM_KEYDOWN;
          if Msg = WM_KEYUP then
            Keyboard.Toggled[Key] := not Keyboard.Toggled[Key];
        end;
      WM_LBUTTONDOWN:
        begin
          SetCapture(FHandle);
          Application.Mouse.FLeftButton := True;
        end;
      WM_LBUTTONUP:
        begin
          Application.Mouse.FLeftButton := False;
          ReleaseCapture;
        end;
      WM_RBUTTONDOWN:
      	Application.Mouse.FRightButton := True;
      WM_RBUTTONUP:
      	Application.Mouse.FRightButton := False;
    end;
  Result := DefWindowProc(Handle, Msg, wParam, lParam);
end;

function TWindow.ScreenToClient(const Point: TPoint): TPoint;
begin
  Result := Point;
  Windows.ScreenToClient(Handle, Result);
end;

procedure TWindow.Show;
begin
  Visible := True;
  Update;
end;

function TWindow.GetCaption: string;
begin
  SetLength(Result, GetWindowTextLength(Handle));
  GetWindowText(Handle, PChar(Result), Length(Result));
end;

procedure TWindow.SetCaption(const Value: string);
begin
  SetWindowText(Handle, PChar(Value))
end;

function TWindow.GetClientHeight: Integer;
begin
  Result := HeightOf(ClientRect);
end;

procedure TWindow.SetClientHeight(Value: Integer);
begin
  Height := Height - ClientHeight + Value;
end;

function TWindow.GetClientRect: TRect;
begin
  Windows.GetClientRect(Handle, Result);
end;

function TWindow.GetClientWidth: Integer;
begin
  Result := WidthOf(ClientRect);
end;

procedure TWindow.SetClientWidth(Value: Integer);
begin
  Width := Width - ClientWidth + Value;
end;

procedure TWindow.SetDim(Index: Integer; Value: Integer);
const
  Flags = SWP_NOZORDER or SWP_NOACTIVATE;
begin
  case Index of
    0: SetWindowPos(Handle, HWND_TOP, Value, Top, 0, 0, Flags or SWP_NOSIZE);
    1: SetWindowPos(Handle, HWND_TOP, Left, Value, 0, 0, Flags or SWP_NOSIZE);
    2: SetWindowPos(Handle, HWND_TOP, 0, 0, Value, Height, Flags or SWP_NOMOVE);
    3: SetWindowPos(Handle, HWND_TOP, 0, 0, Width, Value, Flags or SWP_NOMOVE);
  end;
end;

function TWindow.GetDim(Index: Integer): Integer;
begin
  Result := 0;
  with Position do
    case Index of
      0: Result := Left;
      1: Result := Top;
      2: Result := Width;
      3: Result := Height;
    end;
end;

function TWindow.GetEnabled: Boolean;
begin
  Result := IsWindowEnabled(Handle);
end;

procedure TWindow.SetEnabled(Value: Boolean);
begin
  EnableWindow(Handle, Value);
end;

function TWindow.GetHandle: THandle;
begin
  if FHandle = 0 then
    CreateHandle;
  Result := FHandle;
end;

function TWindow.GetParent: HWND;
begin
  Result := Windows.GetParent(Handle);
end;

procedure TWindow.SetParent(Wnd: HWND);
begin
  Windows.SetParent(Handle, Wnd);
end;

function TWindow.GetPosition: TWindowPosition;
begin
  GetWindowPosition(Handle, Result);
end;

procedure TWindow.SetPosition(const Value: TWindowPosition);
begin
  SetWindowPosition(Handle, Value);
end;

function TWindow.GetVisible: Boolean;
begin
  Result := IsWindowVisible(Handle);
end;

procedure TWindow.SetVisible(Value: Boolean);
const
  WindowVisible: array [Boolean] of Integer = (SW_HIDE, SW_SHOW);
begin
  ShowWindow(Handle, WindowVisible[Value]);
end;

procedure TWindow.WMNCDestroy(var Message: TWMNCDestroy);
begin
  with Message do
  begin
    Result := 0;
    if Application.Window = Self then
    begin
      Application.Quit;
      Application.Window := nil;
    end;
    FHandle := 0;
    if not FDestroying then
      Free;
  end;
end;

procedure TWindow.WMSize(var Message: TWMSize);
begin
  Resize(Message.Width, Message.Height);
  Message.Result := 0;
end;

{ TInterfacedWindow }

function TInterfacedWindow.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

function TInterfacedWindow._AddRef: Integer;
begin
	Result := -1;
end;

function TInterfacedWindow._Release: Integer;
begin
	Result := -1;
end;

{ TOutputWindow }

procedure TOutputWindow.CreateParams(var Params: TCreateParams);
const
  SolidBrush: HBRUSH = 0;
begin
  inherited CreateParams(Params);
  if SolidBrush = 0 then
    SolidBrush := CreateSolidBrush(GetSysColor(COLOR_INFOBK));
  with Params do
  begin
    Brush := SolidBrush;
    Position := CreateWindowPosition(10, 10, 0, 0);
    Style := WS_POPUP or WS_BORDER;
    ExStyle := ExStyle or WS_EX_TOPMOST or WS_EX_TOOLWINDOW;
  end;
end;

procedure TOutputWindow.UpdateDisplay(DC: HDC);
var
  Rect: TRect;
  Font: HFONT;
begin
  Font := SelectObject(DC, GetStockObject(DEFAULT_GUI_FONT));
  SetBkMode(DC, TRANSPARENT);
  with Rect do
  begin
    Left := 0;
    Top := 0;
    Right := 1024;
    Bottom := 1;
    Bottom := DrawText(DC, PChar(FOutputText), -1, Rect, DT_LEFT or DT_TOP or
      DT_WORDBREAK or DT_CALCRECT);
    if FOutputChanged then
    begin
      Width := Right + 12;
      Height := Bottom + 6;
      FOutputChanged := False;
    end;
    Left := 4;
    Top := 2;
    Right := Right + 4;
    Bottom := Bottom + 2;
  end;
  DrawText(DC, PChar(FOutputText), -1, Rect, DT_TOP or DT_LEFT or DT_WORDBREAK);
  SelectObject(DC, Font);
end;

procedure TOutputWindow.Write(const S: string);
var
  DC: HDC;
begin
  FOutputText := S;
  if FOutputText <> '' then
  begin
    ShowWindow(Handle, SW_SHOWNA);
    DC := GetDC(Handle);
    FOutputChanged := True;
    UpdateDisplay(DC);
    ReleaseDC(Handle, DC);
  end
  else
  begin
    Height := 0;
    Width := 0;
    ShowWindow(Handle, SW_HIDE);
  end;
end;

procedure TOutputWindow.WMPaint(var Message: TWMPaint);
var
  PS: TPaintStruct;
begin
  BeginPaint(Handle, PS);
  UpdateDisplay(PS.hdc);
  EndPaint(Handle, PS);
  Message.Result := 0;
end;

{ TDialog }

procedure TDialog.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_SYSMENU or WS_CAPTION;
    Text := Name;
    Position := CreateWindowPosition(0, 0, 150, 100);
  end;
end;

{ TForm }

procedure TForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    Style :=  Style or WS_OVERLAPPEDWINDOW;
end;

procedure TForm.Maximize;
begin
end;

procedure TForm.Minimize;
begin
end;

procedure TForm.Restore;
begin
end;

{ TKeyboard }

constructor TKeyboard.Create;
begin
  inherited Create;
  FKeyBuffer := @FKeys;
end;

procedure TKeyboard.Reset;
begin
  FillChar(FKeys, SizeOf(FKeys), #0);
end;

function TKeyboard.GetKey(Index: Byte): Boolean;
begin
  Result := FKeys[Index] and KeyActive = KeyActive;
end;

procedure TKeyboard.SetKey(Index: Byte; Value: Boolean);
begin
  if Value then
    FKeys[Index] := KeyActive
  else
    FKeys[Index] := 0;
end;

function TKeyboard.GetPressed(Index: Byte): Boolean;
begin
  Result := FKeys[Index] and KeyActive = KeyActive;
  FKeys[Index] := 0;
end;

procedure TKeyboard.SetPressed(Index: Byte; Value: Boolean);
begin
  if Value then
    FKeys[Index] := KeyActive
  else
    FKeys[Index] := 0;
end;

function TKeyboard.GetToggled(Index: Byte): Boolean;
begin
  Result := FToggles[Index] and KeyActive = KeyActive;
end;

procedure TKeyboard.SetToggled(Index: Byte; Value: Boolean);
begin
  if Value then
    FToggles[Index] := KeyActive
  else
    FToggles[Index] := 0;
end;

{ TMouse }

procedure TMouse.Reset;
begin
  FLeftButton := False;
  FRightbutton := False;
end;

function TMouse.GetPosition: TPoint;
begin
  GetCursorPos(Result);
end;

function TMouse.GetX: Integer;
begin
  Result := Position.X;
end;

function TMouse.GetY: Integer;
begin
  Result := Position.Y;
end;

{ TApplication }

constructor TApplication.Create(WindowClass: TWindowClass = nil);
begin
  inherited Create;
  Randomize;
  Application := Self;
  FKeyboard := TKeyboard.Create;
  FMouse := TMouse.Create;
  if WindowClass <> nil then
    FWindow := WindowClass.Create;
end;

destructor TApplication.Destroy;
begin
  FKeyboard.Free;
  FMouse.Free;
  FWindow.Free;
  Application := nil;
  inherited Destroy;
end;

procedure TApplication.DisplayChanged;

  procedure UpdateBorder;
  begin
    SetWindowLong(Window.Handle, GWL_STYLE, Integer(WS_POPUP or WS_VISIBLE));
    SetWindowLong(Window.Handle, GWL_EXSTYLE, WS_EX_APPWINDOW);
  end;

const
  ColorDepths: array[TColorDepth] of Cardinal = (0, 2, 4, 8, 16, 24, 32);
var
  DevMode: TDevMode;
begin
  if FExclusive and FActive then
  begin
    if (Width = 0) or (Height = 0) then
    begin
      Width := GetSystemMetrics(SM_CXSCREEN);
      Height := GetSystemMetrics(SM_CYSCREEN);
    end;
    FillChar(DevMode, SizeOf(TDevMode), #0);
    DevMode.dmSize := SizeOf(TDevMode);
    DevMode.dmPelsWidth := Width;
    DevMode.dmPelsHeight := Height;
    // DevMode.dmDisplayFrequency := 60;  or DM_DISPLAYFREQUENCY
    DevMode.dmFields := DM_PELSWIDTH or DM_PELSHEIGHT;
    if ColorDepth <> cdDefault then
    begin
      DevMode.dmBitsPerPel := ColorDepths[ColorDepth];
      DevMode.dmFields := DevMode.dmFields or DM_BITSPERPEL;
    end;
    FExclusive := ChangeDisplaySettings(DevMode, CDS_FULLSCREEN) =
      DISP_CHANGE_SUCCESSFUL;
    if FExclusive then
    begin
      UpdateBorder;
			Window.Position := CreateWindowPosition(0, 0, Width, Height);
    end
    else
      ResetDisplay;
  end
  else if FExclusive and (not FGreedy) then
  begin
    UpdateBorder;
    Window.Position := CreateWindowPosition(0, 0, 0, 0);
    ChangeDisplaySettings(PDevMode(nil)^, 0);
  end;
end;

procedure TApplication.ResetDisplay;
var
  Params: TCreateParams;
begin
  ChangeDisplaySettings(PDevMode(nil)^, 0);
  FillChar(Params, SizeOf(Params), #0);
  Window.CreateParams(Params);
  SetWindowLong(Window.Handle, GWL_STYLE, Params.Style or WS_VISIBLE);
  SetWindowLong(Window.Handle, GWL_EXSTYLE, Params.ExStyle);
  Window.Position := Params.Position;
end;

destructor TApplication.Run;
label
  Done;
var
  Context: THandle;
  Msg: TMsg;
begin
  Context := 0;
  // if Application.Window <> nil then
  try
    if Terminated then goto Done;
    if UniqueInstance <> '' then
    begin
      Context := BeginUniqueInstance;
      if Context = 0 then
      begin
        Window.Free;
        goto Done;
      end;
    end;
    if Window is TForm then
    begin
  	  Window.Show;
	    SetForegroundWindow(Window.Handle);
    end;
    try
      while not Terminated do
        if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
          if Msg.message <> WM_QUIT then
          begin
            TranslateMessage(Msg);
            DispatchMessage(Msg);
          end
          else
            Terminated := True
        else
          WaitMessage;
    except
      on E: Exception do
        MessageBox(Window.Handle, PChar(E.Message), 'Error', MB_OK or
          MB_ICONERROR or MB_DEFBUTTON1);
    end;
Done:
  finally
    EndUniqueInstance(Context);
    Window.Free;
    Window := nil;
  end;
end;

procedure TApplication.Quit;
begin
  PostQuitMessage(0);
end;

procedure TApplication.Write(const S: string);
begin
  if FOutputWindow = nil then
    FOutputWindow := TOutputWindow.Create;
  FOutputWindow.Write(S);
end;

procedure TApplication.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    FKeyboard.Reset;
    FMouse.Reset;
    DisplayChanged;
  end;
end;

procedure TApplication.SetExclusive(Value: Boolean);
begin
  if Value <> FExclusive then
  begin
    FExclusive := Value;
    if FActive then
      if FExclusive then
        DisplayChanged
      else
        ResetDisplay;
  end;
end;

function BeginUniqueInstance: THandle;
begin
  if UniqueInstance <> '' then
  begin
    Result := CreateMutex(nil, False, PChar(UniqueInstance));
    if GetLastError = ERROR_ALREADY_EXISTS then
    begin
      CloseHandle(Result);
      Result := 0;
    end;
  end
  else
    Result := 0;
end;

procedure EndUniqueInstance(Handle: THandle);
begin
  if Handle <> 0 then CloseHandle(Handle);
end;

procedure InitDisplay(Exclusive: Boolean; Width, Height: Integer;
  ColorDepth: TColorDepth; Prompt: Boolean = False);
var
  DisplayInfo: TDisplayInfo;
begin
  DisplayInfo.Exclusive := Exclusive;
  DisplayInfo.Width := Width;
  DisplayInfo.Height := Height;
  DisplayInfo.ColorDepth := ColorDepth;
  DisplayInfo.Prompt := Prompt;
  DisplayInfo.Caption := '';
  InitDisplay(DisplayInfo);
end;

procedure InitDisplay(const DisplayInfo: TDisplayInfo);
begin
  if Application <> nil then
    with DisplayInfo do
    begin
      if (Application.Window <> nil) and  (Application.Window.Caption =
        Application.Window.ClassName) then
        Application.Window.Caption := Caption;
      if Application.Exclusive and (not Exclusive) then
        Application.ResetDisplay;
      Application.Width := Width;
      Application.Height := Height;
      Application.ColorDepth := ColorDepth;
      if Prompt then
      begin
        Application.Exclusive := False;
        Application.Exclusive := MessageDialog(
          'Would you like to run in fullscreen mode?', 'Start Fullscreen?',
          mtConfirmation, mbYesNo) = IDYES
      end
      else
        Application.Exclusive := Exclusive;
    end;
end;

end.
