{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

{.$DEFINE USE_TIMER}
{ - Use Windows timer instead thread to the animated TrayIcon }

unit RXShell;

{$I RX.INC}
{$P+,W-,R-}

interface

uses
  Windows, Messages,
  Classes, Graphics, SysUtils, Forms, Controls, Menus, ShellAPI,
  {$IFDEF USE_TIMER} ExtCtrls, {$ENDIF} rxIcoList;

type
  TMouseButtons = set of TMouseButton;

{ TRxTrayIcon }

  TRxTrayIcon = class(TComponent)
  private
    FHandle: HWnd;
    FActive: Boolean;
    FAdded: Boolean;
    FAnimated: Boolean;
    FEnabled: Boolean;
    FClicked: TMouseButtons;
    FIconIndex: Integer;
    FInterval: Word;
    FIconData: TNotifyIconData;
    FIcon: TIcon;
    FIconList: TIconList;
{$IFDEF USE_TIMER}
    FTimer: TTimer;
{$ELSE}
    FTimer: TThread;
{$ENDIF}
    FHint: string;
    FShowDesign: Boolean;
    FPopupMenu: TPopupMenu;
    FOnClick: TMouseEvent;
    FOnDblClick: TNotifyEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    procedure ChangeIcon;
{$IFDEF USE_TIMER}
    procedure Timer(Sender: TObject);
{$ELSE}
    procedure Timer;
{$ENDIF}
    procedure SendCancelMode;
    function CheckMenuPopup(X, Y: Integer): Boolean;
    function CheckDefaultMenuItem: Boolean;
    procedure SetHint(const Value: string);
    procedure SetIcon(Value: TIcon);
    procedure SetIconList(Value: TIconList);
    procedure SetPopupMenu(Value: TPopupMenu);
    procedure Activate;
    procedure Deactivate;
    procedure SetActive(Value: Boolean);
    function GetAnimated: Boolean;
    procedure SetAnimated(Value: Boolean);
    procedure SetShowDesign(Value: Boolean);
    procedure SetInterval(Value: Word);
    procedure IconChanged(Sender: TObject);
    procedure WndProc(var Message: TMessage);
    function GetActiveIcon: TIcon;
  protected
    procedure DblClick; dynamic;
    procedure DoClick(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); dynamic;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateNotifyData; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Hide;
    procedure Show;
    property Handle: HWnd read FHandle;
  published
    property Active: Boolean read FActive write SetActive default True;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Hint: string read FHint write SetHint;
    property Icon: TIcon read FIcon write SetIcon;
    property Icons: TIconList read FIconList write SetIconList;
    { Ensure Icons is declared before Animated }
    property Animated: Boolean read GetAnimated write SetAnimated default False;
    property Interval: Word read FInterval write SetInterval default 150;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property ShowDesign: Boolean read FShowDesign write SetShowDesign stored False;
    property OnClick: TMouseEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
  end;

function IconExtract(const FileName: string; Id: Integer): TIcon;
procedure WinAbout(const AppName, Stuff: string);

type
  TExecState = (esNormal, esMinimized, esMaximized, esHidden);

function FileExecute(const FileName, Params, StartDir: string;
  InitialState: TExecState): THandle;
function FileExecuteWait(const FileName, Params, StartDir: string;
  InitialState: TExecState): Integer;

implementation

uses
  RxConst, RxCConst, rxVCLUtils, rxMaxMin;

procedure WinAbout(const AppName, Stuff: string);
var
  Wnd: HWnd;
  Icon: HIcon;
begin
  if Application.MainForm <> nil then
    Wnd := Application.MainForm.Handle
  else
    Wnd := 0;
  Icon := Application.Icon.Handle;
  if Icon = 0 then
    Icon := LoadIcon(0, IDI_APPLICATION);
  ShellAbout(Wnd, PChar(AppName), PChar(Stuff), Icon);
end;

function IconExtract(const FileName: string; Id: Integer): TIcon;
var
  S: array[0..255] of char;
  IconHandle: HIcon;
  Index: Word;
begin
  Result := TIcon.Create;
  try
    StrPLCopy(S, FileName, Length(S) - 1);
    IconHandle := ExtractIcon(hInstance, S, Id);
    if IconHandle < 2 then
    begin
      Index := Id;
      IconHandle := ExtractAssociatedIcon(hInstance, S, Index);
    end;
    if IconHandle < 2 then
    begin
      if IconHandle = 1 then
        raise EResNotFound.Create(LoadStr(SFileNotExec))
      else
      begin
        Result.Free;
        Result := nil;
      end;
    end
    else
      Result.Handle := IconHandle;
  except
    Result.Free;
    raise;
  end;
end;

const
  ShowCommands: array[TExecState] of Integer =
    (SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED, SW_HIDE);

function FileExecute(const FileName, Params, StartDir: string;
  InitialState: TExecState): THandle;
begin
  Result := ShellExecute(Application.Handle, nil, PChar(FileName),
    PChar(Params), PChar(StartDir), ShowCommands[InitialState]);
end;

function FileExecuteWait(const FileName, Params, StartDir: string;
  InitialState: TExecState): Integer;
var
  Info: TShellExecuteInfo;
  ExitCode: DWORD;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.cbSize := SizeOf(TShellExecuteInfo);
  with Info do
  begin
    fMask := SEE_MASK_NOCLOSEPROCESS;
    Wnd := Application.Handle;
    lpFile := PChar(FileName);
    lpParameters := PChar(Params);
    lpDirectory := PChar(StartDir);
    nShow := ShowCommands[InitialState];
  end;
  if ShellExecuteEx(@Info) then
  begin
    repeat
      Application.ProcessMessages;
      GetExitCodeProcess(Info.hProcess, ExitCode);
    until (ExitCode <> STILL_ACTIVE) or Application.Terminated;
    Result := ExitCode;
  end
  else
    Result := -1;
end;

{$IFNDEF USE_TIMER}

{ TTimerThread }

type
  TTimerThread = class(TThread)
  private
    FOwnerTray: TRxTrayIcon;
  protected
    procedure Execute; override;
  public
    constructor Create(TrayIcon: TRxTrayIcon; CreateSuspended: Boolean);
  end;

constructor TTimerThread.Create(TrayIcon: TRxTrayIcon; CreateSuspended: Boolean);
begin
  FOwnerTray := TrayIcon;
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

procedure TTimerThread.Execute;

  function ThreadClosed: Boolean;
  begin
    Result := Terminated or Application.Terminated or (FOwnerTray = nil);
  end;

begin
  while not Terminated do
    if not ThreadClosed and (SleepEx(FOwnerTray.FInterval, False) = 0) then
      if not ThreadClosed and FOwnerTray.Animated then
        FOwnerTray.Timer;
end;

{$ENDIF USE_TIMER}

{ TRxTrayIcon }

constructor TRxTrayIcon.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  FHandle := {$IFDEF RX_D6}Classes.{$ENDIF}AllocateHWnd(WndProc); // Polaris
  FIcon := TIcon.Create;
  FIcon.OnChange := IconChanged;
  FIconList := TIconList.Create;
  FIconList.OnChange := IconChanged;
  FIconIndex := -1;
  FEnabled := True;
  FInterval := 150;
  FActive := True;
end;

destructor TRxTrayIcon.Destroy;
begin
  Destroying;
  FEnabled := False;
  FIconList.OnChange := nil;
  FIcon.OnChange := nil;
  SetAnimated(False);
  Deactivate;
  {$IFDEF RX_D6}Classes.{$ENDIF}DeallocateHWnd(FHandle);  // Polaris
  FIcon.Free;
  FIcon := nil;
  FIconList.Free;
  FIconList := nil;
  inherited Destroy;
end;

procedure TRxTrayIcon.Loaded;
begin
  inherited Loaded;
  if FActive and not (csDesigning in ComponentState) then
    Activate;
end;

procedure TRxTrayIcon.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = PopupMenu) and (Operation = opRemove) then
    PopupMenu := nil;
end;

procedure TRxTrayIcon.SetPopupMenu(Value: TPopupMenu);
begin
  FPopupMenu := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TRxTrayIcon.SendCancelMode;
var
  F: TForm;
begin
  if not (csDestroying in ComponentState) then
  begin
    F := Screen.ActiveForm;
    if F = nil then
      F := Application.MainForm;
    if F <> nil then
      F.SendCancelMode(nil);
  end;
end;

function TRxTrayIcon.CheckMenuPopup(X, Y: Integer): Boolean;
begin
  Result := False;
  if not (csDesigning in ComponentState) and Active and
    (PopupMenu <> nil) and PopupMenu.AutoPopup then
  begin
    PopupMenu.PopupComponent := Self;
    SendCancelMode;
    SwitchToWindow(FHandle, False);
    Application.ProcessMessages;
    try
      PopupMenu.Popup(X, Y);
    finally
      SwitchToWindow(FHandle, False);
    end;
    Result := True;
  end;
end;

function TRxTrayIcon.CheckDefaultMenuItem: Boolean;
var
  Item: TMenuItem;
  I: Integer;
begin
  Result := False;
  if not (csDesigning in ComponentState) and Active and
    (PopupMenu <> nil) and (PopupMenu.Items <> nil) then
  begin
    I := 0;
    while (I < PopupMenu.Items.Count) do
    begin
      Item := PopupMenu.Items[I];
      if Item.Default and Item.Enabled then
      begin
        Item.Click;
        Result := True;
        Break;
      end;
      Inc(I);
    end;
  end;
end;

procedure TRxTrayIcon.SetIcon(Value: TIcon);
begin
  FIcon.Assign(Value);
end;

procedure TRxTrayIcon.SetIconList(Value: TIconList);
begin
  FIconList.Assign(Value);
end;

function TRxTrayIcon.GetActiveIcon: TIcon;
begin
  Result := FIcon;
  if (FIconList <> nil) and (FIconList.Count > 0) and Animated then
    Result := FIconList[Max(Min(FIconIndex, FIconList.Count - 1), 0)];
end;

function TRxTrayIcon.GetAnimated: Boolean;
begin
  Result := FAnimated;
end;

procedure TRxTrayIcon.SetAnimated(Value: Boolean);
begin
  Value := Value and Assigned(FIconList) and (FIconList.Count > 0);
  if Value <> Animated then
  begin
    if Value then
    begin
{$IFDEF USE_TIMER}
      FTimer := TTimer.Create(Self);
      FTimer.Enabled := FAdded;
      FTimer.Interval := FInterval;
      FTimer.OnTimer := Timer;
{$ELSE}
      FTimer := TTimerThread.Create(Self, not FAdded);
{$ENDIF}
      FAnimated := True;
    end
    else
    begin
      FAnimated := False;
{$IFDEF USE_TIMER}
      FTimer.Free;
      FTimer := nil;
{$ELSE}
      TTimerThread(FTimer).FOwnerTray := nil;
      while FTimer.Suspended do
        FTimer.Resume;
      FTimer.Terminate;
{$ENDIF}
    end;
    FIconIndex := 0;
    ChangeIcon;
  end;
end;

procedure TRxTrayIcon.SetActive(Value: Boolean);
begin
  if (Value <> FActive) then
  begin
    FActive := Value;
    if not (csDesigning in ComponentState) then
      if Value
        then Activate
      else
        Deactivate;
  end;
end;

procedure TRxTrayIcon.Show;
begin
  Active := True;
end;

procedure TRxTrayIcon.Hide;
begin
  Active := False;
end;

procedure TRxTrayIcon.SetShowDesign(Value: Boolean);
begin
  if (csDesigning in ComponentState) then
  begin
    if Value then
      Activate
    else
      Deactivate;
    FShowDesign := FAdded;
  end;
end;

procedure TRxTrayIcon.SetInterval(Value: Word);
begin
  if FInterval <> Value then
  begin
    FInterval := Value;
{$IFDEF USE_TIMER}
    if Animated then
      FTimer.Interval := FInterval;
{$ENDIF}
  end;
end;

{$IFDEF USE_TIMER}
procedure TRxTrayIcon.Timer(Sender: TObject);
{$ELSE}
procedure TRxTrayIcon.Timer;
{$ENDIF}
begin
  if not (csDestroying in ComponentState) and Animated then
  begin
    Inc(FIconIndex);
    if (FIconList = nil) or (FIconIndex >= FIconList.Count) then
      FIconIndex := 0;
    ChangeIcon;
  end;
end;

procedure TRxTrayIcon.IconChanged(Sender: TObject);
begin
  ChangeIcon;
end;

procedure TRxTrayIcon.SetHint(const Value: string);
begin
  if FHint <> Value then
  begin
    FHint := Value;
    ChangeIcon;
  end;
end;

procedure TRxTrayIcon.UpdateNotifyData;
var
  Ico: TIcon;
begin
  with FIconData do
  begin
    cbSize := SizeOf(TNotifyIconData);
    Wnd := FHandle;
    uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
    Ico := GetActiveIcon;
    if Ico <> nil then
      hIcon := Ico.Handle
    else
      hIcon := INVALID_HANDLE_VALUE;
    StrPLCopy(szTip, GetShortHint(FHint), SizeOf(szTip) - 1);
    uCallbackMessage := CM_TRAYICON;
    uID := 0;
  end;
end;

procedure TRxTrayIcon.Activate;
var
  Ico: TIcon;
begin
  Deactivate;
  Ico := GetActiveIcon;
  if (Ico <> nil) and not Ico.Empty then
  begin
    FClicked := [];
    UpdateNotifyData;
    FAdded := Shell_NotifyIcon(NIM_ADD, @FIconData);
    if (GetShortHint(FHint) = '') and FAdded then
      Shell_NotifyIcon(NIM_MODIFY, @FIconData);
{$IFDEF USE_TIMER}
    if Animated then
      FTimer.Enabled := True;
{$ELSE}
    if Animated then
      while FTimer.Suspended do
        FTimer.Resume;
{$ENDIF}
  end;
end;

procedure TRxTrayIcon.Deactivate;
begin
  Shell_NotifyIcon(NIM_DELETE, @FIconData);
  FAdded := False;
  FClicked := [];
{$IFDEF USE_TIMER}
  if Animated then
    FTimer.Enabled := False;
{$ELSE}
  if Animated and not FTimer.Suspended then
    FTimer.Suspend;
{$ENDIF}
end;

procedure TRxTrayIcon.ChangeIcon;
var
  Ico: TIcon;
begin
  if (FIconList = nil) or (FIconList.Count = 0) then SetAnimated(False);
  if FAdded then
  begin
    Ico := GetActiveIcon;
    if (Ico <> nil) and not Ico.Empty then
    begin
      UpdateNotifyData;
      Shell_NotifyIcon(NIM_MODIFY, @FIconData);
    end
    else
      Deactivate;
  end
  else
    if ((csDesigning in ComponentState) and FShowDesign) or
      (not (csDesigning in ComponentState) and FActive) then
      Activate;
end;

procedure TRxTrayIcon.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;

procedure TRxTrayIcon.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TRxTrayIcon.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TRxTrayIcon.DblClick;
begin
  if not CheckDefaultMenuItem and Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

procedure TRxTrayIcon.DoClick(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbRight) and CheckMenuPopup(X, Y) then Exit;
  if Assigned(FOnClick) then
    FOnClick(Self, Button, Shift, X, Y);
end;

procedure TRxTrayIcon.WndProc(var Message: TMessage);

  function GetShiftState: TShiftState;
  begin
    Result := [];
    if GetKeyState(VK_SHIFT) < 0 then Include(Result, ssShift);
    if GetKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
    if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
  end;

var
  P: TPoint;
  Shift: TShiftState;
begin
  try
    with Message do
      if (Msg = CM_TRAYICON) and Self.FEnabled then
      begin
        case lParam of
          WM_LBUTTONDBLCLK:
            begin
              DblClick;
              GetCursorPos(P);
              MouseDown(mbLeft, GetShiftState + [ssDouble], P.X, P.Y);
            end;
          WM_RBUTTONDBLCLK:
            begin
              GetCursorPos(P);
              MouseDown(mbRight, GetShiftState + [ssDouble], P.X, P.Y);
            end;
          WM_MBUTTONDBLCLK:
            begin
              GetCursorPos(P);
              MouseDown(mbMiddle, GetShiftState + [ssDouble], P.X, P.Y);
            end;
          WM_MOUSEMOVE:
            begin
              GetCursorPos(P);
              MouseMove(GetShiftState, P.X, P.Y);
            end;
          WM_LBUTTONDOWN:
            begin
              GetCursorPos(P);
              MouseDown(mbLeft, GetShiftState + [ssLeft], P.X, P.Y);
              Include(FClicked, mbLeft);
            end;
          WM_LBUTTONUP:
            begin
              Shift := GetShiftState + [ssLeft];
              GetCursorPos(P);
              if mbLeft in FClicked then
              begin
                Exclude(FClicked, mbLeft);
                DoClick(mbLeft, Shift, P.X, P.Y);
              end;
              MouseUp(mbLeft, Shift, P.X, P.Y);
            end;
          WM_RBUTTONDOWN:
            begin
              GetCursorPos(P);
              MouseDown(mbRight, GetShiftState + [ssRight], P.X, P.Y);
              Include(FClicked, mbRight);
            end;
          WM_RBUTTONUP:
            begin
              Shift := GetShiftState + [ssRight];
              GetCursorPos(P);
              if mbRight in FClicked then
              begin
                Exclude(FClicked, mbRight);
                DoClick(mbRight, Shift, P.X, P.Y);
              end;
              MouseUp(mbRight, Shift, P.X, P.Y);
            end;
          WM_MBUTTONDOWN:
            begin
              GetCursorPos(P);
              MouseDown(mbMiddle, GetShiftState + [ssMiddle], P.X, P.Y);
            end;
          WM_MBUTTONUP:
            begin
              GetCursorPos(P);
              MouseUp(mbMiddle, GetShiftState + [ssMiddle], P.X, P.Y);
            end;
        end;
      end
      else Result := DefWindowProc(FHandle, Msg, wParam, lParam);
  except
    Application.HandleException(Self);
  end;
end;

end.