{ Copyright (C) 1998-2003, written by Shkolnik Mike, Scalabium
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com
       http://www.geocities.com/mshkolnik
  tel: 380-/44/-552-10-29

  The TSMTrayIcon component
}
unit SMTray;

{$P+,W-,R-}

interface

uses Windows, Messages, Classes, Graphics, SysUtils, Forms, Controls, Menus,
     ShellAPI{, IcoList};

type
  TMouseButtons = set of TMouseButton;

{ TSMTrayIcon }

  TSMTrayIcon = class(TComponent)
  private
    FHandle: HWnd;
    FActive: Boolean;
    FAdded: Boolean;
    FAnimated: Boolean;
    FClicked: TMouseButtons;
    FIconIndex: Integer;
    FInterval: Word;
    FIconData: TNotifyIconData;
    FCurIcon: TIcon;
    FIcon: TIcon;
    FIconList: TImageList;
    FTimer: TThread;
    FDestroying: Boolean;
    FHint: string;
    FShowDesign: Boolean;
    FPopupMenu: TPopupMenu;
    FOnClick: TMouseEvent;
    FOnDblClick: TNotifyEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;

    procedure ChangeIcon;
    procedure Timer;
    procedure SendCancelMode;
    procedure SwitchToWindow(Wnd: HWnd; Restore: Boolean);
    function CheckMenuPopup(X, Y: Integer): Boolean;
    function CheckDefaultMenuItem: Boolean;
    procedure SetHint(const Value: string);
    procedure SetIcon(Value: TIcon);
    procedure SetIconList(Value: TImageList);
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
    property Handle: HWnd read FHandle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Hide;
    procedure Show;
  published
    property Active: Boolean read FActive write SetActive default True;
    property Hint: string read FHint write SetHint;
    property Icon: TIcon read FIcon write SetIcon;
    property Icons: TImageList read FIconList write SetIconList;
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

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMTrayIcon]);
end;

const
  CM_TRAYICON = CM_BASE + 100;

{ TTimerThread }
type
  TTimerThread = class(TThread)
  private
    FOwnerTray: TSMTrayIcon;
  protected
    procedure Execute; override;
  public
    constructor Create(TrayIcon: TSMTrayIcon; CreateSuspended: Boolean);
  end;

constructor TTimerThread.Create(TrayIcon: TSMTrayIcon; CreateSuspended: Boolean);
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
  begin
    if not ThreadClosed then
      if SleepEx(FOwnerTray.FInterval, False) = 0 then
        if not ThreadClosed and FOwnerTray.Animated then
          FOwnerTray.Timer;
  end;
end;


{ TSMTrayIcon }
constructor TSMTrayIcon.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);

  FHandle := AllocateHWnd(WndProc);
  FCurIcon := TIcon.Create;
  FIcon := TIcon.Create;
  FIcon.OnChange := IconChanged;
  FIconList := TImageList.Create(nil);
  FIconList.OnChange := IconChanged;
  FIconIndex := -1;
  FInterval := 150;
  FActive := True;
end;

destructor TSMTrayIcon.Destroy;
begin
  FDestroying := True;
  FIconList.OnChange := nil;
  FIcon.OnChange := nil;
  SetAnimated(False);
  Deactivate;
  DeallocateHWnd(FHandle);
  FIcon.Free;
  FIcon := nil;
  FIconList.Free;
  FIconList := nil;
  FCurIcon.Free;
  FCurIcon := nil;

  inherited Destroy;
end;

procedure TSMTrayIcon.Loaded;
begin
  inherited Loaded;

  if FActive and not (csDesigning in ComponentState) then
    Activate;
end;

procedure TSMTrayIcon.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (AComponent = PopupMenu) and (Operation = opRemove) then
    PopupMenu := nil;
end;

procedure TSMTrayIcon.SetPopupMenu(Value: TPopupMenu);
begin
  FPopupMenu := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TSMTrayIcon.SendCancelMode;
var
  F: TForm;
begin
  if not ((csDestroying in ComponentState) or FDestroying) then
  begin
    F := Screen.ActiveForm;
    if F = nil then
      F := Application.MainForm;
    if F <> nil then
      F.SendCancelMode(nil);
  end;
end;

procedure TSMTrayIcon.SwitchToWindow(Wnd: HWnd; Restore: Boolean);
begin
  if IsWindowEnabled(Wnd) then
  begin
    SetForegroundWindow(Wnd);
    if Restore and IsWindowVisible(Wnd) then
    begin
      if not IsZoomed(Wnd) then
        SendMessage(Wnd, WM_SYSCOMMAND, SC_RESTORE, 0);
      SetFocus(Wnd);
    end;
  end;
end;

function TSMTrayIcon.CheckMenuPopup(X, Y: Integer): Boolean;
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

function TSMTrayIcon.CheckDefaultMenuItem: Boolean;
var
  Item: TMenuItem;
  i: Integer;
begin
  Result := False;
  if not (csDesigning in ComponentState) and Active and
    (PopupMenu <> nil) and (PopupMenu.Items <> nil) then
  begin
    i := 0;
    while (i < PopupMenu.Items.Count) do
    begin
      Item := PopupMenu.Items[i];
      if Item.Default and Item.Enabled then
      begin
        Item.Click;
        Result := True;
        Break;
      end;
      Inc(i);
    end;
  end;
end;

procedure TSMTrayIcon.SetIcon(Value: TIcon);
begin
  FIcon.Assign(Value);
end;

procedure TSMTrayIcon.SetIconList(Value: TImageList);
begin
  FIconList.Assign(Value);
end;

function TSMTrayIcon.GetActiveIcon: TIcon;
var i: Integer;
begin
  Result := FIcon;
  if (FIconList <> nil) and (FIconList.Count > 0) and Animated then
  begin
    if (FIconIndex < FIconList.Count) then
      i := FIconIndex
    else
      i := FIconList.Count-1;
    if (i < 0) then
      i := 0;
    FIconList.GetIcon(i, FCurIcon);
    Result := FCurIcon;
  end;
end;

function TSMTrayIcon.GetAnimated: Boolean;
begin
  Result := FAnimated;
end;

procedure TSMTrayIcon.SetAnimated(Value: Boolean);
begin
  Value := Value and Assigned(FIconList) and (FIconList.Count > 0);
  if Value <> Animated then
  begin
    if Value then
    begin
      FTimer := TTimerThread.Create(Self, not FAdded);
      FAnimated := True;
    end
    else
    begin
      FAnimated := False;
      TTimerThread(FTimer).FOwnerTray := nil;
      while FTimer.Suspended do
        FTimer.Resume;
      FTimer.Terminate;
    end;
    FIconIndex := 0;
    ChangeIcon;
  end;
end;

procedure TSMTrayIcon.SetActive(Value: Boolean);
begin
  if (Value <> FActive) then
  begin
    FActive := Value;
    if not (csDesigning in ComponentState) then
      if Value then
        Activate
      else
        Deactivate;
  end;
end;

procedure TSMTrayIcon.Show;
begin
  Active := True;
end;

procedure TSMTrayIcon.Hide;
begin
  Active := False;
end;

procedure TSMTrayIcon.SetShowDesign(Value: Boolean);
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

procedure TSMTrayIcon.SetInterval(Value: Word);
begin
  if FInterval <> Value then
    FInterval := Value;
end;

procedure TSMTrayIcon.Timer;
begin
  if not (csDestroying in ComponentState) and Animated then
  begin
    Inc(FIconIndex);
    if (FIconList = nil) or (FIconIndex >= FIconList.Count) then
      FIconIndex := 0;
    ChangeIcon;
  end;
end;

procedure TSMTrayIcon.IconChanged(Sender: TObject);
begin
  ChangeIcon;
end;

procedure TSMTrayIcon.SetHint(const Value: string);
begin
  if FHint <> Value then
  begin
    FHint := Value;
    ChangeIcon;
  end;
end;

procedure TSMTrayIcon.UpdateNotifyData;
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

procedure TSMTrayIcon.Activate;
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
    if Animated then
      while FTimer.Suspended do
        FTimer.Resume;
  end;
end;

procedure TSMTrayIcon.Deactivate;
begin
  Shell_NotifyIcon(NIM_DELETE, @FIconData);
  FAdded := False;
  FClicked := [];
  if Animated and not FTimer.Suspended then
    FTimer.Suspend;
end;

procedure TSMTrayIcon.ChangeIcon;
var
  Ico: TIcon;
begin
  if (FIconList = nil) or (FIconList.Count = 0) then
    SetAnimated(False);
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
  begin
    if ((csDesigning in ComponentState) and FShowDesign) or
       (not (csDesigning in ComponentState) and FActive) then
      Activate;
  end;
end;

procedure TSMTrayIcon.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;

procedure TSMTrayIcon.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TSMTrayIcon.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TSMTrayIcon.DblClick;
begin
  if not CheckDefaultMenuItem and Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

procedure TSMTrayIcon.DoClick(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbRight) and CheckMenuPopup(X, Y) then Exit;

  if Assigned(FOnClick) then
    FOnClick(Self, Button, Shift, X, Y);
end;

procedure TSMTrayIcon.WndProc(var Message: TMessage);

  function GetShiftState: TShiftState;
  begin
    Result := [];
    if GetKeyState(VK_SHIFT) < 0 then
      Include(Result, ssShift);
    if GetKeyState(VK_CONTROL) < 0 then
      Include(Result, ssCtrl);
    if GetKeyState(Vk_MENU) < 0 then
      Include(Result, ssAlt);
  end;

var
  P: TPoint;
  Shift: TShiftState;
begin
  try
    with Message do
      if Msg = CM_TRAYICON then
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
      else
        Result := DefWindowProc(FHandle, Msg, wParam, lParam);
  except
    Application.HandleException(Self);
  end;
end;

end.