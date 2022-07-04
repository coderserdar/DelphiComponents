unit TrayIconUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ShellAPI;

const
  WM_ICONCALLBACK = WM_APP + 400;
  
type
  TTrayIcon = class(TComponent)
  private
    FAutoUpdate: Boolean;
    FAppInTaskbar: Boolean;
    FHideMainOnStart: Boolean;
    FTip: string;
    FIcon: TIcon;
    FPopupMenu: TPopupMenu;
    FVisible: Boolean;
    Data: NotifyIconData;
    FHandle: THandle;
    FOnClick: TNotifyEvent;
    procedure SetAppInTaskbar(const Value: Boolean);
    procedure SetIcon(const Value: TIcon);
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure SetTip(const Value: string);
    procedure SetVisible(const Value: Boolean);
    procedure WndProc(var Msg: TMessage);
    { Private declarations }
  protected
    { Protected declarations }
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    { Published declarations }
    property AutoUpdate: Boolean read FAutoUpdate write FAutoUpdate default True;
    property Icon: TIcon read FIcon write SetIcon;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property Tip: string read FTip write SetTip;
    property Visible: Boolean read FVisible write SetVisible;
    property AppInTaskbar: Boolean read FAppInTaskbar write SetAppInTaskbar default True;
    property HideMainOnStart: Boolean read FHideMainOnStart write FHideMainOnStart default True;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MLR Sysco', [TTrayIcon]);
end;

{ TTrayIcon }

constructor TTrayIcon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAppInTaskbar := True;
  FHandle := AllocateHWnd(WndProc);
  Data.cbSize := 0;
  Data.Wnd := FHandle;
  Data.uID := 0;
  Data.uCallbackMessage := WM_ICONCALLBACK;
  FIcon := TIcon.Create;
end;

destructor TTrayIcon.Destroy;
begin
  Visible := False;
  FIcon.Free;
  DeallocateHWnd(FHandle);
  inherited Destroy;
end;

procedure TTrayIcon.Loaded;
begin
  inherited Loaded;
  if FVisible then begin
    FVisible := False;
    Visible := True;
  end;
  if not (csDesigning in ComponentState) then begin
    if not FAppInTaskbar then begin
      FAppInTaskbar := False;
      AppInTaskbar := True;
    end;
    if FHideMainOnStart then
      Application.ShowMainForm := False;
  end;
end;

procedure TTrayIcon.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation <> opRemove then exit;
  if AComponent = FPopupMenu then FPopupMenu := nil;
end;

procedure TTrayIcon.SetAppInTaskbar(const Value: Boolean);
begin
  FAppInTaskbar := Value;
  if (csDesigning in ComponentState) or
     (csLoading in ComponentState) then
    exit;
  if FAppInTaskbar then
    ShowWindow(Application.Handle, SW_NORMAL)
  else
    ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TTrayIcon.SetIcon(const Value: TIcon);
begin
  FIcon.Assign(Value);
  if FAutoUpdate and FVisible then begin
    Data.uFlags := NIF_TIP;
    StrPLCopy(Data.szTip, FTip, sizeof(Data.szTip));
    Shell_NotifyIcon(NIM_MODIFY, @Data);
  end;
end;

procedure TTrayIcon.SetPopupMenu(const Value: TPopupMenu);
begin
  FPopupMenu := Value;
  if Assigned(Value) then
    Value.FreeNotification(Self);
end;

procedure TTrayIcon.SetTip(const Value: string);
begin
  FTip := Value;
  if FAutoUpdate and FVisible then begin
    Data.uFlags := NIF_TIP;
    StrPLCopy(Data.szTip, FTip, sizeof(Data.szTip));
    Shell_NotifyIcon(NIM_MODIFY, @Data);
  end;
end;

procedure TTrayIcon.SetVisible(const Value: Boolean);
begin
  if FVisible = Value then exit;
  if csLoading in ComponentState then begin
    FVisible := Value;
    exit;
  end;
  if Value then begin
    StrPLCopy(Data.szTip, FTip, sizeof(Data.szTip));
    Data.hIcon := FIcon.Handle;
    Data.uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
    Shell_NotifyIcon(NIM_ADD, @Data);
  end else begin
    Data.uFlags := 0;
    Shell_NotifyIcon(NIM_DELETE, @Data);
  end;
  FVisible := Value;
end;

procedure TTrayIcon.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_ICONCALLBACK then begin
    case Msg.LParam of
      WM_CAPTURECHANGED:;
      WM_LBUTTONDBLCLK:;      WM_LBUTTONDOWN:
        if Assigned(FOnClick) then          FOnClick(Self);      WM_LBUTTONUP:;      WM_MBUTTONDBLCLK:;      WM_MBUTTONDOWN:;
      WM_MBUTTONUP:;
      WM_MOUSEACTIVATE:;
      WM_MOUSEMOVE:;
      WM_MOUSEWHEEL:;
      WM_NCHITTEST:;
      WM_NCLBUTTONDBLCLK:;
      WM_NCLBUTTONDOWN:;
      WM_NCLBUTTONUP:;
      WM_NCMBUTTONDBLCLK:;
      WM_NCMBUTTONDOWN:;
      WM_NCMBUTTONUP:;
      WM_NCMOUSEMOVE:;
      WM_NCRBUTTONDBLCLK:;
      WM_NCRBUTTONDOWN:;
      WM_NCRBUTTONUP:;
      WM_RBUTTONDBLCLK:;
      WM_RBUTTONDOWN:
        if Assigned(FPopupMenu) then begin
          FPopupMenu.PopupComponent := Self;          FPopupMenu.TrackButton := tbRightButton;          SetForegroundWindow(FHandle);          FPopupMenu.Popup(Mouse.CursorPos.x, Mouse.CursorPos.y);          PostMessage(FHandle, WM_NULL, 0, 0);        end;      WM_RBUTTONUP:;
    end;
  end else
    Msg.Result := DefWindowProc(FHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;

end.
