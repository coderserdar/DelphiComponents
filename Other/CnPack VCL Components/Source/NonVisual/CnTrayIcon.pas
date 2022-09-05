{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2020 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnTrayIcon;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������
* ��Ԫ���ƣ�TCnTrayIcon ��Ԫ
* ��Ԫ���ߣ���Х liuxiao@cnpack.org; http://www.cnpack.org
* ��    ע���� Explorer �Ƿ��������������Զ��ָ�ͼ���ϵͳ���������Ԫ
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2012.06.21 V1.3
*               �ָ���ʾʱ����һ��BringToFront
*           2005.02.05 V1.2
*               ������ʾ������ʾ�󵯳��˵�����ʾʧЧ������
*           2004.03.07 V1.1
*               ������ʾ������ʾ����С��ʱ�Զ����������ڵĹ���
*           2004.03.05 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Messages, Windows, Forms, Menus, Controls,
  Graphics, ShellAPI, CnClasses, CnConsts, CnCompConsts;

type
  EBalloonHintError = class(Exception);

  TMouseButtons = set of TMouseButton;

  TBalloonType = (btNone, btError, btInfo, btWarning);

  TNotifyIconDataXP = record
    cbSize: DWORD;
    Wnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array [0..127] of AnsiChar;
    dwState: DWORD;
    dwStateMask: DWORD;
    szInfo: array [0..255] of AnsiChar;
    uTimeOut: DWORD;
    szInfoTitle: array [0..63] of AnsiChar;
    dwInfoFlags: DWORD;
  end;

  TCnTrayIcon = class(TCnComponent)
  private
    FHandle: HWND;
    FAcceptBalloons: Boolean;
    FActive: Boolean;
    FAdded: Boolean;
    FEnabled: Boolean;
    FClicked: TMouseButtons;
    FIconData: TNotifyIconData;
    FIconXP: TNotifyIconDataXP;
    FIcon: TIcon;
    FHint: string;
    FShowDesign: Boolean;
    FPopupMenu: TPopupMenu;
    FOnClick: TMouseEvent;
    FOnDblClick: TNotifyEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    FOnBalloonShow: TNotifyEvent;
    FUseAppIcon: Boolean;
    FHooked: Boolean;
    FAutoHide: Boolean;
    FSaveWindowState: TWindowState;
    procedure ChangeIcon;
    procedure SendCancelMode;
    function CheckMenuPopup(X: Integer; Y: Integer): Boolean;
    function CheckDefaultMenuItem: Boolean;
    procedure SetHint(const Value: string);
    procedure SetIcon(Value: TIcon);
    procedure SetPopupMenu(Value: TPopupMenu);
    procedure Activate;
    procedure Deactivate;
    procedure HookApp;
    procedure UnHookApp;
    procedure SetActive(Value: Boolean);
    procedure SetShowDesign(Value: Boolean);
    procedure IconChanged(Sender: TObject);
    procedure WndProc(var Message: TMessage);
    procedure SetUseAppIcon(const Value: Boolean);
    function ApplicationHook(var Msg: TMessage): Boolean;
  protected
    procedure DblClick; dynamic;
    procedure DoClick(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); dynamic;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); dynamic;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); dynamic;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); dynamic;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateNotifyData; virtual;

    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure HideIcon;
    procedure ShowIcon;
    procedure HideApplication;
    procedure ShowApplication;
    procedure BalloonHint(const Title, Value: string; BalloonType: TBalloonType = btNone;
      DelaySeconds: Integer = 5);
    property Handle: HWND read FHandle;
  published
    property Active: Boolean read FActive write SetActive default True;
    property AutoHide: Boolean read FAutoHide write FAutoHide;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Hint: string read FHint write SetHint;
    property Icon: TIcon read FIcon write SetIcon;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property ShowDesign: Boolean read FShowDesign write SetShowDesign stored False;
    property UseAppIcon: Boolean read FUseAppIcon write SetUseAppIcon;
    property OnClick: TMouseEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnBalloonShow: TNotifyEvent read FOnBalloonShow write FOnBalloonShow;
  end;

implementation

uses
  CnCommon;

const
  SCnCreateTaskBar: string = 'TaskbarCreated';
  SCnTrayIcon: string = 'CnTrayIcon';

  NIF_STATE = $00000008;
  NIF_INFO = $00000010;

  NIIF_NONE = $00000000;
  NIIF_INFO = $00000001;
  NIIF_WARNING = $00000002;
  NIIF_ERROR = $00000003;

  NIN_BALLOONSHOW = WM_USER + 2;
  NIN_BALLOONHIDE = WM_USER + 3;
  NIN_BALLOONTIMEOUT = WM_USER + 4;
  NIN_BALLOONUSERCLICK = WM_USER + 5;

var
  WM_CNCREATETASKBAR: Cardinal;
  WM_CNTRAYICONCALLBACK: Cardinal;

procedure SwitchToWindow(Wnd: HWnd; Restore: Boolean);
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

{ TCnTrayIcon }

procedure TCnTrayIcon.Activate;
var
  S: string;
begin
  Deactivate;
  if FIcon.Handle <> 0 then
  begin
    FClicked := [];
    UpdateNotifyData;
    FAdded := Shell_NotifyIconA(NIM_ADD, @FIconData);
    S := GetShortHint(FHint);

    if FAdded and (S <> '') then
      Shell_NotifyIconA(NIM_MODIFY, @FIconData);
  end;
end;

procedure TCnTrayIcon.BalloonHint(const Title, Value: string;
  BalloonType: TBalloonType; DelaySeconds: Integer);
begin
  if FAcceptBalloons then
  begin
    FIconXP.cbSize := SizeOf(FIconXP);
    FIconXP.Wnd := FHandle;
    FIconXP.hIcon := FIcon.Handle;
    StrPLCopy(FIconXP.szInfoTitle, {$IFDEF UNICODE}AnsiString{$ENDIF}(Title), SizeOf(FIconXP.szInfoTitle) - 1);
    StrPLCopy(FIconXP.szInfo, {$IFDEF UNICODE}AnsiString{$ENDIF}(Value), SizeOf(FIconXP.szInfo) - 1);
    FIconXP.uFlags := NIF_ICON or NIF_INFO; // �˴����������־�ᵼ��������־ʧЧ
    FIconXP.uTimeOut := DelaySeconds;
    case BalloonType of
      btError:
        FIconXP.dwInfoFlags := NIIF_ERROR;
      btInfo:
        FIconXP.dwInfoFlags := NIIF_INFO;
      btNone:
        FIconXP.dwInfoFlags := NIIF_NONE;
      btWarning:
        FIconXP.dwInfoFlags := NIIF_WARNING;
    end;
    Shell_NotifyIconA(NIM_MODIFY, @FIconXP);

    if Assigned(FOnBalloonShow) then
      FOnBalloonShow(Self);
  end
  else
    raise EBalloonHintError.Create('Balloon Hint not Supported.');
end;

procedure TCnTrayIcon.ChangeIcon;
begin
  if FAdded then
  begin
    if FIcon.Handle <> 0 then
    begin
      UpdateNotifyData;
      Shell_NotifyIconA(NIM_MODIFY, @FIconData);
    end
    else
      Deactivate;
    Exit;
  end;

  if (csDesigning in ComponentState) and FShowDesign or
    (not (csDesigning in ComponentState) and FActive) then
    Activate;
end;

function TCnTrayIcon.CheckDefaultMenuItem: Boolean;
var
  Item: TMenuItem;
  I: Integer;
begin
  Result := False;
  if not (csDesigning in ComponentState) then
    if FActive and (FPopupMenu <> nil) then
      if FPopupMenu.Items <> nil then
      begin
        for I := 0 to FPopupMenu.Items.Count - 1 do
        begin
          Item := FPopupMenu.Items[I];
          if Item.Default and Item.Enabled then
          begin
            Item.Click;
            Result := True;
            Break;
          end;
        end;
      end;
end;

function TCnTrayIcon.CheckMenuPopup(X, Y: Integer): Boolean;
begin
  Result := False;
  if not (csDesigning in ComponentState) then
  begin
    if FActive and (FPopupMenu <> nil) then
    begin
      if FPopupMenu.AutoPopup then
      begin
        FPopupMenu.PopupComponent := Self;
        SendCancelMode;
        SwitchToWindow(FHandle, False);
        Application.ProcessMessages;

        try
          FPopupMenu.Popup(X, Y);
        finally
          SwitchToWindow(FHandle, False);
        end;
        Result := True;
      end;
    end;
  end;
end;

constructor TCnTrayIcon.Create(AOwner: TComponent);
var
  H: THandle;
  F: array[0..255] of Char;
begin
  inherited;
  FHandle := AllocateHwnd(WndProc);
  FIcon := TIcon.Create;
  FIcon.OnChange := IconChanged;
  FActive := True;
  FEnabled := True;

  HookApp;
  H := LoadLibrary('Shell32.DLL');
  if (H <> 0) and (0 <> GetModuleFileName(H, F, SizeOf(F))) then
    FAcceptBalloons := GetFileVersionNumber(F).Major >= 5;
end;

procedure TCnTrayIcon.DblClick;
begin
  if not CheckDefaultMenuItem and Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

procedure TCnTrayIcon.Deactivate;
begin
  Shell_NotifyIconA(NIM_DELETE, @FIconData);
  FAdded := False;
  FClicked := [];
end;

destructor TCnTrayIcon.Destroy;
begin
  Destroying;
  UnHookApp;
  FEnabled := False;
  FIcon.OnChange := nil;
  Deactivate;
  DeallocateHWnd(FHandle);
  FreeAndNil(FIcon);
  inherited;
end;

procedure TCnTrayIcon.DoClick(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if (Button <> mbRight) or not CheckMenuPopup(X, Y) then
    if Assigned(FOnClick) then
      FOnClick(Self, Button, Shift, X, Y);
end;

procedure TCnTrayIcon.HideIcon;
begin
  Active := False;
end;

procedure TCnTrayIcon.HideApplication;
begin
  if (Application.MainForm <> nil) and (Application.MainForm.WindowState <> wsMinimized) then
  begin
    FSaveWindowState := Application.MainForm.WindowState;
    Application.Minimize;
    Application.MainForm.Hide;
  end;
  ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TCnTrayIcon.IconChanged(Sender: TObject);
begin
  ChangeIcon;
end;

procedure TCnTrayIcon.Loaded;
begin
  inherited;
  if FActive and not (csDesigning in ComponentState) then
    Activate;
end;

procedure TCnTrayIcon.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TCnTrayIcon.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;

procedure TCnTrayIcon.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TCnTrayIcon.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FPopupMenu) and (Operation = opRemove) then
    PopupMenu := nil;
end;

procedure TCnTrayIcon.SendCancelMode;
begin
  if not (csDestroying in ComponentState) then
    if Screen.ActiveCustomForm <> nil then
      if Application.MainForm <> nil then
        Application.MainForm.SendCancelMode(nil);
end;

procedure TCnTrayIcon.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    if not (csDesigning in ComponentState) then
    begin
      if Value then
        Activate
      else
        Deactivate;
    end;
  end;
end;

procedure TCnTrayIcon.SetHint(const Value: string);
begin
  if Value <> FHint then
  begin
    FHint := Value;
    ChangeIcon;
  end;
end;

procedure TCnTrayIcon.SetIcon(Value: TIcon);
begin
  FIcon.Assign(Value);
end;

procedure TCnTrayIcon.SetPopupMenu(Value: TPopupMenu);
begin
  FPopupMenu := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TCnTrayIcon.SetShowDesign(Value: Boolean);
begin
  if csDesigning in ComponentState then
  begin
    if Value then
      Activate
    else
      Deactivate;
    FShowDesign := FAdded;
  end;
end;

procedure TCnTrayIcon.SetUseAppIcon(const Value: Boolean);
begin
  FUseAppIcon := Value;
  if Value and (FIcon <> nil) then
    FIcon.Assign(Application.Icon);
end;

procedure TCnTrayIcon.ShowIcon;
begin
  Active := True;
end;

procedure TCnTrayIcon.ShowApplication;
begin
  ShowWindow(Application.Handle, SW_SHOW);
  Application.Restore;
  if Application.MainForm <> nil then
  begin
    if FSaveWindowState <> wsMinimized then
      Application.MainForm.WindowState := FSaveWindowState
    else
      Application.MainForm.WindowState := wsNormal;
    Application.BringToFront;
    Application.MainForm.Show;
  end;
end;

procedure TCnTrayIcon.UpdateNotifyData;
var
  ShortHint: AnsiString;
begin
  FIconData.cbSize := SizeOf(TNotifyIconData);
  FIconData.Wnd := FHandle;
  FIconData.uFlags := NIF_ICON or NIF_MESSAGE or NIF_TIP or NIF_INFO;
  FIconData.hIcon := FIcon.Handle;
  ShortHint := {$IFDEF UNICODE}AnsiString{$ENDIF}(GetShortHint(FHint));
{$IFDEF UNICODE}
  if ShortHint <> '' then
    CopyMemory(@FIconData.szTip, Pointer(ShortHint), 63)
  else
    FIconData.szTip[0] := #0;
{$ELSE}
  StrPLCopy(FIconData.szTip, ShortHint, 63);
{$ENDIF}
  FIconData.uCallbackMessage := WM_CNTRAYICONCALLBACK;
  FIconData.uID := 0;
end;

procedure TCnTrayIcon.WndProc(var Message: TMessage);

  function GetShiftState: TShiftState;
  begin
    Result := [];
    if GetKeyState(VK_SHIFT) < 0 then
      Include(Result, ssShift);
    if GetKeyState(VK_CONTROL) < 0 then
      Include(Result, ssCtrl);
    if GetKeyState(VK_MENU) < 0 then
      Include(Result, ssAlt);
  end;

var
  P: TPoint;
  Shift: TShiftState;
begin
  try
    if Message.Msg = WM_CNTRAYICONCALLBACK then
    begin
      if FEnabled then
      begin
        Shift := GetShiftState;
        case Message.lParam of
          WM_LBUTTONDBLCLK:
            begin
              DblClick;
              GetCursorPos(P);
              MouseDown(mbLeft, Shift, P.x, P.y);
            end;
          WM_RBUTTONDBLCLK:
            begin
              DblClick;
              GetCursorPos(P);
              MouseDown(mbRight, Shift, P.x, P.y);
            end;
          WM_MBUTTONDBLCLK:
            begin
              DblClick;
              GetCursorPos(P);
              MouseDown(mbMiddle, Shift, P.x, P.y);
            end;
          WM_MOUSEMOVE:
            begin
              GetCursorPos(P);
              MouseMove(Shift, P.X, P.Y);
            end;
          WM_LBUTTONDOWN:
            begin
              GetCursorPos(P);
              MouseDown(mbLeft, Shift, P.X, P.Y);
              Include(FClicked, mbLeft);
            end;
          WM_LBUTTONUP:
            begin
              GetCursorPos(P);
              if mbLeft in FClicked then
              begin
                Exclude(FClicked, mbLeft);
                DoClick(mbLeft, Shift, P.x, P.y);
              end;
              MouseUp(mbLeft, Shift, P.x, P.y);
            end;
          WM_RBUTTONDOWN:
            begin
              GetCursorPos(P);
              MouseDown(mbRight, Shift, P.x, P.y);
              Include(FClicked, mbRight);
            end;
          WM_RBUTTONUP:
            begin
              GetCursorPos(P);
              if mbRight in FClicked then
              begin
                Exclude(FClicked, mbRight);
                DoClick(mbRight, Shift, P.x, P.y);
              end;
              MouseUp(mbRight, Shift, P.X, P.Y);
            end;
          WM_MBUTTONDOWN:
            begin
              GetCursorPos(P);
              MouseDown(mbMiddle, Shift, P.X, P.Y);
              Include(FClicked, mbMiddle);
            end;
          WM_MBUTTONUP:
            begin
              GetCursorPos(P);
              if mbMiddle in FClicked then
              begin
                Exclude(FClicked, mbMiddle);
                DoClick(mbMiddle, Shift, P.x, P.y);
              end;
              MouseUp(mbMiddle, Shift, P.X, P.Y);
            end;
          NIN_BALLOONSHOW:
            begin

            end;
          NIN_BALLOONHIDE:
            begin

            end;
          NIN_BALLOONTIMEOUT:
            begin

            end;
          NIN_BALLOONUSERCLICK:
            begin

            end;
        end;  // end of case
      end;
    end
    else if Message.Msg = WM_CNCREATETASKBAR then
    begin
      if not (csDesigning in ComponentState) and FActive then
        Activate;
    end
    else with Message do
      Result := DefWindowProc(FHandle, Msg, wParam, lParam);

  except
    Application.HandleException(Self);
  end;
end;

procedure TCnTrayIcon.HookApp;
begin
  if FHooked then
    Exit;
  Application.HookMainWindow(ApplicationHook);
  FHooked := True;
end;

procedure TCnTrayIcon.UnHookApp;
begin
  if not FHooked then
    Exit;
  Application.UnhookMainWindow(ApplicationHook);
  FHooked := False;
end;

function TCnTrayIcon.ApplicationHook(var Msg: TMessage): Boolean;
begin
  if (Msg.Msg = WM_SYSCOMMAND) and (Msg.WParam = SC_MINIMIZE) and
    FAutoHide and FActive then
    if not (csDesigning in ComponentState) then
      HideApplication;
  Result := False;
end;

procedure TCnTrayIcon.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnTrayIconName;
  Author := SCnPack_LiuXiao;
  Email := SCnPack_LiuXiaoEmail;
  Comment := SCnTrayIconComment;
end;

initialization
  WM_CNCREATETASKBAR := RegisterWindowMessage(PChar(SCnCreateTaskBar));
  WM_CNTRAYICONCALLBACK := RegisterWindowMessage(PChar(SCnTrayIcon));

end.
