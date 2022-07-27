{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmTrayIcon
Purpose  : To allow the program to have an Icon in the Win95 Shell Icontray.
Date     : 12-01-1998
Author   : Ryan J. Mills
Version  : 1.90
Notes    : I don't remember where I originally got the code for this component.
           I have modified very little from the original source.
================================================================================}

unit rmTrayIcon;

interface

{$I CompilerDefines.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ShellAPI, Menus;

const
  wm_IconMessage = wm_user + 1;
  wm_ResetIconToolTip = WM_USER+2;

type
  TrmTrayIcon = class(TComponent)
  private
    { Private declarations }
    fIconData: TNOTIFYICONDATA;

    fIcon: TIcon;
    fToolTip: string;
    fWindowHandle: HWND;

    fOnClick: TNotifyEvent;
    fOnDblClick: TNotifyEvent;
    fOnRightClick: TMouseEvent;
    fPopupMenu: TPopupMenu;
    fShowDesigning: boolean;
    fActive: boolean;
    fShowApp: boolean;

    procedure FillDataStructure;

    function AddIcon : boolean;
    function ModifyIcon : boolean;
    function DeleteIcon : boolean;

    procedure IconTrayWndProc(var msg: TMessage);

    procedure DoRightClick(Sender: TObject);
    procedure SetActive(const Value: boolean);
    procedure SetIcon(const Value: TIcon);
    procedure SetShowApp(const Value: boolean);
    procedure SetShowDesigning(const Value: boolean);
    procedure SetToolTip(const Value: string);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor create(aOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Active: boolean read fActive write SetActive;
    property Icon: TIcon read fIcon write SetIcon;
    property PopupMenu: TPopupMenu read fPopupMenu write fPopupMenu;
    property ShowDesigning: boolean read fShowDesigning write SetShowDesigning;
    property ShowApp: boolean read fShowApp write SetShowApp;
    property ToolTip: string read fTooltip write SetToolTip;

    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnRightClick: TMouseEvent read FOnRightClick write FonRightClick;
  end;

implementation

{ TrmTrayIcon }

function TrmTrayIcon.AddIcon: boolean;
begin
   FillDataStructure;
   result := Shell_NotifyIcon(NIM_ADD,@fIconData);

   if fToolTip = '' then
      PostMessage(fWindowHandle, wm_ResetIconToolTip, 0, 0);
end;

constructor TrmTrayIcon.create(aOwner: TComponent);
begin
  inherited;
  fIcon := TIcon.create;
  {$ifdef D6_or_higher}
  FWindowHandle := Classes.AllocateHWnd(IconTrayWndProc);
  {$else}
  FWindowHandle := AllocateHWnd(IconTrayWndProc);
  {$endif}

  SetShowApp(False);
end;

function TrmTrayIcon.DeleteIcon: boolean;
begin
   result := Shell_NotifyIcon(NIM_DELETE,@fIconData);
end;

destructor TrmTrayIcon.Destroy;
begin
  DeleteIcon;
  {$ifdef D6_or_higher}
  Classes.DeAllocateHWnd(FWindowHandle);
  {$else}
  DeAllocateHWnd(FWindowHandle);
  {$endif}
  fIcon.free;
  inherited;
end;

procedure TrmTrayIcon.DoRightClick(Sender: TObject);
var
  MouseCo: Tpoint;
begin
  GetCursorPos(MouseCo);

  if assigned(fPopupMenu) then
  begin
    SetForegroundWindow(Application.Handle);
    Application.ProcessMessages;
    fPopupmenu.Popup(Mouseco.X, Mouseco.Y);
  end;

  if assigned(FOnRightClick) then
    FOnRightClick(self, mbRight, [], MouseCo.x, MouseCo.y);
end;

procedure TrmTrayIcon.FillDataStructure;
begin
  with fIconData do
  begin
    cbSize := sizeof(TNOTIFYICONDATA);
    wnd := FWindowHandle;
    uID := 1;
    uFlags := NIF_MESSAGE + NIF_ICON + NIF_TIP;
    hIcon := fIcon.Handle;
    StrPCopy(szTip, fToolTip);
    uCallbackMessage := wm_IconMessage;
  end;
end;

procedure TrmTrayIcon.IconTrayWndProc(var msg: TMessage);
begin
   case msg.msg of
      wm_ResetIconToolTip:
         begin
             SetToolTip( fToolTip );
         end;
      wm_IconMessage:
         begin
           case msg.lParam of
             WM_LBUTTONDBLCLK:
               if assigned(FOnDblClick) then FOnDblClick(self);
             WM_LBUTTONUP:
               if assigned(FOnClick) then FOnClick(self);
             WM_RBUTTONUP: DoRightClick(self);
           end;
         end;
   else
     msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.wParam, Msg.lParam);
   end;
end;

function TrmTrayIcon.ModifyIcon: boolean;
begin
   FillDataStructure;
   if fActive then
      result := Shell_NotifyIcon(NIM_MODIFY,@fIconData)
   else
      result := True;
end;

procedure TrmTrayIcon.SetActive(const Value: boolean);
begin
  if value <> fActive then
  begin
    fActive := Value;
    if not (csdesigning in ComponentState) then
    begin
      if Value then
        AddIcon
      else
        DeleteIcon;
    end;
  end;
end;

procedure TrmTrayIcon.SetIcon(const Value: TIcon);
begin
  if Value <> fIcon then
  begin
    fIcon.Assign(value);
    ModifyIcon;
  end;
end;

procedure TrmTrayIcon.SetShowApp(const Value: boolean);
begin
  if value <> fShowApp then
    fShowApp := value;
  if not (csdesigning in ComponentState) then
  begin
    if Value then
      ShowWindow(Application.Handle, SW_SHOW)
    else
      ShowWindow(Application.Handle, SW_HIDE);
  end;
end;

procedure TrmTrayIcon.SetShowDesigning(const Value: boolean);
begin
  if csdesigning in ComponentState then
  begin
    if value <> fShowDesigning then
    begin
      fShowDesigning := Value;
      if Value then
        AddIcon
      else
        DeleteIcon;
    end;
  end;
end;

procedure TrmTrayIcon.SetToolTip(const Value: string);
begin
  if length(Value) > 62 then
    fToolTip := copy(Value, 1, 62)
  else
    fToolTip := value;
  ModifyIcon;
end;

initialization

end.

