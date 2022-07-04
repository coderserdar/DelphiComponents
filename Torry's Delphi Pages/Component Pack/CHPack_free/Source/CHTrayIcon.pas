unit CHTrayIcon;

{ ##############################################################################
  TCHTrayIcon

  Version   		:   1.0.4
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 25.10.2002    - First Release
  1.0.1 - 30.11.2002    - BUG: "Destroy" not get free some resources
                        - BUG: Messagehandler don´t work correct
  1.0.2 - 13.01.2003    - BUG: fixes BlinkIcon-Mode
  1.0.3 - 09.03.2003    - reorganize "uses" for more performance and less memory needed
  1.0.4 - 31.12.2004    - BUG: Popupmenu not close correct

  ############################################################################ }

interface

uses
  Windows, Forms, Messages, SysUtils, Classes, Controls, ExtCtrls,
  Graphics, ShellApi, Menus;


const
  WM_TASKICONEVENT = WM_USER + 1;

type
  TTrayAction = (paLeftDown, paRightDown, paLeftUp, paRightUp,
    paLeftDblClick, paRightDblClick, paNone);
  TTrayShow = (tyAllways, tyNever, tyOnRestore, tyOnMinimized);
  TTrayMode = (trmIcon, trmText);
  TTrayTextAlign = (ttgCenter, ttgCustom, ttgCenterBottom, ttgCenterTop,
    ttgCenterLeft, ttgCenterRight);

  TCHTrayIcon = class;


  TTrayTimer = class(TPersistent)
  private
    FOwner : TCHTrayIcon;
    FTimer : TTimer;
  public
    constructor Create(AOwner: TCHTrayIcon); virtual;
    destructor Destroy; override;
  end;


  TCHTrayAction = class(TPersistent)
  private
    FOwner : TCHTrayIcon;
    FPopup: TTrayAction;
    FRestore: TTrayAction;
  public
    constructor Create(AOwner: TCHTrayIcon); virtual;
  published
    property Popup : TTrayAction read FPopup write FPopup;
    property Restore : TTrayAction read FRestore write FRestore;
  end;


  TCHIconBlink = class(TPersistent)
  private
    FOwner : TCHTrayIcon;
    FEnabled: Boolean;
    FBlinkInterval: Cardinal;
    FBlinkTimer : TTimer;

    procedure SetEnabled(const Value: Boolean);
    procedure SetBlinkInterval(const Value: Cardinal);
  public
    constructor Create(AOwner: TCHTrayIcon); virtual;
    destructor Destroy; override;
  published
    property Enabled : Boolean read FEnabled write SetEnabled;
    property Interval : Cardinal read FBlinkInterval write SetBlinkInterval;
  end;


  TCHIconLabel = class(TPersistent)
  private
    FOwner : TCHTrayIcon;
    FIconFont : TFont;
    FIconText : string;
    FTextBmp : TBitmap;
    FText_X : Integer;
    FText_Y : Integer;
    FIconPosY: Integer;
    FIconPosX: Integer;
    FAlignment: TTrayTextAlign;
    procedure DrawIconText;
    procedure AlignIconText;
    procedure SetIconFont(const Value: TFont);
    procedure SetIconText(const Value: string);
    procedure SetIconPosX(const Value: Integer);
    procedure SetIconPosY(const Value: Integer);
    procedure SetAlignment(const Value: TTrayTextAlign);
  public
    constructor Create(AOwner: TCHTrayIcon); virtual;
    destructor Destroy; override;
  published
    property Alignment : TTrayTextAlign read FAlignment Write SetAlignment;
    property Text : string read FIconText write SetIconText;
    property Font : TFont read FIconFont write SetIconFont;
    property PosX : Integer read FIconPosX Write SetIconPosX;
    property PosY : Integer read FIconPosY Write SetIconPosY;
  end;


  TCHTrayIcon = class(TComponent)
  private
    FHandle: HWnd;
    FEnabled: Boolean;
    FIcon: TIcon;
    FSaveIcon : TIcon;
    FBlankIcon : TIcon;
    FBlinkIcon : TIcon;
    FTextIcon : TIcon;
    FHint: string;
    FPopupMenu: TPopupMenu;
    FOnMouseLeftDown: TNotifyEvent;
    FOnMouseRightDown: TNotifyEvent;
    FOnMouseLeftUp: TNotifyEvent;
    FOnMouseRightUp: TNotifyEvent;
    FOnMouseMove: TNotifyEvent;
    FOnMouseRightDblClick: TNotifyEvent;
    FOnMouseLeftDblClick: TNotifyEvent;
    FNotifyIconData: TNotifyIconData;
    FShowTaskBar: Boolean;
    FShowHint: Boolean;
    FIconCreated : Boolean;
    FIconVisible : Boolean;
    FTrayAction: TCHTrayAction;
    FShowIcon: TTrayShow;
    FIconBlink : TCHIconBlink;
    FShowTaskEntry: TTrayShow;
    FTrayTimer : TTrayTimer;
    FIconLabel : TCHIconLabel;
    FBitmap : TBitmap;
    FIconIndex: Integer;
    FIconList: TImageList;
    FOnBlinkInVisible: TNotifyEvent;
    FOnBlinkVisible: TNotifyEvent;
    FIconMode: TTrayMode;
    FFormMinimized : Boolean;
    procedure CreateIcon;
    procedure UpdateIcon;
    procedure AddIcon;
    procedure HandleIcon;
    procedure DeleteIcon;
    procedure DoPopupmenu;
    function GetIconHandle: HIcon;
    function GetBlinkHandle : HICON;
    procedure BMPtoICO(Bitmap : TBitmap; var Icon : TIcon);
    procedure DoBlinkTimer(Sender : TObject);
    procedure DoTimer(Sender : TObject);
    procedure TaskIconEvent(var Msg: TMessage); Message WM_TASKICONEVENT;
    procedure SetEnabled(const Value: Boolean);
    procedure SetIcon(const Value: TIcon);
    procedure SetHint(const Value: string);
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure Minimize(Sender: TObject);
    procedure Restore(Sender: TObject);
    procedure SetShowHint(const Value: Boolean);
    procedure SetShowTaskbar(const Value: Boolean);
    procedure SetShowIcon(const Value: TTrayShow);
    procedure SetShowTaskEntry(const Value: TTrayShow);
    procedure SetIconIndex(const Value: Integer);
    procedure SetIconList(const Value: TImageList);
    procedure SetIconMode(const Value: TTrayMode);
  protected
    procedure BlinkVisible; virtual;
    procedure BlinkInVisible; virtual;
    procedure MouseMove; virtual;
    procedure MouseLeftDown; virtual;
    procedure MouseLeftUp; virtual;
    procedure MouseLeftDblClick; virtual;
    procedure MouseRightDown; virtual;
    procedure MouseRightUp; virtual;
    procedure MouseRightDblClick; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Handle: HWnd read FHandle;
  published
    property OnBlinkVisible : TNotifyEvent read FOnBlinkVisible Write FOnBlinkVisible;
    property OnBlinkInVisible : TNotifyEvent read FOnBlinkInVisible Write FOnBlinkInVisible;
    property OnMouseMove : TNotifyEvent read FOnMouseMove Write FOnMouseMove;
    property OnMouseLeftDown : TNotifyEvent read FOnMouseLeftDown Write FOnMouseLeftDown;
    property OnMouseRightDown : TNotifyEvent read FOnMouseRightDown Write FOnMouseRightDown;
    property OnMouseLeftUp : TNotifyEvent read FOnMouseLeftUp Write FOnMouseLeftUp;
    property OnMouseRightUp : TNotifyEvent read FOnMouseRightUp Write FOnMouseRightUp;
    property OnMouseLeftDblClick : TNotifyEvent read FOnMouseLeftDblClick Write FOnMouseLeftDblClick;
    property OnMouseRightDblClick : TNotifyEvent read FOnMouseRightDblClick Write FOnMouseRightDblClick;

    property Action : TCHTrayAction read FTrayAction Write FTrayAction;
    property Blink: TCHIconBlink read FIconBlink Write FIconBlink;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Hint: string read FHint write SetHint;
    property Icon: TIcon read FIcon write SetIcon;
    property IconList: TImageList read FIconList write SetIconList;
    property IconIndex: Integer read FIconIndex write SetIconIndex;
    property IconText : TCHIconLabel read FIconLabel Write FIconLabel;
    property Mode : TTrayMode read FIconMode Write SetIconMode;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property ShowHint : Boolean read FShowHint Write SetShowHint;
    property ShowIcon : TTrayShow read FShowIcon Write SetShowIcon;
    property ShowTaskBar : Boolean read FShowTaskBar Write SetShowTaskbar;
    property ShowTaskEntry : TTrayShow read FShowTaskEntry Write SetShowTaskEntry;
  end;

procedure Register;

implementation

{$R *.res}

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHTrayIcon]);
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHTrayIcon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FillChar(FNotifyIconData, SizeOf(TNotifyIconData), 0);
  FHandle := AllocateHWnd(TaskIconEvent);

  FTrayAction := TCHTrayAction.Create(Self);
  FIconBlink := TCHIconBlink.Create(Self);
  FIconLabel := TCHIconLabel.Create(Self);
  FTrayTimer := TTrayTimer.Create(Self);

  FBitmap := TBitmap.Create;
  FBitmap.Height := 16;
  FBitmap.Width := 16;
  FIcon := TIcon.Create;          // Standard icon or icon from IconList
  FTextIcon := TIcon.Create;      // Icon for text
  FBlinkIcon := TIcon.Create;     // FIcon and FBlankIcon in change
  FSaveIcon := TIcon.Create;      // a saving from the current icon
  FBlankIcon := TIcon.Create;     // only a empty icon for the blink effect
  BMPtoICO(FBitmap, FBlankIcon);  // make a blank icon
  FBlankIcon.Assign(FBlankIcon);

  FEnabled := True;
  FShowTaskBar := True;
  FShowHint := True;
  FIconVisible := True;
  FIconIndex := -1;
  FIconMode := trmIcon;
  FShowTaskEntry := tyOnRestore;
  FShowIcon := tyOnMinimized;

  if not (csDesigning in ComponentState) then
  begin
    Application.OnMinimize := Minimize;
    Application.OnRestore := Restore;
  end;

  FSaveIcon.Assign(FIcon);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHTrayIcon.Destroy;
begin
  if FHandle <> 0 then
    DeallocateHWnd(FHandle);
  if FIconCreated then
    DeleteIcon;
  // restore Taskbar, if you set "ShowTaskbar" = False
  ShowWindow(findwindow(pchar('Shell_TrayWnd'),nil),sw_shownormal);

  FBitmap.Free;
  FIcon.Free;
  FSaveIcon.Free;
  FBlankIcon.Free;
  FBlinkIcon.Free;
  FTextIcon.Free;

  FTrayAction.Free;
  FIconBlink.Free;
  FIconLabel.Free;
  FTrayTimer.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ handle the icon on the start of the application }
procedure TCHTrayIcon.DoTimer(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
  begin
    HandleIcon;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.HandleIcon;
begin
  // allways
  if FShowTaskEntry = tyAllways then
    ShowWindow(Application.Handle, SW_SHOW)
  // Never
  else if FShowTaskEntry = tyNever then
    ShowWindow(Application.Handle, SW_HIDE)
  // OnRestore
  else if (FShowTaskEntry = tyOnRestore) then
  begin
    if not FFormMinimized then
      ShowWindow(Application.Handle, SW_SHOW)
    else
      ShowWindow(Application.Handle, SW_HIDE);
  end
  // OnMinimized
  else if (FShowTaskEntry = tyOnMinimized) then
  begin
    if FFormMinimized then
      ShowWindow(Application.Handle, SW_SHOW)
    else
      ShowWindow(Application.Handle, SW_HIDE);
  end
  else
    ShowWindow(Application.Handle, SW_HIDE);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ get the icon handle for the "UpdateIcon" procedure }
function TCHTrayIcon.GetIconHandle: HIcon;
begin
  if FIconMode = trmIcon then
    Result := FIcon.Handle
  else
    Result := FTextIcon.Handle;

  if Result = 0 then
    Result := Application.Icon.Handle;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ get the blink-icon handle for the "UpdateIcon" procedure }
function TCHTrayIcon.GetBlinkHandle: HICON;
begin
  if FIconVisible then
  begin
    if FIconMode = trmIcon then
      Result := FSaveIcon.Handle
    else
      Result := FTextIcon.Handle;
  end
  else
    Result := FBlankIcon.Handle;

  if Result = 0 then
    Result := Application.Icon.Handle;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ create an icon on the first start of the application }
procedure TCHTrayIcon.CreateIcon;
begin
  if FEnabled and not (csDesigning in ComponentState) then
  begin
    FillChar(FNotifyIconData, SizeOf(TNotifyIconData), 0);
    with FNotifyIconData do
    begin
      cbSize := SizeOf(TNotifyIconData);
      uID := 1;
      Wnd := FHandle;

      uFlags := uFlags or NIF_MESSAGE or NIF_ICON;
      if FShowHint and (Length(FHint) > 0) then
      begin
        uFlags := uFlags or NIF_TIP;
        StrPCopy(szTip, PChar(FHint));
      end;

      uCallbackMessage := WM_TASKICONEVENT;
      hIcon := GetIconHandle;
    end;

    AddIcon;
  end;

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Update the icon. The icon must be created. }
procedure TCHTrayIcon.UpdateIcon;
begin
  if (FEnabled) and (FIconCreated) and not (csDesigning in ComponentState) then
  begin
    with FNotifyIconData do
    begin
      if FShowHint and (Length(FHint) > 0) then
      begin
        if uFlags and NIF_TIP = 0 then
          uFlags := uFlags or NIF_TIP;
        StrPCopy(szTip, PChar(FHint));
      end
      else
      begin
        if uFlags and NIF_TIP <> 0 then
          uFlags := uFlags and not NIF_TIP;
      end;

      if FIconBlink.FEnabled then
        hIcon := GetBlinkHandle
      else
        hIcon := GetIconHandle;
      Shell_NotifyIcon(NIM_MODIFY, @FNotifyIconData);
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ make the created icon visible in the tray }
procedure TCHTrayIcon.AddIcon;
begin
  if FEnabled and not (csDesigning in ComponentState) then
  begin
    Shell_NotifyIcon(NIM_ADD, @FNotifyIconData);
    FIconCreated := True;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ remove the icon from the tray }
procedure TCHTrayIcon.DeleteIcon;
begin
  if not (csDesigning in ComponentState) then
  begin
    Shell_NotifyIcon(NIM_DELETE, @FNotifyIconData);
    FIconCreated := False;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Enabled = False has the same effect as close the application}
procedure TCHTrayIcon.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if not FEnabled then
    begin
      if FIconCreated then
        DeleteIcon;
      ShowWindow(Application.Handle, SW_SHOW);
      ShowWindow(findwindow(pchar('Shell_TrayWnd'),nil),sw_shownormal);
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ set an icon hint text }
procedure TCHTrayIcon.SetHint(const Value: string);
begin
  if FHint <> Value then
  begin
    FHint := Value;
    UpdateIcon;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.SetIcon(const Value: TIcon);
begin
  if Value <> FIcon then
  begin
    FIcon.Assign(Value);
    // in design-time set SaveIcon if the icon change, in run-time set SaveIcon
    // only one time in the "Loaded" procedure
    if (csDesigning in ComponentState) then
      FSaveIcon.Assign(Value);
    UpdateIcon;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.SetPopupMenu(const Value: TPopupMenu);
begin
  if FPopupMenu <> Value then
  begin
    FPopupMenu := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.Minimize(Sender: TObject);
begin
  if FEnabled then
  begin
    if FShowIcon = tyOnMinimized then
    begin
      if FIconCreated = False then
        CreateIcon;
    end;

    FFormMinimized := True;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.Restore(Sender: TObject);
begin
  if FEnabled then
  begin
    SetForegroundWindow(Application.Handle);
    if FShowIcon = tyOnMinimized then
    begin
      DeleteIcon;
    end;

    FFormMinimized := False;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.SetShowHint(const Value: Boolean);
begin
  if FShowHint <> Value then
    FShowHint := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.SetShowTaskbar(const Value: Boolean);
begin
  if FShowTaskBar <> Value then
  begin
    FShowTaskBar := Value;
    if not (csDesigning in ComponentState) then
    begin
      if FShowTaskBar then
        showWindow(findwindow(pchar('Shell_TrayWnd'),nil),sw_shownormal)
      else
        showWindow(findwindow(pchar('Shell_TrayWnd'),nil),sw_hide);
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.SetShowIcon(const Value: TTrayShow);
begin
  if FShowIcon <> Value then
  begin
    FShowIcon := Value;
    if not (csDesigning in ComponentState) then
    begin
      if (FShowIcon = tyAllways) then
      begin
        if FIconCreated = False then
          CreateIcon
        else
          AddIcon;
      end
      else if (FShowIcon = tyOnMinimized) and (Application.MainForm.WindowState = wsMinimized) then
      begin
        if FIconCreated = False then
          CreateIcon
        else
          AddIcon;
      end
      else if (FShowIcon = tyNever) then
      begin
        if FIconCreated then
          DeleteIcon;
      end;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.SetShowTaskEntry(const Value: TTrayShow);
begin
  if FShowTaskEntry <> Value then
  begin
    FShowTaskEntry := Value;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.SetIconIndex(const Value: Integer);
begin
  if (FIconIndex <> Value) and (FIconList <> nil) then
  begin
    FIconIndex := Value;
    // if index out of range, set index to the last
    if Value >= FIconList.Count then
      FIconIndex := FIconList.Count - 1
    // if index smaller -1, set index to -1
    else if Value < -1 then
      FIconIndex := -1;

    // if index greater -1, set icon, else restore SaveIcon
    if FIconIndex > -1 then
      FIconList.GetIcon(FIconIndex, FIcon)
    else
      FIcon.Assign(FSaveIcon);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.SetIconList(const Value: TImageList);
begin
  if FIconList <> Value then
  begin
    FIconList := Value;
    if Value <> nil then
      Value.FreeNotification(Self)
    else
    begin
      FIconIndex := -1;
      FIcon.Assign(FSaveIcon);
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.SetIconMode(const Value: TTrayMode);
begin
  if FIconMode <> Value then
  begin
    FIconMode := Value;
    if FIconMode = trmText then
      FIconLabel.DrawIconText
    else
      UpdateIcon;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.DoBlinkTimer(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
  begin
    if FEnabled then
    begin
      if FIconVisible then
      begin
        FIconVisible := False;
        FBlinkIcon.Assign(FBlankIcon);
        BlinkInVisible;
      end
      else
      begin
        FIconVisible := True;
        if FIconMode = trmIcon then
          FBlinkIcon.Assign(FSaveIcon)
        else
          FBlinkIcon.Assign(FTextIcon);
        BlinkVisible;
      end;
      UpdateIcon;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.DoPopupmenu;
var
  P: TPoint;
begin
  if Assigned(FPopupMenu) and (FTrayAction.FPopup <> paNone) then
  begin
    SetForegroundWindow(Handle);

    GetCursorPos(P);
    FPopupMenu.PopupComponent := Self;
    FPopupMenu.Popup(P.X, P.Y);
    PostMessage(Application.Handle, WM_NULL, 0, 0);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.TaskIconEvent(var Msg: TMessage);

  procedure DoAction(Value : String);
  begin
    if (Value = 'WM_LBUTTONDOWN') and (FTrayAction.FPopup = paLeftDown) or
    (Value = 'WM_LBUTTONUP') and (FTrayAction.FPopup = paLeftUp) or
    (Value = 'WM_LButtonDblClk') and (FTrayAction.FPopup = paLeftDblClick) or
    (Value = 'WM_RButtonDown') and (FTrayAction.FPopup = paRightDown) or
    (Value = 'WM_RButtonDblClk') and (FTrayAction.FPopup = paRightDblClick) or
    (Value = 'WM_RBUTTONUP') and (FTrayAction.FPopup = paRightUp) then
    begin
      DoPopupmenu;
    end;

    if (Value = 'WM_LBUTTONDOWN') and (FTrayAction.FRestore = paLeftDown) or
    (Value = 'WM_LBUTTONUP') and (FTrayAction.FRestore = paLeftUp) or
    (Value = 'WM_LButtonDblClk') and (FTrayAction.FRestore = paLeftDblClick) or
    (Value = 'WM_RButtonDown') and (FTrayAction.FRestore = paRightDown) or
    (Value = 'WM_RButtonDblClk') and (FTrayAction.FRestore = paRightDblClick) or
    (Value = 'WM_RBUTTONUP') and (FTrayAction.FRestore = paRightUp) then
    begin
      Application.Restore;
    end;
  end;

begin
  if Msg.Msg = WM_TASKICONEVENT then
  begin
    case Msg.LParam of
      WM_MouseMove:
        begin
          MouseMove;
        end;
      WM_LBUTTONDOWN:
        begin
          DoAction('WM_LBUTTONDOWN');
          MouseLeftDown;
        end;
      WM_LBUTTONUP:
        begin
          DoAction('WM_LBUTTONUP');
          MouseLeftUp;
        end;
      WM_LButtonDblClk:
        begin
          DoAction('WM_LButtonDblClk');
          MouseLeftDblClick;
        end;
      WM_RButtonDown:
        begin
          DoAction('WM_RButtonDown');
          MouseRightDown;
        end;
      WM_RButtonDblClk:
        begin
          DoAction('WM_RButtonDblClk');
          MouseRightDblClick;
        end;
      WM_RBUTTONUP:
        begin
          DoAction('WM_RBUTTONUP');
          MouseRightUp;
        end;
    end;
  end
  else
  begin
    case Msg.Msg of
      WM_CLOSE, WM_QUIT, WM_DESTROY, WM_NCDESTROY:
        begin
          Msg.Result := 1;
        end;

      WM_QUERYENDSESSION,
      WM_ENDSESSION:
        begin
          Msg.Result := 1;
        end;
    else
      Msg.Result := DefWindowProc(FHandle, Msg.Msg, Msg.wParam, Msg.lParam);
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.BlinkInVisible;
begin
  if Assigned(FOnBlinkInVisible) then
    FOnBlinkInVisible(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.BlinkVisible;
begin
  if Assigned(FOnBlinkVisible) then
    FOnBlinkVisible(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.MouseLeftDown;
begin
  if Assigned(FOnMouseLeftDown) then
    FOnMouseLeftDown(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.MouseLeftUp;
begin
  if Assigned(FOnMouseLeftUp) then
    FOnMouseLeftUp(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.MouseRightDown;
begin
  if Assigned(FOnMouseRightDown) then
    FOnMouseRightDown(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.MouseRightUp;
begin
  if Assigned(FOnMouseRightUp) then
    FOnMouseRightUp(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.MouseLeftDblClick;
begin
  if Assigned(FOnMouseLeftDblClick) then
    FOnMouseLeftDblClick(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.MouseMove;
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.MouseRightDblClick;
begin
  if Assigned(FOnMouseRightDblClick) then
    FOnMouseRightDblClick(Self);
end;



{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ TCHTrayAction }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHTrayAction.Create(AOwner: TCHTrayIcon);
begin
  inherited Create;
  FOwner := AOwner;

  FPopup := paRightDown;
  FRestore := paLeftDblClick;
end;



{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ TCHIconBlink }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHIconBlink.Create(AOwner: TCHTrayIcon);
begin
  inherited Create;
  FOwner := AOwner;

  FEnabled := False;
  FBlinkInterval := 1000;

  FBlinkTimer := TTimer.Create(AOwner);
  FBlinkTimer.Interval := FBlinkInterval;
  FBlinkTimer.Enabled := FEnabled;
  FBlinkTimer.OnTimer := FOwner.DoBlinkTimer;

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHIconBlink.Destroy;
begin
  FBlinkTimer.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHIconBlink.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    FBlinkTimer.Enabled := Value;

    if FEnabled = False then
    begin
      FOwner.FIconVisible := True;
      if FOwner.FIconMode = trmIcon then
        FOwner.FBlinkIcon.Assign(FOwner.FSaveIcon)
      else
        FOwner.FBlinkIcon.Assign(FOwner.FTextIcon);
      FOwner.BlinkVisible;
      FOwner.UpdateIcon;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHIconBlink.SetBlinkInterval(const Value: Cardinal);
begin
  if FBlinkInterval <> Value then
  begin
    FBlinkInterval := Value;
    FBlinkTimer.interval := FBlinkInterval;
  end;
end;




{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ TTrayTimer }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TTrayTimer.Create(AOwner: TCHTrayIcon);
begin
  inherited Create;
  FOwner := AOwner;

  FTimer := TTimer.Create(AOwner);
  FTimer.Interval := 250;
  FTimer.Enabled := True;
  FTimer.OnTimer := FOwner.DoTimer;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TTrayTimer.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ TCHIconLabel }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHIconLabel.Create(AOwner: TCHTrayIcon);
begin
  inherited Create;
  FOwner := AOwner;

  FIconFont := TFont.Create;
  FIconFont.Name := 'Arial';
  FIconFont.Size := 10;
  FIconFont.Style := [fsBold];
  FIconFont.Color := clBlack;

  FTextBmp := TBitmap.Create;
  FTextBmp.Height := 16;
  FTextBmp.Width := 16;
  FText_X := 0;
  FText_Y := 0;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHIconLabel.Destroy;
begin
  FIconFont.Free;
  FTextBmp.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHIconLabel.DrawIconText;
begin
  if FOwner.Mode = trmText then
  begin
    // icon text alignment
    AlignIconText;
    with FTextBmp do
    begin
      // set text font
      Canvas.Font.Assign(FIconFont);
      // transparent color must be an other color then font color
      if (Font.Color - 1) < 0 then
        TransparentColor := Font.Color + 1
      else
        TransparentColor := Font.Color - 1;
      // fill Textbitmap with transparent color
      Canvas.Brush.Color := TransparentColor;
      Canvas.FillRect(Rect(0,0,Width,Height));
      // drwa the text
      Canvas.TextOut(FText_X, FText_Y, FIconText);
    end;
    // convert bitmap to icon
    FOwner.BMPtoICO(FTextBmp, FOwner.FTextIcon);
    // update the icon in the taskbar
    FOwner.UpdateIcon;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHIconLabel.AlignIconText;
begin
  if FAlignment = ttgCustom then
  begin
    FText_X := FIconPosX;
    FText_Y := FIconPosY;
  end
  else if FAlignment = ttgCenter then
  begin
    FText_X := (FTextBmp.Width div 2) - (FTextBmp.Canvas.TextWidth(FIconText) div 2);
    FText_Y := (FTextBmp.Height div 2) - (FTextBmp.Canvas.TextHeight(FIconText) div 2);
  end
  else if FAlignment = ttgCenterBottom then
  begin
    FText_X := (FTextBmp.Width div 2) - (FTextBmp.Canvas.TextWidth(FIconText) div 2);
    FText_Y := (FTextBmp.Height - FTextBmp.Canvas.TextHeight(FIconText));
  end
  else if FAlignment = ttgCenterTop then
  begin
    FText_X := (FTextBmp.Width div 2) - (FTextBmp.Canvas.TextWidth(FIconText) div 2);
    FText_Y := 0;
  end
  else if FAlignment = ttgCenterLeft then
  begin
    FText_X := 0;
    FText_Y := (FTextBmp.Height div 2) - (FTextBmp.Canvas.TextHeight(FIconText) div 2);
  end
  else if FAlignment = ttgCenterRight then
  begin
    FText_X := (FTextBmp.Width - FTextBmp.Canvas.TextWidth(FIconText));
    FText_Y := (FTextBmp.Height div 2) - (FTextBmp.Canvas.TextHeight(FIconText) div 2);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHIconLabel.SetAlignment(const Value: TTrayTextAlign);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    DrawIconText;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHIconLabel.SetIconFont(const Value: TFont);
begin
  FIconFont.Assign(Value);
  DrawIconText;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHIconLabel.SetIconPosX(const Value: Integer);
begin
  if FIconPosX <> Value then
  begin
    FIconPosX := Value;
    DrawIconText;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHIconLabel.SetIconPosY(const Value: Integer);
begin
  if FIconPosY <> Value then
  begin
    FIconPosY := Value;
    DrawIconText;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHIconLabel.SetIconText(const Value: string);
begin
  if FIconText <> Value then
  begin
    FIconText := Value;
    DrawIconText;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTrayIcon.BMPtoICO(Bitmap: TBitmap; var Icon: TIcon);
var
  ImageList : TImageList;
begin
  ImageList := TImageList.CreateSize(Bitmap.Width, Bitmap.Height);
  try
    ImageList.AddMasked(Bitmap, Bitmap.TransparentColor);
    ImageList.GetIcon(0, Icon);
  finally
    ImageList.Free;
   end;
end;

end.
