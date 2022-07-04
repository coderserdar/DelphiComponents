unit API_trayicon;

//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
//
// component revision history
//
// r1.05, 29062008, ari pikivirta
//  * added minimize on startup property
//
// r1.04, 29062008, ari pikivirta
//  * added detection of delphi version, since tray icon wasn't working on
//    delphi 2006 and 2007 anymore because of MainformOnTaskbar being on by default
//
// r1.03, 24022007, ari pikivirta
//  * if windows tray has faulted and loaded again, tray icon should be
//    able to now get the icon back there (it's watching the tray handle)
//
// r1.02, 25.7.2006, ari pikivirta
// * added OnMouseEnter and OnMouseLeave properties (mouseleave will occur
//   if mouse have been in same place more than 1 seconds also)
// * added visible property
// * fixed icon to be allowed also trough the icon property if set
// * application title will also be removed on hide
// * added IsHidden function to get if application is hidden or not
//
// r1.01, ari pikivirta
// * just fixed the source code look
//
// r1.00, ari pikivirta

interface

uses
 Windows, Messages, ShellApi, SysUtils, Classes, Graphics, Controls,
 Forms, Dialogs, Menus, ExtCtrls;

const
  WM_CALLBACK_MESSAGE = WM_USER + 1;

type
  TAPI_trayicon = class(tcomponent)
  private
    fversion: string;
    fadded: boolean;
    fvisible: boolean;
    fData: TNotifyIconData;
    fIcon: TIcon;
    fHint: string;
    fhintdelay: cardinal;
    fPopupMenu: TPopupMenu;
    fClicked: Boolean;
    fOnClick: TNotifyEvent;
    fOnDblClick: TNotifyEvent;
    fOnMouseMove: TMouseMoveEvent;
    fOnMouseDown: TMouseEvent;
    fOnMouseUp: TMouseEvent;
    fOnMinimize: TNotifyEvent;
    fOnRestore: TNotifyEvent;
    fOnShow: TNotifyEvent;
    fOnHide: TNotifyEvent;
    fautoshowicon: boolean;
    fautohideicon: boolean;
    fenterstate: boolean;
    fEntered: integer;
    fmouseposx, fmouseposy: integer;        // stores last mouse move positions
    fonmouseenter: Tnotifyevent;
    fonmouseleave: tnotifyevent;
    ftimer: ttimer;
    fAppHidden: boolean;
    fdonothide: boolean;
    foldtrayhandle: thandle;
    ftraychecktimer: integer;
    fminimizeonstart: boolean;
    fAppTitle: string;
    fhidetitle: boolean;

    procedure dummys(s: string);
    procedure setvisible(b: boolean);
    procedure SetIcon(Ico: TIcon);
    procedure SetHint(Hint: string);
    function  FindTrayHandle: THandle;

  protected
    procedure AppMinimize(Sender: TObject);
    procedure AppRestore(Sender: TObject);
    //procedure AppShow(Sender: TObject);
    procedure DoMenu; virtual;
    procedure Click; virtual;
    procedure DblClick; virtual;
    procedure EndSession; virtual;
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure OnMessage(var Msg: TMessage); virtual;
    procedure OnTimer(sender: tobject);
    procedure Loaded; override;

  public
    constructor Create(Owner: TComponent); override;
    destructor  Destroy; override;
    procedure   Changed;
    procedure   Minimize;
    procedure   Restore;
    procedure   Hide;
    procedure   Show;
    function    IsHidden: boolean;
    procedure   GetMousePos(var x, y: integer); overload;
    function    GetMousePos: TPoint; overload;

  published
    property Version: string read fversion write dummys stored false;
    property DoNotHide: boolean read fdonothide write fdonothide;
    property HideTitle: boolean read fhidetitle write fhidetitle;
    property AutoShowIcon: boolean read fautoshowicon write fautoshowicon;
    property AutoHideIcon: boolean read fautohideicon write fautohideicon;
    property Hint: string read fHint write SetHint;
    property HintDelay: cardinal read fhintdelay write fhintdelay;
    property Icon: TIcon read fIcon write SetIcon;
    property Visible: boolean read fvisible write setvisible;
    property PopupMenu: TPopupMenu read fPopupMenu write fPopupMenu;
    property OnClick: TNotifyEvent read fOnClick write fOnClick;
    property OnDblClick: TNotifyEvent read fOnDblClick write fOnDblClick;
    property OnMinimize: TNotifyEvent read fOnMinimize write fOnMinimize;
    property OnShow: TNotifyEvent read fonshow write fonshow;
    property OnHide: TNotifyEvent read fonhide write fonhide;
    property OnMouseMove: TMouseMoveEvent read fOnMouseMove write fOnMouseMove;
    property OnMouseDown: TMouseEvent read fOnMouseDown write fOnMouseDown;
    property OnMouseUp: TMouseEvent read fOnMouseUp write fOnMouseUp;
    property OnRestore: TNotifyEvent read fOnRestore write fOnRestore;
    property OnMouseEnter: Tnotifyevent read fonmouseenter write fonmouseenter;
    property OnMouseLeave: Tnotifyevent read fonmouseleave write fonmouseleave;
    property MinimizeOnStartup: boolean read fminimizeonstart write fminimizeonstart;

  end;

procedure Register;

implementation

const
  versioninfo = 'r1.05/ari.pikivirta(at)kolumbus.fi';

{$r *.RES}
{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_CODE OFF}

procedure TAPI_trayicon.dummys(s: string); begin end;

//------------------------------------------------------------------------------
constructor tAPI_trayIcon.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  fversion:= versioninfo;

  fIcon:= TIcon.Create;                           // create icon
  fadded:= false;                                 // not added yet
  fentered:= 0;                                   // mouse not entered
  fenterstate:= false;                            // -"-
  fvisible:= false;
  fautoshowicon:= true;
  fautohideicon:= true;
  fappHidden:= false;                             // not hidden yet
  fdonothide:= false;
  fminimizeonstart:= false;                       // don't minimize on startup
  fapptitle:= '';
  fhidetitle:= false;                             // hide from task list
  fhint:= '';                                     // no hint as default
  fhintdelay:= 500;                               // hint delay to 500 by default

  ftimer:= ttimer.create(self);                   // create timer
  ftimer.Interval:= 100;                          // set interval to 100ms
  ftimer.OnTimer:= ontimer;                       // set internal event
  ftimer.Enabled:= true;                          // allow timer to run

  //Application.MainFormOnTaskBar:= False;

  Application.OnMinimize:= AppMinimize;
  Application.OnRestore:= AppRestore;

  if not (csDesigning in ComponentState) then
  begin
    fData.Wnd:= classes.AllocateHWnd(OnMessage);           // get window handle for messages
    foldtrayhandle:= findtrayhandle;
    ftraychecktimer:= 0;
  end else
  begin
    foldtrayhandle:= INVALID_HANDLE_VALUE;
    fdata.wnd:= INVALID_HANDLE_VALUE;        // otherwise keep it zero
  end;
end;

//------------------------------------------------------------------------------
destructor tAPI_trayIcon.Destroy;
begin
  ftimer.enabled:= false;                         // disable timer
  ftimer.free;                                    // and free it also
  fvisible:= false;                               // set icon invisible
  changed;                                        // and remove it from tray
  if (fdata.Wnd<>INVALID_HANDLE_VALUE) then       // if message handle exists
    classes.DeallocateHWnd(fdata.wnd);                    // remove it from memory
  fIcon.Free;                                     // free internal icon
  inherited Destroy;
end;

//------------------------------------------------------------------------------
function TAPI_TrayIcon.FindTrayHandle: THandle;
begin
  result:= FindWindow('Shell_TrayWnd', nil);
  if result<>0 then
    Result:= FindWindowEx(result, 0, 'TrayNotifyWnd', nil)
end;

//------------------------------------------------------------------------------
procedure tAPI_trayIcon.Changed;
begin
  if (not (csLoading in ComponentState)) and (fdata.Wnd<>INVALID_HANDLE_VALUE) then
    if fvisible then
    begin
      fData.cbSize:= SizeOf(tnotifyicondata);
      fData.uCallbackMessage:= WM_CALLBACK_MESSAGE;
      if fhint<>'' then StrPLCopy(fData.szTip, fhint, SizeOf(fData.szTip)-1)
        else fdata.szTip:= #0;
      //fdata.uTimeout:= fhintdelay;
      fData.uFlags:= Nif_Message;
      if ficon.Empty then
        fdata.hIcon:= application.Icon.Handle
        else fData.hIcon:= fIcon.Handle;
      if fData.hIcon<>0 then fdata.uflags:= fdata.uflags or Nif_Icon
        else fdata.uflags:= fdata.uflags and not Nif_Icon;
      if fHint<>'' then fData.uFlags:= fData.uFlags or Nif_Tip
        else fData.uFlags:= fData.uFlags and not Nif_Tip;
      // add tray icon and/or notify
      if not fadded then
      begin
        fadded:= Shell_NotifyIcon(NIM_ADD, @fData);
      end else
        Shell_NotifyIcon(NIM_MODIFY, @fData);
    end else
      // hide tray icon
      if fadded then
        fadded:= not Shell_NotifyIcon(NIM_DELETE, @fData);
end;

//------------------------------------------------------------------------------
function TAPI_trayicon.IsHidden: boolean;
begin
  result:= fAppHidden;
end;

//------------------------------------------------------------------------------
procedure TAPI_trayIcon.Loaded;
begin
  inherited Loaded;

  // minimize on startup - DOESN'T WORK!?
  if not (csDesigning in ComponentState) then
  begin
    if fminimizeonstart then
    begin
      (owner as tform).WindowState:= wsminimized;
      if not fdonothide then
      begin
        (*
        application.ShowMainForm:= false;
        (owner as tform).visible:= false;
        *)
        //ShowWindow(application.handle, SW_MINIMIZE);
        showWindow(application.Handle, SW_HIDE);
        fAppHidden:= true;
        fvisible:= true;
      end;
    end;
    // notify icon(s) changed
    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_trayicon.setvisible(b: Boolean);
begin
  if b<>fvisible then                             // visible state changed
  begin
    fvisible:= b;                                 // store new state
    changed;                                      // and notify icon
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_trayicon.OnTimer(sender: tobject);
var
  pt: tpoint;
  h: thandle;
begin
  GetCursorPos(Pt);

  if (fentered = 0)                               // entered counter is zero
    and (fmouseposx<>pt.x)
    and (fmouseposy<>pt.y) then
  begin
    if fenterstate = true then                    // and mouse was entered
    begin
      if assigned(fonmouseleave) then             // if mouseleave is assigned
        fonmouseleave(self);                      // fire leave event
      fenterstate:= false;                        // currently not entered
    end;
  end else

  if (fmouseposx<>pt.x) and (fmouseposy<>pt.y) then
  begin
    if fenterstate = false then                   // not entered yet
    begin
      if assigned(fonmouseenter) then             // if enter event assigned
        fonmouseenter(self);                      // launch enter event
      fenterstate:= true;                         // store enter state
    end;
    fentered:= fentered - 1;                      // decrease entered delay
  end;

  // add new icon handle to tray, because
  // tray was for some reason restarted
  if not (csDesigning in ComponentState) then
  begin
    if ftraychecktimer<20 then ftraychecktimer:= ftraychecktimer + 1
      else begin
        h:= findtrayhandle;
        if foldtrayhandle<>h then
        begin
          foldtrayhandle:= h;
          fadded:= false;
          changed;
        end;
        ftraychecktimer:= 0;
      end;
  end;

  fmouseposx:= pt.x;
  fmouseposy:= pt.y;
end;

//------------------------------------------------------------------------------
procedure TAPI_trayicon.GetMousePos(var x, y: integer);
var
  pt: tpoint;
begin
  getcursorpos(pt);
  x:= pt.X;
  y:= pt.y;
end;

//------------------------------------------------------------------------------
function TAPI_trayicon.GetMousePos: TPoint;
begin
  getcursorpos(result);
end;

//------------------------------------------------------------------------------
procedure TAPI_trayicon.Show;
begin
  if (csDesigning in ComponentState) then
  begin
    inherited;
    exit;
  end;

  if (fapptitle<>'') and (application.title='') then
  begin
    application.title:= fapptitle;
    fapptitle:= '';
  end;

  (*
  if (not application.ShowMainForm) and (not ((owner as tform).visible)) then
  begin
    (owner as tform).visible:= true;
    ShowWindow(application.handle, SW_MINIMIZE);
  end;
  *)

  if assigned(fonshow) then fonshow(self);

  showwindow(application.Handle, SW_SHOW);        // show window

  if (fautohideicon) then
  begin
    fvisible:= false;
    changed;
  end;

  fAppHidden:= false;
end;

//------------------------------------------------------------------------------
procedure TAPI_trayIcon.Hide;
begin
  if (csDesigning in ComponentState) then
  begin
    inherited;
    exit;
  end;

  if assigned(fonhide) then fonhide(self);

  ShowWindow(Application.Handle, SW_HIDE);        // hide application from tray

  if (fautoshowicon) then
  begin
    fvisible:= true;
    changed;
  end;

  if (fhidetitle) and (application.title<>'') and (fapptitle='') then
  begin
    fapptitle:= application.title;
    application.title:= '';
  end;

  fAppHidden:= true;                              // set hidden state to true
end;

//------------------------------------------------------------------------------
procedure TAPI_trayicon.Minimize;
begin
  if assigned(fonminimize) then fonminimize(self);
  showwindow(application.handle, SW_MINIMIZE);      // minimize
  if not fDoNotHide then Hide;                      // hide
end;

//------------------------------------------------------------------------------
procedure tAPI_trayIcon.Restore;
begin
  if assigned(fonrestore) then fonrestore(self);
  ShowWindow(application.Handle, SW_RESTORE);       // restore
  if FAppHidden then show;                          // show
end;

//------------------------------------------------------------------------------
procedure tAPI_trayIcon.AppMinimize(Sender: TObject);
begin
  Minimize;
  //inherited;
end;

//------------------------------------------------------------------------------
procedure tAPI_trayIcon.AppRestore(Sender: TObject);
begin
  Restore;
  //inherited;
end;

//------------------------------------------------------------------------------
procedure tAPI_trayIcon.OnMessage(var Msg: TMessage);

  function ShiftState: TShiftState;
  begin
    Result := [];
    if GetKeyState(VK_SHIFT) < 0 then Include(Result, ssShift);
    if GetKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
    if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
  end;

var
  Pt: TPoint;
  Shift: TShiftState;
begin
  case Msg.Msg of
    Wm_QueryEndSession:
      Msg.Result:= 1;
    Wm_EndSession:
      if TWmEndSession(Msg).EndSession then
        EndSession;
    Wm_Callback_Message:
      case Msg.lParam of
        WM_MOUSEMOVE:
          begin
            Shift:= ShiftState;
            GetCursorPos(Pt);
            DoMouseMove(Shift, Pt.X, Pt.Y);
          end;
        WM_LBUTTONDOWN:
          begin
            Shift:= ShiftState + [ssLeft];
            GetCursorPos(Pt);
            DoMouseDown(mbLeft, Shift, Pt.X, Pt.Y);
            fClicked:= True;
          end;
        WM_LBUTTONUP:
          begin
            Shift:= ShiftState + [ssLeft];
            GetCursorPos(Pt);
            if fClicked then
            begin
              fClicked:= False;
              Click;
            end;
            DoMouseUp(mbLeft, Shift, Pt.X, Pt.Y);
          end;
        WM_LBUTTONDBLCLK:
          DblClick;
        WM_RBUTTONDOWN:
          begin
            Shift:= ShiftState + [ssRight];
            GetCursorPos(Pt);
            DoMouseDown(mbRight, Shift, Pt.X, Pt.Y);
            DoMenu;
          end;
        WM_RBUTTONUP:
          begin
            Shift:= ShiftState + [ssRight];
            GetCursorPos(Pt);
            DoMouseUp(mbRight, Shift, Pt.X, Pt.Y);
          end;
        WM_RBUTTONDBLCLK:
          DblClick;
        WM_MBUTTONDOWN:
          begin
            Shift:= ShiftState + [ssMiddle];
            GetCursorPos(Pt);
            DoMouseDown(mbMiddle, Shift, Pt.X, Pt.Y);
          end;
        WM_MBUTTONUP:
          begin
            Shift:= ShiftState + [ssMiddle];
            GetCursorPos(Pt);
            DoMouseUp(mbMiddle, Shift, Pt.X, Pt.Y);
          end;
        WM_MBUTTONDBLCLK:
          DblClick;
      end;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_trayIcon.SetHint(Hint: string);
begin
  if fhint<>'' then                           // if there is already some hint
  begin
    fhint:= '';                               // make it empty
    changed;                                  // notify tray icon
  end;
  fHint:=Hint;                                // store hint we wanted
  Changed;                                    // notify tray icon once again
end;

//------------------------------------------------------------------------------
procedure tAPI_trayIcon.SetIcon(Ico: TIcon);
begin
  fIcon.Assign(Ico);                          // store new one to ficon
  ficon.Modified:= true;
  Changed;                                    // fire icon notify
end;

//------------------------------------------------------------------------------
procedure tAPI_trayIcon.DoMenu;
var
  Pt: TPoint;
begin
  if (fPopupMenu<>nil) then
  begin
    GetCursorPos(Pt);
    fPopupMenu.Popup(Pt.X, Pt.Y);
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_trayIcon.Click;
begin
  if Assigned(fOnClick) then
    fOnClick(Self);
end;

//------------------------------------------------------------------------------
procedure tAPI_trayIcon.DblClick;
begin
  if Assigned(fOnDblClick) then
    fOnDblClick(Self);
end;

//------------------------------------------------------------------------------
procedure tAPI_trayIcon.DoMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  fentered:= 2;   // 200ms
  if Assigned(fOnMouseMove) then
    fOnMouseMove(Self, Shift, X, Y);
end;

//------------------------------------------------------------------------------
procedure tAPI_trayIcon.DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(fOnMouseDown) then
    fOnMouseDown(Self, Button, Shift, X, Y);
end;

//------------------------------------------------------------------------------
procedure tAPI_trayIcon.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(fOnMouseUp) then
    fOnMouseUp(Self, Button, Shift, X, Y);
end;

//------------------------------------------------------------------------------
procedure tAPI_trayIcon.EndSession;
begin
  Shell_NotifyIcon(Nim_Delete, @fData);
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_trayicon]);
end;

end.
