unit CHApplication;

{ ##############################################################################
  TCHApplication

  Version   		:   1.0.4
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 21.07.2002    - First Release
  1.0.1 - 29.08.2002    - CHANGE: code reorganized
  1.0.2 - 15.12.2002    - BUG: repair some memory leaks
  1.0.3 - 09.03.2003    - reorganize "uses" for more performance and less memory needed
  1.0.4 - 21.03.2003    - NEW: command-line for restart app.

  ############################################################################ }

interface

uses
  Windows, Forms, SysUtils, Classes, Controls, ExtCtrls, Graphics, ActnList, Registry,
  ShellApi;

type
  TRestart = (rsTimer, rsFunction);
  TStartUpKey = (ruRUN, ruRUNSERVICES, ruRUNONCE);
  TRegKey = (rkLocalMachine, rkCurrentUser);
  TRegEvent = (reAdd, reDelete);

  TCHApplication = class;

  TCHHint = class(TPersistent)
  private
    FOwner : TCHApplication;
    FHintShort: Boolean;
    FShow: Boolean;
    FShort: Cardinal;
    FHidePause: Cardinal;
    FHintPause: Cardinal;
    FHint: string;
    FHintColor: TColor;

    procedure SetHintColor(const Value : TColor);
    procedure SetHidePause(const Value : Cardinal);
    procedure SetHint(const Value : string);
    procedure SetHintPause(const Value : Cardinal);
    procedure SetHintShort(const Value : boolean);
    procedure SetShort(const Value : Cardinal);
    procedure SetShow(const Value : boolean);
  public
    constructor Create(AOwner: TCHApplication); virtual;
  published
    property Hint : string read FHint write SetHint;
    property Color : TColor read FHintColor write SetHintColor;
    property HidePause : Cardinal read FHidePause write SetHidePause;
    property Pause : Cardinal read FHintPause write SetHintPause;
    property Shortcuts : Boolean read FHintShort write SetHintShort;
    property ShortPause : Cardinal read FShort write SetShort;
    property ShowHints : Boolean read FShow write SetShow;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHInstance = class(TPersistent)
  private
    FOwner : TCHApplication;
    FJustOneApp : Boolean;
    FMessageText : string;
    FMessageTitle : string;
    FShowMessage : Boolean;

    procedure SetJustOneApp(const Value: Boolean);
    procedure SetMessageText(const Value: string);
    procedure SetMessageTitle(const Value: string);
    procedure SetShowMessage(const Value: Boolean);
  public
    constructor Create(AOwner: TCHApplication); virtual;
  published
    property JustOneApp : Boolean read FJustOneApp Write SetJustOneApp;
    property MessageText : string read FMessageText Write SetMessageText;
    property MessageTitle : string read FMessageTitle Write SetMessageTitle;
    property ShowMessage : Boolean read FShowMessage Write SetShowMessage;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHRegRun= class(TPersistent)
  private
    FOwner : TCHApplication;
    FStartUpKey : TStartUpKey;
    FRegKey : TRegKey;
    FRegEvent : TRegEvent;
    FActive : Boolean;

    procedure SetActive(const Value: Boolean);
    procedure SetRegEvent(const Value: TRegEvent);
    procedure SetRegKey(const Value: TRegKey);
    procedure SetStartUpKey(const Value: TStartUpKey);
  public
    constructor Create(AOwner: TCHApplication); virtual;
  published
    property Active : Boolean read FActive write SetActive;
    property KeyMode : TStartUpKey read FStartUpKey Write SetStartUpKey;
    property RegEvent : TRegEvent read FRegEvent Write SetRegEvent;
    property RegKey : TRegKey read FRegKey Write SetRegKey;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHRestart = class(TPersistent)
  private
    FOwner : TCHApplication;
    FRestartMode : TRestart;
    FRestartTimer : Boolean;
    FTimerInterval : Cardinal;
    FMessageText : string;
    FMessageTitle : string;
    FShowMessage : Boolean;
    FOnlyClose : Boolean;
    FTimer : TTimer;
    FCommand: string;

    procedure SetRestartMode(const Value: TRestart);
    procedure SetRestartTimer(const Value: Boolean);
    procedure SetTimerInterval(const Value: Cardinal);
    procedure SetMessageText(const Value: string);
    procedure SetMessageTitle(const Value: string);
    procedure SetOnlyClose(const Value: Boolean);
    procedure SetShowMessage(const Value: Boolean);
    procedure SetCommand(const Value: string);
  public
    constructor Create(AOwner: TCHApplication); virtual;
    destructor Destroy; override;
  published
    property Command : string read FCommand Write SetCommand;
    property Mode : TRestart read FRestartMode write SetRestartMode;
    property Timer : Boolean read FRestartTimer Write SetRestartTimer;
    property TimerInterval : Cardinal read FTimerInterval Write SetTimerInterval;
    property MessageText : string read FMessageText Write SetMessageText;
    property MessageTitle : string read FMessageTitle Write SetMessageTitle;
    property ShowMessage : Boolean read FShowMessage Write SetShowMessage;
    property OnlyClose : Boolean read FOnlyClose Write SetOnlyClose;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHFlash = class(TPersistent)
  private
    FOwner : TCHApplication;
    FTitleBlink: Boolean;
    FBlinkInterval: Cardinal;
    FTimer : TTimer;

    procedure SetBlinkInterval(const Value: Cardinal);
    procedure SetTitleBlink(const Value: Boolean);
  public
    constructor Create(AOwner: TCHApplication); virtual;
    destructor Destroy; override;
  published
    property Blink : Boolean read FTitleBlink write SetTitleBlink;
    property Interval : Cardinal read FBlinkInterval Write SetBlinkInterval;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHApplication = class(TComponent)
  private
    FHintClass : TCHHint;
    FInstanceClass : TCHInstance;
    FRestartClass : TCHRestart;
    FTitleClass : TCHFlash;
    FRegRunClass : TCHRegRun;

    FException : TExceptionEvent;
    FIdle : TIdleEvent;
    FHelp : THelpEvent;
    FShowHint : TShowHintEvent;
    FActionExecute : TActionEvent;
    FActionUpdate : TActionEvent;
    FActivate : TNotifyEvent;
    FDeactivate : TNotifyEvent;
    FMinimize : TNotifyEvent;
    FRestore : TNotifyEvent;
    FHint : TNotifyEvent;
    FMessage : TMessageEvent;
    FShortcut : TShortCutEvent;
    FCreateInstance : Boolean;
    hMutex: THandle;
    FShowApplication : Boolean;
    FAppTitle : string;
    Fkerneldll : THandle;
    FForm : TCustomForm;

    procedure DoRestart(Sender : TObject);
    procedure DoBlink(Sender : TObject);
    procedure DoRegEdit;

    procedure SetHelp(Value : THelpEvent);
    procedure SetIdle(Value : TIdleEvent);
    procedure SetException(Value : TExceptionEvent);
    procedure SetShowHint(Value : TShowHintEvent);
    procedure SetActionExecute(Value : TActionEvent);
    procedure SetActionUpdate(Value : TActionEvent);
    procedure SetActivate(Value : TNotifyEvent);
    procedure SetDeactivate(Value : TNotifyEvent);
    procedure SetMinimize(Value : TNotifyEvent);
    procedure SetHint(Value : TNotifyEvent);
    procedure SetRestore(Value : TNotifyEvent);
    procedure SetMessage(Value : TMessageEvent);
    procedure SetShortcut(Value : TShortCutEvent);
    procedure SetShowApplication(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    destructor Destroy; override;

    procedure HideInTaskbar;
    procedure ShowInTaskbar;
    procedure HideInTaskManager;
    procedure ShowInTaskManager;
    procedure CanAppClose(Form : TCustomForm; IconClose, MenuClose, MenuActive: Boolean);
  published
    property OnActionExecute : TActionEvent read FACtionExecute write SetActionExecute;
    property OnActionUpdate : TActionEvent read FActionUpdate write SetActionUpdate;
    property OnActivate : TNotifyEvent read FActivate write SetActivate;
    property OnDeactivate : TNotifyEvent read FDeactivate write SetDeactivate;
    property OnException : TExceptionEvent read FException Write SetException;
    property OnHelp : THelpEvent read FHelp write SetHelp;
    property OnHint : TNotifyEvent read FHint write SetHint;
    property OnIdle : TIdleEvent read FIdle write SetIdle;
    property OnMessage : TMessageEvent read FMessage write SetMessage;
    property OnMinimize : TNotifyEvent read FMinimize write SetMinimize;
    property OnRestore : TNotifyEvent read FRestore write SetRestore;
    property OnShowHint : TShowHintEvent read FShowHint write SetShowHint;
    property OnShortcut : TShortCutEvent read FShortcut write SetShortcut;

    property Hint : TCHHint read FHintClass Write FHintClass;
    property Instance : TCHInstance read FInstanceClass Write FInstanceClass;
    property Restart : TCHRestart read FRestartClass Write FRestartClass;
    property ShowApplication : Boolean read FShowApplication Write SetShowApplication;
    property StartUp : TCHRegRun read FRegRunClass Write FRegRunClass;
    property Flash : TCHFlash read FTitleClass Write FTitleClass;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHApplication]);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHintClass := TCHHint.Create(self);
  FInstanceClass := TCHInstance.Create(self);
  FRestartClass := TCHRestart.Create(self);
  FTitleClass := TCHFlash.Create(self);
  FRegRunClass := TCHRegRun.Create(self);

  FForm := GetParentForm(TControl(AOwner));
  FShowApplication := True;

  Fkerneldll := LoadLibrary('kernel32.dll');
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHApplication.Destroy;
begin
  FHintClass.Free;
  FInstanceClass.Free;
  FRestartClass.Free;
  FTitleClass.Free;
  FRegRunClass.Free;
  if FCreateInstance then
    CloseHandle(hMutex);
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHApplication.Loaded;
begin
  inherited Loaded;
  FAppTitle := Application.Title;

  // Instance
  hMutex := CreateMutex(NIL, False, PChar(ExtractFileName(Application.ExeName)));
  if hMutex <> 0 then
    FCreateInstance := True;

  // only one Instance
  if FInstanceClass.JustOneApp then
  begin
    if (hMutex = 0) OR (GetLastError = ERROR_ALREADY_EXISTS) then
    begin
      // should popup a messagebox
      if FInstanceClass.ShowMessage then
      begin
        Application.MessageBox(PChar(FInstanceClass.MessageText),
          PChar(FInstanceClass.MessageTitle), MB_OK + MB_DEFBUTTON1 + MB_APPLMODAL);
      end;

      Application.Terminate;
      FCreateInstance := False;
    end;
  end;

  // ShowApplication
  Application.ShowMainForm := FShowApplication;

  // Registry
  DoRegEdit;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHApplication.HideInTaskbar;
begin
  Showwindow(Application.Handle, SW_HIDE);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHApplication.ShowInTaskbar;
begin
  showwindow(Application.Handle, SW_RESTORE);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ only Win9x }
procedure TCHApplication.HideInTaskManager;
type
  TRegisterService = function(dwProcessId, dwType : dword): Integer; stdcall;
var
  RegisterServiceProcess : TRegisterService;
begin
  if (csDesigning in ComponentState) then
    exit;
  @RegisterServiceProcess := GetProcAddress(Fkerneldll, 'RegisterServiceProcess');
  if @RegisterServiceProcess = nil then
    exit;
  RegisterServiceProcess(GetCurrentProcessID, 1);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ only Win9x }
procedure TCHApplication.ShowInTaskManager;
type
  TRegisterService = function(dwProcessId, dwType : dword): Integer; stdcall;
var
  RegisterServiceProcess : TRegisterService;
begin
  if (csDesigning in ComponentState) then
    exit;
  @RegisterServiceProcess := GetProcAddress(Fkerneldll, 'RegisterServiceProcess');
  if @RegisterServiceProcess = nil then
    exit;
  RegisterServiceProcess(GetCurrentProcessID, 0);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHApplication.DoRestart;
begin
  If (csDesigning in ComponentState) then
    exit
  else
  begin
    if (FRestartClass.Mode = rsTimer) and (FRestartClass.Timer = True) and
      (FRestartClass.TimerInterval > 0) or (FRestartClass.Mode = rsFunction) and
      (FRestartClass.Timer = False) then
    begin
      // should popup a messagebox
      if FRestartClass.ShowMessage then
      begin
        Application.MessageBox(PChar(FRestartClass.MessageText),
            PChar(FRestartClass.MessageTitle), MB_OK+MB_DEFBUTTON1+MB_APPLMODAL);
      end;

      // do Restart
      if FRestartClass.OnlyClose = False then
        //WinExec(AppPath, SW_SHOW);
        ShellExecute(0, 'open', PChar(Application.ExeName), PChar(FRestartClass.FCommand), nil, SW_ShowNormal);

      Application.Terminate;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHApplication.CanAppClose(Form : TCustomForm; IconClose, MenuClose,
  MenuActive: Boolean);
var
  hSysMenu : THandle;
begin
  if IconClose = False then
    SetClassLong(Form.Handle, gcl_style, cs_noclose);

  if MenuClose = False then
  begin
    hSysMenu := GetSystemMenu(Form.handle, false);
    EnableMenuItem(hSysMenu, sc_close, mf_grayed);
  end;

  if MenuActive = False then
  begin
    hSysMenu := GetSystemMenu(Form.handle,false);
    RemoveMenu(hSysMenu, sc_close, mf_bycommand);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHApplication.DoRegEdit;
var
  Reg : TRegistry;
  sRegKey : string;
const
  sReg = '\SOFTWARE\Microsoft\Windows\CurrentVersion';
begin
  Reg := nil;

  with FRegRunClass do
  begin
    if FActive = True then
    begin
      if not(csDesigning in FOwner.ComponentState) then
      begin
        try
          Reg := TRegistry.Create;

          // User (Local, User)
          case FRegKey of
            rkLocalMachine : Reg.RootKey := HKEY_LOCAL_MACHINE;
            rkCurrentUser: Reg.RootKey := HKEY_CURRENT_USER;
          end;

          // Run Key
          case FStartUpKey of
            ruRUN : sRegKey := sReg + '\RUN';
            ruRUNONCE : sRegKey := sReg + '\RUNONCE';
            ruRUNSERVICES : sRegKey := sReg + '\RUNSERVICES';
          end;

          Reg.OpenKey(sRegKey, True);

          // write
          if FRegEvent = reAdd then
          begin
            try
              Reg.WriteString(Application.Title, Application.ExeName);
            except
            end;
          end
          // delete
          else if FRegEvent = reDelete then
          begin
            try
              Reg.DeleteValue(Application.Title);
            except
            end;
          end;
        finally
          Reg.CloseKey;
          Reg.Free;
        end;
      end;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHApplication.DoBlink(Sender: TObject);
begin
  if not(csDesigning in ComponentState) then
    FlashWindow(Application.Handle, true);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHApplication.SetShowApplication(const Value: Boolean);
begin
  if FShowApplication <> Value then
    FShowApplication := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHApplication.SetActionExecute(Value: TActionEvent);
begin
  FActionExecute := Value;
  If not (csDesigning in ComponentState) then
    Application.OnActionExecute := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHApplication.SetActionUpdate(Value: TActionEvent);
begin
  FActionUpdate := Value;
  If not (csDesigning in ComponentState) then
    Application.OnActionUpdate := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHApplication.SetActivate(Value: TNotifyEvent);
begin
  FActivate := Value;
  If not (csDesigning in ComponentState) then
    Application.OnActivate := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHApplication.SetDeactivate(Value: TNotifyEvent);
begin
  FDeactivate := Value;
  If not (csDesigning in ComponentState) then
    Application.OnDeactivate := Value
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHApplication.SetException(Value: TExceptionEvent);
begin
  FException := Value;
  If not (csDesigning in ComponentState) then
    Application.OnException := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHApplication.SetHelp(Value : THelpEvent);
begin
  FHelp := Value;
  If not (csDesigning in ComponentState) then
    Application.OnHelp := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHApplication.SetHint(Value : TNotifyEvent);
begin
  FHint := Value;
  If not (csDesigning in ComponentState) then
    Application.OnHint := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHApplication.SetIdle(Value : TIdleEvent);
begin
  FIdle := Value;
  If not (csDesigning in ComponentState) then
    Application.OnIdle := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHApplication.SetMessage(Value : TMessageEvent);
begin
  FMessage := Value;
  If not (csDesigning in ComponentState) then
    Application.OnMessage := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHApplication.SetMinimize(Value : TNotifyEvent);
begin
  FMinimize := Value;
  If not (csDesigning in ComponentState) then
    Application.OnMinimize := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHApplication.SetRestore(Value : TNotifyEvent);
begin
  FRestore := Value;
  If not (csDesigning in ComponentState) then
    Application.OnRestore := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHApplication.SetShortcut(Value : TShortCutEvent);
begin
  FShortcut := Value;
  If not (csDesigning in ComponentState) then
    Application.OnShortcut := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHApplication.SetShowHint(Value : TShowHintEvent);
begin
  FShowHint := Value;
  If not (csDesigning in ComponentState) then
    Application.OnShowHint := Value;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ TCHHint }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }

constructor TCHHint.Create(AOwner : TCHApplication);
begin
  inherited Create;
  FOwner := AOwner;

  FHint := '';
  FHintColor := clInfoBk;
  FHidePause := 2500;
  FHintPause := 500;
  FShort := 0;
  FHintShort := True;
  FShow := True;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHHint.SetHintColor(const Value : TColor);
begin
  if FHintColor <> Value then
  begin
    FHintColor := Value;
    If not (csDesigning in FOwner.ComponentState) then
      Application.HintColor := Value;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHHint.SetHidePause(const Value : Cardinal);
begin
  if FHidePause <> Value then
  begin
    FHidePause := Value;
    If not (csDesigning in FOwner.ComponentState) then
      Application.HintHidePause := Value;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHHint.SetHint(const Value : string);
begin
  if FHint <> Value then
  begin
    FHint := Value;
    If not (csDesigning in FOwner.ComponentState) then
      Application.Hint := Value;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHHint.SetHintPause(const Value : Cardinal);
begin
  if FHintPause <> Value then
  begin
    FHintPause := Value;
    If not (csDesigning in FOwner.ComponentState) then
      Application.HintPause := Value;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHHint.SetHintShort(const Value : boolean);
begin
  if FHintShort <> Value then
  begin
    FHintShort := Value;
    If not (csDesigning in FOwner.ComponentState) then
      Application.HintShortCuts := Value;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHHint.SetShort(const Value : Cardinal);
begin
  if FShort <> Value then
  begin
    FShort := Value;
    If not (csDesigning in FOwner.ComponentState) then
      Application.HintShortPause := Value;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHHint.SetShow(const Value : boolean);
begin
  if FShow <> Value then
  begin
    FShow := Value;
    If not (csDesigning in FOwner.ComponentState) then
      Application.ShowHint := Value;
  end;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ TCHInstance }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }

constructor TCHInstance.Create(AOwner: TCHApplication);
begin
  inherited Create;
  FOwner := AOwner;

  FJustOneApp := False;
  FMessageText := '';
  FShowMessage := False;
  FShowMessage := False;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHInstance.SetJustOneApp(const Value: Boolean);
begin
  if FJustOneApp <> Value then
    FJustOneApp := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHInstance.SetMessageText(const Value: string);
begin
  if FMessageText <> Value then
    FMessageText := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHInstance.SetMessageTitle(const Value: string);
begin
  if FMessageTitle <> Value then
    FMessageTitle := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHInstance.SetShowMessage(const Value: Boolean);
begin
  if FShowMessage <> Value then
    FShowMessage := Value;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ TCHRestart }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }

constructor TCHRestart.Create(AOwner: TCHApplication);
begin
  inherited Create;
  FOwner := AOwner;

  FMessageText := '';
  FMessageTitle := '';
  FRestartMode := rsTimer;
  FOnlyClose := False;
  FShowMessage := False;
  FTimerInterval := 5000;
  FRestartTimer := False;

  FTimer := TTimer.Create(AOwner);
  FTimer.Interval := FTimerInterval;
  FTimer.Enabled := FRestartTimer;
  FTimer.OnTimer := FOwner.DoRestart;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHRestart.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHRestart.SetCommand(const Value: string);
begin
  FCommand := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHRestart.SetMessageText(const Value: string);
begin
  if FMessageText <> Value then
    FMessageText := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHRestart.SetMessageTitle(const Value: string);
begin
  if FMessageTitle <> Value then
    FMessageTitle := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHRestart.SetOnlyClose(const Value: Boolean);
begin
  if FOnlyClose <> Value then
    FOnlyClose := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHRestart.SetRestartMode(const Value: TRestart);
begin
  if FRestartMode <> Value then
  begin
    FRestartMode := Value;
    if FRestartMode = rsFunction then
      FRestartTimer := False;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHRestart.SetRestartTimer(const Value: Boolean);
begin
  if FRestartTimer <> Value then
  begin
    FRestartTimer := Value;
    if (FRestartTimer = True) and (FRestartMode = rsFunction) then
      FRestartTimer := False;
  end;

  FTimer.Enabled := FRestartTimer;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHRestart.SetShowMessage(const Value: Boolean);
begin
  if FShowMessage <> Value then
    FShowMessage := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHRestart.SetTimerInterval(const Value: Cardinal);
begin
  if FTimerInterval <> Value then
  begin
    FTimerInterval := Value;
  end;

  FTimer.Interval := FTimerInterval;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ TCHTitle }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }

constructor TCHFlash.Create(AOwner: TCHApplication);
begin
  inherited Create;
  FOwner := AOwner;

  FBlinkInterval := 500;
  FTitleBlink := False;

  FTimer := TTimer.Create(AOwner);
  FTimer.Interval := FBlinkInterval;
  FTimer.Enabled := FTitleBlink;
  FTimer.OnTimer := FOwner.DoBlink;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHFlash.Destroy;
begin
  FlashWindow(Application.Handle, False);
  FTimer.Free;
  inherited;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFlash.SetBlinkInterval(const Value: Cardinal);
begin
  if FBlinkInterval <> Value then
  begin
    FBlinkInterval := Value;
  end;

  FTimer.Interval := FBlinkInterval;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFlash.SetTitleBlink(const Value: Boolean);
begin
  if FTitleBlink <> Value then
  begin
    FTitleBlink := Value;
  end;

  FTimer.Enabled := FTitleBlink;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ TCHRegRun }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }

constructor TCHRegRun.Create(AOwner: TCHApplication);
begin
  inherited Create;
  FOwner := AOwner;

  FActive := False;
  FStartUpKey := ruRUN;
  FRegKey := rkLocalMachine;
  FRegEvent := reAdd;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHRegRun.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    FOwner.DoRegEdit;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHRegRun.SetRegEvent(const Value: TRegEvent);
begin
  if FRegEvent <> Value then
  begin
    FRegEvent := Value;
    FOwner.DoRegEdit;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHRegRun.SetRegKey(const Value: TRegKey);
begin
  if FRegKey <> Value then
    FRegKey := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHRegRun.SetStartUpKey(const Value: TStartUpKey);
begin
  if FStartUpKey <> Value then
    FStartUpKey := Value;
end;

end.
