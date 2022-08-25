unit uMain;

{$IFDEF CONDITIONALEXPRESSIONS}  
  {$WARN SYMBOL_PLATFORM   OFF}
  {$WARN SYMBOL_LIBRARY    OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$IF COMPILERVERSION >= 23}
    {$DEFINE UNITSCOPING}
  {$IFEND}
{$ENDIF} 

interface

uses
{$IFDEF UNITSCOPING}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, 
  Vcl.Graphics, Vcl.Controls, Vcl.Dialogs, Vcl.ExtCtrls,
{$ELSE} 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Dialogs, ExtCtrls,
{$ENDIF}  
  DDWindows, DDSvcMgr, DDDbt, DDSvcUtils;

type
  TDDService1 = class(TDDService)
    procedure DDServiceDeviceEvent(Sender: TDDService; EventType: Integer;
      EventData: TDDIntPtr; var MsgResult: Integer);
    procedure DDServiceNetBindChange(Sender: TDDService;
      EventType: Integer);
    procedure DDServiceParamChange(Sender: TDDService);
    procedure DDServicePowerEvent(Sender: TDDService; EventType: Integer;
      EventData: TDDIntPtr; var MsgResult: Integer);
    procedure DDServicePreShutdown(Sender: TDDService);
    procedure DDServiceSessionChange(Sender: TDDService; EventType,
      SessionID: Integer);
    procedure DDServiceShutdown(Sender: TDDService);
    procedure DDServiceStart(Sender: TDDService; var Started: Boolean);
    procedure DDServiceStop(Sender: TDDService; var Stopped: Boolean);
    procedure DDServiceConsoleEvent(Sender: TDDService; CtrlCode: Integer;
      var Handled: Boolean);
    procedure DDServicePause(Sender: TDDService; var Paused: Boolean);
    procedure DDServiceContinue(Sender: TDDService;
      var Continued: Boolean);
    procedure DDServiceAfterInstall(Sender: TDDService);
    procedure DDServiceAfterUninstall(Sender: TDDService);
    procedure DDServiceRunException(Sender: TObject; E: Exception;
      var LogDefaultErrMsg, CanAbort: Boolean);
  private
    Timer1 : TTimer;
    DeviceHandle : Pointer;
    procedure Cleanup;
    procedure Timer1Timer(Sender: TObject);
  protected
    procedure WndProc(var Msg: TMessage); override;
  public
    function GetServiceController: TServiceController; override;
    function GetServiceControllerEx: TServiceControllerEx; override;
    function GetConsoleCtrlHandler: TServiceConsoleCtrlHandler; override;
  end;

var
  DDService1: TDDService1;

implementation

{$R *.DFM}

const
  { Note: Some Message-IDs in the range WM_USER + are used by TDDService!     }
  { So define your custom messages beginning with WM_USER_DDSERVICE.          }
  WM_CUSTOM_1 = WM_USER_DDSERVICE + 0;

{///////////////////////////////////////////////////////////////////////////}
procedure ServiceController(CtrlCode: DWORD); stdcall;
begin
  DDService1.Controller(CtrlCode)
end;

function TDDService1.GetServiceController: TServiceController;
begin
  Result := ServiceController
end;

function ServiceControllerEx(CtrlCode, EventType: DWORD;
  EventData, Context: Pointer): DWORD; stdcall;
begin
  Result := DDService1.ControllerEx(CtrlCode, EventType, EventData, Context)
end;

function TDDService1.GetServiceControllerEx: TServiceControllerEx;
begin
  Result := ServiceControllerEx
end;

function ServiceConsoleCtrlHandler(Ctrl: DWord): Bool; stdcall;
begin
  Result := DDService1.ConsoleCtrlHandler(Ctrl)
end;

function TDDService1.GetConsoleCtrlHandler: TServiceConsoleCtrlHandler;
begin
  Result := ServiceConsoleCtrlHandler
end;

{///////////////////////////////////////////////////////////////////////////}
procedure TDDService1.DDServiceStart(Sender: TDDService;
  var Started: Boolean);
const
  GUID_DEVINTERFACE_USB: TGUID = '{A5DCBF10-6530-11D2-901F-00C04FB951ED}';
var
  dbi : TDevBroadcastDeviceInterface;
begin
  LogMessage('Starting', EVENTLOG_INFORMATION_TYPE);

  // We may register additional device notifications, specify the
  // service _window as recipient. W2K and better
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5) then
  begin
    {if Sender.ServiceWindow = 0 then
    Sender.OnDeviceEvent := DDServiceDeviceEvent; // creates the window if not yet done}
    ZeroMemory(@dbi, SizeOf(dbi));
    dbi.dbcc_size := SizeOf(dbi);
    dbi.dbcc_devicetype := DBT_DEVTYP_DEVICEINTERFACE;
    dbi.dbcc_classguid := GUID_DEVINTERFACE_USB;
    DeviceHandle := RegisterDeviceNotification(Sender.ServiceWindow, @dbi,
                                               DEVICE_NOTIFY_WINDOW_HANDLE);
    if DeviceHandle = nil then
      RaiseLastWin32Error;
  end;
  // TTimer is not thread safe, do not use it in service threads in the real life!
  Timer1 := TTimer.Create(nil);
  Timer1.OnTimer := Timer1Timer;
  
  LogMessage('Started', EVENTLOG_INFORMATION_TYPE);

  // Test our custom message
  PostMessage(ServiceWindow, WM_CUSTOM_1, 0, 0);
end;


{///////////////////////////////////////////////////////////////////////////}
procedure TDDService1.Cleanup;
begin
  Timer1.Free;
  if DeviceHandle <> nil then
  begin
    UnregisterDeviceNotification(DeviceHandle);
    DeviceHandle := nil;
  end;
end;


{///////////////////////////////////////////////////////////////////////////}
procedure TDDService1.Timer1Timer(Sender: TObject);
begin
  Beep; // doesn't work in Vista :(
end;


{///////////////////////////////////////////////////////////////////////////}
{ We may want to override service thread's WndProc.                         }
{ Note: Some Message-IDs in the range WM_USER + are used by TDDService!     }
{ So define your custom messages beginning with WM_USER_DDSERVICE.          }
procedure TDDService1.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_CUSTOM_1 then
    LogMessage('WM_CUSTOM_1 received', EVENTLOG_INFORMATION_TYPE)
  else
    inherited WndProc(Msg);
end;


{///////////////////////////////////////////////////////////////////////////}
procedure TDDService1.DDServiceStop(Sender: TDDService;
  var Stopped: Boolean);
begin
  LogMessage('Stopped', EVENTLOG_INFORMATION_TYPE);
  Cleanup;
end;


{///////////////////////////////////////////////////////////////////////////}
procedure TDDService1.DDServiceShutdown(Sender: TDDService);
begin
  LogMessage('Shutdown', EVENTLOG_INFORMATION_TYPE);
  Cleanup; 
end;


{///////////////////////////////////////////////////////////////////////////}
procedure TDDService1.DDServicePause(Sender: TDDService;
  var Paused: Boolean);
begin
  Timer1.Enabled := False;
  //raise exception.Create('Test'); //Caught in DDServiceRunException
  LogMessage('Paused', EVENTLOG_INFORMATION_TYPE);
end;


{///////////////////////////////////////////////////////////////////////////}
procedure TDDService1.DDServiceContinue(Sender: TDDService;
  var Continued: Boolean);
begin
  Timer1.Enabled := True;
  LogMessage('Continued', EVENTLOG_INFORMATION_TYPE);
end;


{///////////////////////////////////////////////////////////////////////////}
{ This event is triggered when an unhandled exception occured after the     }
{ service has been started.                                                 }
procedure TDDService1.DDServiceRunException(Sender: TObject;
  E: Exception; var LogDefaultErrMsg, CanAbort: Boolean);
begin
  // Service will be aborted.
  CanAbort := True;
  // Set a service specific error code to enable service recovery actions
  ErrCode := 1;
end;


{///////////////////////////////////////////////////////////////////////////}
{ Vista only}
procedure TDDService1.DDServicePreShutdown(Sender: TDDService);
var
  Timeout : Integer;
begin
  LogMessage('PreShutdown', EVENTLOG_INFORMATION_TYPE);
  Cleanup;
  { Test PreShutdown timeout - Vista will show the shutdown screen! }
  Timeout := 1000 * 60 * 1;
  while TimeOut > 0 do
  begin
    Sleep(1000);
    Dec(TimeOut, 1000);
    ReportStatus;
  end;
end;


{///////////////////////////////////////////////////////////////////////////}
{ W2K - Never saw the OS sending this control code, a SCP however may use it}
procedure TDDService1.DDServiceParamChange(Sender: TDDService);
begin
  LogMessage('ParamChange', EVENTLOG_INFORMATION_TYPE)
end;


{///////////////////////////////////////////////////////////////////////////}
{ W2K - Never saw the OS sending this control code, a SCP however may use it}
procedure TDDService1.DDServiceNetBindChange(Sender: TDDService;
  EventType: Integer);
begin
  LogMessage('NetBindChange Type: #' + IntToStr(EventType),
             EVENTLOG_INFORMATION_TYPE);
end;


{///////////////////////////////////////////////////////////////////////////}
{ XP }
procedure TDDService1.DDServiceSessionChange(Sender: TDDService;
  EventType, SessionID: Integer);
begin
  LogMessage('SessionChange - EventType = ' + IntToStr(EventType) +
             ' SessionID = ' + IntToStr(SessionID), EVENTLOG_INFORMATION_TYPE);
end;


{///////////////////////////////////////////////////////////////////////////}
{ W2K }
procedure TDDService1.DDServicePowerEvent(Sender: TDDService; EventType: Integer;
  EventData: TDDIntPtr; var MsgResult: Integer);
begin
  LogMessage('PowerEvent - EventType: $' + IntToHex(EventType, 8),
             EVENTLOG_INFORMATION_TYPE);
  { Note that Vista doesn't send PBT_APMQUERYSUSPEND but PBT_APMSUSPEND at once }
  if EventType = PBT_APMQUERYSUSPEND then
    MsgResult := BROADCAST_QUERY_DENY;  // Deny the request
end;


{///////////////////////////////////////////////////////////////////////////}
{ W2K }
procedure TDDService1.DDServiceDeviceEvent(Sender: TDDService; EventType: Integer;
  EventData: TDDIntPtr; var MsgResult: Integer);
begin
  LogMessage('DeviceEvent - EventType: $' + IntToHex(EventType, 8),
             EVENTLOG_WARNING_TYPE);
end;


{///////////////////////////////////////////////////////////////////////////}
{ Not triggered in Vista }
procedure TDDService1.DDServiceConsoleEvent(Sender: TDDService;
  CtrlCode: Integer; var Handled: Boolean);
begin
  LogMessage('Console Ctrl: ' + IntToStr(CtrlCode) + ' - ThreadID: #' +
             IntToStr(GetCurrentThreadID), EVENTLOG_INFORMATION_TYPE)
end;


{///////////////////////////////////////////////////////////////////////////}
procedure TDDService1.DDServiceAfterInstall(Sender: TDDService);
begin
  RegisterEventLogSource(Sender.ServiceName, ParamStr(0));
end;


{///////////////////////////////////////////////////////////////////////////}
procedure TDDService1.DDServiceAfterUninstall(Sender: TDDService);
begin
  UnRegisterEventLogSource(Sender.ServiceName);
end;


{///////////////////////////////////////////////////////////////////////////}
end.
