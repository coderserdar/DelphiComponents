unit StartProcMain;

{$B-}                                 { Enable partial boolean evaluation   }
{$T-}                                 { Untyped pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long strings                    }
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
  Vcl.Graphics, Vcl.Controls, Vcl.Dialogs, Vcl.ExtCtrls, System.Contnrs,
{$ELSE}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Dialogs, ExtCtrls,
{$ENDIF}
  DDWindows, DDSvcMgr, DDDbt, DDWtsApi, DDSvcUtils;

type
  TDDService1 = class(TDDService)
    procedure DDServiceCreate(Sender: TObject);
    procedure DDServiceSessionChange(Sender: TDDService; EventType,
      SessionID: Integer);
    procedure DDServiceAfterInstall(Sender: TDDService);
    procedure DDServiceAfterUninstall(Sender: TDDService);
    procedure DDServiceStart(Sender: TDDService; var Started: Boolean);
  private
    FAppTostart : String;
    procedure InitStart;
    function  StartProcInSession(const CmdLine: String; SessionID: DWord;
     hUserToken: THandle; AsInvoker: Boolean): Boolean;
  public
    function GetServiceController: TServiceController; override;
    function GetServiceControllerEx: TServiceControllerEx; override;
    function GetConsoleCtrlHandler: TServiceConsoleCtrlHandler; override;
  end;

var
  DDService1 : TDDService1;
  
implementation

{$R *.DFM}                                                                   

{/////////////////////////////////////////////////////////////////////////////}
procedure ServiceController(CtrlCode: DWORD); stdcall;
begin
  DDService1.Controller(CtrlCode);
end;

function TDDService1.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

function ServiceControllerEx(CtrlCode, EventType: DWORD;
  EventData, Context: Pointer): DWORD; stdcall;
begin                                                                        
  Result := DDService1.ControllerEx(CtrlCode, EventType, EventData, Context);
end;                                                                         

function TDDService1.GetServiceControllerEx: TServiceControllerEx;           
begin                                                                        
  Result := ServiceControllerEx;
end;                                                                         

function ServiceConsoleCtrlHandler(Ctrl: DWord): Bool; stdcall;              
begin                                                                        
  Result := DDService1.ConsoleCtrlHandler(Ctrl);
end;                                                                         

function TDDService1.GetConsoleCtrlHandler: TServiceConsoleCtrlHandler;
begin                                                                        
  Result := ServiceConsoleCtrlHandler;
end;


{/////////////////////////////////////////////////////////////////////////////}
function InitializeDefaultSecurityDesriptor(SD: PSecurityDescriptor): Boolean;
begin
  if InitializeSecurityDescriptor(SD, SECURITY_DESCRIPTOR_REVISION) then
    if SetSecurityDescriptorDacl(SD, TRUE, nil, FALSE) then
    begin
      Result := TRUE;
      Exit;
    end;
  Result := FALSE;
end;


{/////////////////////////////////////////////////////////////////////////////}
function EnablePrivilege(const Privilege: String) : Boolean;
var
  NewState: TTokenPrivileges;
  hToken: THandle;
  OldState: PTokenPrivileges;
  RetLen: PDWord;
  Luid: TLargeInteger;
begin
  Result := False;
  if OpenProcessToken(GetCurrentProcess,
                      TOKEN_ADJUST_PRIVILEGES, hToken) then
  try
    if not LookupPrivilegeValue(nil, PChar(Privilege) , Luid) then
      Exit;
    NewState.PrivilegeCount := 1;
    NewState.Privileges[0].Luid := Luid;
    NewState.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
    OldState := nil;
    RetLen := nil;
    if AdjustTokenPrivileges(hToken, False, NewState, SizeOf(TTokenPrivileges),
                             OldState^, RetLen^ ) then
      Result := True;
  finally
    CloseHandle(hToken);
  end;
end;


{/////////////////////////////////////////////////////////////////////////////}
function WtsGetProcessID(const LowerCaseProcessName: String;
  SessionID : DWord): DWORD;
var
  PProcessInfo, P : PWtsProcessInfo;
  Count: DWord;
  I : Integer;
begin
  Result := 0;
  if WtsEnumerateProcesses(WTS_CURRENT_SERVER_HANDLE, 0, 1, PProcessInfo, Count) then
  begin
    P := PProcessInfo;
    for I := 1 to Count do
    begin
      if (P^.SessionId = SessionID) and
        (AnsiLowerCase(P^.pProcessName) = LowerCaseProcessName) then
      begin
        Result := P^.ProcessId;
        Break;
      end;
      Inc(P);
    end;
    WTSFreeMemory(PProcessInfo);
  end;
end;


{/////////////////////////////////////////////////////////////////////////////}
function WtsProcessExists(const LowerCaseProcessName: String;
  SessionID : DWord): Boolean;
begin
  Result := WtsGetProcessID(LowerCaseProcessName, SessionID) > 0;
end;


{/////////////////////////////////////////////////////////////////////////////}
function WtsGetSessionState(SessionID: Integer): Integer;
var
  Buf : Pointer;
  BytesReturned : DWord;
begin
  Result := -1;
  if WTSQuerySessionInformation(WTS_CURRENT_SERVER_HANDLE, SessionID,
                                WTSConnectState, Buf, BytesReturned) then
  begin
    try
      if (BytesReturned = 4) then
        Result := PInteger(Buf)^;
    finally
      WTSFreeMemory(Buf);
    end;
  end;
end;


{/////////////////////////////////////////////////////////////////////////////}
function GetWinDir: String;
var
  Len: Cardinal;
begin
  Len := GetWindowsDirectory(nil, 0);
  if Len > 0 then
  begin
    SetLength(Result, Len);
    GetWindowsDirectory(PChar(Result), Len);
    SetLength(Result, StrLen(PChar(Result)));
  end
  else
    Result := '';
end;


{/////////////////////////////////////////////////////////////////////////////}
function GetSystemDir: String;
var
  Len: Cardinal;
begin
  Len := GetSystemDirectory(nil, 0);
  if Len > 0 then
  begin
    SetLength(Result, Len);
    GetSystemDirectory(PChar(Result), Len);
    SetLength(Result, StrLen(PChar(Result)));
  end
  else
    Result := '';
end;


{/////////////////////////////////////////////////////////////////////////////}
procedure TDDService1.InitStart;
var
  PSessionInfo : PWtsSessionInfo;
  Count        : DWord;
  P            : PWtsSessionInfo;
  I            : Integer;
begin
  if not WtsEnumerateSessions(WTS_CURRENT_SERVER_HANDLE, 0, 1,
                              PSessionInfo, Count) then
  begin
      LogMessage('WtsEnumerateSessions ' + SysErrorMessage(GetLastError),
                 EVENTLOG_WARNING_TYPE);
      Exit;
  end;

  try
    P := PSessionInfo;
    for I := 0 to Count - 1 do
    begin
      if (P^.State = WTSActive) then
        DDServiceSessionChange(Self, WTS_CONSOLE_CONNECT, P^.SessionId);
      Inc(P);
    end;
  finally
    WtsFreeMemory(PSessionInfo);
  end;
end;


{/////////////////////////////////////////////////////////////////////////////}
procedure TDDService1.DDServiceAfterInstall(Sender: TDDService);
begin
  RegisterEventLogSource(Sender.ServiceName, ParamStr(0));
end;


{/////////////////////////////////////////////////////////////////////////////}
procedure TDDService1.DDServiceAfterUninstall(Sender: TDDService);
begin
  UnRegisterEventLogSource(Sender.ServiceName);
end;


{/////////////////////////////////////////////////////////////////////////////}
procedure TDDService1.DDServiceCreate(Sender: TObject);
begin
  ExOptions := ExOptions + [eoForceServiceThreadWindow];
end;


{/////////////////////////////////////////////////////////////////////////////}
procedure TDDService1.DDServiceStart(Sender: TDDService; var Started: Boolean);
begin
  EnablePrivilege(SE_TCB_NAME); // Enabled by default in XP
  FAppTostart := IncludeTrailingBackSlash(GetWinDir) + 'notepad.exe';
  { Start our process in all sessions which own a UserToken }
  InitStart;
end;


{/////////////////////////////////////////////////////////////////////////////}
function TDDService1.StartProcInSession(const CmdLine: String;
  SessionID: DWord; hUserToken: THandle; AsInvoker: Boolean): Boolean;
const
  MaxRetries = 15;
var
  SI             : TStartupInfo;
  PI             : TProcessInformation;
  ProcCreated    : Boolean;
  S              : String;
  I              : Integer;
  LastErr        : DWord;
  EnvBlock       : Pointer;
  hProcessToken  : THandle;
  hDupToken      : THandle;
begin
  Result   := False;
  EnvBlock := nil;
  if AsInvoker then
  begin
    if not OpenProcessToken(GetCurrentProcess, MAXIMUM_ALLOWED,
                            hProcessToken) then
    begin
      LogMessage('OpenProcessToken ' + SysErrorMessage(GetLastError),
                 EVENTLOG_WARNING_TYPE);
      Exit;
    end;
    if not DuplicateTokenEx(hProcessToken, MAXIMUM_ALLOWED, nil,
            SECURITY_MAX_IMPERSONATION_LEVEL, TokenPrimary, hDupToken) then
    begin
      LogMessage('DuplicateTokenEx ' + SysErrorMessage(GetLastError),
                 EVENTLOG_WARNING_TYPE);
      CloseHandle(hProcessToken);
      Exit;
    end;
    CloseHandle(hProcessToken);
    if not DDWindows.SetTokenInformation(hDupToken, TokenSessionId,
                                         @SessionID, SizeOf(DWord)) then
    begin
      LogMessage('SetTokenInformation ' + SysErrorMessage(GetLastError),
                 EVENTLOG_WARNING_TYPE);
      CloseHandle(hUserToken);
      Exit;
    end;
    CloseHandle(hUserToken);
    hUserToken := hDupToken;
  end
  else begin
    if not CreateEnvironmentBlock(EnvBlock, hUserToken, FALSE) then // UNICODE!
    begin
      LogMessage('CreateEnvironmentBlock ' + SysErrorMessage(GetLastError),
               EVENTLOG_WARNING_TYPE);
      Exit;
    end;
  end;

  { Create the process }
  S := '"' + CmdLine + '"';
  FillChar(SI, SizeOf(SI), #0);
  FillChar(PI, SizeOf(PI), #0);
  SI.cb := SizeOf(SI);
  SI.lpDesktop := PChar('WinSta0\Default');
  SI.dwFlags := STARTF_USESHOWWINDOW;
  SI.wShowWindow := SW_SHOWDEFAULT;

  for I := 1 to MaxRetries do
  begin
    ProcCreated := CreateProcessAsUser(
                            hUserToken,
                            nil,
                            PChar(S),    // pointer to command line string
                            nil,         // pointer to process security attributes
                            nil,         // pointer to thread security attributes
                            FALSE,       // handle inheritance
                            CREATE_UNICODE_ENVIRONMENT,  // creation flags
                            EnvBlock,    // pointer to new environment block
                            nil,         // pointer to current directory name
                            SI,          // STARTUPINFO
                            PI);         // PROCESS_INFORMATION

    if ProcCreated then
    begin
      CloseHandle(PI.hThread);
      CloseHandle(PI.hProcess);
      Result := TRUE;
      Break;
    end
    else begin
      LastErr := GetLastError;
      { This seems to be a bug in XP with CreateProcessAsUserA() }
      if (LastErr = ERROR_PIPE_NOT_CONNECTED) and (I < MaxRetries) and
         (not Terminated) then
      begin
        Sleep(1000);
        if not Terminated then
          Continue;
      end;
      LogMessage('CreateProcessAsUser Error #' + IntToStr(LastErr) + ' ' +
                 SysErrorMessage(LastErr), EVENTLOG_ERROR_TYPE);
      Result := FALSE;
      Break;
    end;
  end;
  if EnvBlock <> nil then
    DestroyEnvironmentBlock(EnvBlock);
  if Result then
      LogMessage('CreateProcessAsUser OK ID #' + IntToStr(SessionID),
                 EVENTLOG_INFORMATION_TYPE)
end;


{/////////////////////////////////////////////////////////////////////////////}
procedure TDDService1.DDServiceSessionChange(Sender: TDDService; EventType,
  SessionID: Integer);

  function SessionEventStr: String;
  begin
    case EventType of
      WTS_CONSOLE_CONNECT         : Result := 'WTS_CONSOLE_CONNECT';
      WTS_CONSOLE_DISCONNECT      : Result := 'WTS_CONSOLE_DISCONNECT';
      WTS_REMOTE_CONNECT          : Result := 'WTS_REMOTE_CONNECT';
      WTS_REMOTE_DISCONNECT       : Result := 'WTS_REMOTE_DISCONNECT';
      WTS_SESSION_LOGON           : Result := 'WTS_SESSION_LOGON';
      WTS_SESSION_LOGOFF          : Result := 'WTS_SESSION_LOGOFF';
      WTS_SESSION_LOCK            : Result := 'WTS_SESSION_LOCK';
      WTS_SESSION_UNLOCK          : Result := 'WTS_SESSION_UNLOCK';
      WTS_SESSION_REMOTE_CONTROL  : Result := 'WTS_SESSION_REMOTE_CONTROL';
    else
       Result := 'Unknown event #' + IntToStr(EventType);
    end;
  end;

var
  hUserToken: THandle;
begin
  LogMessage(SessionEventStr + ' ID #' + IntToStr(SessionID),
      EVENTLOG_INFORMATION_TYPE);
  if (EventType = WTS_SESSION_LOGON) then
  begin
    if not WTSQueryUserToken(SessionID, hUserToken) then
    begin
        LogMessage('WTSQueryUserToken: ' + SysErrorMessage(GetLastError));
        Exit;
    end;
    try
      StartProcInSession(FAppTostart, DWord(SessionID), hUserToken, FALSE);
    finally
      CloseHandle(hUserToken);
    end;
  end
  { In Vista WTSQueryUserToken fails on SessionID 0 so no problem }
  else if ((EventType = WTS_CONSOLE_CONNECT) or (EventType = WTS_REMOTE_CONNECT)) then
  begin
    if not WTSQueryUserToken(SessionID, hUserToken) then
    begin
      LogMessage('WTSQueryUserToken: ' + SysErrorMessage(GetLastError));
      Exit;
    end;
    try
      StartProcInSession(FAppTostart, DWord(SessionID), hUserToken, FALSE);
    finally
      CloseHandle(hUserToken);
    end;
  end;
end;


{/////////////////////////////////////////////////////////////////////////////}



end.
