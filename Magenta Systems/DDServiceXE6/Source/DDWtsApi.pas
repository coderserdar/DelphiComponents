{*********************************************************************
*
* WTSAPI32.H
*
*   Windows Terminal Server public APIs
*   
* Portions created by Microsoft are Copyright (c) 1997-2001 Microsoft Corporation
**********************************************************************
Pascal translation by Arno Garrels <arno.garrels@gmx.de>
Not yet complete!
**********************************************************************}

unit DDWtsApi;

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}
{$I DDCompilers.inc}

interface

{$IFDEF COMPILER16_UP}
uses Winapi.Windows;
{$ELSE}
uses Windows;
{$ENDIF}

//===================================================================
//==   Defines
//===================================================================
const
//
//  Specifies the current server
//
  {$EXTERNALSYM WTS_CURRENT_SERVER}
  WTS_CURRENT_SERVER        = THandle(0);
  {$EXTERNALSYM WTS_CURRENT_SERVER_HANDLE}
  WTS_CURRENT_SERVER_HANDLE = THandle(0);
  {$EXTERNALSYM WTS_CURRENT_SERVER_NAME}
  WTS_CURRENT_SERVER_NAME   = '';

//
//  Specifies the current session (SessionId)
//
  {$EXTERNALSYM WTS_CURRENT_SESSION}
  WTS_CURRENT_SESSION       = DWORD(-1);

  { Flags for Console Notification }
  {$EXTERNALSYM NOTIFY_FOR_ALL_SESSIONS}
  NOTIFY_FOR_ALL_SESSIONS   =  1;
  {$EXTERNALSYM NOTIFY_FOR_THIS_SESSION}
  NOTIFY_FOR_THIS_SESSION   =  0;

  { codes passed in WPARAM for WM_WTSSESSION_CHANGE }
  {$EXTERNALSYM WTS_SESSION_REMOTE_CONTROL}
  WTS_SESSION_REMOTE_CONTROL = $9;

type
  {$EXTERNALSYM _WTS_CONNECTSTATE_CLASS}
  _WTS_CONNECTSTATE_CLASS = (
    WTSActive,              // User logged on to WinStation
    WTSConnected,           // WinStation connected to client
    WTSConnectQuery,        // In the process of connecting to client
    WTSShadow,              // Shadowing another WinStation
    WTSDisconnected,        // WinStation logged on without client
    WTSIdle,                // Waiting for client to connect
    WTSListen,              // WinStation is listening for connection
    WTSReset,               // WinStation is being reset
    WTSDown,                // WinStation is down due to error
    WTSInit);               // WinStation in initialization
  {$EXTERNALSYM WTS_CONNECTSTATE_CLASS}
  WTS_CONNECTSTATE_CLASS = _WTS_CONNECTSTATE_CLASS;
  TWtsConnectStateClass = WTS_CONNECTSTATE_CLASS;

  PWTS_SESSION_INFOW = ^WTS_SESSION_INFOW;
  {$EXTERNALSYM PWTS_SESSION_INFOW}
  _WTS_SESSION_INFOW = record
    SessionId       : DWORD;                  // session id
    pWinStationName : PWideChar;              // name of WinStation this session is connected to
    State           : WTS_CONNECTSTATE_CLASS; // connection state (see enum)
  end;
  {$EXTERNALSYM _WTS_SESSION_INFOW}
  WTS_SESSION_INFOW = _WTS_SESSION_INFOW;
  {$EXTERNALSYM WTS_SESSION_INFOW}
  TWtsSessionInfoW = WTS_SESSION_INFOW;
  PWtsSessionInfoW = PWTS_SESSION_INFOW;

  PWTS_SESSION_INFOA = ^WTS_SESSION_INFOA;
  {$EXTERNALSYM PWTS_SESSION_INFOA}
  _WTS_SESSION_INFOA = record
    SessionId       : DWORD;                  // session id
    pWinStationName : PAnsiChar;              // name of WinStation this session is connected to
    State           : WTS_CONNECTSTATE_CLASS; // connection state (see enum)
  end;
  {$EXTERNALSYM _WTS_SESSION_INFOA}
  WTS_SESSION_INFOA = _WTS_SESSION_INFOA;
  {$EXTERNALSYM WTS_SESSION_INFOA}
  TWtsSessionInfoA = WTS_SESSION_INFOA;
  PWtsSessionInfoA = PWTS_SESSION_INFOA;

{$IFDEF UNICODE}
  WTS_SESSION_INFO = WTS_SESSION_INFOW;
  PWTS_SESSION_INFO = PWTS_SESSION_INFOW;
  TWtsSessionInfo = TWtsSessionInfoW;
  PWtsSessionInfo = PWtsSessionInfoW;
{$ELSE}
  WTS_SESSION_INFO = WTS_SESSION_INFOA;
  PWTS_SESSION_INFO = PWTS_SESSION_INFOA;
  TWtsSessionInfo = TWtsSessionInfoA;
  PWtsSessionInfo = PWtsSessionInfoA;
{$ENDIF UNICODE}

  PWTS_PROCESS_INFOW = ^WTS_PROCESS_INFOW;
  {$EXTERNALSYM PWTS_PROCESS_INFOW}
  _WTS_PROCESS_INFOW = record
    SessionId     : DWORD;     // session id
    ProcessId     : DWORD;     // process id
    pProcessName  : PWideChar; // name of process
    pUserSid      : PSID;      // user's SID
  end;
  {$EXTERNALSYM _WTS_PROCESS_INFOW}
  WTS_PROCESS_INFOW = _WTS_PROCESS_INFOW;
  {$EXTERNALSYM WTS_PROCESS_INFOW}
  TWtsProcessInfoW = WTS_PROCESS_INFOW;
  PWtsProcessInfoW = PWTS_PROCESS_INFOW;

  PWTS_PROCESS_INFOA = ^WTS_PROCESS_INFOA;
  {$EXTERNALSYM PWTS_PROCESS_INFOA}
  _WTS_PROCESS_INFOA = record
    SessionId     : DWORD;      // session id
    ProcessId     : DWORD;      // process id
    pProcessName  : PAnsiChar;  // name of process
    pUserSid      : PSID;       // user's SID
  end;
  {$EXTERNALSYM _WTS_PROCESS_INFOA}
  WTS_PROCESS_INFOA = _WTS_PROCESS_INFOA;
  {$EXTERNALSYM WTS_PROCESS_INFOA}
  TWtsProcessInfoA = WTS_PROCESS_INFOA;
  PWtsProcessInfoA = PWTS_PROCESS_INFOA;

{$IFDEF UNICODE}
  WTS_PROCESS_INFO = WTS_PROCESS_INFOW;
  {$EXTERNALSYM WTS_PROCESS_INFO}
  PWTS_PROCESS_INFO = PWTS_PROCESS_INFOW;
  {$EXTERNALSYM PWTS_PROCESS_INFO}
  TWtsProcessInfo = TWtsProcessInfoW;
  PWtsProcessInfo = PWtsProcessInfoW;
{$ELSE}
  WTS_PROCESS_INFO = WTS_PROCESS_INFOA;
  {$EXTERNALSYM WTS_PROCESS_INFO}
  PWTS_PROCESS_INFO = PWTS_PROCESS_INFOA;
  {$EXTERNALSYM PWTS_PROCESS_INFO}
  TWtsProcessInfo = TWtsProcessInfoA;
  PWtsProcessInfo = PWtsProcessInfoA;
{$ENDIF UNICODE}


//=====================================================================
//==   WTS_INFO_CLASS - WTSQuerySessionInformation
//==    (See additional typedefs for more info on structures)
//=====================================================================

const
  {$EXTERNALSYM WTS_PROTOCOL_TYPE_CONSOLE}
  WTS_PROTOCOL_TYPE_CONSOLE       = 0; // Console
  {$EXTERNALSYM WTS_PROTOCOL_TYPE_ICA}
  WTS_PROTOCOL_TYPE_ICA           = 1; // ICA Protocol
  {$EXTERNALSYM WTS_PROTOCOL_TYPE_RDP}
  WTS_PROTOCOL_TYPE_RDP           = 2; // RDP Protocol

type
  {$EXTERNALSYM _WTS_INFO_CLASS}
  _WTS_INFO_CLASS = (
    WTSInitialProgram,
    WTSApplicationName,
    WTSWorkingDirectory,
    WTSOEMId,
    WTSSessionId,
    WTSUserName,
    WTSWinStationName,
    WTSDomainName,
    WTSConnectState,
    WTSClientBuildNumber,
    WTSClientName,
    WTSClientDirectory,
    WTSClientProductId,
    WTSClientHardwareId,
    WTSClientAddress,
    WTSClientDisplay,
    WTSClientProtocolType,
    WTSIdleTime,
    WTSLogonTime,
    WTSIncomingBytes,
    WTSOutgoingBytes,
    WTSIncomingFrames,
    WTSOutgoingFrames,
    WTSClientInfo,
    WTSSessionInfo);
  {$EXTERNALSYM WTS_INFO_CLASS}
  WTS_INFO_CLASS = _WTS_INFO_CLASS;
  TWtsInfoClass = WTS_INFO_CLASS;

  {$EXTERNALSYM WTSQueryUserToken}
  function WTSQueryUserToken(SessionId: Cardinal; var hToken: THandle): BOOL;
  {$EXTERNALSYM WTSFreeMemory}
  procedure WTSFreeMemory(pMemory: Pointer);
  {$EXTERNALSYM WTSEnumerateProcesses}
  function WTSEnumerateProcesses(hServer: THandle; Reserved: DWORD; Version: DWORD; out pProcessInfo: PWtsProcessInfo; out Count: DWORD): BOOL;
  {$EXTERNALSYM WTSEnumerateProcessesW}
  function WTSEnumerateProcessesW(hServer: THandle; Reserved: DWORD; Version: DWORD; out pProcessInfo: PWtsProcessInfoW; out Count: DWORD): BOOL;
  {$EXTERNALSYM WTSEnumerateProcessesA}
  function WTSEnumerateProcessesA(hServer: THandle; Reserved: DWORD; Version: DWORD; out pProcessInfo: PWtsProcessInfoA; out Count: DWORD): BOOL;
  {$EXTERNALSYM WTSEnumerateSessions}
  function WTSEnumerateSessions(hServer: THandle; Reserved: DWord; Version: DWord; out pSessionInfo: PWtsSessionInfo; out Count: DWord): BOOL;
  {$EXTERNALSYM WTSEnumerateSessionsA}
  function WTSEnumerateSessionsA(hServer: THandle; Reserved: DWord; Version: DWord; out pSessionInfo: PWtsSessionInfoA; out Count: DWord): BOOL;
  {$EXTERNALSYM WTSEnumerateSessionsW}
  function WTSEnumerateSessionsW(hServer: THandle; Reserved: DWord; Version: DWord; out pSessionInfo: PWtsSessionInfoW; out Count: DWord): BOOL;
  {$EXTERNALSYM WTSSendMessage}
  function WTSSendMessage(hServer: THandle; SessionId: DWord; Title: PChar; TitleLength: DWord; pMessage: PChar; MessageLength: DWord; Style: DWord; Timeout: DWord; var Response: DWord; Wait: Boolean): BOOL;
  {$EXTERNALSYM WTSSendMessageA}
  function WTSSendMessageA(hServer: THandle; SessionId: DWord; Title: PAnsiChar; TitleLength: DWord; pMessage: PAnsiChar; MessageLength: DWord; Style: DWord; Timeout: DWord; var Response: DWord; Wait: Boolean): BOOL;
  {$EXTERNALSYM WTSSendMessageW}
  function WTSSendMessageW(hServer: THandle; SessionId: DWord; Title: PWideChar; TitleLength: DWord; pMessage: PWideChar; MessageLength: DWord; Style: DWord; Timeout: DWord; var Response: DWord; Wait: Boolean): BOOL;
  {$EXTERNALSYM WTSQuerySessionInformation}
  function WTSQuerySessionInformation(hServer: THandle; SessionId: DWORD; WTSInfoClass: TWTSInfoClass; out pBuffer: Pointer; out BytesReturned: DWord): BOOL;
  {$EXTERNALSYM WTSQuerySessionInformationW}
  function WTSQuerySessionInformationW(hServer: THandle; SessionId: DWORD; WTSInfoClass: TWTSInfoClass; out pBuffer: Pointer; out BytesReturned: DWord): BOOL;
  {$EXTERNALSYM WTSQuerySessionInformationA}
  function WTSQuerySessionInformationA(hServer: THandle; SessionId: DWORD; WTSInfoClass: TWTSInfoClass; out pBuffer: Pointer; out BytesReturned: DWord): BOOL;
  {$EXTERNALSYM WTSRegisterSessionNotification}
  function WTSRegisterSessionNotification(hWnd: HWND; dwFlags: DWORD): BOOL;
  {$EXTERNALSYM WTSRegisterSessionNotificationEx}
  function WTSRegisterSessionNotificationEx(hServer: THandle; hWnd: HWND; dwFlags: DWORD): BOOL;
  {$EXTERNALSYM WTSUnRegisterSessionNotification}
  function WTSUnRegisterSessionNotification(hWnd: HWND): BOOL;
  {$EXTERNALSYM WTSUnRegisterSessionNotificationEx}
  function WTSUnRegisterSessionNotificationEx(hServer: THandle; hWnd: HWND): BOOL;

  function CheckWtsAPILoaded: Boolean;
  
implementation

type
  TWTSQueryUserToken            = function(SessionId: Cardinal; var hToken: THandle): BOOL; stdcall;
  TWTSFreeMemory                = procedure(pMemory: Pointer); stdcall;
  TWTSEnumerateProcessesW       = function(hServer: THandle; Reserved: DWORD; Version: DWORD; out pProcessInfo: PWtsProcessInfoW; out Count: DWORD): BOOL; stdcall;
  TWTSEnumerateProcessesA       = function(hServer: THandle; Reserved: DWORD; Version: DWORD; out pProcessInfo: PWtsProcessInfoA; out Count: DWORD): BOOL; stdcall;
  TWTSEnumerateSessionsA        = function(hServer: THandle; Reserved: DWord; Version: DWord; out pSessionInfo: PWtsSessionInfoA; out Count: DWord): BOOL; stdcall;
  TWTSEnumerateSessionsW        = function(hServer: THandle; Reserved: DWord; Version: DWord; out pSessionInfo: PWtsSessionInfoW; out Count: DWord): BOOL; stdcall;
  TWTSSendMessageA              = function(hServer: THandle; SessionId: DWord; Title: PAnsiChar; TitleLength: DWord; pMessage: PAnsiChar; MessageLength: DWord; Style: DWord; Timeout: DWord; var Response: DWord; Wait: Boolean): BOOL; stdcall;
  TWTSSendMessageW              = function(hServer: THandle; SessionId: DWord; Title: PWideChar; TitleLength: DWord; pMessage: PWideChar; MessageLength: DWord; Style: DWord; Timeout: DWord; var Response: DWord; Wait: Boolean): BOOL; stdcall;
  TWTSQuerySessionInformationW  = function(hServer: THandle; SessionId: DWORD; WTSInfoClass: TWTSInfoClass; out pBuffer: Pointer; out BytesReturned: DWord): BOOL; stdcall;
  TWTSQuerySessionInformationA  = function(hServer: THandle; SessionId: DWORD; WTSInfoClass: TWTSInfoClass; out pBuffer: Pointer; out BytesReturned: DWord): BOOL; stdcall;
  TWTSRegisterSessionNotification = function (hWnd: HWND; dwFlags: DWORD): BOOL; stdcall;
  TWTSRegisterSessionNotificationEx = function (hServer: THandle; hWnd: HWND; dwFlags: DWORD): BOOL; stdcall;
  TWTSUnRegisterSessionNotification = function (hWnd: HWND): BOOL; stdcall;
  TWTSUnRegisterSessionNotificationEx = function (hServer: THandle; hWnd: HWND): BOOL; stdcall;
{$IFDEF UNICODE}
  TWTSEnumerateProcesses      = TWTSEnumerateProcessesW;
  TWTSEnumerateSessions       = TWTSEnumerateSessionsW;
  TWTSSendMessage             = TWTSSendMessageW;
  TWTSQuerySessionInformation = TWTSQuerySessionInformationW;
{$ELSE}
  TWTSEnumerateProcesses      = TWTSEnumerateProcessesA;
  TWTSEnumerateSessions       = TWTSEnumerateSessionsA;
  TWTSSendMessage             = TWTSSendMessageA;
  TWTSQuerySessionInformation = TWTSQuerySessionInformationA;
{$ENDIF}

var
  hWtsAPI : THandle = 0;

  _WTSQueryUserToken                  : TWTSQueryUserToken;
  _WTSFreeMemory                      : TWTSFreeMemory;
  _WTSEnumerateProcesses              : TWTSEnumerateProcesses;
  _WTSEnumerateProcessesW             : TWTSEnumerateProcessesW;
  _WTSEnumerateProcessesA             : TWTSEnumerateProcessesA;
  _WTSEnumerateSessions               : TWTSEnumerateSessions;
  _WTSEnumerateSessionsW              : TWTSEnumerateSessionsW;
  _WTSEnumerateSessionsA              : TWTSEnumerateSessionsA;
  _WTSSendMessage                     : TWTSSendMessage;
  _WTSSendMessageW                    : TWTSSendMessageW;
  _WTSSendMessageA                    : TWTSSendMessageA;
  _WTSQuerySessionInformation         : TWTSQuerySessionInformation;
  _WTSQuerySessionInformationW        : TWTSQuerySessionInformationW;
  _WTSQuerySessionInformationA        : TWTSQuerySessionInformationA;
  _WTSRegisterSessionNotification     : TWTSRegisterSessionNotification;
  _WTSRegisterSessionNotificationEx   : TWTSRegisterSessionNotificationEx;
  _WTSUnRegisterSessionNotification   : TWTSUnRegisterSessionNotification;
  _WTSUnRegisterSessionNotificationEx : TWTSUnRegisterSessionNotificationEx;


function CheckWtsAPILoaded: Boolean;
const
{$IFDEF UNICODE}
    ApiType = 'W';
{$ELSE}
    ApiType = 'A';
{$ENDIF}
begin
  if hWtsAPI = 0 then
  begin
    hWtsAPI := LoadLibrary('WtsApi32.dll');
    if hWtsAPI = 0 then
    begin
      Result := False;
      Exit;
    end;
    @_WTSQueryUserToken                   := GetProcAddress(hWtsAPI, 'WTSQueryUserToken');
    @_WTSFreeMemory                       := GetProcAddress(hWtsAPI, 'WTSFreeMemory');
    @_WTSEnumerateProcesses               := GetProcAddress(hWtsAPI, 'WTSEnumerateProcesses' + ApiType);
    @_WTSEnumerateProcessesW              := GetProcAddress(hWtsAPI, 'WTSEnumerateProcessesW');
    @_WTSEnumerateProcessesA              := GetProcAddress(hWtsAPI, 'WTSEnumerateProcessesW');
    @_WTSEnumerateSessions                := GetProcAddress(hWtsAPI, 'WTSEnumerateSessions' + ApiType);
    @_WTSEnumerateSessionsW               := GetProcAddress(hWtsAPI, 'WTSEnumerateSessionsW');
    @_WTSEnumerateSessionsA               := GetProcAddress(hWtsAPI, 'WTSEnumerateSessionsA');
    @_WTSSendMessage                      := GetProcAddress(hWtsAPI, 'WTSSendMessage' + ApiType);
    @_WTSSendMessageW                     := GetProcAddress(hWtsAPI, 'WTSSendMessageW');
    @_WTSSendMessageA                     := GetProcAddress(hWtsAPI, 'WTSSendMessageA');
    @_WTSQuerySessionInformation          := GetProcAddress(hWtsAPI, 'WTSQuerySessionInformation' + ApiType);
    @_WTSQuerySessionInformationW         := GetProcAddress(hWtsAPI, 'WTSQuerySessionInformationW');
    @_WTSQuerySessionInformationA         := GetProcAddress(hWtsAPI, 'WTSQuerySessionInformationA');
    @_WTSRegisterSessionNotification      := GetProcAddress(hWtsAPI, 'WTSRegisterSessionNotification');
    @_WTSRegisterSessionNotificationEx    := GetProcAddress(hWtsAPI, 'WTSRegisterSessionNotificationEx');
    @_WTSUnRegisterSessionNotification    := GetProcAddress(hWtsAPI, 'WTSUnRegisterSessionNotification');
    @_WTSUnRegisterSessionNotificationEx  := GetProcAddress(hWtsAPI, 'WTSUnRegisterSessionNotificationEx');
  end;
  Result := True;
end;

function WTSQueryUserToken(SessionId: Cardinal; var hToken: THandle): BOOL;
begin
  if CheckWtsAPILoaded then
    Result := _WTSQueryUserToken(SessionId, hToken)
  else Result := False;
end;

procedure WTSFreeMemory(pMemory: Pointer);
begin
  if CheckWtsAPILoaded then
    _WTSFreeMemory(pMemory);
end;

function WTSEnumerateProcesses(hServer: THandle; Reserved: DWORD; Version: DWORD; out pProcessInfo: PWtsProcessInfo; out Count: DWORD): BOOL;
begin
  if CheckWtsAPILoaded then
    Result := _WTSEnumerateProcesses(hServer, Reserved, Version, pProcessInfo, Count)
  else Result := False;
end;

function WTSEnumerateProcessesW(hServer: THandle; Reserved: DWORD; Version: DWORD; out pProcessInfo: PWtsProcessInfoW; out Count: DWORD): BOOL;
begin
  if CheckWtsAPILoaded then
    Result := _WTSEnumerateProcessesW(hServer, Reserved, Version, pProcessInfo, Count)
  else Result := False;
end;

function WTSEnumerateProcessesA(hServer: THandle; Reserved: DWORD; Version: DWORD; out pProcessInfo: PWtsProcessInfoA; out Count: DWORD): BOOL;
begin
  if CheckWtsAPILoaded then
    Result := _WTSEnumerateProcessesA(hServer, Reserved, Version, pProcessInfo, Count)
  else Result := False;
end;

function WTSEnumerateSessions(hServer: THandle; Reserved: DWord; Version: DWord; out pSessionInfo: PWtsSessionInfo; out Count: DWord): BOOL;
begin
  if CheckWtsAPILoaded then
    Result := _WTSEnumerateSessions(hServer, Reserved, Version, pSessionInfo, Count)
  else Result := False;
end;

function WTSEnumerateSessionsA(hServer: THandle; Reserved: DWord; Version: DWord; out pSessionInfo: PWtsSessionInfoA; out Count: DWord): BOOL;
begin
  if CheckWtsAPILoaded then
    Result := _WTSEnumerateSessionsA(hServer, Reserved, Version, pSessionInfo, Count)
  else Result := False;
end;

function WTSEnumerateSessionsW(hServer: THandle; Reserved: DWord; Version: DWord; out pSessionInfo: PWtsSessionInfoW; out Count: DWord): BOOL;
begin
  if CheckWtsAPILoaded then
    Result := _WTSEnumerateSessionsW(hServer, Reserved, Version, pSessionInfo, Count)
  else Result := False;
end;

function WTSSendMessage(hServer: THandle; SessionId: DWord; Title: PChar; TitleLength: DWord; pMessage: PChar; MessageLength: DWord; Style: DWord; Timeout: DWord; var Response: DWord; Wait: Boolean): BOOL;
begin
  if CheckWtsAPILoaded then
    Result := _WTSSendMessage(hServer, SessionId, Title, TitleLength, pMessage, MessageLength, Style, Timeout, Response, Wait)
  else Result := False;
end;

function WTSSendMessageA(hServer: THandle; SessionId: DWord; Title: PAnsiChar; TitleLength: DWord; pMessage: PAnsiChar; MessageLength: DWord; Style: DWord; Timeout: DWord; var Response: DWord; Wait: Boolean): BOOL;
begin
  if CheckWtsAPILoaded then
    Result := _WTSSendMessageA(hServer, SessionId, Title, TitleLength, pMessage, MessageLength, Style, Timeout, Response, Wait)
  else Result := False;
end;

function WTSSendMessageW(hServer: THandle; SessionId: DWord; Title: PWideChar; TitleLength: DWord; pMessage: PWideChar; MessageLength: DWord; Style: DWord; Timeout: DWord; var Response: DWord; Wait: Boolean): BOOL;
begin
  if CheckWtsAPILoaded then
    Result := _WTSSendMessageW(hServer, SessionId, Title, TitleLength, pMessage, MessageLength, Style, Timeout, Response, Wait)
  else Result := False;
end;

function WTSQuerySessionInformation(hServer: THandle; SessionId: DWORD; WTSInfoClass: TWTSInfoClass; out pBuffer: Pointer; out BytesReturned: DWord): BOOL;
begin
  if CheckWtsAPILoaded then
    Result := _WTSQuerySessionInformation(hServer, SessionId, WTSInfoClass, pBuffer, BytesReturned)
  else Result := False;
end;

function WTSQuerySessionInformationW(hServer: THandle; SessionId: DWORD; WTSInfoClass: TWTSInfoClass; out pBuffer: Pointer; out BytesReturned: DWord): BOOL;
begin
  if CheckWtsAPILoaded then
    Result := _WTSQuerySessionInformationW(hServer, SessionId, WTSInfoClass, pBuffer, BytesReturned)
  else Result := False;
end;

function WTSQuerySessionInformationA(hServer: THandle; SessionId: DWORD; WTSInfoClass: TWTSInfoClass; out pBuffer: Pointer; out BytesReturned: DWord): BOOL;
begin
  if CheckWtsAPILoaded then
    Result := _WTSQuerySessionInformationA(hServer, SessionId, WTSInfoClass, pBuffer, BytesReturned)
  else Result := False;
end;

function WTSRegisterSessionNotification(hWnd: HWND; dwFlags: DWORD): BOOL;
begin
  if CheckWtsAPILoaded then
    Result := _WTSRegisterSessionNotification(hWnd, dwFlags)
  else
    Result := False;
end;

function WTSRegisterSessionNotificationEx(hServer: THandle; hWnd: HWND; dwFlags: DWORD): BOOL;
begin
  if CheckWtsAPILoaded then
    Result := _WTSRegisterSessionNotificationEx(hServer, hWnd, dwFlags)
  else
    Result := False;
end;

function WTSUnRegisterSessionNotification(hWnd: HWND): BOOL;
begin
  if CheckWtsAPILoaded then
    Result := _WTSUnRegisterSessionNotification(hWnd)
  else
    Result := False;
end;

function WTSUnRegisterSessionNotificationEx(hServer: THandle; hWnd: HWND): BOOL;
begin
  if CheckWtsAPILoaded then
    Result := _WTSUnRegisterSessionNotificationEx(hServer, hWnd)
  else
    Result := False;
end;

end.
