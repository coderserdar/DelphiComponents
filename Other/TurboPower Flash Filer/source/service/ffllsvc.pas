{*********************************************************}
{* FlashFiler service base classes                       *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit ffllsvc;

interface

uses
  Windows,
  {$IFNDEF DCC4OrLater}
  usvctype,
  {$ENDIF}
  {$IFDEF DCC6OrLater}
  Variants,
  {$ENDIF}
  WinSvc;


type
  PArgArray = ^Pchar;
    { Represents a pointer to an array of pointers where each element in the
      array points to a null-terminated string. }

  procedure Execute;
    { This is the main entry point for the executable.  If no command line
      parameters are present, this method assumes it was started by the
      the Service Control Manager (SCM).

      If command line parameters are present, this method evaluates and carries
      out the parameters.  Each parameter may be prefixed by a '-' or '/'.
      Valid parameters:

        D - Enable debug logging
        I - Install the service
        U - Uninstall the service
     }

  { The following procedures are entrypoints for the Service Control Manager
    (SCM). }

  procedure ControlHandler(CtrlCode : DWORD); stdcall;
    { Called when the SCM sends a control code to the service (e.g., pause,
      continue, stop).  This method must carry out the requested operation.
      Windows SDK topics:
        - Handler
        - Writing a Control Handler function

      Parameters:
        CtrlCode - The control code. }


  procedure ServiceMain(NumArgs : DWORD; Args : PArgArray); stdcall;
    { Called when the SCM wants to execute the service.
      Windows SDK topics:
        - ServiceMain
        - Writing a ServiceMain function

      Parameters:
        numArgs - The number of arguments in argArray.
        argArray - Pointer to an array of pointers that point to null-terminated
          argument strings.  The first argument is the name of the service.
          Subsequent arguments are strings passed to the service by the process
          that initiated the service. }


implementation

{$R FFSVCMSG.RES}

uses
  Registry,
  SysUtils,
  FFLLBase,
  FFLLComm,
  ffllprot,                                                            {!!.02}
{Begin !!.13}
  UffEgMgr,
  ffsrjour,
  uFFSRJrn;
{End !!.13}

const
  ffc_ConflictingParm : string = 'Conflicting parameter: %s';
  ffc_DisplayName     : string = 'FlashFiler Service %5.4f %s';
  ffc_EngMgrStartup   : string = 'Error during Engine Mgr startup: %s';
  ffc_EngMgrShutdown  : string = 'Error during Engine Mgr shutdown: %s';
  ffc_ServiceName     : string = 'FlashFilerService';
  ffc_DebugMode   = 'D';
  ffc_GenError    = 1000;
  ffc_GenDebug    = 1000;                                              {!!.02}
  ffc_Install     = 'I';
  ffc_InvalidParm = 'Invalid parameter: %s';
  ffc_Uninstall   = 'U';

  { Event logging constants }
  ffc_EventLogKey = '\System\CurrentControlSet\Services\EventLog\Application';
  { The registry key in which the event log sources are defined. }
  ffc_EventMsgFile = 'EventMessageFile';
  { The name of the data value specifying the event message file.  NT looks
    here to map an event log message ID to an actual message. }
  ffc_KeyOpenFail = 'Could not open registry key %s';
  ffc_RegisterSrc = 'Could not register event log source %s';
  ffc_TypesSupported = 'TypesSupported';
  { The name of the data value specifying which types of event log messages
    are supported by this source.  This example assumes you will need to log
    informational messages only. }

  ffc_JournalStateEventTypes : array[TJournalState] of Word = (
    EVENTLOG_WARNING_TYPE,
    EVENTLOG_ERROR_TYPE,
    EVENTLOG_ERROR_TYPE,
    EVENTLOG_ERROR_TYPE );

var
  ffDebugMode : boolean;
  ffEngineMgr : TffEngineManager;
  ffIsService : boolean;
  ffSvcEvent  : TffEvent;
  ffSvcStatus : TServiceStatus;
    { For info on this data structure, see topic SERVICE_STATUS in the Windows
      SDK. }
  ffSvcStatusHandle : SERVICE_STATUS_HANDLE;
  ffLog : System.Text;

{===Utility Routines=================================================}
procedure WriteLog(aMsg : string; args : array of const);
{Begin !!.10}
const
  LogName = 'FFSrvice.log';
begin
  System.Assign(ffLog, LogName);
  if FileExists(LogName) then
    System.Append(ffLog)
  else
    System.Rewrite(ffLog);
{End !!.10}
  WriteLn(ffLog, format(aMsg, args));
  System.Close(ffLog);
end;
{--------}
procedure WriteEvent(const aType : Word; const aEventID : DWORD;
                     const aString : string);
var
  LogHandle : THandle;
  PMsg : pointer;
begin

  { Open the event log. }
  LogHandle := RegisterEventSource(nil, PChar(ffc_ServiceName));
  if LogHandle <> NULL then
    try
      if aString = '' then
        ReportEvent(LogHandle, aType, 0, aEventID, nil, 0, 0, nil, nil)
      else begin
        PMsg := PChar(aString);
        ReportEvent(LogHandle, aType, 0, aEventID, nil, 1, 0, @PMsg,
                    nil);
      end;
    finally
      { Close the event log. }
      CloseEventLog(LogHandle);
    end;
end;
{--------}
procedure ReportErrorFmt(const aMsg : string; args : array of const);
begin
  if ffIsService then
    WriteEvent(EVENTLOG_ERROR_TYPE, ffc_GenError, format(aMsg, args))
  else
    WriteLog(aMsg, args);
end;
{--------}
procedure ReportLastError;
var
  Buffer : array[0..255] of char;
  Len : DWORD;
  Status : DWORD;
begin
  Status := GetLastError;
  Len := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, Status, 0,
                       Buffer, SizeOf(Buffer), nil);
  if Len > 0 then
    if ffIsService then
      WriteEvent(EVENTLOG_ERROR_TYPE, ffc_GenError, Buffer)
    else
      WriteLog(Buffer,['']);
end;
{--------}
procedure Debug(const aMsg : string);
begin

  if not ffDebugMode then exit;

  if ffIsService then
    WriteEvent(EVENTLOG_INFORMATION_TYPE, ffc_GenDebug, aMsg)
  else
    WriteLog(aMsg, ['']);
end;
{--------}
procedure DebugFmt(const aMsg : string; args : array of const);
begin
  if not ffDebugMode then exit;

  if ffIsService then
    WriteEvent(EVENTLOG_INFORMATION_TYPE, ffc_GenDebug, format(aMsg, args))
  else
    WriteLog(aMsg, args);
end;
{--------}
procedure NotifySCM(const WaitHint : DWORD);
begin
  ffSvcStatus.dwWaitHint := WaitHint;
  if not SetServiceStatus(ffSvcStatusHandle, ffSvcStatus) then
    ReportLastError;
end;
{--------}
procedure InitEngineMgr(anEngineMgr : TffEngineManager);
begin
  { The following is hard-coded but it is the best we can do
    right now.  The best solution is to have persistent storage
    of transport properties. }
  with anEngineMgr.ServerEngine.Configuration.GeneralInfo^ do begin

    DebugFmt('Loading configuration from %s',
             [anEngineMgr.ServerEngine.ConfigDir]);

{Begin !!.02}
    anEngineMgr.ServerEngine.BufferManager.MaxRAM := giMaxRAM;
    {..ports}
    FFSetTCPPort(giTCPPort);
    FFSetUDPPortServer(giUDPPortSr);
    FFSetUDPPortClient(giUDPPortCl);
    FFSetIPXSocketServer(giIPXSocketSr);
    FFSetIPXSocketClient(giIPXSocketCl);
    FFSetSPXSocket(giSPXSocket);
    {..keepalive stuff}
    ffc_LastMsgInterval := giLastMsgInterval;
    ffc_KeepAliveInterval := giKAInterval;
    ffc_KeepAliveRetries := giKARetries;
{End !!.02}

    anEngineMgr.SUPTransport.Enabled := giSingleUser;
    if giSingleUser then begin
      anEngineMgr.SUPTransport.BeginUpdate;
      try
        Debug('Starting SUP transport');
        anEngineMgr.SUPTransport.ServerName := giServerName;
        anEngineMgr.SUPTransport.Mode := fftmListen;
        anEngineMgr.SUPTransport.EndUpdate;
        Debug('SUP transport started');
      except
        anEngineMgr.SUPTransport.CancelUpdate;
      end;
    end;

    anEngineMgr.IPXSPXTransport.Enabled := giIPXSPX;
    if giIPXSPX then begin
      anEngineMgr.IPXSPXTransport.BeginUpdate;
      try
        Debug('Starting IPXSPX transport');
        anEngineMgr.IPXSPXTransport.ServerName := giServerName;
        anEngineMgr.IPXSPXTransport.RespondToBroadcasts := giIPXSPXLFB;
        anEngineMgr.IPXSPXTransport.Mode := fftmListen;
        anEngineMgr.IPXSPXTransport.EndUpdate;
        Debug('IPXSPX transport started');
      except
        anEngineMgr.IPXSPXTransport.CancelUpdate;
      end;
    end;

    anEngineMgr.TCPIPTransport.Enabled := giTCPIP;
    if giTCPIP then begin
      anEngineMgr.TCPIPTransport.BeginUpdate;
      try
        Debug('Starting TCPIP transport');
        anEngineMgr.TCPIPTransport.ServerName := giServerName;
        anEngineMgr.TCPIPTransport.RespondToBroadcasts := giTCPIPLFB;
        anEngineMgr.TcpIPTransport.Mode := fftmListen;
        anEngineMgr.TCPIPTransport.EndUpdate;
        ffc_TCPInterface := giTCPInterface;                            {!!.02}
        Debug('TCPIP transport started');
      except
        anEngineMgr.TCPIPTransport.CancelUpdate;
      end;
    end else
      Debug('TCPIP turned off in config file');
  end;
end;
{--------}
procedure Install;
const
  Prefix = 'Install.';
var
  BinaryPathAndName : array[0..MAX_PATH] of char;
  Registry  : TRegistry;
  scmHandle : SC_HANDLE;
  svcHandle : SC_HANDLE;
  Args      : PChar;
begin

  Debug(Prefix + 'OpenSCManager');

  { Open the service manager on the local computer. }
  scmHandle := OpenSCManager(nil, nil, SC_MANAGER_CREATE_SERVICE);

  { Did we get a handle? }
  if scmHandle = 0 then begin
    { No.  Assume we are in console mode & output the associated error
      message. }
    ReportLastError;
    exit;
  end;

  try
    Debug(Prefix + 'GetModuleFileName');

    { Get the path and filename of this executable. }
    GetModuleFileName(0, BinaryPathAndName, SizeOf(BinaryPathAndName));

    { Create the service. }
    Debug(Prefix + 'CreateService');
    svcHandle := CreateService(scmHandle,
                               PChar(ffc_ServiceName),
                               PChar(format(ffc_DisplayName,
                                            [ffVersionNumber/10000.0,
                                             ffSpecialString])),
                               SERVICE_ALL_ACCESS,
                               SERVICE_WIN32_OWN_PROCESS,
                               SERVICE_AUTO_START,
                               SERVICE_ERROR_NORMAL,
                               @BinaryPathAndName,
                               nil, nil, nil, nil, nil);
    { Did it work? }
    if svcHandle = 0 then
      ReportLastError
    else begin
      Args := nil;
      StartService(svcHandle, 0, Args);
      CloseServiceHandle(svcHandle);
    end;
  finally
    { Close the handle returned by the service manager. }
    CloseServiceHandle(scmHandle);
  end;

  { Register ourselves as a message source.
    Create registry entries in
    HKEY_LOCAL_MACHINE\System\CurrentControlSet\Services\EventLog\Application

    <application name> <- regarded as a "source" by Win NT
      EventMessageFile - The path for the event identifier message file
      TypesSupported - The type of events that may be logged

    NOTE: In order for the application to write to HKEY_LOCAL_MACHINE, the
      logged in user must have administrative privileges.
  }

  Debug(Prefix + 'TRegistry.Create');
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;

    Debug(Prefix + 'Registry.OpenKey');
    if Registry.OpenKey(ffc_EventLogKey, False) then
      try
        Debug(Prefix + 'Registry.CreateKey');
        Registry.CreateKey(ffc_ServiceName);
        Registry.OpenKey(ffc_EventLogKey + '\' + ffc_ServiceName, False);
        { Assumes the application contains the messages used for logging. }
        Registry.WriteString(ffc_EventMsgFile, BinaryPathAndName);
        Registry.WriteInteger(ffc_TypesSupported,
                              EVENTLOG_INFORMATION_TYPE or
                              EVENTLOG_ERROR_TYPE);
      except
        on E:Exception do
          ReportErrorFmt(ffc_RegisterSrc, [ffc_ServiceName]);
      end
    else begin
      ReportErrorFmt(ffc_KeyOpenFail, [ffc_EventLogKey]);
    end;
  finally
    Registry.Free;
  end;

  Debug(Prefix + 'End');

end;
{--------}
procedure Uninstall;
const
  Prefix= 'Uninstall';
var
  scmHandle : SC_HANDLE;
  svcHandle : SC_HANDLE;
begin

  Debug(Prefix + 'OpenSCManager');

  { Open the service manager on the local computer. }
  scmHandle := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);

  { Did we get a handle? }
  if scmHandle = 0 then begin
    { No.  Assume we are in console mode & output the associated error
      message. }
    ReportLastError;
    exit;
  end;

  try
    Debug(Prefix + 'OpenService');

    { Open the service. }
    svcHandle := OpenService(scmHandle, PChar(ffc_ServiceName),
                             SERVICE_ALL_ACCESS);

    { Did it open? }
    if svcHandle = 0 then
      { No. Report error and exit. }
      ReportLastError
    else begin
      Debug(Prefix + 'DeleteService');
      { Yes.  Stop and delete the service. }
      ControlService(svcHandle, SERVICE_CONTROL_STOP, ffSvcStatus);
      if not DeleteService(svcHandle) then
        ReportLastError;
    end;

  finally
    CloseServiceHandle(scmHandle);
  end;

  Debug(Prefix + 'End');
end;
{=====================================================================}

{===Main entry point==================================================}
procedure Execute;
var
  DispatchTable : array [0..1] of TServiceTableEntry;
  DoInstall : boolean;
  DoUninstall : boolean;
  Index : integer;
begin
  { Any parameters? }
  if ParamCount > 0 then begin
    { Yes.  Must be running from the command line. }
    ffIsService := False;
    DoInstall := False;
    DoUninstall := False;
    { Evaluate the command line parameters. }
    for Index := 1 to ParamCount do
      if (ParamStr(Index)[1] in ['-', '/']) and
         (Length(ParamStr(Index)) = 2) then
        { Which parameter? }
        case UpCase(ParamStr(Index)[2]) of
          ffc_DebugMode :
            ffDebugMode := True;
          ffc_Install :
            if DoUnInstall then
              ReportErrorFmt(ffc_ConflictingParm, [ParamStr(Index)])
            else
              DoInstall := True;
          ffc_Uninstall :
            if DoInstall then
              ReportErrorFmt(ffc_ConflictingParm, [ParamStr(Index)])
            else
              DoUnInstall := True;
        else
          ReportErrorFmt(ffc_InvalidParm, [ParamStr(Index)])
        end  { case }
      else
        ReportErrorFmt(ffc_InvalidParm, [ParamStr(Index)]);

    { Install or uninstall? }
    if DoInstall then
      Install
    else if DoUnInstall then
      UnInstall;

  end
  else
    { No. Must be running as service.  Register ServiceMain. }
    begin
      ffIsService := True;
      FillChar(DispatchTable[0], SizeOf(DispatchTable), 0);            {!!.02}
      DispatchTable[0].lpServiceName := PChar(ffc_ServiceName);
      DispatchTable[0].lpServiceProc := @ServiceMain;
//      DispatchTable[1].lpServiceName := nil;                         {Deleted !!.02}
//      DispatchTable[1].lpServiceProc := nil;                         {Deleted !!.02}
      if not StartServiceCtrlDispatcher(DispatchTable[0]) then
        ReportLastError;
    end
end;
{=====================================================================}

{===SCM Entry points==================================================}
procedure ControlHandler(CtrlCode : DWord);
const
  Prefix = 'ControlHandler.';
var                                                                    {!!.02}
  WaitHint : DWORD;                                                    {!!.02}
begin
  WaitHint := 0;                                                       {!!.02}
  case CtrlCode of
    SERVICE_CONTROL_STOP :
      begin
        Debug(Prefix + 'SERVICE_CONTROL_STOP');
        ffSvcStatus.dwCurrentState := SERVICE_STOP_PENDING;
        WaitHint := 11000;                                             {!!.06}
      end;
    SERVICE_CONTROL_SHUTDOWN :
      begin
        Debug(Prefix + 'SERVICE_CONTROL_SHUTDOWN');
        ffSvcStatus.dwCurrentState := SERVICE_STOP_PENDING;
        WaitHint := 11000;                                             {!!.06}
      end;
    SERVICE_CONTROL_PAUSE :
      begin
        Debug(Prefix + 'SERVICE_CONTROL_PAUSE');
        ffSvcStatus.dwCurrentState := SERVICE_PAUSE_PENDING;
        WaitHint := 11000;                                             {!!.06}
      end;
    SERVICE_CONTROL_CONTINUE :
      begin
        Debug(Prefix + 'SERVICE_CONTROL_CONTINUE');
        ffSvcStatus.dwCurrentState := SERVICE_CONTINUE_PENDING;
        WaitHint := 1000;                                              {!!.02}
      end;
    SERVICE_CONTROL_INTERROGATE :
      Debug(Prefix + 'SERVICE_CONTROL_INTERROGATE');
      { No state change needed. }
  end;  { case }

  { Send our status to the SCM. }
  Debug(Prefix + 'NotifySCM');
  NotifySCM(WaitHint);

  { Signal ServiceMain that a request has arrived. }
  Debug(Prefix + 'SignalEvent');
  ffSvcEvent.SignalEvent;

  Debug(Prefix + 'End');

end;
{--------}
procedure ServiceMain(NumArgs : DWORD; Args : PArgArray);
const
  Prefix = 'ServiceMain.';
var
  AnArg : PChar;
  Finished : boolean;
  Index : DWORD;
begin

  ffEngineMgr := nil;

  ffDebugMode := False;

  for Index := 1 to NumArgs do begin
    AnArg := Args^;
    if (StrLen(AnArg) = 2) and
       (AnArg[0] in ['/', '-']) and
       (Upcase(AnArg[1]) = ffc_DebugMode) then begin
      ffDebugMode := True;
      break;
    end;
    inc(Args);
  end;

  { Create the event that tells us the handler has received a control
    code. }
  ffSvcEvent := TffEvent.Create;

  { Initialize service status. }
  Debug(Prefix + 'InitServiceStatus');
  with ffSvcStatus do begin
    dwServiceType := SERVICE_WIN32_OWN_PROCESS;
    dwCurrentState := SERVICE_START_PENDING;
    dwControlsAccepted := SERVICE_ACCEPT_STOP or
                          SERVICE_ACCEPT_PAUSE_CONTINUE or
                          SERVICE_ACCEPT_SHUTDOWN;
    dwWin32ExitCode := NO_ERROR;
    dwServiceSpecificExitCode := 0;
    dwCheckPoint := 0;
    dwWaitHint := 0;
  end;

  { Are we running as a service? }
  if ffIsService then begin
    Debug(Prefix + 'RegisterServiceCtrlHandler');
    { Yes.  Register our handler for service control requests. }
    ffSvcStatusHandle := RegisterServiceCtrlHandler(PChar(ffc_ServiceName),
                                                    @ControlHandler);
    { Did it work? }
    if ffSvcStatusHandle = 0 then begin                                {!!.02}
      { No.  Report error. }
      ReportLastError;
      Exit;                                                            {!!.02}
    end                                                                {!!.02}
    else
      { Yes.  We must notify the SCM of our status right away. }
      NotifySCM(2000);
  end;

  { Start the engine manager. }
  try
    IsMultiThread := True;
    Debug(Prefix + 'InitEngineMgr');
    ffEngineMgr := TffEngineManager.Create(nil);
    ffEngineMgr.EventLogEnabled := ffDebugMode;
    InitEngineMgr(ffEngineMgr);
    Debug(Prefix + 'StartEngineMgr');
    ffEngineMgr.StartUp;
  except
    on E:Exception do begin
      ReportErrorFmt(ffc_EngMgrStartup, [E.Message]);
      Exit;
    end;
  end;

  ffSvcStatus.dwCurrentState := SERVICE_RUNNING;

  { Are we running as a service? }
  if ffIsService then
    { Yes.  Notify SCM we are running. }
    NotifySCM(0);

  { Go into a loop.  We will stay in the loop until we are told to shut
    down. }
  Finished := False;
  repeat

    Debug(Prefix + 'WaitFor');
    ffSvcEvent.WaitFor(0);

    { What we do depends upon the status set by our control handler. }
    case ffSvcStatus.dwCurrentState of
      SERVICE_CONTINUE_PENDING :
        begin
          Debug(Prefix + 'Receive SERVICE_CONTINUE_PENDING');
          try
            ffEngineMgr.StartUp;
            ffSvcStatus.dwCurrentState := SERVICE_RUNNING;
         except
            on E:Exception do begin
              ReportErrorFmt(ffc_EngMgrStartup, [E.message]);
              ffSvcStatus.dwCurrentState := SERVICE_STOPPED;
              Finished := True;
            end;
          end;
        end;
      SERVICE_PAUSE_PENDING :
        begin
          Debug(Prefix + 'Receive SERVICE_PAUSE_PENDING');
          try
            ffEngineMgr.ShutDown;
            ffSvcStatus.dwCurrentState := SERVICE_PAUSED;
         except
            on E:Exception do begin
              ReportErrorFmt(ffc_EngMgrShutdown, [E.message]);
              ffSvcStatus.dwCurrentState := SERVICE_STOPPED;
              Finished := True;
            end;
          end;
        end;
      SERVICE_STOP_PENDING :
        begin
          Debug(Prefix + 'Receive SERVICE_STOP_PENDING');
          try
            ffEngineMgr.ShutDown;
          except
            on E:Exception do
              ReportErrorFmt(ffc_EngMgrShutdown, [E.message]);
          end;
          Finished := True;
          ffSvcStatus.dwCurrentState := SERVICE_STOPPED;
        end;
    end;  { case }

    { Notify the SCM of our status. }
    if ffIsService then begin
      Debug(Prefix + 'NotifySCM');
      NotifySCM(0);
    end;

  until Finished;

  Debug(Prefix + 'FreeVars');
  ffSvcEvent.Free;
  try
    ffEngineMgr.Free;
  except
  end;

  { Tell the SCM that we have stopped. }
  Debug(Prefix + 'NotifySCM SERVICE_STOPPED');
  ffSvcStatus.dwCurrentState := SERVICE_STOPPED;
//  NotifySCM(0);                                                      {Deleted !!.02}

  Debug(Prefix + 'End');

end;
{=====================================================================}

{Begin !!.13}
{===TffServiceRecoveryEngine==========================================}
type
  TffServiceRecoveryEngine = class(TffRecoveryEngine)
  protected
    function reReportJournalState( JournalState : TJournalState;
                                    Alias : String;
                                    Path : String;
                                    Filename : String;
                                    ExceptionString : String ): Boolean; override;
  end;
{--------}
function TffServiceRecoveryEngine.reReportJournalState(
  JournalState: TJournalState;
  Alias, Path, Filename, ExceptionString: string): Boolean;
begin
  WriteEvent( ffc_JournalStateEventTypes[JournalState],
              ffc_GenError,
              ffc_JournalCompletenessMsgs[JournalState] + ' ' +
              ffc_JournalActionMsgs[JournalState] +
              Format( ' (Alias: %s; Path: %s; Filename: %s; Exception: %s  )',
              [ Alias, Path, Filename, ExceptionString ] ) );
  Result := True;
end;
{=====================================================================}

initialization
  FFRecoveryClass := TffServiceRecoveryEngine;
{End !!.13}
end.
