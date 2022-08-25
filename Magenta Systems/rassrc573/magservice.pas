unit MagService;

{$IFNDEF VER140}
  {$WARN UNSAFE_TYPE off} 
  {$WARN UNSAFE_CAST off}
  {$WARN UNSAFE_CODE off}
{$ENDIF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{
Updated by Angus Robertson, Magenta Systems Ltd, England, 11th August 2010
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

1 March 2010 - split from magsubsa
12 Jan 2005 - added MagCrtlServ
20 June 2005 - added MagInstService and MagRemService
28 Dec 2005 - added MagInstServiceEx
20 Mar 2006 - added MagCrtlServEx, MagInstServiceEx has new Dname and Dependencies params
1 June 2006 - corrected access in MagCrtlServEx
07 Aug 2008 - made functions compatible with unicode strings in Delphi 2009
              note: before Delphi 2009 String=AnsiString, now String=UnicodeString (not WideString)
              now using wide versions of Windows APIs and letting Delphi convert to string
1 March 2010 - use ChangeServiceConfig2 to set description and fail actions in MagInstServiceEx
               added MagSetDescrServ to replace ServiceSetComm (which set registry direcrtly)


}
interface

uses
  Windows, Messages, SysUtils, Classes, Forms, WinSvc, magsubs1 ;

type
  TServFunc = (servfCheck, servfStart, servfStop) ;

function MagCrtlServ (const Sname: string; ServFunc: TServFunc; Wait: integer;
                                                    var Resp: string): boolean ;
function MagInstServiceEx (const Sname, Dname, DriverPath: string;
    ServiceType, ServiceStart: DWORD ; const Account, Password: string;
    Dependencies: PWideChar; const Descr: string ; RestartDelay: integer ;
                                                 var Resp: string): boolean ;
function MagInstService (const Sname, DriverPath: string; ServiceType,
                                ServiceStart: DWORD ; var Resp: string): boolean ;
function MagRemService (const Sname: string; var Resp: string): boolean ;
function MagCrtlServEx (DBHandle: SC_HANDLE; const Sname: string;
             ServFunc: TServFunc; Wait: integer; var Resp: string): boolean ;
function MagSetDescrServ (const Sname, Descr: string): boolean ;

// Delphi winsvc.pas does not include APIs added for Windows 2000 and later, so here they mostly are

// Info levels for ChangeServiceConfig2 and QueryServiceConfig2
const SERVICE_CONFIG_DESCRIPTION              = 1 ;  // Windows 2000 and later
const SERVICE_CONFIG_FAILURE_ACTIONS          = 2 ;
const SERVICE_CONFIG_DELAYED_AUTO_START_INFO  = 3 ;  // Windows Vista/2008 and later
const SERVICE_CONFIG_FAILURE_ACTIONS_FLAG     = 4 ;
const SERVICE_CONFIG_SERVICE_SID_INFO         = 5 ;
const SERVICE_CONFIG_REQUIRED_PRIVILEGES_INFO = 6 ;
const SERVICE_CONFIG_PRESHUTDOWN_INFO         = 7 ;
const SERVICE_CONFIG_TRIGGER_INFO             = 8 ;  // Windows 7/20008 RC2 and later
const SERVICE_CONFIG_PREFERRED_NODE           = 9 ;

// Service description string, Unicode only
type
    _SERVICE_DESCRIPTIONW = record
        lpDescription: LPWSTR ;
    end;
    SERVICE_DESCRIPTIONW = _SERVICE_DESCRIPTIONW ;
    PSERVICE_DESCRIPTIONW = ^_SERVICE_DESCRIPTIONW ;
    TServiceDescriptionW = _SERVICE_DESCRIPTIONW ;
    PServiceDescriptionW = ^_SERVICE_DESCRIPTIONW ;

// Actions to take on service failure
type
    _SC_ACTION_TYPE = DWORD ;
    SC_ACTION_TYPE = _SC_ACTION_TYPE ;
    TScActionType = _SC_ACTION_TYPE ;

const
    SC_ACTION_NONE          = 0 ;
    SC_ACTION_RESTART       = 1 ;
    SC_ACTION_REBOOT        = 2 ;
    SC_ACTION_RUN_COMMAND   = 3 ;

type
    _SC_ACTION = record
        aType : SC_ACTION_TYPE ;
        Delay : DWORD ;
    end;
    SC_ACTION = _SC_ACTION ;
    PSC_ACTION = ^_SC_ACTION ;
    TScAction = _SC_ACTION ;
    PScAction = ^_SC_ACTION ;
type
    _SERVICE_FAILURE_ACTIONSW = record
        dwResetPeriod: DWORD ;
        lpRebootMsg: LPWSTR ;
        lpCommand: LPWSTR ;
        cActions: DWORD ;
        lpsaActions : PSC_ACTION ;
    end;
    SERVICE_FAILURE_ACTIONSW = _SERVICE_FAILURE_ACTIONSW ;
    PSERVICE_FAILURE_ACTIONSW = ^_SERVICE_FAILURE_ACTIONSW ;
    TServiceFailureActionW = _SERVICE_FAILURE_ACTIONSW ;
    PServiceFailureActionW = ^_SERVICE_FAILURE_ACTIONSW ;

var
QueryServiceConfig2W: function (hService: SC_HANDLE ; dwInfoLevel: DWORD ;
        lpBuffer: PBYTE ; cbBufSize: DWORD ; pcbBytesNeeded: LPDWORD): Bool ; stdcall ;

ChangeServiceConfig2W: function (hService: SC_HANDLE ; dwInfoLevel: DWORD ;
                                                     lpInfo: POINTER): BOOL ; stdcall ;

AdvApi32Module: THandle ;


implementation

// control service, start/stop/check

function MagCrtlServEx (DBHandle: SC_HANDLE; const Sname: string;
             ServFunc: TServFunc; Wait: integer; var Resp: string): boolean ;
var
    Args: PChar ;
    LocalHandle: SC_HANDLE ;
    ServiceStatus: TServiceStatus;
    EndTick, access: LongWord ;
    WideName: WideString ;  // Unicode
begin
    result := false ;
    resp := '' ;
    WideName := Sname ;
    FillChar (ServiceStatus, SizeOf (ServiceStatus), 0) ;
    if DBHandle = 0 then
    begin
        resp := 'Failed to Open Service Manager: ' + FormatLastError ;
        exit ;
    end ;
    try // except
    if ServFunc = servfCheck then
        access := GENERIC_READ
    else
        access := SERVICE_ALL_ACCESS ;
    LocalHandle := OpenServiceW (DBHandle, PWideChar (WideName), access) ;
    if LocalHandle <> 0 then
    begin
        try
            if NOT QueryServiceStatus (LocalHandle, ServiceStatus) then
            begin
                resp := 'Failed to Query Service: ' + FormatLastError ;
                exit ;
            end ;
            if ServFunc = servfCheck then
            begin
                result := false ;
                if (ServiceStatus.dwCurrentState = SERVICE_STOPPED) then
                                                    resp := 'Service Stopped' ;
                if (ServiceStatus.dwCurrentState = SERVICE_RUNNING) then
                begin
                    resp := 'Service Runnning' ;
                    result := true ;
                end ;
                exit ;
            end
            else if ServFunc = servfStop then
            begin
                if (ServiceStatus.dwCurrentState = SERVICE_STOPPED) then
                begin
                    resp := 'Service Already Stopped' ;
                    result := true ;
                    exit ;
                end ;
                if NOT ControlService (LocalHandle, SERVICE_CONTROL_STOP,
                                                             ServiceStatus) then
                begin
                    resp := 'Failed to Stop Service: ' + FormatLastError ;
                    exit ;
                end ;
                if (ServiceStatus.dwCurrentState = SERVICE_STOPPED) then
                begin
                    resp := 'Service Stopped OK' ;
                    result := true ;
                    exit ;
                end ;
                resp := 'Unable to Confirm Service Stopped' ;
            end
            else if ServFunc = servfStart then
            begin
                if (ServiceStatus.dwCurrentState = SERVICE_RUNNING) then
                begin
                    resp := 'Service Already Runnning' ;
                    result := true ;
                    exit ;
                end ;
                if NOT StartService (LocalHandle, 0, Args) then
                begin
                    resp := 'Failed to Start Service: ' +  FormatLastError;
                    exit ;
                end ;
                resp := 'Unable to Confirm Service Started' ;
            end ;

          // wait for something to happen
            EndTick := GetTickCount + LongWord ((wait + 1) * 1000) ;
            while (EndTick > GetTickCount) do
            begin
                FillChar (ServiceStatus, SizeOf (ServiceStatus), 0) ;
                if NOT QueryServiceStatus (LocalHandle, ServiceStatus) then
                begin
                    resp := 'Failed to Query Service: ' + FormatLastError ;
                    exit ;
                end ;
                if (ServFunc = servfStart) and
                                (ServiceStatus.dwCurrentState = SERVICE_RUNNING) then
                begin
                    resp := 'Service Run OK' ;
                    result := true ;
                    exit ;
                end ;
                if (ServFunc = servfStop) and
                             (ServiceStatus.dwCurrentState = SERVICE_STOPPED) then
                begin
                    resp := 'Service Stopped OK' ;
                    result := true ;
                    exit ;
                end ;
                Application.ProcessMessages;
                if Application.Terminated then exit ;
                Sleep (10) ;
           end ;
        finally
            CloseServiceHandle (LocalHandle) ;
      end ;
    end
    else
        resp := 'Failed to Open Service: ' + FormatLastError ;
    except
        resp := 'Exception Controlling Service - ' + GetExceptMess (ExceptObject) ;
    end ;
end;

function MagCrtlServ (const Sname: string; ServFunc: TServFunc; Wait: integer;
                                                    var Resp: string): boolean ;
var
    ServiceCDatabaseHandle: SC_HANDLE ; // service control database
begin
    result := false ;
    resp := '' ;
    ServiceCDatabaseHandle := OpenSCManager (Nil , Nil, SC_MANAGER_CONNECT) ;
    if ServiceCDatabaseHandle = 0 then
    begin
        resp := 'Failed to Open Service Manager: ' + FormatLastError ;
        exit ;
    end ;
    try // finally
        result := MagCrtlServEx (ServiceCDatabaseHandle, Sname,
                                                 ServFunc, Wait, Resp) ;
    finally
        if ServiceCDatabaseHandle <> 0 then
                CloseServiceHandle (ServiceCDatabaseHandle) ;
    end ;
end;


// install new service or update existing service - with logon

function MagInstServiceEx (const Sname, Dname, DriverPath: string;
    ServiceType, ServiceStart: DWORD ; const Account, Password: string;
    Dependencies: PWideChar; const Descr: string ; RestartDelay: integer ;
                                                 var Resp: string): boolean ;
var
    LocalHandle: SC_HANDLE ;
    ServiceCDatabaseHandle: SC_HANDLE ; // service control database
    AccountPtr: Pointer;
    WideSName, WideDName, WideDescr: WideString ;  // Unicode
    WidePath, WideAccount, WidePassword: WideString ;  // Unicode
    ServiceDescription: TServiceDescriptionW ;
    ServiceFailureAction: TServiceFailureActionW ;
    ScAction: TScAction ;
 //   PActions: PSC_ACTION;
begin
    result := false ;
    resp := '' ;
    LocalHandle := 0 ;
    WideSName := Sname ;
    WideDName := Dname ;
    WidePath := DriverPath ;
    WideAccount := Account ;
    WidePassword := Password ;
    WideDescr := Descr ; // 1 March 2010
    ServiceCDatabaseHandle := OpenSCManager (Nil , Nil, SC_MANAGER_ALL_ACCESS) ;
    if ServiceCDatabaseHandle = 0 then
    begin
        resp := 'Failed to Open Service Manager: ' + FormatLastError ;
        exit ;
    end ;
    try // finally
    try // except
        if Account = '' then
            AccountPtr := nil
        else
            AccountPtr := PWideChar (WideAccount) ;
        LocalHandle := OpenServiceW (ServiceCDatabaseHandle,
                                        PWideChar (WideSName), SERVICE_ALL_ACCESS) ;
        if LocalHandle <> 0 then
        begin
            if ChangeServiceConfigW (LocalHandle, ServiceType, ServiceStart,
                SERVICE_ERROR_NORMAL, PWideChar (WidePath), nil, nil, Dependencies,
                        AccountPtr, PWideChar (WidePassword), PWideChar (WideDName)) then
            begin
                resp := 'Existing Service Updated' ;
                result := true ;
            end
            else
            begin
                resp := 'Failed to Update Existing Service: ' + FormatLastError ;
                exit ;
            end;
        end
        else
        begin
            LocalHandle := CreateServiceW (ServiceCDatabaseHandle, PWideChar (WideSName),
                PWideChar (WideDName), SERVICE_ALL_ACCESS, ServiceType, ServiceStart,
                SERVICE_ERROR_NORMAL, PWideChar (WidePath), nil, nil, Dependencies,
                                                AccountPtr, PWideChar (WidePassword)) ;
            if LocalHandle <> 0 then
            begin
                resp := 'Service Installed' ;
                result := true ;
            end
            else
            begin
                resp := 'Failed to Install Service: ' + FormatLastError ;
                exit ;
            end;
        end ;

     // 1 March 2010, update description and failure actions
        ServiceDescription.Lpdescription := PWideChar (WideDescr) ;
        if Length (WideDescr) = 0 then ServiceDescription.Lpdescription := Nil ;  // clear it
        if NOT ChangeServiceconfig2W (LocalHandle, SERVICE_CONFIG_DESCRIPTION, @ServiceDescription) then
                                            resp := 'Failed to Set Service Description: ' + FormatLastError ;

     // 1 March 2010 update failure actions
        ServiceFailureAction.lpRebootMsg := Nil ;
        ServiceFailureAction.lpCommand := Nil ;
        ServiceFailureAction.dwResetPeriod := INFINITE ;
        ServiceFailureAction.cActions := 1;
        if RestartDelay > 0 then
        begin
            ScAction.Atype := SC_ACTION_RESTART ;
        end
        else
        begin
            ScAction.Atype := SC_ACTION_NONE ;
        end;
        ScAction.Delay := LongWord (RestartDelay) * TicksPerSecond ;  // convert seconds to millisecs
        ServiceFailureAction.lpsaActions := @ScAction ;
        if NOT ChangeServiceConfig2W (LocalHandle, SERVICE_CONFIG_FAILURE_ACTIONS,
                                                            @ServiceFailureAction) then
                                resp := 'Failed to Set Service Failure Action: ' + FormatLastError ;
    except
        resp := 'Exception Install Service - ' + GetExceptMess (ExceptObject) ;
    end ;
    finally
        if ServiceCDatabaseHandle <> 0 then
                CloseServiceHandle (ServiceCDatabaseHandle) ;
        if LocalHandle <> 0 then
                CloseServiceHandle (LocalHandle) ;
    end ;
end;

// install new service - not allowing a logon, discription or restart actions to be set here

function MagInstService (const Sname, DriverPath: string; ServiceType,
                            ServiceStart: DWORD ; var Resp: string): boolean ;
begin
    result := MagInstServiceEx (Sname, Sname, DriverPath, ServiceType,
                                            ServiceStart, '', '', Nil, '', 0, Resp) ;
end ;

// remove a service

function MagRemService (const Sname: string; var Resp: string): boolean ;
var
    LocalHandle: SC_HANDLE ;
    ServiceCDatabaseHandle: SC_HANDLE ; // service control database
    WideName: WideString ;  // Unicode
begin
    result := false ;
    resp := '' ;
    WideName := Sname ;
    ServiceCDatabaseHandle := OpenSCManager (Nil , Nil, SC_MANAGER_ALL_ACCESS) ;
    if ServiceCDatabaseHandle = 0 then
    begin
        resp := 'Failed to Open Service Manager: ' + FormatLastError ;
        exit ;
    end ;
    try // finally
    try // except
    LocalHandle := OpenServiceW (ServiceCDatabaseHandle,
                                        PWideChar (WideName), SERVICE_ALL_ACCESS) ;
    if LocalHandle <> 0 then
    begin
        if NOT DeleteService (LocalHandle) then
                    resp := 'Failed to Remove Service: ' + FormatLastError
        else
        begin
            resp := 'Service Removed' ;
            result := true ;
        end ;
        CloseServiceHandle (LocalHandle) ;
    end
    else
        resp := 'Failed to Remove Service: ' + FormatLastError ;
    except
        resp := 'Exception Removing Service - ' + GetExceptMess (ExceptObject) ;
    end ;
    finally
        if ServiceCDatabaseHandle <> 0 then
                CloseServiceHandle (ServiceCDatabaseHandle) ;
    end ;
end;

// replaces ServiceSetComm (which set registry direcrtly)

function MagSetDescrServ (const Sname, Descr: string): boolean ;
var
    LocalHandle: SC_HANDLE ;
    ServiceCDatabaseHandle: SC_HANDLE ; // service control database
    WideName, WideDescr: WideString ;  // Unicode
    ServiceDescription: TServiceDescriptionW ;
begin
    result := false ;
    WideName := Sname ;
    WideDescr := Descr ;
    ServiceCDatabaseHandle := OpenSCManager (Nil , Nil, SC_MANAGER_ALL_ACCESS) ;
    if ServiceCDatabaseHandle = 0 then
    begin
        exit ;
    end ;
    try // finally
    try // except
    LocalHandle := OpenServiceW (ServiceCDatabaseHandle,
                                        PWideChar (WideName), SERVICE_ALL_ACCESS) ;
    if LocalHandle <> 0 then
    begin
        ServiceDescription.Lpdescription := PWideChar (WideDescr) ;
        if Length (WideDescr) = 0 then ServiceDescription.Lpdescription := Nil ;  // clear it
        if ChangeServiceconfig2W (LocalHandle, SERVICE_CONFIG_DESCRIPTION, @ServiceDescription) then
        begin
            result := true ;
        end ;
        CloseServiceHandle (LocalHandle) ;
    end ;
    except
    //    resp := 'Exception Removing Service - ' + GetExceptMess (ExceptObject) ;
    end ;
    finally
        if ServiceCDatabaseHandle <> 0 then
                CloseServiceHandle (ServiceCDatabaseHandle) ;
    end ;
end;

// could borrow ChangeServiceConfig from Geovision StartDVR code

initialization
    AdvApi32Module := 0 ;
    if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
        if Win32MajorVersion >= 5 then
        begin
            AdvApi32Module := LoadLibrary ('AdvApi32.DLL') ;
            QueryServiceConfig2W := GetProcaddress (AdvApi32Module,'QueryServiceConfig2W') ;
            ChangeServiceConfig2W := Getprocaddress (AdvApi32Module,'ChangeServiceConfig2W') ;
        end ;
    end ;
finalization
    if AdvApi32Module <> 0 then
    begin
        FreeLibrary (AdvApi32Module) ;
        AdvApi32Module := 0 ;
    end ;

end.

