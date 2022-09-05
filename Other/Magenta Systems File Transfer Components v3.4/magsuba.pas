unit magsuba;

{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{
Updated by Angus Robertson, Magenta Systems Ltd, England, 7th August 2008
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

7 May    2001 - added WrapCaption, added Sleep to sysDelay
7 Apr 2003 - check terminate in delay loop
20 Mar 2004 - added sysDelayNoSleep so other functions in same program don't stop
12 Jan 2005 - added MagCrtlServ
18 Apr 2005 - SysDelay using TestTrgTick which supports wrapping at 49 days
20 June 2005 - added MagInstService and MagRemService
28 Dec 2005 - added MagInstServiceEx
20 Mar 2006 - added MagCrtlServEx, MagInstServiceEx has new Dname and Dependencies params
27 Mar 2006 - added sysDelayNoMess
1 June 2006 - corrected access in MagCrtlServEx
07 Aug 2008 - made functions compatible with unicode strings in Delphi 2009
              note: before Delphi 2009 String=AnsiString, now String=UnicodeString (not WideString)
               now using wide versions of Windows APIs and letting Delphi convert to string


pending - use ChangeServiceConfig2 to set description and fail actions
pending - use QueryServiceStatusEx

}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  stdctrls, WinSvc, magsubs1 ;

type
  TServFunc = (servfCheck, servfStart, servfStop) ;

function Confirm(Msg: string): Boolean;
procedure sysDelay(aMs: longword);
procedure sysDelayNoSleep (aMs: longword);
procedure sysDelayNoMess (aMs: longword);
procedure WrapCaption (CapText: string; CapLabel: TLabel) ;
function MagCrtlServ (const Sname: string; ServFunc: TServFunc; Wait: integer;
                                                    var Resp: string): boolean ;
function MagInstServiceEx (const Sname, Dname, DriverPath: string;
    ServiceType, ServiceStart: DWORD ; const Account, Password: string;
                     Dependencies: PWideChar; var Resp: string): boolean ;
function MagInstService (const Sname, DriverPath: string; ServiceType,
                                ServiceStart: DWORD ; var Resp: string): boolean ;
function MagRemService (const Sname: string; var Resp: string): boolean ;
function MagCrtlServEx (DBHandle: SC_HANDLE; const Sname: string;
             ServFunc: TServFunc; Wait: integer; var Resp: string): boolean ;


implementation

// message dialogue box

function Confirm(Msg: string): Boolean;
begin
  Result := MessageDlg(Msg, mtConfirmation, mbYesNoCancel, 0) = mrYes;
end;

{procedure sysDelay (aMs: longword);
var
    TickCount: longword;
begin
    TickCount := GetTickCount ;
    while ((GetTickCount - TickCount) < aMs) do
    begin
       //  ServiceThread.ProcessRequests (true);   // wait for message? = service version
         Application.ProcessMessages;
         if Application.Terminated then break ;
         if GetTickCount < TickCount then break ; // wrap around
         Sleep(0);   // thread now stops for rest of time slice
    end ;
end;

procedure sysDelayNoSleep (aMs: longword);
var
    TickCount: longword;
begin
    TickCount := GetTickCount ;
    while ((GetTickCount - TickCount) < aMs) do
    begin
         Application.ProcessMessages;
         if Application.Terminated then break ;
         if GetTickCount < TickCount then break ; // wrap around
    end ;
end;     }

procedure sysDelay (aMs: longword);
var
    Trg: longword;
begin
    Trg := GetTrgMsecs (aMs) ;
    while True do
    begin
        Application.ProcessMessages;
        if Application.Terminated then break ;
        if TestTrgTick (Trg) then break ;
        Sleep(0);   // thread now stops for rest of time slice
    end ;
end;

procedure sysDelayNoSleep (aMs: longword);
var
    Trg: longword;
begin
    Trg := GetTrgMsecs (aMs) ;
    while True do
    begin
        Application.ProcessMessages;
        if Application.Terminated then break ;
        if TestTrgTick (Trg) then break ;
    end ;
end;

procedure sysDelayNoMess (aMs: longword);
var
    Trg: longword;
begin
    Trg := GetTrgMsecs (aMs) ;
    while True do
    begin
        if Application.Terminated then break ;
        if TestTrgTick (Trg) then break ;
        Sleep(0);   // thread now stops for rest of time slice
    end ;
end;

// word wrap a caption allowing special file name and URL symbols for breaking

procedure WrapCaption (CapText: string; CapLabel: TLabel) ;
var
    maxcol: integer ;
begin
    if CapText <> '' then
    begin
        maxcol := (CapLabel.Width * Length (CapText)) div
                                        CapLabel.Canvas.TextWidth (CapText) ;
        CapLabel.Caption := WrapText (CapText, #13#10,
                                 [' ', '-', #9, '/', '\', '.', '?'], maxcol) ;
    end
    else
        CapLabel.Caption := CapText ;
end ;

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
                          Dependencies: PWideChar; var Resp: string): boolean ;
var
    LocalHandle: SC_HANDLE ;
    ServiceCDatabaseHandle: SC_HANDLE ; // service control database
    AccountPtr: Pointer;
    WideSName, WideDName: WideString ;  // Unicode
    WidePath, WideAccount, WidePassword: WideString ;  // Unicode
begin
    result := false ;
    resp := '' ;
    WideSName := Sname ;
    WideDName := Dname ;
    WidePath := DriverPath ;
    WideAccount := Account ;
    WidePassword := Password ;
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
                resp := 'Failed to Update Existing Service: ' + FormatLastError ;
            CloseServiceHandle (LocalHandle) ;
        end
        else
        begin
            LocalHandle := CreateServiceW (ServiceCDatabaseHandle, PWideChar (WideSName),
                PWideChar (WideDName), SERVICE_ALL_ACCESS, ServiceType, ServiceStart,
                SERVICE_ERROR_NORMAL, PWideChar (WidePath), nil, nil, Dependencies,
                                                AccountPtr, PWideChar (WidePassword)) ;
            if LocalHandle <> 0 then
            begin
                CloseServiceHandle (LocalHandle) ;
                resp := 'Service Installed' ;
                result := true ;
            end
            else
                resp := 'Failed to Install Service: ' + FormatLastError ;
        end ;
    except
        resp := 'Exception Install Service - ' + GetExceptMess (ExceptObject) ;
    end ;
    finally
        if ServiceCDatabaseHandle <> 0 then
                CloseServiceHandle (ServiceCDatabaseHandle) ;
    end ;
end;

// install new service - not allowing a logon to be set here

function MagInstService (const Sname, DriverPath: string; ServiceType,
                            ServiceStart: DWORD ; var Resp: string): boolean ;
begin
    result := MagInstServiceEx (Sname, Sname, DriverPath, ServiceType,
                                            ServiceStart, '', '', Nil, Resp) ;
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

// could borrow ChangeServiceConfig from Geovision StartDVR code 

end.

