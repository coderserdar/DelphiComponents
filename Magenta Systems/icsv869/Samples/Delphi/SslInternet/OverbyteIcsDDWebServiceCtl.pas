{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  SSL web application server Windows service sample, but may
              also be run interactively for debugging, no real GUI.  It
              requires DDService to be installed. It supports multiple SSL
              hosts with multiple listeners, each with it's own logging file,
              can order it's own SSL certificates, includes hacking protection,
              and will email status information and errors to an administrator.
              This sample is really a commercial web server.
Creation:     June 2021
Updated:      June 2021
Version:      8.67
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2003-2021 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany.

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

This unit is the service controller, see OverbyteIcsDDWebServiceSrv.pas for a
full description of the sample.

DD Service Application Framework Requirement
DDService is an enhanced Windows NT service application framework for Delphi and C++
Builder based on the original VCL service framework. In addition to it it also encapsulates
new Windows NT service APIs introduced since Windows 2000. Original author was the late
Arno Garrels of Duodata in Berlin. Now being maintained by Magenta Systems Ltd.

https://www.magsys.co.uk/delphi/ddservice.asp

DDService must be installed before opening this sample.  Once the sample is built, it
may be installed as a Windows service from an elevated command line prompt with the
command OverbyteIcsDDWebService.exe /install. But this sample is written to it may
also be run as a GUI for debugging.

History:
9 June 2021  V8.67 baseline

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsDDWebServiceCtl;
{$IF CompilerVersion < 15}
  {$MESSAGE FATAL 'This demo requires at least Delphi 7 or better'};
{$IFEND}

{$B-}                                 { Enable partial boolean evaluation   }
{$T-}                                 { Untyped pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long strings                    }
{$J+}                                 { Allow typed constant to be modified }
{ If you use Delphi 7, you may wants to disable warnings for unsage type,   }
{ unsafe code and unsafe typecast in the project options. Those warning are }
{ intended for .NET programs. You may also want to turn off deprecated      }
{ symbol and platform symbol warnings.                                      }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Dialogs,
  DDSvcUtils,
  DDWindows,
  DDSvcMgr,
  OverbyteIcsBlacklist;  // for IcsSimpleLogging

type
  TDDWebServiceCtl = class(TDDService)
    procedure DDServiceCreate(Sender: TObject);
    procedure DDServiceStart(Sender: TDDService; var Started: Boolean);
    procedure DDServiceStop(Sender: TDDService; var Stopped: Boolean);
    procedure DDServiceBeforeUninstall(Sender: TDDService);
    procedure DDServiceExecute(Sender: TDDService);
    procedure DDServiceShutdown(Sender: TDDService);
    procedure DDServiceAfterInstall(Sender: TDDService);
    procedure DDServiceRunException(Sender: TObject; E: Exception; var LogDefaultErrMsg, CanAbort: Boolean);
    procedure DDServicePowerEvent(Sender: TDDService; EventType: Integer; EventData: TDDIntPtr; var MsgResult: Integer);
    procedure DDServiceNetBindChange(Sender: TDDService; EventType: Integer);
    procedure DDServiceBeforeInstall(Sender: TDDService);
  private
    { Private declarations }
  public
    function GetServiceController: TServiceController; override;
    function GetServiceControllerEx: TServiceControllerEx; override;
    function GetConsoleCtrlHandler: TServiceConsoleCtrlHandler; override;
    procedure LogFile (const S: string);
  end;

const
  GServiceIdent = 'Yg738mTsW';   // added to exe when started by service so we know how it ran

var
  DDWebServiceCtl: TDDWebServiceCtl;
  GStartedByScm: Boolean = False;  // was it started as a service
  GInstalling: Boolean = False;    // if we are installing, don't try and run servers


implementation

{$R *.DFM}

uses
    OverbyteIcsDDWebServiceSrv;

{///////////////////////////////////////////////////////////////////////////}
procedure TDDWebServiceCtl.LogFile (const S: string);
begin
    if Assigned (DDWebServiceSrv) then DDWebServiceSrv.Display(S);
end;

{///////////////////////////////////////////////////////////////////////////}
procedure ServiceController(CtrlCode: DWORD); stdcall;
begin
  DDWebServiceCtl.Controller(CtrlCode);
end;

function TDDWebServiceCtl.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

function ServiceControllerEx(CtrlCode, EventType: DWORD;
  EventData, Context: Pointer): DWORD; stdcall;
begin
  Result := DDWebServiceCtl.ControllerEx(CtrlCode, EventType, EventData, Context);
end;

function TDDWebServiceCtl.GetServiceControllerEx: TServiceControllerEx;
begin
  Result := ServiceControllerEx;
end;

function ServiceConsoleCtrlHandler(Ctrl: DWord): Bool; stdcall;
begin
  Result := DDWebServiceCtl.ConsoleCtrlHandler(Ctrl);
end;

procedure TDDWebServiceCtl.DDServiceAfterInstall(Sender: TDDService);
var
    I: Integer;
    S: string;
begin
  S := ParamStr(0);
  I := Pos (' /', S);
  if I > 1 then S := Copy (S, 1, I - 1);  // strip arguments
  RegisterEventLogSource(Sender.ServiceName, S);
end;

procedure TDDWebServiceCtl.DDServiceBeforeInstall(Sender: TDDService);
begin
    ImagePath := ImagePath + ' /' + GServiceIdent;   // add ident to path so we know system is started
end;

procedure TDDWebServiceCtl.DDServiceBeforeUninstall(Sender: TDDService);
begin
//
end;

procedure TDDWebServiceCtl.DDServiceCreate(Sender: TObject);
begin
    IcsSimpleLogging (SimpLogName, 'DD Web App Service Create') ;
end;

procedure TDDWebServiceCtl.DDServiceExecute(Sender: TDDService);
begin
    IcsSimpleLogging (SimpLogName, 'DD Web App Service Execute Start') ;
    while not Terminated do begin
        Sender.ServiceThread.ProcessRequests (true);  // !! must be true else gobbles CPU
    end;
    IcsSimpleLogging (SimpLogName, 'DD Web App Service Execute End') ;
end;

procedure TDDWebServiceCtl.DDServiceNetBindChange(Sender: TDDService; EventType: Integer);
begin
    LogFile('Web App Service TCP Binding Changed') ;
end;

procedure TDDWebServiceCtl.DDServicePowerEvent(Sender: TDDService; EventType: Integer; EventData: TDDIntPtr;
  var MsgResult: Integer);
begin
    LogFile('DD Web App Service P:ower Event') ;
end;

procedure TDDWebServiceCtl.DDServiceRunException(Sender: TObject; E: Exception; var LogDefaultErrMsg,
  CanAbort: Boolean);
begin
    IcsSimpleLogging (SimpLogName, 'Service Run Exception - ' + E.Message) ;
    LogFile ('Service Run Exception - ' + E.Message) ;
    CanAbort := true ;
end;

procedure TDDWebServiceCtl.DDServiceShutdown(Sender: TDDService);
begin
    if DDWebServiceSrv <> nil then begin
        IcsSimpleLogging (SimpLogName, 'DD Web App Service Shutdown') ;
        LogFile ('Service Shutdown') ;
        PostMessage(DDWebServiceSrv.Handle, WM_ClOSE, 0, 0);
    end;
end;

procedure TDDWebServiceCtl.DDServiceStart(Sender: TDDService; var Started: Boolean);
begin
    Started := True;
    IcsSimpleLogging (SimpLogName, 'DD Web App Service Start') ;
end;

procedure TDDWebServiceCtl.DDServiceStop(Sender: TDDService; var Stopped: Boolean);
begin
    if DDWebServiceSrv <> nil then begin
        IcsSimpleLogging (SimpLogName, 'DD Web App Service Stop') ;
        LogFile ('Service Stop') ;
        PostMessage(DDWebServiceSrv.Handle, WM_ClOSE, 0, 0);
    end;
    Stopped := True;
end;

function TDDWebServiceCtl.GetConsoleCtrlHandler: TServiceConsoleCtrlHandler;
begin
  Result := ServiceConsoleCtrlHandler;
end;

{///////////////////////////////////////////////////////////////////////////}

end.
