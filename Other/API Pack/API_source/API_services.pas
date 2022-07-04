unit API_services;

//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------

interface

uses
  SysUtils, Classes;

type
  TAPI_services = class(TComponent)
  private
    { Private declarations }
    fversion: string;
    fservice: string;
    fcomputer: string;
    procedure setcomputer(s: string);
    procedure setservice(s: string);
    procedure setrunning(b: boolean);
    function getrunning: boolean;
    function getlist: tstringlist;
    procedure dummysl(sl: tstringlist);

  protected
    { Protected declarations }

  public
    { Public declarations }
    constructor Create(aowner:tcomponent); override;
    destructor Destroy; override;

  published
    { Published declarations }
    property Version: string read fversion;
    property Computer: string read fcomputer write setcomputer;
    property Service: string read fservice write setservice;
    property Running: boolean read getrunning write setrunning;
    property Services: tstringlist read getlist write dummysl stored false;

  end;

procedure Register;

implementation

{$r *.res}

uses WinSvc, WIndows;

const
  VERSIONINFO = 'r1.00/ari.pikivirta@kolumbus.fi';

//------------------------------------------------------------------------------
constructor TAPI_services.create(aowner: tcomponent);
begin
  inherited create(aowner);
  fversion:= VERSIONINFO;
end;

//------------------------------------------------------------------------------
destructor TAPI_services.destroy;
begin
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_services.dummysl(sl: tstringlist);
begin
  // does nothing.. dummy you know..
end;

//------------------------------------------------------------------------------
// get service status
//
// return status code if successful
// -1 if not
//
// return codes:
//   SERVICE_STOPPED
//   SERVICE_RUNNING
//   SERVICE_PAUSED
//
// following return codes
// are used to indicate that
// the service is in the
// middle of getting to one
// of the above states:
//   SERVICE_START_PENDING
//   SERVICE_STOP_PENDING
//   SERVICE_CONTINUE_PENDING
//   SERVICE_PAUSE_PENDING
//
// sMachine:
//   machine name, ie: \SERVER
//   empty = local machine
//
// sService
//   service name, ie: Alerter
//
function ServiceGetStatus(
  sMachine,
  sService : string ) : DWord;
var
  //
  // service control
  // manager handle
  schm,
  //
  // service handle
  schs   : SC_Handle;
  //
  // service status
  ss     : TServiceStatus;
  //
  // current service status
  dwStat : DWord;
begin
  dwStat := dword(-1);

  // connect to the service
  // control manager 
  schm := OpenSCManager(
    PChar(sMachine),
    Nil,
    SC_MANAGER_CONNECT);

  // if successful...
  if(schm > 0)then
  begin
    // open a handle to
    // the specified service
    schs := OpenService(
      schm,
      PChar(sService),
      // we want to
      // query service status
      SERVICE_QUERY_STATUS);

    // if successful...
    if(schs > 0)then
    begin
      // retrieve the current status
      // of the specified service
      if(QueryServiceStatus(
           schs,
           ss))then
      begin
        dwStat := ss.dwCurrentState;
      end;

      // close service handle
      CloseServiceHandle(schs);
    end;

    // close service control
    // manager handle
    CloseServiceHandle(schm);
  end;

  Result := dwStat;
end;

//------------------------------------------------------------------------------
// return TRUE if the specified
// service is running, defined by
// the status code SERVICE_RUNNING.
// return FALSE if the service
// is in any other state, including
// any pending states
//
function ServiceRunning(
  sMachine,
  sService : string ) : boolean;
begin
  Result := SERVICE_RUNNING =
    ServiceGetStatus(
      sMachine, sService );
end;


//------------------------------------------------------------------------------
// return TRUE if the specified
// service was stopped, defined by
// the status code SERVICE_STOPPED.
//
function ServiceStopped(
  sMachine,
  sService : string ) : boolean;
begin
  Result := SERVICE_STOPPED =
    ServiceGetStatus(
      sMachine, sService );
end;

//------------------------------------------------------------------------------
// start service
//
// return TRUE if successful
//
// sMachine:
//   machine name, ie: \SERVER
//   empty = local machine
//
// sService
//   service name, ie: Alerter
//
function ServiceStart(
  sMachine,
  sService : string ) : boolean;
var
  //
  // service control
  // manager handle
  schm,
  //
  // service handle
  schs   : SC_Handle;
  //
  // service status
  ss     : TServiceStatus;
  //
  // temp char pointer
  psTemp : PChar;
  //
  // check point
  dwChkP : DWord;
begin
  ss.dwCurrentState := dword(-1);
  
  // connect to the service
  // control manager
  schm := OpenSCManager(
    PChar(sMachine),
    Nil,
    SC_MANAGER_CONNECT);

  // if successful...
  if(schm > 0)then
  begin
    // open a handle to
    // the specified service
    schs := OpenService(
      schm,
      PChar(sService),
      // we want to
      // start the service and
      SERVICE_START or
      // query service status
      SERVICE_QUERY_STATUS);

    // if successful...
    if(schs > 0)then
    begin
      psTemp := Nil;
      if(StartService(
           schs,
           0,
           psTemp))then
      begin
        // check status
        if(QueryServiceStatus(
             schs,
             ss))then
        begin
          while(SERVICE_RUNNING
            <> ss.dwCurrentState)do
          begin
            //
            // dwCheckPoint contains a
            // value that the service
            // increments periodically
            // to report its progress
            // during a lengthy
            // operation.
            //
            // save current value
            //
            dwChkP := ss.dwCheckPoint;

            //
            // wait a bit before
            // checking status again
            //
            // dwWaitHint is the
            // estimated amount of time
            // the calling program
            // should wait before calling
            // QueryServiceStatus() again
            //
            // idle events should be
            // handled here...
            //
            Sleep(ss.dwWaitHint);

            if(not QueryServiceStatus(
                 schs,
                 ss))then
            begin
              // couldn't check status
              // break from the loop
              break;
            end;

            if(ss.dwCheckPoint <
              dwChkP)then
            begin
              // QueryServiceStatus
              // didn't increment
              // dwCheckPoint as it
              // should have.
              // avoid an infinite
              // loop by breaking
              break;
            end;
          end;
        end;
      end;

      // close service handle
      CloseServiceHandle(schs);
    end;

    // close service control
    // manager handle
    CloseServiceHandle(schm);
  end;

  // return TRUE if
  // the service status is running
  Result :=
    SERVICE_RUNNING =
      ss.dwCurrentState;
end;

//------------------------------------------------------------------------------
// stop service
//
// return TRUE if successful
//
// sMachine:
//   machine name, ie: \SERVER
//   empty = local machine
//
// sService
//   service name, ie: Alerter
//
function ServiceStop(
  sMachine,
  sService : string ) : boolean;
var
  //
  // service control
  // manager handle
  schm,
  //
  // service handle
  schs   : SC_Handle;
  //
  // service status
  ss     : TServiceStatus;
  //
  // check point
  dwChkP : DWord;
begin
  // connect to the service
  // control manager
  schm := OpenSCManager(
    PChar(sMachine),
    Nil,
    SC_MANAGER_CONNECT);

  // if successful...
  if(schm > 0)then
  begin
    // open a handle to
    // the specified service
    schs := OpenService(
      schm,
      PChar(sService),
      // we want to
      // stop the service and
      SERVICE_STOP or
      // query service status
      SERVICE_QUERY_STATUS);

    // if successful...
    if(schs > 0)then
    begin
      if(ControlService(
           schs,
           SERVICE_CONTROL_STOP,
           ss))then
      begin
        // check status
        if(QueryServiceStatus(
             schs,
             ss))then
        begin
          while(SERVICE_STOPPED
            <> ss.dwCurrentState)do
          begin
            //
            // dwCheckPoint contains a
            // value that the service
            // increments periodically
            // to report its progress
            // during a lengthy
            // operation.
            //
            // save current value
            //
            dwChkP := ss.dwCheckPoint;

            //
            // wait a bit before
            // checking status again
            //
            // dwWaitHint is the
            // estimated amount of time
            // the calling program
            // should wait before calling
            // QueryServiceStatus() again
            //
            // idle events should be
            // handled here...
            //
            Sleep(ss.dwWaitHint);

            if(not QueryServiceStatus(
                 schs,
                 ss))then
            begin
              // couldn't check status
              // break from the loop
              break;
            end;

            if(ss.dwCheckPoint <
              dwChkP)then
            begin
              // QueryServiceStatus
              // didn't increment
              // dwCheckPoint as it
              // should have.
              // avoid an infinite
              // loop by breaking
              break;
            end;
          end;
        end;
      end;

      // close service handle
      CloseServiceHandle(schs);
    end;

    // close service control
    // manager handle
    CloseServiceHandle(schm);
  end;

  // return TRUE if
  // the service status is stopped
  Result :=
    SERVICE_STOPPED =
      ss.dwCurrentState;
end;

//------------------------------------------------------------------------------
procedure TAPI_services.setcomputer(s: string);
begin
  if s<>fcomputer then
  begin
    fcomputer:= s;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_services.setservice(s: string);
begin
  if s<>fservice then
  begin
    fservice:= s;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_services.setrunning(b: boolean);
begin
  if b then                                                                     // if b = true
  begin                                                                         // if not service running
    if not getrunning then                                                      // start service
      ServiceStart( fcomputer, fservice );
  end else
  begin                                                                         // stop service command
    if getrunning then                                                          // if service is running
      ServiceStop( fcomputer, fservice );
  end;
end;

//------------------------------------------------------------------------------
function TAPI_services.getrunning: boolean;
begin
  result:= ServiceRunning( fcomputer, fservice );                               // return true if running
end;

//------------------------------------------------------------------------------
const
  //
  // Service Types
  //
  SERVICE_KERNEL_DRIVER       = $00000001;
  SERVICE_FILE_SYSTEM_DRIVER  = $00000002;
  SERVICE_ADAPTER             = $00000004;
  SERVICE_RECOGNIZER_DRIVER   = $00000008;

  SERVICE_DRIVER              =
    (SERVICE_KERNEL_DRIVER or
     SERVICE_FILE_SYSTEM_DRIVER or
     SERVICE_RECOGNIZER_DRIVER);

  SERVICE_WIN32_OWN_PROCESS   = $00000010;
  SERVICE_WIN32_SHARE_PROCESS = $00000020;
  SERVICE_WIN32               =
    (SERVICE_WIN32_OWN_PROCESS or
     SERVICE_WIN32_SHARE_PROCESS);

  SERVICE_INTERACTIVE_PROCESS = $00000100;

  SERVICE_TYPE_ALL            =
    (SERVICE_WIN32 or
     SERVICE_ADAPTER or
     SERVICE_DRIVER  or
     SERVICE_INTERACTIVE_PROCESS);

//------------------------------------------------------------------------------
// Get a list of services
//
// return TRUE if successful
//
// sMachine:
//   machine name, ie: \SERVER
//   empty = local machine
//
// dwServiceType
//   SERVICE_WIN32,
//   SERVICE_DRIVER or
//   SERVICE_TYPE_ALL
//
// dwServiceState
//   SERVICE_ACTIVE,
//   SERVICE_INACTIVE or
//   SERVICE_STATE_ALL
//
// slServicesList
//   TStrings variable to storage
//
function ServiceGetList(
  sMachine : string;
  dwServiceType,
  dwServiceState : DWord;
  slServicesList : TStrings )
  : boolean;
const
  //
  // assume that the total number of
  // services is less than 4096.
  // increase if necessary
  cnMaxServices = 4096;

type
  TSvcA = array[0..cnMaxServices]
          of TEnumServiceStatus;
  PSvcA = ^TSvcA;
          
var
  //
  // temp. use
  j : integer;

  //
  // service control
  // manager handle
  schm          : SC_Handle;

  //
  // bytes needed for the
  // next buffer, if any
  nBytesNeeded,

  //
  // number of services
  nServices,

  //
  // pointer to the
  // next unread service entry
  nResumeHandle : DWord;

  //
  // service status array
  ssa : PSvcA;
begin
  Result := false;

  // connect to the service
  // control manager
  schm := OpenSCManager(
    PChar(sMachine),
    Nil,
    SC_MANAGER_ALL_ACCESS);

  // if successful...
  if(schm > 0)then
  begin
    nResumeHandle := 0;

    New(ssa);

    EnumServicesStatus(
      schm,
      dwServiceType,
      dwServiceState,
      ssa^[0],
      SizeOf(ssa^),
      nBytesNeeded,
      nServices,
      nResumeHandle );

    //
    // assume that our initial array
    // was large enough to hold all
    // entries. add code to enumerate
    // if necessary.
    //

    for j := 0 to nServices-1 do
    begin
      slServicesList.
        Add( StrPas(
          ssa^[j].lpDisplayName ) + ' ('+ StrPas(ssa^[j].lpServiceName)+')');
    end;

    Result := true;

    Dispose(ssa);

    // close service control
    // manager handle
    CloseServiceHandle(schm);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_services.getlist: tstringlist;
begin
  result:= tstringlist.create;
  result.clear;
  ServiceGetList( '',
    SERVICE_WIN32,
    SERVICE_STATE_ALL,
    result );
end;

procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_services]);
end;

end.
