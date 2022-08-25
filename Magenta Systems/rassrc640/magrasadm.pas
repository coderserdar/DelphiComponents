unit magrasadm ;

// this component includes comments formatted for creation of a help file
// using F1HELP.  // is ignored, { } is used for help, (* *) is ignored

interface

uses
  SysUtils, Windows, Messages, Classes, MagRasApi, MagRasStr, MagSubs1 ;

const
  MagVersion = 'TMagRasAdm, 24th October 2005 - Release 5.00, Copyright 2005, Magenta Systems Ltd' ;

(*  DELPHI RAS COMPONENT - RAS ADMIN for RAS server (NT4/W2K only)
(C) Magenta Systems Ltd, 2005
Created by Angus Robertson, Magenta Systems Ltd, England
in 2001, delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/

Compatible with Delphi 3, 4 and 5 tested with NT4, W2K

The RasAdmin APIs are designed to monitor incoming RAS connections to
NT4 Server, they don't work with NT4 Workstation incoming calls.

They provide similar functionality to the Remote Access Admin application,
showing which ports are configured to answer calls, and the status of
those in-use.

Note there is another set of APIs for RRAS (Routing and Remote Access
Service) that are not currently supported by TMagRasAdm.

The API work in both NT4 and W2K, but only appear to monitor NT4 Server
(they work across a LAN).

----------------------------------------------------------------------------

Microsoft Windows NT
Copyright(c) Microsoft Corp., 1992-1999
Module Name: RASSAPI.H

Description:

    This file contains the RASADMIN structures, defines and
    function prototypes for the following APIs and they can
    be imported from RASSAPI.DLL:

     RasAdminServerGetInfo
     RasAdminGetUserAccountServer
     RasAdminUserSetInfo
     RasAdminUserGetInfo
     RasAdminPortEnum
     RasAdminPortGetInfo
     RasAdminPortClearStatistics
     RasAdminPortDisconnect
     RasAdminFreeBuffer
 
Note:

    This header file and the sources containing the APIs will work
    only with UNICODE strings.   *)

const
  RASSAPI_MAX_PHONENUMBER_SIZE  = 128 ;
  RASSAPI_MAX_MEDIA_NAME        = 16 ;
  RASSAPI_MAX_PORT_NAME         = 16 ;
  RASSAPI_MAX_DEVICE_NAME       = 128 ;
  RASSAPI_MAX_DEVICETYPE_NAME   = 16 ;
  RASSAPI_MAX_PARAM_KEY_SIZE    = 32 ;

// Bits for TRasUser0 indicating user's Remote Access privileges and mask to isolate
// call back privilege.
//
// Note: Bit 0 MUST represent NoCallback due to a quirk of the "userparms"
//       storage method.  When a new LAN Manager user is created, bit 0 of the
//       userparms field is set to 1 and all other bits are 0.  These bits are
//       arranged so this "no Dial-In info" state maps to the "default Dial-In
//       privilege" state.
  RASPRIV_NoCallback        = $01 ;
  RASPRIV_AdminSetCallback  = $02 ;
  RASPRIV_CallerSetCallback = $04 ;
  RASPRIV_DialinPrivilege   = $08 ;
  RASPRIV_CallbackType = RASPRIV_AdminSetCallback OR
                         RASPRIV_CallerSetCallback OR RASPRIV_NoCallback ;

// Modem condition codes for TRasPort1
  RAS_MODEM_OPERATIONAL         = 1 ; // No modem errors.
  RAS_MODEM_NOT_RESPONDING      = 2 ;
  RAS_MODEM_HARDWARE_FAILURE    = 3 ;
  RAS_MODEM_INCORRECT_RESPONSE  = 4 ;
  RAS_MODEM_UNKNOWN             = 5 ;

// Line condition codes for TRasPort1
  RAS_PORT_NON_OPERATIONAL  = 1 ;
  RAS_PORT_DISCONNECTED     = 2 ;
  RAS_PORT_CALLING_BACK     = 3 ;
  RAS_PORT_LISTENING        = 4 ;
  RAS_PORT_AUTHENTICATING   = 5 ;
  RAS_PORT_AUTHENTICATED    = 6 ;
  RAS_PORT_INITIALIZING     = 7 ;

// The following three structures are same as the ones
// defined in rasman.h and have been renamed to prevent
// redefinitions when both header files are included.

// RasAdminPortGetInfo stuff

const
  ParamNumber = 0 ;
  ParamString = 1 ;

type
  TRasParamsFormat = DWORD ;
  Ras_Params_Format = TRasParamsFormat ;

  _RasLenStr = record
    Length: DWORD;
    Data: PChar ;
  end ;

  PRasParamsValue = ^TRasParamsValue ;
  Ras_Params_Value = record
    Number: DWORD ;
    LString: _RasLenStr ;
  end;
  TRasParamsValue = Ras_Params_Value ;

  PRasParameters = ^TRasParameters ;
  Ras_Parameters = record
    P_Key:  packed array [0..RASSAPI_MAX_PARAM_KEY_SIZE-1] of AnsiChar ;
    P_Type: Ras_Params_Format ;
    P_Attributes: Byte ;
    P_Value: Ras_Params_Value ;
  end;
  TRasParameters = Ras_Parameters ;

// RasAdminGetUserParms stuff

type
  PRasUser0 = ^TRasUser0 ;
  _Ras_User_0 = record
    bfPrivilege: Byte ;
    szPhoneNumber: packed array[0..RASSAPI_MAX_PHONENUMBER_SIZE] of WideChar ;
  end ;

  TRasUser0 = _Ras_User_0 ;
  Ras_User_0 = _Ras_User_0 ;

// RasAdminPortEnum stuff

  PRasPort0 = ^TRasPort0;
  _Ras_Port_0 = record
    wszPortName: packed array [0..RASSAPI_MAX_PORT_NAME - 1] of WideChar ;
    wszDeviceType: packed array [0..RASSAPI_MAX_DEVICETYPE_NAME - 1] of WideChar ;
    wszDeviceName: packed array [0..RASSAPI_MAX_DEVICE_NAME - 1] of WideChar ;
    wszMediaName: packed array [0..RASSAPI_MAX_MEDIA_NAME - 1] of WideChar ;
    Reserved: DWORD ;
    Flags: DWORD ;
    wszUserName: packed array [0..UNLEN] of WideChar ;
    wszComputer: packed array [0..NETBIOS_NAME_LEN - 1] of WideChar ;
    dwStartSessionTime: DWORD ; // seconds from 1/1/1970
    wszLogonDomain: packed array [0..DNLEN] of WideChar ;
    fAdvancedServer: BOOL ;
  end;
  TRasPort0 = _Ras_Port_0 ;
  Ras_Port_0 = _Ras_Port_0 ;

const
// Possible values for MediaId - note sure where these are used...
  MEDIA_UNKNOWN       = 0 ;
  MEDIA_SERIAL        = 1 ;
  MEDIA_RAS10_SERIAL  = 2 ;
  MEDIA_X25           = 3 ;
  MEDIA_ISDN          = 4 ;

// Possible bits set in Flags field for TRasPort0
  USER_AUTHENTICATED    = $0001 ;
  MESSENGER_PRESENT     = $0002 ;
  PPP_CLIENT            = $0004 ;
  GATEWAY_ACTIVE        = $0008 ;
  REMOTE_LISTEN         = $0010 ;
  PORT_MULTILINKED      = $0020 ;

type
  PIpAddr = ^TIpAddr ;
  IPADDR = ULONG ;
  TIpAddr = IPADDR ;

// The following PPP structures are same as the ones
// defined in rasppp.h and have been renamed to prevent
// redefinitions when both header files are included
// in a module.

// Maximum length of address string, e.g. "255.255.255.255" for IP.

const
  RAS_IPADDRESSLEN  = 15 ;
  RAS_IPXADDRESSLEN = 22 ;
  RAS_ATADDRESSLEN  = 32 ;

type
  PRasPppNbfcpResult = ^TRasPppNbfcpResult ;
  _Ras_PPP_NBFCP_Result = record
    dwError: DWORD ;
    dwNetBiosError: DWORD ;
    szName: packed array[0..NETBIOS_NAME_LEN] of AnsiChar ;
    wszWksta: packed array[0..NETBIOS_NAME_LEN] of WideChar ;
  end;
  TRasPppNbfcpResult = _Ras_PPP_NBFCP_Result ;
  Ras_PPP_NBFCP_RESULT = _Ras_PPP_NBFCP_Result ;

  PRasPppIpcpResult = ^TRasPppIpcpResult ;
  _Ras_PPP_IPCP_Result = record
    dwError: DWORD ;
    wszAddress: packed array[0..RAS_IPADDRESSLEN] of WideChar ;
  end;
  TRasPppIpcpResult = _Ras_PPP_IPCP_Result ;
  Ras_PPP_IPCP_Result = _Ras_PPP_IPCP_Result ;

  PRasPppIpxcpResult = ^TRasPppIpxcpResult ;
  _RAS_PPP_IPXCP_Result = record
    dwError: DWORD ;
    wszAddress: packed array[0..RAS_IPXADDRESSLEN] of WideChar ;
  end;
  TRasPppIpxcpResult = _Ras_PPP_IPXCP_Result ;
  Ras_PPP_IPXCP_Result = _RAS_PPP_IPXCP_Result ;

  PRasPppAtcpResult = ^TRasPppAtcpResult ;
  _Ras_PPP_ATCP_Result = record
    dwError: DWORD ;
    wszAddress: packed array[0..RAS_ATADDRESSLEN] of WideChar ;
  end;
  TRasPppAtcpResult = _Ras_PPP_ATCP_Result ;
  Ras_PPP_ATCP_Result = _Ras_PPP_ATCP_Result ;

  PRasProjectionResult = ^TRasProjectionResult ;
  _Ras_PPP_Projection_Result = record
    nbf: Ras_PPP_NBFCP_Result ;
    ip: Ras_PPP_IPCP_Result ;
    ipx: Ras_PPP_IPXCP_Result ;
    at: Ras_PPP_ATCP_Result ;
  end;
  TRasProjectionResult = _Ras_PPP_Projection_Result ;
  Ras_PPP_Projection_Result = _RAS_PPP_Projection_Result ;

// RasAdminPortGetInfo stuff

  PRasPort1 = ^TRasPort1 ;
  _Ras_Port_1 = record
    rasport0: Ras_Port_0 ;
    LineCondition: DWORD ;
    HardwareCondition: DWORD ;
    LineSpeed: DWORD ; // in bits/second
    NumStatistics: Word ;  // ignore
    NumMediaParms: Word ;  // number of TRasParameters structures
    SizeMediaParms: DWORD ;
    ProjResult: Ras_PPP_Projection_Result ;
  end;
  TRasPort1 = _Ras_Port_1 ;
  Ras_Port_1 = _Ras_Port_1 ;

// RasAdminPortGetInfo stuff

  PRasPortStatistics = ^TRasPortStatistics;
  _Ras_Port_Statistics = record
    // The connection statistics are followed by port statistics
    // A connection is across multiple ports.
    dwBytesXmited: DWORD ;
    dwBytesRcved: DWORD ;
    dwFramesXmited: DWORD ;
    dwFramesRcved: DWORD ;
    dwCrcErr: DWORD ;
    dwTimeoutErr: DWORD ;
    dwAlignmentErr: DWORD ;
    dwHardwareOverrunErr: DWORD ;
    dwFramingErr: DWORD ;
    dwBufferOverrunErr: DWORD ;
    dwBytesXmitedUncompressed: DWORD ;
    dwBytesRcvedUncompressed: DWORD ;
    dwBytesXmitedCompressed: DWORD ;
    dwBytesRcvedCompressed: DWORD ;
    // the following are the port statistics
    dwPortBytesXmited: DWORD ;
    dwPortBytesRcved: DWORD ;
    dwPortFramesXmited: DWORD ;
    dwPortFramesRcved: DWORD ;
    dwPortCrcErr: DWORD ;
    dwPortTimeoutErr: DWORD ;
    dwPortAlignmentErr: DWORD ;
    dwPortHardwareOverrunErr: DWORD ;
    dwPortFramingErr: DWORD ;
    dwPortBufferOverrunErr: DWORD ;
    dwPortBytesXmitedUncompressed: DWORD ;
    dwPortBytesRcvedUncompressed: DWORD ;
    dwPortBytesXmitedCompressed: DWORD ;
    dwPortBytesRcvedCompressed: DWORD ;
  end;
  TRasPortStatistics = _Ras_Port_Statistics ;
  Ras_Port_Statistics = _Ras_Port_Statistics ;

// RasAdminServerGetInfo stuff
// Server version numbers

const
  RASDOWNLEVEL       = 10 ;   // identifies a LM RAS 1.0 server
  RASADMIN_35        = 35 ;   // Identifies a NT RAS 3.5 server or client
  RASADMIN_CURRENT   = 40 ;   // Identifies a NT RAS 4.0 server or client

type
  PRasServer0 = ^TRasServer0 ;
  _Ras_Server_0 = record
    TotalPorts: Word ;             // Total ports configured on the server and free
    PortsInUse: Word ;             // Ports currently in use by remote clients
    RasVersion: DWORD ;            // version of RAS server
  end;
  TRasServer0 = _Ras_Server_0 ;
  Ras_Server_0 = _Ras_Server_0 ;

// function prototypes - loading dynamically

var

RasAdminServerGetInfo: Function (
    const lpszServer: PWChar ;
    var pRasServer0: TRasServer0
    ): DWORD ; stdcall ;

RasAdminGetUserAccountServer: Function (
    const lpszDomain: PWChar ;
    const lpszServer: PWChar ;
    lpszUserAccountServer: LPWSTR
    ): DWORD ; stdcall ;

RasAdminUserGetInfo: Function (
    const lpszUserAccountServer: PWChar ;
    const lpszUser: PWChar ;
    var pRasUser0: TRasUser0
    ): DWORD ; stdcall ;

RasAdminUserSetInfo: Function (
    lpszUserAccountServer: PWChar ;
    lpszUser: PWChar ;
    pRasUser0: PRasUser0
    ): DWORD ; stdcall ;

RasAdminPortEnum: Function (
    const lpszServer: PWChar ;
    ppRasPort0: PChar ;
    var pcEntriesRead: Word
    ): DWORD ; stdcall ;

RasAdminPortGetInfo: Function (
    const lpszServer, lpszPort: PWChar ;
    var pRasPort1: TRasPort1 ;
    var pRasStats: TRasPortStatistics ;
    ppRasParams: PChar
    ): DWORD ; stdcall ;

RasAdminPortClearStatistics: Function (
    const lpszServer: PWChar ;
    const lpszPort: PWChar
    ): DWORD ; stdcall ;

RasAdminPortDisconnect: Function (
    const lpszServer: PWChar ;
    const lpszPort: PWChar
    ): DWORD ; stdcall ;

RasAdminFreeBuffer: Function (
    _Pointer: Pointer
    ): DWORD ; stdcall ;

RasAdminGetErrorString: Function (
    ResourceId: UINT ;
    lpszString: PWChar ;
    InBufSize: DWORD
    ): DWORD ; stdcall ;

// following four are exported by a user created admin DLL - they are not in RASSAPI

RasAdminAcceptNewConnection: Function (
    pRasPort1: PRasPort1 ;
    pRasStats: PRasPortStatistics ;
    pRasParams: PRasParameters
    ): DWORD ; stdcall ;

RasAdminConnectionHangupNotification: Function (
    pRasPort1: PRasPort1 ;
    pRasStats: PRasPortStatistics ;
    pRasParams: PRasParameters
    ): DWORD ; stdcall ;

RasAdminGetIpAddressForUser: Function (
    lpszUserName: PWChar ;
    lpszPortName: PWChar ;
    var pipAddress: TIpAddr ;
    var bNotifyRelease: BOOL
    ): DWORD ; stdcall ;

RasAdminReleaseIpAddress: Function (
    lpszUserName: PWChar ;
    lpszPortName: PWChar ;
    pipAddress: PIpAddr
    ): DWORD ; stdcall ;

// The following two APIs are used to get/set
// RAS user permissions in to a UsrParms buffer
// obtained by a call to NetUserGetInfo.

// Note that RasAdminUserGetInfo and RasAdminUserSetInfo
// are the APIs you should be using for getting and
// setting RAS permissions.
(*
RasAdminGetUserParms: Function (
    lpszParms: PWChar ;
    var pRasUser0: TRasUser0
    ): DWORD ; stdcall ;

RasAdminSetUserParms: Function (
    lpszParms: PWChar ;
    cchNewParms: DWORD ;
    pRasUser0: TRasUser0
    ): DWORD ; stdcall ;   *)

const
  RASSAPI_DLL = 'RASSAPI.DLL'; // NT4/W2K only, ras server stuff
  TotServPorts = 40 ;          // How many ports MagRasAdm will handle
type

{ Record used to return information about each installed RAS server port,
  and who is currently using it.  The first 10 fields are completed for
  all ports by the CheckAllPorts method and are stable once a call starts,
  the GetPortInfo method completes the remaining fields specifically for
  each port, it's only needed for ports with calls.

  Note a port that has not been used seems to have a Projection result of
  ERROR_PPP_NO_PROTOCOLS_CONFIGURED.

  This record is made public as the array Ports.   }
  TRasPorts = record
  // following fields from rasport0
    PortName: string ;     { Port name, ie COM1 }
    DeviceType: string ;   { Port RAS device type, ie modem  }
    DeviceName: string ;   { Port RAS device name, ie Standard Modem }
    MediaName: string ;    { Port Tapi media type, ie rastapi }
    bFlags: DWORD ;        { Nature of connection on this port, one or more flags }
    UserName: string ;     { Name of the remote user connected to the port }
    Computer: string ;     { Name of the remote computer connected to the port }
    StartDT: TDateTime ;   { Session start time }
    LogonDomain: string ;  { Name of the domain on which the user is authenticated }
    bAdvancedServer: BOOL ; { If Advanced Server is associated with the port }
 // following fields from rasport1
    LineCond: DWORD ;      { Line condition on the port, Ras_Port_xx literals, the main status }
    HardwareCond: DWORD ;  { State on the port, Ras_Modem_xx literals }
    Speed: DWORD ;         { Line speed in bits/second }
    Projection: DWORD ;    { Result of the IP projection }
    IPAddress: string ;    { IP address assigned to the remote client }
// from TRasPortStatistics
    Statistics: TRasPortStatistics ; { Port statistics, data transmitted and received, etc }
  end;

{ TMagRasAdm allows Windows NT4 RAS Server to be monitored. RAS server
  answers incoming RAS calls on NT4 Server (not NT4 Professional). }
  TMagRasAdm = class(TComponent)
  private
//   Private declarations
    fVersion: string ;
    fPortsTotal: integer ;
    fPortsInuse: integer ;
    fPortsFree: integer ;
    fRasAdmVer: integer ;
    fRasServer: string ;
    fWideRasServer: array [0..CNLEN + 2] of WideChar ;
    fServerDomain: string ;
    fUserServer: string ;
    fWideUserServer: array [0..CNLEN + 2] of WideChar ;

    procedure SetServer (ServerName: string) ;

  protected
//     Protected declarations
   public
//    Public declarations

// error handling

{ This variable is set by almost all the TMagRasAdm methods that fail.  The
  error code may be converted to a message using GetErrorString. }
    LastError: LongInt;

{ A message describing the last error condition, set by most TMagRasAdm methods. }
    StatusStr: String ;

{ Array of TRasPorts records, each representing one server port. }
    Ports: array [0..TotServPorts - 1] of TRasPorts ;

{ Initialises various lists and functions in TMagRasAdm, which allows
  Windows NT4 RAS server to be monitored.  }
    constructor Create(AOwner: TComponent); override;

{ Destroys TMagRasAdm. }
    destructor Destroy; override;

{ Checks which ports are currently available for use by RAS server, for
  incoming RAS calls, and shows the basic status of each port.  Note that
  ports being used for outgoing RAS calls 'disappear' from the list until
  freed.  Ports only specified for outgoing calls don't appear atall.
  Calls RasAdminServerGetInfo and RasAdminPortEnum.

  The RasServer properties should be set before the CheckAllPorts method
  is used, to the computer name to monitor, in the format \\servername.
  The GetCompName function may be used to get the local PC name, but remote
  names may also be specified.

  This method completes various properties: PortsFree is free ports (not on
  calls), PortsInuse is busy ports, RasAdmVer is the Ras Admin Version,
  PortsTotal is total ports enumerated.  Information about each port is
  returned in the Ports [x] array, each field of which is a TRasPorts record.
  Note that only part of each record is completed by the Method, and
  GetPortInfo needs to be called for each port in turn to get more info.

  Result is 0 if successful, or an error code. }
    function CheckAllPorts: integer ;

{ Encapsulates the RasAdminGetErrorString function, to convert a RAS error code
  to a message.  If the error code is not from RAS (ie 600 to 782), a windows
  system error message is returned instead.  }
    function GetErrorString(ErrorCode: LongInt): String;

{ Resets the counters representing the various statistics reported in the
  Ports array. The counters are reset to zero and start accumulating.

  The port should be specified, ie COM1, taken from Ports [x].PortName.

  Result is 0 if successful, or an error code. }
    function ClearStats (PortName: string): integer ;

{ Disconnects a port that is currently in use for RAS server.

  The port should be specified, ie COM1, taken from Ports [x].PortName.

  Result is 0 if successful, or an error code. }
    function Disconnect (PortName: string): integer ;

{ Encapsulates the RasAdminPortGetInfo function to get detailed information
  about a specific port, such as the RAS state (idle, online), line speed,
  and performance information.

  The port should be specified, ie COM1, taken from Ports [x].PortName.

  The record for this port in the Ports array is updated.

  Result is 0 if successful, or an error code. }
    function GetPortInfo (PortName: string): integer ;

{ Helper function to convert Ports [x].HardwareCond into a literal
  message. }
    function GetHardwareMess (state: integer): String ;

{ Helper function to convert Ports [x].LineCond into a literal
  message. }
    function GetLineMess (state: integer): String ;

{ Encapsulates the RasAdminGetUserAccountServer function that retrieves the
  name of the server that has the user account database.

  CheckAllPorts must have been called previously to get the RAS server name,
  and the ServerDomain property set if the PC is part of an NT/W2K domain,
  the user server name is then available as the UserServer property.

  Result is 0 if successful, or an error code. }
    function GetUserServer: integer ;

{ Encapsulates the RasAdminUserGetInfo function to get the RAS permissions
  and callback phone number information for a specified user.

  GetUserServer must have been called previously to get the UserServer.

  Result is 0 if successful, or an error code. }
    function GetUserInfo (user: string; var bPrivilege: byte;
                                            var PhoneNum: string): integer ;

{ Encapsulates the RasAdminUserSetInfo function to set the RAS permissions
  and callback phone number information for a specified user.

  GetUserServer must have been called previously to get the UserServer.

  Result is 0 if successful, or an error code. }
    function SetUserInfo (user: string; bPrivilege: byte;
                                            PhoneNum: string): integer ;

  published
// Published declarations


{ The version of TMagRasAdm - note is only only made writable so it displays
  in the object inspector }
    Property Version: string        read fVersion write fversion stored False;

{ The computer name to monitor, in the format \\servername.  The GetCompName
  function may be used to get the local PC name, but remote names may also
  be specified.  This should be specified before calling CheckAllPorts. }
    Property RasServer: string      read fRasServer write SetServer ;

{ If the PC is part of an NT/W2K domain, the name should be specified before
  calling GetUserServer. }
    Property ServerDomain: string   read fServerDomain write fServerDomain ;

{ Result of GetUserServer, the user account server name. }
    Property UserServer: string     read fUserServer ;

{ Result of CheckAllPorts, the total ports currently enumerated in the
  Ports array, either free or in-use.  Note this number may change if
  outgoing RAS calls are being made on the same ports. }
    Property PortsTotal: integer    read fPortsTotal ;

{ Result of CheckAllPorts, the number of busy ports on RAS server. }
    Property PortsInuse: integer    read fPortsInuse ;

{ Result of CheckAllPorts, the number of free ports on RAS server. }
    Property PortsFree: integer     read fPortsFree ;

{ Result of CheckAllPorts, the RAS server version, should be RasAdmin_Current
  for NT4 RAS server. }
    Property RasAdmVer: integer     read fRasAdmVer ;

  end;

var
  MagRasAdmLib: THandle = 0 ;
  MagRasAdmAPI_Loaded: Boolean = false ;   // See if DLL functions are loaded

  function MagLoadRasAdmApi: boolean ;
  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Magenta Systems', [TMagRasAdm]);
end;

// Try and load various RAS Admin DLL functions. Returns false if failed

function MagLoadRasAdmApi: boolean ;
begin
    result := false ;
    if MagRasAdmAPI_Loaded then
    begin
        result := Assigned (RasAdminServerGetInfo) ;
        exit ;
    end ;

// ignore Win9x
    if Win32Platform <> VER_PLATFORM_WIN32_NT then exit ;

// open libraries - only come here once
    RasAdminServerGetInfo := Nil;
    MagRasAdmAPI_Loaded := True ;
    MagRasAdmLib := LoadLibrary (RASSAPI_DLL) ;
    If MagRasAdmLib = 0 then exit ;

// set function addresses to versions in DLL, all are Unicode versions!!!!
    RasAdminServerGetInfo := GetProcAddress (MagRasAdmLib, 'RasAdminServerGetInfo') ;
    RasAdminGetUserAccountServer := GetProcAddress (MagRasAdmLib, 'RasAdminGetUserAccountServer') ;
    RasAdminUserGetInfo := GetProcAddress (MagRasAdmLib, 'RasAdminUserGetInfo') ;
    RasAdminUserSetInfo := GetProcAddress (MagRasAdmLib, 'RasAdminUserSetInfo') ;
    RasAdminPortEnum := GetProcAddress (MagRasAdmLib, 'RasAdminPortEnum') ;
    RasAdminPortGetInfo := GetProcAddress (MagRasAdmLib, 'RasAdminPortGetInfo') ;
    RasAdminPortClearStatistics := GetProcAddress (MagRasAdmLib, 'RasAdminPortClearStatistics') ;
    RasAdminPortDisconnect := GetProcAddress (MagRasAdmLib, 'RasAdminPortDisconnect') ;
    RasAdminFreeBuffer := GetProcAddress (MagRasAdmLib, 'RasAdminFreeBuffer') ;
    RasAdminGetErrorString := GetProcAddress (MagRasAdmLib, 'RasAdminGetErrorString') ;
//    RasAdminGetUserParms := GetProcAddress (MagRasAdmLib, 'RasAdminGetUserParms') ;
//    RasAdminSetUserParms := GetProcAddress (MagRasAdmLib, 'RasAdminSetUserParms') ;
    result := Assigned (RasAdminServerGetInfo) ;
end ;

constructor TMagRasAdm.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    fVersion := MagVersion ;
end;

destructor TMagRasAdm.Destroy;
begin
    inherited Destroy;
end;

procedure TMagRasAdm.SetServer (ServerName: string) ;
begin
    fRasServer := ServerName ;
    StringToWideChar (ServerName, fWideRasServer, CNLEN + 2) ;
end ;

function TMagRasAdm.CheckAllPorts: integer ;
var
    RasServer0: TRasServer0 ;
    EntriesRead: WORD ;
    I: integer ;
    ppRasPort0, curpRasPort0: PChar ;
    RasPort0: TRasPort0 ;
begin
    if (NOT MagLoadRasAdmApi) then
    begin
        LastError := ERROR_DLL_NOT_FOUND ;
        result := LastError ;
        exit ;
    end ;
    FillChar (RasServer0, SizeOf (RasServer0), #0);
    fPortsTotal := 0 ;
    fPortsInuse := 0 ;
    fPortsFree := 0 ;
    fRasAdmVer := 0 ;
    if fRasServer = '' then
        LastError := RasAdminServerGetInfo (Nil, RasServer0)
    else
    begin
        LastError := RasAdminServerGetInfo (fWideRasServer, RasServer0) ;
    end ;
    Result := LastError;
    if LastError <> 0 then
    begin
        StatusStr := GetErrorString (LastError);
        exit ;
    end ;

// keep server info
    fPortsFree := RasServer0.TotalPorts ;
    fPortsInuse := RasServer0.PortsInuse ;
    fRasAdmVer := RasServer0.RasVersion ;

// now enumerate ports
    EntriesRead := 0 ;
    ppRasPort0 := Nil ;
    if fRasServer = '' then
        LastError := RasAdminPortEnum (Nil, @ppRasPort0, EntriesRead)
    else
        LastError := RasAdminPortEnum (fWideRasServer, @ppRasPort0, EntriesRead) ;
    Result := LastError;

// parse ports,
    if EntriesRead > TotServPorts then EntriesRead := TotServPorts ;
    fPortsTotal := EntriesRead ;  // total ports
    if EntriesRead <> 0 then
    begin
        curpRasPort0 := ppRasPort0 ;
        for I := 0 to Pred (EntriesRead) do
        begin
            Move (curpRasPort0^, RasPort0, Sizeof (TRasPort0)) ;  // debugger prefers static stuff
            inc (curpRasPort0, sizeof (TRasPort0)) ;
            with Ports [I], RasPort0 do
            begin
                PortName := wszPortName ;
                DeviceType := wszDeviceType ;
                DeviceName := wszDeviceName ;  // warning, make be corrupted with media name
                MediaName := wszMediaName ;
                bFlags := Flags ;
                UserName := wszUserName ;
                Computer := wszComputer ;
                if dwStartSessionTime <> 0 then
                    StartDT := TStamptoDT (dwStartSessionTime)
                else
                    StartDT := 0 ;
                LogonDomain := wszLogonDomain ;
                fAdvancedServer := fAdvancedServer ;
            // clear stuff filled in later by GetPortInfo
                LineCond := 0 ;
                HardwareCond := 0 ;
                Speed := 0 ;
                FillChar (Statistics, SizeOf (TRasPortStatistics), #0) ;
                Projection := 0 ;
                IPAddress := '' ;
            end ;
        end ;
        if ppRasPort0 <> Nil then RasAdminFreeBuffer (ppRasPort0) ;
    end ;
end ;

function TMagRasAdm.GetErrorString(ErrorCode: LongInt): String;
var
    szErrorString: Array[0..256] of WideChar;
begin
    if (NOT MagLoadRasAdmApi) then
    begin
        LastError := ERROR_DLL_NOT_FOUND ;
        result := RASSAPI_DLL + #32 + SRasGenNotAvailable ;  // Not Available
        exit ;
    end ;
    Result := '';
    FillChar (szErrorString, SizeOf (szErrorString), #0);
    RasAdminGetErrorString (ErrorCode, szErrorString, 256);
    If szErrorString[0] <> #0 THEN
        Result := WideCharLenToString (szErrorString, 256)
    Else
        Result := SysErrorMessage (ErrorCode) ;  // Angus, try a windows error
end;

function TMagRasAdm.ClearStats (PortName: string): integer ;
var
    wideport: array [0..16] of WideChar ;
begin
    if (NOT MagLoadRasAdmApi) then
    begin
        LastError := ERROR_DLL_NOT_FOUND ;
        result := LastError ;
        exit ;
    end ;
    StringToWideChar (PortName, wideport, 16) ;
    if fRasServer = '' then
        LastError := RasAdminPortClearStatistics (Nil, wideport)
    else
        LastError := RasAdminPortClearStatistics (fWideRasServer, wideport) ;
    Result := LastError;
    if LastError <> 0 then StatusStr := GetErrorString (LastError);
end ;

function TMagRasAdm.Disconnect (PortName: string): integer ;
var
    wideport: array [0..16] of WideChar ;
begin
    if (NOT MagLoadRasAdmApi) then
    begin
        LastError := ERROR_DLL_NOT_FOUND ;
        result := LastError ;
        exit ;
    end ;
    StringToWideChar (PortName, wideport, 16) ;
    if fRasServer = '' then
        LastError := RasAdminPortDisconnect (Nil, wideport)
    else
        LastError := RasAdminPortDisconnect (fWideRasServer, wideport) ;
    Result := LastError;
    if LastError <> 0 then StatusStr := GetErrorString (LastError);
end ;

function TMagRasAdm.GetPortInfo (PortName: string): integer ;
var
    wideport: array [0..16] of WideChar ;
    RasPort1: TRasPort1 ;
    RasPortStatistics: TRasPortStatistics ;
    I: integer ;
    ppRasParams: PChar ;
//    RasParameters: TRasParameters ;
//    curpRasParams: PChar ;
//    TotMedia, SizeMedia, J: integer ;
begin
// fill main array with all ports first, if not done
    if fPortsTotal = 0 then
    begin
        result := CheckAllPorts ;
        if result <> 0 then exit ;
    end ;

// now add more stuff for this port
    StringToWideChar (PortName, wideport, 16) ;
    ppRasParams := Nil ;
    if fRasServer = '' then
        LastError := RasAdminPortGetInfo (Nil, wideport, RasPort1,
                                        RasPortStatistics, @ppRasParams)
    else
        LastError := RasAdminPortGetInfo (fWideRasServer, wideport, RasPort1,
                                        RasPortStatistics, @ppRasParams) ;
    Result := LastError;
    if LastError <> 0 then
    begin
        StatusStr := GetErrorString (LastError);
        exit ;
    end ;

// look for correct port record to update
    for I := 0 to Pred (fPortsTotal) do
    begin
        if PortName = Ports [I].PortName then
        begin
       // port info
            with Ports [I], RasPort1 do
            begin
                if rasport0.dwStartSessionTime <> 0 then
                    StartDT := TStamptoDT (rasport0.dwStartSessionTime)
                else
                    StartDT := 0 ;
                LineCond := LineCondition ;
                HardwareCond := HardwareCondition ;
                Speed := LineSpeed ;
                Move (RasPortStatistics, Statistics, SizeOf (TRasPortStatistics)) ;
//                TotMedia := NumMediaParms ;
//                SizeMedia := SizeMediaParms ;
                Projection := ProjResult.ip.dwError ;
                if Projection = 0 then
                    IPAddress := ProjResult.ip.wszAddress
                else
                    IPAddress := '' ;
            end ;

       // media specific info, array of structures, each with keyname and data
       // does not seem to have anything particularly interesting...
          {  if TotMedia <> 0 then
            begin
                curpRasParams := ppRasParams ;
                for J := 0 to Pred (TotMedia) do
                begin
                    if SizeMedia < sizeof (TRasParameters) then break ;
                    Move (curpRasParams^, RasParameters, Sizeof (TRasParameters)) ;
                    inc (curpRasParams, sizeof (TRasParameters)) ;
                    dec (SizeMedia, sizeof (TRasParameters)) ;
                    with Ports [I], RasParameters do
                    begin
                        if P_Key = 'xx' then
                        begin
                            if P_Type = ParamString then
                                        xxx := PValue.LString.Data ;
                        end ;
                    end ;
                end ;
            end ; }
        end ;
    end ;
    if ppRasParams <> Nil then RasAdminFreeBuffer (ppRasParams) ;
end ;

// get text for RasPort1.HardwareCondition

function TMagRasAdm.GetHardwareMess (state: integer): String ;
begin
    Result := '' ;
    case state of
        RAS_MODEM_OPERATIONAL:        Result :=  SRasModemOperational ;
        RAS_MODEM_NOT_RESPONDING:     Result :=  SRasModemNotResponding ;
        RAS_MODEM_HARDWARE_FAILURE:   Result :=  SRasModemHardwareFailure ;
        RAS_MODEM_INCORRECT_RESPONSE: Result :=  SRasModemIncorrectResponse ;
        RAS_MODEM_UNKNOWN:            Result :=  SRasModemUnknown ;
    end ;
end ;

// get text for RasPort1.LineCondition

function TMagRasAdm.GetLineMess (state: integer): String ;
begin
    Result := '' ;
    case state of
        RAS_PORT_NON_OPERATIONAL: Result := SRasPortNonOperational ;
        RAS_PORT_DISCONNECTED:    Result := SRasPortDisconnected ;
        RAS_PORT_CALLING_BACK:    Result := SRasPortCallingBack ;
        RAS_PORT_LISTENING:       Result := SRasPortListening ;
        RAS_PORT_AUTHENTICATING:  Result := SRasPortAuthenicating ;
        RAS_PORT_AUTHENTICATED:   Result := SRasPortAuthenticated ;
        RAS_PORT_INITIALIZING:    Result := SRasPortInitalising ;
    end ;
end ;

function TMagRasAdm.GetUserServer: integer ;
var
    fWideDomain: array [0..CNLEN + 2] of WideChar ;
begin
    if (NOT MagLoadRasAdmApi) then
    begin
        LastError := ERROR_DLL_NOT_FOUND ;
        result := LastError ;
        exit ;
    end ;
    fWideDomain [0] := #0 ;
    fWideUserServer [0] := #0 ;
    fUserServer := '' ;
    if fServerDomain <> '' then
            StringToWideChar (fServerDomain, fWideDomain, CNLEN + 2) ;
    if fRasServer = '' then
        LastError := RasAdminGetUserAccountServer (Nil, fWideDomain,
                                                        fWideUserServer)
    else
    begin
        LastError := RasAdminGetUserAccountServer (fWideRasServer, fWideDomain,
                                                            fWideUserServer) ;
    end ;
    Result := LastError;
    if LastError <> 0 then
    begin
        StatusStr := GetErrorString (LastError);
        exit ;
    end ;
    fUserServer := fWideUserServer ;
end ;

function TMagRasAdm.GetUserInfo (user: string; var bPrivilege: byte;
                                            var PhoneNum: string): integer ;
var
    fWideUser: array [0..UNLEN + 2] of WideChar ;
    RasUser0: TRasUser0 ;
begin
    if (NOT MagLoadRasAdmApi) then
    begin
        LastError := ERROR_DLL_NOT_FOUND ;
        result := LastError ;
        exit ;
    end ;
    bPrivilege := 0 ;
    PhoneNum := '' ;
    FillChar (RasUser0, SizeOf (TRasUser0), #0);
    LastError := RasAdminUserGetInfo (fWideUserServer, StringToWideChar
                                    (user, fWideUser, UNLEN + 2), RasUser0) ;
    Result := LastError;
    if LastError <> 0 then
    begin
        StatusStr := GetErrorString (LastError);
        exit ;
    end ;
    bPrivilege := RasUser0.bfPrivilege ;
    PhoneNum := RasUser0.szPhoneNumber ;
end ;

function TMagRasAdm.SetUserInfo (user: string; bPrivilege: byte;
                                            PhoneNum: string): integer ;
var
    fWideUser: array [0..UNLEN + 2] of WideChar ;
    RasUser0: TRasUser0 ;
begin
    if (NOT MagLoadRasAdmApi) then
    begin
        LastError := ERROR_DLL_NOT_FOUND ;
        result := LastError ;
        exit ;
    end ;
    FillChar (RasUser0, SizeOf (TRasUser0), #0);
    RasUser0.bfPrivilege := bPrivilege ;
    StringToWideChar (PhoneNum, RasUser0.szPhoneNumber,
                                 RASSAPI_MAX_PHONENUMBER_SIZE + 2) ;
    LastError := RasAdminUserGetInfo (fWideUserServer, StringToWideChar
                                    (user, fWideUser, UNLEN + 2), RasUser0) ;
    Result := LastError;
    if LastError <> 0 then
    begin
        StatusStr := GetErrorString (LastError);
        exit ;
    end ;
end ;


Initialization
    MagRasAdmLib := 0 ;
    MagRasAdmAPI_Loaded := false ;
finalization
    if MagRasAdmAPI_Loaded then FreeLibrary (MagRasAdmLib) ;
end.

