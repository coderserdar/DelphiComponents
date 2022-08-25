unit magrasadm2 ;
{$IFNDEF VER140}
  {$WARN UNSAFE_TYPE off}
  {$WARN UNSAFE_CAST off}
  {$WARN UNSAFE_CODE off}
{$ENDIF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}

// this component includes comments formatted for creation of a help file
// using F1HELP.  // is ignored, { } is used for help, (* *) is ignored

interface

uses
  SysUtils, Windows, Messages, Classes, MagRasApi, MagRasStr, MagSubs1 ;

const
  MagVersion = 'TMagRasAdm2, 9th August 2011 - Release 5.70, Copyright 2011, Magenta Systems Ltd' ;
(*Updated by Angus Robertson, Magenta Systems Ltd, England
in 2010, delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

Compatible with Delphi 3, 4, 5, 6, 7, 2005 and 2006 tested with
Windows 95, 98, NT 4.0 and Windows 2000, XP and 2003

Compatible with Delphi 4, 5 and 6 tested with NT4, W2K
Note - uses dynamic arrays so not available on Delphi 3

The RasAdmin APIs are designed to monitor incoming RAS connections to
NT4 Server, they don't work with NT4 Workstation incoming calls.

They provide similar functionality to the Remote Access Admin application,
showing which ports are configured to answer calls, and the status of
those in-use.

The RRAS (Routing and Remote Access Service) APIs are supported by TMagRasAdm
but have not been fully tested since our Windows 2000 with RRAS installed
does not respond to these APIs.    So this is not a fully working unit but
a work in progress.  RRAS is supported primarily on Windows 2000 and 2003,
it may be installed on NT4 but all the exported function names are different
so this unit will not currently work.

4th August 2008 - Release 5.40 - made compatible with Delphi 2009

Release 5.60 - fixed cast warnings for Delphi 2009 and later

Changes in 5.70
RegisterComponent now in magrasreg

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
  RAS_PORT_DISCONNECTED	    = 2 ;
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
	Data: PAnsiChar ;
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

// -------------------------------------------------------------------------------

{
Copyright (c) Microsoft Corporation. All rights reserved.

Module Name:

    mpradmin.h

Abstract:

    This file contains the structures, defines and function prototypes for the
    following APIs:

        MprAdminIsServiceRunning
        MprAdminServerConnect
        MprAdminServerDisconnect
        MprAdminBufferFree
        MprAdminPortEnum
        MprAdminConnectionEnum
        MprAdminPortGetInfo
        MprAdminConnectionGetInfo
        MprAdminPortClearStats
        MprAdminPortReset
        MprAdminConnectionClearStats
        MprAdminPortDisconnect
        MprAdminGetErrorString

        MprAdminAcceptNewConnection
        MprAdminAcceptNewLink
        MprAdminConnectionHangupNotification
        MprAdminLinkHangupNotification
        MprAdminGetIpAddressForUser
        MprAdminReleaseIpAddress
        MprAdminInitializeDll
        MprAdminTerminateDll
        MprAdminAcceptNewConnection2
        MprAdminConnectionHangupNotification2

        MprAdminUserGetInfo
        MprAdminUserSetInfo
        MprAdminSendUserMessage
        MprAdminGetPDCServer

        MprAdminRegisterConnectionNotification
        MprAdminDeregisterConnectionNotification

        MprAdminIsServiceRunning
        MprAdminServerConnect
        MprAdminServerDisconnect
        MprAdminBufferFree
        MprAdminServerGetInfo
        MprAdminGetErrorString
   (more stuff ignored for the moment)

MprAdmin functions are for Windows 2000 and .NET server only
Similar functions are available NT4 RRAS but with different names and are not supported
}

const
  RRAS_SERVICE_NAME      = 'RemoteAccess' ;

// Protocol IDs
  PID_IPX                 = $0000002B ;
  PID_IP                  = $00000021 ;
  PID_NBF                 = $0000003F ;
  PID_ATALK               = $00000029 ;

  MAX_INTERFACE_NAME_LEN  = 256 ;
  MAX_TRANSPORT_NAME_LEN  = 40 ;
  MAX_MEDIA_NAME          = 16 ;
  MAX_PORT_NAME           = 16 ;
  MAX_DEVICE_NAME         = 128 ;
  MAX_PHONE_NUMBER_LEN    = 128 ;
  MAX_DEVICETYPE_NAME     = 16 ;

// MPR Interface structures and definitions.

// MPR Interface types

type
  TROUTER_INTERFACE_TYPE = (
        ROUTER_IF_TYPE_CLIENT,
        ROUTER_IF_TYPE_HOME_ROUTER,
        ROUTER_IF_TYPE_FULL_ROUTER,
        ROUTER_IF_TYPE_DEDICATED,
        ROUTER_IF_TYPE_INTERNAL,
        ROUTER_IF_TYPE_LOOPBACK,
        ROUTER_IF_TYPE_TUNNEL1,
        ROUTER_IF_TYPE_DIALOUT) ;

  TROUTER_CONNECTION_STATE = (
        ROUTER_IF_STATE_UNREACHABLE,
        ROUTER_IF_STATE_DISCONNECTED,
        ROUTER_IF_STATE_CONNECTING,
        ROUTER_IF_STATE_CONNECTED) ;

const
  MPR_INTERFACE_OUT_OF_RESOURCES              = $00000001 ;
  MPR_INTERFACE_ADMIN_DISABLED                = $00000002 ;
  MPR_INTERFACE_CONNECTION_FAILURE            = $00000004 ;
  MPR_INTERFACE_SERVICE_PAUSED                = $00000008 ;
  MPR_INTERFACE_DIALOUT_HOURS_RESTRICTION     = $00000010 ;
  MPR_INTERFACE_NO_MEDIA_SENSE                = $00000020 ;
  MPR_INTERFACE_NO_DEVICE                     = $00000040 ;

type
  PMPR_INTERFACE_0 = ^TMPR_INTERFACE_0 ;
  _MPR_INTERFACE_0 = record
    wszInterfaceName: packed array [0..MAX_INTERFACE_NAME_LEN] of WideChar ;
    hInterface: THANDLE ;
    fEnabled: BOOLEAN ;
    dwIfType: TROUTER_INTERFACE_TYPE ;
    dwConnectionState: TROUTER_CONNECTION_STATE ;
    fUnReachabilityReasons: DWORD ;
    dwLastError: DWORD ;
  end ;
  TMPR_INTERFACE_0 = _MPR_INTERFACE_0 ;

(*  Following stuff is probably only used for outgoing dial-on demand router calls

typedef struct _MPR_IPINIP_INTERFACE_0
{
    WCHAR   wszFriendlyName[MAX_INTERFACE_NAME_LEN+1];

    GUID    Guid;

}MPR_IPINIP_INTERFACE_0, *PMPR_IPINIP_INTERFACE_0;

#if(WINVER >= 0x0500)

typedef struct _MPR_INTERFACE_1
{
    IN OUT  WCHAR                   wszInterfaceName[MAX_INTERFACE_NAME_LEN+1];
    OUT     HANDLE                  hInterface;
    IN OUT  BOOL                    fEnabled;
    IN OUT  ROUTER_INTERFACE_TYPE   dwIfType;
    OUT     ROUTER_CONNECTION_STATE dwConnectionState;
    OUT     DWORD                   fUnReachabilityReasons;
    OUT     DWORD                   dwLastError;
    OUT     LPWSTR                  lpwsDialoutHoursRestriction;

}
MPR_INTERFACE_1, *PMPR_INTERFACE_1;


//
// MPR_INTERFACE_2 definitions
//

#define MPR_MaxDeviceType     RAS_MaxDeviceType
#define MPR_MaxPhoneNumber    RAS_MaxPhoneNumber
#define MPR_MaxIpAddress      RAS_MaxIpAddress
#define MPR_MaxIpxAddress     RAS_MaxIpxAddress

#define MPR_MaxEntryName      RAS_MaxEntryName
#define MPR_MaxDeviceName     RAS_MaxDeviceName
#define MPR_MaxCallbackNumber RAS_MaxCallbackNumber

#define MPR_MaxAreaCode       RAS_MaxAreaCode
#define MPR_MaxPadType        RAS_MaxPadType
#define MPR_MaxX25Address     RAS_MaxX25Address
#define MPR_MaxFacilities     RAS_MaxFacilities
#define MPR_MaxUserData       RAS_MaxUserData

//
// MPR_INTERFACE_2 'dwfOptions' bit flags.
//

#define MPRIO_SpecificIpAddr            RASEO_SpecificIpAddr
#define MPRIO_SpecificNameServers       RASEO_SpecificNameServers
#define MPRIO_IpHeaderCompression       RASEO_IpHeaderCompression
#define MPRIO_RemoteDefaultGateway      RASEO_RemoteDefaultGateway
#define MPRIO_DisableLcpExtensions      RASEO_DisableLcpExtensions
#define MPRIO_SwCompression             RASEO_SwCompression
#define MPRIO_RequireEncryptedPw        RASEO_RequireEncryptedPw
#define MPRIO_RequireMsEncryptedPw      RASEO_RequireMsEncryptedPw
#define MPRIO_RequireDataEncryption     RASEO_RequireDataEncryption
#define MPRIO_NetworkLogon              RASEO_NetworkLogon
#define MPRIO_PromoteAlternates         RASEO_PromoteAlternates
#define MPRIO_SecureLocalFiles          RASEO_SecureLocalFiles
#define MPRIO_RequireEAP                RASEO_RequireEAP
#define MPRIO_RequirePAP                RASEO_RequirePAP
#define MPRIO_RequireSPAP               RASEO_RequireSPAP
#define MPRIO_SharedPhoneNumbers        RASEO_SharedPhoneNumbers
#define MPRIO_RequireCHAP               RASEO_RequireCHAP
#define MPRIO_RequireMsCHAP             RASEO_RequireMsCHAP
#define MPRIO_RequireMsCHAP2            RASEO_RequireMsCHAP2

#if (WINVER >= 0x501)
#define MPRIO_IpSecPreSharedKey         0x80000000
#endif

//
// MPR_INTERFACE_2 'dwProtocols' bit flags.
//

#define MPRNP_Ipx                       RASNP_Ipx
#define MPRNP_Ip                        RASNP_Ip

//
// MPR_INTERFACE_2 'szDeviceType' default strings.
//

#define MPRDT_Modem                     RASDT_Modem
#define MPRDT_Isdn                      RASDT_Isdn
#define MPRDT_X25                       RASDT_X25
#define MPRDT_Vpn                       RASDT_Vpn
#define MPRDT_Pad                       RASDT_Pad
#define MPRDT_Generic                   RASDT_Generic
#define MPRDT_Serial        			RASDT_Serial
#define MPRDT_FrameRelay                RASDT_FrameRelay
#define MPRDT_Atm                       RASDT_Atm
#define MPRDT_Sonet                     RASDT_Sonet
#define MPRDT_SW56                      RASDT_SW56
#define MPRDT_Irda                      RASDT_Irda
#define MPRDT_Parallel                  RASDT_Parallel

//
// MPR_INTERFACE_2 'dwType' settings
//

#define MPRET_Phone    RASET_Phone
#define MPRET_Vpn      RASET_Vpn
#define MPRET_Direct   RASET_Direct

//
// MPR_INTERFACE_2 'dwDialMode' values.
//

#define MPRDM_DialFirst                0
#define MPRDM_DialAll                  RASEDM_DialAll
#define MPRDM_DialAsNeeded             RASEDM_DialAsNeeded

//
// MPR_INTERFACE_2 'dwIdleDisconnectSeconds' constants.
//

#define MPRIDS_Disabled                 RASIDS_Disabled
#define MPRIDS_UseGlobalValue           RASIDS_UseGlobalValue

//
// MPR_INTERFACE_2 encryption types.
//

#define MPR_ET_None         ET_None
#define MPR_ET_Require      ET_Require
#define MPR_ET_RequireMax   ET_RequireMax
#define MPR_ET_Optional     ET_Optional

//
// MPR_INTERFACE_2 Vpn strategies
//

#define MPR_VS_Default		VS_Default
#define MPR_VS_PptpOnly	    VS_PptpOnly
#define MPR_VS_PptpFirst	VS_PptpFirst
#define MPR_VS_L2tpOnly 	VS_L2tpOnly
#define MPR_VS_L2tpFirst	VS_L2tpFirst

//
// Used to create/get/set a demand dial interface plus its
// ras configuration.
//

typedef struct _MPR_INTERFACE_2
{
    IN OUT  WCHAR                   wszInterfaceName[MAX_INTERFACE_NAME_LEN+1];
    OUT     HANDLE                  hInterface;
    IN OUT  BOOL                    fEnabled;
    IN OUT  ROUTER_INTERFACE_TYPE   dwIfType;
    OUT     ROUTER_CONNECTION_STATE dwConnectionState;
    OUT     DWORD                   fUnReachabilityReasons;
    OUT     DWORD                   dwLastError;
   //
    // Demand dial-specific properties
    //
    DWORD       dwfOptions;
    //
    // Location/phone number
    //
    WCHAR       szLocalPhoneNumber[ RAS_MaxPhoneNumber + 1 ];
    PWCHAR      szAlternates;
    //
    // PPP/Ip
    //
    DWORD       ipaddr;
    DWORD       ipaddrDns;
    DWORD       ipaddrDnsAlt;
    DWORD       ipaddrWins;
    DWORD       ipaddrWinsAlt;
    //
    // NetProtocols
    //
    DWORD       dwfNetProtocols;
    //
    // Device
    //
    WCHAR       szDeviceType[ MPR_MaxDeviceType + 1 ];
    WCHAR       szDeviceName[ MPR_MaxDeviceName + 1 ];
    //
    // X.25
    //

    WCHAR       szX25PadType[ MPR_MaxPadType + 1 ];
    WCHAR       szX25Address[ MPR_MaxX25Address + 1 ];
    WCHAR       szX25Facilities[ MPR_MaxFacilities + 1 ];
    WCHAR       szX25UserData[ MPR_MaxUserData + 1 ];
    DWORD       dwChannels;
    //
    // Multilink
    //
    DWORD       dwSubEntries;
    DWORD       dwDialMode;
    DWORD       dwDialExtraPercent;
    DWORD       dwDialExtraSampleSeconds;
    DWORD       dwHangUpExtraPercent;
    DWORD       dwHangUpExtraSampleSeconds;
    //
    // Idle timeout
    //
    DWORD       dwIdleDisconnectSeconds;
    //
    // Entry Type
    //
    DWORD       dwType;
   //
    // EncryptionType
    //
    DWORD       dwEncryptionType;
    //
    // EAP information
    //
    DWORD       dwCustomAuthKey;
    DWORD       dwCustomAuthDataSize;
    LPBYTE      lpbCustomAuthData;
    //
    // Guid of the connection
    //
    GUID        guidId;
    //
    // Vpn Strategy
    //
    DWORD       dwVpnStrategy;
} MPR_INTERFACE_2, *PMPR_INTERFACE_2;

//
// Used to set/get per-link information for multilinked demand
// dial interfaces.
//

typedef struct _MPR_DEVICE_0
{
    //
    // Device
    //
    WCHAR       szDeviceType[ MPR_MaxDeviceType + 1 ];
    WCHAR       szDeviceName[ MPR_MaxDeviceName + 1 ];
}
MPR_DEVICE_0, *PMPR_DEVICE_0;

typedef struct _MPR_DEVICE_1
{
    //
    // Device
    //
    WCHAR       szDeviceType[ MPR_MaxDeviceType + 1 ];
    WCHAR       szDeviceName[ MPR_MaxDeviceName + 1 ];
    //
    // Phone numbers
    //
    WCHAR       szLocalPhoneNumber[ MPR_MaxPhoneNumber + 1 ];
    PWCHAR      szAlternates;
}
MPR_DEVICE_1, *PMPR_DEVICE_1;

//
// Used to get/set extended credentials information such as
// eap credentials info.
//

typedef struct _MPR_CREDENTIALSEX_0
{
    DWORD  dwSize;
    LPBYTE lpbCredentialsInfo;
}
MPR_CREDENTIALSEX_0, *PMPR_CREDENTIALSEX_0;

typedef struct _MPR_CREDENTIALSEX_1
{
    DWORD  dwSize;
    LPBYTE lpbCredentialsInfo;
}
MPR_CREDENTIALSEX_1, *PMPR_CREDENTIALSEX_1;

#endif /* WINVER >= 0x0500 */

typedef struct _MPR_TRANSPORT_0
{
    OUT     DWORD                   dwTransportId;
    OUT     HANDLE                  hTransport;
    OUT     WCHAR                   wszTransportName[MAX_TRANSPORT_NAME_LEN+1];
}

MPR_TRANSPORT_0, *PMPR_TRANSPORT_0;

typedef struct _MPR_IFTRANSPORT_0
{
    OUT     DWORD                  dwTransportId;
    OUT     HANDLE                 hIfTransport;
    OUT     WCHAR                  wszIfTransportName[MAX_TRANSPORT_NAME_LEN+1];
}
MPR_IFTRANSPORT_0, *PMPR_IFTRANSPORT_0;
*)


type

  PMPR_SERVER_0 = ^TMPR_SERVER_0 ;
  _MPR_SERVER_0 = record
    fLanOnlyMode: boolean ;
    dwUpTime: DWORD ;            // in seconds
    dwTotalPorts: DWORD ;
    dwPortsInUse: DWORD ;
  end ;
  TMPR_SERVER_0 = _MPR_SERVER_0;

// Port condition codes

  TRAS_PORT_CONDITION = (
    RRAS_PORT_NON_OPERATIONAL,    // avoid duplicates
    RRAS_PORT_DISCONNECTED,
    RRAS_PORT_CALLING_BACK,
    RRAS_PORT_LISTENING,
    RRAS_PORT_AUTHENTICATING,
    RRAS_PORT_AUTHENTICATED,
    RRAS_PORT_INITIALIZING) ;

// Hardware condition codes

  TRAS_HARDWARE_CONDITION = (
    RAS_HARDWARE_OPERATIONAL,
    RAS_HARDWARE_FAILURE) ;

  PRRAS_PORT_0 = ^TRRAS_PORT_0;
  _RRAS_PORT_0 = record     // NOTE: renamed from _RAS_PORT_0 to avoid conflict with NT4 version which is different
    hPort: THANDLE ;
    hConnection: THANDLE ;
    dwPortCondition: TRAS_PORT_CONDITION ;
    dwTotalNumberOfCalls: DWORD ;
    dwConnectDuration: DWORD ;
    wszPortName: packed array [0..MAX_PORT_NAME] of WideChar ;
    wszMediaName: packed array [0..MAX_MEDIA_NAME] of WideChar ;
    wszDeviceName: packed array [0..MAX_DEVICE_NAME] of WideChar ;
    wszDeviceType: packed array [0..MAX_DEVICETYPE_NAME] of WideChar ;
  end ;
  TRRAS_PORT_0 = _RRAS_PORT_0 ;
  RRAS_PORT_0 = _RRAS_PORT_0 ;

  PRRAS_PORT_1 = ^TRRAS_PORT_1;
  _RRAS_PORT_1 = record     // NOTE: renamed from _RAS_PORT_1 to avoid conflict with NT4 version which is different
    hPort: THANDLE ;
    hConnection: THANDLE ;
    dwHardwareCondition: TRAS_HARDWARE_CONDITION ;
    dwLineSpeed: DWORD ;
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
    dwCompressionRatioIn: DWORD ;
    dwCompressionRatioOut: DWORD ;
  end ;
  TRRAS_PORT_1 = _RRAS_PORT_1 ;
  RRAS_PORT_1 = _RRAS_PORT_1 ;

// Maximum length of address string, e.g. "255.255.255.255" for IP.

const
  IPADDRESSLEN      = 15 ;
  IPXADDRESSLEN     = 22 ;
  ATADDRESSLEN      = 32 ;

type
  _PPP_NBFCP_INFO = record
    dwError: DWORD ;
    wszWksta: packed array [0..NETBIOS_NAME_LEN] of WideChar ;
  end ;
  TPPP_NBFCP_INFO = _PPP_NBFCP_INFO ;

  _PPP_IPCP_INFO = record
    dwError: DWORD ;
    wszAddress: packed array [0..IPADDRESSLEN] of WideChar ;
    wszRemoteAddress: packed array [0..IPADDRESSLEN] of WideChar ;
  end ;
  TPPP_IPCP_INFO = _PPP_IPCP_INFO ;

// PPP_IPCP_INFO2 dwOptions values.

const
    PPP_IPCP_VJ =   $00000001 ;

type
  _PPP_IPCP_INFO2 = record
    dwError: DWORD ;
    wszAddress: packed array [0..IPADDRESSLEN] of WideChar ;
    wszRemoteAddress: packed array [0..IPADDRESSLEN] of WideChar ;
    dwOptions: DWORD ;
    dwRemoteOptions: DWORD ;
  end ;
  TPPP_IPCP_INFO2 = _PPP_IPCP_INFO2;

  _PPP_IPXCP_INFO = record
    dwError: DWORD ;
    wszAddress: packed array [0..IPXADDRESSLEN] of WideChar ;
  end ;
  TPPP_IPXCP_INFO = _PPP_IPXCP_INFO ;

  _PPP_ATCP_INFO = record
    dwError: DWORD ;
    wszAddress: packed array [0..ATADDRESSLEN] of WideChar ;
  end ;
  TPPP_ATCP_INFO = _PPP_ATCP_INFO;

  _PPP_INFO = record
    nbf: TPPP_NBFCP_INFO ;
    ip: TPPP_IPCP_INFO ;
    ipx: TPPP_IPXCP_INFO ;
    at: TPPP_ATCP_INFO ;
  end ;
  TPPP_INFO =_PPP_INFO ;

// PPP_CCP dwCompressionAlgorithm values.
const
  RASCCPCA_MPPC         = $00000006 ;
  RASCCPCA_STAC         = $00000005 ;

// PPP_CCP dwOptions values.

  PPP_CCP_COMPRESSION         = $00000001 ;
  PPP_CCP_ENCRYPTION40BITOLD  = $00000010 ;
  PPP_CCP_ENCRYPTION40BIT     = $00000020 ;
  PPP_CCP_ENCRYPTION128BIT    = $00000040 ;
  PPP_CCP_ENCRYPTION56BIT     = $00000080 ;
  PPP_CCP_HISTORYLESS         = $01000000 ;

type
  _PPP_CCP_INFO = record
    dwError: DWORD ;
    dwCompressionAlgorithm: DWORD ;
    dwOptions: DWORD ;
    dwRemoteCompressionAlgorithm: DWORD ;
    dwRemoteOptions: DWORD ;
  end ;
  TPPP_CCP_INFO = _PPP_CCP_INFO ;

// PPP_LCP dwAuthenticatonProtocol values.
const
  PPP_LCP_PAP          = $0C023 ;
  PPP_LCP_SPAP         = $0C123 ;
  PPP_LCP_CHAP         = $0C223 ;
  PPP_LCP_EAP          = $0C227 ;

// PPP_LCP dwAuthenticatonData values.

  PPP_LCP_CHAP_MD5     = $05 ;
  PPP_LCP_CHAP_MS      = $80 ;
  PPP_LCP_CHAP_MSV2    = $81 ;

// PPP_LCP dwOption values

  PPP_LCP_MULTILINK_FRAMING   = $00000001 ;
  PPP_LCP_PFC                 = $00000002 ;
  PPP_LCP_ACFC                = $00000004 ;
  PPP_LCP_SSHF                = $00000008 ;
  PPP_LCP_DES_56              = $00000010 ;
  PPP_LCP_3_DES               = $00000020 ;

type
  _PPP_LCP_INFO = record
    dwError: DWORD ;
    dwAuthenticationProtocol: DWORD ;
    dwAuthenticationData: DWORD ;
    dwRemoteAuthenticationProtocol: DWORD ;
    dwRemoteAuthenticationData: DWORD ;
    dwTerminateReason: DWORD ;
    dwRemoteTerminateReason: DWORD ;
    dwOptions: DWORD ;
    dwRemoteOptions: DWORD ;
    dwEapTypeId: DWORD ;
    dwRemoteEapTypeId: DWORD ;
  end ;
  TPPP_LCP_INFO = _PPP_LCP_INFO ;

  _PPP_INFO_2 = record
    nbf: TPPP_NBFCP_INFO ;
    ip: TPPP_IPCP_INFO2 ;
    ipx: TPPP_IPXCP_INFO ;
    at: TPPP_ATCP_INFO ;
    ccp: TPPP_CCP_INFO ;
    lcp: TPPP_LCP_INFO ;
  end ;
  TPPP_INFO_2 = _PPP_INFO_2 ;

// Possible bits set in Connection Flags field
const
 RAS_FLAGS_PPP_CONNECTION        = $00000001 ;
 RAS_FLAGS_MESSENGER_PRESENT     = $00000002 ;
 RAS_FLAGS_RAS_CONNECTION        = $00000004 ;

type
  PRAS_CONNECTION_0 = ^TRAS_CONNECTION_0 ;
  _RAS_CONNECTION_0 = record
    hConnection: THANDLE ;
    hInterface: THANDLE ;
    dwConnectDuration: DWORD ;
    dwInterfaceType: TROUTER_INTERFACE_TYPE ;
    dwConnectionFlags: DWORD ;
    wszInterfaceName: packed array [0..MAX_INTERFACE_NAME_LEN] of WideChar ;
    wszUserName: packed array [0..UNLEN] of WideChar ;
    wszLogonDomain: packed array [0..UNLEN] of WideChar ;
    wszRemoteComputer: packed array [0..NETBIOS_NAME_LEN] of WideChar ;
  end ;
  TRAS_CONNECTION_0 = _RAS_CONNECTION_0 ;

  PRAS_CONNECTION_1 = ^TRAS_CONNECTION_1 ;
  _RAS_CONNECTION_1 = record
    hConnection: THANDLE ;
    hInterface: THANDLE ;
    PppInfo: TPPP_INFO ;
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
    dwCompressionRatioIn: DWORD ;
    dwCompressionRatioOut: DWORD ;
  end ;
  TRAS_CONNECTION_1 = _RAS_CONNECTION_1 ;

  PRAS_CONNECTION_2 = ^TRAS_CONNECTION_2 ;
  _RAS_CONNECTION_2 = record
    hConnection: THANDLE ;
    wszUserName: packed array [0..UNLEN] of WideChar ;
    dwInterfaceType: TROUTER_INTERFACE_TYPE ;
    guid: TGUID ;
    PppInfo: TPPP_INFO_2 ;
  end ;
  TRAS_CONNECTION_2 = _RAS_CONNECTION_2 ;

// Structures used by the MPRADMIN USER APIs. Use level 0 to get/set this
// structure.

// Bits indicating user's Remote Access privileges and mask to isolate
// call back privilege.

// Note: Bit 0 MUST represent NoCallback due to a quirk of the "userparms"
//       storage method.  When a new LAN Manager user is created, bit 0 of the
//       userparms field is set to 1 and all other bits are 0.  These bits are
//       arranged so this "no Dial-In info" state maps to the "default Dial-In
//       privilege" state.

const
{ duplicates
  RASPRIV_NoCallback        = $01 ;
  RASPRIV_AdminSetCallback  = $02 ;
  RASPRIV_CallerSetCallback = $04 ;
  RASPRIV_DialinPrivilege   = $08 ;
  RASPRIV_CallbackType = RASPRIV_AdminSetCallback OR
                         RASPRIV_CallerSetCallback OR RASPRIV_NoCallback ;
}

// The following are flags for the bfPrivilege2 member of RAS_USER_1
// structure

  RASPRIV2_DialinPolicy     = $01 ;

type
  PRRAS_USER_0 = ^TRRAS_USER_0 ;
  _RRAS_USER_0 = record
    bfPrivilege: BYTE ;
    wszPhoneNumber: packed array [0..MAX_PHONE_NUMBER_LEN] of WideChar ;
  end ;
  TRRAS_USER_0 = _RRAS_USER_0 ;

  PRRAS_USER_1 = ^TRRAS_USER_1 ;
  _RRAS_USER_1 = record
    bfPrivilege: BYTE ;
    wszPhoneNumber: packed array [0..MAX_PHONE_NUMBER_LEN] of WideChar ;
    bfPrivilege2: BYTE ;
  end ;
  TRRAS_USER_1 = _RRAS_USER_1 ;

// Used as RPC binding handle to server

 TRAS_SERVER_HANDLE = THANDLE ;
 TMPR_SERVER_HANDLE = THANDLE ;
 TMIB_SERVER_HANDLE = THANDLE ;

// RAS/Routing and RAS ADMIN APIs - Windows 2000 and .NET Server only
// NT4 may have them exported with different names

var

MprAdminConnectionEnum: Function (
    const hRasServer: TRAS_SERVER_HANDLE ;
    const dwLevel: DWORD ;
    lplpbBuffer: PAnsiChar ;
    const dwPrefMaxLen: DWORD ;
    var lpdwEntriesRead: DWORD ;
    var lpdwTotalEntries: DWORD ;
    var lpdwResumeHandle: DWORD
    ): DWORD ; stdcall ;

MprAdminPortEnum: Function (
    const hRasServer: TRAS_SERVER_HANDLE ;
    const dwLevel: DWORD ;
    const hConnection: THANDLE ;
    lplpbBuffer: PAnsiCHAR ;
    const dwPrefMaxLen: DWORD ;
    var lpdwEntriesRead: DWORD ;
    var lpdwTotalEntries: DWORD ;
    var lpdwResumeHandle: DWORD
    ): DWORD ; stdcall ;

MprAdminConnectionGetInfo: Function (
    const hRasServer: TRAS_SERVER_HANDLE ;
    const dwLevel: DWORD ;
    const hConnection: THANDLE ;
    lplpbBuffer: PAnsiCHAR
    ): DWORD ; stdcall ;

MprAdminPortGetInfo: Function (
    const hRasServer: TRAS_SERVER_HANDLE ;
    const dwLevel: DWORD ;
    const hPort: THANDLE ;
    lplpbBuffer: PAnsiChar
    ): DWORD ; stdcall ;

MprAdminConnectionClearStats: Function (
    const hRasServer: TRAS_SERVER_HANDLE ;
    const hConnection: THANDLE
    ): DWORD ; stdcall ;

MprAdminPortClearStats: Function (
    const hRasServer: TRAS_SERVER_HANDLE ;
    const hPort: THANDLE
    ): DWORD ; stdcall ;

MprAdminPortReset: Function (
    const hRasServer: TRAS_SERVER_HANDLE ;
    const hPort: THANDLE
    ): DWORD ; stdcall ;

MprAdminPortDisconnect: Function (
    hRasServer: TRAS_SERVER_HANDLE ;
    hPort: THANDLE
    ): DWORD ; stdcall ;

// MprAdminUser APIs

MprAdminUserGetInfo: Function (
    const lpszServer: PWChar ;
    const lpszUser: PWChar ;
    const dwLevel: DWORD ;
    lpbBuffer: PAnsiChar
    ): DWORD ; stdcall ;

MprAdminUserSetInfo: Function (
    const lpszServer: PWChar ;
    const lpszUser: PWChar ;
    const dwLevel: DWORD ;
    lpbBuffer: PAnsiChar
    ): DWORD ; stdcall ;

MprAdminSendUserMessage: Function (
    const hMprServer: TMPR_SERVER_HANDLE ;
    const hConnection: THANDLE ;
    lpwszMessage: PWChar
    ): DWORD ; stdcall ;

MprAdminGetPDCServer: Function (
    const lpszDomain: PWChar ;
    const lpszServer: PWChar ;
    lpszPDCServer: PWChar
    ): DWORD ; stdcall ;

// Router APIs


MprAdminIsServiceRunning: Function (
    const lpwsServerName: PWChar
    ): BOOLEAN ; stdcall ;

MprAdminServerConnect: Function (
    const lpwsServerName: PWChar ;
    var phMprServer: TMPR_SERVER_HANDLE
    ): DWORD ; stdcall ;

MprAdminServerDisconnect: Function (
    const hMprServer: TMPR_SERVER_HANDLE
    ): BOOLEAN ; stdcall ;

MprAdminBufferFree: Procedure (
    pBuffer: PAnsiChar
    ) ; stdcall ;

MprAdminGetErrorString: Function (
    const dwError: DWORD ;
    lpwsErrorString: PWChar
    ): DWORD ; stdcall ;

MprAdminServerGetInfo: Function (
    const hMprServer: TMPR_SERVER_HANDLE ;
    const dwLevel: DWORD ;
    lplpbBuffer: PAnsiChar
    ): DWORD ; stdcall ;


// RAS ADMIN DLL functions - could be used for event logging of dial-in users
// these functions need to be implemented in the admin DLL, not the main program
{
Function MprAdminAcceptNewConnection (
    pRasConnection0: PRAS_CONNECTION_0 ;
    pRasConnection1: PRAS_CONNECTION_1
        ): BOOLEAN ; stdcall ;

Function MprAdminAcceptNewConnection2 (
    pRasConnection0: PRAS_CONNECTION_0 ;
    pRasConnection1: PRAS_CONNECTION_1 ;
    pRasConnection2: PRAS_CONNECTION_2
        ): BOOLEAN ; stdcall ;

Function MprAdminAcceptNewLink  (
    pRasPort0: PRRAS_PORT_0 ;
    pRasPort1: PRRAS_PORT_1
        ): BOOLEAN ; stdcall ;

Procedure MprAdminConnectionHangupNotification (
    pRasConnection0: PRAS_CONNECTION_0 ;
    pRasConnection1: PRAS_CONNECTION_1
        ) ; stdcall ;

Procedure MprAdminConnectionHangupNotification2 (
    pRasConnection0: PRAS_CONNECTION_0 ;
    pRasConnection1: PRAS_CONNECTION_1 ;
    pRasConnection2: PRAS_CONNECTION_2
        ) ; stdcall ;

Procedure MprAdminLinkHangupNotification (
    pRasPort0: PRRAS_PORT_0 ;
    pRasPort1: PRRAS_PORT_1
        ) ; stdcall ;

Function MprAdminGetIpAddressForUser (
    const lpwszUserName: PWChar ;
    const lpwszPortName: PWChar ;
    var lpdwIpAddress: DWORD ;
    var bNotifyRelease: BOOLEAN
        ): DWORD ; stdcall ;

Procedure MprAdminReleaseIpAddress  (
    const lpwszUserName: PWChar ;
    const lpwszPortName: PWChar ;
    const lpdwIpAddress: DWORD
        ); stdcall ;

Procedure MprAdminInitializeDll ; stdcall ;

Procedure MprAdminTerminateDll ; stdcall ;
}

// -------------------------------------------------------------------------------

const
  RASSAPI_DLL = 'RASSAPI.DLL'; // NT4/W2K only, ras server stuff
  MPRAPI_DLL = 'MPRAPI.DLL';   // W2K/.NET only, ras/routing and ras server stuff

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
  // from RRas_Port_0
    hPort: THandle ;
    hConnection: THandle ;
  end;

  TAdminServerType = (ServerUnknown, ServerNT4, ServerW2K) ;


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
    fAdminServerType: TAdminServerType ;
    fMaxPorts: integer ;
    fRRasHandle: TMPR_SERVER_HANDLE ;
    fRasCurServer: string ;

    procedure SetServer (ServerName: string) ;
    procedure SetMaxPorts (value: integer) ;

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

{ Dynamic array of TRasPorts records, each representing one server port. }
    Ports: array of TRasPorts ;

{ Initialises various lists and functions in TMagRasAdm, which allows
  Windows NT4 RAS server to be monitored.  }
    constructor Create(AOwner: TComponent); override;

{ Destroys TMagRasAdm. }
    destructor Destroy; override;

{ Connects to the RAS or RRAS server, and gets the number of ports.
  The RasServer properties should be set before the CheckAllPorts method
  is used, to the computer name to monitor, in the format \\servername.
  The GetCompName function may be used to get the local PC name, but remote
  names may also be specified.

  This method completes various properties: PortsFree is free ports (not on
  calls), PortsInuse is busy ports, RasAdmVer is the Ras Admin Version.

  Result is 0 if successful, or an error code. }
    function ConnectServer: integer ;

{ For Routing/Ras, disconnects the current server, if already connected. }
    procedure DisconnServer ;

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
  Calls ConnectServer.

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

    Property AdminServerType: TAdminServerType read fAdminServerType write fAdminServerType ;

    Property MaxPorts: integer     read fMaxPorts write SetMaxPorts ;

  end;

var
  MagRasAdmLib: THandle = 0 ;
  MagMprAdmLib: THandle = 0 ;
  MagRasAdmAPI_Loaded: Boolean = false ;   // See if NT4 or W2K DLL functions are loaded
  MagMprAdmAPI_Loaded: Boolean = false ;   // See if W2K DLL functions are loaded

  function MagLoadRasAdmApi: boolean ;
//  procedure Register;

implementation

//procedure Register;
//begin
//  RegisterComponents('Magenta Systems', [TMagRasAdm]);
//end;

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
    if MagRasOSVersion = OSW9x then exit ;
    MagMprAdmAPI_Loaded := false ;

// open old NT4 RAS Server Admin library - only come here once
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

// open W2K/.NET RAS Routing Server Admin library
    if MagRasOSVersion >= OSW2K then
    begin
        MagMprAdmLib := LoadLibrary (MPRAPI_DLL) ;
        If MagMprAdmLib = 0 then exit ;

    // set function addresses to versions in DLL, all are Unicode versions!!!!
        MprAdminIsServiceRunning := GetProcAddress (MagMprAdmLib, 'MprAdminIsServiceRunning') ;
        MprAdminServerConnect := GetProcAddress (MagMprAdmLib, 'MprAdminServerConnect') ;
        MprAdminServerDisconnect := GetProcAddress (MagMprAdmLib, 'MprAdminServerDisconnect') ;
        MprAdminBufferFree := GetProcAddress (MagMprAdmLib, 'MprAdminBufferFree') ;
        MprAdminGetErrorString := GetProcAddress (MagMprAdmLib, 'MprAdminGetErrorString') ;
        MprAdminServerGetInfo := GetProcAddress (MagMprAdmLib, 'MprAdminServerGetInfo') ;
        MprAdminConnectionEnum := GetProcAddress (MagMprAdmLib, 'MprAdminConnectionEnum') ;
        MprAdminPortEnum := GetProcAddress (MagMprAdmLib, 'MprAdminPortEnum') ;
        MprAdminConnectionGetInfo := GetProcAddress (MagMprAdmLib, 'MprAdminConnectionGetInfo') ;
        MprAdminPortGetInfo := GetProcAddress (MagMprAdmLib, 'MprAdminPortGetInfo') ;
        MprAdminConnectionClearStats := GetProcAddress (MagMprAdmLib, 'MprAdminConnectionClearStats') ;
        MprAdminPortClearStats := GetProcAddress (MagMprAdmLib, 'MprAdminPortClearStats') ;
        MprAdminPortReset := GetProcAddress (MagMprAdmLib, 'MprAdminPortReset') ;
        MprAdminPortDisconnect := GetProcAddress (MagMprAdmLib, 'MprAdminPortDisconnect') ;
        MprAdminUserGetInfo := GetProcAddress (MagMprAdmLib, 'MprAdminUserGetInfo') ;
        MprAdminUserSetInfo := GetProcAddress (MagMprAdmLib, 'MprAdminUserSetInfo') ;
        MprAdminSendUserMessage := GetProcAddress (MagMprAdmLib, 'MprAdminSendUserMessage') ;
        MprAdminGetPDCServer := GetProcAddress (MagMprAdmLib, 'MprAdminGetPDCServer') ;
        MagMprAdmAPI_Loaded := Assigned (MprAdminIsServiceRunning) ;
    end ;
end ;

constructor TMagRasAdm.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    fRRasHandle := 0 ;
    fRasCurServer := '' ;
    fRasServer := '' ;
    fVersion := MagVersion ;
    fAdminServerType := ServerUnknown ;
    SetMaxPorts (8) ;    // initial number of ports
end;

destructor TMagRasAdm.Destroy;
begin
    DisconnServer ;
    inherited Destroy;
end;

// connect to server (if necessary), see if it exists and find how many ports

function TMagRasAdm.ConnectServer: integer ;
var
    RasServer0: TRasServer0 ;
    MPR_SERVER_0: TMPR_SERVER_0 ;
begin
    result := 0 ;
    if (NOT MagLoadRasAdmApi) then
    begin
        LastError := ERROR_DLL_NOT_FOUND ;
        result := LastError ;
        exit ;
    end ;
    fPortsTotal := 0 ;
    fPortsInuse := 0 ;
    fPortsFree := 0 ;
    fRasAdmVer := 0 ;

// look for NT4 RAS server
    if fAdminServerType in [ServerUnknown, ServerNT4] then
    begin
        FillChar (RasServer0, SizeOf (RasServer0), #0);
        if fRasServer = '' then
            LastError := RasAdminServerGetInfo (Nil, RasServer0)
        else
        begin
            LastError := RasAdminServerGetInfo (fWideRasServer, RasServer0) ;
        end ;
        Result := LastError;
        if (LastError <> 0) and (fAdminServerType = ServerNT4) then
        begin
            StatusStr := GetErrorString (LastError);
            exit ;
        end ;

    // found NT4 RAS server, keep server info
        if LastError = 0 then
        begin
            fAdminServerType := ServerNT4 ;
            fPortsFree := RasServer0.TotalPorts ;
            fPortsInuse := RasServer0.PortsInuse ;
            fRasAdmVer := RasServer0.RasVersion ;
        end ;
    end ;

// look for W2K/.NET RRAS server
    if fAdminServerType in [ServerUnknown, ServerW2K] then
    begin
        if MagMprAdmLib = 0 then
        begin
            result := 99 ;
            StatusStr := SRasErrNoRRas ;
            exit ;
        end ;
        FillChar (MPR_SERVER_0, SizeOf (MPR_SERVER_0), #0);
        if NOT MprAdminIsServiceRunning (fWideRasServer) then
        begin
            result := 99 ;
            StatusStr := SRasErrNoRRas ;
            exit ;
        end ;
        LastError := 0 ;
        if (fRRasHandle = 0) then
        begin
            LastError := MprAdminServerConnect (fWideRasServer, fRRasHandle) ;
        end ;
        if LastError = 0 then
                   LastError := MprAdminServerGetInfo (fRRasHandle, 0, @MPR_SERVER_0) ;
        Result := LastError;
        if (LastError <> 0) then
        begin
            StatusStr := GetErrorString (LastError);
            exit ;
        end ;

// found W2K or .NET RRAS server, keep server info
        fAdminServerType := ServerW2K ;
        fPortsFree := MPR_SERVER_0.dwTotalPorts ;
        fPortsInuse := MPR_SERVER_0.dwPortsInuse ;
    end ;
end ;

procedure TMagRasAdm.DisconnServer ;
begin
    if fRRasHandle = 0 then exit ;
    MprAdminServerDisconnect (fRRasHandle) ;
    fRRasHandle := 0 ;
    fRasCurServer := '' ;
end ;

procedure TMagRasAdm.SetServer (ServerName: string) ;
begin
    if fRasServer <> ServerName then
    begin
        fRasServer := ServerName ;
        StringToWideChar (ServerName, fWideRasServer, CNLEN + 2) ;
        DisconnServer ;
    end ;
end ;

procedure TMagRasAdm.SetMaxPorts (value: integer) ;
begin
    if value <> fMaxPorts then SetLength (Ports, Value) ;
    fMaxPorts := value ;
end ;

function TMagRasAdm.CheckAllPorts: integer ;
var
    EntriesRead: WORD ;
    I: integer ;
    ppRasPort0, curpRasPort0: PAnsiChar ;
    RasPort0: TRasPort0 ;
//    MPR_SERVER_0: TMPR_SERVER_0 ;
    RRAS_PORT_0: TRRAS_PORT_0 ;
//    RRAS_PORT_1: TRRAS_PORT_1 ;
    EmunHandle: DWORD ;  // 15 July 2011, was THandle
    ReadNr, TotNr: DWORD ;
begin
    result := ConnectServer ;
    if result <> 0 then exit ;
    if (fAdminServerType = ServerUnknown) then exit ;  // should not get here

// NT4, enumerate ports
    if (fAdminServerType = ServerNT4) then
    begin
        EntriesRead := 0 ;
        ppRasPort0 := Nil ;
        if fRasServer = '' then
            LastError := RasAdminPortEnum (Nil, @ppRasPort0, EntriesRead)
        else
            LastError := RasAdminPortEnum (fWideRasServer, @ppRasPort0, EntriesRead) ;
        Result := LastError;

    // parse ports,
        if EntriesRead > fMaxPorts then
        begin
            fMaxPorts := EntriesRead + 1 ;
            SetLength (Ports, fMaxPorts) ;
        end ;
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
                // clear stuff filled in later by GetPortInfo
                    LineCond := 0 ;
                    HardwareCond := 0 ;
                    Speed := 0 ;
                    FillChar (Statistics, SizeOf (TRasPortStatistics), #0) ;
                    Projection := 0 ;
                    IPAddress := '' ;
                    hConnection := 0 ;
                end ;
            end ;
            if ppRasPort0 <> Nil then RasAdminFreeBuffer (ppRasPort0) ;
        end ;
    end ;

// W2K, .NET, enumerate ports
    if (fAdminServerType = ServerW2K) then
    begin
        EmunHandle := 0 ;
        fPortsTotal := 0 ;
        while LastError = 0 do
        begin
            ReadNr := 0 ;
            FillChar (RRAS_PORT_0, SizeOf (RRAS_PORT_0), #0);
//          FillChar (RRAS_PORT_1, SizeOf (RRAS_PORT_1), #0);
            LastError := MprAdminPortEnum (fRRasHandle, 0, INVALID_HANDLE_VALUE,
                  @RRAS_PORT_0, SizeOf (RRAS_PORT_0), ReadNr, TotNr, EmunHandle) ;
            if ReadNr = 1 then
            begin
                if Integer (TotNr) > fMaxPorts then
                begin
                    fMaxPorts := TotNr + 1 ;
                    SetLength (Ports, fMaxPorts) ;
                end ;

//              Ret := MprAdminPortGetInfo (fRRasHandle, 1, RRAS_PORT_0.hPort,
//                                                                   @RRAS_PORT_1) ;
                with Ports [fPortsTotal] do
                begin
                    PortName := RRAS_PORT_0.wszPortName ;
                    DeviceType := RRAS_PORT_0.wszDeviceType ;
                    DeviceName := RRAS_PORT_0.wszDeviceName ;
                    MediaName := RRAS_PORT_0.wszMediaName ;
                    HardwareCond := Ord (RRAS_PORT_0.dwPortCondition) ;
                    hPort := RRAS_PORT_0.hPort ;
                    hConnection := RRAS_PORT_0.hConnection ;
                    bFlags := 0 ;
                    UserName := '' ;
                    Computer := '' ;
                    StartDT := 0 ;
                    LogonDomain := '' ;
                // clear stuff filled in later by GetPortInfo
                    LineCond := 0 ;
                    Speed := 0 ;
                    FillChar (Statistics, SizeOf (TRasPortStatistics), #0) ;
                    Projection := 0 ;
                    IPAddress := '' ;
                end ;
                inc (fPortsTotal) ;
            end ;
            if LastError <> ERROR_MORE_DATA then
            begin
                Result := LastError;
                break ;
            end ;
        end ;
        MprAdminServerDisconnect (fRRasHandle) ;
        fRRasHandle := 0 ;
    end ;
end ;

function TMagRasAdm.GetErrorString(ErrorCode: LongInt): String;
var
    szErrorString: Array[0..256] of WideChar;
    pErrorString: PWChar ;
begin
    if (NOT MagLoadRasAdmApi) then
    begin
        LastError := ERROR_DLL_NOT_FOUND ;
        result := RASSAPI_DLL + #32 + SRasGenNotAvailable ;  // Not Available
        exit ;
    end ;
    Result := '';
    pErrorString := Nil ;
    FillChar (szErrorString, SizeOf (szErrorString), #0);
    if (fAdminServerType = ServerW2K) then
    begin
        MprAdminGetErrorString (ErrorCode, pErrorString) ;
        Result := WideCharToString (pErrorString) ;
    end
    else
    begin
        RasAdminGetErrorString (ErrorCode, szErrorString, 256);
        If szErrorString[0] <> #0 THEN
            Result := WideCharLenToString (szErrorString, 256)
        Else
            Result := SysErrorMessage (ErrorCode) ;  // Angus, try a windows error
    end ;
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
    ppRasParams: PAnsiChar ;
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

procedure MagUnloadRasAdmApi ;
begin
    if MagRasAdmAPI_Loaded then FreeLibrary (MagRasAdmLib) ;
    if MagMprAdmLib <> 0 then FreeLibrary (MagMprAdmLib) ;
end ;

Initialization
    MagRasAdmLib := 0 ;
    MagMprAdmLib := 0 ;
    MagRasAdmAPI_Loaded := false ;
finalization
    MagUnloadRasAdmApi ;
end.

