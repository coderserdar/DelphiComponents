Unit MagRasApiA;  // ANSI unit
{$IFNDEF VER140}
  {$WARN UNSAFE_TYPE off}
  {$WARN UNSAFE_CAST off}
  {$WARN UNSAFE_CODE off}
{$ENDIF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}

// Aug 2011, don't want anything packed
// note original API was 4 byte aligned, but Delphi defaults to 8 byte
// aligned, which always seemed to work, except for one 64-bit structure
{$ALIGN ON}

Interface

Uses SysUtils, Windows, Classes, MagRasStr, MagSubs1 ;

{ Copyright (c) 1993, Microsoft Corporation, all rights reserved
  Note: The 'dwSize' member of a structure X must be set to sizeof(X)
  before calling the associated API, otherwise ERROR_INVALID_SIZE is
  returned.  The APIs determine the size using 2-byte packing (the
  default for Microsoft compilers when no /Zp<n> option is supplied).
  Users requiring non-default packing can use the 'dwSize' values
  listed next to each 'dwSize' member in place of sizeof(X). }

{
DELPHI RAS COMPONENT - API
19th August 2011 - Release 6.30, Copyright 2011, Magenta Systems Ltd

Updated by Angus Robertson, Magenta Systems Ltd, England
in early 1998, delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd, 2011

Compatible with Delphi 6, 7, 2005, 2006, 2007, 2009, 2010, XE, XE2 supports
Windows 95, 98, NT 4.0 and Windows 2000, XP, 2003, Vista, 2008 and 7, both
32-bit and 64-bit editions, but note Win9x and NT4 not tested for years.
Win32 and Win64 binaries only.

Changes in 2.0
Added RasMonitorDlg (NT only)
Added .DLL since needed for NT4
Added phone book stuff
Removed explicit DLL calls so application will run without RAS installed
Added extra literals

Changes in 2.8
Changed WINVER41 to NT_EXTNS so code can be compiled under NT4 but using only
   Win9x functions and structures  (removed in 3.2)
Added RASSUBENTRY, RasGetSubEntryProperties for multi-link connections, NT only
Added various AUTODIAL things, NT only

Changes in 3.1
Added RASEntryNT, similar to RASEntry but with NT extensions non-conditionally added

Changes in 3.2
Added RasGetConnectionStatisitcs, Ras_Stats, RasClearConnectionStatistics (W2K only)

Changes in 4.0
All DLL functions loaded here, to avoid duplication in three RAS modules
Duplicates of many structures so the same calls work on 9x/NT4/W2K
Using DWORD not Longint to ease compiler range checking problems
Added Windows 2000 versions of structures, and literals from latest ras.h

Changes in 4.2
Corrected TRasPppLcpW2K from new RAS.H
Changed some VAR arguments to PCHAR to allow nil to be used
Moved TAPI device stuff to magtapiapi

Changes in 4.3
Make sure library variables cleared at start

Changes in 4.41
Implemented autodial functions for NT4/W2K

Changes in 4.51
Added ET_Optional for dwEncryptionType which is W2K default
More error codes for W2K
Added RasDeleteSubEntry for Windows XP/Whistler, also some new literals
  (on NT4 and W2K delete the entry and save with the new name, password
   will be lost on W2K)

Changes in 4.60
Added MagRasPhoneFile property array which is the physical rasphone.pbk
  filenames on NT4/W2K.

Changes in 4.62
Added new stuff for Windows XP, from RAS.H dated 16 June 2001, note this differs
  from the Feb 2001 SDK documentation but seems to work
Corrected (and tested) RasDeleteSubEntry, Windows XP only

Changes in 4.70
Now have August 2001 XP SDK documentation, which almost matches June header file,
but not quite...added a few comments.

Changes in 4.80
Moved MagRasOSVersion to magsubs1 so it's available without initialising RAS APIs

Changes in 4.90
Removed MaxPhonebooks (entries) constant, now no limit

Changes in 4.91
Removed MaxDevices=30, now unlimited (needed for Win .NET Server beta, 260 devices)

Changes in 4.92
Added ERROR_BUFFER_INVALID (same as ERROR_INVALID_BUFFER)

Changes in 5.10
Added 26 new errors for Vista and later (WINVER >= 0x600)
Added new IpV6 stuff for RasEntry and new literals

Changes in 5.20
Added MasRasPBLocationStr array literals for PBLocation
Added RasPhonebookDlg, RasEntryDlg and RasDialDlg APIs since Vista no longer
supports old dialogs

Changes in 5.21
Added MagSetPhoneBkFiles split from MagLoadRasApi so file names can be set
before the RAS is loaded

Changes in 5.40
Made compatible with Delphi 2009, but still using ANSI RAS functions, not Unicode
RasGetEntryProperties and RasGetSubEntryProperties have var for DWORDs instead of PChar

Changes in 6.00
There are two new units, magrasapia is only ANSI, magrasapiw is only Unicode/Widestring
Added all the unicode W structures and functions to support Delphi 2009
Changed all existing structures and functions to A (unless no strings) to ease support
Note TMagRas now always uses the W structures and functions on all compilers, but only
supports widechars/unicode for Delphi 2009 (mainly because there is no wide TStrings)

Changes in 6.30
Fixed pointers in TRasDialExtensionsW2K and TRasDialParamsNT4 for 64-bits
HRasConn is now THandle not DWORD
RasConn only seems to work on Win64 if aligned 4 on DWORD boundaries, not QWORD
No longer use packed in records 


Windows 7 - note that the RasEntry structure has been lengthened with more IP6
elements, but these are not yet added to TMagRas mainly because we don't support IP6



}

Const
  UNLEN = 256;                 // Maximum user name length
  PWLEN = 256;                 // Maximum password length
  CNLEN = 15;                  // Computer name length
  DNLEN = 15;                  // Maximum domain name length
  UNCLEN = (CNLEN+2) ;         // UNC computer name length
  NETBIOS_NAME_LEN      = 16;  // NetBIOS net name (bytes)
  RAS_MaxEntryName      =  256;
  RAS_MaxDeviceName     =  128;
  RAS_MaxDeviceType     =  16;
//RAS_MaxParamKey       =  32;
//RAS_MaxParamValue     = 128;
  RAS_MaxPhoneNumber    = 128;
  RAS_MaxCallbackNumber =  RAS_MaxPhoneNumber;
  RAS_MaxIpAddress      = 15;  // ANGUS
  RAS_MaxIpxAddress     = 21;  // ANGUS
  RAS_MaxAreaCode       = 10;
  RAS_MaxPadType        = 32;
  RAS_MaxX25Address     = 200;
  RAS_MaxFacilities     = 200;
  RAS_MaxUserData       = 200;
  RAS_MaxReplyMessage   = 1024;
  RAS_MaxDnsSuffix      = 256;   // Windows XP

Type
  PHRasConn = ^HRasConn;
  HRasConn = THandle; // 20 July 2011 was DWORD

Const
  { Enumerates intermediate states to a Connection.  (See RasDial) }
  RASCS_Paused      = $1000;     // =4096
  RASCS_Done        = $2000;     // =8192

  RASBase = 600;
  Success = 0;
{ Error Codes }
  PENDING                               = (RASBase+0);
  ERROR_INVALID_PORT_HANDLE             = (RASBase+1);
  ERROR_PORT_ALREADY_OPEN               = (RASBase+2);
  ERROR_BUFFER_TOO_SMALL                = (RASBase+3);
  ERROR_WRONG_INFO_SPECIFIED            = (RASBase+4);
  ERROR_CANNOT_SET_PORT_INFO            = (RASBase+5);
  ERROR_PORT_NOT_CONNECTED              = (RASBase+6);
  ERROR_EVENT_INVALID                   = (RASBase+7);
  ERROR_DEVICE_DOES_NOT_EXIST           = (RASBase+8);
  ERROR_DEVICETYPE_DOES_NOT_EXIST       = (RASBase+9);
  ERROR_INVALID_BUFFER                  = (RASBase+10);
  ERROR_BUFFER_INVALID                  = (RASBase+10);
  ERROR_ROUTE_NOT_AVAILABLE             = (RASBase+11);
  ERROR_ROUTE_NOT_ALLOCATED             = (RASBase+12);
  ERROR_INVALID_COMPRESSION_SPECIFIED   = (RASBase+13);
  ERROR_OUT_OF_BUFFERS                  = (RASBase+14);
  ERROR_PORT_NOT_FOUND                  = (RASBase+15);
  ERROR_ASYNC_REQUEST_PENDING           = (RASBase+16);
  ERROR_ALREADY_DISCONNECTING           = (RASBase+17);
  ERROR_PORT_NOT_OPEN                   = (RASBase+18);
  ERROR_PORT_DISCONNECTED               = (RASBase+19);
  ERROR_NO_ENDPOINTS                    = (RASBase+20);
  ERROR_CANNOT_OPEN_PHONEBOOK           = (RASBase+21);
  ERROR_CANNOT_LOAD_PHONEBOOK           = (RASBase+22);
  ERROR_CANNOT_FIND_PHONEBOOK_ENTRY     = (RASBase+23);
  ERROR_CANNOT_WRITE_PHONEBOOK          = (RASBase+24);
  ERROR_CORRUPT_PHONEBOOK               = (RASBase+25);
  ERROR_CANNOT_LOAD_STRING              = (RASBase+26);
  ERROR_KEY_NOT_FOUND                   = (RASBase+27);
  ERROR_DISCONNECTION                   = (RASBase+28);
  ERROR_REMOTE_DISCONNECTION            = (RASBase+29);
  ERROR_HARDWARE_FAILURE                = (RASBase+30);
  ERROR_USER_DISCONNECTION              = (RASBase+31);
  ERROR_INVALID_SIZE                    = (RASBase+32);
  ERROR_PORT_NOT_AVAILABLE              = (RASBase+33);
  ERROR_CANNOT_PROJECT_CLIENT           = (RASBase+34);
  ERROR_UNKNOWN                         = (RASBase+35);
  ERROR_WRONG_DEVICE_ATTACHED           = (RASBase+36);
  ERROR_BAD_STRING                      = (RASBase+37);
  ERROR_REQUEST_TIMEOUT                 = (RASBase+38);
  ERROR_CANNOT_GET_LANA                 = (RASBase+39);
  ERROR_NETBIOS_ERROR                   = (RASBase+40);
  ERROR_SERVER_OUT_OF_RESOURCES         = (RASBase+41);
  ERROR_NAME_EXISTS_ON_NET              = (RASBase+42);
  ERROR_SERVER_GENERAL_NET_FAILURE      = (RASBase+43);
  WARNING_MSG_ALIAS_NOT_ADDED           = (RASBase+44);
  ERROR_AUTH_INTERNAL                   = (RASBase+45);
  ERROR_RESTRICTED_LOGON_HOURS          = (RASBase+46);
  ERROR_ACCT_DISABLED                   = (RASBase+47);
  ERROR_PASSWD_EXPIRED                  = (RASBase+48);
  ERROR_NO_DIALIN_PERMISSION            = (RASBase+49);
  ERROR_SERVER_NOT_RESPONDING           = (RASBase+50);
  ERROR_FROM_DEVICE                     = (RASBase+51);
  ERROR_UNRECOGNIZED_RESPONSE           = (RASBase+52);
  ERROR_MACRO_NOT_FOUND                 = (RASBase+53);
  ERROR_MACRO_NOT_DEFINED               = (RASBase+54);
  ERROR_MESSAGE_MACRO_NOT_FOUND         = (RASBase+55);
  ERROR_DEFAULTOFF_MACRO_NOT_FOUND      = (RASBase+56);
  ERROR_FILE_COULD_NOT_BE_OPENED        = (RASBase+57);
  ERROR_DEVICENAME_TOO_LONG             = (RASBase+58);
  ERROR_DEVICENAME_NOT_FOUND            = (RASBase+59);
  ERROR_NO_RESPONSES                    = (RASBase+60);
  ERROR_NO_COMMAND_FOUND                = (RASBase+61);
  ERROR_WRONG_KEY_SPECIFIED             = (RASBase+62);
  ERROR_UNKNOWN_DEVICE_TYPE             = (RASBase+63);
  ERROR_ALLOCATING_MEMORY               = (RASBase+64);
  ERROR_PORT_NOT_CONFIGURED             = (RASBase+65);
  ERROR_DEVICE_NOT_READY                = (RASBase+66);
  ERROR_READING_INI_FILE                = (RASBase+67);
  ERROR_NO_ConnECTION                   = (RASBase+68);
  ERROR_BAD_USAGE_IN_INI_FILE           = (RASBase+69);
  ERROR_READING_SECTIONNAME             = (RASBase+70);
  ERROR_READING_DEVICETYPE              = (RASBase+71);
  ERROR_READING_DEVICENAME              = (RASBase+72);
  ERROR_READING_USAGE                   = (RASBase+73);
  ERROR_READING_MAXCONNECTBPS           = (RASBase+74);
  ERROR_READING_MAXCARRIERBPS           = (RASBase+75);
  ERROR_LINE_BUSY                       = (RASBase+76);
  ERROR_VOICE_ANSWER                    = (RASBase+77);
  ERROR_NO_ANSWER                       = (RASBase+78);
  ERROR_NO_CARRIER                      = (RASBase+79);
  ERROR_NO_DIALTONE                     = (RASBase+80);
  ERROR_IN_COMMAND                      = (RASBase+81);
  ERROR_WRITING_SECTIONNAME             = (RASBase+82);
  ERROR_WRITING_DEVICETYPE              = (RASBase+83);
  ERROR_WRITING_DEVICENAME              = (RASBase+84);
  ERROR_WRITING_MAXConnECTBPS           = (RASBase+85);
  ERROR_WRITING_MAXCARRIERBPS           = (RASBase+86);
  ERROR_WRITING_USAGE                   = (RASBase+87);
  ERROR_WRITING_DEFAULTOFF              = (RASBase+88);
  ERROR_READING_DEFAULTOFF              = (RASBase+89);
  ERROR_EMPTY_INI_FILE                  = (RASBase+90);
  ERROR_AUTHENTICATION_FAILURE          = (RASBase+91);
  ERROR_PORT_OR_DEVICE                  = (RASBase+92);
  ERROR_NOT_BINARY_MACRO                = (RASBase+93);
  ERROR_DCB_NOT_FOUND                   = (RASBase+94);
  ERROR_STATE_MACHINES_NOT_STARTED      = (RASBase+95);
  ERROR_STATE_MACHINES_ALREADY_STARTED  = (RASBase+96);
  ERROR_PARTIAL_RESPONSE_LOOPING        = (RASBase+97);
  ERROR_UNKNOWN_RESPONSE_KEY            = (RASBase+98);
  ERROR_RECV_BUF_FULL                   = (RASBase+99);
  ERROR_CMD_TOO_LONG                    = (RASBase+100);
  ERROR_UNSUPPORTED_BPS                 = (RASBase+101);
  ERROR_UNEXPECTED_RESPONSE             = (RASBase+102);
  ERROR_INTERACTIVE_MODE                = (RASBase+103);
  ERROR_BAD_CALLBACK_NUMBER             = (RASBase+104);
  ERROR_INVALID_AUTH_STATE              = (RASBase+105);
  ERROR_WRITING_INITBPS                 = (RASBase+106);
  ERROR_X25_DIAGNOSTIC                  = (RASBase+107);
  ERROR_ACCT_EXPIRED                    = (RASBase+108);
  ERROR_CHANGING_PASSWORD               = (RASBase+109);
  ERROR_OVERRUN                         = (RASBase+110);
  ERROR_RASMAN_CANNOT_INITIALIZE        = (RASBase+111);
  ERROR_BIPLEX_PORT_NOT_AVAILABLE       = (RASBase+112);
  ERROR_NO_ACTIVE_ISDN_LINES            = (RASBase+113);
  ERROR_NO_ISDN_CHANNELS_AVAILABLE      = (RASBase+114);
  ERROR_TOO_MANY_LINE_ERRORS            = (RASBase+115);
  ERROR_IP_CONFIGURATION                = (RASBase+116);
  ERROR_NO_IP_ADDRESSES                 = (RASBase+117);
  ERROR_PPP_TIMEOUT                     = (RASBase+118);
  ERROR_PPP_REMOTE_TERMINATED           = (RASBase+119);
  ERROR_PPP_NO_PROTOCOLS_CONFIGURED     = (RASBase+120);
  ERROR_PPP_NO_RESPONSE                 = (RASBase+121);
  ERROR_PPP_INVALID_PACKET              = (RASBase+122);
  ERROR_PHONE_NUMBER_TOO_LONG           = (RASBase+123);
  ERROR_IPXCP_NO_DIALOUT_CONFIGURED     = (RASBase+124);
  ERROR_IPXCP_NO_DIALIN_CONFIGURED      = (RASBase+125);
  ERROR_IPXCP_DIALOUT_ALREADY_ACTIVE    = (RASBase+126);
  ERROR_ACCESSING_TCPCFGDLL             = (RASBase+127);
  ERROR_NO_IP_RAS_ADAPTER               = (RASBase+128);
  ERROR_SLIP_REQUIRES_IP                = (RASBase+129);
  ERROR_PROJECTION_NOT_COMPLETE         = (RASBase+130);
  ERROR_PROTOCOL_NOT_CONFIGURED         = (RASBase+131);
  ERROR_PPP_NOT_CONVERGING              = (RASBase+132);
  ERROR_PPP_CP_REJECTED                 = (RASBase+133);
  ERROR_PPP_LCP_TERMINATED              = (RASBase+134);
  ERROR_PPP_REQUIRED_ADDRESS_REJECTED   = (RASBase+135);
  ERROR_PPP_NCP_TERMINATED              = (RASBase+136);
  ERROR_PPP_LOOPBACK_DETECTED           = (RASBase+137);
  ERROR_PPP_NO_ADDRESS_ASSIGNED         = (RASBase+138);
  ERROR_CANNOT_USE_LOGON_CREDENTIALS    = (RASBase+139);
  ERROR_TAPI_CONFIGURATION              = (RASBase+140);
  ERROR_NO_LOCAL_ENCRYPTION             = (RASBase+141);
  ERROR_NO_REMOTE_ENCRYPTION            = (RASBase+142);
  ERROR_REMOTE_REQUIRES_ENCRYPTION      = (RASBase+143);
  ERROR_IPXCP_NET_NUMBER_CONFLICT       = (RASBase+144);
  ERROR_INVALID_SMM                     = (RASBase+145);
  ERROR_SMM_UNINITIALIZED               = (RASBase+146);
  ERROR_NO_MAC_FOR_PORT                 = (RASBase+147);
  ERROR_SMM_TIMEOUT                     = (RASBase+148);
  ERROR_BAD_PHONE_NUMBER                = (RASBase+149);
  ERROR_WRONG_MODULE                    = (RASBase+150);
  ERROR_INVALID_CALLBACK_NUMBER         = (RASBase+151);
  ERROR_SCRIPT_SYNTAX                   = (RASBase+152);
  ERROR_HANGUP_FAILED                   = (RASBase+153);
  ERROR_BUNDLE_NOT_FOUND                = (RASBASE+154);  // following probably W2K
  ERROR_CANNOT_DO_CUSTOMDIAL            = (RASBASE+155);
  ERROR_DIAL_ALREADY_IN_PROGRESS        = (RASBASE+156);
  ERROR_RASAUTO_CANNOT_INITIALIZE       = (RASBASE+157);
  ERROR_CONNECTION_ALREADY_SHARED       = (RASBASE+158);
  ERROR_SHARING_CHANGE_FAILED           = (RASBASE+159);
  ERROR_SHARING_ROUTER_INSTALL          = (RASBASE+160);
  ERROR_SHARE_CONNECTION_FAILED         = (RASBASE+161);
  ERROR_SHARING_PRIVATE_INSTALL         = (RASBASE+162);
  ERROR_CANNOT_SHARE_CONNECTION         = (RASBASE+163);
  ERROR_NO_SMART_CARD_READER            = (RASBASE+164);
  ERROR_SHARING_ADDRESS_EXISTS          = (RASBASE+165);
  ERROR_NO_CERTIFICATE                  = (RASBASE+166);
  ERROR_SHARING_MULTIPLE_ADDRESSES      = (RASBASE+167);
  ERROR_FAILED_TO_ENCRYPT               = (RASBASE+168);
  ERROR_BAD_ADDRESS_SPECIFIED           = (RASBASE+169);
  ERROR_CONNECTION_REJECT               = (RASBASE+170);
  ERROR_CONGESTION                      = (RASBASE+171);
  ERROR_INCOMPATIBLE                    = (RASBASE+172);
  ERROR_NUMBERCHANGED                   = (RASBASE+173);
  ERROR_TEMPFAILURE                     = (RASBASE+174);
  ERROR_BLOCKED                         = (RASBASE+175);
  ERROR_DONOTDISTURB                    = (RASBASE+176);
  ERROR_OUTOFORDER                      = (RASBASE+177);
  ERROR_UNABLE_TO_AUTHENTICATE_SERVER   = (RASBASE+178);
  ERROR_SMART_CARD_REQUIRED             = (RASBASE+179);
  ERROR_INVALID_FUNCTION_FOR_ENTRY      = (RASBASE+180);
  ERROR_CERT_FOR_ENCRYPTION_NOT_FOUND   = (RASBASE+181);
  ERROR_SHARING_RRAS_CONFLICT           = (RASBASE+182);
  ERROR_SHARING_NO_PRIVATE_LAN          = (RASBASE+183);
  ERROR_NO_DIFF_USER_AT_LOGON           = (RASBASE+184);
  ERROR_NO_REG_CERT_AT_LOGON            = (RASBASE+185);
  ERROR_OAKLEY_NO_CERT                  = (RASBASE+186);
  ERROR_OAKLEY_AUTH_FAIL                = (RASBASE+187);
  ERROR_OAKLEY_ATTRIB_FAIL              = (RASBASE+188);
  ERROR_OAKLEY_GENERAL_PROCESSING       = (RASBASE+189);
  ERROR_OAKLEY_NO_PEER_CERT             = (RASBASE+190);
  ERROR_OAKLEY_NO_POLICY                = (RASBASE+191);
  ERROR_OAKLEY_TIMED_OUT                = (RASBASE+192);
  ERROR_OAKLEY_ERROR                    = (RASBASE+193);
  ERROR_UNKNOWN_FRAMED_PROTOCOL         = (RASBASE+194);
  ERROR_WRONG_TUNNEL_TYPE               = (RASBASE+195);
  ERROR_UNKNOWN_SERVICE_TYPE            = (RASBASE+196);
  ERROR_CONNECTING_DEVICE_NOT_FOUND     = (RASBASE+197);
  ERROR_NO_EAPTLS_CERTIFICATE           = (RASBASE+198);
  ERROR_SHARING_HOST_ADDRESS_CONFLICT   = (RASBASE+199);  // XP
  ERROR_AUTOMATIC_VPN_FAILED            = (RASBASE+200);  // XP
  ERROR_VALIDATING_SERVER_CERT          = (RASBASE+201);     // Vista
  ERROR_READING_SCARD                   = (RASBASE+202);
  ERROR_INVALID_PEAP_COOKIE_CONFIG      = (RASBASE+203);
  ERROR_INVALID_PEAP_COOKIE_USER        = (RASBASE+204);
  ERROR_INVALID_MSCHAPV2_CONFIG         = (RASBASE+205);
  ERROR_VPN_GRE_BLOCKED                 = (RASBASE+206);  // New Longhorn Errors
  ERROR_VPN_DISCONNECT                  = (RASBASE+207);
  ERROR_VPN_REFUSED                     = (RASBASE+208);
  ERROR_VPN_TIMEOUT                     = (RASBASE+209);
  ERROR_VPN_BAD_CERT                    = (RASBASE+210);
  ERROR_VPN_BAD_PSK                     = (RASBASE+211);
  ERROR_SERVER_POLICY                   = (RASBASE+212);
  ERROR_BROADBAND_ACTIVE                = (RASBASE+213);
  ERROR_BROADBAND_NO_NIC                = (RASBASE+214);
  ERROR_BROADBAND_TIMEOUT               = (RASBASE+215);
  ERROR_FEATURE_DEPRECATED              = (RASBASE+216);
  ERROR_CANNOT_DELETE                   = (RASBASE+217);
  ERROR_RASQEC_RESOURCE_CREATION_FAILED = (RASBASE+218);
  ERROR_RASQEC_NAPAGENT_NOT_ENABLED     = (RASBASE+219);
  ERROR_RASQEC_NAPAGENT_NOT_CONNECTED   = (RASBASE+220);
  ERROR_RASQEC_CONN_DOESNOTEXIST        = (RASBASE+221);
  ERROR_RASQEC_TIMEOUT                  = (RASBASE+222);
  ERROR_PEAP_CRYPTOBINDING_INVALID      = (RASBASE+223);
  ERROR_PEAP_CRYPTOBINDING_NOTRECEIVED  = (RASBASE+224);
  ERROR_INVALID_VPNSTRATEGY             = (RASBASE+225);
  ERROR_EAPTLS_CACHE_CREDENTIALS_INVALID  = (RASBASE+226);
  RASBaseEnd                            = (RASBase+226);

Const
    RASCS_OpenPort = 0;
    RASCS_PortOpened = 1;
    RASCS_ConnectDevice = 2;
    RASCS_DeviceConnected = 3;
    RASCS_AllDevicesConnected = 4;
    RASCS_Authenticate = 5;
    RASCS_AuthNotify = 6;
    RASCS_AuthRetry = 7;
    RASCS_AuthCallback = 8;
    RASCS_AuthChangePassword = 9;
    RASCS_AuthProject = 10;
    RASCS_AuthLinkSpeed = 11;
    RASCS_AuthAck = 12;
    RASCS_ReAuthenticate = 13;
    RASCS_Authenticated = 14;
    RASCS_PrepareForCallback = 15;
    RASCS_WaitForModemReset = 16;
    RASCS_WaitForCallback = 17;
    RASCS_Projected = 18;
    RASCS_StartAuthentication = 19;   // following three are Win95 only
    RASCS_CallbackComplete = 20;
    RASCS_LogonNetwork = 21;
    RASCS_SubEntryConnected = 22 ;    // following are Windows NT
    RASCS_SubEntryDisconnected = 23 ;
    RASCS_Interactive           = RASCS_Paused;         // $1000=4096
    RASCS_RetryAuthentication   = RASCS_Paused + 1;
    RASCS_CallbackSetByCaller   = RASCS_Paused + 2;
    RASCS_PasswordExpired       = RASCS_Paused + 3;
    RASCS_InvokeEapUI           = RASCS_Paused + 4 ;    // Windows 2000
    RASCS_Connected             = RASCS_Done;           // $2000=8192
    RASCS_Disconnected          = RASCS_Done + 1;       // $2001=8193

Type

// 22 July 2011 - for some reason, Win64 seems to require the RasConn
// structures to be aligned on double-word boundaries, not quad word
// seems to be TLUID that matters
{$ALIGN 4}

// Identifies an active RAS Connection, RasConnectEnum
  PRasConnA = ^TRasConnA;             // Windows 9x only
  TRasConnA = record
    dwSize: DWORD;
    rasConn: HRasConn;
    szEntryName: Array[0..RAS_MaxEntryName] Of AnsiChar;
    szDeviceType: Array[0..RAS_MaxDeviceType] Of AnsiChar;
    szDeviceName: Array [0..RAS_MaxDeviceName] Of AnsiChar;
  end;

  PRasConnNT4A = ^TRasConnNT4A;       // Windows NT4
  TRasConnNT4A = record
    dwSize: DWORD;
    rasConn: HRasConn;
    szEntryName: Array[0..RAS_MaxEntryName] Of AnsiChar;
    szDeviceType: Array[0..RAS_MaxDeviceType] Of AnsiChar;
    szDeviceName: Array [0..RAS_MaxDeviceName] Of AnsiChar;
    szPhonebook: Array[0..MAX_PATH - 1] Of AnsiChar;
    dwSubEntry: dword;
  end;

  PRasConnW2KA = ^TRasConnW2KA;        // Windows 2000
  TRasConnW2KA = record
    dwSize: DWORD;
    rasConn: HRasConn;
    szEntryName: Array[0..RAS_MaxEntryName] Of AnsiChar;
    szDeviceType: Array[0..RAS_MaxDeviceType] Of AnsiChar;
    szDeviceName: Array [0..RAS_MaxDeviceName] Of AnsiChar;
    szPhonebook: Array[0..MAX_PATH - 1] Of AnsiChar;
    dwSubEntry: dword;
    guidEntry: TGUID ;        // guid that represents the phone-book entry
   end;

  PRasConnWXPA = ^TRasConnWXPA;        // Windows XP
  TRasConnWXPA = record
    dwSize: DWORD;
    rasConn: HRasConn;
    szEntryName: Array[0..RAS_MaxEntryName] Of AnsiChar;
    szDeviceType: Array[0..RAS_MaxDeviceType] Of AnsiChar;
    szDeviceName: Array [0..RAS_MaxDeviceName] Of AnsiChar;
    szPhonebook: Array[0..MAX_PATH - 1] Of AnsiChar;
    dwSubEntry: dword;      // one based
    guidEntry: TGUID ;      // guid that represents the phone-book entry
    dwFlags: DWORD ;        // RASCF literals
    luid: TLargeInteger     // (64-bits) locally unique identifier (LUID) for the logon session in which the connection was established
   end;

  PRasConnW7A = ^TRasConnW7A;        // Windows 7
  TRasConnW7A = record
    dwSize: DWORD;
    rasConn: HRasConn;
    szEntryName: Array[0..RAS_MaxEntryName] Of AnsiChar;
    szDeviceType: Array[0..RAS_MaxDeviceType] Of AnsiChar;
    szDeviceName: Array [0..RAS_MaxDeviceName] Of AnsiChar;
    szPhonebook: Array[0..MAX_PATH - 1] Of AnsiChar;
    dwSubEntry: dword;      // one based
    guidEntry: TGUID ;      // guid that represents the phone-book entry
    dwFlags: DWORD ;        // RASCF literals
    luid: TLargeInteger     // (64-bits) locally unique identifier (LUID) for the logon session in which the connection was established
    guidCorrelationId: TGUID ;   // 21 July 2011,  Vista SP1 and later
   end;

// 22 July 2011 back to quadword alignment
{$ALIGN ON}

// status of a specific RAS connection, RasGetConnectStatus
  PRasConnStatusA = ^TRasConnStatusA;      // Windows 9x
  TRasConnStatusA = Record
    dwSize: DWORD;
    rasConnstate: Word;
    dwError: DWORD;
    szDeviceType: Array[0..RAS_MaxDeviceType] Of AnsiChar;
    szDeviceName: Array[0..RAS_MaxDeviceName] Of AnsiChar;
  End;

  PRasConnStatusNT4A = ^TRasConnStatusNT4A;      // Windows NT4 and 2000
  TRasConnStatusNT4A = Record
    dwSize: DWORD;
    rasConnstate: Word;
    dwError: DWORD;
    szDeviceType: Array[0..RAS_MaxDeviceType] Of AnsiChar;
    szDeviceName: Array[0..RAS_MaxDeviceName] Of AnsiChar;
    szPhoneNumber: Array[0..RAS_MaxPhoneNumber] Of AnsiChar;
  End;

// dial params, used for both RasDial and RasGetEntryDialParams
  PRasDialParamsA = ^TRasDialParamsA;       // Windows 9x
  TRasDialParamsA = Record
    dwSize: DWORD;
    szEntryName: Array[0..RAS_MaxEntryName] Of AnsiChar;
    szPhoneNumber: Array[0..RAS_MaxPhoneNumber] Of AnsiChar;
    szCallbackNumber: Array[0..RAS_MaxCallbackNumber] Of AnsiChar;
    szUserName: Array[0..UNLEN] Of AnsiChar;
    szPassword: Array[0..PWLEN] Of AnsiChar;
    szDomain: Array[0..DNLEN] Of AnsiChar;
  end;

  PRasDialParamsNT4A = ^TRasDialParamsNT4A;       // Windows NT4 and 2000
  TRasDialParamsNT4A = Record
    dwSize: DWORD;
    szEntryName: Array[0..RAS_MaxEntryName] Of AnsiChar;
    szPhoneNumber: Array[0..RAS_MaxPhoneNumber] Of AnsiChar;
    szCallbackNumber: Array[0..RAS_MaxCallbackNumber] Of AnsiChar;
    szUserName: Array[0..UNLEN] Of AnsiChar;
    szPassword: Array[0..PWLEN] Of AnsiChar;
    szDomain: Array[0..DNLEN] Of AnsiChar;
    dwSubEntry: DWORD;                  // index
    dwCallbackId: Pointer; // for RasDialFunc2, 20 July 2011 was DWORD
  end;

// Ras{Get,Set}Credentials structure.
// These calls supercede Ras{Get,Set}EntryDialParams in NT4
  PRasCredentialsA = ^TRasCredentialsA ;      // Windows NT4 and 2000
  TRasCredentialsA = Record
    dwSize,
    dwMask: DWORD;
    szUserName: Array[0..UNLEN] Of AnsiChar;
    szPassword: Array[0..PWLEN] Of AnsiChar;
    szDomain: Array[0..DNLEN] Of AnsiChar;
  end ;

const
// RASCREDENTIALS 'dwMask' values.

    RASCM_UserName           = $00000001 ;
    RASCM_Password           = $00000002 ;
    RASCM_Domain             = $00000004 ;
    RASCM_DefaultCreds       = $00000008 ; // following Windows XP and later only
    RASCM_PreSharedKey       = $00000010 ;
    RASCM_ServerPreSharedKey = $00000020 ;
    RASCM_DDMPreSharedKey    = $00000040 ;


// for TRasConnWXP dwFlag values - Windows XP and later
    RASCF_AllUsers	  = $00000001 ;
    RASCF_GlobalCreds = $00000002 ;
    RASCF_OwnerKnown  = $00000004 ;
    RASCF_OwnerMatch  = $00000008 ;
//    RASCF_IsOwner(rascFlags) = (((rascFlags) & (RASCF_OwnerKnown | RASCF_OwnerMatch)) == (RASCF_OwnerKnown | RASCF_OwnerMatch));
//    RASCF_IsNotOwner(rascFlags) = (((rascFlags) & (RASCF_OwnerKnown | RASCF_OwnerMatch)) == (RASCF_OwnerKnown));

type
// Authentification
  TRasEapInfo = Record                            // Windows 2000
    dwSizeofEapInfo: DWORD;
    pbEapInfo: Pointer;
    end ;

// dial extensions for RasDial
  PRasDialExtensions = ^TRasDialExtensions;     // Windows NT4
  TRasDialExtensions = Record
    dwSize: DWORD;
    dwfOptions: DWORD;
    hwndParent: HWnd;
    reserved: DWORD;
    end;

  PRasDialExtensionsW2K = ^TRasDialExtensionsW2K;     // Windows 2000
  TRasDialExtensionsW2K = Record
    dwSize: DWORD;
    dwfOptions: DWORD;
    hwndParent: HWnd;
    reserved: Pointer ;    // 20 July 2011, was DWORD
    reserved1: Pointer ;   // 20 July 2011, was DWORD
    RasEapInfo: TRasEapInfo;
    end;

const
// 'dwfOptions' bit flags in TRasDialExtensions

    RDEOPT_UsePrefixSuffix           = $00000001 ;
    RDEOPT_PausedStates              = $00000002 ;
    RDEOPT_IgnoreModemSpeaker        = $00000004 ;
    RDEOPT_SetModemSpeaker           = $00000008 ;
    RDEOPT_IgnoreSoftwareCompression = $00000010 ;
    RDEOPT_SetSoftwareCompression    = $00000020 ;
    RDEOPT_DisableConnectedUI        = $00000040 ;
    RDEOPT_DisableReconnectUI        = $00000080 ;
    RDEOPT_DisableReconnect          = $00000100 ;
    RDEOPT_NoUser                    = $00000200 ;
    RDEOPT_PauseOnScript             = $00000400 ;
    RDEOPT_Router                    = $00000800 ;
    RDEOPT_CustomDial                = $00001000 ;  // Windows 2000
    RDEOPT_UseCustomScripting        = $00002000 ;  // Windows XP

// Phonebooks, aka connection entry list, for RasEnumEntries
type
  PRasEntryNameA = ^TRasEntryNameA;   // Windows 9x and NT
  TRasEntryNameA = Record
    dwSize: DWORD;
    szEntryName: Array[0..RAS_MaxEntryName] Of AnsiChar;
  End;

  PRasEntryNameW2KA = ^TRasEntryNameW2KA;   // Windows W2K and later
  TRasEntryNameW2KA = Record
    dwSize: DWORD;
    szEntryName: Array[0..RAS_MaxEntryName] Of AnsiChar;
    dwFlags: DWORD ;                     // REN_Allusers or REN_User
    szPhonebookPath: Array[0..MAX_PATH] Of AnsiChar;
  End;

// This flag when set in the RASENTRYNAME structure
// indicates that the phonebook to which this entry
// belongs is a system phonebook.
const
    REN_User       = 0 ;
    REN_AllUsers   = 1 ;

// NT dialog box
type
  PRasRasMonitorDlg= ^TRasRasMonitorDlg;    // ANGUS 5Feb98
  TRasRasMonitorDlg= Record
    dwSize: DWORD ;
    hwndOwner: HWnd;
    dwFlags: DWORD ;
    dwStartPage: DWORD ;
    xDlg: DWORD ;
    yDlg: DWORD ;
    dwError: DWORD ;
    reserved: DWORD ;
    reserved2: DWORD ;
    End;

// Describes the result of a PPP NBF (NetBEUI) projection

  PRasPppNbfA = ^TRasPppNbfA;
  TRasPppNbfA = record
    dwSize: DWORD;
    dwError: DWORD;
    dwNetBiosError: DWORD;
    szNetBiosError: Array[0..NETBIOS_NAME_LEN] of AnsiChar;
    szWorkstationName: Array[0..NETBIOS_NAME_LEN] of AnsiChar;
    bLana: Byte;
  end;


// Describes the results of a PPP IPX (Internetwork Packet Exchange)
// projection.

  PRasPppIpxA = ^TRasPppIpxA;
  TRasPppIpxA = record
    dwSize: DWORD;
    dwError: DWORD;
    szIpxAddress: Array[0..RAS_MaxIpxAddress] of AnsiChar;
  end;

// Describes the results of a PPP IP (Internet) projection.

  PRasPppIpA = ^TRasPppIpA;
  TRasPppIpA = record
    dwSize: DWORD;
    dwError: DWORD;
    szIpAddress: Array[0..RAS_MaxIpAddress] of AnsiChar;
    szServerIpAddress: Array[0..RAS_MaxIpAddress] of AnsiChar;
  end ;

// Describes the results of a PPP LCP/multi-link negotiation - Angus 4.0
// Note this structure is undocumented, info from ras.h file

  PRasPppLcp = ^TRasPppLcp;       // Windows 9x and NT4, probably
  TRasPppLcp = record
    dwSize: DWORD;
    fBundled: LongBool;
  end;

  PRasPppLcpW2KA = ^TRasPppLcpW2KA; // Windows 2000, probably
  TRasPppLcpW2KA = record
    dwSize: DWORD;
    fBundled: LongBool;
    dwError,
    dwAuthenticationProtocol,
    dwAuthenticationData,
    dwEapTypeId,
    dwServerAuthenticationProtocol,
    dwServerAuthenticationData,
    dwServerEapTypeId: DWORD;
    fMultilink: LongBool;
    dwTerminateReason,
    dwServerTerminateReason: DWORD;
    szReplyMessage: array[0..RAS_MaxReplyMessage-1] of AnsiChar;
    dwOptions,
    dwServerOptions: DWORD ;
  end;

// Describes the results of a PPP CCP (Compression Control Protocol) projection.

  PTRasPppCcp = ^TRasPppCcp;
  TRasPppCcp = record
    dwSize,
    dwError: DWORD;
    dwCompressionAlgorithm,
    dwOptions,
    dwServerCompressionAlgorithm,
    dwServerOptions: DWORD ;
  end;

// Describes the results of a SLIP (Serial Line IP) projection - gone with Vista

  PRasSlipA = ^TRasSlipA;
  TRasSlipA = record
    dwSize: DWORD;
    dwError: DWORD;
    szIpAddress: Array[0..RAS_MaxIpAddress] of AnsiChar;
  end;

// Describes the results of a Ipv6 projection info - new with Vista

  PRasPppIpV6 = ^TRasPppIpV6;
  TRasPppIpV6 = record
    dwSize: DWORD;
    dwError: DWORD;
    bLocalInterfaceIdentifier: array[0..7] of Byte;
    bPeerInterfaceIdentifier: array[0..7] of Byte;
    bLocalCompressionProtocol: array[0..1] of Byte;
    bPeerCompressionProtocol: array[0..1] of Byte;
  end;

// Protocol code to projection data structure mapping.
const
// TRasProjection literals for RasGetProjectionInfo
    RASP_Amb        = $10000 ;
    RASP_PppNbf     = $803F ;
    RASP_PppIpx     = $802B ;
    RASP_PppIp      = $8021 ;
    RASP_PppCcp     = $80FD ;
    RASP_PppLcp     = $C021 ;
    RASP_PppIpv6    = $8057 ;  // added for Vista
    RASP_Slip       = $20000 ; // gone with Vista

// RASPPPLCP 'dwAuthenticatonProtocol' values.
    RASLCPAP_PAP    = $C023 ;
    RASLCPAP_SPAP   = $C123 ;
    RASLCPAP_CHAP   = $C223 ;
    RASLCPAP_EAP    = $C227 ;

// RASPPPLCP 'dwAuthenticatonData' values.
    RASLCPAD_CHAP_MD5   = $05 ;
    RASLCPAD_CHAP_MS    = $80 ;
    RASLCPAD_CHAP_MSV2  = $81 ;

// RASPPPLCP 'dwOptions' values and 'dwServerOptions' flags.
    RASLCPO_PFC        = $00000001 ;
    RASLCPO_ACFC       = $00000002 ;
    RASLCPO_SSHF       = $00000004 ;
    RASLCPO_DES_56     = $00000008 ;
    RASLCPO_3_DES      = $00000010 ;
    RASLCPO_AES_128	   = $00000020 ;  // added for Vista
    RASLCPO_AES_256	   = $00000040 ;  // added for Vista

// RASPPPCCP 'dwCompressionAlgorithm' values.
    RASCCPCA_MPPC      = $00000006 ;
    RASCCPCA_STAC      = $00000005 ;

// RASPPPCCP 'dwOptions' values.
    RASCCPO_Compression      = $00000001 ;
    RASCCPO_HistoryLess      = $00000002 ;
    RASCCPO_Encryption56bit  = $00000010 ;
    RASCCPO_Encryption40bit  = $00000020 ;
    RASCCPO_Encryption128bit = $00000040 ;
    RAS_IPSEC_ESP_DES        = $00000080 ;
    RAS_IPSEC_ESP_DES_40     = $00000100 ;
    RAS_IPSEC_ESP_3_DES      = $00000200 ;

//Flags for RasConnectionNotification()
    RASCN_Connection       = $00000001;
    RASCN_Disconnection    = $00000002;
    RASCN_BandwidthAdded   = $00000004;
    RASCN_BandwidthRemoved = $00000008;

type
//  Information describing a RAS-capable device - ie modems or ISDN cards

  PRasDevInfoA = ^TRasDevInfoA;
  TRasDevInfoA = record
    dwSize: DWORD;
    szDeviceType: Array[0..RAS_MaxDeviceType] of AnsiChar;
    szDeviceName: Array[0..RAS_MaxDeviceName] of AnsiChar;
  end;


// RAS Country Information (currently retreieved from TAPI).

  PRasCtryInfo = ^TRasCtryInfo;
  TRasCtryInfo = record
    dwSize,
    dwCountryID,
    dwNextCountryID,
    dwCountryCode,
    dwCountryNameOffset: DWORD;
  end;

  TRasExtCtryInfoA = record             // version for RasGetCountryInfo
    RasCtryInfo: TRasCtryInfo ;
    buffer: array [0..255] of AnsiChar ;   // returns the country name
  end ;

// A RAS IP Address.

  PRasIPAddr = ^TRasIPAddr;
  TRasIPAddr = record
    a, b, c, d: Byte;
  end;

// A RAS IPv6 address, added for Vista

  in6_addr = record
    case Integer of
      0: (Byte: array[0..15] of Byte);
      1: (Word: array[0..7] of Word);
  end;
  in_addr6 = in6_addr;
  Tin6_addr = in6_addr;
  TRasIpV6Addr = in6_addr;

// A RAS phonebook entry, five versions, minimum Win9x, NT4, W2K, XP/2003, Vista/2008

  PRasEntryA = ^TRasEntryA;
  TRasEntryA = record
    dwSize,
    dwfOptions,
    //
    // Location/phone number.
    //
    dwCountryID,
    dwCountryCode: DWORD;
    szAreaCode: array[0.. RAS_MaxAreaCode] of AnsiChar;
    szLocalPhoneNumber: array[0..RAS_MaxPhoneNumber] of AnsiChar;
    dwAlternateOffset: DWORD;
    //
// PPP/Ip
    //
    ipaddr,
    ipaddrDns,
    ipaddrDnsAlt,
    ipaddrWins,
    ipaddrWinsAlt: TRasIPAddr;
    //
    // Framing
    //
    dwFrameSize,
    dwfNetProtocols,
    dwFramingProtocol: DWORD;
    //
    // Scripting
    //
    szScript: Array[0..MAX_PATH - 1] of AnsiChar;
    //
    // AutoDial
    //
    szAutodialDll: Array [0..MAX_PATH - 1] of AnsiChar;
    szAutodialFunc: Array [0..MAX_PATH - 1] of AnsiChar;
    //
    // Device
    //
    szDeviceType: Array [0..RAS_MaxDeviceType] of AnsiChar;
    szDeviceName: Array [0..RAS_MaxDeviceName] of AnsiChar;
    //
    // X.25
    //
    szX25PadType: Array [0..RAS_MaxPadType] of AnsiChar;
    szX25Address: Array [0..RAS_MaxX25Address] of AnsiChar;
    szX25Facilities: Array [0..RAS_MaxFacilities] of AnsiChar;
    szX25UserData: Array [0..RAS_MaxUserData] of AnsiChar;
    //
    // ISDN number of B channels, only if DeviceType=isdn
    //
    dwChannels: DWORD;
    //
    // Reserved
    //
    dwReserved1,
    dwReserved2: DWORD;
    //
  end;   // followed by alternate numbers

  PRasEntryNT4A = ^TRasEntryNT4A ;    // this version fails under Win9x (might be OK with DUN 1.3)
  TRasEntryNT4A = record
    dwSize,
    dwfOptions,
    //
    // Location/phone number.
    //
    dwCountryID,
    dwCountryCode: DWORD;
    szAreaCode: array[0.. RAS_MaxAreaCode] of AnsiChar;
    szLocalPhoneNumber: array[0..RAS_MaxPhoneNumber] of AnsiChar;
    dwAlternateOffset: DWORD;
    //
// PPP/Ip
    //
    ipaddr,
    ipaddrDns,
    ipaddrDnsAlt,
    ipaddrWins,
    ipaddrWinsAlt: TRasIPAddr;
    //
    // Framing
    //
    dwFrameSize,
    dwfNetProtocols,
    dwFramingProtocol: DWORD;
    //
    // Scripting
    //
    szScript: Array[0..MAX_PATH - 1] of AnsiChar;
    //
    // AutoDial
    //
    szAutodialDll: Array [0..MAX_PATH - 1] of AnsiChar;
    szAutodialFunc: Array [0..MAX_PATH - 1] of AnsiChar;
    //
    // Device
    //
    szDeviceType: Array [0..RAS_MaxDeviceType] of AnsiChar;
    szDeviceName: Array [0..RAS_MaxDeviceName] of AnsiChar;
    //
    // X.25
    //
    szX25PadType: Array [0..RAS_MaxPadType] of AnsiChar;
    szX25Address: Array [0..RAS_MaxX25Address] of AnsiChar;
    szX25Facilities: Array [0..RAS_MaxFacilities] of AnsiChar;
    szX25UserData: Array [0..RAS_MaxUserData] of AnsiChar;
    //
    // ISDN number of B channels, only if DeviceType=isdn
    //
    dwChannels: DWORD;
    //
    // Reserved
    //
    dwReserved1,
    dwReserved2: DWORD;
    //
    // Multilink
    //
    dwSubEntries,
    dwDialMode,
    dwDialExtraPercent,
    dwDialExtraSampleSeconds,
    dwHangUpExtraPercent,
    dwHangUpExtraSampleSeconds: DWORD;
    //
    // Idle timeout
    //
    dwIdleDisconnectSeconds: DWORD;
  end;    // followed by alternate numbers

  PRasEntryW2KA = ^TRasEntryW2KA ;    // this version fails under Win9x and NT4
  TRasEntryW2KA = record
    dwSize,
    dwfOptions,
    //
    // Location/phone number.
    //
    dwCountryID,
    dwCountryCode: DWORD;
    szAreaCode: array[0.. RAS_MaxAreaCode] of AnsiChar;
    szLocalPhoneNumber: array[0..RAS_MaxPhoneNumber] of AnsiChar;
    dwAlternateOffset: DWORD;
    //
// PPP/Ip
    //
    ipaddr,
    ipaddrDns,
    ipaddrDnsAlt,
    ipaddrWins,
    ipaddrWinsAlt: TRasIPAddr;
    //
    // Framing
    //
    dwFrameSize,
    dwfNetProtocols,
    dwFramingProtocol: DWORD;
    //
    // Scripting
    //
    szScript: Array[0..MAX_PATH - 1] of AnsiChar;
    //
    // AutoDial
    //
    szAutodialDll: Array [0..MAX_PATH - 1] of AnsiChar;
    szAutodialFunc: Array [0..MAX_PATH - 1] of AnsiChar;
    //
    // Device
    //
    szDeviceType: Array [0..RAS_MaxDeviceType] of AnsiChar;
    szDeviceName: Array [0..RAS_MaxDeviceName] of AnsiChar;
    //
    // X.25
    //
    szX25PadType: Array [0..RAS_MaxPadType] of AnsiChar;
    szX25Address: Array [0..RAS_MaxX25Address] of AnsiChar;
    szX25Facilities: Array [0..RAS_MaxFacilities] of AnsiChar;
    szX25UserData: Array [0..RAS_MaxUserData] of AnsiChar;
    //
    // ISDN number of B channels, only if DeviceType=isdn
    //
    dwChannels: DWORD;
    //
    // Reserved
    //
    dwReserved1,
    dwReserved2: DWORD;
    //
    // Multilink - NT4 and W2K
    //
    dwSubEntries,
    dwDialMode,
    dwDialExtraPercent,          // note - percent stuff only supported by W2K
    dwDialExtraSampleSeconds,
    dwHangUpExtraPercent,
    dwHangUpExtraSampleSeconds: DWORD;
    //
    // Idle timeout
    //
    dwIdleDisconnectSeconds: DWORD;
    //
    // Windows 2000 extensions
    //
    dwType,                     // entry type
    dwEncryptionType,           // type of encryption to use
    dwCustomAuthKey: DWORD ;    // authentication key for EAP
    guidId: TGUID ;              // guid that represents the phone-book entry
    szCustomDialDll: Array [0..MAX_PATH - 1] of AnsiChar;  // DLL for custom dialing
    dwVpnStrategy: DWORD ;      // specifies type of VPN protocol
  end;  // followed by alternate numbers

  PRasEntryXPA = ^TRasEntryXPA ;    // also Windows 2003, this version fails under Win9x, NT4 and W2K
  TRasEntryXPA = record
    dwSize,
    dwfOptions,
    //
    // Location/phone number.
    //
    dwCountryID,
    dwCountryCode: DWORD;
    szAreaCode: array[0.. RAS_MaxAreaCode] of AnsiChar;
    szLocalPhoneNumber: array[0..RAS_MaxPhoneNumber] of AnsiChar;
    dwAlternateOffset: DWORD;
    //
// PPP/Ip
    //
    ipaddr,
    ipaddrDns,
    ipaddrDnsAlt,
    ipaddrWins,
    ipaddrWinsAlt: TRasIPAddr;
    //
    // Framing
    //
    dwFrameSize,
    dwfNetProtocols,
    dwFramingProtocol: DWORD;
    //
    // Scripting
    //
    szScript: Array[0..MAX_PATH - 1] of AnsiChar;
    //
    // AutoDial
    //
    szAutodialDll: Array [0..MAX_PATH - 1] of AnsiChar;
    szAutodialFunc: Array [0..MAX_PATH - 1] of AnsiChar;
    //
    // Device
    //
    szDeviceType: Array [0..RAS_MaxDeviceType] of AnsiChar;
    szDeviceName: Array [0..RAS_MaxDeviceName] of AnsiChar;
    //
    // X.25
    //
    szX25PadType: Array [0..RAS_MaxPadType] of AnsiChar;
    szX25Address: Array [0..RAS_MaxX25Address] of AnsiChar;
    szX25Facilities: Array [0..RAS_MaxFacilities] of AnsiChar;
    szX25UserData: Array [0..RAS_MaxUserData] of AnsiChar;
    //
    // ISDN number of B channels, only if DeviceType=isdn
    //
    dwChannels: DWORD;
    //
    // Reserved
    //
    dwReserved1,
    dwReserved2: DWORD;
    //
    // Multilink - NT4 and W2K
    //
    dwSubEntries,
    dwDialMode,
    dwDialExtraPercent,          // note - percent stuff only supported by W2K
    dwDialExtraSampleSeconds,
    dwHangUpExtraPercent,
    dwHangUpExtraSampleSeconds: DWORD;
    //
    // Idle timeout
    //
    dwIdleDisconnectSeconds: DWORD;
    //
    // Windows 2000 extensions
    //
    dwType,                     // entry type
    dwEncryptionType,           // type of encryption to use
    dwCustomAuthKey: DWORD ;    // authentication key for EAP
    guidId: TGUID ;              // guid that represents the phone-book entry
    szCustomDialDll: Array [0..MAX_PATH - 1] of AnsiChar;  // DLL for custom dialing
    dwVpnStrategy: DWORD ;      // specifies type of VPN protocol
    //
    // Windows XP extensions
    //
    dwfOptions2: DWORD ;  // a set of bits that specify connection options
    dwfOptions3: DWORD ;  // spare
	dzDnsSuffix: Array [0..RAS_MaxDnsSuffix - 1] of AnsiChar; // Domain Name Service (DNS) suffix for the connection
	dwTcpWindowSize: DWORD ;     // TCP window size for all sessions on this connection, increase for high latency devices like mobile phones
	szPrerequisitePbk: Array [0..MAX_PATH - 1] of AnsiChar;  // VPN only, see next element
	szPrerequisiteEntry: Array [0..RAS_MaxEntryName] of AnsiChar;  // VPN only, an entry RAS dials before establishing this connecton
	dwRedialCount: DWORD ;    // number of times RAS attempts to dial
	dwRedialPause: DWORD ;    // seconds to wait between retry attempts 
  end;  // followed by alternate numbers

  PRasEntryVistaA = ^TRasEntryVistaA ;   // also Windows 2008/7, this version fails under Win9x, NT4, W2K and XP
  TRasEntryVistaA = record
    dwSize,
    dwfOptions,
    //
    // Location/phone number.
    //
    dwCountryID,
    dwCountryCode: DWORD;
    szAreaCode: array[0.. RAS_MaxAreaCode] of AnsiChar;
    szLocalPhoneNumber: array[0..RAS_MaxPhoneNumber] of AnsiChar;
    dwAlternateOffset: DWORD;
    //
// PPP/Ip
    //
    ipaddr,
    ipaddrDns,
    ipaddrDnsAlt,
    ipaddrWins,
    ipaddrWinsAlt: TRasIPAddr;
    //
    // Framing
    //
    dwFrameSize,
    dwfNetProtocols,
    dwFramingProtocol: DWORD;
    //
    // Scripting
    //
    szScript: Array[0..MAX_PATH - 1] of AnsiChar;
    //
    // AutoDial
    //
    szAutodialDll: Array [0..MAX_PATH - 1] of AnsiChar;
    szAutodialFunc: Array [0..MAX_PATH - 1] of AnsiChar;
    //
    // Device
    //
    szDeviceType: Array [0..RAS_MaxDeviceType] of AnsiChar;
    szDeviceName: Array [0..RAS_MaxDeviceName] of AnsiChar;
    //
    // X.25
    //
    szX25PadType: Array [0..RAS_MaxPadType] of AnsiChar;
    szX25Address: Array [0..RAS_MaxX25Address] of AnsiChar;
    szX25Facilities: Array [0..RAS_MaxFacilities] of AnsiChar;
    szX25UserData: Array [0..RAS_MaxUserData] of AnsiChar;
    //
    // ISDN number of B channels, only if DeviceType=isdn
    //
    dwChannels: DWORD;
    //
    // Reserved
    //
    dwReserved1,
    dwReserved2: DWORD;
    //
    // Multilink - NT4 and W2K
    //
    dwSubEntries,
    dwDialMode,
    dwDialExtraPercent,          // note - percent stuff only supported by W2K
    dwDialExtraSampleSeconds,
    dwHangUpExtraPercent,
    dwHangUpExtraSampleSeconds: DWORD;
    //
    // Idle timeout
    //
    dwIdleDisconnectSeconds: DWORD;
    //
    // Windows 2000 extensions
    //
    dwType,                     // entry type
    dwEncryptionType,           // type of encryption to use
    dwCustomAuthKey: DWORD ;    // authentication key for EAP
    guidId: TGUID ;              // guid that represents the phone-book entry
    szCustomDialDll: Array [0..MAX_PATH - 1] of AnsiChar;  // DLL for custom dialing
    dwVpnStrategy: DWORD ;      // specifies type of VPN protocol
    //
    // Windows XP extensions
    //
    dwfOptions2: DWORD ;  // a set of bits that specify connection options
    dwfOptions3: DWORD ;  // spare
	dzDnsSuffix: Array [0..RAS_MaxDnsSuffix - 1] of AnsiChar; // Domain Name Service (DNS) suffix for the connection
	dwTcpWindowSize: DWORD ;     // TCP window size for all sessions on this connection, increase for high latency devices like mobile phones
	szPrerequisitePbk: Array [0..MAX_PATH - 1] of AnsiChar;  // VPN only, see next element
	szPrerequisiteEntry: Array [0..RAS_MaxEntryName] of AnsiChar;  // VPN only, an entry RAS dials before establishing this connecton
	dwRedialCount: DWORD ;    // number of times RAS attempts to dial
	dwRedialPause: DWORD ;    // seconds to wait between retry attempts
    //
    // PPP/IPv6 added with Vista
    //
    ipv6addrDns: TRasIpV6Addr;
    ipv6addrDnsAlt: TRasIpV6Addr;
    dwIPv4InterfaceMetric: DWORD ;  // 5.50 added during lifetime of Vista
    dwIPv6InterfaceMetric: DWORD ;  // 5.50 added during lifetime of Vista

  end;  // followed by alternate numbers

// A RAS phone book multilinked sub-entry - NT4 and W2K

type

  LpRasSubEntryNT4A = ^TRasSubEntryNT4A;
  TRasSubEntryNT4A = record
    dwSize,
    dwfFlags: DWORD;
    //
    // Device
    //
    szDeviceType: Array[0..RAS_MaxDeviceType] Of AnsiChar;
    szDeviceName: Array[0..RAS_MaxDeviceName] Of AnsiChar;
    //
    // Phone numbers
    //
    szLocalPhoneNumber: Array[0..RAS_MaxPhoneNumber] Of AnsiChar;
    dwAlternateOffset: DWORD;
  end;    // followed by alternate numbers

  PRasProjection = ^TRasProjection;
  TRasProjection = Integer;

const

// TRasEntry 'dwfOptions' bit flags.

    RASEO_UseCountryAndAreaCodes    = $00000001;
    RASEO_SpecificIpAddr            = $00000002;
    RASEO_SpecificNameServers       = $00000004;
    RASEO_IpHeaderCompression       = $00000008;
    RASEO_RemoteDefaultGateway      = $00000010;
    RASEO_DisableLcpExtensions      = $00000020;
    RASEO_TerminalBeforeDial        = $00000040;
    RASEO_TerminalAfterDial         = $00000080;
    RASEO_ModemLights               = $00000100;
    RASEO_SwCompression             = $00000200;
    RASEO_RequireEncryptedPw        = $00000400;
    RASEO_RequireMsEncryptedPw      = $00000800;
    RASEO_RequireDataEncryption     = $00001000;
    RASEO_NetworkLogon              = $00002000;
    RASEO_UseLogonCredentials       = $00004000;
    RASEO_PromoteAlternates         = $00008000;
    RASEO_SecureLocalFiles          = $00010000;    // NT4 and W2K
    RASEO_RequireEAP                = $00020000;    // following W2K only
    RASEO_RequirePAP                = $00040000;
    RASEO_RequireSPAP               = $00080000;
    RASEO_Custom                    = $00100000;
    RASEO_PreviewPhoneNumber        = $00200000;
    RASEO_SharedPhoneNumbers        = $00800000;
    RASEO_PreviewUserPw             = $01000000;
    RASEO_PreviewDomain             = $02000000;
    RASEO_ShowDialingProgress       = $04000000;
    RASEO_RequireCHAP               = $08000000;
    RASEO_RequireMsCHAP             = $10000000;
    RASEO_RequireMsCHAP2            = $20000000;
    RASEO_RequireW95MSCHAP          = $40000000;
    RASEO_CustomScript              = $80000000;     // 4.51

// TRasEntry dwfOptions2 bit flags   Windows XP

    RASEO2_SecureFileAndPrint       = $00000001;  // also disables NBT probing
    RASEO2_SecureClientForMSNet     = $00000002;  // also disables NBT probing
    RASEO2_DontNegotiateMultilink   = $00000004;  // 'negotiate multilink for single link connection'
    RASEO2_DontUseRasCredentials    = $00000008;  // use default credentials
    RASEO2_UsePreSharedKey          = $00000010;  // use a pre-shared key for authentication
    RASEO2_Internet                 = $00000020;  // the connection is to the internet (but why?)
    RASEO2_DisableNbtOverIP         = $00000040;  // disable NBT probing
    RASEO2_UseGlobalDeviceSettings  = $00000080;  // ignore device settings in phonebook and use those from modem applet
    RASEO2_ReconnectIfDropped       = $00000100;  // 'redial if line is dropped'
    RASEO2_SharePhoneNumbers        = $00000200;  // 'all devices call the same number' for multilink subentries

// TRasEntry dwfOptions2 bit flags   Windows Vista and 2008
    RASEO2_SecureRoutingCompartment    = $00000400;
    RASEO2_UseTypicalSettings          = $00000800;
    RASEO2_IPv6SpecificNameServers     = $00001000;
    RASEO2_IPv6RemoteDefaultGateway    = $00002000;
    RASEO2_RegisterIpWithDNS           = $00004000;
    RASEO2_UseDNSSuffixForRegistration = $00008000;
    RASEO2_IPv4ExplicitMetric          = $00010000;    // 5.50 added during lifetime of Vista
    RASEO2_IPv6ExplicitMetric          = $00020000;    // 5.50 added during lifetime of Vista
    RASEO2_DisableIKENameEkuCheck      = $00040000;    // 5.50 added during lifetime of Vista

// TRasEntry 'dwfNetProtocols' bit flags. (session negotiated protocols)

    RASNP_Netbeui   = $00000001;  // Negotiate NetBEUI - dropped from Windows XP and later
    RASNP_Ipx       = $00000002;  // Negotiate IPX - dropped from Windows 2000 64-bit and later
    RASNP_Ip        = $00000004;  // Negotiate TCP/IP
    RASNP_Ipv6      = $00000008;  // IpV6 added with Vista 

// TRasEntry 'dwFramingProtocols' (framing protocols used by the server)

    RASFP_Ppp  = $00000001;  // Point-to-Point Protocol (PPP)
    RASFP_Slip = $00000002;  // Serial Line Internet Protocol (SLIP)
    RASFP_Ras  = $00000004;  // Microsoft proprietary protocol

// TRasEntry 'szDeviceType' strings

    RASDT_Modem     = 'modem';
    RASDT_Isdn      = 'isdn';
    RASDT_X25       = 'x25';
    RASDT_Vpn       = 'vpn';
    RASDT_Pad       = 'pad';
    RASDT_Generic   = 'generic';  // following are Windows 2000 only
    RASDT_Serial    = 'serial';
    RASDT_FrameRelay= 'framerelay';
    RASDT_Atm       = 'atm';
    RASDT_Sonet     = 'sonet';
    RASDT_SW56      = 'sw56';
    RASDT_Irda      = 'irda';
    RASDT_Parallel  = 'parallel';
    RASDT_PPPoE     = 'pppoe' ;   // Windows XP Point-to-Point Protocol over Ethernet

// TRasEntry 'dwDialMode' values.
    RASEDM_DialAll      = 1;
    RASEDM_DialAsNeeded = 2;

// TRasEntry 'dwIdleDisconnectSeconds' constants
    RASIDS_Disabled       = $ffffffff;
    RASIDS_UseGlobalValue = 0;

// The entry type used to determine which UI properties
// are to be presented to user.
    RASET_Phone    = 1 ;     // Phone lines: modem, ISDN, X.25, etc
    RASET_Vpn      = 2 ;     // Virtual private network
    RASET_Direct   = 3 ;     // Direct connect: serial, parallel
    RASET_Internet = 4 ;     // Connection Manager (CM), reserved for internal use
    RASET_Broadband = 5 ;    // Windows XP - DSL

// TRasEntry 'dwEncryptionType', NT4?
    ET_40Bit        = 1 ;
    ET_128Bit       = 2 ;

// dwEncryptionType, alternative to the above for W2K
    ET_None         = 0 ;  // No encryption
    ET_Require      = 1 ;  // Require Encryption
    ET_RequireMax   = 2 ;  // Require max encryption
    ET_Optional     = 3 ;  // Do encryption if possible. None Ok.  // 4.51

// TRasEntry 'dwVnpStrategy'
    VS_Default      = 0 ;  // default (PPTP for now)
    VS_PptpOnly     = 1 ;  // Only PPTP is attempted.
    VS_PptpFirst    = 2 ;  // PPTP is tried first.
    VS_L2tpOnly     = 3 ;  // Only L2TP is attempted.
    VS_L2tpFirst    = 4 ;  // L2TP is tried first.

type

// AutoDial DLL function parameter block

  LpRasADParams = ^TRasADParams;
  TRasADParams = record
    dwSize: DWORD;
    hwndOwner: THandle;
    dwFlags: DWORD;
    xDlg,
    yDlg: DWORD;
  end;

const

// AutoDial DLL function parameter block 'dwFlags.'

  RASADFLG_PositionDlg = $00000001;

type

// AutoDial address properties

  LPRasAutoDialEntryA = ^TRasAutoDialEntryA;
  TRasAutoDialEntryA = record
    dwSize,
    dwFlags,
    dwDialingLocation: DWORD;
    szEntry: Array[0..RAS_MaxEntryName] Of AnsiChar;
  end;


const

//  AutoDial control parameter values for
//  Ras(Get,Set)AutodialParam.

  RASADP_DisableConnectionQuery  = 0;
  RASADP_LoginSessionDisable     = 1;
  RASADP_SavedAddressesLimit     = 2;
  RASADP_FailedConnectionTimeout = 3;
  RASADP_ConnectionQueryTimeout  = 4;


// RAS statistics structure, Windows 2000/NT 5 only

type
    PTRas_Stats = ^TRas_Stats ;
    TRas_Stats = Record
        dwSize,
        dwBytesXmited,
        dwBytesRcved,
        dwFramesXmited,
        dwFramesRcved,
        dwCrcErr,
        dwTimeoutErr,
        dwAlignmentErr,
        dwHardwareOverrunErr,
        dwFramingErr,
        dwBufferOverrunErr,
        dwCompressionRatioIn,
        dwCompressionRatioOut,
        dwBps,
        dwConnectDuration: DWORD ;
    end ;

// dialogs for NT4 and later from rasdlg.h

// RasPhonebookDlg API callback.

type
  TRasPbdDlgFuncA = procedure (dwCallbackId: Pointer; dwEvent: DWORD;  // 22 July 2011 callback was DWORD
                            pszText: PAnsiChar; pData: Pointer); stdcall;
//  TRasPbdDlgFunc = TRasPbdDlgFuncA;

const
  RASPBDEVENT_AddEntry = 1;
  RASPBDEVENT_EditEntry = 2;
  RASPBDEVENT_RemoveEntry = 3;
  RASPBDEVENT_DialEntry = 4;
  RASPBDEVENT_EditGlobals = 5;
  RASPBDEVENT_NoUser = 6;
  RASPBDEVENT_NoUserEdit = 7;

  RASNOUSER_SmartCard = $00000001;

// Defines the information passed in the 4th argument of RASPBDLGFUNC on
// "NoUser" and "NoUserEdit" events. Usage shown is for "NoUser". For
// "NoUserEdit", the timeout is ignored and the three strings are INs.
type
  PRasNoUserA = ^TRasNoUserA;
//  PRasNoUser = PRasNoUserA;
  tagRASNOUSERA = record
    dwSize: DWORD;
    dwFlags: DWORD;
    dwTimeoutMs: DWORD;
    szUserName: array[0..UNLEN] of AnsiChar;  // 18 Aug 2011
    szPassword: array[0..PWLEN] of AnsiChar;  // 18 Aug 2011
    szDomain: array[0..DNLEN] of AnsiChar;    // 18 Aug 2011
  end;
  TRasNoUserA = tagRASNOUSERA;
//  TRasNoUser = TRasNoUserA;

// RasPhonebookDlg API parameters.
const
  RASPBDFLAG_PositionDlg = $00000001;
  RASPBDFLAG_ForceCloseOnDial = $00000002;
  RASPBDFLAG_NoUser = $00000010;
  RASPBDFLAG_UpdateDefaults = $80000000;

type
  PRasPbdDlgA = ^TRasPbdDlgA;
//  PRasPbdDlg = PRasPbdDlgA;
  tagRASPBDLGA = record
    dwSize: DWORD;
    hwndOwner: HWND;
    dwFlags: DWORD;
    xDlg: LongInt;
    yDlg: LongInt;
    dwCallbackId: Pointer ;  // 22 July 2011 was DWORD
    pCallback: TRasPbdDlgFuncA;
    dwError: DWORD;
    reserved: Pointer ;  // 22 July 2011 was DWORD
    reserved2: Pointer ;  // 22 July 2011 was DWORD
  end;
  TRasPbdDlgA = tagRASPBDLGA;
//  TRasPbdDlg = TRasPbdDlgA;

// RasEntryDlg API parameters.
const
  RASEDFLAG_PositionDlg = $00000001;
  RASEDFLAG_NewEntry = $00000002;
  RASEDFLAG_CloneEntry = $00000004;
  RASEDFLAG_NoRename = $00000008;
  RASEDFLAG_ShellOwned = $40000000;
  RASEDFLAG_NewPhoneEntry = $00000010;
  RASEDFLAG_NewTunnelEntry = $00000020;
  RASEDFLAG_NewDirectEntry = $00000040;
  RASEDFLAG_NewBroadbandEntry = $00000080;
  RASEDFLAG_InternetEntry = $00000100;
  RASEDFLAG_IncomingConnection = $00000400;

type
  PRasEntryDlgA = ^TRasEntryDlgA;
//  PRasEntryDlg = PRasEntryDlgA;
  tagRASENTRYDLGA = record
    dwSize: DWORD;
    hwndOwner: HWND;
    dwFlags: DWORD;
    xDlg: LongInt;
    yDlg: LongInt;
    szEntry: array[0..RAS_MaxEntryName] of AnsiChar;  // 18 Aug 2011
    dwError: DWORD;
    reserved: Pointer ;  // 22 July 2011 was DWORD
    reserved2: Pointer ;  // 22 July 2011 was DWORD
  end;
  TRasEntryDlgA = tagRASENTRYDLGA;
//  TRasEntryDlg = TRasEntryDlgA;

// RasDialDlg API parameters.
const
  RASDDFLAG_PositionDlg = $00000001;
  RASDDFLAG_NoPrompt = $00000002;
  RASDDFLAG_NoErrorDlg = $00000004;
  RASDDFLAG_LinkFailure = $80000000;

type
  PRasDialDlg = ^TRasDialDlg;
  tagRASDIALDLG = record
    dwSize: DWORD;
    hwndOwner: HWND;
    dwFlags: DWORD;
    xDlg: LongInt;
    yDlg: LongInt;
    dwSubEntry: DWORD;
    dwError: DWORD;
    reserved: Pointer ;  // 22 July 2011 was DWORD
    reserved2: Pointer ;  // 22 July 2011 was DWORD
  end;
  TRasDialDlg = tagRASDIALDLG;

// RAS functions, all loaded dynamically since some only available in certain OSs
var

RasDialA: Function (
    lpRasDialExtensions : PRasDialExtensionsW2K;    // pointer to function extensions data
    lpszPhonebook: PAnsiChar;   // pointer to full path and filename of phonebook file
    lpRasDialParams : PRasDialParamsNT4A;    // pointer to calling parameters data
    dwNotifierType : DWORD; // specifies type of RasDial event handler
    lpvNotifier: Pointer;   // specifies a handler for RasDial events
    var rasConn: HRASConn   // pointer to variable to receive connection handle
    ): DWORD; stdcall;

RasEnumConnectionsA: Function (
    lpRASConn: PRasConnA;      { buffer to receive Connections data }
    var BufSize: DWord;     { size in bytes of buffer }
    var Connections: DWord  { number of Connections written to buffer }
    ): DWORD; stdcall;

RasEnumEntriesA: Function (
    reserved: PAnsiChar;    // reserved, must be NULL
    lpszPhonebook: PAnsiChar  ; // pointer to full path and filename of phonebook file
    lprasentryname: PRasEntryNameW2KA ;  // buffer to receive phonebook entries
    var lpcb :  DWORD;// size in bytes of buffer
    var lpcEntries : DWORD// number of entries written to buffer
    ) : DWORD; stdcall;

RasGetConnectStatusA: Function (
    RASConn: hrasConn;  { handle to Remote Access Connection of interest }
    RASConnStatus: PRASConnStatusNT4A    { buffer to receive status data }
    ): DWORD; stdcall;

RasGetErrorStringA: Function (
    ErrorCode: DWord;   { error code to get string for }
    szErrorString: PAnsiChar;   { buffer to hold error string }
    BufSize: DWord  { sizeof buffer }
    ): DWORD; stdcall;

RasHangUpA: Function (
    RASConn: hrasConn   { handle to the Remote Access Connection to hang up }
    ): DWORD; stdcall;

RasGetEntryDialParamsA: Function (         // ANGUS
    lpszPhonebook:PAnsiChar;    // pointer to the full path and filename of the phonebook file
    var lprasdialparams: TRasDialParamsNT4A; // pointer to a structure that receives the connection parameters
    var lpfPassword : BOOL  // indicates whether the user's password was retrieved
    ): DWORD; stdcall;

RasSetEntryDialParamsA: Function (         // ANGUS
    lpszPhonebook:PAnsiChar;    // pointer to the full path and filename of the phonebook file
    var lprasdialparams: TRasDialParamsNT4A; // pointer to a structure that contains the connection parameters
    fRemovePassword: BOOL   // // indicates whether to remove password from entry's parameters
    ): DWORD; stdcall;

RasMonitorDlgA: Function (
    lpszDeviceName:PAnsiChar;   // pointer to the name of the device to display initially
    var lprasmonitordlg:TRasRasMonitorDlg   // pointer to a structure with I/O parms
    ): DWORD; stdcall;

RasEditPhonebookEntryA: Function (    // ANGUS
    hWndParent : HWND;   // handle to the parent window of the dialog box
    lpszPhonebook : PAnsiChar; // pointer to the full path and filename of the phonebook file
    lpszEntryName : PAnsiChar  // pointer to the phonebook entry name
    ) : DWORD; stdcall;

RasCreatePhonebookEntryA: Function ( // ANGUS
    hWndParent : HWND;  // handle to the parent window of the dialog box
    lpszPhonebook : PAnsiChar // pointer to the full path and filename of the phonebook file
    ) : DWORD; stdcall;

RasGetProjectionInfoA: Function (    // ANGUS
    hConn: HRasConn;
    rasproj: TRasProjection;
    lpProjection: Pointer;
    var lpcb: DWORD
    ): DWORD; stdcall;

RasGetCountryInfoA: Function (   // ANGUS  -  from RNAPH.DLL
    lpCtryInfo: PAnsiChar;
    var lpdwSize: DWORD
    ): DWORD; stdcall;

RasGetEntryPropertiesA: Function (   // ANGUS     -  from RNAPH.DLL
    lpszPhonebook,
    szEntry: PAnsiChar;
    lpbEntry: PAnsiChar;              // Angus 4.11
    var lpdwEntrySize: DWORD;         // Angus 5.40 made var
    lpbDeviceInfo: PAnsiChar;         // Angus 4.11
    var lpdwDeviceInfoSize: DWORD     // Angus 5.40 made var
    ): DWORD; stdcall;

RasSetEntryPropertiesA: Function (   // ANGUS    -  from RNAPH.DLL
    lpszPhonebook,
    szEntry: PAnsiChar;
    lpbEntry: PAnsiChar;               // Angus 4.11
    dwEntrySize: DWORD;
    lpbDeviceInfo: PAnsiChar;          // Angus 4.11
    dwDeviceInfoSize: DWORD
    ): DWORD; stdcall;

RasRenameEntryA: Function (  // ANGUS         -  from RNAPH.DLL
    lpszPhonebook,
    szEntryOld,
    szEntryNew: PAnsiChar
    ): DWORD; stdcall;

RasDeleteEntryA: Function (  // ANGUS          -  from RNAPH.DLL
    lpszPhonebook,
    szEntry: PAnsiChar
    ): DWORD; stdcall;

RasValidateEntryNameA: Function (    // ANGUS     -  from RNAPH.DLL
    lpszPhonebook,
    szEntry: PAnsiChar
    ): DWORD; stdcall;

RasEnumDevicesA: Function (  // ANGUS      -  from RNAPH.DLL
    lpBuff: PRasDevInfoA;
    var lpcbSize: DWORD;
    var lpcDevices: DWORD
    ): DWORD; stdcall;


// following functions are NT4 and later only


RasGetSubEntryHandleA: Function (    // ANGUS     - NT4 only
    hrasconn: HRasConn;  // 20 July 2011, was hrasconn which was duplicate
    dwSubEntry: DWORD;
    var lphrasconn: HRasConn
    ): DWORD; stdcall;

RasGetCredentialsA: Function (   // ANGUS     - NT4 only
    lpszPhoneBook,
    lpszEntry: PAnsiChar;
    var lpCredentials: TRasCredentialsA
    ): DWORD; stdcall;

RasSetCredentialsA: Function (   // ANGUS     - NT4 only
    lpszPhoneBook,
    lpszEntry: PAnsiChar;
    var lpCredentials: TRasCredentialsA;
    fRemovePassword: LongBool
    ): DWORD; stdcall;

RasConnectionNotificationA: Function (   // ANGUS     - NT4 only
  rasconn: HRasConn;  // 20 July 2011, was hrasconn which was duplicate
  hEvent: THandle;
  dwFlags: DWORD
  ): DWORD; stdcall;

RasGetSubEntryPropertiesA: Function (    // ANGUS     - NT4 only
    lpszPhoneBook,
    lpszEntry: PAnsiChar;
    dwSubEntry: DWORD;
    lpRasSubEntry: PAnsiChar;            // Angus 4.11
    var lpdwcb: DWORD;                   // Angus 5.40 made var
    lpbDeviceInfo: PAnsiChar;            // Angus 4.11  unused
    var lpdwDeviceInfoSize: DWORD        // Angus 5.40 made var
    ): DWORD; stdcall;

RasSetSubEntryPropertiesA: Function (    // ANGUS     - NT4 only
    lpszPhoneBook,
    lpszEntry: PAnsiChar;
    dwSubEntry: DWORD;
    lpRasSubEntry: PAnsiChar;             // Angus 4.11
    dwcb: DWORD;
    lpbDeviceInfo: PAnsiChar;             // Angus 4.11
    dwDeviceInfoSize: DWORD
    ): DWORD; stdcall;

RasGetAutodialAddressA: Function (   // ANGUS     - NT4 only
    lpszAddress: PAnsiChar;
    lpdwReserved: Pointer;
    lpAutoDialEntries: LPRasAutoDialEntryA;
    var lpdwcbAutoDialEntries: DWORD;
    var lpdwcAutoDialEntries: DWORD
    ): DWORD; stdcall;

RasSetAutodialAddressA: Function (   // ANGUS     - NT4 only
    lpszAddress: PAnsiChar;
    dwReserved: DWORD;
    lpAutoDialEntries: LPRasAutoDialEntryA;
    dwcbAutoDialEntries: DWORD;
    dwcAutoDialEntries: DWORD
    ): DWORD; stdcall;

RasEnumAutodialAddressesA: Function (    // ANGUS     - NT4 only
    lppAddresses: Pointer;
    var lpdwcbAddresses: DWORD;
    var lpdwAddresses: DWORD
    ): DWORD; stdcall;

RasGetAutodialEnableA: Function (    // ANGUS     - NT4 only
    dwDialingLocation: DWORD;
    var lpfEnabled: LongBool
    ): DWORD; stdcall;

RasSetAutodialEnableA: Function (    // ANGUS     - NT4 only
    dwDialingLocation: DWORD;
    fEnabled: LongBool
    ): DWORD; stdcall;

RasGetAutodialParamA: Function (     // ANGUS     - NT4 only
    dwKey: DWORD;
    lpvValue: Pointer;
    var lpdwcbValue: DWORD
    ): DWORD; stdcall;

RasSetAutodialParamA: Function (     // ANGUS     - NT4 only
    dwKey: DWORD;
    lpvValue: Pointer;
    dwcbValue: DWORD
    ): DWORD; stdcall;

RasPhonebookDlgA: function (        // Angus 5.20 - NT4 and later
    lpszPhonebook: PAnsiChar;
    lpszEntry: PAnsiChar;
    var lpInfo: TRasPbdDlgA): Bool stdcall;

RasEntryDlgA: function (             // Angus 5.20 - NT4 and later
    lpszPhonebook: PAnsiChar;
    lpszEntry: PAnsiChar;
    var lpInfo: TRasEntryDlgA): Bool stdcall;

RasDialDlgA: function (               // Angus 5.20 - NT4 and later
    lpszPhonebook: PAnsiChar;
    lpszEntry: PAnsiChar;
    lpszPhoneNumber: PAnsiChar;
    var lpInfo: TRasDialDlg): Bool stdcall ;

// note: skipped RasMonitorDlg since gone with XP and later

// following functions are Windows 2000/NT5 only

RasGetConnectionStatistics: Function (      // ANGUS    - W2K only
    rasconn: HRasConn;  // 20 July 2011, was hrasconn which was duplicate
    lpStatistics: PTRas_Stats  // buffer to receive statistics
    ): DWORD; stdcall;

RasGetLinkStatistics: Function (            // ANGUS    - W2K only
    rasconn: HRasConn;  // 20 July 2011, was hrasconn which was duplicate
    dwSubEntry: DWORD ;
    lpStatistics: PTRas_Stats  // buffer to receive statistics
    ): DWORD; stdcall;

RasClearConnectionStatistics: Function (    // ANGUS    - W2K only
    rasconn: HRasConn  // 20 July 2011, was hrasconn which was duplicate
    ): DWORD; stdcall;

RasClearLinkStatistics: Function (          // ANGUS    - W2K only
    rasconn: HRasConn;  // 20 July 2011, was hrasconn which was duplicate
    dwSubEntry: DWORD
    ): DWORD; stdcall;

RasDeleteSubEntryA: function (               // Angus  - W2K SP2 and XP only
    lpszPhoneBook,
    lpszEntry: PAnsiChar;
    dwSubEntryId: DWORD
    ): DWORD; stdcall;

// ------------------------------------------------------------------------

// following stuff is part of TMagRas


const
  RASAPI_DLL = 'RASAPI32.DLL';   // Angus - WinNT needs file extension
  RNAPH_DLL  = 'RNAPH.DLL';      // Angus - functions are in rasapi32 from Win95 DUN 1.2 and later
  RASDLG_DLL = 'RASDLG.DLL';      // Angus 5.20 - NT4 and later

{ type moved to magsubs1 4.80
  TOSVersion = (OSW9x, OSNT4, OSW2K, OSWXP) ;    // Angus 4.0, getting messy to check
}

const
  MaxConnections = 8 ;         // number of connections monitored
//  MaxPhonebooks = 100 ;        // number of phonebooks in list, no limit in 4.90
//  MaxDevices = 30 ;            // number of modems, no limit in 4.91
  MaxCountries = 260 ;         // number of countries
  MaxSubEntries = 4 ;          // number of subentries
  MaxPhoneBkFiles = 2 ;        // total rasphone.pbk files handled
  W2KRasphone = '\Microsoft\Network\Connections\Pbk\rasphone.pbk' ;    // Angus 4.60
  NT4Rasphone = '\System32\ras\rasphone.pbk' ;                         // Angus 4.60

var
  MagRasLib: THandle = 0 ;
  MagRasxLib: THandle = 0 ;
  MagRasDlgLib: THandle = 0 ;   // Angus 5.20
  MagRasAPI_Loaded: Boolean = false ;   // See if DLL functions are loaded
  MagRasExtn_Flag: Boolean = false ;    // See if extensions are available
//  MagRasOSVersion: TOSVersion ;         // simplified OS version - Angus 3.2

{ List of the rasphone.pbk files in various directories, NT4/W2K only }
  MagRasPhoneFiles: array [0..MaxPhoneBkFiles-1] of string ;     // Angus 4.60

{ List of the ages of each MagRasPhoneFiles, NT4/W2K only }
  MagRasPhoneAges: array [0..MaxPhoneBkFiles-1] of integer ;     // Angus 4.60

{ Literals for display of Phonebook Location (PBLocation) in TEntryRec. }
  MasRasPBLocationStr: array [0..MaxPhoneBkFiles-1] of
                         string = (SRasRENUser, SRasRENAllUsers) ;  // 5.20

{ Loaded by MagRasGetPhoneBookFiles, content of the one or more
  rasphone.pbk file, NT4/W2K only }
  MagRasPhoneBookInf: TStringList ;

// make these functions public

{ Try and load various RAS DLL functions, depending on OS and RAS versions.
  Also sets MagRasPhoneFiles.

  Returns false if failed to load RAS, in which case RAS functions
  must not be called. }
  function MagLoadRasApi: boolean ;

{ Sets the default Phonebook file names.  This is called automatically
  by MagLoadRasApi or when a phonebook file is set. }
  procedure MagSetPhoneBkFiles ;

{ Unload the RAS DLLs, and clear internal variables.  This is called
  automatically during unit finalisation.  }

  procedure MagUnloadRasApi ;

implementation

// Try and load various RAS DLL functions. Returns false if failed.


function MagLoadRasApi: boolean ;
begin
    if MagRasAPI_Loaded then
    begin
        result := Assigned (RasDialA) ;
        exit ;
    end ;

// open libraries - only come here once
    result := false ;
    RasDialA := Nil;
    MagRasAPI_Loaded := True ;
    MagRasExtn_Flag := false ;
    MagRasLib := LoadLibrary (RASAPI_DLL) ;
    If MagRasLib = 0 then exit ;

// set function addresses to ASCII versions in DLL
    RasDialA := GetProcAddress(MagRasLib, 'RasDialA') ;
    RasEnumConnectionsA := GetProcAddress(MagRasLib, 'RasEnumConnectionsA');
    RasEnumEntriesA := GetProcAddress(MagRasLib, 'RasEnumEntriesA');
    RasGetConnectStatusA := GetProcAddress(MagRasLib, 'RasGetConnectStatusA');
    RasGetErrorStringA := GetProcAddress(MagRasLib, 'RasGetErrorStringA');
    RasHangUpA := GetProcAddress(MagRasLib, 'RasHangUpA');
    RasGetEntryDialParamsA := GetProcAddress(MagRasLib, 'RasGetEntryDialParamsA');
    RasSetEntryDialParamsA := GetProcAddress(MagRasLib, 'RasSetEntryDialParamsA');
    RasMonitorDlgA := GetProcAddress(MagRasLib, 'RasMonitorDlgA');
    RasEditPhonebookEntryA := GetProcAddress(MagRasLib, 'RasEditPhonebookEntryA');
    RasCreatePhonebookEntryA := GetProcAddress(MagRasLib, 'RasCreatePhonebookEntryA');
    RasGetProjectionInfoA := GetProcAddress(MagRasLib, 'RasGetProjectionInfoA');

// Windows 98 stuff
    if (MagRasOSVersion >= OSNT4) or
               ((MagRasOSVersion = OSW9x) and (Win32MinorVersion >= 10)) then
    begin
        RasConnectionNotificationA := GetProcAddress(MagRasLib, 'RasConnectionNotificationA');
    end ;

// Windows NT4 stuff, and later
    if MagRasOSVersion >= OSNT4 then
    begin
        RasGetSubEntryHandleA := GetProcAddress(MagRasLib, 'RasGetSubEntryHandleA');
        RasGetSubEntryPropertiesA := GetProcAddress(MagRasLib, 'RasGetSubEntryPropertiesA');
        RasSetSubEntryPropertiesA := GetProcAddress(MagRasLib, 'RasSetSubEntryPropertiesA');
        RasGetAutodialAddressA := GetProcAddress(MagRasLib, 'RasGetAutodialAddressA');
        RasSetAutodialAddressA := GetProcAddress(MagRasLib, 'RasSetAutodialAddressA');
        RasEnumAutodialAddressesA := GetProcAddress(MagRasLib, 'RasEnumAutodialAddressesA');
        RasGetAutodialEnableA := GetProcAddress(MagRasLib, 'RasGetAutodialEnableA');
        RasSetAutodialEnableA := GetProcAddress(MagRasLib, 'RasSetAutodialEnableA');
        RasGetAutodialParamA := GetProcAddress(MagRasLib, 'RasGetAutodialParamA');
        RasSetAutodialParamA := GetProcAddress(MagRasLib, 'RasSetAutodialParamA');
        RasGetCredentialsA := GetProcAddress(MagRasLib, 'RasGetCredentialsA');   // 5.10
        RasSetCredentialsA := GetProcAddress(MagRasLib, 'RasSetCredentialsA');   // 5.10

// Windows 2000 (NT5.0) stuff, and later
        if MagRasOSVersion >= OSW2K then
        begin
            RasGetConnectionStatistics := GetProcAddress(MagRasLib, 'RasGetConnectionStatistics');
            RasClearConnectionStatistics := GetProcAddress(MagRasLib, 'RasClearConnectionStatistics');
            RasGetLinkStatistics := GetProcAddress(MagRasLib, 'RasGetLinkStatistics');
            RasClearLinkStatistics := GetProcAddress(MagRasLib, 'RasClearLinkStatistics');
        end ;

// Windows XP (NT5.1) stuff, and later
        if MagRasOSVersion >= OSWXP then
        begin
            RasDeleteSubEntryA := GetProcAddress(MagRasLib, 'RasDeleteSubEntryA');
        end ;
    end ;

// now get API extensions that may be in rasapi32.dll or rnaph.dll
    MagRasxLib := MagRasLib ;
    RasGetCountryInfoA := GetProcAddress(MagRasxLib, 'RasGetCountryInfoA');
    if Assigned (RasGetCountryInfoA) then
        MagRasExtn_Flag := true
    else
    begin
        MagRasxLib := LoadLibrary (RNAPH_DLL) ;
        If MagRasxLib <> 0 then MagRasExtn_Flag := true ;
    end ;
    if MagRasExtn_Flag then
    begin
        RasGetCountryInfoA := GetProcAddress(MagRasxLib, 'RasGetCountryInfoA');
        RasGetEntryPropertiesA := GetProcAddress(MagRasxLib, 'RasGetEntryPropertiesA');
        RasSetEntryPropertiesA := GetProcAddress(MagRasxLib, 'RasSetEntryPropertiesA');
        RasRenameEntryA := GetProcAddress(MagRasxLib, 'RasRenameEntryA');
        RasDeleteEntryA := GetProcAddress(MagRasxLib, 'RasDeleteEntryA');
        RasValidateEntryNameA := GetProcAddress(MagRasxLib, 'RasValidateEntryNameA');
        RasEnumDevicesA := GetProcAddress(MagRasxLib, 'RasEnumDevicesA');
    end ;
    result := Assigned (RasDialA) ;

// rasdlg.dll dialogs
    if MagRasOSVersion >= OSNT4 then
    begin
        MagRasDlgLib := LoadLibrary (RASDLG_DLL) ;
        if MagRasDlgLib <> 0 then
        begin
            RasEntryDlgA := GetProcAddress(MagRasDlgLib, 'RasEntryDlgA');               // 5.20
            RasPhonebookDlgA := GetProcAddress(MagRasDlgLib, 'RasPhonebookDlgA');       // 5.20
            RasDialDlgA := GetProcAddress(MagRasDlgLib, 'RasDialDlgA');                 // 5.20
        end ;
    end ;
    MagSetPhoneBkFiles ;  // 5.21
    MagRasPhoneBookInf := TStringList.Create ;
end ;

// default phone book locations - may be any file with pbk extension
// W9x = registry, encoded!
// NT4 = c:\winnt\system32\ras\rasphone.pbk
//     or %System root%\System32\ras\rasphone.pbk

// All Users - REN_AllUsers
// W2K = c:\Documents and Settings\All Users\Application Data\Microsoft\Network\Connections\Pbk\rasphone.pbk
//     or (CSIDL_COMMON_APPDATA)\Microsoft\Network\Connections\Pbk\rasphone.pbk
// XP - same as W2K
// Vista = c:\ProgramData\Microsoft\Network\Connections\Pbk\rasphone.pbk
//     or (CSIDL_COMMON_APPDATA)\Microsoft\Network\Connections\Pbk\rasphone.pbk

// Current User - REN_User
// W2K = c:\Documents and Settings\(user)\Application Data\Microsoft\Network\Connections\Pbk\rasphone.pbk
//     or (CSIDL_LOCAL_APPDATA)\Microsoft\Network\Connections\Pbk\rasphone.pbk
// XP - same as W2K
// Vista = c:\Users\(user)\AppData\Roaming\Microsoft\Network\Connections\Pbk\rasphone.pbk
//     or (CSIDL_LOCAL_APPDATA)\Microsoft\Network\Connections\Pbk\rasphone.pbk

procedure MagSetPhoneBkFiles ;     // 5.21
var
    I: integer ;
begin
    for I := 0 to (MaxPhoneBkFiles - 1) do
    begin
        MagRasPhoneFiles [I] := '' ;
        MagRasPhoneAges [I] := -1 ;
    end ;
    if MagRasOSVersion = OSNT4 then
        MagRasPhoneFiles [0] := GetWinDir + NT4Rasphone
    else if MagRasOSVersion >= OSW2K then  // 5.10, allow for future windows versions
    begin
        MagRasPhoneFiles [REN_User] := GetShellPath (CSIDL_APPDATA) + W2KRasphone ;
        MagRasPhoneFiles [REN_AllUsers] := GetShellPath (CSIDL_COMMON_APPDATA) + W2KRasphone ;
    end ;
end ;

procedure MagUnloadRasApi ;
begin
    if (MagRasxLib <> MagRasLib) and (MagRasxLib <> 0) then
                                                    FreeLibrary (MagRasxLib) ;
    if MagRasDlgLib <> 0 then FreeLibrary (MagRasDlgLib) ;
    if MagRasAPI_Loaded then
    begin
        FreeLibrary (MagRasLib) ;
        MagRasPhoneBookInf.Free ;
    end ;
    RasDialA := Nil;
    MagRasxLib := 0 ;
    MagRasLib := 0 ;
    MagRasDlgLib := 0 ;
    MagRasAPI_Loaded := false ;
end ;

initialization
    MagRasxLib := 0 ;
    MagRasLib := 0 ;
    MagRasDlgLib := 0 ;
    MagRasAPI_Loaded := false ;
finalization
    MagUnloadRasApi ;
end.


