unit MagDBT ;

// Magenta Systmes  1st March 2000

interface
	uses Windows, Messages, SysUtils, ComObj ;

const
{****************************************************************************
 *
 *  (C) Copyright 1993 - 1998 Microsoft Corporation
 *
 *  Title:      DBT.H - Equates for WM_DEVICECHANGE and BroadcastSystemMessage
 *
 *  Version:    4.00
 *
 *  Date:       24-May-1993
 *
 *  Author:     rjc
 *
 *----------------------------------------------------------------------------
 *
 *  Change log:
 *
 *     DATE     REV                 DESCRIPTION
 *  ----------- --- ----------------------------------------------------------
 *
 ****************************************************************************}


{ * BroadcastSpecialMessage constants. }
// #define WM_DEVICECHANGE         0x0219


{#ifndef GUID_DEFINED
    // #define GUID_DEFINED
    typedef struct _GUID {
        ULONG   Data1;
        unsigned short Data2;
        unsigned short Data3;
        unsigned char Data4[8];
     GUID;
#endif // !defined(GUID_DEFINED)

{
 * Broadcast message and receipient flags.
 *
 * Note that there is a third "flag". If the wParam has:
 *
 * bit 15 on:   lparam is a pointer and bit 14 is meaningfull.
 * bit 15 off:  lparam is just a UNLONG data type.
 *
 * bit 14 on:   lparam is a pointer to an ASCIIZ string.
 * bit 14 off:  lparam is a pointer to a binary struture starting with
 *              a dword describing the length of the structure.
 }

BSF_QUERY               = $00000001 ;
BSF_IGNORECURRENTTASK   = $00000002 ;     { Meaningless for VxDs }
BSF_FLUSHDISK           = $00000004 ;     { Shouldn't be used by VxDs }
BSF_NOHANG              = $00000008 ;
BSF_POSTMESSAGE         = $00000010 ;
BSF_FORCEIFHUNG         = $00000020 ;
BSF_NOTIMEOUTIFNOTHUNG  = $00000040 ;
BSF_MSGSRV32ISOK        = $80000000 ;     { Called synchronously from PM API }
BSF_MSGSRV32ISOK_BIT    = 31 ;             { Called synchronously from PM API }
BSM_ALLCOMPONENTS       = $00000000 ;
BSM_VXDS                = $00000001 ;
BSM_NETDRIVER           = $00000002 ;
BSM_INSTALLABLEDRIVERS  = $00000004 ;
BSM_APPLICATIONS        = $00000008 ;

{
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_APPYBEGIN
 * lParam  = (not used)
 *
 *      'Appy-time is now available.  This message is itself sent
 *      at 'Appy-time.
 *
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_APPYEND
 * lParam  = (not used)
 *
 *      'Appy-time is no longer available.  This message is *NOT* sent
 *      at 'Appy-time.  (It cannot be, because 'Appy-time is gone.)
 *
 * NOTE!  It is possible for DBT_APPYBEGIN and DBT_APPYEND to be sent
 * multiple times during a single Windows session.  Each appearance of
 * 'Appy-time is bracketed by these two messages, but 'Appy-time may
 * momentarily become unavailable during otherwise normal Windows
 * processing.  The current status of 'Appy-time availability can always
 * be obtained from a call to _SHELL_QueryAppyTimeAvailable.
 }
DBT_APPYBEGIN                   = $0000 ;
DBT_APPYEND                     = $0001 ;

{
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_DEVNODES_CHANGED
 * lParam  = 0
 *
 *      send when configmg finished a process tree batch. Some devnodes
 *      may have been added or removed. This is used by ring3 people which
 *      need to be refreshed whenever any devnode changed occur (like
 *      device manager). People specific to certain devices should use
 *      DBT_DEVICE* instead.
 }

DBT_DEVNODES_CHANGED            = $0007 ;

{
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_QUERYCHANGECONFIG
 * lParam  = 0
 *
 *      sent to ask if a config change is allowed
 }

DBT_QUERYCHANGECONFIG           = $0017 ;

{
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_CONFIGCHANGED
 * lParam  = 0
 *
 *      sent when a config has changed
 }

DBT_CONFIGCHANGED               = $0018 ;

{
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_CONFIGCHANGECANCELED
 * lParam  = 0
 *
 *      someone cancelled the config change
 }

DBT_CONFIGCHANGECANCELED        = $0019 ;

{
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_MONITORCHANGE
 * lParam  = new resolution to use (LOWORD=x, HIWORD=y)
 *           if 0, use the default res for current config
 *
 *      this message is sent when the display monitor has changed
 *      and the system should change the display mode to match it.
 }

DBT_MONITORCHANGE               = $001B ;

{
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_SHELLLOGGEDON
 * lParam  = 0
 *
 *      The shell has finished login on: VxD can now do Shell_EXEC.
 }

DBT_SHELLLOGGEDON               = $0020 ;

{
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_CONFIGMGAPI
 * lParam  = CONFIGMG API Packet
 *
 *      CONFIGMG ring 3 call.
 }
DBT_CONFIGMGAPI32               = $0022 ;

{
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_VXDINITCOMPLETE
 * lParam  = 0
 *
 *      CONFIGMG ring 3 call.
 }
DBT_VXDINITCOMPLETE             = $0023 ;

{
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_VOLLOCK*
 * lParam  = pointer to VolLockBroadcast structure described below
 *
 *      Messages issued by IFSMGR for volume locking purposes on WM_DEVICECHANGE.
 *      All these messages pass a pointer to a struct which has no pointers.
 }

DBT_VOLLOCKQUERYLOCK    = $8041 ;
DBT_VOLLOCKLOCKTAKEN    = $8042 ;
DBT_VOLLOCKLOCKFAILED   = $8043 ;
DBT_VOLLOCKQUERYUNLOCK  = $8044 ;
DBT_VOLLOCKLOCKRELEASED = $8045 ;
DBT_VOLLOCKUNLOCKFAILED = $8046 ;

// Device broadcast header

type

PDEV_BROADCAST_HDR = ^DEV_BROADCAST_HDR ;
DEV_BROADCAST_HDR = record
    dbch_size: DWORD  ;
    dbch_devicetype: DWORD ;
    dbch_reserved: DWORD ;
end ;

// Structure for volume lock broadcast
{
VolLockBroadcast = ^pVolLockBroadcast ;
VolLockBroadcast = record
        vlb_dbh: DEV_BROADCAST_HDR ;
        vlb_owner: DWORD ;              // thread on which lock request is being issued
        vlb_perms: BYTE ;              // lock permission flags defined below
        vlb_lockType: BYTE ;           // type of lock
        vlb_drive: BYTE ;              // drive on which lock is issued
        vlb_flags: BYTE ;              // miscellaneous flags
end ;  }

const
{
 * Values for vlb_perms
 }
LOCKP_ALLOW_WRITES              = $01 ;   // Bit 0 set - allow writes
LOCKP_FAIL_WRITES               = $00 ;   // Bit 0 clear - fail writes
LOCKP_FAIL_MEM_MAPPING          = $02 ;   // Bit 1 set - fail memory mappings
LOCKP_ALLOW_MEM_MAPPING         = $00 ;   // Bit 1 clear - allow memory mappings
LOCKP_USER_MASK                 = $03 ;   // Mask for user lock flags
LOCKP_LOCK_FOR_FORMAT           = $04 ;   // Level 0 lock for format

{
 * Values for vlb_flags
 }
LOCKF_LOGICAL_LOCK              = $00 ;   // Bit 0 clear - logical lock
LOCKF_PHYSICAL_LOCK             = $01 ;   // Bit 0 set - physical lock

{
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_NODISKSPACE
 * lParam  = drive number of drive that is out of disk space (1-based)
 *
 * Message issued by IFS manager when it detects that a drive is run out of
 * free space.
 }

DBT_NO_DISK_SPACE               = $0047 ;

{
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_LOW_DISK_SPACE
 * lParam  = drive number of drive that is low on disk space (1-based)
 *
 * Message issued by VFAT when it detects that a drive it has mounted
 * has the remaning free space below a threshold specified by the
 * registry or by a disk space management application.
 * The broadcast is issued by VFAT ONLY when space is either allocated
 * or freed by VFAT.
 }

DBT_LOW_DISK_SPACE  		    = $0048 ;

DBT_CONFIGMGPRIVATE             = $7FFF ;

{
 * The following messages are for WM_DEVICECHANGE. The immediate list
 * is for the wParam. ALL THESE MESSAGES PASS A POINTER TO A STRUCT
 * STARTING WITH A DWORD SIZE AND HAVING NO POINTER IN THE STRUCT.
 *
 }
DBT_DEVICEARRIVAL               = $8000 ; // system detected a new device
DBT_DEVICEQUERYREMOVE           = $8001 ; // wants to remove, may fail
DBT_DEVICEQUERYREMOVEFAILED     = $8002 ; // removal aborted
DBT_DEVICEREMOVEPENDING         = $8003 ; // about to remove, still avail.
DBT_DEVICEREMOVECOMPLETE        = $8004 ; // device is gone
DBT_DEVICETYPESPECIFIC          = $8005 ; // type specific event
DBT_CUSTOMEVENT                 = $8006 ; // user-defined event

DBT_DEVTYP_OEM                  = $00000000 ; // oem-defined device type
DBT_DEVTYP_DEVNODE              = $00000001 ; // devnode number
DBT_DEVTYP_VOLUME               = $00000002 ; // logical volume
DBT_DEVTYP_PORT                 = $00000003 ; // serial, parallel
DBT_DEVTYP_NET                  = $00000004 ; // network resource - not used
DBT_DEVTYP_DEVICEINTERFACE      = $00000005 ; // device interface class
DBT_DEVTYP_HANDLE               = $00000006 ; // file system handle

//struct _DEV_BROADCAST_HEADER { { }
//    DWORD       dbcd_size;
//    DWORD       dbcd_devicetype;
//    DWORD       dbcd_reserved;
//; }

type

PDEV_BROADCAST_OEM = ^DEV_BROADCAST_OEM ;
DEV_BROADCAST_OEM = record
    dbch_size: DWORD  ;
    dbch_devicetype: DWORD ;
    dbch_reserved: DWORD ;
    dbco_identifier: DWORD ;
    dbco_suppfunc: DWORD ;
end ;

PDEV_BROADCAST_DEVNODE = ^DEV_BROADCAST_DEVNODE ;
DEV_BROADCAST_DEVNODE = record
    dbch_size: DWORD  ;
    dbch_devicetype: DWORD ;
    dbch_reserved: DWORD ;
	dbcd_devnode: DWORD ;
end ;

PDEV_BROADCAST_VOLUME = ^DEV_BROADCAST_VOLUME ;
DEV_BROADCAST_VOLUME = record
    dbch_size: DWORD  ;
    dbch_devicetype: DWORD ;
    dbch_reserved: DWORD ;
    dbcv_unitmask: DWORD ;
    dbcv_flags: WORD ;
end ;

const

DBTF_MEDIA      = $0001 ;         // media comings and goings
DBTF_NET        = $0002 ;         // network volume

type

PDEV_BROADCAST_PORT_A = ^DEV_BROADCAST_PORT_A ;
DEV_BROADCAST_PORT_A = record
    dbch_size: DWORD  ;
    dbch_devicetype: DWORD ;
    dbch_reserved: DWORD ;
    dbcp_name: array [0..127] of ansichar ;
end ;

PDEV_BROADCAST_PORT_W = ^DEV_BROADCAST_PORT_W ;
DEV_BROADCAST_PORT_W = record
    dbch_size: DWORD  ;
    dbch_devicetype: DWORD ;
    dbch_reserved: DWORD ;
    dbcp_name: array [0..127] of wchar ;
end ;

//#ifdef UNICODE
//typedef DEV_BROADCAST_PORT_W     DEV_BROADCAST_PORT;
//typedef PDEV_BROADCAST_PORT_W    PDEV_BROADCAST_PORT;
//#else
//typedef DEV_BROADCAST_PORT_A     DEV_BROADCAST_PORT;
//typedef PDEV_BROADCAST_PORT_A    PDEV_BROADCAST_PORT;
//#endif

// not used
PDEV_BROADCAST_NET = ^DEV_BROADCAST_NET ;
DEV_BROADCAST_NET = record
    dbch_size: DWORD  ;
    dbch_devicetype: DWORD ;
    dbch_reserved: DWORD ;
    dbcn_resource: DWORD ;
    dbcn_flags: DWORD ;
end ;

PDEV_BROADCAST_DEVICEINTERFACE_A = ^DEV_BROADCAST_DEVICEINTERFACE_A ;
DEV_BROADCAST_DEVICEINTERFACE_A = record
    dbch_size: DWORD  ;
    dbch_devicetype: DWORD ;
    dbch_reserved: DWORD ;
    dbcc_classguid: TGUID ;
    dbcc_name: array [0..127] of ansichar ;
end ;

PDEV_BROADCAST_DEVICEINTERFACE_W = ^DEV_BROADCAST_DEVICEINTERFACE_W ;
DEV_BROADCAST_DEVICEINTERFACE_W = record
    dbch_size: DWORD  ;
    dbch_devicetype: DWORD ;
    dbch_reserved: DWORD ;
    dbcc_classguid: TGUID ;
    dbcc_name: array [0..127] of wchar ;
end ;

//#ifdef UNICODE
//typedef DEV_BROADCAST_DEVICEINTERFACE_W   DEV_BROADCAST_DEVICEINTERFACE;
//typedef PDEV_BROADCAST_DEVICEINTERFACE_W  PDEV_BROADCAST_DEVICEINTERFACE;
//#else
//typedef DEV_BROADCAST_DEVICEINTERFACE_A   DEV_BROADCAST_DEVICEINTERFACE;
//typedef PDEV_BROADCAST_DEVICEINTERFACE_A  PDEV_BROADCAST_DEVICEINTERFACE;
//#endif

PDEV_BROADCAST_HANDLE = ^DEV_BROADCAST_HANDLE ;
DEV_BROADCAST_HANDLE = record
    dbch_size: DWORD  ;
    dbch_devicetype: DWORD ;
    dbch_reserved: DWORD ;
    dbch_handle: THANDLE ;     // file handle used in call to RegisterDeviceNotification
    dbch_hdevnotify: DWORD ; // HDEVNOTIFY returned from RegisterDeviceNotification
    //
    // The following 3 fields are only valid if wParam is DBT_CUSTOMEVENT.
    //
    dbch_eventguid: TGUID ;
    dbch_nameoffset: LONGWORD ; // offset (bytes) of variable-length string buffer (-1 if none)
    dbch_data: array [0..127] of BYTE ;    // variable-sized buffer, potentially containing binary and/or text data
end ;

const

DBTF_RESOURCE   = $00000001 ;     // network resource
DBTF_XPORT      = $00000002 ;     // new transport coming or going
DBTF_SLOWNET    = $00000004 ;     // new incoming transport is slow
                                        // (dbcn_resource undefined for now)

DBT_VPOWERDAPI  = $8100 ;         // VPOWERD API for Win95

{
 *  User-defined message types all use wParam = 0xFFFF with the
 *  lParam a pointer to the structure below.
 *
 *  dbud_dbh - DEV_BROADCAST_HEADER must be filled in as usual.
 *
 *  dbud_szName contains a case-sensitive ASCIIZ name which names the
 *  message.  The message name consists of the vendor name, a backslash,
 *  then arbitrary user-defined ASCIIZ text.  For example:
 *
 *      "WidgetWare\QueryScannerShutdown"
 *      "WidgetWare\Video Q39S\AdapterReady"
 *
 *  After the ASCIIZ name, arbitrary information may be provided.
 *  Make sure that dbud_dbh.dbch_size is big enough to encompass
 *  all the data.  And remember that nothing in the structure may
 *  contain pointers.
 }

DBT_USERDEFINED  = $FFFF ;

type

DEV_BROADCAST_USERDEFINED = record
    dbch_size: DWORD  ;
    dbch_devicetype: DWORD ;
    dbch_reserved: DWORD ;
    dbud_szName: array [0..127] of ansichar ;     { ASCIIZ name }
{  BYTE        dbud_rgbUserDefined[];} { User-defined contents }
end ;

implementation

end.

