unit magrascon;
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

// --------------------------------------------------
{
DELPHI RAS COMPONENT - Connections - Part 1 of 3
(C) 2014 Magenta Systems Ltd

Updated by Angus Robertson, Magenta Systems Ltd, England
in 2013, delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

Compatible with Delphi 7, 2007, 2009, 2010, XE, XE2, XE3, XE4,
XE5, XE6, XE7,
supports Windows XP, 2003, Vista, 2008, 7, 8, 2012, 10 and 2015
32-bit and 64-bit editions. Win32 and Win64 binaries only.

TMagRas is a set of installable Delphi non-visual components,
supplied with a demo program, for accessing Dial Up Networking
or Remote Access Services functions.  This is a major update
of Daniel Polistchuck's and Mike Armstrong's earlier TRAS component
(little of which remains).  It allows Delphi developers to add full
RAS functionality to their applications, including dialling and
monitoring multiple connections, creating and editing phonebooks
(without using Windows dialogs), and getting performance information for
connections.  TMagRas supports Windows XP and later.

TMagRas is supplied as three separate Delphi components, TMagRasCon
which includes dialling and monitoring and limited connection editing,
TMagRasPer with performance statistics, and TMagRasEdt to create and
edit connections/phonebooks.  A demo program illustrates use of the
components, including monitoring multiple connections, creating a
simple new connection with minimal properties, and editing detailed
connection properties (seven tabs of settings) including Windows 2000
extensions.  Seven example programs illustrate various aspects of the
components and may be copied to quickly add specific RAS functionality
to applications.

TMagRas is copyrighted software, but the compiled component DCUs may be
used without cost in applications for use by the developer or within
their company but which are not otherwise distributed (including
freeware and shareware).  The component source code may be licensed for
a cost of £100 (UKP) plus tax).  Developers that license the source
may distribute applications using TMagRas.

Since TMagRasCon does need design time properties, it need not really be
installed on the component palette, but may be created in the application
as needed, as shown in the demo program.


Windows 2000 and Later Warning
------------------------------

Microsoft has made some subtle changes to the RAS APIs for Windows 2000, the
most annoying being that GetDialParams no longer returns the connection
password but what Microsoft call a 'handle' that is actually 16 asterisks.
Passing this 'handle' back to SetDialParams will cause the existing password
to be unchanged, and to ConnectEx/AutoConnectEx causes the existing password to
be used.  So if your applications checks that what is saved is also returned,
it will fail under W2K and later.

}
(*------------------------------------------------------------------------

Updated in 1998 by Angus Robertson, Magenta Systems Ltd, England

Added various functions:
IntDisconnect       - disconnect without error handling
AutoConnect         - GetDialParams, then Connect
LeaveOpen           - stops disconnection when component destroyed
ReOpen              - access an existing connection
GetDialParams       - read phone book logon and password
SetDialParams       - write phone book logon and password
MessText            - gets description for CurrentState
TestRasCon          - see if RAS available
EditPhonebook       - displays a dialog to edit a connection
CreatePhonebook     - displays a dialog to create a connection
DeletePhonebook     - deletes a connection
CreatePhonebook     - renames a connection
StateChanged        - event called whenever RAS status changes
ResetPerfStats      - resets the performance counters
EnablePerfStats     - enable performance counters
GetPerfStats        - get current performance counters
GetEntryProperties  - read connection properties (dial number etc), note
                        that not all are processed yet)
GetConnection       - check for existing connection and get name

Other minor changes include:

Improved error handling
StateChanged event handles all progress messages
Get RAS username and password, access phone books
Only call functions if DLL is loaded so application will run without RAS installed
When disconnecting, wait until it happens
Added more progress messages
Error during connection now reported properly
Improved connection progress literals
Corrected a major memory leak in TConnectionList.Clear

Changes in 2.6
Win95/98 performance stats needs registry settings that may be translated
or change, so added search option to EnablePerfStats which will set the
first Dial Up Adaptor found listed

Changes in 2.7
Ensure ConnectState cleared during disconnection so application does not
   think we are still on-line
Increased maximum number of phone book entries from 20 to 100

Changes in 2.8
Supported multi-link connections, using RASSUBENTRY - NT4 only
Changed compiler directive so that programs compiled under NT4 will
  only use the Win9x structures for backward compatibility

Changes in 2.9
Added PhoneCanonical property which is completed from Phonebook with
  full canonical number that may be passed to TAPI lineTranslateAddress to
  create a diallable number for Connect
Clear DialParams if request fails
Ensure MagRasAPI_Loaded cleared during destroy, so RAS can be used again
Close both libraries

Changes in 3.0
Added GetDeviceList to fill DeviceTypeList and DeviceNameList with RAS TAPI devices
Added DevicePort property, NT only for phonebooks
Corrected canonical number where area code is blank

Changes in 3.1
Now using callback procedure for dialling events rather than windows messages
   (avoids creating a windows handle)
RASEntry is now public to allow access from application
Under NT, RASEntryNT has more information (overlayed on RASEntry)
Added CountryID property
Added PutEntryProperties to save RASEntry (connection)
Added CopyPhonebook to create new connection from an old one by copying
Added UpdatePhonebook to update the Canonical phone number number in a connection

Changes in 3.2
Added AutoDialDLL and AutoDialFunc - RAS calls the specified DLL function to dial
Added RasGetConnectionStatisitcs, Ras_Stats, RasClearConnectionStatistics (W2K only)

Changes in 4.0
Split the original component into two parts and added a third.
TMagRasCon includes all connection related stuff, lists, dialling, status
TMagRasPer includes performance statistics
TMagRasEdt is new to edit phonebooks (includes country list)

Added Ex versions of all main functions using a specified handle rather than
  the internal one, to allow proper support for multiple connections
All functions now use NT4 and W2K extensions dynamically, so the same compiled
application will run on all three platforms ignoring stuff not supported
Added Subhandle handling so identify multilink ISDN connections separately
Added some minimal TAPI functions to translate addresses, but note there is
  no TAPI modem list so dialling preferences always come from the first modem.
  The separate Magenta TAPI component does all this properly

Changes in 4.10
Added DefCountryId propertry
No longer store version on form
Made all stringlists available as published properties
TotSubEntries set as 2 for Win9x ISDN multilink, but no modem/dial details
Changed TConnectionList.Clear to ClearFree to avoid overwrite problem in D3

Changes in 4.20
Don't get modem device configuration for subentries, nor NT4/W2K
Don't corrupt Win9x modem device configuration when editing entries
Set default modem device configuration when updating Win9x connections, if it's missing
Corrected canonical phone number on NT4/W2K when area code set but no dialling props

Changes in 4.30
Corrected PhoneNumber being ignored for dialling (error in 4.00 and later)
   (set PhoneNumber to blank before calling Connect to use default number)
Added GetTransCaps which get modem and country dialling properties so we can offer
      better display of canonical numbers (we need to know the local city code)
Added UseCountryAndAreaCodes, PromoteAlternates, EntryOptions (flags)
Added DUNVersion, DUNInfo properties
GetEntryProperties now completes LocalPhoneNumber not PhoneNumber

Changes in 4.40
Recognise Windows ME (sort of)
Conditional compile as TComponent again to allow use as NT service
OneRasConn changed to IntRasConn so it does not look like an event

Changes in 4.41
CreatePhonebook and EditPhonebook now both need Application.Handle passed
as a parameter.  This avoids the Forms unit being included in some apps.
Graphics and Control units now conditional as well to ease linking

Changes in 4.42
For ConnectEx, if fSubEntry left as 0 set to 1 to make NT4/W2K work the same
Added GetEntryProps similar to GetEntryProperties but makes return of sub
entries optional (since this doubles function time).

Changes in 4.50
Literals moved to resource strings for translation in magrasstr.pas
Added onStateEvent (as an alternative to onStateChanged) which buffers
    dial callback state messages to avoid dialling being blocked and
    possible re-entrancy issues (since callback can not be stopped) it
    uses a queue to avoid missing rapidly changing events,  The event returns
    TRasStateRec with all the state properties, rather than reading component
    properties that may have already been changed by subsequent state changes.
    Note that CurDevName, CurDevType and ConnectPhoneNr are only set if
    the event source is status (not dial).
Added ConnectNT similar to ConnectEx but NT4/W2K only, supports dialextensions
    for paused states and speaker on/off.  The handle passed must be zero to
    dial a new call, a non-zero handle will resume dialling a call that has
    been paused for a callback number or authentication. Note that paused states
    have not been tested yet!!
GetConnectStatusEx now updates CurDevName and CurDevType since the W2K final
    now sets these correctly. ConnectPhoneNr is returned by NT4/W2K.  These
    settings are very useful to see if RAS connected using the device specified
    in the phonebook entry (it may choose others) and to see what phone number
    was really dialler (note it's the diallable format usually starting with T
    (for tone dialling) and may include a calling card number, so it not really
    suitable for displaying to users.

Changes in 4.51
Removed ConnectEx/ConnectNT 4.42 fix for fSubEntry not allowed as zero
    so that W2K will dial multilink
Default fSubEntry on creation to 1 so W2K does not dial multilink (unless set to 0)
Recognise Windows XP/Whistler, only basic testing so far, seems OK
Corrected DisconnectEx so the timeout actually works and state events are done

Changes in 4.60
Moved some common functions to MagSubs1 and MagRasEnt
GetPhoneBookEntries now calls MagRasGetEntryList to get the entry list
   which is made available (sorted) in an array MagRasEntryRecs
Finally got around to testing PhoneBookPath, simplified some related code

Changes in 4.62
Added support for Windows XP, tested on RC1 build 2505.  This appears to be
fully backward compatible with Windows 2000 so old applications should
continue working OK on XP.  But XP adds new elements to the RASCONN and
RASENTRY structures, which are TRasConnWXP and TRasEntryWXP.
MagRasOSVersion nows returns OSWXP for Windows XP, if your application uses
this (possibly for performance statistics) it will need to be changed.
TConnectionList has new Flags and LogonSessId properties to support multiple
users being logged onto windows at the same time.

Changes in 4.70
Tested Windows XP final build 2600, updated help.

Changes in 4.71
Fixed potential bug in Create checking RASAPI32 version if the DLL is
not installed

Changes in 4.80
Added TConnectionList.CheckIndex which is used to validate the argument to
all Connections properties, to avoid possible 'listindex out range' errors

Changes in 4.90
Removed 100 maximum number of supported phonebooks/entryrecs, now unlimited

Changes in 4.91
Removed 30 maximum number of devices, now unlimited (needed for Windows .NET Server)
Note that Windows .NET Server displays as WinXP, current beta is 5.1.3406.0

Changes in 4.92
Corrected RasConnectWXP incorrectly declared causing possible problems if two or more
  simultaneous connection on Windows XP
Windows .NET Server now called Windows 2003

Changes in 4.94
Supporting Delphi 2005 Win32, no changes needed

Changes in 5.00
Supporting Delphi 2006 Win32, no changes needed
Poss fix for illegal fsubentry

Changes in 5.10
Supporting Windows Vista (and maybe Longhorn)
PPutDialParams and GetDialParams now use NT4 and later RasGet/SetCredentials
APIs instead of RasGet/SetEntryDialProps which might allow a password to
be saved on Vista

Changes in 5.20
DUN version reports Vista and Windows Server 2008
EditPhonebook and CreatePhonebook use RasEntryDlg on NT4 and better - needed for Vista
New PBLocation property which should be set to REN_AllUsers or REN_User before
  calling most methods, Win2K/XP default to REN_AllUsers, Vista/2008 to REN_User

Changes in 5.21
Ensure phonebook file names are always set before setting PBLocation
Only allow setting PhoneBookPath on NT4 (W2K and later have fixed names)

Changes in 5.30
Added bDefaultCreds property set true before calling PutDialParams or GetDialParams
  to set user name and password for 'anyone who uses this computer'

Changes in 5.40
Made compatible with Delphi 2009, but still using ANSI RAS functions, not Unicode
Many functions deliberately use AnsiString for compatability with PAnsiChar APIs

Changes in 5.50
GetTransCaps now creates a blank 'My Location' if missing to avoid Windows
  'Location Information' dialog appearing, note this needs administrator rights
  to write to HLM registry keys and will silently fail otherwise
DefCountryId set from LOCALE_ICOUNTRY using GetLcTypeInfo
Recognise Windows 7 and 2008 R2, but note that no W7 RAS extensions are
  currently supported in TMagRas and W7 is MagRasOSVersion=OSVista

Changes in 5.60
Fixed cast warnings for Delphi 2009 and later

Changes in 5.70
Fixes for 64-bit compiler, CallbackId no longer published since it's a pointer
Recognise Windows 8
RegisterComponent now in magrasreg
Added SetDefDialLocation to avoid calling GetTransCaps to do the same thing.

Changes in 5.71
Recognise Windows 2012, MagRasOSVersion returns OS7 and OS8 but check >= OSVista

Changes in 5.72
Recognise Windows 8.1, 2012 R2.
No longer supporting Windows XP or earlier, although no code has been removed yet.

Changes in 5.73
Recognise Windows 10 and 2015

------------------------------------------------------------------------ *)

// following define changes TMagRas from being descended from TComponent
// to TCustomControl, allowing use in ActiveX (but stopping use in NT Service)
{.$DEFINE CUSTCNTL}

interface

//Include the resource with the bitmap. Note bitmap
//must have same name as the class
{$R MAGRASCON.RES}

uses
  SysUtils, Windows, Messages, Classes, {Forms,} Registry,
  {$IFDEF CUSTCNTL} Graphics, Controls, {$ENDIF}
  MagRasApi, MagTapiApi, MagRasStr, MagRasEnt, MagSubs1 ;

const
  MagVersion = 'TMagRasCon, 10th October 2014 - Release 5.73, Copyright 2014, Magenta Systems Ltd' ;

  Reg_RemAccProfile = 'RemoteAccess\Profile' ;    // Win9x only, for multilink, etc

//  Reg_RemAccAddress = 'RemoteAccess\Addresses' ;

// windows message used RAS events, random numbers
    WM_RAS_EVENT = WM_USER + 366 ;

type
  TRasConStateEvent = Procedure( Sender: TObject; Error: Longint;
                                            ErrorString: String) of Object;

{ Used by OnStateChanged event handler to indicate the source of the event }
  TStateEventSource = (SourceDial, SourceStatus, SourceHangup) ; // Angus 4.0

{ Used by GetSubHandles,  zero element is total in list }
  TSubHandList = array[0..MaxConnections] of HRasConn ;

{ Dialling properties extracted from LineTranslateCaps, these are totals.
  Used by GetTransCaps }
  TDialProps = record
    NumLocations: integer ;
    CurrentLocationId: DWORD ;
    NumCards: integer ;
    CurrentPreferredCardId: DWORD;
  end ;

{ Dialling properties extracted from LineLocationEntry, these are for
  a specific location.  Used by GetTransCaps }
  TDialLocation = record
    PermLocationID: DWORD ;
    LocationName: string ;
    CountryCode: DWORD;
    CityCode: string ;
    PreferredCardID: DWORD ;
    LocalAccessCode: string ;
    LongDistanceCode: string ;
    TollPrefixList: string ;
    CountryID: DWORD ;
    Options: DWORD ;
    CancelCallWaiting: string ;
  end ;

{ Calling card information extracted from LineTranslateCaps, these are for
  a specific location.  Used by GetTransCaps }
  TDialCard = record
    PermCardId: DWORD ;
    CardName: string ;
    CardNumberDigits: integer ;
    SameAreaRule: string ;
    LongDistanceRule: string ;
    InternationalRule: string ;
    Options: DWORD ;
  end ;

{ Dialling information for a single country extracted from LineCountryEntry,
  these are for a specific location.  Used by GetTransCaps }
  TDialCountry = record
    CountryId: DWORD ;
    CountryCode: DWORD ;
    NextCountryId: DWORD ;
    CountryName: string ;
    SameAreaRule: string ;
    LongDistanceRule: string ;
    InternationalRule: string ;
  end ;

{ Modem speaker mode when dialling, NT4/W2K/XP only   }   // 4.50
    TSpeakerMode = (SpeakerDefault, SpeakerOn, SpeakerOff) ;

// need some public stuff outside component, so Absolute works
// this means you can (reliably) only use one TRasCon object in your application

var
// W2K RASEntry size is 2,088, XP 2,884, plus space for alternate phone numbers
{ A common buffer used for RasEntryAll }
  EntryInfo: array [0..4095] of AnsiChar ;          // 4.62 Angus

{ Size of EntryInfo }
  EntrySize: DWORD ;                            // 3.1 Angus

//  RasEntry: TRasEntry absolute EntryInfo ;    // 3.1 Angus - always use W2K version
//  RasEntryNT4: TRasEntryNT4 absolute EntryInfo ;  // 3.1 Angus

{ Used for reading and writing phonebook entries, followed by a variable number
 of alternate locaal numbers.  Current size is set at EntrySize.  It is not
 normally necessary to access this information, it is public primarily for
 debugging purposes.  Used by GetEntryProperties and PutEntryProperties.  }
  RasEntryAll: TRasEntryWXP absolute EntryInfo ;  // 3.2 Angus

// NT RASSubEntry is 288 + (10 * 20) for 10 alternate phone numbers + 2 nulls
{ A common buffer used for RasSubEntryNT4 }
  SubEntryInfo: array [0..490] of AnsiChar ;            // 3.2 Angus

{ Size of SubEntryInfo }
  SubEntrySize: DWORD ;                             // 3.2 Angus

{ Used for reading and writing phonebook sub entries, followed by a variable number
 of alternate locaal numbers.  Current size is set at SubEntrySize.  It is not
 normally necessary to access this information, it is public primarily for
 debugging purposes.  Used by GetEntryProperties and PutEntryProperties.  }
  RasSubEntryNT4: TRasSubEntryNT4 absolute SubEntryInfo ;  // 3.2 Angus

// 108 bytes for modem, 126 bytes for ISDN/NDIS
{ A common buffer used for TAPI device inforomation, Win9x only, Used by
  GetEntryProperties and PutEntryProperties.  }
  DeviceInfo: array [0..255] of AnsiChar ;      // 3.1 Angus

{ Size of DeviceInfo }
  DeviceSize: DWORD ;                       // 3.1 Angus

{ TAPI device configuration for a Win9x modem only }
  DevCfg: TDevCfg absolute DeviceInfo ;     // 4.11 Angus

// need three versions - used for TRasCon.GetConnection

{ Array used to temporarily read active connections for all OSs, do
  not read it, used by GetConnection. }
  RasConnectAll: array[1..MaxConnections] of TRasConnWXP ; // Angus 4.62

{ Array used to temporarily read active connections for Win9x, NT4 and W2K do
  not read it, used by GetConnection. }
  RasConnect: array[1..MaxConnections] of TRasConn absolute RasConnectAll ; // Angus 3.2
  RasConnectNT4: array[1..MaxConnections] of TRasConnNT4 absolute RasConnectAll ; // Angus 3.2
  RasConnectW2K: array[1..MaxConnections] of TRasConnW2K absolute RasConnectAll ; // Angus 4.62
  RasConnectWXP: array[1..MaxConnections] of TRasConnWXP absolute RasConnectAll ; // Angus 4.92

{ Public variable to make TMagRasCon available to callback function  }
  ObjRAS: TObject ;

{ Private callback function for Win9x, called during dialling }
  procedure RasDialFunc1 (ConnHandle: HRASConn; Msg: UINT;
                    RasCS: integer; dwError, dwExtendedError: DWORD) stdcall;

{ Private callback function for NT4/W2K/XP, called during dialling }
// 20 July 2011 changed dwCallBackId to Pointer from DWORD
  procedure RasDialFunc2 (dwCallBackId: Pointer; dwSubEntry: DWORD; ConnHandle: HRASConn;
        Msg: UINT; RasCS: integer; dwError, dwExtendedError: DWORD); stdcall;

{ Private call function for TAPI, not used }
  procedure lineCallback (hDevice, dwMsg, dwCallbackInstance,
                            dwParam1, dwParam2, dwParam3: DWORD); stdcall;

type
{ State event parameters when dialling or getting call status }
  TRasStateRec = Record
    ConnectState: integer ;
    StatusStr: string ;
    CurDevName: string ;
    CurDevType: string ;
    ConnectPhoneNr: string ;
    StateRasConn: HRASConn ;
    StateSubEntry: integer  ;
    ConnectError: integer ;
    StateEventSource: TStateEventSource ;
    TickCount: DWORD ;
  end ;
  TStateEvent = Procedure (Sender: TObject; ConnState: TRasStateRec) of Object;

const
  StateQueueSize = 16 ;   // number of state events to buffer, rotary queue

type

{ Class containing details of active connection, set by GetConnections.   }
  TConnectionList = class(TList)
    function AddConnection(Connection: TRasConnWXP): Word;
    function RasConn(Index: Integer): HRASConn;
    function EntryName(Index: Integer): String;
    function DeviceType(Index: Integer): String;
    function DeviceName(Index: Integer): String;
    function Phonebook(Index: Integer): String;
    function SubEntry(Index: Integer): Integer;
    function guidEntry(Index: Integer): TGUID;
    function Flags(Index: Integer): DWORD ;                  // 4.62
    function LogonSessId(Index: Integer): TLargeInteger;
    function GetConn(Index: Integer): TRasConnWXP;
    procedure ClearFree;
    procedure Delete(Index: Integer);
    function CheckIndex(Index: Integer): boolean ;           // 4.80
  end;

{$IFDEF CUSTCNTL}
  TMagRasCon = class(TCustomControl)
{$ELSE}
  TMagRasCon = class(TComponent)
{$ENDIF}
  private
    { Private declarations }
{$IFDEF CUSTCNTL}
    fDesignBitmap : TBitmap;
{$ENDIF}
    fDAXCtrl : Boolean;
    fVersion: string ;

    fEntryName,
    fPhoneNumber,        // when dialling connection
    fPhoneBookPath,
    fCallbackNumber,
    fUserName,
    fPassword,
    fDomain,
    fDeviceType,
    fClientIP,
    fServerIP,
    fDeviceName,
    fDevicePort: AnsiString;    // Angus, 3.0

    fOnCallback,
    fStateChanged,  // ANGUS
    fOnConnect,
    fAboutToOpenPort,
    fPortOpened,
    fAboutToConnDev,
    fDevConnected,
    fAllDevsConnected,
    fAuthenticate,
    fAuthNotify,
    fAuthRetry,
    fAuthCallBack,
    fAuthChangePassword,
    fAuthProject,
    fAuthLinkSpeed,
    fAuthAck,
    fReAuthenticate,
    fAuthenticated,
    fPrepareforCallback,
    fWaitForModemReset,
    fInteractiveStarted,
    fRetryAuth,
    fPasswordExpired : TNotifyEvent;
    fOnDisconnect : TRasConStateEvent;

    RasDialParamsNT4: TRasDialParamsNT4;

// Angus - more useful things
    fLastError: LongInt;
    fRasConn: HRasConn;        // Connection handle, when dealing with only one!
    fConnectState: Word;
    fSavedState: Word ;        // saved connect state to avoid same thing repeatedly
    fConnectError: Word ;
    fStatusStr: String ;
    fCurConnName: AnsiString ;   // reported by RasEnumConnections
    fNumConns: DWord ;       // reported by RasEnumConnections
    fCurRasConn: HRasConn;   // reported by RasEnumConnections
    fPhoneCanonical: Ansistring ;  // formatted phone number for TAPI lineTranslateAddress
    fCountryID: integer ;       // Angus 3.1
    fPasswordFlag: longbool ;   // Angus 3.1,  false if password not returned from phonebook
    fAutoDialDLL: string ;      // Angus 3.1
    fAutoDialFunc: string ;     // Angus 3.1
    fSubEntry: integer ;        // Angus 3.2
    fCallbackId: pointer ;      // Angus 5.70 was integer
    fCallbackIdDummy: integer ; // Angus 5.70 copy of fCallbackId for backward compatibility
    fTotSubEntries: integer ;   // Angus 3.2
    fDialMode: integer ;        // Angus 3.2
    fCurDevName: Ansistring ;       // Angus 3.2  reported by RasEnumConnections
    fCurDevType: Ansistring ;       // Angus 3.2  reported by RasEnumConnections
    fCurPhonebook: Ansistring ;     // Angus 3.2  reported by RasEnumConnections
    fConnChangedFlag: boolean ; // Angus 3.2 true if Connections changed
    fStateRasConn: HRasConn;    // Angus 3.2 for StateChanged (on callback)
    fStateSubEntry: integer ;   // Angus 3.2 for StateChanged (on callback)
    fConnectPhoneNr: Ansistring ;   // Angus 3.2 reported by GetConnectStatus, etc (NT4)
    fPPPError: integer ;        // Angus 4.0 reported by GetIPAddress
    fConnProtocol: Ansistring ;     // Angus 4.0 reported by GetIPAddress
    fPPPReplyMessage: Ansistring ;  // Angus 4.0 reported by GetIPAddress
    fStateEventSource: TStateEventSource ; // Angus 4.0, set before StateChanged
    fCountryCode: integer ;      // Angus 4.0
    fAreaCode: Ansistring ;         // Angus 4.0
    fUseCountryAndAreaCodes: boolean ; // Angus 4.30
    fEntryOptions: DWORD ;      // Angus 4.30
    fPromoteAlternates: boolean ;   // Angus 4.30
    fDefCountryId: integer ;    // Angus 4.10
    fDUNVersion: string ;       // Angus 4.30 version of installed DUN, ie 4.10.1998
    fDUNInfo: string ;          // Angus 4.30 description of DUN, ie DUN 1.2c
    fLocalPhoneNumber: Ansistring ;     // Angus 4.30 local phone number from connection
//    OsInfo: TOSVERSIONINFO ;    // Angus 4.31 removed 5.20 using MagSubs1 extended version
    fPBPtr: PAnsiChar ;           // Angus 4.60 Pointer to fPhoneBookPath or Nil for Win9x // 5.40 was pointer
    fPBLocation: integer ;      // Angus 5.20 phone book location, allusers or current user
    fDefaultCreds: boolean ;    // Angus 5.22 set default credentials

// string lists
    fPhoneBookEntries: TStringList;
    fConnections: TConnectionList;
    fDeviceTypeList: TStringList;    // 3.0 Angus
    fDeviceNameList: TStringList;    // 3.0 Angus
    fAltPhoneNrList: TStringList;    // 3.2 Angus
    fSubLocalPhoneNumber: array [1..MaxSubEntries] of AnsiString ; // Angus 4.0
    fSubPhoneCanonical: array [1..MaxSubEntries] of AnsiString ;   // Angus 4.0
    fSubDeviceType: array [1..MaxSubEntries] of AnsiString ;       // Angus 4.0
    fSubDeviceName: array [1..MaxSubEntries] of AnsiString ;       // Angus 4.0
    fSubDevicePort: array [1..MaxSubEntries] of AnsiString ;       // Angus 4.0
//  fSubAltPhoneNrList: array [1..MaxSubEntries] of TStringList ;   // not yet
    fSubCurTotal: integer ;   // number of entries set in arrays   // Angus 4.0

 // TAPI stuff
    fTAPILineApp: TLineApp ;    // TAPI application handle
    fTAPIRunning: boolean ;     // is TAPI running
    fTAPIModemInst: DWORD ;   // configured devices, installed

// auto dial stuff, 4.41
    fAutoDialList: TStringList;    // 4.41 Angus
    fNotifyHandle: THandle ;
    fADConnEntryList: TStringList ;

// queue used to buffer state event changes that happen too fast for application to process, 4.50
    fRasStateRecQueue: array [1..StateQueueSize] of TRasStateRec ;
    fStateRecNext: integer ;
    fStateEvent: TStateEvent ;
    fWindowHandle: HWND ;

{$IFDEF CUSTCNTL}
    procedure WindowPosChanging (var msg : TWMWindowPosChanging); message
     WM_WINDOWPOSCHANGING;
{$ENDIF}

    procedure SetPhoneBookPath( Value: AnsiString );
    procedure SetPBLocation (Index: Integer) ;    // 5.20
    procedure StateChanged; // ANGUS
    procedure Connected;
    procedure Disconnected;
    procedure WaitingForCallBack;
    procedure AboutToOpenPort;
    procedure PortOpened;
    procedure AboutToConnDev;
    procedure DevConnected;
    procedure AllDevsConnected;
    procedure Authenticate;
    procedure AuthNotify;
    procedure AuthRetry;
    procedure AuthCallBack;
    procedure AuthChangePassword;
    procedure AuthProject;
    procedure AuthLinkSpeed;
    procedure AuthAck;
    procedure ReAuthenticate;
    procedure Authenticated;
    procedure PrepareforCallback;
    procedure WaitForModemReset;
    procedure InteractiveStarted;
    procedure RetryAuth;
    procedure PasswordExpired;
    procedure MoveDialParms ;

  protected
    { Protected declarations }
  public
    { Public declarations }

{ Initialises various lists and functions in TMagRasCon.

  Properties set are DUNVersion which contains the version string from
  RASAPI32.DLL, DUNInfo contains a more user friendly RAS/DUN version,
  DefCountryId contains the default country id used for creating new
  entries in the phonebook.   }
    CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;

{ Destroys TMagRasCon, calls IntDisconnect and StopTAPI. }
    DESTRUCTOR Destroy; override;

{$IFDEF CUSTCNTL}
    procedure Paint; override;
{$ENDIF}

{ Encapsulates the RasEnumConnections function to list all active RAS connections.

  This function is typically used in a timer event, every second or so, to check
  for connections.

  Returns the number of active connections, if any (also in NumConns).

  The Connections property is of type TConnectionList and returns various
  properties for each connection:
  RasConn (the handle so status can be checked and it may be hung-up),
  EntryName, Phonebook (if any, NT4/W2/XP only), DeviceName and DeviceType
  the modem or ISDN adapter used, NT4/W2/XP only), guidEntry (W2K/XP only),
  Flags (RASCF_AllUsers or RASCF_GlobalCreds, XP only), LogonSessId (logon
  session identifier of the user who established the connection, XP only).

  ConnChangedFlag is set true if the connection list has changed since the
  last time GetConnections was called.

  Note that on Win9x active connections are identified as soon as they start
  to access the device (ie before dialling), but on NT4/W2K/XP not until the
  device is connected and starts authentication.  So failed calls will not
  be detected.

  A maximum of eight active RAS connections will be listed. }
    FUNCTION GetConnections: LongInt;

{ Encapsulates the RasEnumConnections function to list one active RAS connection.
  If more than one concurrent connection is likely, GetConnections should be used instead.

  This function is typically used in a timer event, every second or so, to check
  for connections.  Returns the entry name of the active connections or blank if none.

  Note that on Win9x active connections are identified as soon as they start
  to access the device (ie before dialling), but on NT4/W2K/XP not until the
  device is connected and starts authentication.  So failed calls will not
  be detected on NT4, W2K and XP, TAPI monitoring must be used instead.

  Sets the CurConnName (same as result), CurRASConn, CurDevName, CurDevType and
  CurPhonebook properties.   }
    function GetConnection: AnsiString ;     // ANGUS

{ Encapsulates the RasEnumEntries function to list all entry names in a RAS phone
  book.  Always returns all entries.

  Returns 0 if successful, or an error code.

  The PhoneBookEntries property returns the list of entries as a TStringList
  (sorted).  }
    FUNCTION GetPhoneBookEntries: LongInt;

{ Encapsulates the RasGetConnectStatus function to retrieve information on the
 current status of the 'current' RAS connection.

 Note this function is retained for compatibility with earlier releases of
 TMagRas.  The 'current' connection is  defined as the one opened by Connect
 or RasReopen and uses an internal handle available as property IntRasConn.

 Result is 0 if  successful, or an error code.

 Properties set are ConnectState, ConnectError, CurDevName, CurDevType,
 ConnectPhoneNr (NT4/W2K/XP only, not Win9x).

 Calls GetConnectStatusEx.  }
    FUNCTION GetConnectStatus: LongInt;

{ Encapsulates the RasGetConnectStatus function to retrieve information on
 the current status of the specified RAS connection.

 ConnHandle is the connection handle returned by ConnectEx, AutoConnectEx
 or GetConnections. SubEntry is effectively ignored.

 Result is 0 if successful, or an error code.

 Properties set are ConnectState, ConnectError, CurDevName, CurDevType,
 ConnectPhoneNr (NT4/W2K/XP only, not Win9x).  Also sets StateRasConn to the passed
 handle and StateSubEntry to the passed subentry, for use in event handlers.

 Does not call any event handlers or set StatusStr. }
    FUNCTION GetConnectStatusEx (ConnHandle: HRasConn; SubEntry: integer): LongInt;   // 3.2 ANGUS

{ Encapsulates the RasGetConnectStatus function to retrieve information on
 the current status of the 'current' RAS connection.  This is similar to
 GetConnectStatus, but sets the StatusStr property with a message describing
 the current connection state, and calls the OnStateChanged event handler, with
 the StateEventSource property set to SourceStatus.

 Returns a message describing the current connection state (same as StatusStr).

 Properties set are StatusStr, ConnectState, ConnectError, CurDevName, CurDevType,
 ConnectPhoneNr (NT4/W2K/XP only, not Win9x), StateSubEntry, StateEventSource and
 StateRasConn.

 Calls GetConnectStatus and GetConnectStatusEx.  }
    FUNCTION CurrentStatus: String;

{ Encapsulates the RasGetConnectStatus function to retrieve information on the
 current status of the specified RAS connection.

 ConnHandle is the connection handle returned by ConnectEx, AutoConnectEx
 or GetConnections.  SubEntry is used to set the StateSubEntry property which
 can be checked in the OnStateChanged event handler so it knows which channel
 of a multilink connection the state applies to.

 This is similar to GetConnectStatusEx, but sets the StatusStr property with
 a message describing the current connection state, and calls the OnStateChanged
 event handler, with the StateEventSource property set to SourceStatus.

 Returns a message describing the current connection state (same as StatusStr).

 Properties set are StatusStr, ConnectState, ConnectError, CurDevName, CurDevType,
 ConnectPhoneNr (NT4/W2K/XP only, not Win9x), StateSubEntry, StateEventSource and
 StateRasConn.

 Calls GetConnectStatusEx.  }
    FUNCTION CurrentStatusEx (ConnHandle: HRasConn; SubEntry: integer): String;       // 3.2 ANGUS

{ Encapsulates the RasHangUp function to terminate the 'current' RAS connection.

  This function calls GetConnectStatusEx every 50ms until the connection
  hangs-up, and then returns, or times out after three seconds.  Calls the
  OnStateChanged event handler, with the StateEventSource property set to
  SourceHangup.

 Note this function is retained for compatibility with earlier releases of TMagRas.
 The 'current' connection is defined as the one opened by Connect or RasReopen
 and uses an internal handle available as property IntRasConn.

 Result is 0 if successful, or an error code.

 Properties set are StatusStr, ConnectState, ConnectError, StateSubEntry,
 StateEventSource and StateRasConn.

 Calls IntDisconnect, GetConnectStatusEx and DisconnectEx.  }
    FUNCTION Disconnect: LongInt;         // waits for disconnection


{ Encapsulates the RasHangUp function to terminate the specified RAS connection.

 ConnHandle is the connection handle returned by ConnectEx, AutoConnectEx
 or GetConnections.  SubEntry is used to set the StateSubEntry property which
 can be checked in the OnStateChanged event handler so it knows which channel
 of a multilink connection the state applies to.'current' RAS connection.
 WaitMs is the period to wait for confirmation the connection has died,
 typically 3,000 for three seconds.  The Events flag determines whether the
 OnStateChanged event handler should be called.  This must not be done if
 hang-up was originally called from the event handler (because of an error).

  This function calls GetConnectStatusEx every 50ms until the connection
  hangs-up, and then returns, or times out after three seconds.  Optionally
  calls the OnStateChanged event handler, with the StateEventSource property
  set to SourceHangup.

 Result is 0 if successful, or an error code.

 Properties set are StatusStr, ConnectState, ConnectError, StateSubEntry,
 StateEventSource and StateRasConn.

 Calls IntDisconnect and GetConnectStatusEx.  }
    FUNCTION DisconnectEx (ConnHandle: HRasConn; SubEntry, WaitMs: DWORD;
                                                 Events: boolean ): LongInt;  // 3.2 ANGUS

{ Encapsulates the RasHangUp function to terminate the 'current' RAS connection.
  Returns immediately, although the connection itself may not drop for a few
  seconds.

 Note this function is retained for compatibility with earlier releases of TMagRas.
 The 'current' connection is defined as the one opened by Connect or RasReopen
 and uses an internal handle available as property IntRasConn.

 The internal handle is cleared so it's not possible to check the connection
 status to see when it really hangs-up.

 Result is 0 if successful, or an error code. }

    function IntDisconnect: LongInt; // does not wait for disconnection, used internally to bypass fOnDisconnect

{ Encapsulates the RasHangUp function to terminate he specified RAS connection.

 ConnHandle is the connection handle returned by ConnectEx, AutoConnectEx
 or GetConnections.

 Returns immediately, although the connection itself may not drop for a few
 seconds.  GetConnectStatusEx may be used to wait until ConnectState is
 ERROR_INVALID_PORT_HANDLE which means the connection has died.

 Result is 0 if successful, or an error code. }
    function IntDisconnectEx (ConnHandle: HRasConn): LongInt;        // 3.2 ANGUS

{ Encapsulates the RasGetErrorString function, to convert a RAS error code
  to a message.  If the error code is not from RAS (ie 600 to 782), a windows
  system error message is returned instead.  }
    FUNCTION GetErrorString(ErrorCode: LongInt): String;

{ Encapsulates the RasDial function to establish a RAS connection between a
  RAS client and a RAS server, usually by dialling a using a modem or ISDN
  adapter.

 Note this function is retained for compatibility with earlier releases of TMagRas.
 The handle for the connection becomes 'current' and is available as property
 IntRasConn, and can be used by CurrentStatus, GetConnectState and Disconnect.

 See ConnectEx for details of properties that need setting before calling
 this method, and the events called.

  Calls ConnectEx }
    FUNCTION Connect: LongInt;

{ Encapsulates the RasDial function to establish a RAS connection between a
  RAS client and a RAS server, usually by dialling a using a modem or ISDN
  adapter.

  Various properties need be set before the Connect method is used.
  EntryName and PBLocation must be set, PhoneNumber, CallbackNumber,
  UserName, PassWord, Domain, SubEntry and CallbackId may all be left
  blank to use defaults.

  On NT4/W2K/XP, if both UserName and Password are empty RAS uses the user
  name and password of the current logon context for authentication. For a
  user mode application, RAS uses the credentials of the currently logged-on
  interactive user.

  SubEntry is ignored for Win9x, since multlink can not be controlled.
  For NT4, SubEntry 1 or 2 specifies the subentry to dial, but ignored
  if DialMode is set to dialAll.
  For W2K/XP, SubEntry 1 or 2 specifies the subentry to dial ignoring
  DialMode, with SubEntry set to 0 to dial all subentries

  If PhoneNumber is specified, it must include an extra digits or characters
  necessary to satisfy dialling properties.  It is normally created using
  TranslateAddr from the canonical number.

  Returns immediately, setting ConnHandle which may be used to get the call
  status using CurrentStatusEx or GetConnectStatusEx, or to hang-up the call
  using DisconnectEx.  If a multilink call has been started, the handle refers
  to all sub entries (or channels) and GetSubHandles and GetSubHandle may be
  used to get handles to the separate entries.  If a handle is returned, the
  application is responsible for freeing all memory allocated for the call by
  calling DisconnectEx whether the connection is successful or fails.

  Result is 0 if successful, or an error code.

  Use ConnectNT for NT4/W2K/XP which supports some extensions.

  Property set is StatusStr if there is an error.  Causes the OnStateChanged
  event handler to be called repeated during the dialling process, detailing
  progress, setting ConnectState, ConnectError, StateSubEntry, StateEventSource
  and StateRasConn properties.  The event handler is blocking and RAS is stalled
  while it is being processed.  }
    FUNCTION ConnectEx (var ConnHandle: HRasConn): LongInt;        // 3.2 ANGUS

{ Encapsulates the RasDial and RasGetEntryDialParams functions to establish
  a RAS connection between a RAS client and a RAS server, usually by dialling
  a using a modem or ISDN adapter.

  Note this function is retained for compatibility with earlier releases of TMagRas.
  The handle for the connection becomes 'current' and is available as property
  IntRasConn, and can be used by CurrentStatus, GetConnectState and Disconnect.

  The EntryName and PBLocation properties must be set before the AutoConnect
  method is used. SubEntry and CallbackId may be optionally set. All other
  properties are taken from the phonebook entry.

  See ConnectEx for details of the events called.

  Calls GetDialParams and Connect}
    FUNCTION AutoConnect: LongInt;       // ANGUS

{ Encapsulates the RasDial and RasGetEntryDialParams functions to establish
  a RAS connection between a RAS client and a RAS server, usually by dialling
  a using a modem or ISDN adapter.

  The EntryName and PBLocation properties must be set before the AutoConnectEx
  method is used. SubEntry and CallbackId may be optionally set. All other
  properties are taken from the phonebook entry.

  See ConnectEx for details of the events called.

  Calls GetDialParams and ConnectEx}
    FUNCTION AutoConnectEx (var ConnHandle: HRasConn): LongInt;      // 3.2 ANGUS

{ Leave connection open but disable access from TMagRas before terminating
  the program if the connection is to be left open.  Otherwise it's closed
  automatically when the component is finalised.

  Note this function is retained for compatibility with earlier releases of TMagRas.
  The 'current' connection is defined as the one opened by Connect or RasReopen
  and uses an internal handle available as property IntRasConn. The internal
  handle is cleared. }
    FUNCTION LeaveOpen: LongInt;        // ANGUS

{ Reopen for access by TMagRas an existing connection found by Connections.

  Note this function is retained for compatibility with earlier releases of TMagRas.
  The handle for the connection becomes 'current' and is available as property
  IntRasConn, and can be used by CurrentStatus, GetConnectState and Disconnect.

  The item parameter (base 1) is the connection number found by Connections,
  there may be more than one.  }
    FUNCTION ReOpen (item: integer) : LongInt;  // ANGUS

{ Encapsulates the RasDial function to establish a RAS connection between a
  RAS client and a RAS server, usually by dialling a using a modem or ISDN
  adapter.  This version only works on NT4/W2K/XP, use ConnectEx for Win9x.

  Various properties need be set before the Connect method is used.
  EntryName and PBLocation must be set, PhoneNumber, CallbackNumber,
  UserName, PassWord, Domain and CallbackId may all be left
  blank to use defaults.

  SubEntry should be set to zero to dial all multilink subentries, or
  one or two for a specific subentry only.  With NT4 this is ignored if
  the phonebook specifies dial all entries, but for W2K/XP it works.

  ConnHandle must be set to zero before starting to dial a call.  This
  method is also used to resume dialling from a paused state by passing
  the same handle returned when ConnectNT was first called.

  If both UserName and Password are empty RAS uses the user
  name and password of the current logon context for authentication. For a
  user mode application, RAS uses the credentials of the currently logged-on
  interactive user.

  If PhoneNumber is specified, it must include an extra digits or characters
  necessary to satisfy dialling properties.  It is normally created using
  TranslateAddr from the canonical number.

  AllowPause is set to true if paused states are allowed, to set a callback
  number or correct authentication details after receiving such an error.

  Speaker is set to SpeakerDefault to use the setting in the entry, or
  SpeakerOn or SpeakerOff to override for this call only.

  Returns immediately, setting ConnHandle which may be used to get the call
  status using CurrentStatusEx or GetConnectStatusEx, or to hang-up the call
  using DisconnectEx.  If a multilink call has been started, the handle refers
  to all sub entries (or channels) and GetSubHandles and GetSubHandle may be
  used to get handles to the separate entries.  If a handle is returned, the
  application is responsible for freeing all memory allocated for the call by
  calling DisconnectEx whether the connection is successful or fails.

  Result is 0 if successful, or an error code.

  Use ConnectNT for NT4/W2K/XP which supports some extensions.

  Property set is StatusStr if there is an error.  Causes the OnStateChanged
  event handler to be called repeated during the dialling process, detailing
  progress, setting ConnectState, ConnectError, StateSubEntry, StateEventSource
  and StateRasConn properties.  The event handler is blocking and RAS is stalled
  while it is being processed.  }
    FUNCTION ConnectNT (var ConnHandle: HRasConn;
                   AllowPause: boolean; Speaker: TSpeakerMode): LongInt;   // 4.50 ANGUS

{ Encapsulates the RasGetEntryDialParams function to retrieve connection
  information saved by the last successful call to the ConnectEx (etc) or
  SetDialParams function for a specified phonebook entry.  The properties
  returned are those required to authenticate a connection.  Note the
  telephone number is not returned.

  The EntryName and PBLocation properties must be set before the GetDialParams
  method is used.

  Result is 0 if successful, or an error code.

  Properties set are CallbackNumber, UserName, PassWord and Domain. }
    FUNCTION GetDialParams: longInt;    // ANGUS

{ Encapsulates the RasSetEntryDialParams function to save connection
  information for a specified phonebook entry.  Note the telephone number is
  not saved with this method.

  The The EntryName, PBLocation, CallbackNumber, UserName, PassWord and Domain
  properties   must be set before the PutDialParams method is used.
  Note that a password can not be removed at present.

  Result is 0 if successful, or an error code.  }
    FUNCTION SetDialParams: longInt;    // ANGUS

{ This is an internal function that takes the ConnectState property and
returns an message appropriate to the stats. }
    function GetMessText: String ;          // ANGUS

{ Check if RASAPI32.DLL is available without calling any functions. Not
  all PCs have RAS installed, and it's better to not to call such functions
  if they are likely to fail.

  After calling this method, the MagRasOSVersion variable will indicate the
  current OS as: OSW9x, OSNT4, OSW2K or OSWXP.  Some RAS properties are only
  available on NT4 and/or W2K and /or XP.  }
    function TestRas: boolean ;         // ANGUS

{ Encapsulates the RasEditPhonebookEntry function to edits an existing
  phonebook entry by displaying a dialog box in which the user can modify
  the existing information.

  The The EntryName and PBLocation properties must be set before the
  EditPhonebook method is used.  The winHandle parameter should be set to
  Application.Handle.

  Result is 0 if successful, or an error code.  }
    function EditPhonebook (winHandle: HWnd): LongInt ;     // ANGUS 4.41

{ Encapsulates the RasEditPhonebookEntry function to edits an existing
  phonebook entry by displaying a dialog box in which the user can modify
  the existing information.

  The The EntryName and PBLocation properties must be set before the
  EditPhonebook method is used. The winHandle parameter should be set
  to Application.Handle.

  Result is 0 if successful, or an error code.  }
    function CreatePhonebook (winHandle: HWnd): LongInt ;   // ANGUS 4.41

{ Encapsulates the RasDeleteEntry function which deletes an entry from a
  phonebook.

  The EThe EntryName and PBLocation properties must be set before the
  DeletePhonebook method is used.

  Result is 0 if successful, or an error code.  }
    function DeletePhonebook: LongInt ; // ANGUS

{ Encapsulates the RasRenameEntry function to change the name of an entry in
  a phonebook.  ValidateName should be called first to check Newname is
  valid.

  The The EntryName and PBLocation properties must be set before the
  RenamePhonebook method is used.

  Result is 0 if successful, or an error code.  }
    function RenamePhonebook (Newname: AnsiString): LongInt ;   // ANGUS

{ Encapsulates the RasValidateEntryName function to validate the format of
  a connection entry name. Newname must contain at least one non-white-space
  alphanumeric character, and must not already exist.  The PBLocation property
  must be set.

  Result is 0 if successful, or an error code.  }
    function ValidateName (Newname: AnsiString): LongInt ;      // ANGUS

{ Encapsulates the RasGetProjectionInfo function to obtain information about
  a RAS projection operation for a specified RAS component protocol.
  Specifically it returns the IP address alloocated to the connection.

 Note this function is retained for compatibility with earlier releases of TMagRas.
 The 'current' connection is defined as the one opened by Connect or RasReopen
 and uses an internal handle available as property IntRasConn.

  Calls GetProtocolEx. }
    function GetIPAddress: LongInt;     // ANGUS

{ Encapsulates the RasGetProjectionInfo function to obtain information about
  a RAS projection operation for a specified RAS component protocol.
  Specifically it returns the IP address alloocated to the connection.

  ConnHandle is the connection handle returned by ConnectEx, AutoConnectEx
  or GetConnections.

  Result is 0 if successful, or an error code.

  Properties set are PPPError, ConnProtocol, ClientIP, ServerIP, and
  PPPReplyMessage. }
    function GetProtocolEx (ConnHandle: HRasConn) : LongInt;    // Angus 4.0

{ Encapsulates the RasEnumDevices function to return the name and type of
  all available RAS capable devices (modems and ISDN adaptors).  This
  information is needed to create or validate entries in the phonebook.

  Result is 0 if successful, or an error code.

  Properties set are two TStringLists, DeviceNameList and DeviceTypeList
  with the device name and type (modem, isdn, vnp).  Do not sort these list
  otherwise they can not be matched.  }
    function GetDeviceList: LongInt ;    // 3.0 Angus

{ Encapsulates the RasGetEntryProperties and RasGetSubEntryProperties functions
  to retrieve properties of a main phonebook entry and any sub entries (for
  multilink connections), including telephone number needed to support
  dialling calls.  See GetEntryProps for full details.  This version always
  get subentries.

  Calls GetEntryProps.  }
    function GetEntryProperties: LongInt ;  // ANGUS

{ Encapsulates the RasGetEntryProperties and RasGetSubEntryProperties functions
  to retrieve properties of a main phonebook entry and any sub entries (for
  multilink connections), including telephone number needed to support
  dialling calls.

  Note that this method only returns a very limited range of properties, there
  is another method in TMagRasEdt that returns all properties.

  Also note that on Windows 9x, the RAS APIs were never properly updated by
  Microsoft for multilink calls, so no sub entries are available, although
  TotSubEntries may be set to 2 to indicate multilink is being used.

  The The EntryName and PBLocation properties must be set before the
  GetEntryProperties methodis used.  The SubEnts flag determines whether
  subentries are also returned, this double the time taken to get the entry.

  Result is 0 if successful, or an error code.

  Properties set are EntryOptions, UseCountryAndAreaCodes, PromoteAlternates,
  CountryCode, CountryID, AreaCode, LocalPhoneNumber, PhoneCanonical,
  DeviceName, DevicePort (NT4 only), DeviceType, AutoDialDLL, AutoDialFunc,
  DialMode, AltPhoneNrList (TStringList), TotSubEntries, SubCurTotal,
  SubDeviceType (array), SubDeviceName (array), SubLocalPhoneNumber (array) and
  SubPhoneCanonical (array).  Note that alternate numbers are not currently
  available for subentries.  The subentry arrays are of size SubCurTotal.

  Calls GetSubEntryProps.  }
    function GetEntryProps (subents: boolean): LongInt ;  // new 4.42

{ Creates a canonical phone number from CountryCode, AreaCode and
  LocalPhoneNumber, if UseCountryAndAreaCodes is true.

  The canonical number forrmat is '+44 (845) 123456' or
  '+country code (area code) local number'.  A canonical number may be
  missing the + in which case it will not be processed by dialling properties.
  The canonical number is passed to TranslateAddr or TransAddr to translate
  the number in a dialable number with optional calling card, etc }
    function GetCanonical (UseCountryAndAreaCodes: boolean; CountryCode: DWORD;
                                AreaCode, LocalPhoneNumber: AnsiString): AnsiString ;

{ Encapsulates the RasSetEntryProperties function to change the connection
  information for an entry in the phonebook.

  This method expects all the necessary entry information to have been
  read using GetEntryProperties and updated using UpdatePhonebook, otherwise
  an error 'Must Read Entry Properties First' appears in StatusStr. It can
  not be used to create a new entry, except by copying an existing entry
  and saving with a new name.

  The The EntryName and PBLocation properties must be set before the
  PutEntryProperties method is used.

  Note that the entry will only be saved correctly if it has a valid
  CountryId, DeviceName and DeviceType.

  Result is 0 if successful, or an error code.}
    function PutEntryProperties: LongInt ;              // 3.1 ANGUS

{ Copies an entry previously read with GetEntryProperties to create a new
  entry with Newname.  Note does not copy the logon and password.

  Result is 0 if successful, or an error code.

  Calls ValidateName and PutEntryProperties. }
    function CopyPhonebook (Newname: AnsiString): LongInt ; // 3.1 ANGUS

{ Encapsulates the RasSetEntryProperties function to update the connection
  information for an entry in the phonebook.

  The The EntryName and PBLocation properties must be set before the
  PutEntryProperties method is used.  The following properties
  are saved in the entry: PhoneCanonical, AutoDialDLL and AutoDialFunc.
  If the telephone number is already broken down into parts, use
  UpdatePhonebook instead.

  Result is 0 if successful, or an error code.

  Calls PutEntryProperties.  }
    function UpdatePhonebook: LongInt ;                 // 3.1 ANGUS

{ Encapsulates the RasSetEntryProperties function to update the connection
  information for an entry in the phonebook.

  The The EntryName and PBLocation properties must be set before the
  PutEntryProperties method is used.  The following properties are saved in
  the entry: UseCountryAndAreaCodes, CountryCode, CountryID, AreaCode,
  LocalPhoneNumber, AutoDialDLL and AutoDialFunc.

  Result is 0 if successful, or an error code.

  Calls PutEntryProperties.  }
    function PutPhonebook: LongInt ;

{ Splits a canonical phone number into CountryCode, AreaCode,
  LocalPhoneNumber and UseCountryAndAreaCodes.

  The canonical number forrmat is '+44 (845) 123456' or
  '+country code (area code) local number'. }
    procedure UnpackCanonical (PhoneCanonical: AnsiString;
            var UseCountryAndAreaCodes: boolean; var CountryCode: integer;
                                    var AreaCode, LocalPhoneNumber: AnsiString) ;

{ Encapsulates the RasGetSubEntryHandle function to retrieve a connection
  handle for a specified subentry of a multilink connection.  Using this
  handle allows the status of separate channels to be checked, and for
  the channels to be hung-up separately.  NT4/W2K/XP only.

  ConnHandle is the handle returned by ConnectEx (etx) or GetConnections,
  SubEnt (base 1) is the entry for which the handle should be returned in
  SubRasConn.

  Result is 0 if successful, or an error code.}
    function GetSubHandle (ConnHandle: HRasConn; SubEnt: integer;
                             var SubRasConn: HRasConn): LongInt;  // Angus 4.0

{ Encapsulates the RasGetSubEntryHandle function to retrieve all the
  connection handle for the subentries of a multilink connection.  Using
  these handles allows the status of separate channels to be checked, and
  for the channels to be hung-up separately.  NT4/W2K/XP only.

  ConnHandle is the handle returned by ConnectEx (etx) or GetConnections,
  SubEntries is the number of subentry handles to retrieve, SubHandList is
  an array (base 1) that will be filled with the handles, with the zero
  element being the total sub entries.

  Result is 0 if successful, or an error code.

  Calls GetSubHandle. }
    function GetSubHandles (ConnHandle: HRasConn; SubEntries: integer ;
                                    var SubHandList: TSubHandList): LongInt ;

{ Encapsulates the RasGetCountryInfo function to retrieve country specific
  dialing information from the Windows Telephony list of countries.

  Set CountryId (from GetEntryProperties), which then returns the CountryCode
  and CountryName.

  Another function is TMagRasEdt builds lists of all countries.

  Result is 0 if successful, or an error code.}
    function GetOneCountryInfo (CountryId: integer;
            var CountryCode: integer; var CountryName: AnsiString) : integer; // Angus 4.0

{ Encapsulates the lineInitialize function to initialise TAPI.  This is
  called automatically before the TAPI functions TranslateDialog,
  ConfigDialog, TranslateAddr and TransAddr.  TAPI is normally closed
  automatically with StopTAPI when the Close method is used.

  Returns true if successful. }
    function InitTAPI: boolean ;

{ Encapsulates the lineShutDown function to stop TAPI, if it has been
  started.  Used automatically when the Close method is used. }
    procedure StopTAPI ;

{ Encapsulates the lineTranslateDialog function to display the Dialing
  Properties modal dialog which allows the user to change the current
  location, adjust location and calling card parameters, and see the effect
  on a phone number about to be dialed.

  Parh is a handle to a window to which the dialog is to be attached, DevId
  should set to zero, Addr is optionally a canonical number whose translated
  version will be shown.

  Calls InitTAPI. }
    function TranslateDialog (Parh: HWND; Devid: integer;
                                                Addr : AnsiString) : Boolean;

{ Encapsulates the lineConfigDialog function causes the provider of the
  specified line device to display a dialog to allow the user to configure
  properters related to the line device.

  Parh is a handle to a window to which the dialog is to be attached, DevId
  (base 0) is the device Id to configure.  TMagRas does not provide an
  list of TAPI devices, this comes from TMagTapi.

  Calls InitTAPI. }
    function ConfigDialog (Parh: HWND; Devid: integer) : Boolean;

{ Encapsulates the lineTranslateAddress function which translates the
  specified canonical telephone number into other formats.

  InputNum must be in either the canonical address format, or an arbitrary
  string of dialable digits (non-canonical). Card and Flags are usually
  left as zero (unless special translation is needed).

  The canonical number forrmat is '+44 (845) 123456' or
  '+country code (area code) local number'.

  DisNum contains the translated output which can be displayed to the user
  for confirmation. It will be identical to DialableString, except that
  calling card digits will be replaced with the "friendly name" of the card
  enclosed within bracket characters (for example, "[AT&T Card]"). It should
  normally be safe to display this string in call-status dialog boxes without
  exposing private information to unauthorised persons. This information is
  also appropriate to include in call logs.

  DialNum the translated output which can be passed to the ConnectExt as a
  dialable string.  This property may contain private information such as
  calling card numbers. It should not be displayed to the user, to prevent
  inadvertent visibility to unauthorised persons.

  Returns true if successful. Failure usually means an invalid device id.

  Calls InitTAPI. }
    function TransAddr (Devid: integer; InputNum: AnsiString;           // 4.3
              Card, Flags: integer ; var DisNum, DialNum: AnsiString): Boolean ;

{ Encapsulates the lineTranslateAddress function which translates the
  specified canonical telephone number into other formats.

  This is a simplifed version of TransAddr that omits two rarely used
  parameters.

  Calls TransAddr. }
    function TranslateAddr (Devid: integer; InputNum: AnsiString;
                                var DisNum, DialNum: AnsiString) : Boolean;

{ Encapsulates the lineGetTranslateCaps and lineGetCountry functions to
  return address translation capabilities.  The four records returned
  contain all the information for the current location set in the Dialing
  Properties dialog, and all the information required to translate
  a canonical number into a dialable number (but it's much easier to use
  TransAddr to do this).  Note that currently this function only returns
  information for the current location.

  The application should declare:
    DialProps: TDialProps;
    DialLocation: TDialLocation;
    DialCard: TDialCard;
    DialCountry: TDialCountry;

  and pass them to be completed by GetTransCaps.  Important elements
  completed include DialLocation.CountryCode, DialLocation.CityCode and
  DialLocation.CountryId which may be used to default a new entry in the
  phonebook.

  Returns 0 if successful otherwise an error code.

  Call InitTAPI. }
    function GetTransCaps (var DialProps: TDialProps;
            var DialLocation: TDialLocation; var DialCard: TDialCard;
                                var DialCountry: TDialCountry ): integer ;  // 4.3

{ Ensure at least one dialling location exists otherwise create a default
  ;ocation for the default country to avoid the 'Location Information' dialog
  being displayed the first time a Tapi or RAS function is called. Note
  the program needs administrator rights to update the HLM registry keys.
  This function is automatically called by GetTransCaps.

  Returns true if a location exists or was created OK.}
    function SetDefDialLocation: boolean ;  // 4.70

{ Encapsulates the RasGetSubEntryProperties function to retrieve properties
  of  sub entries in a phonebook entry.  This is not called by applications,
  only by GetEntryProperties. }
    function GetSubEntryProps (SubEntry: integer): LongInt ;

{ Helper to information from an array. }
    function GetSubLocalPhoneNumber (Index: Integer): AnsiString ;

{ Helper to information from an array. }
    function GetSubPhoneCanonical (Index: Integer): AnsiString ;

{ Helper to information from an array. }
    function GetSubDeviceType (Index: Integer): AnsiString ;

{ Helper to information from an array. }
    function GetSubDeviceName (Index: Integer): AnsiString ;

{ Helper to information from an array. }
    function GetSubDevicePort (Index: Integer): AnsiString ;

// run time properties =======================================================


{ Contains the client's IP address on the RAS connection. This address has
  the form a.b.c. d; for example, 11.101.237.71.
  Set by GetProtocolEx. }
    PROPERTY ClientIP:    AnsiString      read fClientIP ;      // ANGUS

{ Contains the IP address of the remote PPP peer (that is, the server's IP
  address). This string is in "a.b.c.d" form. PPP does not require that servers
  provide this address, but Windows NT servers will consistently return the
  address anyway. Other PPP vendors may not provide the address.
  Set by GetProtocolEx. }
    PROPERTY ServerIP:    AnsiString      read fServerIP ;      // ANGUS

{ Contains the result of the PPP control protocol negotiation. A value of
  zero indicates success. A nonzero value indicates failure, and is the actual
  fatal error that occurred during the control protocol negotiation, the error
  that prevented the projection from completing successfully.
  Set by GetProtocolEx. }
    PROPERTY PPPError: integer        read fPPPError ;              // Angus 3.2

{ Contains the information on protocols negotiated, varies between windows
  versions, but should always have 'PPP', maybe also authentication details
  like 'PAP' for a LCP/multi-link negotiation.
  Set by GetProtocolEx. }
    PROPERTY ConnProtocol: AnsiString     read fConnProtocol ;          // Angus 3.2

//    PROPERTY PPPMultilink: boolean    read fPPPMultilink ;          // Angus 3.2

{ Reply message from a LCP/multi-link negotiation.
Set by GetProtocolEx. }
    PROPERTY PPPReplyMessage: AnsiString  read fPPPReplyMessage ;       // Angus 3.2

{ This property is set by almost all the TMagRasCon methods that fail.  The
  error code may be converted to a message using GetErrorString. }
    PROPERTY LastError:   LongInt     read fLastError ;     // Angus

{ The handle to the 'current' connection, use by Connect, AutoConnect,
  Reopen, CurrentStatus, GetConnectState and Disconnect.  May be checked
  for being non-zero to see if a connection is open }
    PROPERTY IntRasConn: HRASConn     read fRasConn ;        // Angus 3.2 changed from RasConn

{ Indicates the current state of the connection process, according to RASCS
  constants.  Below RASCS_Paused, the connection is being dialled,
  RASCS_Connected indicates that the connection has been successfully
  established, RASCS_Disconnected   indicates that the connection has failed.
  Set by ConnectStatus, ConnectStatusEx, Connect, AutoConnect, ConnectEx and
  AutoConnectEx. }
    PROPERTY ConnectState: Word       read fConnectState ;  // Angus

{ If nonzero, indicates the reason for connection failure.  The value is one
  of the RAS error values in the range 600 to 782, or ERROR_INVALID_HANDLE.
  Set by ConnectStatus, ConnectStatusEx, Connect, AutoConnect, ConnectEx and
  AutoConnectEx. }
    PROPERTY ConnectError: Word       read fConnectError ;  // Angus

{ A message describing the connection state or error condition, set
  by most TMagRasCon methods. }
    PROPERTY StatusStr:   String      read fStatusStr write fStatusStr ;   // Angus

{ Completed by GetConnection, entry name for the first active RAS connection. }
    PROPERTY CurConnName: AnsiString      read fCurConnName ;   // Angus

{ Completed by GetConnection, handle for the first active RAS connection. }
    PROPERTY CurRasConn: HRasConn     read fCurRasConn ;    // Angus

{ Completed by GetConnection, device name for the first active RAS connection. }
    PROPERTY CurDevName: AnsiString       read fCurDevName ;            // Angus 3.2

{ Completed by GetConnection, device type for the first active RAS connection. }
    PROPERTY CurDevType: AnsiString       read fCurDevType ;            // Angus 3.2

{ Completed by GetConnection, phonebook for the first active RAS connection. }
    PROPERTY CurPhonebook: AnsiString     read fCurPhonebook ;          // Angus 3.2

{ Completed by GetConnection, telephone number dialled by the the first
  active RAS connection (NT4 only). }
    PROPERTY ConnectPhoneNr: AnsiString   read fConnectPhoneNr ;        // Angus 3.2

{ Completed by GetConnection and GetConnections, number of active RAS
  connection.  If more than one, the properties for the first connection
  are unreliable and may change as connections start and stop.  }
    PROPERTY NumConns: DWORD          read fNumConns ;      // Angus

{ Completed by GetConnections, true if any connection details have changed
  since the last time the method was called. }
    PROPERTY ConnChangedFlag: boolean read fConnChangedFlag ;       // Angus 3.2

{ Completed by GetDialParams, true if a connection entry password returned.
  Set to true if PutDialParams should save a password }      // Angus 5.20
    PROPERTY PasswordFlag: longbool   read fPasswordFlag write fPasswordFlag ; // Angus 5.20

{ Completed by GetEntryProperties, total sub entries for the entry. }
    PROPERTY TotSubEntries: integer   read fTotSubEntries ;                 // Angus 3.2

{ Completed before the OnStateChanged event handler triggers, handle of the
  connection for which information is available, may be the main or sub
  entry handles (for multilink.}
    PROPERTY StateRasConn: HRasConn   read fStateRasConn ;          // Angus 3.2

{ Completed before the OnStateChanged event handler triggers, if non-zero
  information is for the sub entry (for multilink.}
    PROPERTY StateSubEntry: integer   read fStateSubEntry ;         // Angus 3.2

{ Completed before the OnStateChanged event handler triggers, the source
  of the event SourceDial, SourceStatus or SourceHangup.}
    PROPERTY StateEventSource: TStateEventSource read fStateEventSource ; // Angus 4.0

{ Completed by the the Create method, the default Country Id for creating
  a new phoneboook entry. }
    PROPERTY DefCountryId: integer    read fDefCountryId ;           // Angus 4.10

{ Completed by GetPhoneBookEntries, an unsorted list of the entries in the
  phonebook.  }
    PROPERTY PhoneBookEntries: TStringList read fPhoneBookEntries ; // Angus 4.10

{ Completed by GetConnections, an unsorted list of active connections.
  Sub properties are Connections.RasConn (I), Connections.EntryName (I),
  Connections.DeviceName (I), Connections.DeviceType (I),
  Connections.Phonebook (I), Connections.SubEntry (I), and
  Connections.guidEntry (I) (W2K/XP only).  Number of active is
  Connections.Count.  To avoid needing to check all this each time
  GetConnections is polled, the ConnChangedFlag is true if something
  changed. }
    PROPERTY Connections: TConnectionList  read fConnections ;      // Angus 4.10

{ Completed by GetDeviceList, unsorted list of the names of all available
  RAS capable devices (modems and ISDN adaptors).   The associated property
  DeviceTypeList contains the type for each name. }
    PROPERTY DeviceNameList: TStringList   read fDeviceNameList ;   // Angus 4.10

{ Completed by GetDeviceList, unsorted list of the type (modem, isdn, vnp)
  of all available RAS capable devices (modems and ISDN adaptors).   The
  associated property DeviceTypeName contains the name for each name. }
    PROPERTY DeviceTypeList: TStringList   read fDeviceTypeList ;   // Angus 4.10

{ Completed by GetEntryProperties, a list of alternate phone numbers that
  RAS dials in the order listed if the primary number (LocalPhoneNumber)
  fails to connect.  Note these numbers are alternatives for LocalPhoneNumber
  (but exclude LocalPhoneNumber) so they need to be combined into a canonical
  number for dialling.}
    PROPERTY AltPhoneNrList: TStringList   read fAltPhoneNrList ;   // Angus 4.10

{ Specifies a telephone number for a sub entry in a phonebook entry.

  Used by GetEntryProperties. }
    PROPERTY SubLocalPhoneNumber [index: integer]: AnsiString read GetSubLocalPhoneNumber ;  // Angus 4.10

{ Specifies the canonical phone number for a sub entry in a phonebook entry.
  This is created using GetCanonical from the properties UseCountryAndAreaCodes,
  CountryCode, AreaCode and sub entry LocalPhoneNumber.

  Used by GetEntryProperties. }
    PROPERTY SubPhoneCanonical [index: integer]: AnsiString read GetSubPhoneCanonical ; // Angus 4.10

{ Specifies the RAS device type for DeviceName for a sub entry in a phonebook.
  This member can be one of the following constants:
  RASDT_Modem  - A modem accessed through a COM port.
  RASDT_Isdn - An ISDN card with corresponding NDISWAN driver installed.
  RASDT_X25 - An X.25 card with corresponding NDISWAN driver installed.
  RASDT_Vpn  - A virtual private network connection.

  Returned by GetEntryProperties. }
    PROPERTY SubDeviceType [index: integer]: AnsiString read GetSubDeviceType ; // Angus 4.10

{ Specifies the name of a TAPI device use with a sub entry in a phonebook entry.

  Returned by GetEntryProperties. }
    PROPERTY SubDeviceName [index: integer]: AnsiString read GetSubDeviceName ; // Angus 4.10

{ Specifies the com port of a TAPI device use for a sub entry in a phonebook
  entry, NT4 only.

  Returned by GetEntryProperties. }
    PROPERTY SubDevicePort [index: integer]: AnsiString read GetSubDevicePort ; // Angus 4.10

{ Specifies the number of sub entries in a phonebook entry, NT4/W2K/XP only.

  Returned by GetEntryProperties. }
    PROPERTY SubCurTotal: integer     read fSubCurTotal ;           // Angus 4.10

{ Specifies the notify handle to check for conenctions, should be use by
  WaitForSingleObject, etc, W98, NT4,  W2K/XP only.

  Returned bySetConnNotify. }
    PROPERTY NotifyHandle: THandle      read fNotifyHandle ;

{ Specifies a list of all addresses in theAutoDial mapping database,
  NT4/W2K/XP only.

  Returned by ListAutoDialAddress. }
    PROPERTY AutoDialList: TStringList read  fAutoDialList ;       // Angus 4.41

{ Specifies a list of location IDs for a specific address in the AutoDial
  mapping database, matching ADConnEntryList, NT4/W2K/XP only.

  Returned by GetAutodialAddress, used by SetAutodialAddress. }
//    PROPERTY ADLocationIdList [index: integer]: DWORD read fADLocationIdList write fADLocationIdList ;

{ Specifies a list of connection entries for a specific address in the AutoDial
  mapping database, matching ADLocationIdList, NT4/W2K/XP only.

  Returned by GetAutodialAddress, used by SetAutodialAddress. }
    PROPERTY ADConnEntryList: TStringList   read fADConnEntryList write fADConnEntryList ;

  PUBLISHED

{ Used for ActiveX only }
    Property DAXCtrl : Boolean        read FDAXCtrl write FDAXCtrl default False;

{ The version of TMagRasCon - note is only only made writable so it displays
  in the object inspector }
    Property Version: string          read fVersion write fversion stored False;

{ The version of DUN, set by Create - note is only only made writable so it
  displays in the object inspector }
    PROPERTY DUNVersion: string       read fDUNVersion write fDUNVersion stored False;      // Angus 4.30

{ The description of DUN, set by Create - note is only only made writable so
  it displays in the object inspector }
    PROPERTY DUNInfo: string          read fDUNInfo write fDUNInfo stored False;            // Angus 4.30

{ Specifies the phonebook entry to use for various methods. Note that entries
  may be stored in different phonebooks.  For NT4, the Phonebookpath property
  may be set to specify a particular phonebook file.  For W2K and later, there
  are fixed phonebook files for each user logon and 'all users' for entries
  shared between all logons which are selected using the PBLocation property.
  Note that administrator rights are required to modify 'all users' entries.

 Used by Connect, ConnectEx, AutoConnect, AutoConnectEx, GetEntryProperties,
 PutEntryProperties, UpdatePhonebook, PutPhonebook, GetDialParams,
 SetDialParams, EditPhonebook. DeletePhonebook, RenamePhonebook. }
    PROPERTY EntryName: AnsiString        read fEntryName write fEntryName;

{ Specifies an overriding phone number to be used when dialling a RAS
  connection. If empty, the phonebook entry's phone number will be used.
  This number must include an extra digits or characters necessary to satisfy
  dialling properties.  It is normally created using TranslateAddr from the
  canonical number.

 Used by Connect, ConnectEx, AutoConnect, and AutoConnectEx. }
    PROPERTY PhoneNumber: AnsiString      read fPhoneNumber write fPhoneNumber;

{ Specifies the full path and filename of a phonebook (.PBK)
 file to be used when dialling a RAS connection (NT4 only).
 If this property is blank, the current default phone-book file is used.
 The default phone-book file is the one selected by the user in the
 User Preferences property sheet of the Dial-Up Networking dialog box.
 For W2K and later, the PBLocation property should be used to
 set the correct phonebook file.

 Used by Connect, ConnectEx, AutoConnect, AutoConnectEx, GetEntryProperties,
 PutEntryProperties, UpdatePhonebook, PutPhonebook, GetDialParams,
 SetDialParams, EditPhonebook. DeletePhonebook, RenamePhonebook. }
    PROPERTY PhoneBookPath:  AnsiString   read fPhoneBookPath write SetPhoneBookPath;

{ Specifies the Phonebook Location, either REN_User or REN_AllUsers for W2K
  and later. Note that administrator rights are required to modify 'all
  users' entries. The default PBLocation is REN_AllUsers for W2K and XP, but
  for Vista and later it's REN_User because applications normally run without
  administrator access (the IsProgAdmin function may be used access rights).

 Used by Connect, ConnectEx, AutoConnect, AutoConnectEx, GetEntryProperties,
 PutEntryProperties, UpdatePhonebook, PutPhonebook, GetDialParams,
 SetDialParams, EditPhonebook. DeletePhonebook, RenamePhonebook. }
    PROPERTY PBLocation:  integer     read fPBLocation write SetPBLocation ;  // Angus 5.20

{ Specifies PutDialProps or GetDialProps should to set or get user name and
  password for 'anyone who uses this computer', W2K and later only. }
    property bDefaultCreds : Boolean  read fDefaultCreds write fDefaultCreds ; // Angus 5.30

{ Specifies a callback phone number to be used when dialling a RAS connection.
  If left empty, no callback should not be used.  It is ignored unless the
  user has "Set By Caller" callback permission on the RAS server. An
  asterisk indicates that the number stored in the phonebook should be
  used for callback.

 Used by Connect, ConnectEx, AutoConnect, AutoConnectEx, GetDialParams and
 SetDialParams. }
    PROPERTY CallbackNumber: AnsiString   read fCallbackNumber write fCallbackNumber;

{ Specifies the user's user name to authenticate the user's access to the
  remote access server.

 Used by Connect, ConnectEx, AutoConnect, AutoConnectEx, GetDialParams and
 SetDialParams. }
    PROPERTY UserName:  AnsiString        read fUserName write fUserName;

{ Specifies the user's password to authenticate the user's access to the
  remote access server.  Note that on Windows 2000, the real password is
  is not available, instead a 'handle' comprising 14 asterisks are returned.

 Used by Connect, ConnectEx, AutoConnect, AutoConnectEx, GetDialParams and
 SetDialParams. }
    PROPERTY Password:  AnsiString        read fPassword write fPassword;

{ Specifies the domain on which authentication is to occur. If empty, the
  domain in which the remote access server is a member. An asterisk specifies
  the domain stored in the phonebook for the entry.

 Used by Connect, ConnectEx, AutoConnect, AutoConnectEx, GetDialParams and
 SetDialParams. }
    PROPERTY Domain:      AnsiString      read fDomain write fDomain;

{ Specifies the RAS device type for DeviceName. This member can be one of
  the following constants:
  RASDT_Modem  - A modem accessed through a COM port.
  RASDT_Isdn - An ISDN card with corresponding NDISWAN driver installed.
  RASDT_X25 - An X.25 card with corresponding NDISWAN driver installed.
  RASDT_Vpn  - A virtual private network connection.

  Returned by GetEntryProperties. }
    PROPERTY DeviceType:  AnsiString      read fDeviceType write fDeviceType;

{ Specifies the name of a TAPI device use with a phonebook entry.

  Returned by GetEntryProperties. }
    PROPERTY DeviceName:  AnsiString      read fDeviceName write fDeviceName;

{ Specifies the com port of a TAPI device use with a phonebook entry, NT4
  only.

  Returned by GetEntryProperties. }
    PROPERTY DevicePort:  AnsiString      read fDevicePort write fDevicePort;  // Angus 3.0

{ Specifies the canonical phone number for a phonebook entry.  This is
  created using GetCanonical from the properties UseCountryAndAreaCodes,
  CountryCode, AreaCode and LocalPhoneNumber.

  Used by GetEntryProperties and UpdatePhonebook. }
    PROPERTY PhoneCanonical: AnsiString   read fPhoneCanonical write fPhoneCanonical ;  // Angus

{ Specifies the full path and filename of the dynamic link library (DLL) for
  the customised AutoDial handler  for a phonebook entry. If AutoDialDLL
  contains an empty string,  RAS uses the default dialing user interface and
  the AutoDialFunc member is ignored.

  Used by GetEntryProperties, PutPhonebook and UpdatePhonebook. }
    PROPERTY AutoDialDLL: string      read fAutoDialDLL write fAutoDialDLL ;  // Angus 3.1

{ Specifies the the exported name of the RASADFunc function for the
  customized AutoDial handler. An AutoDial DLL must provide both ANSI and
  Unicode versions of the RASADFunc handler.  However, do not include the
  "A" or "W" suffix in the name specified by AutoDialFunc.

  Used by GetEntryProperties, PutPhonebook and UpdatePhonebook. }
    PROPERTY AutoDialFunc: string     read fAutoDialFunc write fAutoDialFunc ;  // Angus 3.1

{ Specifies the index of the initial subentry to dial. If the phonebook
  entry has no subentries or the dial mode of the phone-book entry is
  RASEDM_DialAll, this is ignored. If the dial mode is RASEDM_DialAsNeeded,
  RAS dials the specified subentry. If not a valid subentry index, RAS dials
  the first subentry.  NT4/W2K/XP only.

 Used by Connect, ConnectEx, AutoConnect, and AutoConnectEx. }
    PROPERTY SubEntry: integer        read fSubEntry write fSubEntry ;      // Angus 3.2

{ Note this is a dummy property and always blank. }
    PROPERTY CallbackId: integer      read fCallbackIdDummy write fCallbackIdDummy ; // Angus 5.70 publish dummy for backward compatibility

{ Specifies the TAPI country identifier for a phonebook entry. This is
  ignored unless UseCountryAndAreaCodes is true.  Note this may not be left
  blank when updating a phonebook entry.

  Used by GetEntryProperties and PutPhonebook. }
    PROPERTY CountryID: integer       read fCountryID write fCountryID ;  // Angus 3.1

{ Specifies the country code portion of the phone number for a phonebook
  entry.  The country code must correspond to the country identifier
  specified by CountryID. If CountryCode is zero, the country code is based
  on the country identifier specified by CountryID.   This is ignored unless
  UseCountryAndAreaCodes is true.

  Used by GetEntryProperties and PutPhonebook. }
    PROPERTY CountryCode: integer     read fCountryCode write fCountryCode ; // Angus 4.0

{ Specifies the area code for a phonebook entry.  If the dialing location
  does not have an area code, specify an blank. Do not include parentheses
  or other delimiters in the area code string, for example, "206" is a
  valid area code; "(206)" is not. Do not include a trunk access code either,
  for instance 0 in the UK, for example 845 is valid, 0845 is not.  This is
  ignored unless UseCountryAndAreaCodes is true.

  Used by GetEntryProperties and PutPhonebook. }
    PROPERTY AreaCode: AnsiString         read fAreaCode write fAreaCode ;    // Angus 4.0

{ Specifies a telephone number for a phonebook entry. The way RAS uses this
  string depends on UseCountryAndAreaCodes. If true, RAS combines
  LocalPhoneNumber with the country and area codes specified by CountryID,
  CountryCode, and AreaCode members. If false, RAS uses LocalPhoneNumber
  as the entire phone number.

  Used by GetEntryProperties and PutPhonebook. }
    PROPERTY LocalPhoneNumber: AnsiString read fLocalPhoneNumber write fLocalPhoneNumber; // Angus 4.30

{ Specifies if the CountryID, CountryCode and AreaCode properties are used
  to construct the phone number for a phonebook entry. If not set, these
  properties are ignored.  This corresponds to the Use Country and Area Codes
  check boxes in the Phone dialog box.

  Used by GetEntryProperties and PutPhonebook. }
    PROPERTY UseCountryAndAreaCodes: boolean read fUseCountryAndAreaCodes ; // Angus 4.30

{ Specifies entry option flags for a phonebook entry, used for special
  purposes.

  Used by GetEntryProperties. }
    PROPERTY EntryOptions: DWORD read fEntryOptions write fEntryOptions ;    // Angus 4.41

{ Specifies an effect when alternate phone numbers are defined AltPhoneNrList
  for a phonebook entry. If set, an alternate phone number that connects
  successfully becomes the primary phone number, and the current primary
  phone number is moved to the alternate list.  Note this functionality
  is not supported by ConnectEx (etc), it must be done in the application.

  Used by GetEntryProperties. }
    PROPERTY PromoteAlternates: boolean read fPromoteAlternates ;   // Angus 4.30

{ Specifies whether RAS should dial all of this entry's multilink subentries
  when the entry is first connected for a phonebook entry. This property can
  be one of the following values, DialAll (Dial all subentries initially) or
  DialAsNeeded (adjust the number of subentries as bandwidth is needed).

  Used by GetEntryProperties. }
    PROPERTY DialMode: integer        read fDialMode write fDialMode ;      // Angus 3.2

{ This event handler is called during connection establishment and during
  a connection.  onStateEvent is now preferred.

  Properties set before the event is triggered are ConnectState,
  ConnectError, StateSubEntry, StateEventSource and StateRasConn.
  The event handler is blocking and RAS is stalled while it is being
  processed.  The event handler will be called once after CurrentStatusEx,
  but multiple times after ConnectEx.

  Called by ConnectEx, Connect, AutoConnectEx, AutoConnect, CurrentStatusEx
  and CurrentStatus.  }
    PROPERTY OnStateChanged:    TNotifyEvent read fStateChanged write fStateChanged;

{ This event handler is called during connection establishment and
  during a connection.  It is preferred to onStateChanged since all the
  state information is passed as parameters, see TStateEvent.

  The event handler is not blocking but uses a queue so events are not
  lost.  The event handler will be called once after CurrentStatusEx,
  but multiple times after ConnectEx.

  Called by ConnectEx, Connect, AutoConnectEx, AutoConnect, CurrentStatusEx
  and CurrentStatus.  }
    PROPERTY OnStateEvent:      TStateEvent read fStateEvent write fStateEvent ;

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }
    PROPERTY OnConnect:         TNotifyEvent read fOnconnect write fOnConnect;

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }
    PROPERTY OnDisconnect:      TRasConStateEvent read fOnDisconnect write fOnDisconnect;

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }
    PROPERTY OnCallBack:        TNotifyEvent read fOnCallBack write fOnCallBack;

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }
    PROPERTY OnAboutToOpenPort: TNotifyEvent read fAboutToOpenPort write fAboutToOpenPort;

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }
    PROPERTY OnPortOpened:      TNotifyEvent read fPortOpened write fPortOpened;

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }
    PROPERTY OnAboutToConnDev:  TNotifyEvent read fAboutToConnDev write fAboutToConnDev;

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }
    PROPERTY OnDevConnected:    TNotifyEvent read fAllDevsConnected write fAllDevsConnected;

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }
    PROPERTY OnAllDevsConnected: TNotifyEvent read fAllDevsConnected write fAllDevsConnected;

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }
    PROPERTY OnAuthenticate:    TNotifyEvent read fAuthenticate write fAuthenticate;

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }
    PROPERTY OnAuthNotify:      TNotifyEvent read fAuthNotify write fAuthNotify;

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }
    property OnAuthRetry:       TNotifyEvent read fAuthRetry write fAuthRetry;

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }
    property OnAuthCallBack:    TNotifyEvent read fAuthCallBack write fAuthCallBack;

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }
    property OnAuthChangePassword: TNotifyEvent read fAuthChangePassword write fAuthChangePassword;

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }
    property OnAuthProject:     TNotifyEvent read fAuthProject write fAuthProject;

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }
    property OnAuthLinkSpeed:   TNotifyEvent read fAuthLinkSpeed write fAuthLinkSpeed;

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }
    property OnAuthAck:         TNotifyEvent read fAuthAck write fAuthAck;

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }
    property OnReAuthenticate:  TNotifyEvent read fReAuthenticate write fReAuthenticate;

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }
    property OnAuthenticated:   TNotifyEvent read fAuthenticated write fAuthenticated;

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }
    property OnPrepareforCallback: TNotifyEvent read fPrepareforCallback write fPrepareforCallback;

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }
    property OnWaitForModemReset:  TNotifyEvent read fWaitForModemReset write fWaitForModemReset;

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }
    property OnInteractiveStarted: TNotifyEvent read fInteractiveStarted write fInteractiveStarted;

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }
    property OnRetryAuth:       TNotifyEvent read fRetryAuth write fRetryAuth;

{ This event handler is retained for compatibility with earlier versions of
  of TMagRas, but is only triggered on the Win9x platform. }
    property OnPasswordExpired: TNotifyEvent read fPasswordExpired write fPasswordExpired;

private
    procedure WndProc (var Msgr: TMessage) ;
    procedure WMRASEVENT (var Msgr : TMessage); message WM_RAS_EVENT ;
  end;

// procedure Register;

implementation

//procedure Register;
//begin
//    RegisterComponents('Magenta Systems', [TMagRasCon]);
//end;

{ ********************************************************************* }
{                           TConnectionList                          }
{ ********************************************************************* }

function TConnectionList.AddConnection (Connection: TRasConnWXP): Word;
var
    Conn: PRasConnWXP;
begin
    Conn := New (PRasConnWXP);
    Conn^ := Connection;
    Add (Conn);
    result := 0 ;
end;

function TConnectionList.CheckIndex(Index: Integer): boolean ;   // 4.80
begin
    result := (Index >= 0) and (Index < Count) ;
end ;

function TConnectionList.RasConn(Index: Integer): HRasConn;
begin
    result := 0 ;
    if NOT CheckIndex (Index) then exit ;
    result := PRasConnWXP (Items[Index])^.RASConn; // handle
end;

function TConnectionList.EntryName (Index: Integer): String;
begin
    result := '' ;
    if NOT CheckIndex (Index) then exit ;
    if PRasConnWXP (Items [Index])^.szEntryName [0] <> #0 then
                result := String (PRasConnWXP (Items [Index])^.szEntryName) ; // 9 Aug 2010
end;

function TConnectionList.DeviceType (Index: Integer): String;
begin
    result := '' ;
    if NOT CheckIndex (Index) then exit ;
    if PRasConnWXP (Items [Index])^.szDeviceType [0] <> #0 then
        result := String (LowerCaseAnsi (PRasConnWXP (Items [Index])^.szDeviceType)) ; // 9 Aug 2010
end;

function TConnectionList.DeviceName (Index: Integer): String;
begin
    result := '' ;
    if NOT CheckIndex (Index) then exit ;
    if PRasConnWXP (Items [Index])^.szDeviceName [0] <> #0 then
                result := String (PRasConnWXP (Items [Index])^.szDeviceName) ; // 9 Aug 2010
end;

function TConnectionList.Phonebook (Index: Integer): String;
begin
    result := '' ;
    if NOT CheckIndex (Index) then exit ;
    if PRasConnWXP (Items [Index])^.szPhonebook [0] <> #0 then
                result := String (PRasConnWXP (Items [Index])^.szPhonebook) ; // 9 Aug 2010
end;

function TConnectionList.SubEntry (Index: Integer): Integer;
begin
    result := 0 ;
    if NOT CheckIndex (Index) then exit ;
    result := PRasConnWXP (Items [Index])^.dwSubEntry ;
end;

function TConnectionList.guidEntry (Index: Integer): TGUID;
begin
    FillChar (result, SizeOf (result), #0) ;
    if NOT CheckIndex (Index) then exit ;
    result := PRasConnWXP (Items [Index])^.guidEntry ;
end;

function TConnectionList.Flags(Index: Integer): DWORD ;                  // 4.62
begin
    result := 0 ;
    if NOT CheckIndex (Index) then exit ;
    result := PRasConnWXP (Items [Index])^.dwFlags ;
end;

function TConnectionList.LogonSessId(Index: Integer): TLargeInteger ;
begin
    result := 0 ;
    if NOT CheckIndex (Index) then exit ;
    result := PRasConnWXP (Items [Index])^.luid ;
end;

function TConnectionList.GetConn(Index: Integer): TRasConnWXP;
begin
    FillChar (result, SizeOf (result), #0) ;
    if NOT CheckIndex (Index) then exit ;
    result := PRasConnWXP (Items [Index])^ ;
end;

procedure TConnectionList.ClearFree;  // Angus, must clear memory before Tlist
begin
    while (Count > 0) do Delete (Count - 1) ;
    Clear ;
end;

procedure TConnectionList.Delete(Index: Integer);
begin
    if NOT CheckIndex (Index) then exit ;
    Dispose (PRasConnWXP (Items [Index]));
    Items [Index] := Nil;
    Inherited Delete (Index) ;
end;

{ ********************************************************************* }
{                           TMagRasCon Connection                       }
{ ********************************************************************* }

CONSTRUCTOR TMagRasCon.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    ObjRAS := self ;    // keep component OBJ for use by callback
    fWindowHandle := AllocateHWnd (WndProc) ;   // 4.50
    fRasConn := 0;
    fConnectState := 0;
    fDeviceName := '' ;     // Angus
    fDeviceType := '' ;     // Angus
    fDevicePort := '' ;     // Angus
    fCurDevName := '' ;     // Angus
    fCurDevType := '' ;     // Angus
    fLastError := 0 ;
    fStatusStr :=  '' ;
    fTAPIRunning := false ;
    fVersion := MagVersion ;
    fPhoneBookEntries := TStringList.Create;
    fConnections := TConnectionList.Create;
    fDeviceTypeList := TStringList.Create;
    fDeviceNameList := TStringList.Create;
    fAltPhoneNrList := TStringList.Create;
    fAutoDialList := TStringList.Create;
    fStateRecNext := 1 ;    // Angus 4.50
    fSubEntry := 1 ; // Angus 4.51, stop auto multilink on W2K
    fPhoneBookPath := '' ;   // 4.60
    fPBPtr := Nil ;          // 4.60

    // 5.20 default to current user profile on Vista, sets fPhoneBookPath and fPBPtr
    fDefaultCreds := false ; // 5.30
    MagSetPhoneBkFiles ;  // 5.21
    if MagRasOSVersion >= OSVista then
        PBLocation := REN_User
    else
        PBLocation := REN_AllUsers ;
  // should bDefaultCreds be set true for AllUsers?????

    // get default country ID - Angus 4.01
    // changed LOCALE_IDEFAULTCOUNTRY (obsoleted) to LOCALE_ICOUNTRY - Angus 5.50
    fDefCountryId := AscToInt (GetLcTypeInfo (LOCALE_ICOUNTRY)) ;
    MagSubs1.GetOSInfo ;   // 5.20 use MagSubs1 extended version which has wProductType

  // get DUN version, try and tell user what it means - Angus 4.30
    fDUNVersion := GetFileVerInfo (RASAPI_DLL, 'ProductVersion') ;
    fDUNInfo :=  SRasGenDUNInfo ; // Unknown DUN Version
    if Length (fDUNVersion) > 3 then    // 4.71
    begin
        if fDUNVersion = '4.00.950' then fDUNInfo := 'DUN 1.0/Win95/Retail'
        else if fDUNVersion = '4.00.1111' then fDUNInfo := 'DUN 1.1/Win95/OSR2'
        else if fDUNVersion = '4.00.1150' then fDUNInfo := 'DUN 1.1/Win95/ISDN'
        else if fDUNVersion = '4.10.1537' then fDUNInfo := 'DUN 1.2b/Win95/Patch'
        else if fDUNVersion = '4.10.1903' then fDUNInfo := 'DUN 1.3/Win95/Patch'
        else if fDUNVersion = '4.10.1998' then fDUNInfo := 'DUN 1.2c/Win98'
        else if fDUNVersion = '4.10.2000' then fDUNInfo := 'DUN 1.3/Win98/Patch'
        else if fDUNVersion = '4.10.2222' then fDUNInfo := 'DUN 1.3/Win98SE'
        else if copy (fDUNVersion, 1, 4) = '4.90' then fDUNInfo := 'DUN 1.3/WinME'
        else if fDUNVersion = '4.00' then fDUNInfo := 'DUN NT4 ' + OsInfo.szCSDVersion
        else if fDUNVersion [1] = '5' then
        begin
            fDUNInfo := 'DUN W2K ' + OsInfo.szCSDVersion ;
            if fDUNVersion [3] = '1' then fDUNInfo := 'DUN WinXP ' + OsInfo.szCSDVersion ;
            if fDUNVersion [3] = '2' then fDUNInfo := 'DUN Win2003 ' + OsInfo.szCSDVersion ;
        end
        else if fDUNVersion [1] = '6' then // 5.20
        begin
            if fDUNVersion [3] = '0' then
            begin
                if OsInfo.wProductType <= VER_NT_WORKSTATION then
                    fDUNInfo := 'DUN Vista ' + OsInfo.szCSDVersion
                else
                    fDUNInfo := 'DUN Win2008 ' + OsInfo.szCSDVersion ;
            end
            else if fDUNVersion [3] = '1' then  // 5.50
            begin
                if OsInfo.wProductType <= VER_NT_WORKSTATION then
                    fDUNInfo := 'DUN Windows 7 ' + OsInfo.szCSDVersion
                else
                    fDUNInfo := 'DUN Win2008 R2 ' + OsInfo.szCSDVersion ;
            end
            else if fDUNVersion [3] = '2' then  // 5.61
            begin
                if OsInfo.wProductType <= VER_NT_WORKSTATION then
                    fDUNInfo := 'DUN Windows 8 ' + OsInfo.szCSDVersion
                else
                    fDUNInfo := 'DUN Win2012 ' + OsInfo.szCSDVersion ;  // 5.71
            end
            else if fDUNVersion [3] = '3' then  // 5.72
            begin
                if OsInfo.wProductType <= VER_NT_WORKSTATION then
                    fDUNInfo := 'DUN Windows 8.1 ' + OsInfo.szCSDVersion
                else
                    fDUNInfo := 'DUN Win2012 R2 ' + OsInfo.szCSDVersion ;
            end
            else if fDUNVersion [3] = '4' then  // 5.73
            begin
                if OsInfo.wProductType <= VER_NT_WORKSTATION then
                    fDUNInfo := 'DUN Windows 10 ' + OsInfo.szCSDVersion
                else
                    fDUNInfo := 'DUN Win2015 ' + OsInfo.szCSDVersion ;
            end
            else
                fDUNInfo := 'Unknown Windows 6/7/8/10 version ' + OsInfo.szCSDVersion ;
        end
        else
            fDUNInfo := 'DUN ' + OsInfo.szCSDVersion ;
    end ;

  //Create a bitmap which is used to show the Components - Angus 4.0
  //in both Delphi and non Delphi environments.
{$IFDEF CUSTCNTL}
    fDesignBitmap := TBitmap.Create;
    fDesignBitmap.LoadFromResourceName (hInstance, ClassType.ClassName);
    Width := fDesignBitmap.Width;
    Height := fDesignBitmap.Height;
  //Note you can rely on this working in a non Delphi environment.
    if Not(csDesigning In ComponentState) then Visible := False;
{$ENDIF}
end;

destructor TMagRasCon.Destroy;
begin
    IntDisconnect;
    StopTAPI ;
    fPhoneBookEntries.Free;
    fConnections.ClearFree;
    fConnections.Free;
    fDeviceTypeList.Free ;
    fDeviceNameList.Free ;
    fAltPhoneNrList.Free;
    fAutoDialList.Free;
{$IFDEF CUSTCNTL}
    fDesignBitmap.Free;
{$ENDIF}
    DeallocateHWnd (fWindowHandle);  // 4.50
    inherited Destroy;
end;

procedure TMagRasCon.WndProc(var Msgr: TMessage);
begin
    with Msgr do
    begin
         if Msg = WM_RAS_EVENT then
             WMRASEVENT (Msgr)
         else
             result := DefWindowProc (fWindowHandle, Msg, wParam, lParam);
    end;
end;

{$IFDEF CUSTCNTL}
procedure TMagRasCon.WindowPosChanging (var msg :
  TWMWindowPosChanging);
begin
  //Don't let the user resize the component
  //at design time.
  msg.WindowPos.cx := Width;
  msg.WindowPos.cy := Height
end;

procedure TMagRasCon.Paint;
begin
  inherited Paint;
  //Draw the bitmap if we are in a Delphi form design state
  //or the component is being used as an Active X control.
  if (csDesigning in ComponentState)or (DAXCtrl) then
  begin
    Width := FDesignBitmap.Width;
    Height := FDesignBitmap.Height;
    Canvas.Draw(0, 0, FDesignBitmap);
  end;
end;
{$ENDIF}

// allow to check if RAS available without calling any functions

function TMagRasCon.TestRas: boolean ;
begin
    result := MagLoadRasApi ;
    if NOT result then
    begin
        fLastError := ERROR_DLL_NOT_FOUND ;
        fStatusStr := RASAPI_DLL + #32 + SRasGenNotAvailable ; // Not Available
    end ;
end ;

// alternate way of showing phone number

function TMagRasCon.GetCanonical (UseCountryAndAreaCodes: boolean;
            CountryCode: DWORD; AreaCode, LocalPhoneNumber: AnsiString): AnsiString ;
begin
    result := MagRasGetCanonical (UseCountryAndAreaCodes,
                                    CountryCode, AreaCode, LocalPhoneNumber) ;
end ;

// get entry properties from specified Phone Book (aka Dialup Connection)
// also get subentries, if any

function TMagRasCon.GetEntryProperties: LongInt ;
begin
    result := GetEntryProps (true) ;
end ;

// get entry properties from specified Phone Book (aka Dialup Connection)
// optionally get subentries, if any

function TMagRasCon.GetEntryProps (subents: boolean): LongInt ;  // new 4.42
var
    strptr: PAnsiChar ;
    ch: AnsiChar ;
    offset, I, stringlen: integer ;
    tempkey: HKEY ;
    keyname: AnsiString ;
    tempsize, dwType, dwSize, dwInfo: DWORD ;
//    reginfo: array [1..400] of AnsiChar ;
begin
    if (NOT MagLoadRasApi) or (NOT MagRasExtn_Flag) then
    begin
        fLastError := ERROR_DLL_NOT_FOUND ;
        result := fLastError ;
        exit ;
    end ;

// subentries
    for I := 1 to MaxSubEntries do
    begin
        fSubLocalPhoneNumber [I] := '' ;
        fSubPhoneCanonical [I] := '' ;
        fSubDeviceType [I] := '' ;
        fSubDeviceName [I] := '' ;
        fSubDevicePort [I] := '' ;
//      SubAltPhoneNrList [I].Clear ;
    end ;
    fSubCurTotal := 0 ;

// WARNING - RASEntry and RASEntryNT are in same absolute memory as EntryInfo
// NT returns more info, but first part of record is identical to Win9x
    EntrySize := 0 ;
    DeviceSize := 0 ;
    FillChar (EntryInfo, SizeOf (EntryInfo), #0) ;
    FillChar (DeviceInfo, SizeOf (TDevCfg), #0) ;
    RasEntryAll.dwSize := Sizeof (TRasEntryWXP);  // 9 July 2012 default to XP
    case MagRasOSVersion of
        OSW9x: RasEntryAll.dwSize := Sizeof (TRasEntry);
        OSNT4: RasEntryAll.dwSize := Sizeof (TRasEntryNT4);
        OSW2K: RasEntryAll.dwSize := Sizeof (TRasEntryW2K);
    end ;

// get buffer sizes
    Result := RasGetEntryProperties (fPBPtr, PAnsiChar (fEntryName), nil,
                                            EntrySize, nil, tempsize) ;
    if result = ERROR_BUFFER_TOO_SMALL then result := 0 ;

// get entry details from RAS
    if result = 0 then
    begin
        if EntrySize > SizeOf (EntryInfo) then EntrySize := SizeOf (EntryInfo) ;
        if MagRasOSVersion = OSW9x then
        begin
            if (DeviceSize > SizeOf (DeviceInfo)) or (DeviceSize = 0) then
                                            DeviceSize := Sizeof (DeviceInfo) ;
        end ;
        Result := RasGetEntryProperties (fPBPtr, PAnsiChar (fEntryName),
                                @EntryInfo, EntrySize, @DeviceInfo, DeviceSize) ;
    end ;
    fLastError := Result ;
    if fLastError <> 0 then
        fStatusStr := GetErrorString (fLastError)
    else
    begin
        with RasEntryAll do
        begin
            fEntryOptions := dwfOptions ;
            fUseCountryAndAreaCodes := (fEntryOptions and
                 RASEO_UseCountryAndAreaCodes) = RASEO_UseCountryAndAreaCodes ;
            fPromoteAlternates := (fEntryOptions and
                            RASEO_PromoteAlternates) = RASEO_PromoteAlternates ;
            fCountryCode := dwCountryCode;
            fCountryID := dwCountryID;
            fAreaCode := TrimAnsi (szAreaCode) ;
            fLocalPhoneNumber := TrimAnsi (szLocalPhoneNumber) ;    // Angus, 4.3
            fPhoneCanonical := MagRasGetCanonical (fUseCountryAndAreaCodes,
                                 fCountryCode, fAreaCode, fLocalPhoneNumber) ;

        // device device name, port and type
        // warning, NT devices have two nulls strings, device and port
            fDeviceName := GetDevNamePort (szDeviceName,
                                        sizeof (szDeviceName), fDevicePort);
            fDeviceType := LowerCaseAnsi (szDeviceType) ;

        // get AutoDial DLL info, and sub entries total (for dual channel ISDN)
            fAutoDialDLL := String (szAutoDialDLL) ;  // 9 Aug 2010
            fAutoDialFunc := String (szAutodialFunc) ;  // 9 Aug 2010

    // other connection stuff here, if we need it
            fTotSubEntries := dwSubEntries ;
            fDialMode := dwDialMode ;

    // may be extra phone numbers after structure, null separated list
            fAltPhoneNrList.Clear;
            if dwAlternateOffset <> 0 then
            begin
                strptr := @EntryInfo [dwAlternateOffset] ;
                offset := dwAlternateOffset ;
                ch := strptr^ ;
                while (ch <> #00) do
                begin
                    stringlen := StrLen (strptr) ;
                    fAltPhoneNrList.Add (Trim (String (FixedToPasStr (strptr, stringlen)))) ; // 9 Aug 2010
                    strptr := strptr + stringlen + 1 ;
                    offset := offset + stringlen + 1 ;
                    if offset >= abs (EntrySize) then break ;
                    ch := strptr^ ;
                end ;
            end ;

    // now get some subentries - the first is probably the same as the default above
            fSubCurTotal := 0 ;
            if (fTotSubEntries > 0) and subents then    // 4.42 slows down access
            begin
                for I := 1 to fTotSubEntries do
                begin
                    result := GetSubEntryProps (I) ;
                    fLastError := Result ;
                    if fLastError <> 0 then
                    begin
                        StatusStr := GetErrorString (fLastError) ;
                        break ;
                    end
                    else
                        inc (fSubCurTotal) ;
                end ;
            end ;
        end ;

    // check some Win9x device specific stuff - not really sure we care!
    {   if DevCfg.DfgHdr.dwSize = sizeof (DevCfg) then
        begin
            with DevCfg.DfgHdr do
            begin
                I := dwVersion ;
                if I = 0 then keyname := '123' ;  // prevents compiler removing prev line to we can debug it
                I := fwOptions ;  // flags
                if I = 0 then keyname := '123' ;
            end ;
            with DevCfg.CommConfig do
            begin
                if (dwProviderSize <> 0) and
                                    (dwProviderSubType = PST_Modem) then
                begin
                    with DevCfg.ModemSettings do
                    begin
                        I := dwInactivityTimeout ;
                        if I = 0 then keyname := '123' ;
                    end ;
                end ;
            end ;
        end ;  }

    // Win9x, get some stuff from registry
        if MagRasOSVersion = OSW9x then
        begin
            keyname := Reg_RemAccProfile + '\' + fEntryName ;
            if RegOpenKeyExA (HKEY_CURRENT_USER, PAnsiChar(keyname),
                                   0, KEY_READ, TempKey) = ERROR_SUCCESS then
            begin
                dwSize := 4 ;
                I := RegQueryValueExA (TempKey, PAnsiChar('MultiLink'), nil,
                                                @dwType, @dwInfo, @dwSize) ;
                if I = ERROR_SUCCESS then
                begin
                    if dwInfo = 1 then fTotSubEntries := 2 ; // but no real subentries
                end ;
                RegCloseKey (TempKey) ;
            end ;
       {        I := RegOpenKeyEx (HKEY_CURRENT_USER, PAnsiChar(Reg_RemAccAddress),
                                                        0, KEY_READ, TempKey) ;
            if I = ERROR_SUCCESS then
            begin
                dwSize := sizeof (reginfo) ;
                I := RegQueryValueEx (TempKey, PAnsiChar(fEntryName), nil,
                                            @dwType, @reginfo, @dwSize) ;
                if I = ERROR_SUCCESS then
                begin
                    reginfo [1] := reginfo [1] ;
                end ;
                RegCloseKey (TempKey) ;
            end ;   }
        end ;
    end ;
end ;

function TMagRasCon.GetSubEntryProps (SubEntry: integer): LongInt ;
var
    tempsize: DWORD ;
//    strptr: PAnsiChar ;
//    ch: AnsiChar ;
//  offset, stringlen: integer ;
begin
    result := -1 ;
    if MagRasOSVersion < OSNT4 then exit ;
    if (SubEntry <= 0) or (SubEntry > MaxSubEntries) then exit ;
    if (NOT MagLoadRasApi) then
    begin
        fLastError := ERROR_DLL_NOT_FOUND ;
        result := fLastError ;
        exit ;
    end ;

// WARNING - RASSubEntryNT4 is in same absolute memory as SubEntryEdtInfo
    SubEntrySize := 0 ;
    tempsize := 0 ;
    FillChar (SubEntryInfo, SizeOf (SubEntryInfo), #0) ;
    RasSubEntryNT4.dwSize := Sizeof (TRasSubEntryNT4);

// get buffer sizes
    Result := RasGetSubEntryProperties (fPBPtr, PAnsiChar (EntryName),
                                SubEntry, nil, SubEntrySize, nil, tempsize) ;
    if result = ERROR_BUFFER_TOO_SMALL then result := 0 ;

// get entry details
    if result = 0 then
    begin
        tempsize := 0 ;
        if SubEntrySize > SizeOf (SubEntryInfo) then
                                SubEntrySize := SizeOf (SubEntryInfo) ;
        Result := RasGetSubEntryProperties (fPBPtr, PAnsiChar (fEntryName),
                    SubEntry, @SubEntryInfo, SubEntrySize, nil, tempsize) ;
    end ;
    fLastError := Result ;
    if fLastError <> 0 then
        fStatusStr := GetErrorString (fLastError)
    else
    begin
        with RASSubEntryNT4 do   // get properties in same order as RASUBTRY
        begin
           // dwfFlags     // none at present

       // Device name, port and type
        // warning, NT4 (not W2K) devices have two nulls strings, device and port
            fSubDeviceType [SubEntry] := LowerCaseAnsi (StrPas (szDeviceType)) ;
            fSubDeviceName [SubEntry] := GetDevNamePort (szDeviceName,
                            sizeof (szDeviceName), fSubDevicePort [SubEntry]);

        // location/phone number - create canonical number by combining fields
            fSubLocalPhoneNumber [SubEntry] := szLocalPhoneNumber;
            fSubPhoneCanonical [SubEntry] :=
                         MagRasGetCanonical (fUseCountryAndAreaCodes,
                                 fCountryCode, fAreaCode, szLocalPhoneNumber) ;

     // may be extra phone numbers after structure, null separated list
         {  fSubAltPhoneNrList [SubEntry].Clear;
            if (dwAlternateOffset <> 0) then
            begin
                strptr := @EntryEdtInfo [dwAlternateOffset] ;
                offset := dwAlternateOffset ;
                ch := strptr^ ;
                while (ch <> #00) do
                begin
                    stringlen := StrLen (strptr) ;
                    fSubAltPhoneNrList [SubEntry].Add
                                            (FixedToPasStr (strptr, stringlen)) ;
                    strptr := strptr + stringlen + 1 ;
                    offset := offset + stringlen + 1 ;
                    if offset >= SubEntryEdtSize then break ;
                    ch := strptr^ ;
                end ;
            end ;   }
       end ;
    end ;
end ;

// save entry properties for specified Phone Book (aka Dialup Connection)
// note that EntryInfo must have been filled before calling this

function TMagRasCon.PutEntryProperties: LongInt ;
var
    devptr: pointer ;
begin
    if (NOT MagLoadRasApi) or (NOT MagRasExtn_Flag) then
    begin
        fLastError := ERROR_DLL_NOT_FOUND ;
        result := fLastError ;
        exit ;
    end ;

// make sure the entry and device buffers have been read by GetEntryProperties
    Result := ERROR_BUFFER_TOO_SMALL ;
    fStatusStr := SRasErrReadEntProp ; // Must Read Entry Properties First
    if (EntrySize = 0) or (RasEntryAll.dwSize = 0) then exit ;

// check for reasonably valid device specific information, else default it
// leaving DeviceEdtSize as zero will get proper default, but for new connection only
    if MagRasOSVersion = OSW9x then
    begin
        if (DeviceSize = 0) or
                    (DevCfg.DfgHdr.dwSize <> sizeof (DevCfg)) then
        begin
            if RasEntryAll.szDeviceType = RASDT_Modem then
            begin
                Move (DevCfgDefault, DeviceInfo, Sizeof (DevCfgDefault)) ;
                DeviceSize := Sizeof (DevCfgDefault) ;
            end ;
       end ;
    end ;

// update phonebook currently ignoring the device specific information
    devptr := Nil ;
    if DeviceSize <> 0 then devptr := @DeviceInfo ;
    Result := RasSetEntryProperties (fPBPtr, PAnsiChar (fEntryName),
                                @EntryInfo, EntrySize, devptr, DeviceSize) ;
    fLastError := Result ;
    if fLastError <> 0 then fStatusStr := GetErrorString (fLastError) ;
end ;

// unpack canonical phone number into it's separate parts
// format is +44 (0845) 123 456 where +country code (area code) local number
// no + means it's not a canonical number

procedure TMagRasCon.UnpackCanonical (PhoneCanonical: AnsiString;
    var UseCountryAndAreaCodes: boolean; var CountryCode: integer;
                                var AreaCode, LocalPhoneNumber: AnsiString) ;
begin
    MagRasUnpackCanonical (PhoneCanonical, UseCountryAndAreaCodes,
                                 CountryCode, AreaCode, LocalPhoneNumber) ;
end ;

// update and save entry properties for specified Phone Book (aka Dialup Connection)
// this version uses telephone number from separate components

function TMagRasCon.PutPhonebook: LongInt ;
begin
    with RasEntryAll do
    begin
        dwfOptions := fEntryOptions ;     // 4.41
        if fUseCountryAndAreaCodes then
            dwfOptions := dwfOptions OR RASEO_UseCountryAndAreaCodes   // set bit
        else
            dwfOptions := dwfOptions AND (NOT RASEO_UseCountryAndAreaCodes) ;   // clear bit
        dwCountryCode := fCountryCode ;
        dwCountryID := fCountryID ;
        StrPLCopy (szAreaCode, fAreaCode, sizeof (szAreaCode)) ;
        StrPLCopy (szLocalPhoneNumber, fLocalPhoneNumber,
                                             sizeof (szLocalPhoneNumber)) ;
        StrPLCopy (szAutoDialDLL, AnsiString (fAutoDialDLL), sizeof (szAutoDialDLL)) ; // 9 Aug 2010
        StrPLCopy (szAutodialFunc, AnsiString (fAutoDialFunc), sizeof (szAutodialFunc)) ; // 9 Aug 2010
        dwSubEntries := 0 ;     // these are added using another function
    end ;
    result := PutEntryProperties ;   // write phonebook
end ;

// update and save entry properties for specified Phone Book (aka Dialup Connection)
// this version uses canonical number

function TMagRasCon.UpdatePhonebook: LongInt ;
var
    oldcountry: integer ;
begin
    if fPhoneCanonical = '' then
    begin
        Result := ERROR_BUFFER_TOO_SMALL ;
        fStatusStr := SRasErrCanonNum ;  // Must Specify Canonical Number
        exit ;
    end ;
    oldcountry := fCountryCode ;
    MagRasUnpackCanonical (fPhoneCanonical, fUseCountryAndAreaCodes,
                            fCountryCode, fAreaCode, fLocalPhoneNumber) ;
// bodge, set countryid from from code - should really look it up
    if (oldcountry <> fCountryCode) or (fCountryId = 0) then
                                                fCountryID := fCountryCode ;
    result := PutPhonebook ;   // write phonebook
end ;

// save existing phonebook (got with GetEntryProperties) with new name
// note the logon and password are not copied

function TMagRasCon.CopyPhonebook (newname: AnsiString): LongInt ;
begin
    if (NOT MagLoadRasApi) or (NOT MagRasExtn_Flag) then
    begin
        fLastError := ERROR_DLL_NOT_FOUND ;
        result := fLastError ;
        exit ;
    end ;

// check new name
    Result := RasValidateEntryName (nil, PAnsiChar(newname));
    fLastError := Result ;
    if fLastError = 0 then
    begin
        fEntryName := newname ;
        result := PutEntryProperties ;   // write phonebook
    end
    else
        fStatusStr := GetErrorString (fLastError) ;
end ;

// get dial parms from specified Phone Book (aka Dialup Connection)

FUNCTION TMagRasCon.GetDialParams: LongInt;
var
    RasCredentials: TRasCredentials ;
begin
    if (NOT MagLoadRasApi) then
    begin
        fLastError := ERROR_DLL_NOT_FOUND ;
        result := fLastError ;
        exit ;
    end ;
    fUserName := '' ;
    FPassword := '' ;
    fCallBackNumber := '' ;
    fDomain := '' ;
    fPasswordFlag := false ;
    if MagRasOSVersion = OSW9x then
    begin
        FillChar (RasDialParamsNT4, SizeOf (TRasDialParamsNT4), 0);
        RasDialParamsNT4.dwSize := Sizeof (TRasDialParams);
        StrPCopy (RasDialParamsNT4.szEntryName, fEntryName) ;
        fLastError := RasGetEntryDialParams (fPBPtr,
                                        RasDialParamsNT4, fPasswordFlag) ;
        result := fLastError ;
        if fLastError = 0 then
        begin
            with RasDialParamsNT4 do
            begin
            // note phone number not supported with DialParams (use RASEntry)
                fUserName := szUserName ;
            // flag false means no password
            // could be because there is no network logon name
            //    or windows can not find the password file or it's corrupted
                if fPasswordFlag then fPassword := szPassword ;
                fCallBackNumber := szCallBackNumber ;
                fDomain := szDomain ;
            end ;
        end
        else
            fStatusStr := GetErrorString (LastError);
    end
    else
  // 5.10  use NT4 and later API
    begin
        FillChar (RasCredentials, SizeOf (RasCredentials), 0);
        RasCredentials.dwSize := Sizeof (RasCredentials);
        RasCredentials.dwMask := RASCM_UserName OR RASCM_Password OR
                                                             RASCM_Domain ;
        if (fPBLocation = REN_AllUsers) and fDefaultCreds then     // Angus 5.30
                RasCredentials.dwMask := RasCredentials.dwMask OR RASCM_DefaultCreds ;
        fLastError := RasGetCredentials (fPBPtr, PAnsiChar (fEntryName),
                                                         RasCredentials) ;
        result := fLastError ;
        if fLastError = 0 then
        begin
            with RasCredentials do
            begin
                if RasCredentials.dwMask AND RASCM_UserName =
                             RASCM_UserName then fUserName := szUserName ;
                if RasCredentials.dwMask AND RASCM_Password = RASCM_Password then
                begin
                    Fpassword := szPassword ;
                    fPasswordFlag := true ;  // 5.20
                end ;
                if RasCredentials.dwMask AND RASCM_Domain =
                             RASCM_Domain then fDomain := szDomain ;
            end ;
        end
        else
            fStatusStr := GetErrorString (LastError);
    end ;
end;

// internal proc used to setup dial params for Set and Dial

procedure TMagRasCon.MoveDialParms ;
begin
    if fSubEntry < 0 then fSubEntry := 1 ; // Angus 5.0 sanity check
    FillChar (RasDialParamsNT4, SizeOf (RasDialParamsNT4), #0);
    With RasDialParamsNT4 DO
    Begin
        if MagRasOSVersion = OSW9x then   // 9 Huly 2012
            dwSize := Sizeof (TRasDialParams)
        else
            dwSize := Sizeof (TRasDialParamsNT4);
        StrPLCopy (szEntryName, fEntryName, RAS_MaxEntryName);
        StrPLCopy (szPhoneNumber, fPhoneNumber, RAS_MaxPhoneNumber);  // Angus 4.30
        StrPLCopy (szCallbackNumber, fCallBackNumber, RAS_MaxCallbackNumber);
        StrPLCopy (szUserName, fUserName, UNLEN);
        StrPLCopy (szPassword, fPassWord, PWLEN);
        StrPLCopy (szDomain, fDomain, DNLEN);
        dwSubEntry := fSubEntry ;        // Angus 3.2  NT4/W2K which sub entry to dial, 0 for all on W2K
        dwCallbackId := fCallbackId ;    // Angus 3.2  NT4/W2K only
    End;
end ;

// update dial parms for specified Phonebook (aka Dialup Connection)

function TMagRasCon.SetDialParams: LongInt;
var
    removePW: boolean ;
    RasCredentials: TRasCredentials ;
begin
    if (NOT MagLoadRasApi) then
    begin
        fLastError := ERROR_DLL_NOT_FOUND ;
        result := fLastError ;
        exit ;
    end ;
    if MagRasOSVersion = OSW9x then
    begin
        MoveDialParms ;
        removePW := false ;         //  Angus 3.1, allow password to be removed
        if (fPassWord = '') and (NOT fPasswordFlag) then removePW := true ;  // Angus 5.20
        fLastError := RasSetEntryDialParams (Nil, RASDialParamsNT4, removePW) ;
        if fLastError <> 0 then
                fStatusStr := GetErrorString (fLastError);
    end
    else
  // Angus 5.10, use NT4 and later API that might work properly on Visa
    begin
        FillChar (RasCredentials, SizeOf (RasCredentials), 0);
        RasCredentials.dwSize := Sizeof (RasCredentials);
        RasCredentials.dwMask := RASCM_UserName ;
        if (fPassWord <> '') or (fPasswordFlag) then RasCredentials.dwMask :=
                               RasCredentials.dwMask OR RASCM_Password ; // Angus 5.20
        if fDomain <> '' then RasCredentials.dwMask :=
                               RasCredentials.dwMask OR RASCM_Domain ;   // Angus 5.20
        if (fPBLocation = REN_AllUsers) and fDefaultCreds then           // Angus 5.30
            RasCredentials.dwMask := RasCredentials.dwMask OR RASCM_DefaultCreds ;
        StrPLCopy (RasCredentials.szUserName, fUserName, UNLEN);
        StrPLCopy (RasCredentials.szPassword, fPassWord, PWLEN);
        StrPLCopy (RasCredentials.szDomain, fDomain, DNLEN);
        fLastError := RasSetCredentials (fPBPtr, PAnsiChar (fEntryName),
                                                     RasCredentials, false) ;
        if fLastError <> 0 then
                    fStatusStr := GetErrorString (LastError);
    end ;
    Result := fLastError;
end;

// edit specified Phonebook (aka Dialup Connection)

function TMagRasCon.EditPhonebook (winHandle: HWnd): LongInt ;      // 4.41
var
    EntryDlg: TRasEntryDlg ;
begin
    if (NOT MagLoadRasApi) then
    begin
        fLastError := ERROR_DLL_NOT_FOUND ;
        result := fLastError ;
        exit ;
    end ;
    if (MagRasOSVersion < OSNT4) or (MagRasDlgLib = 0) then
        Result := RasEditPhonebookEntry (winHandle, fPBPtr, PAnsiChar(fEntryName))    // 4.60
    else
    begin
 // Angus 5.20 - NT4 and later
        result := 0 ;
        FillChar (EntryDlg, SizeOf (EntryDlg), #0);
        EntryDlg.dwSize := SizeOf (EntryDlg) ;
        EntryDlg.hwndOwner := winHandle ;
        EntryDlg.dwFlags := RASEDFLAG_NoRename ;
        if NOT RasEntryDlg (fPBPtr, PAnsiChar(fEntryName), EntryDlg) then
                                            result := EntryDlg.dwError ;
        fLastError := Result ;
    end ;
    fLastError := Result ;
    if fLastError <> 0 then
                fStatusStr := GetErrorString (fLastError);
end ;

// delete specified Phonebook (aka Dialup Connection)

function TMagRasCon.DeletePhonebook: LongInt ;
begin
    if (NOT MagLoadRasApi) or (NOT MagRasExtn_Flag) then
    begin
        result := fLastError ;
        exit ;
    end ;
    Result := RasDeleteEntry (fPBPtr, PAnsiChar(fEntryName));   // 5.20 added phonebook
    fLastError := Result ;
    if Result = 0 then
        fEntryName := ''
    else
        fStatusStr := GetErrorString (fLastError);
end ;

// rename specified Phonebook (aka Dialup Connection)
// checks that name is valid first

function TMagRasCon.RenamePhonebook (newname: AnsiString): LongInt ;
begin
    if (NOT MagLoadRasApi) or (NOT MagRasExtn_Flag) then
    begin
        fLastError := ERROR_DLL_NOT_FOUND ;
        result := fLastError ;
        exit ;
    end ;
    Result := RasValidateEntryName (fPBPtr, PAnsiChar(newname));    // 5.20 added phonebook
    fLastError := Result ;
    if fLastError = 0 then
    begin
        Result := RasRenameEntry (fPBPtr, PAnsiChar(fEntryName), PAnsiChar(newname));    // 5.20 added phonebook
        if Result = 0 then fEntryName := newname ;
        fLastError := Result ;
    end  ;
    if fLastError <> 0 then
            fStatusStr := GetErrorString (fLastError);
end ;

// check specified Phonebook name is valid (aka Dialup Connection)

function TMagRasCon.ValidateName (newname: AnsiString): LongInt ;
begin
    if (NOT MagLoadRasApi) or (NOT MagRasExtn_Flag) then
    begin
        fLastError := ERROR_DLL_NOT_FOUND ;
        result := fLastError ;
        exit ;
    end ;
    Result := RasValidateEntryName (fPBPtr, PAnsiChar(newname));    // 5.20 added phonebook
    fLastError := Result ;
    if fLastError <> 0 then
                fStatusStr := GetErrorString (fLastError);
end ;

// create new Phonebook (aka Dialup Connection)

function TMagRasCon.CreatePhonebook (winHandle: HWnd): LongInt ; // 4.41
var
    EntryDlg: TRasEntryDlg ;
begin
    if (NOT MagLoadRasApi) then
    begin
        fLastError := ERROR_DLL_NOT_FOUND ;
        result := fLastError ;
        exit ;
    end ;
    if (MagRasOSVersion < OSNT4) or (MagRasDlgLib = 0) then
        Result := RasCreatePhonebookEntry (winHandle, Nil)
    else
    begin
 // Angus 5.20 - NT4 and later
        result := 0 ;
        FillChar (EntryDlg, SizeOf (EntryDlg), #0);
        EntryDlg.dwSize := SizeOf (EntryDlg) ;
        EntryDlg.hwndOwner := winHandle ;
        EntryDlg.dwFlags := RASEDFLAG_NewEntry OR RASEDFLAG_NewPhoneEntry;
        if NOT RasEntryDlg (fPBPtr, Nil, EntryDlg) then
                                        result := EntryDlg.dwError ;
        fLastError := Result ;
    end ;
    if fLastError <> 0 then
                fStatusStr := GetErrorString (fLastError);
end ;


// for specified Phonebook, get username/password/number, then dial it

function TMagRasCon.AutoConnectEx (var ConnHandle: HRasConn): LongInt;        // 3.2 ANGUS
begin
    GetDialParams ;
//    if fLastError = 0 then GetEntryProperties ;    // Angus 4.4, not used
    if fLastError = 0 then ConnectEx (ConnHandle) ;
    result := fLastError ;
end ;

function TMagRasCon.AutoConnect: LongInt;
begin
    GetDialParams ;
//    if fLastError = 0 then GetEntryProperties ;  // Angus 4.4, not used
    if fLastError = 0 then Connect ;
    result := fLastError ;
end ;

// for specified Phonebook, dial it (with given logon and password)
// returns handle to connection, which will be need for DisconnectEx and GetConnectStatusEx
// handle is also returned by event handlers

function TMagRasCon.ConnectEx (var ConnHandle: HRasConn): LongInt;        // 3.2 ANGUS
begin
    ConnHandle := 0 ;
    if (NOT MagLoadRasApi) then
    begin
        fLastError := ERROR_DLL_NOT_FOUND ;
        result := fLastError ;
        exit ;
    end ;
    fConnectState := 0;   // ANGUS, 16 Apr 98
    fSavedState := 9999 ;
// fSubEntry ignored for Win9x
// for NT4, 1 or 2 specifies the subentry to dial, but ignored if DialAll set
// for W2K, 1 or 2 specifies the subentry to dial ignoring DialAll, 0 to dial all
    if fSubEntry < 0 then fSubEntry := 1 ; // Angus 5.0 sanity check
//    if fSubEntry = 0 then fSubEntry := 1 ; // Angus 4.51, allow multilink on W2K
    MoveDialParms ;
    if MagRasOSVersion >= OSNT4 then
        fLastError := RasDial (Nil, fPBPtr, @RasDialParamsNT4,
                                                 2, @RasDialFunc2, ConnHandle)
    else
        fLastError := RasDial (Nil, Nil, @RasDialParamsNT4, 1,
                                                 @RasDialFunc1, ConnHandle) ;
    if fLastError <> 0 then     // Angus, get more info about failure
        fStatusStr := GetErrorString (fLastError) ;
    Result := fLastError;
end;

// for specified Phonebook, dial it (with given logon and password)
// makes connection current in TRasCon, but only one supported at a time

function TMagRasCon.Connect: LongInt;
begin
    If fRASConn <> 0 THEN { Allow only one connection }
                  IntDisconnect;
    fRasConn := 0;
    result := ConnectEx (fRasConn) ;
end ;

// for specified Phonebook, dial it (with given logon and password) - NT4/W2k only
// if ConnHandle is zero, will start dialling, non-zero will resume dialling from paused state
// returns handle to connection, which will be need for DisconnectEx and GetConnectStatusEx
// handle is also returned by event handlers

function TMagRasCon.ConnectNT (var ConnHandle: HRasConn;
                   AllowPause: boolean; Speaker: TSpeakerMode): LongInt;   // 4.50 ANGUS
var
    RasDialExtensions: TRasDialExtensions ;
begin
    if MagRasOSVersion < OSNT4 then
    begin
        fLastError := -1 ;
        result := fLastError ;
        exit ;
    end ;
    if (NOT MagLoadRasApi) then
    begin
        fLastError := ERROR_DLL_NOT_FOUND ;
        result := fLastError ;
        exit ;
    end ;
    fConnectState := 0;
    fSavedState := 9999 ;
    if fSubEntry < 0 then fSubEntry := 1 ; // Angus 5.0 sanity check
//  if fSubEntry = 0 then fSubEntry := 1 ;  // Angus 4.51, allow multilink on W2K
    MoveDialParms ;

// set-up dial extensions
    FillChar (RasDialExtensions, SizeOf (RasDialExtensions), #0);
    with RasDialExtensions do
    begin
        dwSize := SizeOf (RasDialExtensions) ;
        if AllowPause then dwfOptions := dwfOptions OR RDEOPT_PausedStates ;
        if Speaker <> SpeakerDefault then
        begin
            dwfOptions := dwfOptions OR RDEOPT_IgnoreModemSpeaker ;
            if Speaker = SpeakerOn then
                    dwfOptions := dwfOptions OR RDEOPT_SetModemSpeaker ;
        // currently ignoring software compression, useprefixsuffix (???)
        end ;
    end ;
    fLastError := RasDial (@RasDialExtensions, fPBPtr,
                            @RasDialParamsNT4, 2, @RasDialFunc2, ConnHandle) ;
    if fLastError <> 0 then
        fStatusStr := GetErrorString (fLastError) ;
    Result := fLastError;
end;

// get a standard Windows Error String

function TMagRasCon.GetErrorString(ErrorCode: LongInt): String;
var
    szErrorString: Array[0..256] of AnsiChar;
begin
    if (NOT MagLoadRasApi) then
    begin
        fLastError := ERROR_DLL_NOT_FOUND ;
        result := RASAPI_DLL + #32 + SRasGenNotAvailable ;  // Not Available
        exit ;
    end ;
    Result := '';
    if ErrorCode < 0 then exit ;
    FillChar (szErrorString, SizeOf (szErrorString), #0);
    RasGetErrorString (ErrorCode, szErrorString, 256);
    If szErrorString[0] <> #0 THEN
        Result := String (szErrorString) // 9 Aug 2010
    Else
        Result := SysErrorMessage (ErrorCode) ;  // Angus, try a windows error
end;

// Leave connection open but disable access from this component
// use this before terminating the program if the connection is
// to be left open, otherwise it's closed automatically

function TMagRasCon.LeaveOpen: LongInt;
begin
    fRASConn := 0;
    Result := IntDisconnect;
end ;

// ReOpen RAS an existing coonection for access from this component
// used after RAS.GetConnections finds one or more new connections

function TMagRasCon.ReOpen (item: integer) : LongInt;
begin
    if fRASConn = 0 then
    begin
        if fCurRASConn = 0 then
        begin
            fLastError := 6 ;    // bad handle
            result := fLastError ;
            exit ;
        end ;
       if item > 0 then
            fRASConn := fConnections.RasConn (item)
        else
            fRASConn := fCurRASConn ;
    end ;
    Result := GetConnectStatus ;
//  ResetPerfStats ;  // clear performance statistics
end ;

// Close RAS specified connection, do not wait for it to finish
// but application should wait until handle is closed

function TMagRasCon.IntDisconnectEx (ConnHandle: HRasConn): LongInt;        // 3.2 ANGUS
begin
    Result := 6 ;  // bad handle
    fConnectState := 0 ;     // 11 Nov 98 - ensure not left 'connected'
    If ConnHandle <> 0 THEN
    begin
        if (NOT MagLoadRasApi) then
        begin
            fLastError := ERROR_DLL_NOT_FOUND ;
            result := fLastError ;
            exit ;
        end ;
        Result := RASHangUp (ConnHandle);
    end ;
    fLastError := Result;
end;

// Close RAS current connection, do not wait for it to finish (used by Destroy)

function TMagRasCon.IntDisconnect: LongInt;
begin
    result := IntDisconnectEx (fRASConn) ;
    fRASConn := 0;
end;

// Close RAS specified connection, wait for it to finish, optional events
// warning - if using onStateEvent, messages are buffered since no message pump here

function TMagRasCon.DisconnectEx (ConnHandle: HRasConn; SubEntry,
                                 WaitMs: DWORD; Events: boolean ): LongInt;
var
    oldstate: integer ;
    TickCount: DWORD;
begin
    result := IntDisconnectEx (ConnHandle) ;
    OldState := -1 ;
    if ConnHandle <> 0 then
    begin
        TickCount := GetTickCount ;
        while true do   // forever
        begin
            GetConnectStatusEx (ConnHandle, SubEntry) ;   // ANGUS, wait for it to die
            if (oldstate <> fConnectState) and Events then
            begin
                fStatusStr := GetMessText ;
                fStateEventSource := SourceHangup ;
                StateChanged ;
                oldstate := fConnectState ;
            end ;
            if (fConnectState = RASCS_Disconnected) then break ;
            if (fConnectState = ERROR_INVALID_PORT_HANDLE) then break ;
            if (GetTickCount - TickCount) > WaitMs then break ;  // typically 3 seconds
            Sleep (50) ;  // 50 ms
        end ;
    end ;
    if ConnHandle = fRasConn then fRasConn := 0;
end;

// Close RAS current connection, wait for it to finish

function TMagRasCon.Disconnect: LongInt;
begin
    result := DisconnectEx (fRasConn, 0, 3000, true) ;
    Disconnected;
    fRasConn := 0;
end ;

// get IP addresses and other protocol information for specific RAS connections
// all subentries return the same information

function TMagRasCon.GetProtocolEx (ConnHandle: HRasConn) : LongInt;
var
    RasPppIp: TRasPppIp ;
    RasPppLcpW2K: TRasPppLcpW2K ;
    varsize: DWORD ;
    retval: integer ;
begin
    fPPPError := -1 ;
    fConnProtocol := '' ;
    fClientIP := '' ;
    fServerIP := '' ;
//    fPPPMultilink := false ;
    fPPPReplyMessage := '' ;
    if (NOT MagLoadRasApi) then
    begin
        fLastError := ERROR_DLL_NOT_FOUND ;
        result := fLastError ;
        exit ;
    end ;
    FillChar (RasPppIp, SizeOf(RasPppIp), #0);
    RasPppIp.dwSize := SizeOf (RasPppIp);
    varsize := SizeOf (RasPppIp);
    Result := RasGetProjectionInfo (ConnHandle, RASP_PppIp,
                                                @RasPppIp, varsize) ;
    fLastError := Result;
    if Result = 0 then
    begin
        fConnProtocol := 'PPP ';
        fPPPError := RasPppIp.dwError ;
        fClientIP := RasPppIp.szIpAddress;
        fServerIP := RasPppIp.szServerIpAddress;
    end ;

// now get LCP stuff
    FillChar (RasPppLcpW2K, SizeOf(RasPppLcpW2K), #0);
    RasPppLcpW2K.dwSize := SizeOf (TRasPppLcpW2K);  // 9 July 2012
    case MagRasOSVersion of     // Angus 3.2
        OSW9x: RasPppLcpW2K.dwSize := 28 ; //  SizeOf (TRasConPppLcp);
        OSNT4: RasPppLcpW2K.dwSize := SizeOf (TRasPppLcp);
    end ;
    varsize := RasPppLcpW2K.dwSize;
    retval:= RasGetProjectionInfo (ConnHandle, RASP_PppLcp,
                                                @RasPppLcpW2K, varsize) ;
    if retval = 0 then
    begin
        result := 0 ;
        with RasPppLcpW2K do
        begin
            case dwServerAuthenticationProtocol of
                RASLCPAP_PAP:  fConnProtocol := fConnProtocol + 'PAP ';
                RASLCPAP_SPAP: fConnProtocol := fConnProtocol + 'SPAP ';
                RASLCPAP_CHAP: fConnProtocol := fConnProtocol + 'CHAP ';
                RASLCPAP_EAP:  fConnProtocol := fConnProtocol + 'EAP ';
            end ;
       {    if MagRasOSVersion <> OSW2K then    // W2K always come back with multilink
                fPPPMultilink := fBundled
            else
                fPPPMultilink := fMultilink ;
            if fPPPMultilink then fConnProtocol := 'M' + fConnProtocol ;  }
            fPPPReplyMessage := szReplyMessage ;
            fPPPError := dwServerTerminateReason ;
        end ;
    end ;
end;

// get IP addresses for current RAS connections

function TMagRasCon.GetIPAddress: LongInt;
begin
    result := GetProtocolEx (fRASConn)
end ;

// for multilink connections, get the connection handle for a subentry

function TMagRasCon.GetSubHandle (ConnHandle: HRasConn; SubEnt: integer;
                                         var SubRasConn: HRasConn): LongInt;
begin
    SubRasConn := 0 ;
    result := -1 ;
    if (NOT MagLoadRasApi) then
    begin
        fLastError := ERROR_DLL_NOT_FOUND ;
        result := fLastError ;
        exit ;
    end ;
    if MagRasOSVersion < OSNT4 then exit ;
    result := RASGetSubEntryHandle (ConnHandle, SubEnt, SubRasConn) ;
    fLastError := result ;
end ;

// for multilink connections, get the connection handles for all subentries
// this assumes subentries are numbered contiguously - API documentation does not say

function TMagRasCon.GetSubHandles (ConnHandle: HRasConn; SubEntries: integer ;
                                    var SubHandList: TSubHandList): LongInt ;
var
    J, subtot: integer ;
begin
    result := -1 ;
    for J := 0 to MaxConnections do SubHandList [J] := 0 ;
    if (NOT MagLoadRasApi) then
    begin
        fLastError := ERROR_DLL_NOT_FOUND ;
        result := fLastError ;
        exit ;
    end ;
    if MagRasOSVersion < OSNT4 then exit ;
    subtot := 0 ;
    for J := 1 to SubEntries do
    begin
        result := GetSubHandle (ConnHandle, J, SubHandList [J]) ;
        if (result = 0) then inc (subtot) ;
   //       if result = ERROR_NO_MORE_ITEMS then break ;
   //     if (result = ERROR_PORT_NOT_OPEN)  // port may still be connecting
    end ;
    SubHandList [0] := subtot ;
    if subtot > 1 then result := 0 ;  // got any, OK
end ;

// get list of active RAS connections, ie things online

function TMagRasCon.GetConnections: LongInt;
var
    I, conntot: integer;
    newconn, oldconn: TRasConnWXP ;

// need to cope with three arrays of different lengths, with trailing nulls
    procedure CopyConnect (index: integer) ;
    begin
        inc (index) ;  // array is 1 based
        FillChar (newconn, SizeOf (TRasConnWXP), 0);
        Move (RasConnectWXP [index], newconn, Sizeof (TRasConnWXP)) ;  // 9 July 2012
        case MagRasOSVersion of
            OSW9x: Move (RasConnect [index], newconn, Sizeof (TRasConn)) ;
            OSNT4: Move (RasConnectNT4 [index], newconn, Sizeof (TRasConnNT4)) ;
            OSW2K: Move (RasConnectW2K [index], newconn, Sizeof (TRasConnW2K)) ;
        end ;
    end ;

begin
    fConnChangedFlag := false ; // will get set if any details are changed
    GetConnection ;             // fills array with connection details
    result := fLastError ;
    conntot := fNumConns ;
    if conntot > MaxConnections then conntot := MaxConnections ;
    if (conntot <> 0) then
    begin

    // see if connections have changed, avoids rebuilding list and changing display
        if ConnTot <> fConnections.Count then fConnChangedFlag := true ;
        if NOT fConnChangedFlag then
        begin
            for I := 0 to conntot - 1 do
            begin
                oldconn := fConnections.GetConn (I) ;  // previously saved details
                CopyConnect (I) ;                      // new details, in NewConn
                if NOT CompareMem (@oldconn, @newconn, Sizeof (TRasConnWXP))
                                                then fConnChangedFlag := true ;
            end ;
        end ;

    // add connections to list, if changed
        if fConnChangedFlag then
        begin
            fConnections.ClearFree;
            for I := 0 to conntot - 1 do
            begin
                CopyConnect (I) ;
                fConnections.AddConnection (newconn) ;
            end ;
        end ;
    end
    else
    begin

     // clear connection list, if none
        if fConnections.Count <> 0 then
        begin
            fConnections.ClearFree;
            fConnChangedFlag := true ;
        end ;
    end ;
end;

// get single connection details
// this avoids messing with string lists when one connection is common

function TMagRasCon.GetConnection: AnsiString;
var
    BufSize: DWord ;
begin
    fCurConnName := '' ;
    fCurRASConn := 0 ;
    fNumConns := 0 ;
    result := '' ;
    fLastError := ERROR_DLL_NOT_FOUND ;
    if (NOT MagLoadRasApi) then exit ;
    FillChar (RasConnectAll, SizeOf (RasConnectAll), 0);
    RasConnectAll [1].dwSize := Sizeof (TRasConnWXP);  // 9 July 2012
    case MagRasOSVersion of     // Angus 3.2
        OSW9x: RasConnectAll [1].dwSize := Sizeof (TRasConn);
        OSNT4: RasConnectAll [1].dwSize := Sizeof (TRasConnNT4);
        OSW2K: RasConnectAll [1].dwSize := Sizeof (TRasConnW2K);
    end ;
    BufSize := SizeOf (RASConnectAll);
//    BufSize := RasConnectAll [1].dwSize * MaxConnections;
    fLastError := RasEnumConnections (@RasConnectAll, BufSize, fNumConns);
    if ((fLastError  = 0) OR (fLastError = ERROR_BUFFER_TOO_SMALL)) and
                                                    (fNumConns <> 0) THEN
    begin
        fCurConnName := RASConnectAll [1].szEntryName ;
        fCurRASConn := RASConnectAll [1].rasConn ;
        fCurDevName := RASConnectAll [1].szDeviceName ;  // no port name!!
        fCurDevType := LowerCaseAnsi (RASConnectAll [1].szDeviceType) ;
        fCurPhonebook := RASConnectAll [1].szPhonebook ;    // NT 4 and later
        result := fCurConnName ;
    end ;
end;

// get list of defined TAPI device, ie modems or ISDN cards

function TMagRasCon.GetDeviceList: integer;
var
    RasDevNames, RasDevNames2: PRasDevInfo ;  // 4.91 dynamic instead of fixed memory
    BufSize, Entries: DWORD ;
    I: integer ;
begin
    if (NOT MagLoadRasApi) or (NOT MagRasExtn_Flag) then
    begin
        fLastError := ERROR_DLL_NOT_FOUND ;
        result := fLastError ;
        exit ;
    end ;
    fDeviceTypeList.Clear;
    fDeviceNameList.Clear;
    BufSize := 0 ;
    Entries := 0 ;
    result := RasEnumDevices (Nil, BufSize, Entries) ;  // 4.91 find memory needed
    if (result <> 0) and (result <> ERROR_BUFFER_TOO_SMALL) then exit ;
    if (Entries = 0) then exit ;
    GetMem (RasDevNames, BufSize) ;  // 4.91 allocate memory for all devices
    try
        FillChar (RasDevNames^, BufSize, 0) ;
        RasDevNames.dwSize := SizeOf (TRasDevInfo) ;
        Entries := 0 ;
        result := RasEnumDevices (RasDevNames, BufSize, Entries) ;
        fLastError := result;
        if (result <> 0) then exit ;
        RasDevNames2 := RasDevNames ;
        for I := 1 to Entries do
        begin
            if (RasDevNames2.szDeviceName[0] <> #0) then
            begin
                fDeviceNameList.Add (String (RasDevNames2.szDeviceName)) ;  // !! no port name // 9 Aug 2010
                fDeviceTypeList.Add (String (LowerCaseAnsi (RasDevNames2.szDeviceType))) ; // 9 Aug 2010
            end ;
            inc (RasDevNames2) ;   // next device
        end ;
    finally
        FreeMem (RasDevNames) ;
    end ;
end;

function TMagRasCon.GetOneCountryInfo (CountryId: integer;
            var CountryCode: integer; var CountryName: AnsiString)  : integer;
var
    RasExtCtryInfo: TRasExtCtryInfo ;
    BuffSize: DWORD ;
    strptr: PAnsiChar ;
    stringlen: integer ;
begin
    if (NOT MagLoadRasApi) or (NOT MagRasExtn_Flag) then
    begin
        fLastError := ERROR_DLL_NOT_FOUND ;
        result := fLastError ;
        exit ;
    end ;
    CountryCode := 0 ;
    CountryName := '' ;
    Buffsize := SizeOf(TRasExtCtryInfo) ;
    FillChar (RasExtCtryInfo, Buffsize, 0);
    RasExtCtryInfo.RasCtryInfo.dwSize := SizeOf(TRasCtryInfo) ;
    RasExtCtryInfo.RasCtryInfo.dwCountryId := CountryId ;
    Result := RasGetCountryInfo (@RasExtCtryInfo, Buffsize);
    fLastError := Result;
    If (Result <> 0) THEN exit ;
    CountryCode := RasExtCtryInfo.RasCtryInfo.dwCountryCode ;
    strptr := @RasExtCtryInfo.buffer
                [RasExtCtryInfo.RasCtryInfo.dwCountryNameOffset -
                                                     SizeOf(TRasCtryInfo)] ;
    stringlen := StrLen (strptr) ;
    CountryName := TrimAnsi (FixedToPasStr (strptr, stringlen)) ;
end ;

// get list of defined phonebook entries (aka DUN connections)

function TMagRasCon.GetPhoneBookEntries: integer;
var
    I: integer ;
begin
    fPhoneBookEntries.Clear;
    result := MagRasGetEntryList (fPhoneBookPath) ;
    fLastError := Result;
    if (Result <> 0) then exit ;
    if (MagRasNumEntryRec = 0) then exit ;
    for I := 0 to Pred (MagRasNumEntryRec) do
                fPhoneBookEntries.Add (String (MagRasEntryRecs [I].EntryName)) ; // 9 Aug 2010
end ;

// get text for RAS progress message

function TMagRasCon.GetMessText: String ;
begin
    Result := '' ;
    Case fConnectState OF
        RASCS_OpenPort: Result := SRasCsOpenPort ;           // Opening Serial Port
        RASCS_PortOpened: Result := SRasCsPortOpened ;       // Serial Port Opened
        RASCS_ConnectDevice: Result := SRasCsConnectDevice ; // Connecting/Dialling
        RASCS_DeviceConnected: Result := SRasCsDeviceConnected ; // Connected/Answered
        RASCS_AllDevicesConnected: Result := SRasCsAllDevicesConnected ; // Connected/Negotiation
        RASCS_Authenticate: Result := SRasCsAuthenticate ; // Validating User and Password
        RASCS_AuthNotify: Result := SRasCsAuthNotify ; // Authentication Notification
        RASCS_AuthCallBack: Result := SRasCsAuthCallBack ; // Authentication Call Back
        RASCS_AuthProject: Result := SRasCsAuthProject ; // Projection Started
        RASCS_AuthLinkSpeed: Result := SRasCsAuthLinkSpeed ; // Calculating Link speed
        RASCS_AuthAck: Result := SRasCsAuthAck ; // Authentication acknowledged
        RASCS_ReAuthenticate: Result := SRasCsReAuthenticate ; // Reauthenticating
        RASCS_Authenticated: Result := SRasCsAuthenticated ; // Login Authenticated
        RASCS_PrepareforCallBack: Result := SRasCsPrepareforCallBack ; // Preparing for Callback
        RASCS_WaitForModemReset: Result := SRasCsWaitForModemReset ; // Waiting for Modem Reset
        RASCS_WaitForCallBack: Result := SRasCsWaitForCallBack ; // Waiting for Callback
        RASCS_Projected: Result := SRasCsProjected ; // Projection Completion
        RASCS_StartAuthentication: Result := SRasCsStartAuthentication ; // Start Authentication
        RASCS_CallbackComplete: Result := SRasCsCallbackComplete ; // Callback Complete
        RASCS_LogonNetwork: Result := SRasCsLogonNetwork ; // Logon to Network
        RASCS_SubEntryConnected: Result := SRasCsSubEntryConnected ; // Extra Channel Connected
        RASCS_SubEntryDisconnected: Result := SRasCsSubEntryDisconnected ; // Extra Channel Disconnected
        RASCS_Interactive: Result := SRasCsInteractive ; // Interactive Terminal
        RASCS_RetryAuthentication: Result := SRasCsRetryAuthentication ; // Retry Authentication
        RASCS_CallbackSetByCaller: Result := SRasCsCallbackSetByCaller ; // Callback Set By Caller
        RASCS_PasswordExpired: Result := SRasCsPasswordExpired ; // Password Expires
        RASCS_InvokeEapUI: Result := SRasCsInvokeEapUI ; // Paused for Authentication
        RASCS_Connected: Result := SRasCsConnected ; // Connected/Online
        RASCS_Disconnected: Result := SRasCsDisconnected ; // Disconnected/Offline
    End; { Case }
    if Result = '' then
    begin
    // connect state should not have errors, but of course it does!
        If fConnectState > Pending THEN     // 600
            Result := GetErrorString (fConnectState)
         else
            Result := Format (SRasGenBadState, [fConnectState]) ; // Unknown State
    end ;

end ;

// message handler for RAS events, 4.50

procedure  TMagRasCon.WMRASEVENT (var Msgr: TMessage);
var
    I: integer ;
begin
    if Assigned (fStateEvent) then
    begin
        I := Msgr.WParam ;
        if (I = 0) or (I > StateQueueSize) then exit ;  // sanity check
        fStateEvent (Self, fRasStateRecQueue [I]) ;
{        with fRasStateRecQueue [I] do
        begin
            fStateEvent (Self, eConnectState, eStatusStr, eStateRasConn,
                  eStateSubEntry, eConnectError, eStateEventSource, eTickCount) ;
        end ;  }
    end ;
end ;

PROCEDURE TMagRasCon.StateChanged;
BEGIN
// implement queue for event changes, since application may not process them fast enough, 4.50
    if Assigned (fStateEvent) then
    begin
        with fRasStateRecQueue [fStateRecNext] do
        begin
            ConnectState := fConnectState ;
            StatusStr := fStatusStr ;
            CurDevName := String (fCurDevName) ;   // 9 Aug 2010
            CurDevType := String (fCurDevType) ;  // 9 Aug 2010
            ConnectPhoneNr := String (fConnectPhoneNr) ;  // 9 Aug 2010
            StateRasConn := fStateRasConn ;
            StateSubEntry := fStateSubEntry ;
            ConnectError :=  fConnectError ;
            StateEventSource := fStateEventSource ;
            TickCount := GetTickCount ;
        end ;
        PostMessage (fWindowHandle, WM_RAS_EVENT, fStateRecNext, 0) ;
        inc (fStateRecNext) ;
        if fStateRecNext > StateQueueSize then fStateRecNext := 1 ;
    end ;

// optional event handler - RAS is blocked until this returns
    If Assigned (fStateChanged) THEN
    begin
        fStateChanged (Self) ;
    end ;
END;

// callback function, called by RASDial - blocking

procedure RasDialFunc1 (ConnHandle: HRASConn; Msg: UINT;
        RasCS: integer; dwError, dwExtendedError: DWORD); stdcall;
var
    RASOBJ: TMagRasCon absolute ObjRAS ; // cludge because this is a callback ;
begin
    with RASOBJ do
    begin
        fStateEventSource := SourceDial ;
        fStateRasConn := ConnHandle ;  // so we know which connection is being reported
        fStateSubEntry := 0 ;       // none on Win9x
        fConnectError := dwError ;
        fCurDevName := '' ;
        fCurDevType := '' ;
        fLocalPhoneNumber := '' ;
        if dwError <> 0 then
        begin
            fLastError := dwError ;
            fConnectState := fLastError ;    // ANGUS, ensure errors handled
            fStatusStr := GetErrorString (fLastError);
            StateChanged ;            // ANGUS - general catch all
        end
        else
        begin
            fConnectState := RasCS ;
            fStatusStr := GetMessText ;  // get description
            StateChanged ;            // ANGUS - general catch all
            case fConnectState of
                RASCS_OpenPort : AboutToOpenPort;
                RASCS_PortOpened : PortOpened;
                RASCS_ConnectDevice : AboutToConnDev;
                RASCS_DeviceConnected : DevConnected;
                RASCS_AllDevicesConnected : AllDevsConnected;
                RASCS_Authenticate : Authenticate;
                RASCS_AuthNotify : AuthNotify;
                RASCS_AuthRetry : AuthRetry;
                RASCS_AuthCallback : AuthCallBack;
                RASCS_AuthChangePassword : AuthChangePassword;
                RASCS_AuthProject : AuthProject;
                RASCS_AuthLinkSpeed : AuthLinkSpeed;
                RASCS_AuthAck : AuthAck;
                RASCS_ReAuthenticate : ReAuthenticate;
                RASCS_Authenticated : Authenticated;
                RASCS_PrepareForCallback : PrepareforCallback;
                RASCS_WaitForModemReset : WaitForModemReset;
                RASCS_Interactive : InteractiveStarted;
                RASCS_RetryAuthentication : RetryAuth;
                RASCS_PasswordExpired : PasswordExpired;
                RASCS_Connected : Connected;
                RASCS_Disconnected : Disconnected;
                RASCS_WaitForCallBack: WaitingForCallBack;
            end;
        end;
    end ;
end ;

// callback function for NT4/W2K only
// 20 July 2011 changed dwCallBackId to Pointer from DWORD
procedure RasDialFunc2 (dwCallBackId: Pointer; dwSubEntry: DWORD; ConnHandle: HRASConn;
        Msg: UINT; RasCS: integer; dwError, dwExtendedError: DWORD); stdcall;
var
    RASOBJ: TMagRasCon absolute ObjRAS ; // cludge because this is a callback ;
begin
    with RASOBJ do
    begin
        fStateEventSource := SourceDial ;
        fStateRasConn := ConnHandle ;  // so we know which connection is being reported
        fStateSubEntry := dwSubEntry ; // ditto
        fConnectError := dwError ;
        fCurDevName := '' ;
        fCurDevType := '' ;
        fLocalPhoneNumber := '' ;
        if dwError <> 0 then
        begin
            fLastError := dwError ;
            fConnectState := fLastError ;    // ANGUS, ensure errors handled
            fStatusStr := GetErrorString (fLastError);
            StateChanged ;            // ANGUS - general catch all
        end
        else
        begin
            fConnectState := RasCS ;
            fStatusStr := GetMessText ;  // get description
            StateChanged ;            // ANGUS - general catch all
        // no other events with this version
        end;
    end ;
end ;

// get numeric status of specified RAS connection

function TMagRasCon.GetConnectStatusEx (ConnHandle: HRasConn; SubEntry: integer): LongInt;  // 3.2 ANGUS
var
    RasConnStatusNT4: TRasConnStatusNT4 ;
begin
    fConnectState := ERROR_INVALID_PORT_HANDLE ;
    fStatusStr := Format (SRasGenBadState, [fConnectState]) ; // Unknown State
    fStateRasConn := ConnHandle ;  // so we know which connection is being reported
    fStateSubEntry := SubEntry ;    // special, so we know it was a status request
    fCurDevName := '' ;
    fCurDevType := '' ;
    fConnectPhoneNr := '' ;
    fConnectError := 0 ;
    fLastError := fConnectState ;
    Result := fConnectState ;
    if (ConnHandle = 0) then Exit;
    if (NOT MagLoadRasApi) then
    begin
        fLastError := ERROR_DLL_NOT_FOUND ;
        result := fLastError ;
        exit ;
    end ;
    FillChar (RasConnStatusNT4, SizeOf(RasConnStatusNT4), #0);
    if MagRasOSVersion = OSW9x then
        RasConnStatusNT4.dwSize := Sizeof (TRasConnStatus)
    else
        RasConnStatusNT4.dwSize := Sizeof (TRasConnStatusNT4);
    fLastError := RasGetConnectStatus (ConnHandle, @RasConnStatusNT4);
    if fLastError = 0 then
    begin
        fCurDevName := RasConnStatusNT4.szDeviceName ;
        fCurDevType := RasConnStatusNT4.szDeviceType ;
        fConnectState := RasConnStatusNT4.RasConnState;
        fConnectError := RasConnStatusNT4.dwError ;
        if RasConnStatusNT4.dwError > Pending then   // ANGUS
                                      fLastError := RasConnStatusNT4.dwError;
        fConnectPhoneNr := RasConnStatusNT4.szPhoneNumber ;  // Angus 3.2, NT4/W2K only
    end;
    if fLastError <> 0 then     // Angus, get more info about failure
         fStatusStr := GetErrorString (fLastError) ;
    Result := fLastError;    // 0 - OK
end;

// get numeric status of currently open RAS connection

function TMagRasCon.GetConnectStatus: LongInt;
begin
    Result := GetConnectStatusEx (fRasConn, 0) ;
end ;

// get ascii status of specified RAS connection

FUNCTION TMagRasCon.CurrentStatusEx (ConnHandle: HRasConn; SubEntry: integer): String;       // 3.2 ANGUS
BEGIN
    GetConnectStatusEx (ConnHandle, SubEntry) ;  // actually makes RasGetConnectStatusEx
    if fLastError = 0 then
        fStatusStr := GetMessText   // success, get connect message
    else
    begin
        if fLastError > Pending then     // 600
            fStatusStr := GetErrorString (fLastError)
        else
        case fLastError of
            6: fStatusStr := SRasGenDisconn; // Disconnected   // bad handle
            8: fStatusStr := SRasGenNoMem ;  // Not enough memory
            Pending: fStatusStr := SRasGenPending ; // Device Connecting/Dialling // better than pending
        end;
    end ;
// if ConnHandle = 0 then Result := 'Not Connected';  ???
    result := fStatusStr ;
    fStateEventSource := SourceStatus ;
    StateChanged ;            // ANGUS - general catch all event
end;

// get ascii status of current RAS connection

FUNCTION TMagRasCon.CurrentStatus: String;
begin
    Result := CurrentStatusEx (fRasConn, 0) ;
end ;

PROCEDURE TMagRasCon.SetPhoneBookPath (Value: AnsiString);
BEGIN
    if (MagRasOSVersion <> OSNT4) then exit ;    // 5.21 only allow path on NT4
    fPhoneBookPath := TrimAnsi (Value) ;
    fPBPtr := Nil ;                                         // 4.60
    if fPhoneBookPath <> '' then
                     fPBPtr := PAnsiChar (fPhoneBookPath) ;     // 4.60
END;

procedure TMagRasCon.SetPBLocation (Index: Integer) ;    // 5.20
begin
    fPBPtr := Nil ;
    if (MagRasOSVersion >= OSW2K) then
    begin
        if Index < MaxPhoneBkFiles then
        begin
            fPBLocation := Index ;
            fPhoneBookPath := MagRasPhoneFiles [Index] ;
            fPBPtr := PAnsiChar (fPhoneBookPath) ;
        end ;
    end ;
end;

PROCEDURE TMagRasCon.Connected;
BEGIN
    If Assigned( fOnConnect ) THEN fOnConnect( Self );
END;

PROCEDURE TMagRasCon.AboutToOpenPort;
BEGIN
    If Assigned(fAboutToOpenPort) THEN fAboutToOpenPort (Self);
end;

procedure TMagRasCon.PortOpened;
begin
    If Assigned(fPortOpened) THEN fPortOpened(Self);
end;

procedure TMagRasCon.AboutToConnDev;
begin
    If Assigned(fAboutToConnDev) THEN fAboutToConnDev (Self);
end;

procedure TMagRasCon.DevConnected;
begin
    If Assigned(fDevConnected) THEN fDevConnected(Self);
end;

procedure TMagRasCon.AllDevsConnected;
begin
    If Assigned(fAllDevsConnected) THEN fAllDevsConnected(Self);
end;

procedure TMagRasCon.Authenticate;
begin
    If Assigned(fAuthenticate) THEN fAuthenticate(Self);
end;

procedure TMagRasCon.AuthNotify;
begin
    If Assigned(fAuthNotify) THEN fAuthNotify(Self);
end;

procedure TMagRasCon.AuthRetry;
begin
    If Assigned(fAuthRetry) THEN fAuthRetry(Self);
end;

procedure TMagRasCon.AuthCallBack;
begin
    If Assigned(fAuthCallBack) THEN fAuthCallBack(Self);
end;

procedure TMagRasCon.AuthChangePassword;
begin
    If Assigned(fAuthChangePassword) THEN fAuthChangePassword(Self);
end;

procedure TMagRasCon.AuthProject;
begin
    If Assigned(fAuthProject) THEN fAuthProject(Self);
end;

procedure TMagRasCon.AuthLinkSpeed;
begin
    If Assigned(fAuthLinkSpeed) THEN fAuthLinkSpeed(Self);
end;

procedure TMagRasCon.AuthAck;
begin
    If Assigned(fAuthAck) THEN fAuthAck(Self);
end;

procedure TMagRasCon.ReAuthenticate;
begin
    If Assigned(fReAuthenticate) THEN fReAuthenticate(Self);
end;

procedure TMagRasCon.Authenticated;
begin
    If Assigned(fAuthenticated) THEN fAuthenticated(Self);
end;

procedure TMagRasCon.PrepareforCallback;
begin
    If Assigned(fPrepareforCallback) THEN fPrepareforCallback(Self);
end;

procedure TMagRasCon.WaitForModemReset;
begin
    If Assigned(fWaitForModemReset) THEN fWaitForModemReset(Self);
end;

procedure TMagRasCon.InteractiveStarted;
begin
    If Assigned(fInteractiveStarted) THEN fInteractiveStarted(Self);
end;

procedure TMagRasCon.RetryAuth;
begin
    If Assigned(fRetryAuth) THEN fRetryAuth(Self);
end;

procedure TMagRasCon.PasswordExpired;
begin
    If Assigned(fPasswordExpired) THEN fPasswordExpired(Self);
end;

procedure TMagRasCon.Disconnected;
var
    RasConnStatus : TRasConnStatus;
    ErrorStr : String;
begin
    If (fRASConn = 0) THEN Exit;   // Angus, not sure why we need this !!!
    If Assigned(fOnDisconnect) THEN
    begin
        fLastError := ERROR_DLL_NOT_FOUND ;
        if (NOT MagLoadRasApi) then exit ;
        FillChar(RASConnStatus, SizeOf(RASConnStatus), #0);
        RASConnStatus.dwSize := Sizeof (RasConnStatus);
        fLastError := RasGetConnectStatus(fRasConn, @RASConnStatus);
        ErrorStr := GetErrorString (fLastError);
        fOnDisconnect(Self,fLastError,ErrorStr);
    end;
end;

procedure TMagRasCon.WaitingForCallBack;
begin
//  If (fRASConn = 0) THEN Exit;
    If Assigned(fOnCallBack) THEN fOnCallBack(Self);
end;

// callback whenever a TAPI event happens on an open line
// not used here since lines never opened

procedure lineCallback (hDevice, dwMsg, dwCallbackInstance,
                            dwParam1, dwParam2, dwParam3: DWORD); stdcall;
begin
    ;
end ;

function TMagRasCon.InitTAPI: boolean ;
var
    errcode: DWORD ;
begin
    result := false ;
    if fTAPIRunning then
    begin
        result := true ;
        exit ;
    end ;
    if NOT LoadTAPI then exit ;
    if fTAPILineApp = 0 then
    begin
        errcode := lineInitialize (fTAPILineApp, HInstance, lineCallback,
                                                    nil, fTAPIModemInst) ;
        if errcode <> 0 then    // 0 is OK
        begin
            fStatusStr := 'TAPI Error' ;
            fTAPILineApp := 0 ;
            exit ;
        end
        else
        begin
            if fTAPIModemInst = 0 then      // no devices
            begin
                lineShutDown (fTAPIlineApp);
                fTAPILineApp := 0;
                exit ;
            end ;
        end ;
    end ;
    fTAPIRunning := true ;
    result := true ;
end ;

// terminate TAPI

procedure TMagRasCon.StopTAPI ;
begin
    if fTAPILineApp <> 0 then
    begin
        lineShutDown (fTAPIlineApp);   // TAPI shutdown
        fTAPIlineApp := 0 ;
        fTAPIRunning := false ;
    end ;
end ;

// display the Translate dialog box

function TMagRasCon.TranslateDialog (parh: HWND; devid:
                                    integer; addr : AnsiString) : Boolean;
begin
    result := false ;
    if NOT fTAPIRunning then
    begin
        if NOT InitTAPI then exit ;
    end ;
    Result := (lineTranslateDialog (fTAPILineApp, devid,
                            TAPI_CURRENT_VERSION, parh, pAnsiChar(Addr)) = 0);
end;

// display the modem configure dialog box

function TMagRasCon.ConfigDialog (parh: HWND; devid: integer) : Boolean;
begin
    Result := (lineConfigDialog (devid, parh, nil) = 0);
end;

// 5.70 ensure at least one dialling location exists,
// to avoid 'Location Information' dialog being displayed
// program needs admin rights

function TMagRasCon.SetDefDialLocation: boolean;
var
    nextid: integer ;
    IniFile: TRegistry ;
const
    RegTelLoc = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Telephony\Locations' ;
    RegTelNextId = 'NextID' ;
    RegTelCurId = 'CurrentID' ;
    RegTelLoc1 = '\Location1' ;
begin
    result := false ;
    if MagRasOSVersion < OSNT4 then exit ;
    IniFile := TRegistry.Create ;
    try
        with IniFile do
        begin
            try
                nextid := 0 ;
                RootKey := HKEY_LOCAL_MACHINE;
                if OpenKey (RegTelLoc, false) then
                begin
                    if ValueExists (RegTelNextId) then nextid := ReadInteger (RegTelNextId) ;
                    if nextid = 0 then   // no locations yet, create one
                    begin
                        WriteInteger (RegTelCurId, 1) ; // will fail unless Admistrator
                        WriteInteger (RegTelNextId, 2) ;
                        CloseKey ;
                        if OpenKey (RegTelLoc + RegTelLoc1, true) then
                        begin
                            WriteInteger ('Country', fDefCountryId) ;
                            WriteInteger ('Flags', 3) ;   // 3 seems to be tone dial, but undocumented in TAPI LINELOCATIONOPTION_xx
                            WriteString ('Name', 'My Location') ;
                            WriteString ('AreaCode', '') ;
                            WriteString ('DisableCallWaiting', '') ;
                            WriteString ('LongDistanceCarrierCode', '') ;
                            WriteString ('InternationalCarrierCode', '') ;
                            WriteString ('LongDistanceAccess', '') ;
                            WriteString ('OutsideAccess', '') ;
                            CloseKey ;
                            result := true ;
                        end ;
                    end
                    else
                        result := true ;
                end ;
            except
                CloseKey ;
            end ;
        end ;
    finally
        IniFile.Free ;
    end ;

end ;

// get dialling properties, locations, defaults and country information

function TMagRasCon.GetTransCaps (var DialProps: TDialProps;
        var DialLocation: TDialLocation; var DialCard: TDialCard;
                                var DialCountry: TDialCountry): integer ;
var
    LineTranslateCaps: TLineTranslateCaps ;
    LineLocationEntry: LPLineLocationEntry ;
    LineCardEntry: LPLineCardEntry ;
    LineCountryList: TLineCountryList ;
    LineCountryEntry: LPLineCountryEntry ;
    I, offset, totcountry: integer ;
begin
    result := -1 ;
    DialProps.NumLocations := 0 ;
    DialProps.NumCards := 0 ;
    DialLocation.PermLocationID := $FFFFFFFF ;
    DialLocation.LocationName := '' ;
    DialLocation.CountryId := 0 ;
    DialCard.PermCardId := $FFFFFFFF ; ;
    DialCard.CardName := '' ;
    DialCountry.CountryId := 0 ;
    DialCountry.CountryCode := 0 ;
    DialCountry.CountryName := '' ;
    if NOT fTAPIRunning then
    begin
        if NOT InitTAPI then exit ;
    end ;
    try

// 5.50 ensure at least one location exists, to avoid 'Location Information' dialog being displayed
    SetDefDialLocation ;  // 5.70 moved code to separate function

// dialling properties first, with current location and calling card
    FillChar(LineTranslateCaps, SizeOf(LineTranslateCaps), #0);
    LineTranslateCaps.dwTotalSize := SizeOf (LineTranslateCaps) ;
    result := lineGetTranslateCaps (fTAPILineApp, TAPI_CURRENT_VERSION,
                                                      @LineTranslateCaps) ;
    if result = 0 then
    begin
        with DialProps, LineTranslateCaps do
        begin
            NumLocations := dwNumLocations ;
            CurrentLocationId := dwCurrentLocationID ;
            NumCards := dwNumCards ;
            CurrentPreferredCardId := dwCurrentPreferredCardID ;

        // get default location information
            if (NumLocations > 0) and (dwLocationListSize > 0) then
            begin
                offset := dwLocationListOffset ;
                for I := 0 to Pred (dwNumLocations) do
                    begin
                    pointer (LineLocationEntry) := @Data [offset] ;
                    with DialLocation, LineLocationEntry^ do
                    begin
                        PermLocationID := dwPermanentLocationID ;
                        LocationName := Trim (String (FixedToPasStr (@Data [dwLocationNameOffset], dwLocationNameSize)));  // 9 Aug 2010
                        CountryCode := dwCountryCode ;
                        CityCode := Trim (String (FixedToPasStr (@Data [dwCityCodeOffset], dwCityCodeSize))); // 9 Aug 2010
                        PreferredCardID := dwPreferredCardID ;
                        LocalAccessCode := Trim (String (FixedToPasStr (@Data [dwLocalAccessCodeOffset], dwLocalAccessCodeSize))); // 9 Aug 2010
                        LongDistanceCode := Trim (String (FixedToPasStr (@Data [dwLongDistanceAccessCodeOffset], dwLongDistanceAccessCodeSize)));
                        TollPrefixList := Trim (String (FixedToPasStr (@Data [dwTollPrefixListOffset], dwTollPrefixListSize)));  // 9 Aug 2010
                        CountryID := dwCountryID ;
                        Options := dwOptions ;
                        CancelCallWaiting := Trim (String (FixedToPasStr (@Data [dwCancelCallWaitingOffset], dwCancelCallWaitingSize)));  // 9 Aug 2010
                    // found current location, give up
                        if dwCurrentLocationID = dwPermanentLocationID then break ;
                    end ;
                    offset := offset + Sizeof (TLineLocationEntry) ;
                end ;
            end ;

        // get default calling card information
            if (dwNumCards > 0) and (dwCardListSize > 0) then
            begin
                offset := dwCardListOffset ;
                for I := 0 to Pred (dwNumCards) do
                    begin
                    pointer (LineCardEntry) := @Data [offset] ;
                    with DialCard, LineCardEntry^ do
                    begin
                        PermCardId := dwPermanentCardID ;
                        CardName := Trim (String (FixedToPasStr (@Data [dwCardNameOffset], dwCardNameSize))); // 9 Aug 2010
                        CardNumberDigits := dwCardNumberDigits ;
                        SameAreaRule := Trim (String (FixedToPasStr (@Data [dwSameAreaRuleOffset], dwSameAreaRuleSize))); // 9 Aug 2010
                        LongDistanceRule := Trim (String (FixedToPasStr (@Data [dwLongDistanceRuleOffset], dwLongDistanceRuleSize))); // 9 Aug 2010
                        InternationalRule := Trim (String (FixedToPasStr (@Data [dwInternationalRuleOffset], dwInternationalRuleSize))); // 9 Aug 2010
                        Options := dwOptions ;
                    // found current card, give up
                        if dwCurrentPreferredCardID = dwPermanentCardID then break ;
                    end ;
                    offset := offset + Sizeof (TLineCardEntry) ;
                end ;
            end ;
        end ;
    end ;

// now country
    if DialLocation.CountryId <> 0 then
    begin
        FillChar(LineCountryList, SizeOf(LineCountryList), #0);
        LineCountryList.dwTotalSize := SizeOf (LineCountryList) ;
        result := lineGetCountry (DialLocation.CountryId,
                                    TAPI_CURRENT_VERSION, @LineCountryList) ;
        if result = 0 then
        begin
            with DialCountry, LineCountryList do
            begin
                totcountry := dwNumCountries ;
                offset := dwCountryListOffset ;
                for I := 0 to Pred (totcountry) do
                begin
                    pointer (LineCountryEntry) := @Data [offset] ;
                    with LineCountryEntry^ do
                    begin
                        CountryID := dwCountryID ;
                        NextCountryID := dwNextCountryID ;
                        CountryCode := dwCountryCode ;
                        CountryName := Trim (String (FixedToPasStr (@Data [dwCountryNameOffset], dwCountryNameSize))) ;  // 9 Aug 2010
                        SameAreaRule := Trim (String (FixedToPasStr (@Data [dwSameAreaRuleOffset], dwSameAreaRuleSize))); // 9 Aug 2010
                        LongDistanceRule := Trim (String (FixedToPasStr (@Data [dwLongDistanceRuleOffset], dwLongDistanceRuleSize))); // 9 Aug 2010
                        InternationalRule := Trim (String (FixedToPasStr (@Data [dwInternationalRuleOffset], dwInternationalRuleSize))); // 9 Aug 2010
                    // found current card, give up
                        break ;
                    end ;
                    offset := offset + Sizeof (TLineCountryEntry) ;
                end ;

            end ;
        end ;
    end ;
    except
    end ;
end;

// translate a canonical number into displayable and dialling formats
// allow special options such as new calling card, etc - LineTranslateOption_constants

function TMagRasCon.TransAddr (devid: integer; inputnum: AnsiString;
              card, flags: integer ; var disnum, dialnum: AnsiString): Boolean ;
var
    TransOutput: TLineTranslateOutput ;
begin
    result := false ;
    if NOT fTAPIRunning then
    begin
        if NOT InitTAPI then exit ;
    end ;
    disnum := '' ;
    dialnum := '' ;
    if inputnum = '' then exit ;
    TransOutput.dwTotalSize := sizeof (TransOutput.data) ;
    try
    if lineTranslateAddress (fTAPILineApp, devid, TAPI_CURRENT_VERSION,
                           pAnsiChar(inputnum), card, flags, TransOutput) = 0 then
        begin
        with TransOutput do
        begin
            disnum := TrimAnsi (FixedToPasStr (@TransOutput.Data
                    [dwDisplayableStringOffset], dwDisplayableStringSize)) ;
            if length (disnum) > 4 then     // change 0 845 xxx into 0845 xxx
            begin
                if copy (disnum, 1, 2) = '0 ' then delete (disnum, 2, 1) ;
            end ;
            dialnum := TrimAnsi (FixedToPasStr (@TransOutput.Data
                            [dwDialableStringOffset], dwDialableStringSize)) ;
            result := true ;
        end;
    end ;
    except
    end ;
end;

// translate a canonical number into displayable and dialling formats
// no special options

function TMagRasCon.TranslateAddr (devid: integer; inputnum: AnsiString;
                                var disnum, dialnum: AnsiString): Boolean ;
begin
    result := TransAddr (devid, inputnum, 0, 0, disnum, dialnum) ;
end ;

function TMagRasCon.GetSubLocalPhoneNumber (Index: Integer): AnsiString ;
begin
    result := fSubLocalPhoneNumber [Index] ;
end ;

function TMagRasCon.GetSubPhoneCanonical (Index: Integer): AnsiString ;
begin
    result := fSubPhoneCanonical [Index] ;
end ;

function TMagRasCon.GetSubDeviceType (Index: Integer): AnsiString ;
begin
    result := fSubDeviceType [Index] ;
end ;

function TMagRasCon.GetSubDeviceName (Index: Integer): AnsiString ;
begin
    result := fSubDeviceName [Index] ;
end ;

function TMagRasCon.GetSubDevicePort (Index: Integer): AnsiString ;
begin
    result := fSubDevicePort [Index] ;
end ;

Initialization
finalization

end.

