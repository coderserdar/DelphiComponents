unit magrasedt;
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

// -----------------------------------------------------
{
DELPHI RAS COMPONENT - Phonebook Edtiting - Part 3 of 3
(C) 2013 Magenta Systems Ltd

Created by Angus Robertson, Magenta Systems Ltd, England
in 2001, delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

Parts of this component came from unfinished work by Ian Barton.

Compatible with Delphi 7, 2007, 2009, 2010, XE, XE2, XE3, XE4,
supports Windows XP, 2003, Vista, 2008, 7, 8 and 2012,
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
a cost of £100 (UKP) (£117.50 with tax), which is about $200 or 147 euro
less tax.  Developers that license the source may distribute applications
using TMagRas.

Since TMagRasCon does need design time properties, it need not really be
installed on the component palette, but may be created in the application
as needed, as shown in the demo program.
}


(*
Changes in 4.10
Added DefCountryId propertry

Changes in 4.20
Make sure Win9x modem device configuration set correctly
Structures are now available to edit the Win9x modem device info, but
  the properties are not currently being processed

Changes in 4.30
Added Str2IP to convert/validate ascii IP address
Avoided what appears to be a bug in W2K 2195 whereby dialling properties
  were lost after saving an entry and it's subentries - now save a second
  time after doing the subentries, silly but it works.

Changes in 4.40
Conditional compile as TComponent again to allow use as NT service

Changes in 4.41
Corrected Str2IP so that it correctly converts IP addresses with a single
last digit and without trailing spaces (function rewitten).  This problem
meant that PutAllEntryProps sometimes ignored IP addresses, unless they had
trailing spaces (which TMaskEdit added in the demo programs).
Added IsIPStr to check an ASCII representation of an IP address is valid.
Improved validation of IP addresses, stop numbers > 255 and zero first.
Graphics and Control units now conditional as well to ease linking

Changes in 4.50
Literals moved to resource strings for translation in magrasstr.pas

Changes in 4.51
Added TEncryptionType = encryptOptional which is the 'Typical' default in W2K
Added bCustomScript property for W2K

Changes in 4.60
Moved some common functions to MagSubs1 and MagRasEnt
Finally got around to testing PhoneBookPath, simplified some related code

Changes in 4.62
Supporting Windows XP, added 10 new binary and 6 other properties, mostly
  seem to work, but no proper MSDN documentation yet
  (most of the new stuff was in Windows 2000, but never supported by RASAPI32)
Deleting subentries on Windows XP only, if the number of subentries is reduced.
On earlier OS, it's necessary to delete the entry before saving it to delete
  old subentries, but this causes the password to be lost so is not done
  automatically by this component.

Changes in 4.70
Updated help conments for new XP stuff.  Tested Windows XP final build 2600.

Changes in 4.71
On W2K, make sure that both phonebook files exist, creating if necessary
Ensure that path exists before creating empty phonebook file

Changes in 4.90
Added typeBroadband to TPType (WinXP)

Changes in 5.10
Supporting Windows Vista (and maybe Longhorn)
Note: not yet using RasEnty Vista IP6 extensions since not documented in MSDN

Changes in 5.20
Finally tested on Windows Vista RTM and Longhorn (2008 Server) beta 3
PutDialProps and GetDialProps now use NT4 and later RasGet/SetCredentials
APIs instead of RasGet/SetEntryDialProps
New PBLocation property which should be set to REN_AllUsers or REN_User before
  calling most methods, Win2K/XP default to REN_AllUsers, Vista/2008 to REN_User

Changes in 5.21
Ensure phonebook file names are always set before setting PBLocation
Only allow setting PhoneBookPath on NT4 (W2K and later have fixed names)

Changes in 5.30
Added bDefaultCreds property set true before calling PutDialProps or GetDialProps
  to set or get user name and password for 'anyone who uses this computer'

Changes in 5.40
Made compatible with Delphi 2009, but still using ANSI RAS functions, not Unicode
Many functions deliberately use AnsiString for compatability with PAnsiChar APIs

Changes in 5.50
DefCountryId set from LOCALE_ICOUNTRY using GetLcTypeInfo

Changes in 5.60
Fixed cast warnings for Delphi 2009 and later

Changes in 5.70
RegisterComponent now in magrasreg

Changes in 5.71
MagRasOSVersion returns OS7 and OS8 but check >= OSVista


Known Problems
--------------

It does not appear to be possible to get Multilink (ISDN dual channel) info
from the structures under Win95/98, despite DUN 1.2 supporting this stuff.
See Knowledgebase RASAPI32 Does Not Support Multilink on Windows 95/98,
Article ID: Q198777

Under NT4/W2K and later, it's not possible to detect a device is specified for
incoming calls only and saving an entry with such a device returns an
invalid parameters error.  This is primarily an issue with server versions
of Windows where devices are installed as incoming only by default.



------------------------------------------------------------------------ *)

// following define changes TMagRas from being descended from TComponent
// to TCustomControl, allowing use in ActiveX (but stopping use in NT Service)
{.$DEFINE CUSTCNTL}

interface

// Include the resource with the bitmap. Note bitmap
// must have same name as the class
{$R MAGRASEDT.RES}

uses
  SysUtils, Windows, Messages, Classes,
 {$IFDEF CUSTCNTL} Graphics, Controls, {$ENDIF}
  MagRasApi, MagTapiApi, MagRasStr, MagRasEnt, MagSubs1 ;

const
  MagVersion = 'TMagRasEdt, 12th August 2013 - Release 5.72, Copyright 2013, Magenta Systems Ltd' ;

// need some public stuff outside component, so Absolute works
// this means you can (reliably) only use one TMagRasEdt object in your application

var
// W2K RASEntry size is 2,088, XP 2,884, plus space for alternate phone numbers
{ A common buffer used for RasEntryAll }
 EntryEdtInfo: array [0..4095] of AnsiChar ;          // 4.70 Angus, lots of space

{ Size of EntryEdtInfo }
  EntryEdtSize: DWORD ;

{ Used for reading and writing phonebook entries, followed by a variable number
 of alternate locaal numbers.  Current size is set at EntryEdtSize.  It is not
 normally necessary to access this information, it is public primarily for
 debugging purposes.  Used by GetAllEntryProps and PutAllEntryProps.  }
  RasEntryAll: TRasEntryWXP absolute EntryEdtInfo ;

// NT RASSubEntry is 288 (10 * 20) for 10 alternate phone numbers + 2 nulls
{ A common buffer used for RasSubEntryNT4 }
  SubEntryEdtInfo: array [0..490] of AnsiChar ;

{ Size of SubEntryEdtInfo }
  SubEntryEdtSize: DWORD ;

{ Used for reading and writing phonebook sub entries, followed by a variable number
 of alternate locaal numbers.  Current size is set at SubEntrySize.  It is not
 normally necessary to access this information, it is public primarily for
 debugging purposes.  Used by GetAllEntryProps and PutAllEntryProps.  }
  RasSubEntryNT4: TRasSubEntryNT4 absolute SubEntryEdtInfo ;

// TAPI device info, Win9x only - 108 bytes for modem, 126 bytes for ISDN/NDIS
{ A common buffer used for TAPI device inforomation, Win9x only, Used by
  GetAllEntryProps and PutAllEntryProps.  }
  DeviceEdtInfo: array [0..255] of AnsiChar ;       // 4.11 Angus

{ Size of DeviceInfo }
  DeviceEdtSize: DWORD ;                        // 4.11 Angus

{ TAPI device configuration for a Win9x modem only }
  DevCfgEdt: TDevCfg absolute DeviceEdtInfo ;   // 4.11 Angus

type

  TFramingProtocol = (framePPP, frameSLIP, frameNetbeui) ;
  TDialMode = (dialSingle, dialAll, dialAsNeeded) ;
  TEncryptionType = (encryptNone, encrypt40bit, encrypt128bit, encryptOptional) ;

  TPType = (typeUnknown, typePhone, typeVNP, typeDirect, typeInternet, typeBroadband) ;
  TVpnStrategy = (vpnDefault, vpnPptpOnly, vpnPptpFirst, vpnL2tpOnly, vpnL2tpFirst) ;


{$IFDEF CUSTCNTL}
  TMagRasEdt = class(TCustomControl)
{$ELSE}
  TMagRasEdt = class(TComponent)
{$ENDIF}
  private
//   Private declarations
{$IFDEF CUSTCNTL}
    fDesignBitmap : TBitmap;
{$ENDIF}
    fDAXCtrl : Boolean;
    fVersion: string ;

    // general options, all flags set from dwfOptions
    fUseCountryAndAreaCodes     : Boolean;
    fSpecificIPAddress          : Boolean;
    fSpecificNameServers        : Boolean;
    fHeaderCompression          : Boolean;
    fRemoteDefaultGateway       : Boolean;
    fDisableLCPExtensions       : Boolean;
    fTerminalBeforeDial         : Boolean;
    fTerminalAfterDial          : Boolean;
    fModemLights                : Boolean;
    fSoftwareCompression        : Boolean;
    fRequireEncryptedPassword   : Boolean;
    fRequireMSEncryptedPassword : Boolean;
    fRequireDataEncryption      : Boolean;
    fNetworkLogon               : Boolean;
    fUseLogonCredentials        : Boolean;
    fPromoteAlternates          : Boolean;
    fSecureLocalFiles           : Boolean;
    fRequireEAP                 : Boolean;  // NT4
    fRequirePAP                 : Boolean;  // following W2K only
    fRequireSPAP                : Boolean;
    fCustom                     : Boolean;
    fPreviewPhoneNumber         : Boolean;
    fSharedPhoneNumbers         : Boolean;
    fPreviewUserPw              : Boolean;
    fPreviewDomain              : Boolean;
    fShowDialingProgress        : Boolean;
    fRequireCHAP                : Boolean;
    fRequireMsCHAP              : Boolean;
    fRequireMsCHAP2             : Boolean;
    fRequireW95MSCHAP           : Boolean;
    fCustomScript               : Boolean;  // 4.51

    // local and phone numbers
    fCountryCode                : LongInt;
    fCountryID                  : LongInt;
    fAreaCode                   : AnsiString;
    fLocalPhoneNumber           : AnsiString;
    fPhoneCanonical             : AnsiString ;

    // PPP/IP
    fIPAddress                  : AnsiString;
    fDNSAddress                 : AnsiString;
    fDNSAddressAlt              : AnsiString;
    fWINSAddress                : AnsiString;
    fWINSAddressAlt             : AnsiString;

    // Framing - some flags
    fFrameSize                  : LongInt;
    fFramingProtocol            : TFramingProtocol ;

    fNetIPX                     : Boolean;
    fNetBEUI                    : Boolean;
    fNetTCPIP                   : Boolean;

    // Script
    fScript                     : AnsiString;

    // auto dial
    fAutoDialDll                : AnsiString;
    fAutoDialFunc               : AnsiString;

    // device
    fDeviceName                 : AnsiString;
    fDevicePort                 : AnsiString ;     // read only, NT4 only
    fDeviceType                 : AnsiString;

    // X25
    fX25Address                 : AnsiString;
    fX25Facilities              : AnsiString;
    fX25PadType                 : AnsiString;
    fX25UserData                : AnsiString;

   // who knows
    fISDNChannels               : LongInt;

    // Multilink and BAP - NT4 and W2K and later
    fSubEntries                 : integer ;
    fDialMode                   : TDialMode ;
    fDialExtraPercent           : integer;
    fDialExtraSampleSeconds     : integer;
    fHangUpExtraPercent         : integer;
    fHangUpExtraSampleSeconds   : integer;

    // Idle timeout - NT4 and W2K and later
    fIdleDisconnectSeconds      : DWORD ;

    // W2K and later stuff only
    fPType                      : TPType;
    fEncryptionType             : TEncryptionType;
    fCustomAuthKey              : integer ;
    fguidId                     : TGUID ;
    fCustomDialDll              : AnsiString ;
    fVpnStrategy                : TVpnStrategy ;

    // XP dwfOptions2 bit flags
    fSecureFileAndPrint         : Boolean;
    fSecureClientForMSNet       : Boolean;
    fDontNegotiateMultilink     : Boolean;
    fDontUseRasCredentials      : Boolean;
    fUsePreSharedKey            : Boolean;
    fInternet                   : Boolean;
    fDisableNbtOverIP           : Boolean;
    fUseGlobalDeviceSettings    : Boolean;
    fReconnectIfDropped         : Boolean;
    fSharePhoneNumbers          : Boolean;

    // more XP stuff only
    fDnsSuffix                  : AnsiString ;
    fTcpWindowSize              : DWORD ;
    fPrerequisitePbk            : AnsiString ;
    fPrerequisiteEntry          : AnsiString ;
    fRedialCount                : DWORD ;
    fRedialPause                : DWORD ;

    // dial params - NOT in entry properties
    fUserName                   : AnsiString;
    fPassword                   : AnsiString;
    fDomain                     : AnsiString;
    fCallBackNumber             : AnsiString;
//  fSubEntry                   : integer ;  // probably only needed for dialling
//  fCallbackId                 : integer ;  // probably only needed for dialling
    fPasswordFlag               : longbool ;  // 5.20 was fPasswordReturned

    // special stuff
    fPhoneBookPath              : AnsiString;
    RasDialParamsNT4            : TRasDialParamsNT4;
    fDefCountryId               : integer ;     // Angus 4.01
    fAltPhoneNrList             : TStringList;  // Angus 4.01
    fCountryList                : TStringList ; // Angus 4.01
    fCountryIds : array [0..MaxCountries] of integer ;  // maximum likely countries??
    fCountryCodes: array [0..MaxCountries] of integer ;  // maximum likely countries??
    fPBPtr: Pointer ;           // Angus 4.60 Pointer to fPhoneBookPath or Nil
    fPBLocation: integer ;      // Angus 5.20 phone book location, allusers or current user
    fDefaultCreds: boolean ;    // Angus 5.22 set default credentials

    // subrecords
    fSubLocalPhoneNumber: array [1..MaxSubEntries] of AnsiString ; // Angus 4.0
    fSubPhoneCanonical: array [1..MaxSubEntries] of AnsiString ;   // Angus 4.0
    fSubDeviceType: array [1..MaxSubEntries] of AnsiString ;       // Angus 4.0
    fSubDeviceName: array [1..MaxSubEntries] of AnsiString ;       // Angus 4.0
    fSubDevicePort: array [1..MaxSubEntries] of AnsiString ;       // Angus 4.0
    fSubAltPhoneNrList: array [1..MaxSubEntries] of TStringList ;
    fSubCurTotal: integer ;   // number of entries set in arrays or read/write  // Angus 4.0
    fSubInitTotal: integer ;  // number when read only, used to see if deleting any // Angus 4.62

{$IFDEF CUSTCNTL}
    procedure WindowPosChanging (var msg : TWMWindowPosChanging); message
     WM_WINDOWPOSCHANGING;
{$ENDIF}

// real procs to manipulate properties
    procedure GetRASOptions (lngOptions :DWORD);
    function SetRASOptions: DWORD ;
    procedure GetRASOptions2 (Options :DWORD);
    function SetRASOptions2 : DWORD ;
    procedure GetNetProtocols(lngProtocols: DWORD);
    function SetNetProtocols: DWORD;
    procedure GetFramingProtocol (lngProtocol :DWORD);
    function SetFramingProtocol: DWORD ;
    function GetSubLocalPhoneNumber (Index: Integer): AnsiString ;
    procedure SetSubLocalPhoneNumber (Index: Integer; Value: AnsiString) ;
    function GetSubPhoneCanonical (Index: Integer): AnsiString ;
    procedure SetSubPhoneCanonical (Index: Integer; Value: AnsiString) ;
    function GetSubDeviceType (Index: Integer): AnsiString ;
    procedure SetSubDeviceType (Index: Integer; Value: AnsiString) ;
    function GetSubDeviceName (Index: Integer): AnsiString ;
    procedure SetSubDeviceName (Index: Integer; Value: AnsiString) ;
    function GetSubDevicePort (Index: Integer): AnsiString ;
    procedure SetSubDevicePort (Index: Integer; Value: AnsiString) ;
    function GetSubAltPhoneNrList (Index: Integer): TStringList ;
    procedure SetSubAltPhoneNrList (Index: Integer; Value: TStringList) ;
    function GetCountryIds (Index: Integer): integer ;
    function GetCountryCodes (Index: Integer): integer ;
    function GetEntry (EntryName: AnsiString): LongInt ;
    function PutEntry (EntryName: AnsiString): LongInt ;
    procedure SetPhoneBookPath (Value: AnsiString) ;    // 4.60
    procedure SetPBLocation (Index: Integer) ;      // 5.20

  protected
//     Protected declarations
   public
//    Public declarations

// error handling

{ This variable is set by almost all the TMagRasEdt methods that fail.  The
  error code may be converted to a message using GetErrorString. }
    LastError: LongInt;

{ A message describing the last error condition, set by most TMagRasEdt methods. }
    StatusStr: String ;

{ Initialises various lists and functions in TMagRasEdt.

  DefCountryId contains the default country id used for creating new
  entries in the phonebook.   }
    constructor Create(AOwner: TComponent); override;

{ Destroys TMagRasEdt. }
    destructor Destroy; override;

{$IFDEF CUSTCNTL}
    procedure Paint; override;
{$ENDIF}

{ Defaults all the entry properties to standard values, ready to
  create a new phonebook entry.

  Sets UseCountryAndAreaCodes, SpecificIPAddress, SpecificNameServers,
  HeaderCompression, RemoteDefaultGateway, DisableLCPExtensions,
  TerminalBeforeDial, TerminalAfterDial, ModemLights, SoftwareCompression,
  RequireEncryptedPassword, RequireMSEncryptedPassword, RequireDataEncryption,
  NetworkLogon, UseLogonCredentials, PromoteAlternates, SecureLocalFiles,
  RequireEAP, RequirePAP, RequireSPAP, Custom, PreviewPhoneNumber,
  SharedPhoneNumbers, PreviewUserPw, PreviewDomain, ShowDialingProgress,
  RequireCHAP, RequireMsCHAP, RequireMsCHAP2, RequireW95MSCHAP,
  bSecureFileAndPrint, bSecureClientForMSNet, bDontNegotiateMultilink,
  bDontUseRasCredentials, bUsePreSharedKey, bInternet, bDisableNbtOverIP,
  bUseGlobalDeviceSettings, bReconnectIfDropped, bSharePhoneNumbers to false.

  Clears CountryCode, CountryID, AreaCode, LocalPhoneNumber, PhoneCanonical,
  IPAddress, DNSAddress, DNSAddressAlt, WINSAddress, WINSAddressAlt,
  FrameSize, NetIPX, NetBEUI, NetTCPIP, Script, AutoDialDll, AutoDialFunc,
  DeviceName, DevicePort, DeviceType, X25Address, X25Facilities, X25PadType,
  X25UserData, ISDNChannels, SubEntries, DialExtraPercent,
  DialExtraSampleSeconds, HangUpExtraPercent, HangUpExtraSampleSeconds,
  UserName, Password, PasswordReturned, Domain, CallBackNumber, CustomAuthKey,
  CustomDialDll, AltPhoneNrList, SubLocalPhoneNumber, SubPhoneCanonical,
  SubDeviceType, SubDeviceName, SubDevicePort, SubAltPhoneNrList,
  SubCurTotal, guidId, DnsSuffix, TcpWindowSize, PrerequisitePbk,
  PrerequisiteEntry, RedialCount, RedialPause.

  Sets FramingProtocol to framePPP, DialMode to dialAll,
  IdleDisconnectSeconds to RASIDS_Disabled, PType to typePhone,
  EncryptionType to encryptNone, VpnStrategy to vpnDefault.   }
    procedure DefaultProps ;

{ Defaults all the entry properties to standard values, ready to create
  a new PPP phonebook entry.

  Calls DefaultProps and sets all the same properties, except NetTCPIP,
  HeaderCompression and RemoteDefaultGateway are true, CountryId and
  CountryCode are set to DefCountryId.   }
    procedure PPPDefault ;

{ Encapsulates the RasGetErrorString function, to convert a RAS error code
  to a message.  If the error code is not from RAS (ie 600 to 782), a windows
  system error message is returned instead.  }
    function GetErrorString (ErrorCode: LongInt): String;

{ Check if RASAPI32.DLL is available without calling any functions. Not
  all PCs have RAS installed, and it's better to not to call such functions
  if they are likely to fail.

  After calling this method, the MagRasOSVersion variable will indicate the
  current OS as: OSW9x, OSNT4, OSW2K or OSWXP.  Some RAS properties are only
  available on NT4 and/or W2K and/or WXP.  }
    function TestRas: boolean ;

{ Convert a RASIP address into a ASCII representation (ie 127.0.0.1).

  IPAddr is the binary IP address.

  Result is string with an ASCII version of the IP address.}
    function IPToStr (IPAddr: TRASIPAddr): AnsiString;

{ Check if an ASCII representation of an IP address (ie 127.0.0.1) is valid.

  strIP is the ASCII version of the IP address.

  Result is true if a valid IP address was found.

  Calls Str2IP. }
    function IsIPStr (strIP: AnsiString): boolean ;

{ Check if an ASCII representation of an IP address (ie 127.0.0.1) is valid,
  and return a correctly formatted version, ie removing leading zeros, etc.

  strIP is the ASCII version of the IP address.

  Result is true if a valid IP address was found.

  Calls Str2IP. }
    function IsFmtIPStr (var strIP: AnsiString): boolean ;

{ Convert an ASCII representation of an IP address (ie 127.0.0.1) into
  a RASIP address, without any error handling.

  strIP is the ASCII version of the IP address.

  Result is the binary IP address, or 0 is there is an error.

  Calls Str2IP. }
    function StrToIP (strIP: AnsiString): TRASIPAddr;

{ Convert an ASCII representation of an IP address (ie 127.0.0.1) into
  a RASIP address, with error handling.

  strIP is the ASCII version of the IP address.

  Result is true if a valid IP address was converted.

  IPAddr is the binary IP address. }
    function Str2IP (strIP: AnsiString; var IPAddr: TRASIPAddr): boolean ;

{ Encapsulates the RasGetEntryProperties and RasGetSubEntryProperties functions
  to retrieve all properties of a main phonebook entry and any sub entries (for
  multilink connections).

  Note that this method returns returns all properties, there is another
  method in TMagRasCon with a very limited range of properties.

  Also note that on Windows 9x, the RAS APIs were never properly updated by
  Microsoft for multilink calls, so no sub entries are available, although
  TotSubEntries may be set to 2 to indicate multilink is being used.

  EntryName is the phonebook entry to access.  The PhoneBookPath property
  may be optionally set.

  Result is 0 if successful, or an error code.

  Properties set are bUseCountryAndAreaCodes, bSpecificIPAddress,
  bSpecificNameServers, bHeaderCompression, bRemoteDefaultGateway,
  bDisableLCPExtensions, bTerminalBeforeDial, bTerminalAfterDial, bModemLights,
  bSoftwareCompression, bRequireEncryptedPassword, bRequireMSEncryptedPassword,
  bRequireDataEncryption, bNetworkLogon, bUseLogonCredentials, bPromoteAlternates,
  bSecureLocalFiles, bRequireEAP, bRequirePAP, bRequireSPAP, bCustom,
  bPreviewPhoneNumber, bSharedPhoneNumbers, bPreviewUserPw, bPreviewDomain,
  bShowDialingProgress, bRequireCHAP, bRequireMsCHAP, bRequireMsCHAP2,
  bRequireW95MSCHAP, CountryCode, CountryID, AreaCode, LocalPhoneNumber,
  PhoneCanonical, IPAddress, DNSAddress, DNSAddressAlt, WINSAddress,
  WINSAddressAlt, FrameSize, bNetIPX, bNetBEUI, bNetTCPIP, Script, AutoDialDll,
  AutoDialFunc, DeviceName, DevicePort, DeviceType, X25Address, X25Facilities,
  X25PadType,  X25UserData, SubEntries, DialExtraPercent,
  DialExtraSampleSeconds, HangUpExtraPercent, HangUpExtraSampleSeconds,
  FramingProtocol, DialMode, IdleDisconnectSeconds, PType, EncryptionType,
  VpnStrategy, CustomAuthKey, CustomDialDll, bSecureFileAndPrint,
  bSecureClientForMSNet, bDontNegotiateMultilink, bDontUseRasCredentials,
  bUsePreSharedKey, bInternet, bDisableNbtOverIP, bUseGlobalDeviceSettings,
  bReconnectIfDropped, bSharePhoneNumbers, DnsSuffix, TcpWindowSize,
  PrerequisitePbk, PrerequisiteEntry, RedialCount, RedialPause, AltPhoneNrList
  (TStringList), TotSubEntries, SubCurTotal, SubDeviceType (array),
  SubDeviceName (array), SubLocalPhoneNumber (array), SubAltPhoneNrList
  (TStringList) and SubPhoneCanonical (array).  The subentry arrays are of
  size SubCurTotal.

  Note that some of these properties are platform specific, alternate
  phone numbers and sub entries are not supported on Win9x, dial on demand
  properties are W2K and later only.

  Calls GetSubEntryProps.  }
    function GetAllEntryProps (EntryName: AnsiString): LongInt ;

{ The canonical number forrmat is '+44 (845) 123456' or
  '+country code (area code) local number'.  A canonical number may be
  missing the + in which case it will not be processed by dialling properties.
  The canonical number is passed to TranslateAddr or TransAddr to translate
  the number in a dialable number with optional calling card, etc }
    function GetCanonical (UseCountryAndAreaCodes: boolean;
            CountryCode: DWORD; AreaCode, LocalPhoneNumber: AnsiString): AnsiString ;

{ Splits a canonical phone number into the properties  CountryCode, AreaCode,
  LocalPhoneNumber, UseCountryAndAreaCodes.

  The canonical number forrmat is '+44 (845) 123456' or
  '+country code (area code) local number'. }
    procedure SetCanonical (PhoneCanonical: AnsiString) ;

{ Encapsulates the RasSetEntryProperties and RasSetSubEntryProperties
  function to change the connection information for an entry in the phonebook
  or create a new entry.

  EntryName is the phonebook entry to access.  The PhoneBookPath property
  may be optionally set.

  Properties used are bUseCountryAndAreaCodes, bSpecificIPAddress,
  bSpecificNameServers, bHeaderCompression, bRemoteDefaultGateway,
  bDisableLCPExtensions, bTerminalBeforeDial, bTerminalAfterDial, bModemLights,
  bSoftwareCompression, bRequireEncryptedPassword, bRequireMSEncryptedPassword,
  bRequireDataEncryption, bNetworkLogon, bUseLogonCredentials, bPromoteAlternates,
  bSecureLocalFiles, bRequireEAP, bRequirePAP, bRequireSPAP, bCustom,
  bPreviewPhoneNumber, bSharedPhoneNumbers, bPreviewUserPw, bPreviewDomain,
  bShowDialingProgress, bRequireCHAP, bRequireMsCHAP, bRequireMsCHAP2,
  bRequireW95MSCHAP, CountryCode, CountryID, AreaCode, LocalPhoneNumber,
  PhoneCanonical, IPAddress, DNSAddress, DNSAddressAlt, WINSAddress,
  WINSAddressAlt, FrameSize, bNetIPX, bNetBEUI, bNetTCPIP, Script, AutoDialDll,
  AutoDialFunc, DeviceName, DevicePort, DeviceType, X25Address, X25Facilities,
  X25PadType,  X25UserData, SubEntries, DialExtraPercent,
  DialExtraSampleSeconds, HangUpExtraPercent, HangUpExtraSampleSeconds,
  FramingProtocol, DialMode, IdleDisconnectSeconds, PType, EncryptionType,
  VpnStrategy, CustomAuthKey, CustomDialDll, bSecureFileAndPrint,
  bSecureClientForMSNet, bDontNegotiateMultilink, bDontUseRasCredentials,
  bUsePreSharedKey, bInternet, bDisableNbtOverIP, bUseGlobalDeviceSettings,
  bReconnectIfDropped, bSharePhoneNumbers, DnsSuffix, TcpWindowSize,
  PrerequisitePbk, PrerequisiteEntry, RedialCount, RedialPause, AltPhoneNrList
  (TStringList), TotSubEntries, SubCurTotal, SubDeviceType (array),
  SubDeviceName (array), SubLocalPhoneNumber (array), SubAltPhoneNrList
  (TStringList) and SubPhoneCanonical (array).  The subentry arrays are of
  size SubCurTotal.

  Note that the entry will only be saved correctly if it has a valid
  CountryId, DeviceName and DeviceType, and if IP addresses have been
  logically specified (ie if SpecificIPAddress is true a valid IP address
  is required).

  Note that some of these properties are platform specific, alternate
  phone numbers and sub entries are not supported on Win9x, dial on demand
  properties are W2K and later only.

  Result is 0 if successful, or an error code.

  Calls PutSubEntryProps.  }
    function PutAllEntryProps (EntryName: AnsiString): LongInt ;

{ Encapsulates the RasGetSubEntryProperties function to retrieve properties
  of  sub entries in a phonebook entry.  This is not called by applications,
  only by GetAllEntryProperties. }
    function GetSubEntryProps (EntryName: AnsiString;
                                            SubEntry: integer): LongInt ;
 
{ Encapsulates the RasSetSubEntryProperties function to set properties
  of  sub entries in a phonebook entry.  This is not called by applications,
  only by PutAllEntryProperties. }
    function PutSubEntryProps (EntryName: AnsiString;
                                            SubEntry: integer): LongInt ;

{ Encapsulates the RasGetEntryDialParams function to retrieve connection
  information

  EntryName is the phonebook entry to access.  The PhoneBookPath property
  may be optionally set.

  Result is 0 if successful, or an error code.

  Properties set are CallbackNumber, UserName, PassWord, PasswordReturned,
  and Domain. }
    function GetDialProps (EntryName: AnsiString): LongInt ;

{ Encapsulates the RasSetEntryDialParams function to save connection
  information for a specified phonebook entry.

  EntryName is the phonebook entry to access.  The CallbackNumber, UserName,
  PassWord and Domain properties must be set before the PutDialProps method
  is used. PhoneBookPath may be optionally set.  Note that a password can
  not be removed at present.

  Result is 0 if successful, or an error code.  }
    function PutDialProps (EntryName: AnsiString): LongInt ;

{ Encapsulates the RasGetCountryInfo function to retrieve all country
  specific dialing information from the Windows Telephony list of countries.

  Result is 0 if successful, or an error code.

  Returns CountryList (TStringList), CountryCodes (array) and CountryIds
  (array).  Do not sort CountryList otherwise it will no longer match the
  arrays.}
    function GetAllCountryInfo : integer;

// run time properties

{ A list of alternate phone numbers that RAS dials in the order listed if the
  primary number (LocalPhoneNumber) fails to connect.  Note these numbers are
  alternatives for LocalPhoneNumber (but exclude LocalPhoneNumber) so they
  need to be combined into a canonical number for dialling.}
    PROPERTY AltPhoneNrList: TStringList   read fAltPhoneNrList write fAltPhoneNrList ;   // Angus 4.01

{ Specifies a telephone number for a sub entry in a phonebook entry.

  Note that SubLocalPhoneNumber for the first sub entry must be the same as
  LocalPhoneNumber in the main entry.  But it is not necessary to specify
  SubLocalPhoneNumber since the LocalPhoneNumber will be used instead for
  all sub entries.}
    PROPERTY SubLocalPhoneNumber [index: integer]: AnsiString read GetSubLocalPhoneNumber write SetSubLocalPhoneNumber ;

{ Specifies the canonical phone number for a sub entry in a phonebook entry.
  This is created using GetCanonical from the properties UseCountryAndAreaCodes,
  CountryCode, AreaCode and sub entry LocalPhoneNumber.}
    PROPERTY SubPhoneCanonical [index: integer]: AnsiString read GetSubPhoneCanonical write SetSubPhoneCanonical ;

{ Specifies the RAS device type for DeviceName for a sub entry in a phonebook.
  This member can be one of the following constants:
  RASDT_Modem  - A modem accessed through a COM port.
  RASDT_Isdn - An ISDN card with corresponding NDISWAN driver installed.
  RASDT_X25 - An X.25 card with corresponding NDISWAN driver installed.
  RASDT_Vpn  - A virtual private network connection.}
    PROPERTY SubDeviceType [index: integer]: AnsiString read GetSubDeviceType write SetSubDeviceType ;

{ Specifies the name of a TAPI device use with a sub entry in a phonebook
  entry.}
    PROPERTY SubDeviceName [index: integer]: AnsiString read GetSubDeviceName write SetSubDeviceName ;

{ Specifies the com port of a TAPI device use for a sub entry in a phonebook
  entry, NT4 only. }
    PROPERTY SubDevicePort [index: integer]: AnsiString read GetSubDevicePort write SetSubDevicePort ;

{ A list of alternate phone numbers that RAS dials in the order listed if the
  primary number (SubLocalPhoneNumber) fails to connect. }
    PROPERTY SubAltPhoneNrList [index: integer]: TStringList read GetSubAltPhoneNrList write SetSubAltPhoneNrList ;

{ Specifies the number of sub entries in a phonebook entry, NT4 and W2K and later only. }
    PROPERTY SubCurTotal: integer    read fSubCurTotal write fSubCurTotal ; // Angus 4.01

{ Completed by GetAllCountryInfo, an unsorted list of all the countries
  defined in TAPI. The associated properties CountryIds and CountryCodes
  contain further country information. }
    PROPERTY CountryList: TStringList   read fCountryList ;   // Angus 4.01

{ Completed by GetAllCountryInfo, an unsorted list of all the country ids
  defined in TAPI, see the CountryList property. }
  PROPERTY CountryIds [index: integer]: integer read GetCountryIds ;      // Angus 4.01

{ Completed by GetAllCountryInfo, an unsorted list of all the country codes
  defined in TAPI, see the CountryList property. }
    PROPERTY CountryCodes [index: integer]: integer read GetCountryCodes ;  // Angus 4.01

  published
// Published declarations

{ Used for ActiveX only }
    Property DAXCtrl : Boolean    read FDAXCtrl write FDAXCtrl default False;

{ The version of TMagRasEdt - note is only only made writable so it displays
  in the object inspector }
    Property Version: string      read fVersion write fversion stored False;

// general options, all flags set from dwfOptions bitmap

{ Specifies if the CountryID, CountryCode and AreaCode properties are used
  to construct the phone number for a phonebook entry. If not set, these
  properties are ignored.  This corresponds to the Use Country and Area Codes
  check boxes in the Phone dialog box. }
    property bUseCountryAndAreaCodes : Boolean      read fUseCountryAndAreaCodes write fUseCountryAndAreaCodes;

{ Specifies that RAS should try to use the IP address specified by IPAddress
  as the IP address for the dial-up connection. If not set, the value of
  IPAddress is ignored. This corresponds to selecting the
  Specify an IP Address setting in the TCP/IP settings dialog box.
  Clearing it corresponds to selecting the Server Assigned IP Address
  setting.  Currently, an IP address set in the phonebook entry properties
  or retrieved from a server overrides the IP address set in the network
  control panel. }
    property bSpecificIPAddress : Boolean           read fSpecificIPAddress write fSpecificIPAddress;

{ Specifies that RAS uses DNSAddress, DNSAddressAlt, WINSAddress and
  WINSAddressAlt to specify the name server addresses for the dial-up
  connection. If not set, RAS ignores these properties.
  Setting this corresponds to selecting the Specify Name Server Addresses
  setting in the TCP/IP Settings dialog box. Clearing it corrresponds to
  selecting the Server Assigned Name Server Addresses. }
    property bSpecificNameServers : Boolean         read fSpecificNameServers write fSpecificNameServers;

{ Specifies that RAS negotiates to use IP header compression on PPP
  connections. If not set, IP header compression is not negotiated.
  This corresponds to the Use IP Header Compression check box in the
  TCP/IP settings dialog box. It is generally advisable to set this flag
  because IP header compression significantly improves performance. The
  flag should be cleared only when connecting to a server that does not
  correctly negotiate IP header compression.}
    property bHeaderCompression : Boolean           read fHeaderCompression write fHeaderCompression;

{ Specifies that the default route for IP packets is through the dial-up
  adapter when the connection is active. If not set, the default route
  is not modified. This flag corresponds to the Use Default Gateway on
  Remote Network check box in the TCP/IP settings dialog box.}
    property bRemoteDefaultGateway : Boolean        read fRemoteDefaultGateway write fRemoteDefaultGateway;

{ Specifies that RAS disables the PPP LCP extensions defined in RFC 1570.
  This may be necessary to connect to certain older PPP implementations,
  but interferes with features such as server callback. Do not set unless
  specifically required. }
    property bDisableLCPExtensions : Boolean        read fDisableLCPExtensions write fDisableLCPExtensions;

{ Specifies that RAS displays a terminal window for user input before
  dialing the connection. }
    property bTerminalBeforeDial : Boolean          read fTerminalBeforeDial write fTerminalBeforeDial;

{ Specifies that RAS displays a terminal window for user input after dialing
  the connection. Do not set if a dial-up networking script is to be
  associated with the connection, because scripting has its own terminal
  implementation. }
    property bTerminalAfterDial : Boolean           read fTerminalAfterDial write fTerminalAfterDial;

{ Specifies that a status monitor will be displayed in the Task Bar, W2K and later only. }
    property bModemLights : Boolean                 read fModemLights write fModemLights;

{ Specifies that software compression is negotiated on the link. Setting it
  causes the PPP driver to attempt to negotiate CCP with the server. Should
  be set by default, but clearing it can reduce the negotiation period if
  the server does not support a compatible compression protocol.}
    property bSoftwareCompression : Boolean         read fSoftwareCompression write fSoftwareCompression;

{ Specifies that only secure password schemes can be used to authenticate
  the client with the server. This prevents the PPP driver from using the
  PAP plain-text authentication protocol to authenticate the client. The
  CHAP and SPAP authentication protocols are also supported. Clear for
  increased interoperability, and set for increased security.
  This corresponds to the Require Encrypted Password check box in the
  Security dialog box. See also bRequireMsEncryptedPw. }
    property bRequireEncryptedPassword : Boolean    read fRequireEncryptedPassword write fRequireEncryptedPassword;

{ Specifies that only the Microsoft secure password schemes can be used
  to authenticate the client with the server. This prevents the PPP driver
  from using the PPP plain-text authentication protocol, MD5-CHAP, MS-CHAP,
  or SPAP. Should be cleared for maximum interoperability and should be set
  for maximum security. This takes precedence over bRequireEncryptedPw.
  This corresponds to the Require Microsoft Encrypted Password check
  box in the Security dialog box. See also bRequireDataEncryption. }
    property bRequireMSEncryptedPassword : Boolean  read fRequireMSEncryptedPassword write fRequireMSEncryptedPassword;

{ Specifies that data encryption must be negotiated successfully or the
  connection should be dropped. Ignored unless bRequireMsEncryptedPw is also
  set.  Corresponds to the Require Data Encryption check box in the
  Security dialog box.}
    property bRequireDataEncryption : Boolean       read fRequireDataEncryption write fRequireDataEncryption;

{ Specifies that RAS logs on to the network after the point-to-point
  connection is established, Win9x only.  This option is only used if
  accessing a LAN server to share files and printers.  It is not necessary
  for the internet, and will indeed slow down call negotiation by up to
  one minute.  }
    property bNetworkLogon : Boolean                read fNetworkLogon write fNetworkLogon;

{ Specifies that RAS uses the user name, password, and domain of the
  currently logged-on user when dialing this entry. Ignored unless
  bRequireMsEncryptedPw is also set. Note that this setting is ignored by
  the ConnectEx, where specifying empty strings for UserName and Password
  gives the same result.  Corresponds to the Use Current Username and
  Password check box in the Security dialog box.}
    property bUseLogonCredentials : Boolean         read fUseLogonCredentials write fUseLogonCredentials;

{ Specifies an effect when alternate phone numbers are defined AltPhoneNrList
  for a phonebook entry. If set, an alternate phone number that connects
  successfully becomes the primary phone number, and the current primary
  phone number is moved to the alternate list.  Note this functionality
  is not supported by ConnectEx (etc), it must be done in the application.}
    property bPromoteAlternates : Boolean           read fPromoteAlternates write fPromoteAlternates;

{ Specifies that RAS checks for existing remote file system and remote
  printer bindings before making a connection with this entry. Typically,
  you set this on phonebook entries for public networks to remind users to
  break connections to their private network before connecting to a public
  network, NT4/W2K and later. }
    property bSecureLocalFiles : Boolean            read fSecureLocalFiles write fSecureLocalFiles ;

{ Specifies that an Extensible Authentication Protocol (EAP) must be
  supported for authentication, W2K and later only. }
    property bRequireEAP : Boolean                  read fRequireEAP write fRequireEAP ;

{ Specifies that Password Authentication Protocol must be supported for
  authentication, W2K and later only. }
    property bRequirePAP : Boolean                  read fRequirePAP write fRequirePAP ;

{ Specifies that Shiva's Password Authentication Protocol must be supported
  for authentication, W2K and later only. }
    property bRequireSPAP : Boolean                 read fRequireSPAP write fRequireSPAP ;

{ Specifies that the connection will use custom encryption, W2K and later only. }
    property bCustom : Boolean                      read fCustom write fCustom  ;

{ Specifies that the remote access dialer displays the phone number to be
  dialed, W2K and later only. }
    property bPreviewPhoneNumber : Boolean          read fPreviewPhoneNumber write fPreviewPhoneNumber ;

{ Specifies that phone numbers are shared, W2K and later only. }
    property bSharedPhoneNumbers : Boolean          read fSharedPhoneNumbers write fSharedPhoneNumbers ;

{ Specifies that the remote access dialer displays the user's name and
  password prior to dialing, W2K and later only. }
    property bPreviewUserPw : Boolean               read fPreviewUserPw write fPreviewUserPw ;

{ Specifies that the remote access dialer displays the domain name prior
  to dialing. }
    property bPreviewDomain : Boolean               read fPreviewDomain write fPreviewDomain ;

{ Specifies that the remote access dialer displays its progress in
  establishing the connection, W2K and later only. }
    property bShowDialingProgress : Boolean         read fShowDialingProgress write fShowDialingProgress  ;

{ Specifies that the Challenge Handshake Authentication Protocol must be
  supported for authentication, W2K and later only. }
    property bRequireCHAP : Boolean                 read fRequireCHAP write fRequireCHAP ;

{ Specifies that the Microsoft Challenge Handshake Authentication Protocol
  must be supported for authentication, W2K and later only. }
    property bRequireMsCHAP : Boolean               read fRequireMsCHAP write fRequireMsCHAP ;

{ Specifies that version 2 of the Microsoft Challenge Handshake
  Authentication Protocol must be supported for authentication, W2K and later only. }
    property bRequireMsCHAP2 : Boolean              read fRequireMsCHAP2 write fRequireMsCHAP2 ;

{ Specifies that Windows 9x version of Microsoft Challenge Handshake
  Authentication Protocol must be supported for authentication, W2K and later only. }
    property bRequireW95MSCHAP : Boolean            read fRequireW95MSCHAP write fRequireW95MSCHAP ;

{ Specifies a custom script is used, W2K and later only. }
    property bCustomScript : Boolean            read fCustomScript write fCustomScript ;

    // Location/phone number

{ Specifies the country code portion of the phone number for a phonebook
  entry.  The country code must correspond to the country identifier
  specified by CountryID. If CountryCode is zero, the country code is based
  on the country identifier specified by CountryID.   This is ignored unless
  bUseCountryAndAreaCodes is true.}
    property CountryCode : LongInt          read fCountryCode write fCountryCode;

{ Specifies the TAPI country identifier for a phonebook entry. This is
  ignored unless UseCountryAndAreaCodes is true.  Note this may not be left
  blank when updating a phonebook entry. }
    property CountryID : LongInt            read fCountryID write fCountryID;

{ Specifies the area code for a phonebook entry.  If the dialing location
  does not have an area code, specify an blank. Do not include parentheses
  or other delimiters in the area code string, for example, "206" is a
  valid area code; "(206)" is not. Do not include a trunk access code either,
  for instance 0 in the UK, for example 845 is valid, 0845 is not.  This is
  ignored unless bUseCountryAndAreaCodes is true.}
    property AreaCode : AnsiString              read fAreaCode write fAreaCode ;

{ Specifies a telephone number for a phonebook entry. The way RAS uses this
  string depends on bUseCountryAndAreaCodes. If true, RAS combines
  LocalPhoneNumber with the country and area codes specified by CountryID,
  CountryCode, and AreaCode members. If false, RAS uses LocalPhoneNumber
  as the entire phone number.  }
    property LocalPhoneNumber : AnsiString      read fLocalPhoneNumber write fLocalPhoneNumber;

{ Specifies the canonical phone number for a phonebook entry.  This is
  created using GetCanonical from the properties bUseCountryAndAreaCodes,
  CountryCode, AreaCode and LocalPhoneNumber.  }
    property PhoneCanonical : AnsiString        read fPhoneCanonical write fPhoneCanonical ;

    // PPP/IP

{ Specifies the IP address to be used while this connection is active.
  Ignored unless bSpecificIpAddr set. }
    property IPAddress : AnsiString             read fIPAddress write fIPAddress;

{ Specifies the IP address of the DNS server to be used while this
  connection is active.  Ignored unless bSpecificNameServers is set. }
    property DNSAddress : AnsiString            read fDNSAddress write fDNSAddress;

{ Specifies the IP address of a secondary or backup DNS server to be used
  while this connection is active. Ignored unless bSpecificNameServers is set. }
    property DNSAddressAlt : AnsiString         read fDNSAddressAlt write fDNSAddressAlt;

{ Specifies the IP address of the WINS server to be used while this
  connection is active. Ignored unless bSpecificNameServers is set. }
    property WINSAddress : AnsiString           read fWINSAddress write fWINSAddress;

{ Specifies the IP address of a secondary WINS server to be used while
  this connection is active. Ignored unless bSpecificNameServers is set. }
    property WINSAddressAlt : AnsiString        read fWINSAddressAlt write fWINSAddressAlt;

    //Framing Protocols

{ Specifies the network protocol frame size. Should be either 1006 or 1500.
  Ignored unless FramingProtocol specifies frameSLIP. }
    property FrameSize : LongInt            read fFrameSize write fFrameSize;

{ Specifies the framing protocol used by the server. PPP is the emerging
  standard. SLIP is used mainly in UNIX environments. This can be framePPP
  (Point-to-Point Protocol), frameSLIP (Serial Line Internet Protocol) or
  frameNetbeui (Asynchronous NetBEUI, Microsoft proprietary protocol).
  To use Compressed SLIP, set Slip and set the bIpHeaderCompression.
  With W2K and later frameNetbeui is no longer supported. }
    property FramingProtocol: TFramingProtocol read fFramingProtocol write fFramingProtocol;

{ Specifies negotiate the NetBEUI protocol.  }
    property bNetBEUI : boolean             read fNetBEUI write fNetBEUI;

{ Specifies negotiate the IPX protocol. }
    property bNetIPX : boolean              read fNetIPX write fNetIPX;

{ Specifies negotiate the TCP/IP protocol. }
    property bNetTCPIP : boolean            read fNetTCPIP write fNetTCPIP;


    // Script
{ Specifies the name of the script file. The filename should be a full path.
  With NT4/W2K and later, to indicate a Windows NT SWITCH.INF script name, set
  the first character of the name to "[". }
    property Script : AnsiString                read fScript write fScript;

    // Auto dial
{ Specifies the full path and filename of the dynamic link library (DLL) for
  the customised AutoDial handler  for a phonebook entry. If AutoDialDll
  contains an empty string,  RAS uses the default dialing user interface and
  the AutoDialFunc member is ignored.}
    property AutoDialDLL : AnsiString           read fAutoDialDLL write fAutoDialDll;

{ Specifies the the exported name of the RASADFunc function for the
  customized AutoDial handler. An AutoDial DLL must provide both ANSI and
  Unicode versions of the RASADFunc handler.  However, do not include the
  "A" or "W" suffix in the name specified by AutoDialFunc. }
    property AutoDialFunc : AnsiString          read fAutoDialFunc write fAutoDialFunc;

    // Device
{ Specifies the name of a TAPI device use with a phonebook entry.}
    property DeviceName : AnsiString            read fDeviceName write fDeviceName;

{ Specifies the com port of a TAPI device use with a phonebook entry, NT4
  only, can not be updated.}
    property DevicePort : AnsiString            read fDevicePort ;

{ Specifies the RAS device type for DeviceName. This member can be one of
  the following constants:
  RASDT_Modem  - A modem accessed through a COM port.
  RASDT_Isdn - An ISDN card with corresponding NDISWAN driver installed.
  RASDT_X25 - An X.25 card with corresponding NDISWAN driver installed.
  RASDT_Vpn  - A virtual private network connection. }
    property DeviceType : AnsiString            read fDeviceType write fDeviceType;

    // X25
{ Specifies the X.25 address to connect to. }
    property X25Address : AnsiString            read fX25Address write fX25Address ;

{ Specifies the XX.25 facilities. }
    property X25Facilities : AnsiString         read fX25Facilities write fX25Facilities ;

{ Specifies the X.25 PAD type. }
    property X25PadType : AnsiString            read fX25PadType write fX25PadType ;

{ Specifies additional connection information supplied to the X.25 host
  at connection. }
    property X25UserData : AnsiString           read fX25UserData write fX25UserData ;

   // who knows

{ Probably not used. }
    property ISDNChannels : LongInt     read fISDNChannels write fISDNChannels ;

    // Multilink and BAP
{ Specifies the number of multilink subentries associated with this entry,
  NT4/W2K and later only. }
    property SubEntries: integer            read fSubEntries ;

{ Specifies whether RAS should dial all of this entry's multilink subentries
  when the entry is first connected for a phonebook entry. This property can
  be one of the following values, DialAll (Dial all subentries initially) or
  DialAsNeeded (adjust the number of subentries as bandwidth is needed).  }
    property DialMode: TDialMode            read fDialMode write fDialMode ;

{ Specifies a percent of the total bandwidth available from the currently
  connected subentries. RAS dials an additional subentry when the total
  bandwidth used exceeds DialExtraPercent percent of the available bandwidth
  for at least DialExtraSampleSeconds seconds. Ignored unless DialMode
  is DialAsNeeded, W2K and later only. }
    property DialExtraPercent: integer      read fDialExtraPercent write fDialExtraPercent ;

{ Specifies the number of seconds that current bandwidth usage must exceed
  the threshold specified by DialExtraPercent before RAS dials an additional
  subentry. Ignored unless DialMode is DialAsNeeded, W2K and later only. }
    property DialExtraSampleSeconds: integer    read fDialExtraSampleSeconds write fDialExtraSampleSeconds ;

{ Specifies a percent of the total bandwidth available from the currently
  connected subentries. RAS terminates (hangs up) an existing subentry
  connection when total bandwidth used is less than HangUpExtraPercent percent
  of the available bandwidth for at least HangUpExtraSampleSeconds seconds.
  Ignored unless DialMode is DialAsNeeded, W2K and later only. }
    property HangUpExtraPercent: integer    read fHangUpExtraPercent write fHangUpExtraPercent ;

{ Specifies the number of seconds that current bandwidth usage must be less
  than the threshold specified by HangUpExtraPercent before RAS terminates
  an existing subentry connection. Ignored unless DialMode is DialAsNeeded,
  W2K and later only. }
    property HangUpExtraSampleSeconds: integer  read fHangUpExtraSampleSeconds write fHangUpExtraSampleSeconds ;

// idling

{ Specifies the number of seconds after which the connection is terminated
  due to inactivity. Note that unless the idle timeout is disabled, the
  entire connection is terminated if the connection is idle for the
  specified interval. This member can specify a number of seconds, or
  RASIDS_Disabled (no idle timeout for this connection) or
  RASIDS_UseGlobalValue (use the user preference value as the default).

  Warning, this setting is very unreliable, NT4/W2K and later only. }
    property IdleDisconnectSeconds: DWORD read fIdleDisconnectSeconds write fIdleDisconnectSeconds ;

// W2K and later stuff

{ Specifies the type of phone-book entry. This member can be one of
  typeUnknown, typePhone, typeVNP, typeDirect, typeInternet, W2K and later only. }
    property PType: TPtype                  read fPType write fPType ;

{ Specifies the type of encryption to use for Microsoft Point to Point
  Encryption (MPPE) with the connection, encryptNone, encrypt40bit,
  encrypt128bit.  This doesn't affect how passwords are encrypted. Whether
  passwords are encrypted and how passwords are encrypted is determined
  by the authentication protocol, e.g. PAP, MS-CHAP, EAP. W2K and later only.  }
    property EncryptionType: TEncryptionType    read fEncryptionType write fEncryptionType ;

{ Used for the Extensible Authentication Protocol (EAP). Contains the
  authentication key provided to the EAP vendor, W2K and later only. }
    property CustomAuthKey: integer         read fCustomAuthKey write fCustomAuthKey ;

{ The GUID (Globally Unique IDentifier) that represents this phone-book
  entry.  Not settable.  W2K and later only. }
    property guidId: TGUID                  read fguidId write fguidId ;

{ Specifies the the full path and filename for the DLL that implements
  the custom dialing functions, W2K and later only. }
    property CustomDialDll: AnsiString          read fCustomDialDll write fCustomDialDll  ;

{ Specifies the VPN strategy to use when dialing a VPN connection, as
  vpnDefault, vpnPptpOnly, vpnPptpFirst, vpnL2tpOnly, vpnL2tpFirst.
  Whichever protocol succeeds is tried first in subsequent dialing for
  this entry, W2K and later only. }
    property VpnStrategy: TVpnStrategy      read fVpnStrategy write fVpnStrategy ;

// new XP stuff

{ Specifies that remote users should be prevented from using file and print
  services over the RAS connection. Setting this flag is equivalent to
  clearing the File and Print Sharing for Microsoft Networks checkbox in
  the connection properties dialog box, XP only. }
   property  bSecureFileAndPrint: Boolean       read fSecureFileAndPrint       write fSecureFileAndPrint ;

{ Specifies that remote users should be prevented from logging onto the PC
  over the RAS connection.  Setting this bit flag is equivalent to clearing
  the Client for Microsoft Networks checkbox in the connection properties
  dialog box, XP only. }
   property  bSecureClientForMSNet : Boolean    read fSecureClientForMSNet     write fSecureClientForMSNet ;

{ Specified the default behavior for the RAS client is not to negotiate
  multilink. Setting this flag is equivalent to unchecking the "Negotiate
  multi-link for single-link connection" checkbox in the connection
  properties dialog box, XP only. }
   property  bDontNegotiateMultilink: Boolean   read fDontNegotiateMultilink   write fDontNegotiateMultilink ;

{ Specifies RAS should use the default credentials to access network
  resources, XP only. }
   property  bDontUseRasCredentials: Boolean    read fDontUseRasCredentials    write fDontUseRasCredentials ;

{ Specifies RAS should use a pre-shared key for authentication, XP only. }
   property  bUsePreSharedKey: Boolean          read fUsePreSharedKey          write fUsePreSharedKey ;

{ Specifies that the connection is to the Internet, XP only. }
   property  bInternet: Boolean                 read fInternet                 write fInternet ;

{ Specifies that NBT probing is disabled for this connection, XP only. }
   property  bDisableNbtOverIP: Boolean         read fDisableNbtOverIP         write fDisableNbtOverIP ;

{ Specifies RAS should ignore the device settings specified in the phone-book
  entry. Instead, RAS uses the device settings specified in the modem control
  panel applet, XP only. }
   property  bUseGlobalDeviceSettings: Boolean  read fUseGlobalDeviceSettings  write fUseGlobalDeviceSettings ;

{ Specifies that RAS automatically attempts to re-establish the connection
  if the connection is lost. This flag corresponds to the "Redial if line is
  dropped" checkbox in the Properties sheet for the connection, XP only. }
   property  bReconnectIfDropped: Boolean       read fReconnectIfDropped       write fReconnectIfDropped ;

{ Specifies RAS should use the same set of phone numbers for all subentries.
  RAS uses the set of phone numbers assigned to the first subentry. This
  flag has an effect only in the context of multi-link connections. This
  flag corresponds to the "All devices call the same number" checkbox in
  the Properties sheet for the connection, XP only. }
   property  bSharePhoneNumbers: Boolean        read fSharePhoneNumbers        write fSharePhoneNumbers ;

{ Specifies the Domain Name Service (DNS) suffix for the connection,
  XP only. }
   property  DnsSuffix: AnsiString                  read fDnsSuffix                write fDnsSuffix ;

{ Specifies the TCP window size for all TCP sessions that run over this
  connection. Setting this value can increase the throughput of high
  latency devices such as cellular phones, XP only. }
   property  TcpWindowSize: DWORD               read fTcpWindowSize            write fTcpWindowSize ;

{ Specifies the full path and file name of a phone-book (PBK) file. This
  phone-book file contains the entry specified by the PrerequisiteEntry
  property, used only for VPN connections, XP only. }
   property  PrerequisitePbk: AnsiString            read fPrerequisitePbk          write fPrerequisitePbk ;

{ Specifies a phone-book entry. This entry should exist in the phone-book
  file specified by the PrerequisitePbk property. This entry should be
  dialled by RAS prior to establishing the connection specified by these
  properties, used only for VPN connections, XP only. }
   property  PrerequisiteEntry: AnsiString          read fPrerequisiteEntry        write fPrerequisiteEntry ;

{ Specifies the number of times RAS attempts to redial a connection.
  Note that the RAS component does not support redialling directly, it
  is the application's responsibility to check for connection failures
  and redial if necessary.  XP only. }
   property  RedialCount: DWORD                 read fRedialCount              write fRedialCount ;

{ Specifies the number of seconds to wait between redial attempts, XP only. }
   property  RedialPause: DWORD                 read fRedialPause              write fRedialPause ;

    // special stuff
{ Specifies the full path and filename of a phonebook (.PBK)
 file to be used when dialling a RAS connection (NT4 only).
 If this property is blank, the current default phone-book file is used.
 The default phone-book file is the one selected by the user in the
 User Preferences property sheet of the Dial-Up Networking dialog box.  }
    property PhoneBookPath: AnsiString          read fPhoneBookPath write SetPhoneBookPath ;

{ Specifies the Phone Book Location, either REN_User or REN_AllUsers for W2K
  and later.  For Vista and later, use REN_User unless program has Admin access. }
    PROPERTY PBLocation:  integer           read fPBLocation write SetPBLocation ;  // Angus 5.20

{ Specifies PutDialProps or GetDialProps should to set or get user name and
  password for 'anyone who uses this computer', W2K and later only. }
    property bDefaultCreds : Boolean        read fDefaultCreds write fDefaultCreds ;  // Angus 5.30

{ Specifies the default country Id obtained using LOCALE_IDEFAULTCOUNTRY. }
    property DefCountryId: integer          read fDefCountryId ;        // Angus 4.01

    // dial params - NOT in entry properties
{ Specifies the user's user name to authenticate the user's access to the
  remote access server.}
    property UserName: AnsiString               read fUserName write fUserName;

{ Specifies the user's password to authenticate the user's access to the
  remote access server.  Note that on Windows 2000, the real password is
  is not available, instead a 'handle' comprising 14 asterisks are returned.}
    property Password: AnsiString               read fPassword write fPassword;

{ Completed by GetDialProps, true if a connection entry password returned.
  Set to true if PutDialProps should save a password }      // Angus 5.20
    property PasswordFlag: longbool         read fPasswordFlag write fPasswordFlag ;

{ Specifies the domain on which authentication is to occur. If empty, the
  domain in which the remote access server is a member. An asterisk specifies
  the domain stored in the phonebook for the entry.}
    property Domain: AnsiString                 read fDomain write fDomain;

{ Specifies a callback phone number to be used when dialling a RAS connection.
  If left empty, no callback should not be used.  It is ignored unless the
  user has "Set By Caller" callback permission on the RAS server. An
  asterisk indicates that the number stored in the phonebook should be
  used for callback.}
    property CallbackNumber: AnsiString         read fCallbackNumber write fCallbackNumber;

  end;

//procedure Register;

implementation

//procedure Register;
//begin
//  RegisterComponents('Magenta Systems', [TMagRasEdt]);
//end;

constructor TMagRasEdt.Create(AOwner: TComponent);
var
    I: integer ;
begin
    inherited Create(AOwner);
    fAltPhoneNrList := TStringList.Create ;
    for I := 1 to MaxSubEntries do
                        fSubAltPhoneNrList [I] := TStringList.Create ;
    fCountryList := TStringList.Create ;
    DefaultProps ;
    fVersion := MagVersion ;

    // get default country ID - Angus 4.01
    // changed LOCALE_IDEFAULTCOUNTRY (obsoleted) to LOCALE_ICOUNTRY - Angus 5.50
    fDefCountryId := AscToInt (GetLcTypeInfo (LOCALE_ICOUNTRY)) ;

// try and load API
    if (NOT MagLoadRasApi) then
    begin
        LastError := ERROR_DLL_NOT_FOUND ;
        exit ;
    end ;

// see if creating empty phonebook file (the RAS API is unable to do this !)
// on W2K, create both files, 4.71
// 5.20 warning - on Vista elevated admin rights are needed to create AllUsers phonebook
    for I := 0 to (MaxPhoneBkFiles - 1) do
    begin
        if CharPos (':', MagRasPhoneFiles [I]) <> 2 then continue ;
        if NOT FileExists (String (MagRasPhoneFiles [I])) then  // 9 Aug 2010
        begin
            MagRasPhoneBookInf.Add (' ') ;  // one blank line
            try
                if ForceDirs (ExtractFileDir (String (MagRasPhoneFiles [I]))) then // 4.71  // 9 Aug 2010
                        MagRasPhoneBookInf.SaveToFile (String (MagRasPhoneFiles [I])) ; // 9 Aug 2010
            except
            end ;
            MagRasPhoneBookInf.Clear ;
        end ;
    end ;

// 5.20 default to current user profile on Vista, sets fPhoneBookPath and fPBPtr
    fPhoneBookPath := '' ;   // 4.60
    fPBPtr := Nil ;          // 4.60
    fSubInitTotal := 0 ;     // 4.62
    fDefaultCreds := false ; // 5.30
    MagSetPhoneBkFiles ;     // 5.21
    if MagRasOSVersion >= OSVista then
        PBLocation := REN_User
    else
        PBLocation := REN_AllUsers ;
  // should bDefaultCreds be set true for AllUsers?????

  //Create a bitmap which is used to show the Components
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

destructor TMagRasEdt.Destroy;
var
    I: integer ;
begin
    try
        fAltPhoneNrList.Free ;
        for I := 1 to MaxSubEntries do fSubAltPhoneNrList [I].Free ;
        fCountryList.Free ;
{$IFDEF CUSTCNTL}
    fDesignBitmap.Free;
{$ENDIF}
    except
    end ;
    inherited Destroy;
end;

{$IFDEF CUSTCNTL}
procedure TMagRasEdt.WindowPosChanging (var msg :
  TWMWindowPosChanging);
begin
  //Don't let the user resize the component
  //at design time.
  msg.WindowPos.cx := Width;
  msg.WindowPos.cy := Height
end;

procedure TMagRasEdt.Paint;
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

// set defaults to blanks

procedure TMagRasEdt.DefaultProps ;
const
    blankIP = '0.0.0.0' ;
var
    I: integer ;
begin

// force device configuration to be defaulted by RAS
    DeviceEdtSize := 0 ;

// general options, all flags set from dwfOptions and dwfOptions2
    GetRASOptions (0) ;       // 4.62, easier than setting them one by one
    GetRASOptions2 (0) ;
{    fUseCountryAndAreaCodes := false ;
    fSpecificIPAddress := false ;
    fSpecificNameServers:= false ;
    fHeaderCompression  := false ;
    fRemoteDefaultGateway := false ;
    fDisableLCPExtensions := false ;
    fTerminalBeforeDial := false ;
    fTerminalAfterDial := false ;
    fModemLights := false ;
    fSoftwareCompression := false ;
    fRequireEncryptedPassword := false ;
    fRequireMSEncryptedPassword := false ;
    fRequireDataEncryption := false ;
    fNetworkLogon := false ;
    fUseLogonCredentials := false ;
    fPromoteAlternates := false ;
    fSecureLocalFiles := false ;
    fRequireEAP := false ;
    fRequirePAP := false ;
    fRequireSPAP := false ;
    fCustom := false ;
    fPreviewPhoneNumber := false ;
    fSharedPhoneNumbers := false ;
    fPreviewUserPw := false ;
    fPreviewDomain := false ;
    fShowDialingProgress := false ;
    fRequireCHAP := false ;
    fRequireMsCHAP := false ;
    fRequireMsCHAP2 := false ;
    fRequireW95MSCHAP := false ;
    fCustomScript := false ;     }

    // local and phone numbers
    fCountryCode := 0;
    fCountryID  := 0;
    fAreaCode := '';
    fLocalPhoneNumber := '';
    fPhoneCanonical := '' ;

    // PPP/IP
    fIPAddress := blankIP ;
    fDNSAddress  := blankIP ;
    fDNSAddressAlt  := blankIP ;
    fWINSAddress  := blankIP ;
    fWINSAddressAlt := blankIP ;

    // Framing
    fFrameSize := 0 ;   //  1500 or 1006
    fFramingProtocol := framePPP ;  // must be one set
    fNetIPX := false;
    fNetBEUI := false;
    fNetTCPIP := false;

    // script
    fScript := '';

    // auto dial
    fAutoDialDll := '';
    fAutoDialFunc := '';

    // device
    fDeviceName := '';
    fDevicePort := '' ;
    fDeviceType := '';

    // X25
    fX25Address := '' ;
    fX25Facilities := '' ;
    fX25PadType := '' ;
    fX25UserData := '' ;

    // ??
    fISDNChannels := 0;

    // Multilink and BAP - NT4 and W2K and later
    fSubEntries := 0;
    fDialMode := dialAll;
    fDialExtraPercent   := 0;
    fDialExtraSampleSeconds := 0;
    fHangUpExtraPercent := 0;
    fHangUpExtraSampleSeconds := 0;

    // Idle timeout - NT4 and W2K and later
    fIdleDisconnectSeconds  := RASIDS_Disabled;

    // W2K and later stuff only
    fPType := typePhone;
    fEncryptionType := encryptOptional;  // 4.51, shows up in W2K DUN as 'Typical'
    fCustomAuthKey := 0;
    FillChar (fguidId, SizeOf (fguidId), #0);
    fCustomDialDll := '';
    fVpnStrategy := vpnDefault;

    // XP stuff
    fDnsSuffix := '' ;
    fTcpWindowSize := 0 ;
    fPrerequisitePbk := '' ;
    fPrerequisiteEntry := '' ;
    fRedialCount := 0 ;
    fRedialPause := 0 ;

    // dial params
    fUserName := '';
    fPassword := '';
    fPasswordFlag := False;
    fDomain := '';
    fCallBackNumber := '';
//  fSubEntry := 0 ;
//  fCallbackId := 0 ;

    // misc
    fAltPhoneNrList.Clear ;

    // subentries
    for I := 1 to MaxSubEntries do
    begin
        fSubLocalPhoneNumber [I] := '' ;
        fSubPhoneCanonical [I] := '' ;
        fSubDeviceType [I] := '' ;
        fSubDeviceName [I] := '' ;
        fSubDevicePort [I] := '' ;
        fSubAltPhoneNrList [I].Clear ;
    end ;
    fSubCurTotal := 0 ;
end;

// set defaults to minimal PPP connection

procedure TMagRasEdt.PPPDefault ;
begin
    DefaultProps ;
    fNetTCPIP := true;
    fHeaderCompression := true ;
    fRemoteDefaultGateway := true ;
    fCountryId := fDefCountryId ;
    fCountryCode := fDefCountryId ;  // cheating, different for some countries

    // 5.20 default to current user profile on Vista, sets fPhoneBookPath and fPBPtr
    fPhoneBookPath := '' ;
    if MagRasOSVersion >= OSVista then
        PBLocation := REN_User
    else
        PBLocation := REN_AllUsers ;
end ;

procedure TMagRasEdt.GetRASOptions (lngOptions: DWORD);
begin
    if lngOptions And RASEO_UseCountryAndAreaCodes
                                    = RASEO_UseCountryAndAreaCodes then
        fUseCountryAndAreaCodes  := True
    else
        fUseCountryAndAreaCodes  := False;
    if lngOptions And RASEO_SpecificIpAddr = RASEO_SpecificIpAddr then
        fSpecificIPAddress := True
    else
        fSpecificIPAddress := False;
    if lngOptions And RASEO_SpecificNameServers = RASEO_SpecificNameServers then
        fSpecificNameServers := True
    else
        fSpecificNameServers := False;
    if lngOptions And RASEO_IpHeaderCompression = RASEO_IpHeaderCompression then
        fHeaderCompression := True
    else
        fHeaderCompression := False;
    if lngOptions And RASEO_RemoteDefaultGateway = RASEO_RemoteDefaultGateway then
        fRemoteDefaultGateway := True
    else
        fRemoteDefaultGateway := False;
    if lngOptions And RASEO_DisableLcpExtensions = RASEO_DisableLcpExtensions then
        fDisableLCPExtensions := True
    else
        fDisableLCPExtensions := False;
    if lngOptions And RASEO_TerminalBeforeDial = RASEO_TerminalBeforeDial then
        fTerminalBeforeDial := True
    else
        fTerminalBeforeDial := False;
    if lngOptions And RASEO_TerminalAfterDial = RASEO_TerminalAfterDial then
        fTerminalAfterDial := True
    else
        fTerminalAfterDial := False;
    if lngOptions And RASEO_ModemLights = RASEO_ModemLights then
        fModemLights := True
    else
        fModemLights := False;
    if lngOptions And RASEO_SwCompression = RASEO_SwCompression then
        fSoftwareCompression := True
    else
        fSoftwareCompression := False;
    if lngOptions And RASEO_RequireEncryptedPw = RASEO_RequireEncryptedPw then
        fRequireEncryptedPassword := True
    else
        fRequireEncryptedPassword := False;
    if lngOptions And RASEO_RequireMsEncryptedPw = RASEO_RequireMsEncryptedPw then
        fRequireMSEncryptedPassword := True
    else
        fRequireMSEncryptedPassword :=  False;
    if lngOptions And RASEO_RequireDataEncryption = RASEO_RequireDataEncryption then
        fRequireDataEncryption := True
    else
        fRequireDataEncryption := False;
    if lngOptions And RASEO_NetworkLogon = RASEO_NetworkLogon then
        fNetworkLogon := True
    else
        fNetworkLogon := False;
    if lngOptions And RASEO_UseLogonCredentials = RASEO_UseLogonCredentials then
        fUseLogonCredentials := True
    else
        fUseLogonCredentials := False;
    if lngOptions And RASEO_PromoteAlternates = RASEO_PromoteAlternates then
        fPromoteAlternates := True
    else
        fPromoteAlternates := False;
    if lngOptions And RASEO_SecureLocalFiles = RASEO_SecureLocalFiles then
        fSecureLocalFiles := True
    else
        fSecureLocalFiles := False;
    if lngOptions And RASEO_RequireEAP = RASEO_RequireEAP then
        fRequireEAP := True
    else
        fRequireEAP := False;
    if lngOptions And RASEO_RequirePAP = RASEO_RequirePAP then
        fRequirePAP := True
    else
        fRequirePAP := False;
    if lngOptions And RASEO_RequireSPAP = RASEO_RequireSPAP then
        fRequireSPAP := True
    else
        fRequireSPAP := False;
    if lngOptions And RASEO_Custom = RASEO_Custom then
        fCustom := True
    else
        fCustom := False;
    if lngOptions And RASEO_PreviewPhoneNumber = RASEO_PreviewPhoneNumber then
        fPreviewPhoneNumber := True
    else
        fPreviewPhoneNumber := False;
    if lngOptions And RASEO_SharedPhoneNumbers = RASEO_SharedPhoneNumbers then
        fSharedPhoneNumbers := True
    else
        fSharedPhoneNumbers := False;
    if lngOptions And RASEO_PreviewUserPw = RASEO_PreviewUserPw then
        fPreviewUserPw := True
    else
        fPreviewUserPw := False;
    if lngOptions And RASEO_PreviewDomain = RASEO_PreviewDomain then
        fPreviewDomain := True
    else
        fPreviewDomain := False;
    if lngOptions And RASEO_ShowDialingProgress = RASEO_ShowDialingProgress then
        fShowDialingProgress := True
    else
        fShowDialingProgress := False;
    if lngOptions And RASEO_RequireCHAP = RASEO_RequireCHAP then
        fRequireCHAP := True
    else
        fRequireCHAP := False;
    if lngOptions And RASEO_RequireMsCHAP = RASEO_RequireMsCHAP then
        fRequireMsCHAP := True
    else
        fRequireMsCHAP := False;
    if lngOptions And RASEO_RequireMsCHAP2 = RASEO_RequireMsCHAP2 then
        fRequireMsCHAP2 := True
    else
        fRequireMsCHAP2 := False;
    if lngOptions And RASEO_RequireW95MSCHAP = RASEO_RequireW95MSCHAP then
        fRequireW95MSCHAP := True
    else
        fRequireW95MSCHAP := False;
    if lngOptions And RASEO_CustomScript = RASEO_CustomScript then
        fCustomScript := True
    else
        fCustomScript := False;
end;

function TMagRasEdt.SetRASOptions : DWORD ;
var
    lngOptions : DWORD ;
begin
    lngOptions := 0;

    if fUseCountryAndAreaCodes then
            lngOptions := lngOptions Or RASEO_UseCountryAndAreaCodes;
    if fSpecificIPAddress then
                    lngOptions := lngOptions Or RASEO_SpecificIpAddr;
    if fSpecificNameServers then
                lngOptions:= lngOptions Or RASEO_SpecificNameServers;
    if fHeaderCompression then
                lngOptions := lngOptions Or RASEO_IpHeaderCompression;
    if fRemoteDefaultGateway then
                lngOptions := lngOptions Or RASEO_RemoteDefaultGateway;
    if fDisableLCPExtensions then
                lngOptions := lngOptions Or RASEO_DisableLcpExtensions;
    if fTerminalBeforeDial then
                lngOptions := lngOptions Or RASEO_TerminalBeforeDial;
    if fTerminalAfterDial then
                lngOptions := lngOptions Or RASEO_TerminalAfterDial;
    if fModemLights then
                        lngOptions := lngOptions Or RASEO_ModemLights;
    if fSoftwareCompression then
                        lngOptions := lngOptions Or RASEO_SwCompression;
    if fRequireEncryptedPassword then
                    lngOptions := lngOptions Or RASEO_RequireEncryptedPw;
    if fRequireMSEncryptedPassword then
                lngOptions := lngOptions Or RASEO_RequireMsEncryptedPw;
    if fRequireDataEncryption then
                lngOptions := lngOptions Or RASEO_RequireDataEncryption;
    if fNetworkLogon then
                        lngOptions := lngOptions Or RASEO_NetworkLogon;
    if fUseLogonCredentials then
                    lngOptions := lngOptions Or RASEO_UseLogonCredentials;
    if fPromoteAlternates then
                    lngOptions := lngOptions Or RASEO_PromoteAlternates;
    if fSecureLocalFiles then
                    lngOptions := lngOptions Or RASEO_SecureLocalFiles ;
    if fRequireEAP then
                    lngOptions := lngOptions Or RASEO_RequireEAP ;
    if fRequirePAP then
                    lngOptions := lngOptions Or RASEO_RequirePAP ;
    if fRequireSPAP then
                    lngOptions := lngOptions Or RASEO_RequireSPAP ;
    if fCustom then
                    lngOptions := lngOptions Or RASEO_Custom ;
    if fPreviewPhoneNumber then
                    lngOptions := lngOptions Or RASEO_PreviewPhoneNumber ;
    if fSharedPhoneNumbers then
                    lngOptions := lngOptions Or RASEO_SharedPhoneNumbers ;
    if fPreviewUserPw then
                    lngOptions := lngOptions Or RASEO_PreviewUserPw ;
    if fPreviewDomain then
                    lngOptions := lngOptions Or RASEO_PreviewDomain ;
    if fShowDialingProgress then
                    lngOptions := lngOptions Or RASEO_ShowDialingProgress ;
    if fRequireCHAP then
                    lngOptions := lngOptions Or RASEO_RequireCHAP ;
    if fRequireMsCHAP then
                    lngOptions := lngOptions Or RASEO_RequireMsCHAP ;
    if fRequireMsCHAP2 then
                    lngOptions := lngOptions Or RASEO_RequireMsCHAP2 ;
    if fRequireW95MSCHAP then
                    lngOptions := lngOptions Or RASEO_RequireW95MSCHAP ;
    if fCustomScript then
                    lngOptions := lngOptions Or RASEO_CustomScript ;
    Result := lngOptions;
end;

procedure TMagRasEdt.GetNETProtocols (lngProtocols: DWORD) ;
{Takes the bit mapped value for the various network protocols
 e.g. IPX, TCPIP and sets the value of the corresponding property}
begin
    if lngProtocols And RASNP_Netbeui =  RASNP_Netbeui then
        fNETBEUI := True
    else
        fNETBEUI := False;

    if lngProtocols And RASNP_Ipx  = RASNP_Ipx then
        fNETIPX := True
    else
        fNETIPX := False;

    if lngProtocols And RASNP_Ip = RASNP_Ip then
        fNetTCPIP := True
    else
        fNetTCPIP := False;
end;

function TMagRasEdt.SetNetProtocols: DWORD ;
var
    lngProtocols : longint;
begin
    lngProtocols := 0;
    if fNetBEUI then lngProtocols := lngProtocols Or RASNP_Netbeui;
    if fNetIPX then lngProtocols := lngprotocols Or RASNP_Ipx;
    if fNetTCPIP then lngProtocols := lngProtocols Or RASNP_Ip;
    Result := lngProtocols;
end;

procedure TMagRasEdt.GetFramingProtocol (lngProtocol: DWORD);
begin
    if lngProtocol = RASFP_Ppp then fFramingProtocol := framePPP ;
    if lngProtocol = RASFP_Slip then fFramingProtocol := frameSlip ;
    if lngProtocol = RASFP_Ras then fFramingProtocol := frameNetbeui ;
end;

function TMagRasEdt.SetFramingProtocol: DWORD ;
begin
    result := 0 ;
    if fFramingProtocol = framePPP then result := RASFP_PPP;
    if fFramingProtocol = frameSlip then result := RASFP_Slip;
    if fFramingProtocol = frameNetbeui then result := RASFP_Ras;
end;

function TMagRasEdt.GetSubLocalPhoneNumber (Index: Integer): AnsiString ;
begin
    result := fSubLocalPhoneNumber [Index] ;
end ;

function TMagRasEdt.GetSubPhoneCanonical (Index: Integer): AnsiString ;
begin
    result := fSubPhoneCanonical [Index] ;
end ;

function TMagRasEdt.GetSubDeviceType (Index: Integer): AnsiString ;
begin
    result := fSubDeviceType [Index] ;
end ;

function TMagRasEdt.GetSubDeviceName (Index: Integer): AnsiString ;
begin
    result := fSubDeviceName [Index] ;
end ;

function TMagRasEdt.GetSubDevicePort (Index: Integer): AnsiString ;
begin
    result := fSubDevicePort [Index] ;
end ;

function TMagRasEdt.GetSubAltPhoneNrList (Index: Integer): TStringList ;
begin
    result := fSubAltPhoneNrList [Index] ;
end ;

function TMagRasEdt.GetCountryIds (Index: Integer): integer ;
begin
    result := fCountryIds [Index] ;
end ;

function TMagRasEdt.GetCountryCodes (Index: Integer): integer ;
begin
    result := fCountryCodes [Index] ;
end ;

procedure TMagRasEdt.SetSubLocalPhoneNumber (Index: Integer; Value: AnsiString) ;
begin
    if value <> fSubLocalPhoneNumber [Index] then
                       fSubLocalPhoneNumber [Index] := value ;
end ;

procedure TMagRasEdt.SetSubPhoneCanonical (Index: Integer; Value: AnsiString) ;
begin
    if value <> fSubPhoneCanonical [Index] then
                       fSubPhoneCanonical [Index] := value ;
end ;

procedure TMagRasEdt.SetSubDeviceType (Index: Integer; Value: AnsiString) ;
begin
    if value <> fSubDeviceType [Index] then
                       fSubDeviceType [Index] := value ;
end ;

procedure TMagRasEdt.SetSubDeviceName (Index: Integer; Value: AnsiString) ;
begin
    if value <> fSubDeviceName [Index] then
                       fSubDeviceName [Index] := value ;
end ;

procedure TMagRasEdt.SetSubDevicePort (Index: Integer; Value: AnsiString) ;
begin
    if value <> fSubDevicePort [Index] then
                       fSubDevicePort [Index] := value ;
end ;

procedure TMagRasEdt.SetSubAltPhoneNrList (Index: Integer; Value: TStringList) ;
begin
    fSubAltPhoneNrList [Index].Assign (Value) ;
end ;

procedure TMagRasEdt.SetPhoneBookPath (Value: AnsiString) ;    // 4.60
begin
    if (MagRasOSVersion <> OSNT4) then exit ;    // 5.21 only allow path on NT4
    fPhoneBookPath := TrimAnsi (Value) ;
    fPBPtr := Nil ;
    if fPhoneBookPath <> '' then fPBPtr := PAnsiChar (fPhoneBookPath) ;
end;

procedure TMagRasEdt.SetPBLocation (Index: Integer) ;    // 5.20
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

function TMagRasEdt.GetErrorString(ErrorCode: LongInt): String;
var
    szErrorString: Array[0..256] of AnsiChar;
begin
    if (NOT MagLoadRasApi) then
    begin
        LastError := ERROR_DLL_NOT_FOUND ;
        result := RASAPI_DLL + #32 + SRasGenNotAvailable ; // Not Available
        exit ;
    end ;
    Result := '';
    if ErrorCode < 0 then exit ;
    FillChar (szErrorString, SizeOf (szErrorString), #0);
    RasGetErrorString (ErrorCode, szErrorString, 256);
    If szErrorString[0] <> #0 THEN
        Result := String (szErrorString)  // 9 Aug 2010
    Else
        Result := SysErrorMessage (ErrorCode) ;  // Angus, try a windows error
end;

// alternate way of showing phone number

function TMagRasEdt.GetCanonical (UseCountryAndAreaCodes: boolean;
        CountryCode: DWORD; AreaCode, LocalPhoneNumber: AnsiString): AnsiString ;
begin
    result := MagRasGetCanonical (UseCountryAndAreaCodes,
                                    CountryCode, AreaCode, LocalPhoneNumber) ;
end ;

// Convert a RASIP address into a string representation

function TMagRasEdt.IPToStr(IPAddr : TRASIPAddr) : AnsiString;
var
  strTemp : AnsiString;
begin
  strTemp := IntToStrAnsi (IPAddr.a);
  strTemp := strTemp + '.' + IntToStrAnsi (IPAddr.b);
  strTemp := strTemp + '.' + IntToStrAnsi (IPAddr.c);
  strTemp := strTemp + '.' + IntToStrAnsi (IPAddr.d);
  Result := strTemp;
end;

function TMagRasEdt.StrToIP (strIP: AnsiString): TRASIPAddr ;
begin
    Str2IP (strIP, result) ;
end ;

function TMagRasEdt.IsIPStr (strIP: AnsiString): boolean ;
var
    IPAddr: TRASIPAddr ;
begin
   result := Str2IP (strIP, IPAddr) ;
end ;

function TMagRasEdt.IsFmtIPStr (var strIP: AnsiString): boolean ;
var
    IPAddr: TRASIPAddr ;
begin
    result := Str2IP (strIP, IPAddr) ;
    if result then strIP := IPToStr (IPAddr) ;  // formats less space, zeros, etc.
end ;

function TMagRasEdt.Str2IP (strIP: AnsiString; var IPAddr: TRASIPAddr): boolean ;
var
    I, len, value, startpos, dotpos: Integer;
    MyIPAddr: array [1..4] of byte ;
    nonzeroflag: boolean ;
begin
    result := false ;
    FillChar (IPAddr, SizeOf(TRasIPAddr), #0);
    len := Length (strIP) ;
    if len < 7 then exit ;    // 0.0.0.0 bare IP address

// read each dotted number
    nonzeroflag := false ;
    startpos := 1 ;
    for I := 1 to 4 do
    begin
        if len <= 0 then exit ;
        if I < 4 then
            dotpos := CharPos ('.', Copy (strIP, startpos, len))
        else
            dotpos := len + 1 ;
        if dotpos <= 0 then exit ;   // not enough dots
        if dotpos > 1 then
            value := AscToIntAnsi (Copy (strIP, startpos, Pred (dotpos)))
        else
            value := 0 ;  // allow for blank
        if value > 255 then exit ;   // number invalid for conversion
        if value > 0 then nonzeroflag := true ;
        MyIPAddr [I] := value ;
        startpos := startpos + dotpos ;
        len := len - dotpos ;
    end ;

// checl valid IP address, only allowed all zeroes
    if (MyIPAddr [1] = 0) and nonzeroflag then exit ;

// found a valid IP address, keep it
    Move (MyIPAddr, IPAddr, SizeOf(TRasIPAddr)) ;
    result := true ;
end ;

// allow to check if RAS available without calling any functions

function TMagRasEdt.TestRas: boolean ;
begin
    result := MagLoadRasApi ;
    if NOT result then
    begin
        LastError := ERROR_DLL_NOT_FOUND ;
        StatusStr := RASAPI_DLL + #32 + SRasGenNotAvailable ; // Not Available
    end ;
end ;

// get Windows XP dwfOptions2 bit flags

procedure TMagRasEdt.GetRASOptions2 (Options: DWORD);
begin
    if Options And RASEO2_SecureFileAndPrint = RASEO2_SecureFileAndPrint then
        fSecureFileAndPrint := True
    else
        fSecureFileAndPrint := False;
    if Options And RASEO2_SecureClientForMSNet = RASEO2_SecureClientForMSNet then
        fSecureClientForMSNet := True
    else
        fSecureClientForMSNet := False;
    if Options And RASEO2_DontNegotiateMultilink = RASEO2_DontNegotiateMultilink then
        fDontNegotiateMultilink := True
    else
        fDontNegotiateMultilink := False;
    if Options And RASEO2_DontUseRasCredentials = RASEO2_DontUseRasCredentials then
        fDontUseRasCredentials := True
    else
        fDontUseRasCredentials := False;
    if Options And RASEO2_UsePreSharedKey = RASEO2_UsePreSharedKey then
        fUsePreSharedKey := True
    else
        fUsePreSharedKey := False;
    if Options And RASEO2_Internet = RASEO2_Internet then
        fInternet := True
    else
        fInternet := False;
    if Options And RASEO2_DisableNbtOverIP = RASEO2_DisableNbtOverIP then
        fDisableNbtOverIP := True
    else
        fDisableNbtOverIP := False;
    if Options And RASEO2_UseGlobalDeviceSettings = RASEO2_UseGlobalDeviceSettings then
        fUseGlobalDeviceSettings := True
    else
        fUseGlobalDeviceSettings := False;
    if Options And RASEO2_ReconnectIfDropped = RASEO2_ReconnectIfDropped then
        fReconnectIfDropped := True
    else
        fReconnectIfDropped := False;
    if Options And RASEO2_SharePhoneNumbers = RASEO2_SharePhoneNumbers then
        fSharePhoneNumbers := True
    else
        fSharePhoneNumbers := False;
end ;

function TMagRasEdt.SetRASOptions2 : DWORD ;
var
    Options : DWORD ;
begin
    Options := 0;
    if fSecureFileAndPrint then
            Options := Options Or RASEO2_SecureFileAndPrint ;
    if fSecureClientForMSNet then
            Options := Options Or RASEO2_SecureClientForMSNet ;
    if fDontNegotiateMultilink then
            Options := Options Or RASEO2_DontNegotiateMultilink ;
    if fDontUseRasCredentials then
            Options := Options Or RASEO2_DontUseRasCredentials ;
    if fUsePreSharedKey then
            Options := Options Or RASEO2_UsePreSharedKey ;
    if fInternet then
            Options := Options Or RASEO2_Internet ;
    if fDisableNbtOverIP then
            Options := Options Or RASEO2_DisableNbtOverIP ;
    if fUseGlobalDeviceSettings then
            Options := Options Or RASEO2_UseGlobalDeviceSettings ;
    if fReconnectIfDropped then
            Options := Options Or RASEO2_ReconnectIfDropped ;
    if fSharePhoneNumbers then
            Options := Options Or RASEO2_SharePhoneNumbers ;
    result := Options ;
end ;

// get details - entryname may be blank to get defaults

function TMagRasEdt.GetEntry (EntryName: AnsiString): LongInt ;
begin
// WARNING - RasEntryAll is in same absolute memory as EntryEdtInfo
// NT and W2K and later return more info, but first part of record is identical to Win9x
    EntryEdtSize := 0 ;
    DeviceEdtSize := 0 ;
    FillChar (EntryEdtInfo, SizeOf (EntryEdtInfo), #0) ;
    FillChar (DeviceEdtInfo, SizeOf (DeviceEdtInfo), #0) ;
    RasEntryAll.dwSize := Sizeof (TRasEntryWXP);   // 9 July 2012
    case MagRasOSVersion of
        OSW9x: RasEntryAll.dwSize := Sizeof (TRasEntry);
        OSNT4: RasEntryAll.dwSize := Sizeof (TRasEntryNT4);
        OSW2K: RasEntryAll.dwSize := Sizeof (TRasEntryW2K);
    end ;

// get buffer sizes
    Result := RasGetEntryProperties (fPBPtr, PAnsiChar (EntryName),
                                    nil, EntryEdtSize, nil, DeviceEdtSize) ;
    if result = ERROR_BUFFER_TOO_SMALL then result := 0 ;

// get entry details
    if result = 0 then
    begin
        if EntryEdtSize > SizeOf (EntryEdtInfo) then
                                        EntryEdtSize := SizeOf (EntryEdtInfo) ;
        if MagRasOSVersion = OSW9x then
        begin
            if (DeviceEdtSize > SizeOf (DeviceEdtInfo)) or
                             (DeviceEdtSize = 0) then
                                    DeviceEdtSize := Sizeof (DeviceEdtInfo) ;
        end ;
        Result := RasGetEntryProperties (fPBPtr, PAnsiChar (EntryName),
                    @EntryEdtInfo, EntryEdtSize, @DeviceEdtInfo, DeviceEdtSize) ;
    end ;
    LastError := Result ;
    if LastError <> 0 then  StatusStr := GetErrorString (LastError)
end ;

// get entry properties from specified Phone Book (aka Dialup Connection)
// this version gets absolutely everything - including all subentries
// it may be used with a blank

function TMagRasEdt.GetAllEntryProps (EntryName: AnsiString): LongInt ;
var
    strptr: PAnsiChar ;
    ch: AnsiChar ;
    I, offset, stringlen: integer ;
//    temp: string ;
//    offset: DWORD ;
begin
    if (NOT MagLoadRasApi) or (NOT MagRasExtn_Flag) then
    begin
        LastError := ERROR_DLL_NOT_FOUND ;
        result := LastError ;
        exit ;
    end ;

// clear properties, including subentries
    DefaultProps ;
    fSubInitTotal := 0 ;     // 4.62

// get entry and device details, unpack for properties
    result := GetEntry (EntryName) ;
    if result = 0 then
    begin
        with RasEntryAll do   // get properties in same order as RASENTRY
        begin
            GetRASOptions (dwfOptions);

        // location/phone number - create canonical number by combining fields
            fCountryCode := dwCountryCode;
            fCountryID  := dwCountryID;
            fAreaCode := szAreaCode;
            fLocalPhoneNumber := szLocalPhoneNumber;
            fPhoneCanonical := MagRasGetCanonical (fUseCountryAndAreaCodes,
                                 fCountryCode, fAreaCode, fLocalPhoneNumber) ;

        // PPP/IP
            fIPAddress := IPToStr(ipaddr);
            fDNSAddress := IPToStr(ipaddrDns);
            fDNSAddressAlt := IPToStr(ipaddrDnsAlt);
            fWINSAddress := IPToStr(ipaddrWins);
            fWINSAddressAlt := IPToStr(ipaddrWinsAlt);

        // Framing
            fFrameSize := dwFrameSize;
            GetNETProtocols (dwfNetProtocols);
            GetFramingProtocol (dwFramingProtocol);

        // Scripting
            fScript := szScript;

        // Auto Dial
            fAutoDialDll := szAutodialDll;
            fAutoDialFunc := szAutodialFunc;

        // Device name, port and type
        // warning, NT4 (not W2K and later) devices have two nulls strings, device and port
            fDeviceType := LowerCaseAnsi (StrPas (szDeviceType)) ;
            fDeviceName := GetDevNamePort (szDeviceName,
                                        sizeof (szDeviceName), fDevicePort);

        // X25
            fX25Address := szX25Address;
            fX25Facilities := szX25Facilities;
            fX25UserData := szX25UserData;
            fX25PadType := szX25PadType;

        // basic multilink
            fISDNChannels := dwChannels;

        // dwReserved1 ??
        // dwReserved2 ??

        // following stuff is NT4 extensions

        // Multilink and BAP
        // note - percent stuff only appears to be supported by W2K and later
            if MagRasOSVersion >= OSNT4 then
            begin
                fSubEntries := dwSubEntries ;
                fDialMode := TDialMode (dwDialMode) ;
                fDialExtraPercent := dwDialExtraPercent ;
                fDialExtraSampleSeconds := dwDialExtraSampleSeconds ;
                fHangUpExtraPercent := dwHangUpExtraPercent ;
                fHangUpExtraSampleSeconds := dwHangUpExtraSampleSeconds ;

        // Idle timeout
                fIdleDisconnectSeconds := dwIdleDisconnectSeconds ;
            end ;

        // Windows 2000 extensions
            if MagRasOSVersion >= OSW2K then
            begin
                fPType := TPType (dwType) ;
                fEncryptionType := TEncryptionType (dwEncryptionType) ;
                fCustomAuthKey := dwCustomAuthKey ;
                fguidId := guidId ;
                fCustomDialDll := szCustomDialDll ;
                fVpnStrategy := TVpnStrategy (dwVpnStrategy) ;
            end ;

        // Windows XP extensions - 4.62
            if MagRasOSVersion >= OSWXP then
            begin
                GetRASOptions2 (dwfOptions2) ;
                // dwfOptions3 not used yet
                fDnsSuffix := dzDnsSuffix ;
                fTcpWindowSize := dwTcpWindowSize ;
                fPrerequisitePbk := szPrerequisitePbk ;
                fPrerequisiteEntry := szPrerequisiteEntry ;
                fRedialCount := dwRedialCount ;
                fRedialPause := dwRedialPause ;
            end
            else
                GetRASOptions2 (0) ;

    // may be extra phone numbers after structure, null separated list
            if (dwAlternateOffset <> 0) and (MagRasOSVersion >= OSNT4) then
            begin
                strptr := @EntryEdtInfo [dwAlternateOffset] ;
                offset := dwAlternateOffset ;
                ch := strptr^ ;
                while (ch <> #00) do
                begin
                    stringlen := StrLen (strptr) ;
                    fAltPhoneNrList.Add (Trim (String (FixedToPasStr (strptr, stringlen)))) ; // 9 Aug 2010
                    strptr := strptr + stringlen + 1 ;
                    offset := offset + stringlen + 1 ;
                    if offset >= Abs (EntryEdtSize) then break ;
                    ch := strptr^ ;
                end ;
            end ;
        end ;

    // check some Win9x device specific stuff - not really sure we care!
     {  if DevCfgEdt.DfgHdr.dwSize = sizeof (DevCfgEdt) then
        begin
            with DevCfgEdt.DfgHdr do
            begin
                I := dwVersion ;
                if I = 0 then temp := '123' ;  // prevents compiler removing prev line to we can debug it
                I := fwOptions ;  // flags
                if I = 0 then temp := '123' ;
            end ;
            with DevCfgEdt.CommConfig do
            begin
                if (dwProviderSize <> 0) and
                                    (dwProviderSubType = PST_Modem) then
                begin
                    with DevCfgEdt.ModemSettings do
                    begin
                        I := dwInactivityTimeout ;
                        if I = 0 then temp := '123' ;
                    end ;
                end ;
            end ;
        end ;  }

    // now get some subentries - the first is probably the same as the default above
        fSubCurTotal := 0 ;
        if fSubEntries > 0 then
        begin
            for I := 1 to fSubEntries do
            begin
                result := GetSubEntryProps (EntryName, I) ;
                LastError := Result ;
                if LastError <> 0 then
                begin
                    StatusStr := GetErrorString (LastError) ;
                    break ;
                end
                else
                    inc (fSubCurTotal) ;
            end ;
        // keep it so when saving can delete un-needed entries
            fSubInitTotal := fSubCurTotal ;     // 4.62
        end ;
    end ;
end ;

// get sub entry properties from specified Phone Book (aka Dialup Connection), NT4
// this is generally only called from GetAllEntryProps

function TMagRasEdt.GetSubEntryProps (EntryName: AnsiString;
                                            SubEntry: integer): LongInt ;
var
    strptr: PAnsiChar ;
    ch: AnsiChar ;
    offset, stringlen: integer ;
    tempsize: DWORD ;
begin
    result := -1 ;
    if MagRasOSVersion < OSNT4 then exit ;
    if (SubEntry = 0) or (SubEntry > MaxSubEntries) then exit ;
    if (NOT MagLoadRasApi) then
    begin
        LastError := ERROR_DLL_NOT_FOUND ;
        result := LastError ;
        exit ;
    end ;

// WARNING - RASSubEntryNT4 is in same absolute memory as SubEntryEdtInfo
    SubEntryEdtSize := 0 ;
    tempsize := 0 ;
    FillChar (SubEntryEdtInfo, SizeOf (SubEntryEdtInfo), #0) ;
    RasSubEntryNT4.dwSize := Sizeof (TRasSubEntryNT4);

// get buffer sizes
    Result := RasGetSubEntryProperties (fPBPtr, PAnsiChar (EntryName),
                                SubEntry, nil, SubEntryEdtSize, nil, tempsize) ;
    if result = ERROR_BUFFER_TOO_SMALL then result := 0 ;

// get entry details
    if result = 0 then
    begin
        tempsize := 0 ;
        if SubEntryEdtSize > SizeOf (SubEntryEdtInfo) then
                                SubEntryEdtSize := SizeOf (SubEntryEdtInfo) ;
        Result := RasGetSubEntryProperties (fPBPtr, PAnsiChar (EntryName),
                    SubEntry, @SubEntryEdtInfo, SubEntryEdtSize, nil, tempsize) ;
    end ;
    LastError := Result ;
    if LastError <> 0 then
        StatusStr := GetErrorString (LastError)
    else
    begin
        with RASSubEntryNT4 do   // get properties in same order as RASUBTRY
        begin
           // dwfFlags     // none at present

       // Device name, port and type
        // warning, NT4 (not W2K and later) devices have two nulls strings, device and port
            fSubDeviceType [SubEntry] := LowerCaseAnsi (StrPas (szDeviceType)) ;
            fSubDeviceName [SubEntry] := GetDevNamePort (szDeviceName,
                            sizeof (szDeviceName), fSubDevicePort [SubEntry]);

        // location/phone number - create canonical number by combining fields
            fSubLocalPhoneNumber [SubEntry] := szLocalPhoneNumber;
            fSubPhoneCanonical [SubEntry] :=
                        MagRasGetCanonical (fUseCountryAndAreaCodes,
                                 fCountryCode, fAreaCode, szLocalPhoneNumber) ;

     // may be extra phone numbers after structure, null separated list
            fSubAltPhoneNrList [SubEntry].Clear;
            if (dwAlternateOffset <> 0) then
            begin
                strptr := @EntryEdtInfo [dwAlternateOffset] ;
                offset := dwAlternateOffset ;
                ch := strptr^ ;
                while (ch <> #00) do
                begin
                    stringlen := StrLen (strptr) ;
                    fSubAltPhoneNrList [SubEntry].Add (Trim (String (FixedToPasStr (strptr, stringlen)))) ; // 9 Aug 2010
                    strptr := strptr + stringlen + 1 ;
                    offset := offset + stringlen + 1 ;
                    if offset >= Abs (SubEntryEdtSize) then break ;
                    ch := strptr^ ;
                end ;
            end ;
       end ;
    end ;
end ;

// alternate way of setting phone number elements 

procedure TMagRasEdt.SetCanonical (PhoneCanonical: AnsiString) ;
begin
    MagRasUnpackCanonical (PhoneCanonical, fUseCountryAndAreaCodes,
                            fCountryCode, fAreaCode, fLocalPhoneNumber) ;
end ;

// save entry properties for specified Phone Book (aka Dialup Connection)

function TMagRasEdt.PutEntry (EntryName: AnsiString): LongInt ;
var
    devptr: pointer ;
begin
    devptr := Nil ;
    if DeviceEdtSize <> 0 then devptr := @DeviceEdtInfo ;
    Result := RasSetEntryProperties (fPBPtr, PAnsiChar (EntryName),
                           @EntryEdtInfo, EntryEdtSize, devptr, DeviceEdtSize) ;
    LastError := Result ;
    if LastError <> 0 then  StatusStr := GetErrorString (LastError) ;
end ;

// save all entry properties for specified Phone Book (aka Dialup Connection)

function TMagRasEdt.PutAllEntryProps (EntryName: AnsiString): LongInt ;
var
    strptr: PAnsiChar ;
    I: integer ;
    stringlen: DWORD ;
    temp: AnsiString ;
begin
    if (NOT MagLoadRasApi) or (NOT MagRasExtn_Flag) then
    begin
        LastError := ERROR_DLL_NOT_FOUND ;
        result := LastError ;
        exit ;
    end ;

// WARNING - RasEntryAll is in same absolute memory as EntryEdtInfo
    FillChar (EntryEdtInfo, SizeOf (EntryEdtInfo), #0) ;
    RasEntryAll.dwSize := Sizeof (TRasEntryWXP);  // 9 July 2012
    case MagRasOSVersion of
        OSW9x: RasEntryAll.dwSize := Sizeof (TRasEntry);
        OSNT4: RasEntryAll.dwSize := Sizeof (TRasEntryNT4);
        OSW2K: RasEntryAll.dwSize := Sizeof (TRasEntryW2K);
    end ;
    EntryEdtSize := RasEntryAll.dwSize ;  // may be expanded with alternate numbers

// check for reasonably valid device specific information, else default it
// leaving DeviceEdtSize as zero will get proper default, but for new connection only
    if MagRasOSVersion = OSW9x then
    begin
        if (DeviceEdtSize <> 0) and
                    (DevCfgEdt.DfgHdr.dwSize <> sizeof (DevCfgEdt)) then
        begin
            Move (DevCfgDefault, DeviceEdtInfo, Sizeof (DevCfgDefault)) ;
            DeviceEdtSize := Sizeof (DevCfgDefault) ;
       end ;
    end
    else
        DeviceEdtSize := 0 ;    // make sure nothing written on NT4/W2K and later

 // set properties in same order as RASENTRY
    with RasEntryAll Do
    begin
        dwfOptions := SetRASOptions;

    // Location/phone numbers
        dwCountryID := fCountryID;
        dwCountryCode := fCountryCode;
        strPLCopy (szLocalPhoneNumber, fLocalPhoneNumber, RAS_MaxEntryName);
        StrPLCopy (szAreaCode, fAreaCode, RAS_MaxAreaCode);

   // PPP/IP
        ipaddr := StrToIP (fIPAddress);
        ipaddrDNS := StrToIP (fDNSAddress);
        ipaddrDNSAlt := StrToIP (fDNSAddressAlt);
        ipaddrWins := StrToIp (fWINSAddress);
        ipaddrWinsAlt := StrToIP (fWINSAddressAlt);

    // Framing
        if fFramingProtocol = frameSLIP then dwFrameSize := fFrameSize;
        dwfNetProtocols := SetNetProtocols;
        dwFramingProtocol := SetFramingProtocol ;

    // Scripting
        StrPLCopy (szScript, fScript, SizeOf(szScript));

    // Autodial
        StrPLCopy (szAutodialDll, fAutoDialDll, SizeOf(szAutodialDll));
        StrPLCopy (szAutodialFunc, fAutoDialFunc, SizeOf(szAutoDialFunc));

    // Device
        StrPLCopy (szDeviceType, fDeviceType, RAS_MaxDeviceType );
        StrPLCopy (szDeviceName, fDeviceName, RAS_MaxDeviceName );

    // X25
        StrPLCopy (szX25PadType, fX25PadType, RAS_MaxPadType);
        StrPLCopy (szX25Address, fX25Address, RAS_MaxX25Address );
        StrPLCopy (szX25Facilities, fX25Facilities, RAS_MaxFacilities);
        strPLCopy (szX25UserData, fX25UserData, RAS_MaxUserData);

    // basic multilink
     // dwChannels := fISDNChannels;  - not used

     // dwReserved1  ??
     // dwReserved2  ??

    // following stuff is NT4 extensions

    // Multilink and BAP
    // note - percent stuff only appears to be supported by W2K and later
        if MagRasOSVersion >= OSNT4 then
        begin
            dwSubEntries := 0 ;  //  fSubCurTotal  always zero
            dwDialMode := Ord (fDialMode) ;
            dwDialExtraPercent := fDialExtraPercent ;
            dwDialExtraSampleSeconds := fDialExtraSampleSeconds ;
            dwHangUpExtraPercent := fHangUpExtraPercent ;
            dwHangUpExtraSampleSeconds := fHangUpExtraSampleSeconds ;

    // Idle timeout
            dwIdleDisconnectSeconds := fIdleDisconnectSeconds ;
        end ;

    // Windows 2000 extensions
        if MagRasOSVersion >= OSW2K then
        begin
            dwType := Ord (fPType) ;   // Type is not a good identifer in Delphi
            dwEncryptionType := Ord (fEncryptionType) ;
            dwCustomAuthKey := Ord (fCustomAuthKey) ;
      //        guidId := fguidId ;  // not settable  ??  // Angus 4.3
            strPLCopy (szCustomDialDll, fCustomDialDll, MAX_PATH - 1);
            dwVpnStrategy := Ord (fVpnStrategy) ;
        end ;

    // Windows XP extensions - 4.62
        if MagRasOSVersion >= OSWXP then
        begin
            dwfOptions2 := SetRASOptions2 ;
      //    dwfOptions3;  // spare
            strPLCopy (dzDnsSuffix, fDnsSuffix,RAS_MaxDnsSuffix - 1) ;
            dwTcpWindowSize := fTcpWindowSize ;
            strPLCopy (szPrerequisitePbk, fPrerequisitePbk, MAX_PATH - 1) ;
            strPLCopy (szPrerequisiteEntry, fPrerequisiteEntry, RAS_MaxEntryName) ;
            dwRedialCount := fRedialCount ;
            dwRedialPause := fRedialPause ;
        end ;

    // alternate numbers, after structure - remove LocalPhoneNumber if first alternate
        dwAlternateOffset := 0 ;
        if (MagRasOSVersion >= OSNT4) and (fAltPhoneNrList.Count <> 0) then
        begin
//          if fLocalPhoneNumber = fAltPhoneNrList [0] then
//                                              fAltPhoneNrList.Delete (0) ;
            if (fAltPhoneNrList.Count <> 0) then
            begin
                dwAlternateOffset := EntryEdtSize ;
                strptr := @EntryEdtInfo [EntryEdtSize] ;
                for I := 0 to fAltPhoneNrList.Count - 1 do
                begin
                    stringlen := length (fAltPhoneNrList [I]) ;
                    if stringlen > 0 then
                    begin
                        temp := AnsiString (fAltPhoneNrList [I]) ;
                        move (temp[1], strptr^, stringlen) ;
                        strptr := strptr + stringlen + 1 ;
                        EntryEdtSize := EntryEdtSize + stringlen + 1 ;
                    end ;
                end ;
                EntryEdtSize := EntryEdtSize + 1 ;  // extra null at end
            end ;
        end ;
    end;

// see if deleting un-needed subentries (before saving main entry), XP only - 4.62
    if (MagRasOSVersion >= OSWXP) and (fSubCurTotal <> 0) then
    begin
        if (fSubInitTotal > fSubCurTotal) then
        begin
            RasDeleteSubEntry (fPBPtr, PAnsiChar (EntryName), fSubInitTotal) ;
            dec (fSubInitTotal) ;
        end ;
    end ;

// update phonebook
    result := PutEntry (EntryName) ;

// set sub entries
    if result = 0 then
    begin
        if (MagRasOSVersion >= OSNT4) and (fSubCurTotal <> 0) then
        begin

        // save sub entries
            for I := 1 to fSubCurTotal do
            begin
                Result := PutSubEntryProps (EntryName, I) ;
                LastError := Result ;
                if LastError <> 0 then
                begin
                    StatusStr := GetErrorString (LastError) ;
                    exit ;
                end ;
            end ;
            result := PutEntry (EntryName) ;  // a second time !!!
        end ;
    end ;
end ;

// put sub entry properties into specified Phone Book (aka Dialup Connection), NT4
// this is generally only called from PutAllEntryProps

function TMagRasEdt.PutSubEntryProps (EntryName: AnsiString;
                                            SubEntry: integer): LongInt ;
var
    strptr: PAnsiChar ;
    temp: AnsiString ;
    I: integer ;
    stringlen: DWORD ;
begin
    result := -1 ;
    if MagRasOSVersion < OSNT4 then exit ;
    if (SubEntry = 0) or (SubEntry > MaxSubEntries) then exit ;
    if (NOT MagLoadRasApi) then
    begin
        LastError := ERROR_DLL_NOT_FOUND ;
        result := LastError ;
        exit ;
    end ;

// WARNING - RASSubEntryNT4 is in same absolute memory as SubEntryEdtInfo
    SubEntryEdtSize := Sizeof (TRasSubEntryNT4);
    FillChar (SubEntryEdtInfo, SizeOf (SubEntryEdtInfo), #0) ;
    RasSubEntryNT4.dwSize := Sizeof (TRasSubEntryNT4);

    with RASSubEntryNT4 do   // get properties in same order as RASUBTRY
    begin
        dwfFlags := 0 ;      // none at present

        // Device
        StrPLCopy (szDeviceType, fSubDeviceType [SubEntry], RAS_MaxDeviceType );
        StrPLCopy (szDeviceName, fSubDeviceName [SubEntry], RAS_MaxDeviceName );

        // phone number
        strPLCopy (szLocalPhoneNumber, fSubLocalPhoneNumber [SubEntry],
                                                             RAS_MaxEntryName);

    // alternate numbers, after structure
        dwAlternateOffset := 0 ;
        if (fSubAltPhoneNrList [SubEntry].Count <> 0) then
        begin
//          if fSubLocalPhoneNumber [SubEntry] =
//                      fSubAltPhoneNrList [SubEntry] [0] then
//                                  fSubAltPhoneNrList [SubEntry].Delete (0) ;
            if (fSubAltPhoneNrList [SubEntry].Count <> 0) then
            begin
                dwAlternateOffset := SubEntryEdtSize ;
                strptr := @SubEntryEdtInfo [SubEntryEdtSize] ;
                for I := 0 to fSubAltPhoneNrList [SubEntry].Count - 1 do
                begin
                    stringlen := length (fSubAltPhoneNrList [SubEntry] [I]) ;
                    if stringlen > 0 then
                    begin
                        temp := AnsiString (fSubAltPhoneNrList [SubEntry] [I]) ; // 9 Aug 2010
                        move (temp[1], strptr^, stringlen) ;
                        strptr := strptr + stringlen + 1 ;
                        SubEntryEdtSize := SubEntryEdtSize + stringlen + 1 ;
                    end ;
                end ;
                SubEntryEdtSize := SubEntryEdtSize + 1 ;  // extra null at end
            end ;
        end ;
    end;

// update phonebookcurrently ignoring the device specific information
    Result := RasSetSubEntryProperties (fPBPtr, PAnsiChar (EntryName),
                    SubEntry, @SubEntryEdtInfo, SubEntryEdtSize, nil, 0) ;
    LastError := Result ;
    if LastError <> 0 then
            StatusStr := GetErrorString (LastError) ;
 end ;

// get dial parms from specified Phone Book (aka Dialup Connection)

FUNCTION TMagRasEdt.GetDialProps (EntryName: AnsiString): LongInt ;
var
    RasCredentials: TRasCredentials ;
begin
    if (NOT MagLoadRasApi) then
    begin
        LastError := ERROR_DLL_NOT_FOUND ;
        result := LastError ;
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
        StrPCopy (RasDialParamsNT4.szEntryName, EntryName) ;
        LastError := RasGetEntryDialParams (Nil,
                                        RasDialParamsNT4, fPasswordFlag) ;
        result := LastError ;
        if LastError = 0 then
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
            StatusStr := GetErrorString (LastError);
    end
    else
  // 5.20  use NT4 and later API
    begin
        FillChar (RasCredentials, SizeOf (RasCredentials), 0);
        RasCredentials.dwSize := Sizeof (RasCredentials);
        RasCredentials.dwMask := RASCM_UserName OR RASCM_Password OR
                                                             RASCM_Domain ;
        if (fPBLocation = REN_AllUsers) and fDefaultCreds then     // Angus 5.30
                RasCredentials.dwMask := RasCredentials.dwMask OR RASCM_DefaultCreds ;
        LastError := RasGetCredentials (fPBPtr, PAnsiChar (EntryName),
                                                         RasCredentials) ;
        result := LastError ;
        if LastError = 0 then
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
            StatusStr := GetErrorString (LastError);
    end ;
end;

// update dial parms for specified Phonebook (aka Dialup Connection)

function TMagRasEdt.PutDialProps (EntryName: AnsiString): LongInt ;
var
    removePW: boolean ;
    RasCredentials: TRasCredentials ;
begin
    if (NOT MagLoadRasApi) then
    begin
        LastError := ERROR_DLL_NOT_FOUND ;
        result := LastError ;
        exit ;
    end ;
    if MagRasOSVersion = OSW9x then
    begin
        FillChar (RasDialParamsNT4, SizeOf (RasDialParamsNT4), #0);
        With RasDialParamsNT4 DO
        Begin
            if MagRasOSVersion = OSW9x then
                dwSize := Sizeof (TRasDialParams)
            else
                dwSize := Sizeof (TRasDialParamsNT4);
            StrPLCopy (szEntryName, EntryName, RAS_MaxEntryName);
            StrPLCopy (szCallbackNumber, fCallBackNumber, RAS_MaxCallbackNumber);
            StrPLCopy (szUserName, fUserName, UNLEN);
            StrPLCopy (szPassword, fPassWord, PWLEN);
            StrPLCopy (szDomain, fDomain, DNLEN);
        End;
        removePW := false ;         //  Angus 3.1, allow password to be removed
        if (fPassWord = '') and (NOT fPasswordFlag) then removePW := true ;  // Angus 5.20
        LastError := RasSetEntryDialParams (fPBPtr, RASDialParamsNT4, removePW) ;
        if LastError <> 0 then
                    StatusStr := GetErrorString (LastError);
    end
    else
  // Angus 5.20, use NT4 and later API that might work properly on Visa
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
        LastError := RasSetCredentials (fPBPtr, PAnsiChar (EntryName),
                                                     RasCredentials, false) ;
        if LastError <> 0 then
                    StatusStr := GetErrorString (LastError);
    end ;
    Result := LastError;
end;

function TMagRasEdt.GetAllCountryInfo : integer;
var
    RasExtCtryInfo: TRasExtCtryInfo ;
    BuffSize: DWORD ;
    strptr: PAnsiChar ;
    stringlen, nextid, I, totcountry: integer ;
begin
    result := -1 ;  // 20 July 2011 ?????
    if (NOT MagLoadRasApi) or (NOT MagRasExtn_Flag) then
    begin
        LastError := ERROR_DLL_NOT_FOUND ;
        result := LastError ;
        exit ;
    end ;
    fCountryList.Clear ;
    for I := 0 to MaxCountries do
    begin
        fCountryIds [I] := 0 ;
        fCountryCodes [I] := 0 ;
    end ;
    nextid := 1 ;
    totcountry := 0 ;
    while nextid <> 0 do
    begin
        Buffsize := SizeOf(TRasExtCtryInfo) ;
        FillChar (RasExtCtryInfo, Buffsize, 0);
        RasExtCtryInfo.RasCtryInfo.dwSize := SizeOf(TRasCtryInfo) ;
        RasExtCtryInfo.RasCtryInfo.dwCountryId := nextid ;
        Result := RasGetCountryInfo (@RasExtCtryInfo, Buffsize);
        LastError := Result;
        If (Result <> 0) THEN exit ;
        strptr := @RasExtCtryInfo.buffer
            [RasExtCtryInfo.RasCtryInfo.dwCountryNameOffset -  SizeOf(TRasCtryInfo)] ;
        stringlen := StrLen (strptr) ;
        fCountryList.Add (Trim (String (FixedToPasStr (strptr, stringlen))) + ' (' +
                IntToStr (RasExtCtryInfo.RasCtryInfo.dwCountryCode) + ')') ;  // 9 Aug 2010
        fCountryCodes [totcountry] := RasExtCtryInfo.RasCtryInfo.dwCountryCode ;
        fCountryIds [totcountry] := nextid ;
        inc (totcountry) ;
        if totcountry > MaxCountries then exit ;
        nextid := RasExtCtryInfo.RasCtryInfo.dwNextCountryId ;
    end ;
end ;

Initialization
finalization

end.
