// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //
//                                                                            //
// For further information / comments, visit our WEB site at                  //
//   www.mcm-design.com                                                       //
// or e-mail to                                                               //
//   CustomerCare@mcm-design.dk                                               //
//----------------------------------------------------------------------------//
//
// $Log:  15984: mcmSTI.pas
//
//    Rev 1.8    2014-01-15 13:41:56  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//    Rev 1.7    2013-12-04 23:16:10  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//    Rev 1.6    26-08-2009 22:39:48  mcm    Version: DT 3.9
// Fixed unicode issues (PChar -> PAnsiChar)
//
//    Rev 1.5    30-12-2008 16:33:22  mcm    Version: DT 3.8
// Delphi 2009
//
//   Rev 1.4    04-07-2003 16:22:02  mcm    Version: DT 2.5

//
//   Rev 1.3    14-06-2003 10:58:00  mcm    Version: DT 2.4
// Added a check for a successful load of the STI DLL file. Solves an exception
// when executed in Windows 95 and NT 4.0.

//
//   Rev 1.2    04-03-2003 18:52:24  mcm    Version: DT 2.2
// Delphi 4 & HRESULT did not accept -1, replaced with STI_NOTCONNECTED.

//
//   Rev 1.1    04-03-2003 18:15:54  mcm    Version: DT 2.2
// Completed TmcmSTI and IStillImage etc.

//
//   Rev 1.0    04-12-2001 16:51:04  mcm

unit mcmSTI;

{$INCLUDE mcmDefines.pas}

interface

{$IFDEF GE_DXE2}
uses WinApi.Windows, WinApi.Messages, System.Classes, Vcl.Controls;
{$ELSE}
uses Windows, Messages, Classes, Controls;
{$ENDIF}


//------------------------------------------------------------------------------
// The sti.h,  was translated to Delphi by MCM DESIGN 2003.
//------------------------------------------------------------------------------

var
  mcmSTIDLL : HMODULE = 0;

//------------------------------------------------------------------------------
// Copyright (c) 1986-1997  Microsoft Corporation
//
// Module Name:
//   sti.h
// Abstract:
//   This module contains the user mode still image APIs in COM format
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Set packing
//------------------------------------------------------------------------------
{$A+}

//------------------------------------------------------------------------------
// Error codes are constructed as compound COM status codes
//------------------------------------------------------------------------------
const
  // The operation completed successfully
  STI_OK                     = S_OK;
  STI_ERROR_NO_ERROR         = STI_OK;
  // The device exists but not currently attached to the system
  STI_NOTCONNECTED           = S_FALSE;
  // The requested change in device mode settings had no effect
  STI_CHANGENOEFFECT         = S_FALSE;
  // The application requires newer version
  STIERR_OLD_VERSION         = SEVERITY_ERROR shl 31 or FACILITY_WIN32 shl 16 or ERROR_OLD_WIN_VERSION;
  // The application was written for pre-release version of provider DLL
  STIERR_BETA_VERSION        = SEVERITY_ERROR shl 31 or FACILITY_WIN32 shl 16 or ERROR_RMODE_APP;
  // The requested object could not be created due to incompatible or mismatched driver
  STIERR_BADDRIVER           = SEVERITY_ERROR shl 31 or FACILITY_WIN32 shl 16 or ERROR_BAD_DRIVER_LEVEL;
  // The device is not registered
  STIERR_DEVICENOTREG        = REGDB_E_CLASSNOTREG;
  // The requested container does not exist
  STIERR_OBJECTNOTFOUND      = SEVERITY_ERROR shl 31 or FACILITY_WIN32 shl 16 or ERROR_FILE_NOT_FOUND;
  // An invalid or not state matching parameter was passed to the API
  STIERR_INVALID_PARAM       = E_INVALIDARG;
  // The specified interface is not supported
  STIERR_NOINTERFACE         = E_NOINTERFACE;
  // The undetermined error occured
  STIERR_GENERIC             = E_FAIL;
  // There is not enough memory to perform requested operation
  STIERR_OUTOFMEMORY         = E_OUTOFMEMORY;
  // The application called unsupported (at this time)function
  STIERR_UNSUPPORTED         = E_NOTIMPL;
  // The application requires newer version
  STIERR_NOT_INITIALIZED     = SEVERITY_ERROR shl 31 or FACILITY_WIN32 shl 16 or ERROR_NOT_READY;
  // The application requires newer version
  STIERR_ALREADY_INITIALIZED = SEVERITY_ERROR shl 31 or FACILITY_WIN32 shl 16 or ERROR_ALREADY_INITIALIZED;
  // The operation can not performed while device is locked
  STIERR_DEVICE_LOCKED       = SEVERITY_ERROR shl 31 or FACILITY_WIN32 shl 16 or ERROR_LOCK_VIOLATION;
  // The specified propery can not be changed for this device
  STIERR_READONLY            = E_ACCESSDENIED;
  // The device already has notification handle associated with it
  STIERR_NOTINITIALIZED      = E_ACCESSDENIED;
  // The device needs to be locked before attempting this operation
  STIERR_NEEDS_LOCK          = SEVERITY_ERROR shl 31 or FACILITY_WIN32 shl 16 or ERROR_NOT_LOCKED;
  // The device is opened by another application in data mode
  STIERR_SHARING_VIOLATION   = SEVERITY_ERROR shl 31 or FACILITY_WIN32 shl 16 or ERROR_SHARING_VIOLATION;
  // Handle already set for this context
  STIERR_HANDLEEXISTS        = SEVERITY_ERROR shl 31 or FACILITY_WIN32 shl 16 or ERROR_ALREADY_EXISTS;
  // Device name is not recognized
  STIERR_INVALID_DEVICE_NAME = SEVERITY_ERROR shl 31 or FACILITY_WIN32 shl 16 or ERROR_INVALID_NAME;
  // Device hardware type is not valid
  STIERR_INVALID_HW_TYPE     = SEVERITY_ERROR shl 31 or FACILITY_WIN32 shl 16 or ERROR_INVALID_DATA;
  // No events available
  STIERR_NOEVENTS            = SEVERITY_ERROR shl 31 or FACILITY_WIN32 shl 16 or ERROR_NO_MORE_ITEMS;
  // Device appears as not ready
  STIERR_DEVICE_NOTREADY     = SEVERITY_ERROR shl 31 or FACILITY_WIN32 shl 16 or ERROR_NOT_READY;

//------------------------------------------------------------------------------
// Registry keys and values
//------------------------------------------------------------------------------
const
  REGSTR_VAL_TYPE_W            = 'Type';
  REGSTR_VAL_VENDOR_NAME_W     = 'Vendor';
  REGSTR_VAL_DEVICETYPE_W      = 'DeviceType';
  REGSTR_VAL_DEVICESUBTYPE_W   = 'DeviceSubType';
  REGSTR_VAL_DEV_NAME_W        = 'DeviceName';
  REGSTR_VAL_DRIVER_DESC_W     = 'DriverDesc';
  REGSTR_VAL_FRIENDLY_NAME_W   = 'FriendlyName';
  REGSTR_VAL_GENERIC_CAPS_W    = 'Capabilities';
  REGSTR_VAL_HARDWARE_W        = 'HardwareConfig';
  REGSTR_VAL_HARDWARE          = 'HardwareConfig';
  REGSTR_VAL_DEVICE_NAME_W     = 'DriverDesc';
  REGSTR_VAL_DATA_W            = 'DeviceData';
  REGSTR_VAL_GUID_W            = 'GUID';
  REGSTR_VAL_GUID              = 'GUID';
  REGSTR_VAL_LAUNCH_APPS_W     = 'LaunchApplications';
  REGSTR_VAL_LAUNCH_APPS       = 'LaunchApplications';
  REGSTR_VAL_LAUNCHABLE_W      = 'Launchable';
  REGSTR_VAL_LAUNCHABLE        = 'Launchable';

//------------------------------------------------------------------------------
// Device instance value names
//------------------------------------------------------------------------------
const
  STI_DEVICE_VALUE_TWAIN_NAME               = 'TwainDS';
  STI_DEVICE_VALUE_ISIS_NAME                = 'ISISDriverName';
  STI_DEVICE_VALUE_ICM_PROFILE              = 'ICMProfile';
  STI_DEVICE_VALUE_DEFAULT_LAUNCHAPP        = 'DefaultLaunchApp';
  STI_DEVICE_VALUE_TIMEOUT                  = 'PollTimeout';
  STI_DEVICE_VALUE_DISABLE_NOTIFICATIONS    = 'DisableNotifications';
  REGSTR_VAL_BAUDRATE                       = 'BaudRate';

  STI_DEVICE_VALUE_TWAIN_NAME_A             = 'TwainDS';
  STI_DEVICE_VALUE_ISIS_NAME_A              = 'ISISDriverName';
  STI_DEVICE_VALUE_ICM_PROFILE_A            = 'ICMProfile';
  STI_DEVICE_VALUE_DEFAULT_LAUNCHAPP_A      = 'DefaultLaunchApp';
  STI_DEVICE_VALUE_TIMEOUT_A                = 'PollTimeout';
  STI_DEVICE_VALUE_DISABLE_NOTIFICATIONS_A  = 'DisableNotifications';
  REGSTR_VAL_BAUDRATE_A                     = 'BaudRate';

//------------------------------------------------------------------------------
// Only use UNICODE STI interfaces
//------------------------------------------------------------------------------
const
  STI_UNICODE = 1;

//------------------------------------------------------------------------------
// Include COM definitions
//------------------------------------------------------------------------------

//#include <stireg.h>
//#include <stierr.h>

//------------------------------------------------------------------------------
// Class IID's
//------------------------------------------------------------------------------
const
// B323F8E0-2E68-11D0-90EA-00AA0060F86C
  CLSID_Sti : TGUID = '{B323F8E0-2E68-11D0-90EA-00AA0060F86C}';
//DEFINE_GUID(CLSID_Sti, 0xB323F8E0L, 0x2E68, 0x11D0, 0x90, 0xEA, 0x00, 0xAA, 0x00, 0x60, 0xF8, 0x6C);

//------------------------------------------------------------------------------
// Interface IID's
//------------------------------------------------------------------------------

// {641BD880-2DC8-11D0-90EA-00AA0060F86C}
  IID_IStillImageW : TGUID = '{641BD880-2DC8-11D0-90EA-00AA0060F86C}';
//DEFINE_GUID(IID_IStillImageW, 0x641BD880L, 0x2DC8, 0x11D0, 0x90, 0xEA, 0x00, 0xAA, 0x00, 0x60, 0xF8, 0x6C);

// {A7B1F740-1D7F-11D1-ACA9-00A02438AD48}
  IID_IStillImageA : TGUID = '{A7B1F740-1D7F-11D1-ACA9-00A02438AD48}';
//DEFINE_GUID(IID_IStillImageA, 0xA7B1F740L, 0x1D7F, 0x11D1, 0xAC, 0xA9, 0x00, 0xA0, 0x24, 0x38, 0xAD, 0x48);

// {6CFA5A80-2DC8-11D0-90EA-00AA0060F86C}
  IID_IStiDevice : TGUID = '{6CFA5A80-2DC8-11D0-90EA-00AA0060F86C}';
//DEFINE_GUID(IID_IStiDevice, 0x6CFA5A80L, 0x2DC8, 0x11D0, 0x90, 0xEA, 0x00, 0xAA, 0x00, 0x60, 0xF8, 0x6C);

//------------------------------------------------------------------------------
// Standard event GUIDs
//------------------------------------------------------------------------------

// {740D9EE6-70F1-11d1-AD10-00A02438AD48}
  GUID_DeviceArrivedLaunch : TGUID = '{740D9EE6-70F1-11d1-AD10-00A02438AD48}';
//DEFINE_GUID(GUID_DeviceArrivedLaunch, 0x740d9ee6, 0x70f1, 0x11d1, 0xad, 0x10, 0x0, 0xa0, 0x24, 0x38, 0xad, 0x48);

// {A6C5A715-8C6E-11d2-977A-0000F87A926F}
  GUID_ScanImage : TGUID = '{A6C5A715-8C6E-11d2-977A-0000F87A926F}';
//DEFINE_GUID(GUID_ScanImage, 0xa6c5a715, 0x8c6e, 0x11d2, 0x97, 0x7a, 0x0, 0x0, 0xf8, 0x7a, 0x92, 0x6f);

// {B441F425-8C6E-11d2-977A-0000F87A926F}
  GUID_ScanPrintImage : TGUID = '{B441F425-8C6E-11d2-977A-0000F87A926F}';
//DEFINE_GUID(GUID_ScanPrintImage, 0xb441f425, 0x8c6e, 0x11d2, 0x97, 0x7a, 0x0, 0x0, 0xf8, 0x7a, 0x92, 0x6f);


// {C00EB793-8C6E-11d2-977A-0000F87A926F}
  GUID_ScanFaxImage : TGUID = '{C00EB793-8C6E-11d2-977A-0000F87A926F}';
//DEFINE_GUID(GUID_ScanFaxImage, 0xc00eb793, 0x8c6e, 0x11d2, 0x97, 0x7a, 0x0, 0x0, 0xf8, 0x7a, 0x92, 0x6f);

// {C00EB795-8C6E-11d2-977A-0000F87A926F}
  GUID_STIUserDefined1 : TGUID = '{C00EB795-8C6E-11d2-977A-0000F87A926F}';
//DEFINE_GUID(GUID_STIUserDefined1, 0xc00eb795, 0x8c6e, 0x11d2, 0x97, 0x7a, 0x0, 0x0, 0xf8, 0x7a, 0x92, 0x6f);

// {C77AE9C5-8C6E-11d2-977A-0000F87A926F}
  GUID_STIUserDefined2 : TGUID = '{C77AE9C5-8C6E-11d2-977A-0000F87A926F}';
//DEFINE_GUID(GUID_STIUserDefined2, 0xc77ae9c5, 0x8c6e, 0x11d2, 0x97, 0x7a, 0x0, 0x0, 0xf8, 0x7a, 0x92, 0x6f);

// {C77AE9C6-8C6E-11d2-977A-0000F87A926F}
  GUID_STIUserDefined3 : TGUID = '{C77AE9C6-8C6E-11d2-977A-0000F87A926F}';
//DEFINE_GUID(GUID_STIUserDefined3, 0xc77ae9c6, 0x8c6e, 0x11d2, 0x97, 0x7a, 0x0, 0x0, 0xf8, 0x7a, 0x92, 0x6f);


//------------------------------------------------------------------------------
// Generic constants and definitions
//------------------------------------------------------------------------------
const
  STI_VERSION_FLAG_MASK    = $FF000000;
  STI_VERSION_FLAG_UNICODE = $01000000;

  function GET_STIVER_MAJOR(dwVersion : dword) : dword;
  //GET_STIVER_MAJOR(dwVersion)   (HIWORD(dwVersion) & ~STI_VERSION_FLAG_MASK)
  function GET_STIVER_MINOR(dwVersion : dword) : dword;
  //GET_STIVER_MINOR(dwVersion)   LOWORD(dwVersion)

const
  STI_VERSION_REAL         = $00000002;
  STI_VERSION_MIN_ALLOWED  = $00000002;

  {$IFDEF UNICODE}
  STI_VERSION              = (STI_VERSION_REAL or STI_VERSION_FLAG_UNICODE);
  {$ELSE}
  STI_VERSION              = (STI_VERSION_REAL);
  {$ENDIF}

//------------------------------------------------------------------------------
// Maximum length of internal device name
//------------------------------------------------------------------------------
const
  STI_MAX_INTERNAL_NAME_LENGTH = 128;

//------------------------------------------------------------------------------
// begin sti_device_information

//------------------------------------------------------------------------------
//  Device information definitions and prototypes
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//  Following information is used for enumerating still image devices , currently configured
//  in the system. Presence of the device in the enumerated list does not mean availability
// of the device, it only means that device was installed at least once and had not been removed since.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Type of device ( scanner, camera) is represented by DWORD value with
// hi word containing generic device type , and lo word containing sub-type
//------------------------------------------------------------------------------
type
  TSTI_DEVICE_MJ_TYPE = (StiDeviceTypeDefault,
                         StiDeviceTypeScanner,
                         StiDeviceTypeDigitalCamera,
                         StiDeviceTypeStreamingVideo);

type
  TSTI_DEVICE_TYPE = dword;

//------------------------------------------------------------------------------
// Macros to extract device type/subtype from single type field
//------------------------------------------------------------------------------
//#define GET_STIDEVICE_TYPE(dwDevType)   HIWORD(dwDevType)
function GET_STIDEVICE_TYPE(dwDevType : dword) : dword;
//#define GET_STIDEVICE_SUBTYPE(dwDevType)   LOWORD(dwDevType)
function GET_STIDEVICE_SUBTYPE(dwDevType : dword) : dword;

//------------------------------------------------------------------------------
// Device capabilities bits.
// Various capabilities are grouped into separate bitmasks
//------------------------------------------------------------------------------
type
  TSTI_DEV_CAPS = packed record
  dwGeneric : dword;
  end;
  PSTI_DEV_CAPS = ^TSTI_DEV_CAPS;

//------------------------------------------------------------------------------
// Generic capabilities mask contain 16 bits , common for all devices, maintained by MS
// and 16 bits , which USD can use for proprietary capbailities reporting.
//------------------------------------------------------------------------------

//#define GET_STIDCOMMON_CAPS(dwGenericCaps)   LOWORD(dwGenericCaps)
function GET_STIDCOMMON_CAPS(dwGenericCaps : dword) : dword;
//#define GET_STIVENDOR_CAPS(dwGenericCaps)    HIWORD(dwGenericCaps)
function GET_STIVENDOR_CAPS(dwGenericCaps : dword) : dword;

const
  STI_GENCAP_COMMON_MASK : dword = $00FF;

//------------------------------------------------------------------------------
// Notifications are supported.
// If this capability set , device can be subscribed to .
//------------------------------------------------------------------------------
const
  STI_GENCAP_NOTIFICATIONS =   $00000001;

//------------------------------------------------------------------------------
// Polling required .
// This capability is used when previous is set to TRUE. Presence of it means
// that device is not capable of issuing "truly" asyncronous notifications, but
// can be polled to determine the moment when event happened
//------------------------------------------------------------------------------
const
  STI_GENCAP_POLLING_NEEDED = $00000002;

//------------------------------------------------------------------------------
// Generate event on device arrival
// If this capability is set, still image service will generate event when device
// instance is successfully initialized ( typically in response to PnP arrival)

//------------------------------------------------------------------------------
// Note: on initial service enumeration events will nto be generated to avoid
// end-user confusion.
//------------------------------------------------------------------------------
const
  STI_GENCAP_GENERATE_ARRIVALEVENT = $00000004;

//------------------------------------------------------------------------------
// Auto port selection on non-PnP buses
// This capability indicates that USD is able to detect non-PnP device on a
// bus , device is supposed to be attached to.
//------------------------------------------------------------------------------
const
  STI_GENCAP_AUTO_PORTSELECT = $00000008;

//------------------------------------------------------------------------------
// Type of bus connection for those in need to know
//------------------------------------------------------------------------------
const
  STI_HW_CONFIG_UNKNOWN  = $0001;
  STI_HW_CONFIG_SCSI     = $0002;
  STI_HW_CONFIG_USB      = $0004;
  STI_HW_CONFIG_SERIAL   = $0008;
  STI_HW_CONFIG_PARALLEL = $0010;

//------------------------------------------------------------------------------
// Device information structure, this is not configurable. This data is returned from
// device enumeration API and is used for populating UI or selecting which device
// should be used in current session
//------------------------------------------------------------------------------
type
  TSTI_DEVICE_INFORMATIONW = packed record
    dwSize : dword;
    // Type of the hardware imaging device
    DeviceType : TSTI_DEVICE_TYPE;
    // Device identifier for reference when creating device object
    szDeviceInternalName : array[0..STI_MAX_INTERNAL_NAME_LENGTH-1] of wchar;
    // Set of capabilities flags
    DeviceCapabilities : TSTI_DEV_CAPS;
    // This includes bus type
    dwHardwareConfiguration : dword;
    // Vendor description string
    pszVendorDescription : PWideChar; // LPWSTR
    // Device description , provided by vendor
    pszDeviceDescription : PWideChar; // LPWSTR
    // String , representing port on which device is accessible.
    pszPortName : PWideChar; // LPWSTR
    // Control panel propery provider
    pszPropProvider : PWideChar; // LPWSTR
    // Local specific ("friendly") name of the device, mainly used for showing in the UI
    pszLocalName : PWideChar; // LPWSTR
  end;
  PSTI_DEVICE_INFORMATIONW = ^TSTI_DEVICE_INFORMATIONW;

type
  TSTI_DEVICE_INFORMATIONA = packed record
    dwSize : dword;
    // Type of the hardware imaging device
    DeviceType : TSTI_DEVICE_TYPE;
    // Device identifier for reference when creating device object
    szDeviceInternalName : array[0..STI_MAX_INTERNAL_NAME_LENGTH-1] of AnsiChar;
    // Set of capabilities flags
    DeviceCapabilities : TSTI_DEV_CAPS;
    // This includes bus type
    dwHardwareConfiguration : dword;
    // Vendor description string
    pszVendorDescription : PAnsiChar; // LPCSTR
    // Device description , provided by vendor
    pszDeviceDescription : PAnsiChar; // LPCSTR
    // String , representing port on which device is accessible.
    pszPortName : PAnsiChar; // LPCSTR
    // Control panel propery provider
    pszPropProvider : PAnsiChar; // LPCSTR
    // Local specific ("friendly") name of the device, mainly used for showing in the UI
    pszLocalName : PAnsiChar; // LPCSTR
  end;
  PSTI_DEVICE_INFORMATIONA = ^TSTI_DEVICE_INFORMATIONA;

  {$IFDEF UNICODE} // STI_UNICODE
  TSTI_DEVICE_INFORMATION = TSTI_DEVICE_INFORMATIONW;
  PSTI_DEVICE_INFORMATION = PSTI_DEVICE_INFORMATIONW;
  {$ELSE}
  TSTI_DEVICE_INFORMATION = TSTI_DEVICE_INFORMATIONA;
  PSTI_DEVICE_INFORMATION = PSTI_DEVICE_INFORMATIONA;
  {$ENDIF}

//------------------------------------------------------------------------------
// EXTENDED STI INFORMATION TO COVER WIA
//------------------------------------------------------------------------------
type
  TSTI_WIA_DEVICE_INFORMATIONW = packed record
    dwSize : dword;
    // Type of the hardware imaging device
    DeviceType : TSTI_DEVICE_TYPE;
    // Device identifier for reference when creating device object
    szDeviceInternalName : array[0..STI_MAX_INTERNAL_NAME_LENGTH-1] of wchar;
    // Set of capabilities flags
    DeviceCapabilities : TSTI_DEV_CAPS;
    // This includes bus type
    dwHardwareConfiguration : dword;
    // Vendor description string
    pszVendorDescription : PWideChar; // LPWSTR
    // Device description , provided by vendor
    pszDeviceDescription : PWideChar; // LPWSTR
    // String , representing port on which device is accessible.
    pszPortName : PWideChar; // LPWSTR
    // Control panel propery provider
    pszPropProvider : PWideChar; // LPWSTR
    // Local specific ("friendly") name of the device, mainly used for showing in the UI
    pszLocalName : PWideChar; // LPWSTR
    //-----------
    // WIA values
    pszUiDll : PWideChar; // LPWSTR
    pszServer : PWideChar; // LPWSTR
  end;
  PSTI_WIA_DEVICE_INFORMATIONW = ^TSTI_WIA_DEVICE_INFORMATIONW;

type
  TSTI_WIA_DEVICE_INFORMATIONA = packed record
    dwSize : dword;
    // Type of the hardware imaging device
    DeviceType : TSTI_DEVICE_TYPE;
    // Device identifier for reference when creating device object
    szDeviceInternalName : array[0..STI_MAX_INTERNAL_NAME_LENGTH-1] of AnsiChar;
    // Set of capabilities flags
    DeviceCapabilities : TSTI_DEV_CAPS;
    // This includes bus type
    dwHardwareConfiguration : dword;
    // Vendor description string
    pszVendorDescription : PAnsiChar; // LPCSTR
    // Device description , provided by vendor
    pszDeviceDescription : PAnsiChar; // LPCSTR
    // String , representing port on which device is accessible.
    pszPortName : PAnsiChar; // LPCSTR
    // Control panel propery provider
    pszPropProvider : PAnsiChar; // LPCSTR
    // Local specific ("friendly") name of the device, mainly used for showing in the UI
    pszLocalName : PAnsiChar; // LPCSTR
    //-----------
    // WIA values
    pszUiDll : PAnsiChar; // LPCSTR
    pszServer : PAnsiChar; // LPCSTR
  end;
  PSTI_WIA_DEVICE_INFORMATIONA = ^TSTI_WIA_DEVICE_INFORMATIONA;


  {$IFDEF UNICODE} // STI_UNICODE
  TSTI_WIA_DEVICE_INFORMATION = TSTI_WIA_DEVICE_INFORMATIONW;
  PSTI_WIA_DEVICE_INFORMATION = PSTI_WIA_DEVICE_INFORMATIONW;
  {$ELSE}
  TSTI_WIA_DEVICE_INFORMATION = TSTI_WIA_DEVICE_INFORMATIONA;
  PSTI_WIA_DEVICE_INFORMATION = PSTI_WIA_DEVICE_INFORMATIONA;
  {$ENDIF}

// end sti_device_information
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// Device state information.
// ------------------------
//
// Following types  are used to inquire state characteristics of the device after
// it had been opened.
//
// Device configuration structure contains configurable parameters reflecting
// current state of the device
//
//
// Device hardware status.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Individual bits for state acquiring  through StatusMask
//------------------------------------------------------------------------------
const
// State of hardware as known to USD
  STI_DEVSTATUS_ONLINE_STATE        = $0001;

// State of pending events ( as known to USD)
  STI_DEVSTATUS_EVENTS_STATE        = $0002;

//------------------------------------------------------------------------------
// Online state values
//------------------------------------------------------------------------------
const
  STI_ONLINESTATE_OPERATIONAL       = $00000001;
  STI_ONLINESTATE_PENDING           = $00000002;
  STI_ONLINESTATE_ERROR             = $00000004;
  STI_ONLINESTATE_PAUSED            = $00000008;
  STI_ONLINESTATE_PAPER_JAM         = $00000010;
  STI_ONLINESTATE_PAPER_PROBLEM     = $00000020;
  STI_ONLINESTATE_OFFLINE           = $00000040;
  STI_ONLINESTATE_IO_ACTIVE         = $00000080;
  STI_ONLINESTATE_BUSY              = $00000100;
  STI_ONLINESTATE_TRANSFERRING      = $00000200;
  STI_ONLINESTATE_INITIALIZING      = $00000400;
  STI_ONLINESTATE_WARMING_UP        = $00000800;
  STI_ONLINESTATE_USER_INTERVENTION = $00001000;
  STI_ONLINESTATE_POWER_SAVE        = $00002000;

//------------------------------------------------------------------------------
// Event processing parameters
//------------------------------------------------------------------------------
const
  STI_EVENTHANDLING_ENABLED         = $00000001;
  STI_EVENTHANDLING_POLLING         = $00000002;
  STI_EVENTHANDLING_PENDING         = $00000004;

type
  TSTI_DEVICE_STATUS = packed record
    dwSize : dword;
    // Request field - bits of status to verify
    StatusMask : dword;
    //
    // Fields are set when status mask contains STI_DEVSTATUS_ONLINE_STATE bit set
    //
    // Bitmask describing  device state
    dwOnlineState : dword;
    // Device status code as defined by vendor
    dwHardwareStatusCode : dword;
    //
    // Fields are set when status mask contains STI_DEVSTATUS_EVENTS_STATE bit set
    //
    // State of device notification processing (enabled, pending)
    dwEventHandlingState : dword;
    // If device is polled, polling interval in ms
    dwPollingInterval : dword;
  end;
  PSTI_DEVICE_STATUS = ^TSTI_DEVICE_STATUS;

//------------------------------------------------------------------------------
// Structure to describe diagnostic ( test ) request to be processed by USD
//------------------------------------------------------------------------------
const
  // Basic test for presence of associated hardware
  STI_DIAGCODE_HWPRESENCE = $00000001;

//------------------------------------------------------------------------------
// Status bits for diagnostic
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// generic diagnostic errors
//------------------------------------------------------------------------------
type
  TSTI_ERROR_INFOW = packed record
    dwSize : dword;
    // Generic error , describing results of last operation
    dwGenericError : dword;
    // vendor specific error code
    dwVendorError : dword;
    // String, describing in more details results of last operation if it failed
    szExtendedErrorText : array[0..254] of wchar;
  end;
  PSTI_ERROR_INFOW = ^TSTI_ERROR_INFOW;

  TSTI_ERROR_INFOA = packed record
    dwSize : dword;
    dwGenericError : dword;
    dwVendorError : dword;
    szExtendedErrorText : array[0..254] of AnsiChar;
  end;
  PSTI_ERROR_INFOA = ^TSTI_ERROR_INFOA;

  {$IFDEF UNICODE} // STI_UNICODE
  TSTI_ERROR_INFO = TSTI_ERROR_INFOW;
  {$ELSE}
  TSTI_ERROR_INFO = TSTI_ERROR_INFOA;
  {$ENDIF}
  PSTI_ERROR_INFO = ^TSTI_ERROR_INFO;


  TSTI_DIAG = packed record
    dwSize : dword;
    // Diagnostic request fields. Are set on request by caller
    // One of the
    dwBasicDiagCode : dword;
    dwVendorDiagCode : dword;
    // Response fields
    dwStatusMask : dword;
    sErrorInfo : TSTI_ERROR_INFO;
  end;
  PSTI_DIAG = ^TSTI_DIAG;

//
//  DIAG = TSTI_DIAG;
//  LPDIAG = PSTI_DIAG;


// end device state information.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Flags passed to WriteToErrorLog call in a first parameter, indicating type of the message
// which needs to be logged
//------------------------------------------------------------------------------
const
  STI_TRACE_INFORMATION = $00000001;
  STI_TRACE_WARNING     = $00000002;
  STI_TRACE_ERROR       = $00000004;

//------------------------------------------------------------------------------
// Event notification mechansims.
// ------------------------------
//
// Those are used to inform last subscribed caller of the changes in device state, initiated by
// device.
//
// The only supported discipline of notification is stack. The last caller to subscribe will be notified
// and will receive notification data. After caller unsubscribes , the previously subscribed caller will
// become active.
//------------------------------------------------------------------------------

// Notifications are sent to subscriber via window message. Window handle is passed as
// parameter
const
  STI_SUBSCRIBE_FLAG_WINDOW = $0001;

// Device notification is signalling Win32 event ( auto-set event). Event handle
// is passed as a parameter
const
  STI_SUBSCRIBE_FLAG_EVENT  = $0002;

type
  TSTISUBSCRIBE = packed record
    dwSize : dword;
    dwFlags : dword;
    // Not used . Will be used for subscriber to set bit mask filtering different events
    dwFilter : dword;
    // When STI_SUBSCRIBE_FLAG_WINDOW bit is set, following fields should be set
    // Handle of the window which will receive notification message
    hWndNotify : HWND;
    // Handle of Win32 auto-reset event , which will be signalled whenever device has
    // notification pending
    hEvent : THandle;
    // Code of notification message, sent to window
    uiNotificationMessage : dword; // UINT
  end;
  PSTISUBSCRIBE = ^TSTISUBSCRIBE;

const
  MAX_NOTIFICATION_DATA = 64;


//------------------------------------------------------------------------------
// Structure to describe notification information
//------------------------------------------------------------------------------
type
  TSTINOTIFY = packed record
    dwSize : dword;                 // Total size of the notification structure
    // GUID of the notification being retrieved
    guidNotificationCode : TGUID;
    // Vendor specific notification description
    abNotificationData : array[0..MAX_NOTIFICATION_DATA-1] of byte;     // USD specific
  end;
  PSTINOTIFY = ^TSTINOTIFY;

// end event_mechanisms
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// STI device broadcasting
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// When STI Device is being added or removed, PnP broadacst is being sent, but it is not obvious
// for application code to recognize if it is STI device and if so, what is the name of the
// device. STI subsystem will analyze PnP broadcasts and rebroadcast another message via
// BroadcastSystemMessage / WM_DEVICECHANGE / DBT_USERDEFINED.

// String passed as user defined message contains STI prefix, action and device name
const
  STI_ADD_DEVICE_BROADCAST_ACTION    = 'Arrival';
  STI_REMOVE_DEVICE_BROADCAST_ACTION = 'Removal';

  STI_ADD_DEVICE_BROADCAST_STRING    = 'STI\\STI_ADD_DEVICE_BROADCAST_ACTION\\%s';
  STI_REMOVE_DEVICE_BROADCAST_STRING = 'STI\\STI_REMOVE_DEVICE_BROADCAST_ACTION\\%s';


// end STI broadcasting
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// Device create modes
//------------------------------------------------------------------------------
const
// Device is being opened only for status querying and notifications receiving
  STI_DEVICE_CREATE_STATUS = $00000001;

// Device is being opened for data transfer ( supersedes status mode)
  STI_DEVICE_CREATE_DATA   = $00000002;

  STI_DEVICE_CREATE_BOTH   = $00000003;

//------------------------------------------------------------------------------
// Bit mask for legitimate mode bits, which can be used when calling CreateDevice
//------------------------------------------------------------------------------
  STI_DEVICE_CREATE_MASK   = $0000FFFF;

//------------------------------------------------------------------------------
// Flags controlling device enumeration
//------------------------------------------------------------------------------
  STIEDFL_ALLDEVICES       = $00000000;
  STIEDFL_ATTACHEDONLY     = $00000001;

//------------------------------------------------------------------------------
// Control code , sent to the device through raw control interface
//------------------------------------------------------------------------------
type
  TSTI_RAW_CONTROL_CODE = dword;

//------------------------------------------------------------------------------
// All raw codes below this one are reserved for future use.
//------------------------------------------------------------------------------
const
  STI_RAW_RESERVED = $1000;

//------------------------------------------------------------------------------
// COM Interfaces to STI
//------------------------------------------------------------------------------

type
  IStillImageW = interface;
  IStillImageA = interface;
  IStiDevice   = interface;

//------------------------------------------------------------------------------
// IStillImage interface
//
// Top level STI access interface.
//
//------------------------------------------------------------------------------

  IStillImageW = interface (IUnknown)
    ['{641BD880-2DC8-11D0-90EA-00AA0060F86C}']
    (*** IStillImage methods ***)
    function Initialize(hInstance : HINST; dwVersion : dword) : HResult; stdcall;
    function GetDeviceList(dwType : dword; dwFlags : dword; var pdwItemsReturned : dword; var ppBuffer : THandle) : HResult; stdcall;
    function GetDeviceInfo(pwszDeviceName : PWideChar; var ppBuffer : THandle) : HResult; stdcall;
    function CreateDevice(pwszDeviceName : PWideChar; dwMode : dword; var pDevice : IStiDevice; punkOuter : IUnknown) : HResult; stdcall;

    // Device instance values. Used to associate various data with device.
    function GetDeviceValue(pwszDeviceName : PWideChar; pValueName : PWideChar; var DataType : dword; pData : PByte; var cbData : dword) : HResult; stdcall;
    function SetDeviceValue(pwszDeviceName : PWideChar; pValueName : PWideChar; DataType : dword; pData : PByte; cbData : dword) : HResult; stdcall;

    // For appllication started through push model launch, returns associated information
    function GetSTILaunchInformation(pwszDeviceName : PWideChar;  var pdwEventCode : dword; pwszEventName : PWideChar) : HResult; stdcall;
    function RegisterLaunchApplication(pwszAppName : PWideChar; pwszCommandLine : PWideChar) : HResult; stdcall;
    function UnregisterLaunchApplication(pwszAppName : PWideChar) : HResult; stdcall;

    // To control state of notification handling. For polled devices this means state of monitor
    // polling, for true notification devices means enabling/disabling notification flow
    // from monitor to registered applications
    function EnableHwNotifications(pwszDeviceName : PWideChar; bNewState : bool) : HResult; stdcall;
    function GetHwNotificationState(pwszDeviceName : PWideChar; var pbCurrentState : bool) : HResult; stdcall;

    // When device is installed but not accessible, application may request bus refresh
    // which in some cases will make device known. This is mainly used for nonPnP buses
    // like SCSI, when device was powered on after PnP enumeration
    function RefreshDeviceBus(pwszDeviceName : PWideChar) : HResult; stdcall;

    // Launch application to emulate event on a device. Used by "control center" style components,
    // which intercept device event, analyze and later force launch based on certain criteria.
    function LaunchApplicationForDevice(pwszDeviceName : PWideChar; pwszAppName : PWideChar; pStiNotify : PSTINOTIFY) : HResult; stdcall;

    // For non-PnP devices with non-known bus type connection, setup extension, associated with the
    // device can set it's parameters
    function SetupDeviceParameters(Info : PSTI_DEVICE_INFORMATIONW) : HResult; stdcall;

    // Write message to STI error log
    function WriteToErrorLog(dwMessageType : dword; pszMessage : PWideChar) : HResult; stdcall;

    {$IFDEF NOT_IMPLEMENTED}
      // TO register application for receiving various STI notifications
      function RegisterDeviceNotification(pwszAppName : PWideChar; lpSubscribe : PSTISUBSCRIBE) : HResult; stdcall;
      function UnregisterDeviceNotification : HResult; stdcall;
    {$ENDIF}
  end;

  IStillImageA = interface (IUnknown)
    ['{A7B1F740-1D7F-11D1-ACA9-00A02438AD48}']
    (*** IStillImage methods ***)
    function Initialize(hInstance : HINST; dwVersion : dword) : HResult; stdcall;
    function GetDeviceList(dwType : dword; dwFlags : dword; var pdwItemsReturned : dword; var ppBuffer : THandle) : HResult; stdcall;
    function GetDeviceInfo(pwszDeviceName : PAnsiChar; var ppBuffer : THandle) : HResult; stdcall;

    function CreateDevice(pwszDeviceName : PAnsiChar; dwMode : dword; var pDevice : IStiDevice; punkOuter : IUnknown) : HResult; stdcall;

    // Device instance values. Used to associate various data with device.
    function GetDeviceValue(pwszDeviceName : PAnsiChar; pValueName : PAnsiChar; var DataType : dword; pData : PByte; var cbData : dword) : HResult; stdcall;
    function SetDeviceValue(pwszDeviceName : PAnsiChar; pValueName : PAnsiChar; DataType : dword; pData : PByte; cbData : dword) : HResult; stdcall;

    // For appllication started through push model launch, returns associated information
    function GetSTILaunchInformation(pwszDeviceName : PAnsiChar;  var pdwEventCode : dword; pwszEventName : PAnsiChar) : HResult; stdcall;
    function RegisterLaunchApplication(pwszAppName : PAnsiChar; pwszCommandLine : PAnsiChar) : HResult; stdcall;
    function UnregisterLaunchApplication(pwszAppName : PAnsiChar) : HResult; stdcall;

    // To control state of notification handling. For polled devices this means state of monitor
    // polling, for true notification devices means enabling/disabling notification flow
    // from monitor to registered applications
    //
    function EnableHwNotifications(pwszDeviceName : PAnsiChar; bNewState : bool) : HResult; stdcall;
    function GetHwNotificationState(pwszDeviceName : PAnsiChar; var pbCurrentState : bool) : HResult; stdcall;

    // When device is installed but not accessible, application may request bus refresh
    // which in some cases will make device known. This is mainly used for nonPnP buses
    // like SCSI, when device was powered on after PnP enumeration
    function RefreshDeviceBus(pwszDeviceName : PAnsiChar) : HResult; stdcall;

    // Launch application to emulate event on a device. Used by "control center" style components,
    // which intercept device event , analyze and later force launch based on certain criteria.
    function LaunchApplicationForDevice(pwszDeviceName : PAnsiChar; pwszAppName : PAnsiChar; pStiNotify : PSTINOTIFY) : HResult; stdcall;

    // For non-PnP devices with non-known bus type connection, setup extension, associated with the
    // device can set it's parameters
    function SetupDeviceParameters(Info : PSTI_DEVICE_INFORMATIONA) : HResult; stdcall;

    // Write message to STI error log
    function WriteToErrorLog(dwMessageType : dword; pszMessage : PAnsiChar) : HResult; stdcall;

    {$IFDEF NOT_IMPLEMENTED}
      // TO register application for receiving various STI notifications
      function RegisterDeviceNotification(pwszAppName : PAnsiChar; lpSubscribe : PSTISUBSCRIBE) : HResult; stdcall;
      function UnregisterDeviceNotification : HResult; stdcall;
    {$ENDIF}
  end;

//------------------------------------------------------------------------------
// IStillImage_Device interface
//
// This is generic per device interface. Specialized interfaces are also
// available
//------------------------------------------------------------------------------

  IStiDevice = interface (IUnknown)
    ['{6CFA5A80-2DC8-11D0-90EA-00AA0060F86C}']
    (*** IStillImage methods ***)
    function Initialize(hInstance : HINST; pwszDeviceName : PWideChar; dwVersion : dword; dwMode : dword) : HResult; stdcall;
    function GetCapabilities(pDevCaps : PSTI_DEV_CAPS) : HResult; stdcall;
    function GetStatus(pDevStatus : PSTI_DEVICE_STATUS) : HResult; stdcall;
    function DeviceReset : HResult; stdcall;
    function Diagnostic(pBuffer : PSTI_DIAG) : HResult; stdcall;
    function Escape(EscapeFunction : TSTI_RAW_CONTROL_CODE; lpInData : pointer; cbInDataSize : dword; pOutData : pointer; dwOutDataSize : dword; var pdwActualData : dword) : HResult; stdcall;
    function GetLastError(pdwLastDeviceError : dword) : HResult; stdcall;
    function LockDevice(dwTimeOut : dword) : HResult; stdcall;
    function UnLockDevice : HResult; stdcall;
    function RawReadData(lpBuffer : Pointer; var lpdwNumberOfBytes : dword; lpOverlapped : pointer) : HResult; stdcall;
    function RawWriteData(lpBuffer : Pointer; var nNumberOfBytes : dword; lpOverlapped : pointer) : HResult; stdcall;
    function RawReadCommand(lpBuffer : Pointer; var lpdwNumberOfBytes : dword; lpOverlapped : pointer) : HResult; stdcall;
    function RawWriteCommand(lpBuffer : Pointer; var nNumberOfBytes : dword; lpOverlapped : pointer) : HResult; stdcall;
    // Subscription is used to enable "control center" style applications , where flow of
    // notifications should be redirected from monitor itself to another "launcher"
    function Subscribe(lpSubsribe : PSTISUBSCRIBE) : HResult; stdcall;
    function GetLastNotificationData(lpNotify : PSTINOTIFY) : HResult; stdcall;
    function UnSubscribe : HResult; stdcall;
    function GetLastErrorInfo(var pLastErrorInfo : TSTI_ERROR_INFO) : HResult; stdcall;
  end;


  {$IFDEF UNICODE} // STI_UNICODE
    IID_IStillImage    = IStillImageW;
    IStillImage        = IStillImageW;
  {$ELSE}
    IID_IStillImage    = IStillImageA;
    IStillImage        = IStillImageA;
  {$ENDIF}

  TStiCreateInstanceW = function(hinst : HINST; dwVer : DWORD; var ppSti : IStillImageW; const punkOuter : IUnknown) : HResult; stdcall;
  TStiCreateInstanceA = function (hinst : HINST; dwVer : DWORD; var ppSti : IStillImageA; const punkOuter : IUnknown) : HResult; stdcall;
  {$IFDEF UNICODE} // STI_UNICODE
    TStiCreateInstance = TStiCreateInstanceW;
  {$ELSE}
    TStiCreateInstance = TStiCreateInstanceA;
  {$ENDIF}

//------------------------------------------------------------------------------
type
  TmcmSTI = class(TComponent)
  private
    FStiObject        : IStillImage;
    FDeviceInfo       : TSTI_DEVICE_INFORMATIONW;
    FDeviceName       : array[0..64] of char;
    FDeviceType       : TSTI_DEVICE_MJ_TYPE;
    FEventName        : array[0..64] of char;
    FActualDeviceName : array[0..255] of char;
    FEventCode        : dword;
    //FOnDeviceEvent    : TNotifyEvent;
  protected
    function    GetActualDeviceName : string;
    function    GetDeviceName : String;
    function    GetDeviceType : TSTI_DEVICE_MJ_TYPE;
    function    GetEventName : String;
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    function    GetDeviceInfo(DeviceName : string) : boolean;
    function    LaunchedBySTI : string;
    function    RegisterApplication(Name : String; Path : String) : boolean;
    function    UnRegisterApplication(Name : String) : boolean;
  published
    property    DeviceName : string
      read      GetDeviceName;
    property    DeviceType : TSTI_DEVICE_MJ_TYPE
      read      GetDeviceType;
    property    EventName : string
      read      GetEventName;
    property    ActualDeviceName : string
      read      GetActualDeviceName;
  {
    property    OnDeviceEvent : TNotifyEvent
      read      FOnDeviceEvent
      write     FOnDeviceEvent;
  }
  end;

var StiCreateInstance : TStiCreateInstance = Nil;

implementation

{$IFDEF GE_DXE2}
uses  Vcl.Dialogs, System.SysUtils;
{$ELSE}
uses SysUtils, dialogs;
{$ENDIF}

{$IFOPT T+} {$DEFINE TYPED_ADDRESS_ON} {$T-} {$ENDIF}
{$IFOPT X-} {$DEFINE EXTENDED_SYNTAX} {$X+} {$ENDIF}

//------------------------------------------------------------------------------
// Generic "macro"
//------------------------------------------------------------------------------

function GET_STIVER_MAJOR(dwVersion : dword) : dword;
begin
  Result := HIWORD(dwVersion) and Not(STI_VERSION_FLAG_MASK);
end; // GET_STIVER_MAJOR.


function GET_STIVER_MINOR(dwVersion : dword) : dword;
begin
  Result := LOWORD(dwVersion);
end; // GET_STIVER_MINOR.


function GET_STIDEVICE_TYPE(dwDevType : dword) : dword;
begin
  Result := HIWORD(dwDevType);
end; // GET_STIDEVICE_TYPE.


function GET_STIDEVICE_SUBTYPE(dwDevType : dword) : dword;
begin
  Result := LOWORD(dwDevType);
end; // GET_STIDEVICE_SUBTYPE.


function GET_STIDCOMMON_CAPS(dwGenericCaps : dword) : dword;
begin
  Result := LOWORD(dwGenericCaps);
end; // GET_STIDCOMMON_CAPS.


function GET_STIVENDOR_CAPS(dwGenericCaps : dword) : dword;
begin
  Result := HIWORD(dwGenericCaps);
end; // GET_STIVENDOR_CAPS.


function IsDelphiApp : boolean;
var AppName : string;
begin
  // Can if the app instance is Delphi itself.
  AppName := ParamStr(0);
  AppName := StrUpper(PChar(AppName));
  //lstrcat(AppName, PChar(ParamStr(0)));  // ParamStr(0) = Application.ExeName
  // CharUpperBuff(AppName, SizeOf(AppName));
  Result := (Pos('DELPHI32.EXE', AppName) <> 0);
end; // IsDelphiApp.


constructor TmcmSTI.Create(AOwner : TComponent);
var hRes   : HRESULT;
    Module : HMODULE;
begin
  Inherited Create(AOwner);
  FStiObject := Nil;
  FActualDeviceName[0] := #0;
  FDeviceName[0] := #0;
  FEventName[0] := #0;

  if Not(csDesigning in ComponentState) and (mcmSTIDLL <> 0)
  then begin
       // Register application with STI
       Module := GetModuleHandle(Nil);
       hRes := StiCreateInstance(Module, STI_VERSION, FStiObject, Nil);
       if (hRes <> 0)
       then FStiObject := Nil;
  end;
end; // TmcmSTI.Create.


destructor TmcmSTI.Destroy;
begin
  //if Assigned(FStiObject)
  //then FStiObject._Release;
  FStiObject := Nil;
  Inherited Destroy;
end; // TmcmSTI.Destroy.


function TmcmSTI.LaunchedBySTI : string;
var hRes      : HRESULT;
    Value     : dword;
    ValueType : dword;
begin
  // Was this a STI launch?
  // Call STI-API and get the device name, event and event code
  FActualDeviceName[0] := #0;
  if (FStiObject <> Nil)
  then begin
       hRes := FStiObject.GetSTILaunchInformation(FDeviceName, FEventCode, FEventName);
       if (hRes = STI_OK)
       then begin
            Value := 256;
            FStiObject.GetDeviceValue(FDeviceName, STI_DEVICE_VALUE_TWAIN_NAME, ValueType, @FActualDeviceName, Value);
            GetDeviceInfo(FDeviceName);
       end;
  end;
  Result := StrPas(FActualDeviceName);
end; // TmcmSTI.LaunchedBySTI.


function TmcmSTI.RegisterApplication(Name : String; Path : String) : boolean;
var hRes : HRESULT;
begin
  if Assigned(FStiObject)
  then hRes := FStiObject.RegisterLaunchApplication(PChar(Name), PChar(Path))
  else hRes := STI_NOTCONNECTED;
  Result := (hRes = STI_OK);
end; // TmcmSTI.RegisterApplication.


function TmcmSTI.UnRegisterApplication(Name : String) : boolean;
var hRes : HRESULT;
begin
  // Un-register application with STI
  if Assigned(FStiObject)
  then hRes := FStiObject.UnregisterLaunchApplication(PChar(Name))
  else hRes := STI_NOTCONNECTED;
  Result := (hRes = STI_OK);
end; // TmcmSTI.UnRegisterApplication.


function TmcmSTI.GetDeviceInfo(DeviceName : string) : boolean;
var hRes        : HRESULT;
    hDeviceInfo : THandle;
    pDeviceInfo : PSTI_DEVICE_INFORMATIONW;
begin
  hRes := STI_NOTCONNECTED;
  // Was this a STI launch?
  // Call STI-API and get the device name, event and event code
  if (FStiObject <> Nil)
  then begin
       hRes := FStiObject.GetDeviceInfo(PChar(DeviceName), hDeviceInfo);
       if (hres = STI_OK)
       then begin
            pDeviceInfo := LocalLock(hDeviceInfo);
            CopyMemory(@FDeviceInfo, pDeviceInfo, SizeOf(TSTI_DEVICE_INFORMATIONW));
            LocalUnlock(hDeviceInfo);
            LocalFree(hDeviceInfo);
            FDeviceType := TSTI_DEVICE_MJ_TYPE(GET_STIDEVICE_TYPE(FDeviceInfo.DeviceType));
       end;
  end;
  Result := (hRes = STI_OK);
end; // TmcmSTI.GetDeviceInfo.


function TmcmSTI.GetDeviceName : string;
begin
  Result := FDeviceName;
end; // TmcmSTI.GetDeviceName.


function TmcmSTI.GetDeviceType : TSTI_DEVICE_MJ_TYPE;
begin
  Result := FDeviceType;
end; // TmcmSTI.GetDeviceType.


function TmcmSTI.GetEventName : string;
begin
  Result := FEventName;
end; // TmcmSTI.GetEventName.


function TmcmSTI.GetActualDeviceName : string;
begin
  Result := FActualDeviceName;
end; // TmcmSTI.GetActualDeviceName.


initialization
begin
  if Not(IsDelphiApp)
  then begin
       mcmSTIDLL := LoadLibrary('STI.DLL');
       if (mcmSTIDLL <> 0)
       then begin
            {$IFDEF UNICODE}
            @StiCreateInstance := GetProcAddress(mcmSTIDLL, 'StiCreateInstanceW');
            {$ELSE}
            @StiCreateInstance := GetProcAddress(mcmSTIDLL, 'StiCreateInstanceA');
            {$ENDIF}
       end;
  end;
end; // initialization.

finalization
begin
  if (mcmSTIDLL <> 0)
  then FreeLibrary(mcmSTIDLL);
  mcmSTIDLL := 0;
end; // finalization.

{$IFDEF TYPED_ADDRESS_ON} {$T+} {$UNDEF TYPED_ADDRESS_ON} {$ENDIF}
{$IFDEF EXTENDED_SYNTAX} {$X-} {$UNDEF EXTENDED_SYNTAX} {$ENDIF}

end.
