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
// $Log:  15902: twain.pas 
//
//    Rev 1.16    2014-03-28 17:52:52  mcm    Version: DT 4.1
// Added TWAIN 2.x support, and thereby support for Windows 7 & 8
//
//    Rev 1.15    2014-01-15 13:42:02  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//    Rev 1.14    2013-12-04 23:16:14  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//    Rev 1.13    08-01-2009 21:09:22  mcm    Version: DT 3.8
// Added support for Delphi 2009
//
//   Rev 1.12    11-06-2005 09:51:12  mcm    Version: DT 3.5
// Moved ICAP_COMPRESSION valuess to an enumeration.

//
//   Rev 1.11    19-02-2005 00:06:40  mcm    Version: DT 3.4
// Added ConfigureDS definition.

//
//   Rev 1.10    07-03-2004 13:55:28  mcm    Version: DT3.0
// Added TOnNegotiate.

//
//   Rev 1.9    21-11-2003 22:02:10  mcm    Version: DT3.0
// Added WM_CLEAREVENTS.

//
//   Rev 1.8    12-11-2003 14:03:46  mcm    Version: DT3.0
// Added two message constants.

//
//   Rev 1.7    06-11-2003 09:28:30  mcm    Version: DT3.0
// Added DefaultWait constant.

//
//   Rev 1.6    04-11-2003 20:14:40  mcm    Version: DT3.0
// Added thread message definitions.

//
//   Rev 1.5    06-07-2003 11:07:10  mcm    Version: DT 2.5

//
//   Rev 1.4    14-06-2003 10:38:30  mcm    Version: DT 2.4
// Added definition for notification event "DeviceNotReady".

//
//   Rev 1.3    17-05-2003 12:56:40  mcm
// Added support for SPIFF and EXIF.

//
//   Rev 1.2    06-03-2003 10:53:22  mcm    Version: DT 2.2
// Added conditional define to disable warnings on "Unsafe Type, Cast and Code"
// for Delphi 7.
// Added a missing parameter in the TW_PENDINGXFERS structure.

//
//   Rev 1.1    11-01-2002 15:11:54  mcm    Version: DT 2.0
// Minor fix for BCB.

//
//   Rev 1.0    04-12-2001 16:49:10  mcm    Version: DT 2.0

//------------------------------------------------------------------------------
// IMPORTANT NOTES:
// ----------------
// * Align Record Fields $A must be set to $A- in 32 bit (Delphi 2.0), in order
//   for the DSM_Entry and DS_Entry functions to pack parameters correctly on
//   the stack.
//
// * In the TWAIN manual chapter 6 page 1, it is noted that parameters must be
//   ordered on the stack in Pascal form, which is the reverse order that C
//   uses.
//   Now, looking at the delphi 2.0's on-line help, Calling conventions, you
//   would think that placing the directive "pascal" at the declaration of the
//   external functions DSM_Entry & DS_Entry would ensure correct placement of
//   the parameters on the stack. This is not so. The directive "stdcall" will
//   do the job. Whether the problem is in Delphi or the TWAIN_32.DLL file, who
//   knows !
//------------------------------------------------------------------------------

Unit twain;

{$IFDEF WIN32} {$A-} {$ELSE} {$A+} {$ENDIF}
{$F+}
{$N-}
{$R-}
{$W-}
{$X+}

{$IFNDEF TWAIN}
  {$DEFINE TWAIN}
{$ENDIF}
{$DEFINE WINDOWS}


interface

{$INCLUDE mcmDefines.pas}

{$IFDEF GE_DXE2}
uses WinApi.Windows, WinApi.Messages;
{$ELSE}
uses WinTypes, Messages;
{$ENDIF}

{$IFDEF MCMDEMO}
const
    mcmAppxName       = 'Application';
    mcmDelphixName    = 'Delphi';
    mcmBuilderxName   = 'Builder';
    mcmCxName         = 'C++';
    mcmAlignxName     = 'Align';
    mcmPalettexName   = 'Palette';
    mcmBuildAppxName  = 'AppBuilder';
{$ENDIF}


const
  TWN_VALID_HANDLE = 32;            // Valid windows handle SB >= 32.
{$IFOPT J-}{$DEFINE TYPED_CONSTANTS_ON}{$J+}{$ENDIF}
  {$IFDEF MCMDEMO}
  DefaultWait      : integer = 1;
  {$ELSE}
  DefaultWait      : integer = 60000;
  {$ENDIF}
{$IFDEF TYPED_CONSTANTS_ON}{$UNDEF TYPED_CONSTANTS_ON}{$J-}{$ENDIF}

const
  // Thread messages

  WM_OPENDSM      = WM_USER + 100;
  WM_CLOSEDSM     = WM_USER + 101;
  WM_OPENDS       = WM_USER + 102;
  WM_CLOSEDS      = WM_USER + 103;
  WM_ENABLEDS     = WM_USER + 104;
  WM_DISABLEDS    = WM_USER + 105;
  WM_CONFIGUREDS  = WM_USER + 106;
  WM_DSMENTRY     = WM_USER + 107;
  WM_IMAGEINFO    = WM_USER + 108;
  WM_IMAGELAYOUT  = WM_USER + 109;
  WM_NEXTXFER     = WM_USER + 110;
  WM_MCMWINRECT   = WM_USER + 111;
  WM_STARTUP      = WM_USER + 112;
  WM_CLEAREVENTS  = WM_USER + 113;

  WM_ONCLOSEREQUEST  = WM_USER + 140;
  WM_ONCLOSESOURCE   = WM_USER + 141;
  WM_ONDEVICEEVENT   = WM_USER + 142;
  WM_ONFAILURE       = WM_USER + 143;
  WM_ONSTATECHANGED  = WM_USER + 144;
  WM_ONXFERREADY     = WM_USER + 145;
  WM_ONFATALERROR    = WM_USER + 146;

  // TWAIN version
  TWON_PROTOCOLMINOR     = 9; // Changed for Version 1.9
  TWON_PROTOCOLMAJOR     = 1;

//------------------------------------------------------------------------------
// ICAP_IMAGEFILEFORMAT values (FF_means File Format)
type TTwnFileFmt  = (TWFF_TIFF,
                     TWFF_PICT,
                     TWFF_BMP,
                     TWFF_XBM,
                     TWFF_JFIF,
                     TWFF_FPX,
                     TWFF_TIFFMULTI,
                     TWFF_PNG,
                     TWFF_SPIFF,
                     TWFF_EXIF);
     TTwnFileFmts = set of TTwnFileFmt;

     // ICAP_COMPRESSION values (CP_ means ComPression)
     TTwnCompresFmt = (TWCP_NONE,
                       TWCP_PACKBITS,
                       TWCP_GROUP31D,    // Follows CCITT spec (no End Of Line)
                       TWCP_GROUP31DEOL, // Follows CCITT spec (has End Of Line)
                       TWCP_GROUP32D,    // Follows CCITT spec (use cap for K Factor)
                       TWCP_GROUP4,      // Follows CCITT spec
                       TWCP_JPEG,        // Use capability for more info
                       TWCP_LZW,         // Must license from Unisys and IBM to use
                       TWCP_JBIG,        // For Bitonal images  -- Added 1.7 KHL
                       // Added 1.8
                       TWCP_PNG,
                       TWCP_RLE4,
                       TWCP_RLE8,
                       TWCP_BITFIELDS);
     TTwnCompresFmts = set of TTwnCompresFmt;

     TTwnDeviceEvent = (TWDE_CHECKAUTOMATICCAPTURE,
                        TWDE_CHECKBATTERY,
                        TWDE_CHECKDEVICEONLINE,
                        TWDE_CHECKFLASH,
                        TWDE_CHECKPOWERSUPPLY,
                        TWDE_CHECKRESOLUTION,
                        TWDE_DEVICEADDED,
                        TWDE_DEVICEOFFLINE,
                        TWDE_DEVICEREADY,
                        TWDE_DEVICEREMOVED,
                        TWDE_IMAGECAPTURED,
                        TWDE_IMAGEDELETED,
                        TWDE_PAPERDOUBLEFEED,
                        TWDE_PAPERJAM,
                        TWDE_LAMPFAILURE,
                        TWDE_POWERSAVE,
                        TWDE_POWERSAVENOTIFY);
     TTwnDeviceEvents = set of TTwnDeviceEvent;

type
  TDeviceEvent      = procedure(    Sender       : TObject;
                                    Event        : TTwnDeviceEvent;
                                    DeviceName   : string;
                                    A, B, C      : Variant) of object;
  TImageEvent       = procedure(    Sender       : TObject;
                                    pBmp         : pointer;
                                    pBmpInfo     : PBitmapInfo;
                                    hImage       : hBitmap;
                                    FilePath     : string) of object;
  TMemXferSizeEvent = procedure(    Sender       : TObject;
                                    MinSize      : integer;
                                    MaxSize      : integer;
                                var BufSize      : integer;
                                    pBmpInfo     : PBitmapInfo) of object;
  TMemXferBufEvent  = procedure(    Sender       : TObject;
                                    pBmpInfo     : PBitmapInfo;
                                    BytesPerRow  : integer;
                                    Rows         : integer;
                                    DataSize     : integer;
                                var pData        : pointer) of object;
  TXferNextEvent    = procedure(    Sender       : TObject;
                                var NumImages    : integer;
                                var SkipNext     : boolean) of object;
  TProgressEvent    = procedure(    Sender       : TObject;
                                    Percent      : word) of object;
  TFailureEvent     = procedure(    Sender       : TObject;
                                    DG           : longint;
                                    DAT          : word;
                                    CAP          : word;
                                    MSG          : word;
                                    Error        : integer;
                                    Status       : integer) of object;
  TStateChanged     = procedure(    Sender       : TObject;
                                    State        : word) of object;
  TOnDeviceNotReady = procedure(    Sender       : TObject;
                                var DoOpenSource : boolean) of object;
  TOnNegotiate      = procedure(    Sender       : TObject;
                                var CancelScan   : boolean) of object;



//------------------------------------------------------------------------------
// Platform Dependent Definitions and Typedefs
//------------------------------------------------------------------------------

{$DEFINE MSWIN}
{$IFDEF MSWIN}
type
  TW_HANDLE = THandle;
  TW_MEMREF = Pointer;
{$ENDIF}

//------------------------------------------------------------------------------
// MCM DESIGN, Types and constants below does not exsist in pascal's WinTypes
// nor WinProcs, and are therefore included her.
//------------------------------------------------------------------------------
{$IFDEF VER100} {$DEFINE TWNDELPHI} {$ENDIF}
// {$IFDEF VER110} {$DEFINE TWNDELPHI} {$ENDIF}
{$IFDEF VER120} {$DEFINE TWNDELPHI} {$ENDIF}
// {$IFDEF VER125} {$DEFINE TWNDELPHI} {$ENDIF}

{$IFDEF TWNDELPHI}
  const HFILE_ERROR = -1;
        FILE_END    = 2;
{$ENDIF}
{$IFDEF TWNDELPHI} {$UNDEF TWNDELPHI} {$ENDIF}
{$IFDEF TWNDELPHI}
  type  HFILE       = word;
{$ENDIF}

{$IFOPT J-}{$DEFINE TYPED_CONSTANTS_ON}{$J+}{$ENDIF}
{$IFDEF MCMDEMO}
const pCR_LF      : array[0..2] of char = (chr($0B), chr($0A), #0);
{$ELSE}
const pCR_LF      : array[0..2] of char = (chr($0D), chr($0A), #0);
{$ENDIF}
{$IFDEF TYPED_CONSTANTS_ON}{$UNDEF TYPED_CONSTANTS_ON}{$J-}{$ENDIF}
var   CR_LF       : string;

//------------------------------------------------------------------------------
// Transfer mechanisme.
//------------------------------------------------------------------------------

type  TTwnXferType = (TWFX_NATIVE,
                      TWFX_FILES,
                      TWFX_MEMORY,
                      TWFX_FILES2);

//------------------------------------------------------------------------------
// Acquire Flag.
//------------------------------------------------------------------------------
const TWFG_NONE     = 0;
      TWFG_CLOSE    = 1;
      TWFG_DISABLE  = 2;
      TWFG_KEEPOPEN = 3;


//------------------------------------------------------------------------------
// Memory handle type def's.
//------------------------------------------------------------------------------
type  TTwnHdlType  = (THDT_DIBHANDLE,
                      THDT_DIBSEC,
                      THDT_DIBRAW,
                      THDT_MEMPTR);

      TDIBSecRec   = record
                     hMem     : THandle;
                     pBmp     : pointer;
                     pBmpInfo : PBitmapInfo;
                     end;
      PDIBSecRec   = ^TDIBSecRec;

//------------------------------------------------------------------------------
// Error message level flag.
//------------------------------------------------------------------------------

type TTwnErrorLevel = (ML_NONE, ML_ERROR, ML_INFO, ML_FULL);

//------------------------------------------------------------------------------
// Type Definitions
//------------------------------------------------------------------------------

// These types were added to simplify vector addressing.
// Using these types, prevents the use of range checking!
type TVectorB    = array[0..0] of byte;
     PVectorB    = ^TVectorB;
     TVectorW    = array[0..0] of word;
     PVectorW    = ^TVectorW;
     TVectorL    = array[0..0] of longint;
     PVectorL    = ^TVectorL;
     TVectorI    = array[0..0] of integer;
     PVectorI    = ^TVectorI;
     TLongType   = record
                   case Word of
       	           0  : (Ptr  : Pointer);
                   1  : (Long : Longint);
	           2  : (Lo   : Word;
	                 Hi   : Word);
                   end;

//------------------------------------------------------------------------------
// String types. These include room for the strings and a NULL char,
// or, on the Mac, a length byte followed by the string.
// TW_STR255 must hold less than 256 chars so length fits in first byte.
//------------------------------------------------------------------------------
Type
  TW_STR32    = array[0..33] of AnsiChar;
  pTW_STR32   = ^TW_STR32;
  TW_STR64    = array[0..65] of AnsiChar;
  pTW_STR64   = ^TW_STR64;
  TW_STR128   = array[0..129] of AnsiChar;
  pTW_STR128  = ^TW_STR128;
  TW_STR255   = array[0..256] of AnsiChar;
  pTW_STR255  = ^TW_STR255;
  // Added 1.9
  TW_STR1024  = array[0..1026] of AnsiChar; // Depricated
  pTW_STR1024 = ^TW_STR1024;
  TW_UNI512   = array[0..512] of WideChar;  // Depricated
  pTW_UNI512 = ^TW_UNI512;


Type
  // Numeric types.
  TW_INT8    = shortint;
  TW_INT16   = smallint;
  TW_INT32   = longint;
  TW_UINT8   = byte;
  TW_UINT16  = word;
  // {$IFDEF VER120}
  //  TW_UINT32  = longword;
  // {$ELSE}
    TW_UINT32  = longint; // Should be longword. Is typecasted to longword when
                          // necessary.
  // {$ENDIF}
  TW_BOOL    = smallint; // MCM 25.02.98 changed from bool;

  pTW_INT8   = ^TW_INT8;
  pTW_INT16  = ^TW_INT16;
  pTW_INT32  = ^TW_INT32;
  pTW_UINT8  = ^TW_UINT8;
  pTW_UINT16 = ^TW_UINT16;
  pTW_UINT32 = ^TW_UINT32;
  pTW_BOOL   = ^TW_BOOL;


  // Fixed point structure type.
  TW_FIX32 = record
             case Word of
             0 : (Long  : Longint);
             1 : (Whole : TW_INT16;
                  Frac  : TW_UINT16);
             end;

  pTW_FIX32  = ^TW_FIX32;

  TW_AINT8    = array[0..0] of shortint;
  TW_AINT16   = array[0..0] of smallint;
  TW_AINT32   = array[0..0] of longint;
  TW_AUINT8   = array[0..0] of byte;
  TW_AUINT16  = array[0..0] of word;
  // {$IFDEF VER120}
  //  TW_AUINT32  = array[0..0] of longword;
  // {$ELSE}
    TW_AUINT32  = array[0..0] of longint;
  // {$ENDIF}
  TW_ABOOL    = array[0..0] of smallint; // MCM 25.02.98 changed from bool;
  TW_AFIX32   = array[0..0] of TW_FIX32;
  TW_ASTR32   = array[0..0] of TW_STR32;
  TW_ASTR64   = array[0..0] of TW_STR64;
  TW_ASTR128  = array[0..0] of TW_STR128;
  TW_ASTR255  = array[0..0] of TW_STR255;

  pTW_AINT8   = ^TW_AINT8;
  pTW_AINT16  = ^TW_AINT16;
  pTW_AINT32  = ^TW_AINT32;
  pTW_AUINT8  = ^TW_AUINT8;
  pTW_AUINT16 = ^TW_AUINT16;
  pTW_AUINT32 = ^TW_AUINT32;
  pTW_ABOOL   = ^TW_ABOOL;
  pTW_AFIX32  = ^TW_AFIX32;
  pTW_ASTR32  = ^TW_ASTR32;
  pTW_ASTR64  = ^TW_ASTR64;
  pTW_ASTR128 = ^TW_ASTR128;
  pTW_ASTR255 = ^TW_ASTR255;


//------------------------------------------------------------------------------
// TW_VERSION
//------------------------------------------------------------------------------
  // No DAT needed. Describes version of software that's running.
  TW_VERSION = packed record
  MajorNum   : TW_UINT16;  // Major revision number of the software.
  MinorNum   : TW_UINT16;  // Incremental revision number of the software.
  Language   : TW_UINT16;  // e.g. TWLG_SWISSFRENCH
  Country    : TW_UINT16;  // e.g. TWCY_SWITZERLAND
  Info       : TW_STR32;   // e.g. "1.0b3 Beta release"
  end;
  pTW_VERSION = ^TW_VERSION;

//------------------------------------------------------------------------------
// DAT_IDENTITY
//------------------------------------------------------------------------------
  // DAT_IDENTITY. Identifies the program/library/code resource.
  TW_IDENTITY = packed record
  Id              : TW_UINT32;  // Unique number.  In Windows, app hWnd
  Version         : TW_VERSION; // Identifies the piece of code
  ProtocolMajor   : TW_UINT16;  // App and DS must set to TWON_PROTOCOLMAJOR
  ProtocolMinor   : TW_UINT16;  // App and DS must set to TWON_PROTOCOLMINOR
  SupportedGroups : TW_UINT32;  // Bit field OR combination of DG_ constants
  Manufacturer    : TW_STR32;   // Manufacturer name, e.g. "MCM-DESIGN"
  ProductFamily   : TW_STR32;   // Product family name, e.g. "SuperScanner"
  ProductName     : TW_STR32;   // Product name, e.g. "Scanner Plus"
  end;
  pTW_IDENTITY = ^TW_IDENTITY;


//------------------------------------------------------------------------------
// Entry Points
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Function: DSM_Entry, the only entry point into the Data Source Manager.
//
// Parameters:
// pOrigin Identifies the source module of the message. This could
//          identify an Application, a Source, or the Source Manager.
//
// pDest   Identifies the destination module for the message.
//         This could identify an application or a data source.
//         If this is NULL, the message goes to the Source Manager.
//
// DG      The Data Group.
//         Example: DG_IMAGE.
//
// DAT     The Data Attribute Type.
//         Example: DAT_IMAGEMEMXFER.
//
// MSG     The message.  Messages are interpreted by the destination module
//         with respect to the Data Group and the Data Attribute Type.
//         Example: MSG_GET.
// pData   A pointer to the data structure or variable identified
//         by the Data Attribute Type.
//
//         Example: (TW_MEMREF)&ImageMemXfer
//                  where ImageMemXfer is a TW_IMAGEMEMXFER structure.
//
// Returns:
// ReturnCode
//         Example: TWRC_SUCCESS.
//
//------------------------------------------------------------------------------

type
  TDSM_Entry_Proc = function(pOrigin : pTW_IDENTITY;
                             pDest   : pTW_IDENTITY;
                             DG      : TW_UINT32;
                             DAT     : TW_UINT16;
                             MSG     : TW_UINT16;
                             pData   : TW_MEMREF) : TW_UINT16; stdcall;

  pDSM_Entry_Proc = ^TDSM_Entry_Proc;


//------------------------------------------------------------------------------
// Function: DS_Entry, the entry point provided by a Data Source.
//
// Parameters:
// pOrigin Identifies the source module of the message. This could
//         identify an application or the Data Source Manager.
//
// DG      The Data Group.
//         Example: DG_IMAGE.
//
// DAT     The Data Attribute Type.
//         Example: DAT_IMAGEMEMXFER.
//
// MSG     The message.  Messages are interpreted by the data source
//         with respect to the Data Group and the Data Attribute Type.
//         Example: MSG_GET.
//
// pData   A pointer to the data structure or variable identified
//         by the Data Attribute Type.
//         Example: (TW_MEMREF)&ImageMemXfer
//                   where ImageMemXfer is a TW_IMAGEMEMXFER structure.
//
// Returns:
// ReturnCode
//         Example: TWRC_SUCCESS.
//
// Note:
// The DSPROC type is only used by an application when it calls
// a Data Source directly, bypassing the Data Source Manager.
//
//------------------------------------------------------------------------------

type
  TDS_Entry_Proc = function(pOrigin : pTW_IDENTITY;
                            DG      : TW_UINT32;
                            DAT     : TW_UINT16;
                            MSG     : TW_UINT16;
                            pData   : TW_MEMREF) : TW_UINT16; stdcall;

  pDS_Entry_Proc = ^TDS_Entry_Proc;

//------------------------------------------------------------------------------
// Structure Definitions
//------------------------------------------------------------------------------

type
  TDSM_MemAllocate = function(Size : TW_UINT32) : TW_HANDLE far stdcall;

  TDSM_MemFree = procedure(Handle : TW_HANDLE) far stdcall;

  TDSM_MemLock = function(Handle : TW_HANDLE) : TW_MEMREF far stdcall;

  TDSM_MemUnlock = procedure(Handle : TW_HANDLE) far stdcall;

  // DAT_ENTRYPOINT. returns essential entry points.
  TW_ENTRYPOINT = packed record
    Size : TW_UINT32;
    DSM_Entry : ^TDSM_Entry_Proc;
    DSM_MemAllocate : ^TDSM_MemAllocate;
    DSM_MemFree : ^TDSM_MemFree;
    DSM_MemLock : ^TDSM_MemLock;
    DSM_MemUnlock : ^TDSM_MemUnlock;
  end;
  pTW_ENTRYPOINT = ^TW_ENTRYPOINT;


  // No DAT needed.
  TW_CIEPOINT = packed record
                X : TW_FIX32;
                Y : TW_FIX32;
                Z : TW_FIX32;
              end;
  pTW_CIEPOINT = ^TW_CIEPOINT;


  // No DAT needed.
  TW_DECODEFUNCTION = packed record
                      StartIn     : TW_FIX32;
                      BreakIn     : TW_FIX32;
                      EndIn       : TW_FIX32;
                      StartOut    : TW_FIX32;
                      BreakOut    : TW_FIX32;
                      EndOut      : TW_FIX32;
                      Gamma       : TW_FIX32;
                      SampleCount : TW_FIX32;  // if = 0 use the gamma.
                      end;
 pTW_DECODEFUNCTION = ^TW_DECODEFUNCTION;


  // No DAT needed.
  TW_ELEMENT8 = packed record
  Index       : TW_UINT8; // Value used to index into the color table.
  Channel1    : TW_UINT8; // First  tri-stimulus value (e.g Red)
  Channel2    : TW_UINT8; // Second tri-stimulus value (e.g Green)
  Channel3    : TW_UINT8; // Third  tri-stimulus value (e.g Blue)
  end;
  pTW_ELEMENT8 = ^TW_ELEMENT8;


  // No DAT. Defines a frame rectangle in ICAP_UNITS coordinates.
  TW_FRAME = packed record
  Left     : TW_FIX32;
  Top      : TW_FIX32;
  Right    : TW_FIX32;
  Bottom   : TW_FIX32;
  end;
  pTW_FRAME  = ^TW_FRAME;
  TW_AFRAME  = array[0..0] of TW_FRAME;
  pTW_AFRAME = ^TW_AFRAME;


  // No DAT needed. Used to manage memory buffers.
  TW_MEMORY = packed record
  Flags     : TW_UINT32; // Any combination of the TWMF_ constants.
  Length    : TW_UINT32; // Number of bytes stored in buffer TheMem.
  TheMem    : TW_MEMREF; // Pointer or handle to the allocated memory buffer.
  end;
  pTW_MEMORY = ^TW_MEMORY;


  // No DAT needed.
  TW_TRANSFORMSTAGE = packed record
  Decode            : array[0..2] of TW_DECODEFUNCTION;
  Mix               : array[0..2,0..2] of TW_FIX32;
  end;
  pTW_TRANSFORMSTAGE = ^TW_TRANSFORMSTAGE;


  // TWON_ARRAY. Container for array of values (a simplified TW_ENUMERATION)
  TW_ARRAY = packed record
  ItemType : TW_UINT16;
  NumItems : TW_UINT32;                // How many items in ItemList
  ItemList : array[0..0] of TW_UINT32; // Array of ItemType values starts here.
  end;
  pTW_ARRAY = ^TW_ARRAY;


  // TWON_ENUMERATION. Container for a collection of values.
  TW_ENUMERATION = packed record
  ItemType       : TW_UINT16;
  NumItems       : TW_UINT32; // How many items in ItemList
  CurrentIndex   : TW_UINT32; // Current value is in ItemList[CurrentIndex]
  DefaultIndex   : TW_UINT32; // Powerup value is in ItemList[DefaultIndex]
  ItemList       : array[0..0] of TW_UINT32; // Array of ItemType values starts
                                             // here.
  end;
  pTW_ENUMERATION = ^TW_ENUMERATION;


  // TWON_ONEVALUE. Container for one value.
  TW_ONEVALUE = packed record
  ItemType    : TW_UINT16;
  Item        : TW_UINT32;
  end;
  pTW_ONEVALUE = ^TW_ONEVALUE;


  // TWON_RANGE. Container for a range of values.
  TW_RANGE = packed record
  ItemType     : TW_UINT16;
  MinValue     : TW_UINT32; // Starting value in the range.
  MaxValue     : TW_UINT32; // Final value in the range.
  StepSize     : TW_UINT32; // Increment from MinValue to MaxValue.
  DefaultValue : TW_UINT32; // Power-up value.
  CurrentValue : TW_UINT32; // The value that is currently in effect.
  end;
  pTW_RANGE = ^TW_RANGE;


  // DAT_CAPABILITY. Used by app to get/set capability from/in a data source.
  TW_CAPABILITY = packed record
  Cap        : TW_UINT16; // id of capability to set or get, e.g CAP_BRIGHTNESS
  ConType    : TW_UINT16; // TWON_ONEVALUE, RANGE, ENUMERATION or_ARRAY.
  hContainer : TW_HANDLE; // Handle to container of type Dat
  end;
  pTW_CAPABILITY = ^TW_CAPABILITY;


  // DAT_CIECOLOR.
  TW_CIECOLOR = packed record
  ColorSpace      : TW_UINT16;
  LowEndian       : TW_INT16;
  DeviceDependent : TW_INT16;
  VersionNumber   : TW_INT32;
  StageABC        : TW_TRANSFORMSTAGE;
  StageLMN        : TW_TRANSFORMSTAGE;
  WhitePoint      : TW_CIEPOINT;
  BlackPoint      : TW_CIEPOINT;
  WhitePaper      : TW_CIEPOINT;
  BlackInk        : TW_CIEPOINT;
  Samples         : array[0..0] of TW_FIX32;
  end;
  pTW_CIECOLOR = ^TW_CIECOLOR;


  // DAT_EVENT. For passing events down from the app to the DS.
  TW_EVENT  = packed record
  pEvent    : TW_MEMREF; // Windows pMSG or Mac pEvent.
  TWMessage : TW_UINT16; // TW msg from data source, MSG_XFERREADY
  end;
  pTW_EVENT = ^TW_EVENT;


  // DAT_GRAYRESPONSE
  TW_GRAYRESPONSE = packed record
  Response        : array[0..0] of TW_ELEMENT8;
  end;
  pTW_GRAYRESPONSE = ^TW_GRAYRESPONSE;


  // DAT_IMAGEINFO. App gets detailed image info from DS with this.
  TW_IMAGEINFO = packed record
  XResolution     : TW_FIX32;  // Resolution in the horizontal
  YResolution     : TW_FIX32;  // Resolution in the vertical
  ImageWidth      : TW_INT32;  // Columns in the image, -1 if unknown by DS
  ImageLength     : TW_INT32;  // Rows in the image, -1 if unknown by DS
  SamplesPerPixel : TW_INT16;  // Number of samples per pixel, 3 for RGB
  BitsPerSample   : array[0..7] of TW_INT16; // Number of bits for each sample
  BitsPerPixel    : TW_INT16;  // Number of bits for each padded pixel
  Planar          : TW_BOOL;   // True if Planar, False if chunky
  PixelType       : TW_INT16;  // How to interp data; photo interp (TWPT_)
  Compression     : TW_UINT16; // How the data is compressed (TWCP_xxxx)
  end;
  pTW_IMAGEINFO = ^TW_IMAGEINFO;


  // DAT_IMAGELAYOUT. Provides image layout information in current units.
  TW_IMAGELAYOUT = packed record
  Frame          : TW_FRAME;  // Frame coords within larger document
  DocumentNumber : TW_UINT32;
  PageNumber     : TW_UINT32; // Reset when you go to next document
  FrameNumber    : TW_UINT32; // Reset when you go to next page
  end;
  pTW_IMAGELAYOUT = ^TW_IMAGELAYOUT;


  // DAT_IMAGEMEMXFER. Used to pass image data (e.g. in strips) from DS to app.
  TW_IMAGEMEMXFER = packed record
  Compression     : TW_UINT16; // How the data is compressed
  BytesPerRow     : TW_UINT32; // Number of bytes in a row of data
  Columns         : TW_UINT32; // How many columns
  Rows            : TW_UINT32; // How many rows
  XOffset         : TW_UINT32; // How far from the side of the image
  YOffset         : TW_UINT32; // How far from the top of the image
  BytesWritten    : TW_UINT32; // How many bytes written in Memory
  Memory          : TW_MEMORY; // Mem struct used to pass actual image data
  end;
  pTW_IMAGEMEMXFER = ^TW_IMAGEMEMXFER;


  // DAT_JPEGCOMPRESSION. Based on JPEG Draft International Std, ver 10918-1.
  TW_JPEGCOMPRESSION = packed record
  ColorSpace         : TW_UINT16; // One of the TWPT_xxxx values
  SubSampling        : TW_UINT32; // Two word "array" for subsampling values
  NumComponents      : TW_UINT16; // Number of color components in image
  RestartFrequency   : TW_UINT16; // Frequency of restart marker codes in MDU's
  QuantMap           : array[0..3] of TW_UINT16; // Mapping of components to
                                                 // QuantTables
  QuantTable         : array[0..3] of TW_MEMORY; // Quantization tables
  HuffmanMap         : array[0..3] of TW_UINT16; // Mapping of components to
                                                 // Huffman tables
  HuffmanDC          : array[0..1] of TW_MEMORY; // DC Huffman tables
  HuffmanAC          : array[0..1] of TW_MEMORY; // AC Huffman tables
  end;
  pTW_JPEGCOMPRESSION = ^TW_JPEGCOMPRESSION;


  // DAT_PALETTE8. Color palette when TWPT_PALETTE pixels xfer'd in mem buf.
  TW_PALETTE8 = packed record
  NumColors   : TW_UINT16;   // Number of colors in the color table.
  PaletteType : TW_UINT16;   // TWPA_xxxx, specifies type of palette.
  Colors      : array[0..255] of TW_ELEMENT8; // Array of palette values starts
                                              // here.
  end;
  pTW_PALETTE8 = ^TW_PALETTE8;


  // DAT_PENDINGXFERS. Used with MSG_ENDXFER to indicate additional data.
  TW_PENDINGXFERS = packed record
  Count           : TW_UINT16; // Number of additional "images" pending.
  EOJ             : TW_UINT32;
  Reserved        : TW_UINT32;
  end;
  pTW_PENDINGXFERS = ^TW_PENDINGXFERS;


  // DAT_RGBRESPONSE
  TW_RGBRESPONSE = packed record
  Response       : array[0..0] of TW_ELEMENT8;
  end;
  pRGBRESPONSE = ^TW_RGBRESPONSE;


  // DAT_SETUPFILEXFER. Sets up DS to app data transfer via a file.
  TW_SETUPFILEXFER = packed record
  FileName         : TW_STR255;
  Format           : TW_UINT16; // Any TWFF_ constant
  VRefNum          : TW_INT16;  // Used for Mac only
  end;
  pTW_SETUPFILEXFER = ^TW_SETUPFILEXFER;


  // DAT_SETUPFILEXFER2. Sets up DS to app data transfer via a file.  // Depricated
  // Added 1.9
  TW_SETUPFILEXFER2 = packed record
  FileName         : TW_MEMREF; // Pointer to file name text
  FileNameType     : TW_UINT16; // TWTY_STR1024 or TWTY_UNI512
  Format           : TW_UINT16; // Any TWFF_ constant
  VRefNum          : TW_INT16;  // Used for Mac only
  parID            : TW_UINT32; // Used for Mac only
  end;
  pTW_SETUPFILEXFER2 = ^TW_SETUPFILEXFER2;


  // DAT_SETUPMEMXFER. Sets up DS to app data transfer via a memory buffer.
  TW_SETUPMEMXFER = packed record
  MinBufSize : TW_UINT32;
  MaxBufSize : TW_UINT32;
  Preferred  : TW_UINT32;
  end;
  pTW_SETUPMEMXFER = ^TW_SETUPMEMXFER;


  // DAT_STATUS. App gets detailed status info from a data source with this.
  TW_STATUS = packed record
  ConditionCode : TW_UINT16; // Any TWCC_ constant
  Reserved      : TW_UINT16; // Future expansion space
  end;
  pTW_STATUS = ^TW_STATUS;


  // DAT_USERINTERFACE. Coordinates UI between app and data source.
  TW_USERINTERFACE = packed record
  ShowUI  : TW_BOOL;   // TRUE if DS should bring up its UI
  ModalUI : TW_BOOL;   // For Mac only - true if the DS's UI is modal
  hParent : TW_HANDLE; // For windows only - App window handle
  end;
  pTW_USERINTERFACE = ^TW_USERINTERFACE;


(* Depricated
  // DAT_TWUNKIDENTITY. Provides DS identity and 'other' information necessary
  // across thunk link.
  TW_TWUNKIDENTITY  = packed record
                      identity : TW_IDENTITY; // Identity of data source.
                      dsPath   : TW_STR255;   // Full path and file name of
                                              // data source.
                      end;
  pTW_TWUNKIDENTITY  = ^TW_TWUNKIDENTITY;
*)

(* Depricated
  // Provides DS_Entry parameters over thunk link.
  TW_TWUNKDSENTRYPARAMS = packed record
                          destFlag    : TW_INT8;     // TRUE if dest is not NULL
                          dest        : TW_IDENTITY; // Identity of data source (if used)
                          dataGroup   : TW_INT32;    // DSM_Entry dataGroup parameter
                          dataArgType : TW_INT16;    // DSM_Entry dataArgType parameter
                          message     : TW_INT16;    // DSM_Entry message parameter
                          pDataSize   : TW_INT32;    // Size of pData (0 if NULL)
                          { pData      : TW_MEMREF; } // Based on implementation specifics,
                                                     // a pData parameter makes no sense
                                                     // in this structure, but data (if
                                                     // provided) will be appended in the
                                                     // data block.
   end;
   pTW_TWUNKDSENTRYPARAMS = ^TW_TWUNKDSENTRYPARAMS;
*)

(* Depricated
  // Provides DS_Entry results over thunk link.
  TW_TWUNKDSENTRYRETURN = packed record
                          returnCode    : TW_UINT16; // Thunker DsEntry return code.
                          conditionCode : TW_UINT16; // Thunker DsEntry condition code.
                          pDataSize     : TW_INT32;  // Size of pData (0 if NULL)
                          {pData  TW_MEMREF   pData;}// Based on implementation specifics
                                                     // a pData parameter makes no sense
                                                     // in this structure, but data (if
                                                     // provided) will be appended in the
                                                     // data block.
  end;
  pTW_TWUNKDSENTRYRETURN = ^TW_TWUNKDSENTRYRETURN;
*)

(* Depricated
  // Added for 1.6 Specification
  // TWAIN 1.6 CAP_SUPPORTEDCAPSEXT structure
  TW_CAPEXT = packed record
	      Cap        : TW_UINT16; // Which CAP/ICAP info is relevant to
	      Properties : TW_UINT16; // Messages this CAP/ICAP supports
              end;
  pTW_CAPEXT = ^TW_CAPEXT;
*)

//------------------------------------------------------------------------------
//  Version 1.7:      Added Following data structure for Document Imaging
//  July 1997         Enhancement.
//
//  KHL               TW_CUSTOMDSDATA -  For Saving and Restoring Source's
//                                       state.
//
//                    TW_INFO         -  Each attribute for extended image
//                                       information.
//
//                    TW_EXTIMAGEINFO -  Extended image information structure.
//------------------------------------------------------------------------------

  TW_CUSTOMDSDATA = packed record
                    InfoLength : TW_UINT32; // Length of Information in bytes.
                    hData      : TW_HANDLE; // Place holder for data, DS
                                            // Allocates
                    end;
  pTW_CUSTOMDSDATA = ^TW_CUSTOMDSDATA;

  TW_INFO = packed record
            InfoID   : TW_UINT16;
            ItemType : TW_UINT16;
            NumItems : TW_UINT16;
            CondCode : TW_UINT16;
            Item     : TW_UINT32;
            end;
  pTW_INFO = ^TW_INFO;

  TW_EXTIMAGEINFO = packed record
                    NumInfos : TW_UINT32;
                    Info     : array[0..0] of TW_INFO;
                    end;
  pTW_EXTIMAGEINFO = ^TW_EXTIMAGEINFO;


//------------------------------------------------------------------------------
// Added 1.8
//------------------------------------------------------------------------------

  // DAT_AUDIOINFO, information about audio data
  TW_AUDIOINFO = packed record
                 Name     : TW_STR255; // name of audio data
                 Reserved : TW_UINT32; // reserved space
                 end;
  pTW_AUDIOINFO = ^TW_AUDIOINFO;

  // DAT_DEVICEEVENT, information about events }
  TW_DEVICEEVENT = packed record
                   Event             : TW_UINT32; // One of the TWDE_xxxx values
                   DeviceName        : TW_STR255; // The name of the device
                                                  // that generated the event
                   BatteryMinutes    : TW_UINT32; // Battery Minutes Remaining
                   BatteryPercentage : TW_INT16;  // Battery Percentage
                                                  // Remaining
                   PowerSupply       : TW_INT32;  // Power Supply
                   XResolution       : TW_FIX32;  // Resolution
                   YResolution       : TW_FIX32;  // Resolution
                   FlashUsed2        : TW_UINT32; // Flash Used2
                   AutomaticCapture  : TW_UINT32; // Automatic Capture
                   TimeBeforeFirstCapture : TW_UINT32; // Automatic Capture
                   TimeBetweenCaptures    : TW_UINT32; // Automatic Capture
                   end;
  pTW_DEVICEEVENT = ^TW_DEVICEEVENT;

  // DAT_FILESYSTEM, information about TWAIN file system
  TW_FILESYSTEM = packed record
                  // DG_CONTROL / DAT_FILESYSTEM / MSG_xxxx fields
                  InputName        : TW_STR255; // The name of the input or
                                                // source file
                  OutputName       : TW_STR255; // The result of an operation
                                                // or the name of a destination
                                                // file.
                  Context          : TW_MEMREF; // Source specific data used to
                                                // remember state information
                  // DG_CONTROL / DAT_FILESYSTEM / MSG_DELETE field
                  Recursive        : integer;   // recursively delete all sub-
                                                // directories
                  // DG_CONTROL / DAT_FILESYSTEM / MSG_GETINFO fields
                  FileType         : TW_INT32;  // One of the TWFT_xxxx values
                  Size             : TW_UINT32; // Size of current FileType
                  CreateTimeDate   : TW_STR32;  // creation date of the file
                  ModifiedTimeDate : TW_STR32;  // last date the file was
                                                // modified
                  FreeSpace        : TW_UINT32; // bytes of free space on the
                                                // current device.
                  NewImageSize     : TW_INT32;  // estimate of the amount of
                                                // space a new image would take
                                                // up.
                  NumberOfFiles    : TW_UINT32; // number of files, depends on
                                                // FileType.
                  NumberOfSnippets : TW_UINT32;
                  Reserved         : array[0..511] of char;
                  end;
  pTW_FILESYSTEM = ^TW_FILESYSTEM;

  // DAT_PASSTHRU, device dependant data to pass through Data Source.
  TW_PASSTHRU = packed record
                pCommand        : TW_MEMREF; // Pointer to Command buffer
                CommandBytes    : TW_UINT32; // Number of bytes in Command
                                             // buffer.
                Direction       : TW_INT32;  // One of the TWDR_xxxx values.
                                             // Defines the direction of data
                                             // flow.
                pData           : TW_MEMREF; // Pointer to Data buffer.
                DataBytes       : TW_UINT32; // Number of bytes in Data buffer.
                DataBytesXfered : TW_UINT32; // Number of bytes successfully
                                             // transferred.
                end;
  pTW_PASSTHRU = ^TW_PASSTHRU;

(* Depricated  
  // DAT_SETUPAUDIOFILEXFER, information required to setup an audio file
  // transfer.
  TW_SETUPAUDIOFILEXFER = packed record
                          FileName : TW_STR255; // full path target file
                          Format   : TW_UINT16; // one of TWAF_xxxx
                          VRefNum  : TW_INT16;
                          end;
   pTW_SETUPAUDIOFILEXFER = ^TW_SETUPAUDIOFILEXFER;
*)

//------------------------------------------------------------------------------
// Generic Constants
//------------------------------------------------------------------------------

const
  TWON_ARRAY             = 3;   // indicates TW_ARRAY container
  TWON_ENUMERATION       = 4;   // indicates TW_ENUMERATION container
  TWON_ONEVALUE          = 5;   // indicates TW_ONEVALUE container
  TWON_RANGE             = 6;   // indicates TW_RANGE container

  TWON_ICONID            = 962; // res Id of icon used in USERSELECT lbox
  TWON_DSMID             = 461; // res Id of the DSM version num resource
  TWON_DSMCODEID         = 63;  // res Id of the Mac SM Code resource

  TWON_DONTCARE8         = $0FF;
  TWON_DONTCARE16        = $0FFFF;
  TWON_DONTCARE32        = $0FFFFFFFF;


  // Flags used in TW_MEMORY structure.
  TWMF_APPOWNS           = $01;
  TWMF_DSMOWNS           = $02;
  TWMF_DSOWNS            = $04;
  TWMF_POINTER           = $08;
  TWMF_HANDLE            = $010;


  // Palette types for TW_PALETTE8
  TWPA_RGB               = 0;
  TWPA_GRAY              = 1;
  TWPA_CMY               = 2;


//------------------------------------------------------------------------------
// There are four containers used for capabilities negotiation:
// TWON_ONEVALUE, TWON_RANGE, TWON_ENUMERATION, TWON_ARRAY
// In each container structure ItemType can be TWTY_INT8, TWTY_INT16, etc.
// The kind of data stored in the container can be determined by doing
// TWItemSize[ItemType] where the following is defined in TWAIN glue code:
//
// TWItemSize[]= sizeof(TW_INT8),
//               sizeof(TW_INT16),
//               etc.
//               sizeof(TW_UINT32);
//
//------------------------------------------------------------------------------
const
  TWTY_INT8              = $00000;    // Means Item is a TW_INT8
  TWTY_INT16             = $00001;    // Means Item is a TW_INT16
  TWTY_INT32             = $00002;    // Means Item is a TW_INT32

  TWTY_UINT8             = $00003;    // Means Item is a TW_UINT8
  TWTY_UINT16            = $00004;    // Means Item is a TW_UINT16
  TWTY_UINT32            = $00005;    // Means Item is a TW_UINT32

  TWTY_BOOL              = $00006;    // Means Item is a TW_BOOL

  TWTY_FIX32             = $00007;    // Means Item is a TW_FIX32

  TWTY_FRAME             = $00008;    // Means Item is a TW_FRAME

  TWTY_STR32             = $00009;    // Means Item is a TW_STR32
  TWTY_STR64             = $0000A;    // Means Item is a TW_STR64
  TWTY_STR128            = $0000B;    // Means Item is a TW_STR128
  TWTY_STR255            = $0000C;    // Means Item is a TW_STR255
  TWTY_STR1024           = $0000D;    // Means Item is a TW_STR1024, Added 1.9
  TWTY_UNI512            = $0000E;    // Means Item is a TW_UNI512, Added 1.9

  // TWTY_SPECIAL           = $0002A;
  TWTY_MEMREF            = $0002B;
  TWTY_HANDLE            = $0002C;
  TWTY_ELEMENT8          = $0002D;
  // TWTY_LIST              = $0002E;


const TWItemSize  : array[0..14] of integer = (SizeOf(TW_INT8),
                                               SizeOf(TW_INT16),
	                                       SizeOf(TW_INT32),
	                                       SizeOf(TW_UINT8),
	                                       SizeOf(TW_UINT16),
	                                       SizeOf(TW_UINT32),
	                                       SizeOf(TW_BOOL),
	                                       SizeOf(TW_FIX32),
	                                       SizeOf(TW_FRAME),
	                                       SizeOf(TW_STR32),
	                                       SizeOf(TW_STR64),
	                                       SizeOf(TW_STR128),
	                                       SizeOf(TW_STR255),
                                               SizeOf(TW_STR1024),
                                               SizeOf(TW_UNI512));


//------------------------------------------------------------------------------
// Capability Constants
//------------------------------------------------------------------------------
const

  // ICAP_BITORDER values (BO_ means Bit Order)
  TWBO_LSBFIRST          = 0;
  TWBO_MSBFIRST          = 1;


  // ICAP_COMPRESSION values (CP_ means ComPression)
  (*
  TWCP_NONE              = 0;
  TWCP_PACKBITS          = 1;
  TWCP_GROUP31D          = 2; // Follows CCITT spec (no End Of Line)
  TWCP_GROUP31DEOL       = 3; // Follows CCITT spec (has End Of Line)
  TWCP_GROUP32D          = 4; // Follows CCITT spec (use cap for K Factor)
  TWCP_GROUP4            = 5; // Follows CCITT spec
  TWCP_JPEG              = 6; // Use capability for more info
  TWCP_LZW               = 7; // Must license from Unisys and IBM to use
  TWCP_JBIG              = 8; // For Bitonal images  -- Added 1.7 KHL
  // Added 1.8
  TWCP_PNG               = 9;
  TWCP_RLE4              = 10;
  TWCP_RLE8              = 11;
  TWCP_BITFIELDS         = 12;
  *)

const
  (*
  TWFF_TIFF              = 0; // Tagged Image File Format
  TWFF_PICT              = 1; // Macintosh PICT
  TWFF_BMP               = 2; // Windows Bitmap
  TWFF_XBM               = 3; // X-Windows Bitmap
  TWFF_JFIF              = 4; // JPEG File Interchange Format
  TWFF_FPX               = 5; // FlashPix, (Kodak) used with digital cameras.
  TWFF_TIFFMULTI         = 6; // Multi-page TIFF files.
  TWFF_PNG               = 7; // Portable Network Graphics.
  TWFF_SPIFF             = 8; //
  TWFF_EXIF              = 9; //
  *)
  TWFF_NONE              = 1000;

  // ICAP_FILTER values (FT_ means Filter Type)
  TWFT_RED               = 0;
  TWFT_GREEN             = 1;
  TWFT_BLUE              = 2;
  TWFT_NONE              = 3;
  TWFT_WHITE             = 4;
  TWFT_CYAN              = 5;
  TWFT_MAGENTA           = 6;
  TWFT_YELLOW            = 7;
  TWFT_BLACK             = 8;

  // ICAP_LIGHTPATH values (LP_ means Light Path)
  TWLP_REFLECTIVE        = 0;
  TWLP_TRANSMISSIVE      = 1;

  // ICAP_LIGHTSOURCE values (LS_ means Light Source)
  TWLS_RED               = 0;
  TWLS_GREEN             = 1;
  TWLS_BLUE              = 2;
  TWLS_NONE              = 3;
  TWLS_WHITE             = 4;
  TWLS_UV                = 5;
  TWLS_IR                = 6;

  // ICAP_ORIENTATION values (OR_ means ORientation)
  TWOR_ROT0              = 0;
  TWOR_ROT90             = 1;
  TWOR_ROT180            = 2;
  TWOR_ROT270            = 3;
  TWOR_PORTRAIT          = TWOR_ROT0;
  TWOR_LANDSCAPE         = TWOR_ROT270;

  // ICAP_PLANARCHUNKY values (PC_ means Planar/Chunky )
  TWPC_CHUNKY            = 0;
  TWPC_PLANAR            = 1;

  // ICAP_PIXELFLAVOR values (PF_ means Pixel Flavor)
  TWPF_CHOCOLATE         = 0;  // zero pixel represents darkest shade.
  TWPF_VANILLA           = 1;  // zero pixel represents lightest shade.

  // ICAP_PIXELTYPE values (PT_ means Pixel Type)
  TWPT_BW                = 0; // Black and White
  TWPT_GRAY              = 1;
  TWPT_RGB               = 2;
  TWPT_PALETTE           = 3;
  TWPT_CMY               = 4;
  TWPT_CMYK              = 5;
  TWPT_YUV               = 6;
  TWPT_YUVK              = 7;
  TWPT_CIEXYZ            = 8;

  // ICAP_SUPPORTEDSIZES values (SS_ means Supported Sizes)
  TWSS_NONE              = 0;
  TWSS_A4LETTER          = 1;
  TWSS_B5LETTER          = 2;
  TWSS_USLETTER          = 3;
  TWSS_USLEGAL           = 4;
  // Added 1.5
  TWSS_A5                = 5;
  TWSS_B4                = 6;
  TWSS_B6                = 7;
  // TWSS_B              = 8;
  // Added 1.7
  TWSS_USLEDGER          = 9;
  TWSS_USEXECUTIVE       = 10;
  TWSS_A3                = 11;
  TWSS_B3                = 12;
  TWSS_A6                = 13;
  TWSS_C4                = 14;
  TWSS_C5                = 15;
  TWSS_C6                = 16;
  // Added 1.8
  TWSS_4A0               = 17;
  TWSS_2A0               = 18;
  TWSS_A0                = 19;
  TWSS_A1                = 20;
  TWSS_A2                = 21;
  TWSS_A4                = TWSS_A4LETTER;
  TWSS_A7                = 22;
  TWSS_A8                = 23;
  TWSS_A9                = 24;
  TWSS_A10               = 25;
  TWSS_ISOB0             = 26;
  TWSS_ISOB1             = 27;
  TWSS_ISOB2             = 28;
  TWSS_ISOB3             = TWSS_B3;
  TWSS_ISOB4             = TWSS_B4;
  TWSS_ISOB5             = 29;
  TWSS_ISOB6             = TWSS_B6;
  TWSS_ISOB7             = 30;
  TWSS_ISOB8             = 31;
  TWSS_ISOB9             = 32;
  TWSS_ISOB10            = 33;
  TWSS_JISB0             = 34;
  TWSS_JISB1             = 35;
  TWSS_JISB2             = 36;
  TWSS_JISB3             = 37;
  TWSS_JISB4             = 38;
  TWSS_JISB5             = TWSS_B5LETTER;
  TWSS_JISB6             = 39;
  TWSS_JISB7             = 40;
  TWSS_JISB8             = 41;
  TWSS_JISB9             = 42;
  TWSS_JISB10            = 43;
  TWSS_C0                = 44;
  TWSS_C1                = 45;
  TWSS_C2                = 46;
  TWSS_C3                = 47;
  TWSS_C7                = 48;
  TWSS_C8                = 49;
  TWSS_C9                = 50;
  TWSS_C10               = 51;
  TWSS_USSTATEMENT       = 52;
  TWSS_BUSINESSCARD      = 53;

  // ICAP_XFERMECH values (SX_ means Setup XFer)
  TWSX_NATIVE            = 0;
  TWSX_FILE              = 1;
  TWSX_MEMORY            = 2;
  //TWSX_FILE2             = 3; // Added 1.9 // Depricated

  // ICAP_UNITS values (UN_ means UNits)
  TWUN_INCHES            = 0;
  TWUN_CENTIMETERS       = 1;
  TWUN_PICAS             = 2;
  TWUN_POINTS            = 3;
  TWUN_TWIPS             = 4;
  TWUN_PIXELS            = 5;

  // Added 1.5
  // ICAP_BITDEPTHREDUCTION values (BR_ means Bitdepth Reduction)
  TWBR_THRESHOLD         = 0;
  TWBR_HALFTONE          = 1;
  TWBR_CUSTHALFTONE      = 2;
  TWBR_DIFFUSION         = 3;

  // Added 1.7
  // CAP_DUPLEX values
  TWDX_NONE              = 0;
  TWDX_1PASSDUPLEX       = 1;
  TWDX_2PASSDUPLEX       = 2;

  // Added 1.7
  // TWEI_BARCODETYPE values
  TWBT_3OF9                 = 0;
  TWBT_2OF5INTERLEAVED      = 1;
  TWBT_2OF5NONINTERLEAVED   = 2;
  TWBT_CODE93               = 3;
  TWBT_CODE128              = 4;
  TWBT_UCC128               = 5;
  TWBT_CODABAR              = 6;
  TWBT_UPCA                 = 7;
  TWBT_UPCE                 = 8;
  TWBT_EAN8                 = 9;
  TWBT_EAN13                = 10;
  TWBT_POSTNET              = 11;
  TWBT_PDF417               = 12;
  // Added 1.8
  TWBT_2OF5INDUSTRIAL       = 13;
  TWBT_2OF5MATRIX           = 14;
  TWBT_2OF5DATALOGIC        = 15;
  TWBT_2OF5IATA             = 16;
  TWBT_3OF9FULLASCII        = 17;
  TWBT_CODABARWITHSTARTSTOP = 18;
  TWBT_MAXICODE             = 19;

  // Added 1.7
  // TWEI_DESKEWSTATUS values
  TWDSK_SUCCESS          = 0;
  TWDSK_REPORTONLY       = 1;
  TWDSK_FAIL             = 2;
  TWDSK_DISABLED         = 3;

  // Added 1.7
  // TWEI_PATCHCODE values
  TWPCH_PATCH1           = 0;
  TWPCH_PATCH2           = 1;
  TWPCH_PATCH3           = 2;
  TWPCH_PATCH4           = 3;
  TWPCH_PATCH6           = 4;
  TWPCH_PATCHT           = 5;

  // Added 1.7 }
  // CAP_JOBCONTROL values
  TWJC_NONE              = 0;
  TWJC_JSIC              = 1;
  TWJC_JSIS              = 2;
  TWJC_JSXC              = 3;
  TWJC_JSXS              = 4;

  // Added 1.7
  // TWEI_BARCODEROTATION values (BCOR_ means barcode rotation)
  TWBCOR_ROT0            = 0;
  TWBCOR_ROT90           = 1;
  TWBCOR_ROT180          = 2;
  TWBCOR_ROT270          = 3;
  TWBCOR_ROTX            = 4;

(* Depricated  
  // Added 1.8
  // ACAP_AUDIOFILEFORMAT values (AF_ means audio format)
  TWAF_WAV               = 0;
  TWAF_AIFF              = 1;
  TWAF_AU                = 3;
  TWAF_SND               = 4;
*)

  // CAP_ALARMS values (AL_ means alarms)
  TWAL_ALARM             = 0;
  TWAL_FEEDERERROR       = 1;
  TWAL_FEEDERWARNING     = 2;
  TWAL_BARCODE           = 3;
  TWAL_DOUBLEFEED        = 4;
  TWAL_JAM               = 5;
  TWAL_PATCHCODE         = 6;
  TWAL_POWER             = 7;
  TWAL_SKEW              = 8;

  // CAP_CLEARBUFFERS values (CB_ means clear buffers)
  TWCB_AUTO              = 0;
  TWCB_CLEAR             = 1;
  TWCB_NOCLEAR           = 2;

  // CAP_DEVICEEVENT values (DE_ means device event)
  TWDE_CUSTOMEVENTS           = $08000;
  // See TTwnDeviceEvent.
  (*
  TWDE_CHECKAUTOMATICCAPTURE  = 0;
  TWDE_CHECKBATTERY           = 1;
  TWDE_CHECKDEVICEONLINE      = 2;
  TWDE_CHECKFLASH             = 3;
  TWDE_CHECKPOWERSUPPLY       = 4;
  TWDE_CHECKRESOLUTION        = 5;
  TWDE_DEVICEADDED            = 6;
  TWDE_DEVICEOFFLINE          = 7;
  TWDE_DEVICEREADY            = 8;
  TWDE_DEVICEREMOVED          = 9;
  TWDE_IMAGECAPTURED          = 10;
  TWDE_IMAGEDELETED           = 11;
  TWDE_PAPERDOUBLEFEED        = 12;
  TWDE_PAPERJAM               = 13;
  TWDE_LAMPFAILURE            = 14;
  TWDE_POWERSAVE              = 15;
  TWDE_POWERSAVENOTIFY        = 16;
  *)

  // CAP_FEEDERALIGNMENT values (FA_ means feeder alignment)
  TWFA_NONE                   = 0;
  TWFA_LEFT                   = 1;
  TWFA_CENTER                 = 2;
  TWFA_RIGHT                  = 3;

  // CAP_FEEDERORDER values (FO_ means feeder order)
  TWFO_FIRSTPAGEFIRST         = 0;
  TWFO_LASTPAGEFIRST          = 1;

  // CAP_FILESYSTEM values (FS_ means file system)
  TWFS_FILESYSTEM             = 0;
  TWFS_RECURSIVEDELETE        = 1;

  // CAP_POWERSUPPLY values (PS_ means power supply)
  TWPS_EXTERNAL               = 0;
  TWPS_BATTERY                = 1;

  // CAP_PRINTER values (PR_ means printer)
  TWPR_IMPRINTERTOPBEFORE     = 0;
  TWPR_IMPRINTERTOPAFTER      = 1;
  TWPR_IMPRINTERBOTTOMBEFORE  = 2;
  TWPR_IMPRINTERBOTTOMAFTER   = 3;
  TWPR_ENDORSERTOPBEFORE      = 4;
  TWPR_ENDORSERTOPAFTER       = 5;
  TWPR_ENDORSERBOTTOMBEFORE   = 6;
  TWPR_ENDORSERBOTTOMAFTER    = 7;

  // CAP_PRINTERMODE values (PM_ means printer mode)
  TWPM_SINGLESTRING           = 0;
  TWPM_MULTISTRING            = 1;
  TWPM_COMPOUNDSTRING         = 2;

  // ICAP_BARCODESEARCHMODE values (SRCH_ means search)
  SRCH_HORZ                   = 0;
  SRCH_VERT                   = 1;
  SRCH_HORZVERT               = 2;
  SRCH_VERTHORZ               = 3;

  // ICAP_FLASHUSED2 values (FL_ means flash)
  TWFL_NONE                   = 0;
  TWFL_OFF                    = 1;
  TWFL_ON                     = 2;
  TWFL_AUTO                   = 3;
  TWFL_REDEYE                 = 4;

  // ICAP_FLIPROTATION values (FR_ means flip rotation)
  TWFR_BOOK                   = 0;
  TWFR_FANFOLD                = 1;

  // ICAP_IMAGEFILTER values (IF_ means image filter)
  TWIF_NONE                   = 0;
  TWIF_AUTO                   = 1;
  TWIF_LOWPASS                = 2;
  TWIF_BANDPASS               = 3;
  TWIF_HIGHPASS               = 4;
  TWIF_TEXT                   = TWIF_BANDPASS;
  TWIF_FINELINE               = TWIF_HIGHPASS;

  // ICAP_NOISEFILTER values (NF_ means noise filter)
  TWNF_NONE                   = 0;
  TWNF_AUTO                   = 1;
  TWNF_LONEPIXEL              = 2;
  TWNF_MAJORITYRULE           = 3;

  // ICAP_OVERSCAN values (OV_ means overscan)
  TWOV_NONE                   = 0;
  TWOV_AUTO                   = 1;
  TWOV_TOPBOTTOM              = 2;
  TWOV_LEFTRIGHT              = 3;
  TWOV_ALL                    = 4;

  // TW_FILESYSTEM.FileType values (FT_ means file type)
  TWFY_CAMERA                 = 0;
  TWFY_CAMERATOP              = 1;
  TWFY_CAMERABOTTOM           = 2;
  TWFY_CAMERAPREVIEW          = 3;
  TWFY_DOMAIN                 = 4;
  TWFY_HOST                   = 5;
  TWFY_DIRECTORY              = 6;
  TWFY_IMAGE                  = 7;
  TWFY_UNKNOWN                = 8;

  // ICAP_JPEGQUALITY values (JQ_ means jpeg quality)
  // Added 1.9
  TWJQ_UNKNOWN                = -4;
  TWJQ_LOW                    = -3;
  TWJQ_MEDIUM                 = -2;
  TWJQ_HIGH                   = -1;

//------------------------------------------------------------------------------
// Country Constants
//------------------------------------------------------------------------------

  TWCY_AFGHANISTAN       = 1001;
  TWCY_ALGERIA           =  213;
  TWCY_AMERICANSAMOA     =  684;
  TWCY_ANDORRA           =  033;
  TWCY_ANGOLA            = 1002;
  TWCY_ANGUILLA          = 8090;
  TWCY_ANTIGUA           = 8091;
  TWCY_ARGENTINA         =   54;
  TWCY_ARUBA             =  297;
  TWCY_ASCENSIONI        =  247;
  TWCY_AUSTRALIA         =   61;
  TWCY_AUSTRIA           =   43;
  TWCY_BAHAMAS           = 8092;
  TWCY_BAHRAIN           =  973;
  TWCY_BANGLADESH        =  880;
  TWCY_BARBADOS          = 8093;
  TWCY_BELGIUM           =   32;
  TWCY_BELIZE            =  501;
  TWCY_BENIN             =  229;
  TWCY_BERMUDA           = 8094;
  TWCY_BHUTAN            = 1003;
  TWCY_BOLIVIA           =  591;
  TWCY_BOTSWANA          =  267;
  TWCY_BRITAIN           =    6;
  TWCY_BRITVIRGINIS      = 8095;
  TWCY_BRAZIL            =   55;
  TWCY_BRUNEI            =  673;
  TWCY_BULGARIA          =  359;
  TWCY_BURKINAFASO       = 1004;
  TWCY_BURMA             = 1005;
  TWCY_BURUNDI           = 1006;
  TWCY_CAMAROON          =  237;
  TWCY_CANADA            =    2;
  TWCY_CAPEVERDEIS       =  238;
  TWCY_CAYMANIS          = 8096;
  TWCY_CENTRALAFREP      = 1007;
  TWCY_CHAD              = 1008;
  TWCY_CHILE             =   56;
  TWCY_CHINA             =   86;
  TWCY_CHRISTMASIS       = 1009;
  TWCY_COCOSIS           = 1009;
  TWCY_COLOMBIA          =   57;
  TWCY_COMOROS           = 1010;
  TWCY_CONGO             = 1011;
  TWCY_COOKIS            = 1012;
  TWCY_COSTARICA         =  506;
  TWCY_CUBA              =  005;
  TWCY_CYPRUS            =  357;
  TWCY_CZECHOSLOVAKIA    =   42;
  TWCY_DENMARK           =   45;
  TWCY_DJIBOUTI          = 1013;
  TWCY_DOMINICA          = 8097;
  TWCY_DOMINCANREP       = 8098;
  TWCY_EASTERIS          = 1014;
  TWCY_ECUADOR           =  593;
  TWCY_EGYPT             =   20;
  TWCY_ELSALVADOR        =  503;
  TWCY_EQGUINEA          = 1015;
  TWCY_ETHIOPIA          =  251;
  TWCY_FALKLANDIS        = 1016;
  TWCY_FAEROEIS          =  298;
  TWCY_FIJIISLANDS       =  679;
  TWCY_FINLAND           =  358;
  TWCY_FRANCE            =   33;
  TWCY_FRANTILLES        =  596;
  TWCY_FRGUIANA          =  594;
  TWCY_FRPOLYNEISA       =  689;
  TWCY_FUTANAIS          = 1043;
  TWCY_GABON             =  241;
  TWCY_GAMBIA            =  220;
  TWCY_GERMANY           =   49;
  TWCY_GHANA             =  233;
  TWCY_GIBRALTER         =  350;
  TWCY_GREECE            =   30;
  TWCY_GREENLAND         =  299;
  TWCY_GRENADA           = 8099;
  TWCY_GRENEDINES        = 8015;
  TWCY_GUADELOUPE        =  590;
  TWCY_GUAM              =  671;
  TWCY_GUANTANAMOBAY     = 5399;
  TWCY_GUATEMALA         =  502;
  TWCY_GUINEA            =  224;
  TWCY_GUINEABISSAU      = 1017;
  TWCY_GUYANA            =  592;
  TWCY_HAITI             =  509;
  TWCY_HONDURAS          =  504;
  TWCY_HONGKONG          =  852;
  TWCY_HUNGARY           =   36;
  TWCY_ICELAND           =  354;
  TWCY_INDIA             =   91;
  TWCY_INDONESIA         =   62;
  TWCY_IRAN              =   98;
  TWCY_IRAQ              =  964;
  TWCY_IRELAND           =  353;
  TWCY_ISRAEL            =  972;
  TWCY_ITALY             =   39;
  TWCY_IVORY_COAST       =  225;
  TWCY_JAMAICA           = 8010;
  TWCY_JAPAN             =   81;
  TWCY_JORDAN            =  962;
  TWCY_KENYA             =  254;
  TWCY_KIRIBATI          = 1018;
  TWCY_KOREA             =   82;
  TWCY_KUWAIT            =  965;
  TWCY_LAOS              = 1019;
  TWCY_LEBANON           = 1020;
  TWCY_LIBERIA           =  231;
  TWCY_LIBYA             =  218;
  TWCY_LIECHTENSTEIN     =   41;
  TWCY_LUXENBOURG        =  352;
  TWCY_MACAO             =  853;
  TWCY_MADAGASCAR        = 1021;
  TWCY_MALAWI            =  265;
  TWCY_MALAYSIA          =   60;
  TWCY_MALDIVES          =  960;
  TWCY_MALI              = 1022;
  TWCY_MALTA             =  356;
  TWCY_MARSHALLIS        =  692;
  TWCY_MAURITANIA        = 1023;
  TWCY_MAURITIUS         =  230;
  TWCY_MEXICO            =    3;
  TWCY_MICRONESIA        =  691;
  TWCY_MIQUELON          =  508;
  TWCY_MONACO            =   33;
  TWCY_MONGOLIA          = 1024;
  TWCY_MONTSERRAT        = 8011;
  TWCY_MOROCCO           =  212;
  TWCY_MOZAMBIQUE        = 1025;
  TWCY_NAMIBIA           =  264;
  TWCY_NAURU             = 1026;
  TWCY_NEPAL             =  977;
  TWCY_NETHERLANDS       =   31;
  TWCY_NETHANTILLES      =  599;
  TWCY_NEVIS             = 8012;
  TWCY_NEWCALEDONIA      =  687;
  TWCY_NEWZEALAND        =   64;
  TWCY_NICARAGUA         =  505;
  TWCY_NIGER             =  227;
  TWCY_NIGERIA           =  234;
  TWCY_NIUE              = 1027;
  TWCY_NORFOLKI          = 1028;
  TWCY_NORWAY            =   47;
  TWCY_OMAN              =  968;
  TWCY_PAKISTAN          =   92;
  TWCY_PALAU             = 1029;
  TWCY_PANAMA            =  507;
  TWCY_PARAGUAY          =  595;
  TWCY_PERU              =   51;
  TWCY_PHILLIPPINES      =   63;
  TWCY_PITCAIRNIS        = 1030;
  TWCY_PNEWGUINEA        =  675;
  TWCY_POLAND            =   48;
  TWCY_PORTUGAL          =  351;
  TWCY_QATAR             =  974;
  TWCY_REUNIONI          = 1031;
  TWCY_ROMANIA           =   40;
  TWCY_RWANDA            =  250;
  TWCY_SAIPAN            =  670;
  TWCY_SANMARINO         =  039;
  TWCY_SAOTOME           = 1033;
  TWCY_SAUDIARABIA       =  966;
  TWCY_SENEGAL           =  221;
  TWCY_SEYCHELLESIS      = 1034;
  TWCY_SIERRALEONE       = 1035;
  TWCY_SINGAPORE         =   65;
  TWCY_SOLOMONIS         = 1036;
  TWCY_SOMALI            = 1037;
  TWCY_SOUTH_AFRICA      =   27;
  TWCY_SPAIN             =   34;
  TWCY_SRILANKA          =   94;
  TWCY_STHELENA          = 1032;
  TWCY_STKITTS           = 8013;
  TWCY_STLUCIA           = 8014;
  TWCY_STPIERRE          =  508;
  TWCY_STVINCENT         = 8015;
  TWCY_SUDAN             = 1038;
  TWCY_SURINAME          =  597;
  TWCY_SWAZILAND         =  268;
  TWCY_SWEDEN            =   46;
  TWCY_SWITZERLAND       =   41;
  TWCY_SYRIA             = 1039;
  TWCY_TAIWAN            =  886;
  TWCY_TANZANIA          =  255;
  TWCY_THAILAND          =   66;
  TWCY_TOBAGO            = 8016;
  TWCY_TOGO              =  228;
  TWCY_TONGAIS           =  676;
  TWCY_TRINIDAD          = 8016;
  TWCY_TUNISIA           =  216;
  TWCY_TURKEY            =   90;
  TWCY_TURKSCAICOS       = 8017;
  TWCY_TUVALU            = 1040;
  TWCY_UGANDA            =  256;
  TWCY_USSR              =    7;
  TWCY_UAEMIRATES        =  971;
  TWCY_UNITEDKINGDOM     =   44;
  TWCY_USA               =    1;
  TWCY_URUGUAY           =  598;
  TWCY_VANUATU           = 1041;
  TWCY_VATICANCITY       =   39;
  TWCY_VENEZUELA         =   58;
  TWCY_WAKE              = 1042;
  TWCY_WALLISIS          = 1043;
  TWCY_WESTERNSAHARA     = 1044;
  TWCY_WESTERNSAMOA      = 1045;
  TWCY_YEMEN             = 1046;
  TWCY_YUGOSLAVIA        =   38;
  TWCY_ZAIRE             =  243;
  TWCY_ZAMBIA            =  260;
  TWCY_ZIMBABWE          =  263;
  // Added for 1.8
  TWCY_ALBANIA           = 355;
  TWCY_ARMENIA           = 374;
  TWCY_AZERBAIJAN        = 994;
  TWCY_BELARUS           = 375;
  TWCY_BOSNIAHERZGO      = 387;
  TWCY_CAMBODIA          = 855;
  TWCY_CROATIA           = 385;
  TWCY_CZECHREPUBLIC     = 420;
  TWCY_DIEGOGARCIA       = 246;
  TWCY_ERITREA           = 291;
  TWCY_ESTONIA           = 372;
  TWCY_GEORGIA           = 995;
  TWCY_LATVIA            = 371;
  TWCY_LESOTHO           = 266;
  TWCY_LITHUANIA         = 370;
  TWCY_MACEDONIA         = 389;
  TWCY_MAYOTTEIS         = 269;
  TWCY_MOLDOVA           = 373;
  TWCY_MYANMAR           = 95;
  TWCY_NORTHKOREA        = 850;
  TWCY_PUERTORICO        = 787;
  TWCY_RUSSIA            = 7;
  TWCY_SERBIA            = 381;
  TWCY_SLOVAKIA          = 421;
  TWCY_SLOVENIA          = 386;
  TWCY_SOUTHKOREA        = 82;
  TWCY_UKRAINE           = 380;
  TWCY_USVIRGINIS        = 340;
  TWCY_VIETNAM           = 84;


//------------------------------------------------------------------------------
// Language Constants
//------------------------------------------------------------------------------

  TWLG_DAN               =  0; // Danish
  TWLG_DUT               =  1; // Dutch
  TWLG_ENG               =  2; // International English
  TWLG_FCF               =  3; // French Canadian
  TWLG_FIN               =  4; // Finnish
  TWLG_FRN               =  5; // French
  TWLG_GER               =  6; // German
  TWLG_ICE               =  7; // Icelandic
  TWLG_ITN               =  8; // Italian
  TWLG_NOR               =  9; // Norwegian
  TWLG_POR               = 10; // Portuguese
  TWLG_SPA               = 11; // Spanish
  TWLG_SWE               = 12; // Swedish
  TWLG_USA               = 13; // U.S. English
  // Added for 1.8
  TWLG_USERLOCALE           = -1;
  TWLG_AFRIKAANS            = 14;
  TWLG_ALBANIA              = 15;
  TWLG_ARABIC               = 16;
  TWLG_ARABIC_ALGERIA       = 17;
  TWLG_ARABIC_BAHRAIN       = 18;
  TWLG_ARABIC_EGYPT         = 19;
  TWLG_ARABIC_IRAQ          = 20;
  TWLG_ARABIC_JORDAN        = 21;
  TWLG_ARABIC_KUWAIT        = 22;
  TWLG_ARABIC_LEBANON       = 23;
  TWLG_ARABIC_LIBYA         = 24;
  TWLG_ARABIC_MOROCCO       = 25;
  TWLG_ARABIC_OMAN          = 26;
  TWLG_ARABIC_QATAR         = 27;
  TWLG_ARABIC_SAUDIARABIA   = 28;
  TWLG_ARABIC_SYRIA         = 29;
  TWLG_ARABIC_TUNISIA       = 30;
  TWLG_ARABIC_UAE           = 31; // United Arabic Emirates
  TWLG_ARABIC_YEMEN         = 32;
  TWLG_BASQUE               = 33;                                                     
  TWLG_BYELORUSSIAN         = 34;
  TWLG_BULGARIAN            = 35;
  TWLG_CATALAN              = 36;
  TWLG_CHINESE              = 37;
  TWLG_CHINESE_HONGKONG     = 38;
  TWLG_CHINESE_PRC          = 39; // People's Republic of China
  TWLG_CHINESE_SINGAPORE    = 40;
  TWLG_CHINESE_SIMPLIFIED   = 41;
  TWLG_CHINESE_TAIWAN       = 42;
  TWLG_CHINESE_TRADITIONAL  = 43;
  TWLG_CROATIA              = 44;
  TWLG_CZECH                = 45;
  TWLG_DANISH               = TWLG_DAN;
  TWLG_DUTCH                = TWLG_DUT;
  TWLG_DUTCH_BELGIAN        = 46;
  TWLG_ENGLISH              = TWLG_ENG;
  TWLG_ENGLISH_AUSTRALIAN   = 47;
  TWLG_ENGLISH_CANADIAN     = 48;
  TWLG_ENGLISH_IRELAND      = 49;
  TWLG_ENGLISH_NEWZEALAND   = 50;
  TWLG_ENGLISH_SOUTHAFRICA  = 51;
  TWLG_ENGLISH_UK           = 52;
  TWLG_ENGLISH_USA          = TWLG_USA;
  TWLG_ESTONIAN             = 53;
  TWLG_FAEROESE             = 54;
  TWLG_FARSI                = 55;
  TWLG_FINNISH              = TWLG_FIN;
  TWLG_FRENCH               = TWLG_FRN;
  TWLG_FRENCH_BELGIAN       = 56;
  TWLG_FRENCH_CANADIAN      = TWLG_FCF;
  TWLG_FRENCH_LUXEMBOURG    = 57;
  TWLG_FRENCH_SWISS         = 58;
  TWLG_GERMAN               = TWLG_GER;
  TWLG_GERMAN_AUSTRIAN      = 59;
  TWLG_GERMAN_LUXEMBOURG    = 60;
  TWLG_GERMAN_LIECHTENSTEIN = 61;
  TWLG_GERMAN_SWISS         = 62;
  TWLG_GREEK                = 63;
  TWLG_HEBREW               = 64;
  TWLG_HUNGARIAN            = 65;
  TWLG_ICELANDIC            = TWLG_ICE;
  TWLG_INDONESIAN           = 66;
  TWLG_ITALIAN              = TWLG_ITN;
  TWLG_ITALIAN_SWISS        = 67;
  TWLG_JAPANESE             = 68;
  TWLG_KOREAN               = 69;
  TWLG_KOREAN_JOHAB         = 70;
  TWLG_LATVIAN              = 71;
  TWLG_LITHUANIAN           = 72;
  TWLG_NORWEGIAN            = TWLG_NOR;
  TWLG_NORWEGIAN_BOKMAL     = 73;
  TWLG_NORWEGIAN_NYNORSK    = 74;
  TWLG_POLISH               = 75;
  TWLG_PORTUGUESE           = TWLG_POR;
  TWLG_PORTUGUESE_BRAZIL    = 76;
  TWLG_ROMANIAN             = 77;
  TWLG_RUSSIAN              = 78;
  TWLG_SERBIAN_LATIN        = 79;
  TWLG_SLOVAK               = 80;
  TWLG_SLOVENIAN            = 81;
  TWLG_SPANISH              = TWLG_SPA;
  TWLG_SPANISH_MEXICAN      = 82;
  TWLG_SPANISH_MODERN       = 83;
  TWLG_SWEDISH              = TWLG_SWE;
  TWLG_THAI                 = 84;
  TWLG_TURKISH              = 85;
  TWLG_UKRANIAN             = 86;

  // More stuff added for 1.8
  TWLG_ASSAMESE             = 87;
  TWLG_BENGALI              = 88;
  TWLG_BIHARI               = 89;
  TWLG_BODO                 = 90;
  TWLG_DOGRI                = 91;
  TWLG_GUJARATI             = 92;
  TWLG_HARYANVI             = 93;
  TWLG_HINDI                = 94;
  TWLG_KANNADA              = 95;
  TWLG_KASHMIRI             = 96;
  TWLG_MALAYALAM            = 97;
  TWLG_MARATHI              = 98;
  TWLG_MARWARI              = 99;
  TWLG_MEGHALAYAN           = 100;
  TWLG_MIZO                 = 101;
  TWLG_NAGA                 = 102;
  TWLG_ORISSI               = 103;
  TWLG_PUNJABI              = 104;
  TWLG_PUSHTU               = 105;
  TWLG_SERBIAN_CYRILLIC     = 106;
  TWLG_SIKKIMI              = 107;
  TWLG_SWEDISH_FINLAND      = 108;
  TWLG_TAMIL                = 109;
  TWLG_TELUGU               = 110;
  TWLG_TRIPURI              = 111;
  TWLG_URDU                 = 112;
  TWLG_VIETNAMESE           = 113;

//------------------------------------------------------------------------------
// Data Groups
//------------------------------------------------------------------------------

const

//------------------------------------------------------------------------------
// More Data Groups may be added in the future.
// Possible candidates include text, vector graphics, sound, etc.
// NOTE: Data Group constants must be powers of 2 as they are used
//       as bitflags when App asks DSM to present a list of DSs.
//------------------------------------------------------------------------------
  DG_CONTROL             = $00001; // data pertaining to control
  DG_IMAGE               = $00002; // data pertaining to raster images
  // Added 1.8
  DG_AUDIO               = $00004; // data pertaining to audio

// More Data Functionality may be added in the future.
// These are for items that need to be determined before DS is opened.
// NOTE: Supported Functionality constants must be powers of 2 as they are
//       used as bitflags when Application asks DSM to present a list of DSs.
//       to support backward capability the App and DS will not use the fields

  DF_DSM2                = $010000000;
  DF_APP2                = $020000000;
  DF_DS2                 = $040000000;                                     
  DG_MASK                = $0FFFF;    

//------------------------------------------------------------------------------
// Data Argument Types
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// The thunker requires knowledge about size of data being passed in the
// lpData parameter to DS_Entry (which is not readily available due to
// type LPVOID.  Thus, we key off the DAT_ argument to determine the size.
// This has a couple implications:
// 1) Any additional DAT_ features require modifications to the thunk code
//    for thunker support.
// 2) Any applications which use the custom capabailites are not supported
//    under thunking since we have no way of knowing what size data (if
//    any) is being passed.
//------------------------------------------------------------------------------

  DAT_NULL               = $00000; // No data or structure.
  DAT_CUSTOMBASE         = $08000; // Base of custom DATs.

  // Data Argument Types for the DG_CONTROL Data Group.
  DAT_CAPABILITY         = $00001; // TW_CAPABILITY
  DAT_EVENT              = $00002; // TW_EVENT
  DAT_IDENTITY           = $00003; // TW_IDENTITY
  DAT_PARENT             = $00004; // TW_HANDLE, app win handle in Windows
  DAT_PENDINGXFERS       = $00005; // TW_PENDINGXFERS
  DAT_SETUPMEMXFER       = $00006; // TW_SETUPMEMXFER
  DAT_SETUPFILEXFER      = $00007; // TW_SETUPFILEXFER
  DAT_STATUS             = $00008; // TW_STATUS
  DAT_USERINTERFACE      = $00009; // TW_USERINTERFACE
  DAT_XFERGROUP          = $0000A; // TW_UINT32
  // TWUNK
  // Additional message required for thunker to request the special
  // identity information.
  // DAT_TWUNKIDENTITY      = $0000B; // TW_TWUNKIDENTITY // Depricated
  DAT_CUSTOMDSDATA       = $0000C; // TW_CUSTOMDSDATA.
  // Added 1.8
  DAT_DEVICEEVENT        = $0000D; // TW_DEVICEEVENT
  DAT_FILESYSTEM         = $0000E; // TW_FILESYSTEM
  DAT_PASSTHRU           = $0000F; // TW_PASSTHRU

  DAT_CALLBACK           = $00010; //
  DAT_STATUSUTF8         = $00011; //
  DAT_CALLBACK2          = $00012; //

  // Data Argument Types for the DG_IMAGE Data Group.
  DAT_IMAGEINFO          = $00101; // TW_IMAGEINFO
  DAT_IMAGELAYOUT        = $00102; // TW_IMAGELAYOUT
  DAT_IMAGEMEMXFER       = $00103; // TW_IMAGEMEMXFER
  DAT_IMAGENATIVEXFER    = $00104; // TW_UINT32 loword is hDIB, PICHandle
  DAT_IMAGEFILEXFER      = $00105; // Null data
  DAT_CIECOLOR           = $00106; // TW_CIECOLOR
  DAT_GRAYRESPONSE       = $00107; // TW_GRAYRESPONSE
  DAT_RGBRESPONSE        = $00108; // TW_RGBRESPONSE
  DAT_JPEGCOMPRESSION    = $00109; // TW_JPEGCOMPRESSION
  DAT_PALETTE8           = $0010A; // TW_PALETTE8
  DAT_EXTIMAGEINFO       = $0010B; // TW_EXTIMAGEINFO -- for 1.7 Spec.
  DAT_FILTER             = $0010C; // 

  // Added 1.8
  // Data Argument Types for the DG_AUDIO Data Group.
  DAT_AUDIOFILEXFER      = $00201; // Null data
  DAT_AUDIOINFO          = $00202; // TW_AUDIOINFO
  DAT_AUDIONATIVEXFER    = $00203; // TW_UINT32 handle to WAV, (AIFF Mac)
  // Added 1.9
  DAT_SETUPFILEXFER2     = $00301; // New file xfer operation // Depricated

  // misplaced 
  DAT_ICCPROFILE         = $00401; //
  DAT_IMAGEMEMFILEXFER   = $00402; //
  DAT_ENTRYPOINT         = $00403; //
  
//------------------------------------------------------------------------------
// Messages
//------------------------------------------------------------------------------

const

//------------------------------------------------------------------------------
// All message constants are unique.
// Messages are grouped according to which DATs they are used with.
//------------------------------------------------------------------------------

  MSG_NULL               = $00000; // Used in TW_EVENT structure
  MSG_CUSTOMBASE         = $08000; // Base of custom messages

  // Generic messages may be used with any of several DATs.
  MSG_GET                = $00001; // Get one or more values
  MSG_GETCURRENT         = $00002; // Get current value
  MSG_GETDEFAULT         = $00003; // Get default (e.g. power up) value
  MSG_GETFIRST           = $00004; // Get first of a series of items, e.g. DSs
  MSG_GETNEXT            = $00005; // Iterate through a series of items.
  MSG_SET                = $00006; // Set one or more values
  MSG_RESET              = $00007; // Set current value to default value
  MSG_QUERYSUPPORT       = $00008; // Get supported operations on the cap.
  MSG_GETHELP            = $00009; //
  MSG_GETLABEL           = $0000a; //
  MSG_GETLABELENUM       = $0000b; //
  MSG_SETCONSTRAINT      = $0000c; //

  // Messages used with DAT_NULL
  MSG_XFERREADY          = $00101; // The data source has data ready
  MSG_CLOSEDSREQ         = $00102; // Request for App. to close DS
  MSG_CLOSEDSOK          = $00103; // Tell the App. to save the state.
  // Added 1.8
  MSG_DEVICEEVENT        = $00104; // Some event has taken place

  // Messages used with a pointer to a DAT_STATUS structure
  MSG_CHECKSTATUS        = $00201; // Get status information

  // Messages used with a pointer to DAT_PARENT data
  MSG_OPENDSM            = $00301; // Open the DSM
  MSG_CLOSEDSM           = $00302; // Close the DSM

  // Messages used with a pointer to a DAT_IDENTITY structure
  MSG_OPENDS             = $00401; // Open a data source
  MSG_CLOSEDS            = $00402; // Close a data source
  MSG_USERSELECT         = $00403; // Put up a dialog of all DS

  // Messages used with a pointer to a DAT_USERINTERFACE structure
  MSG_DISABLEDS          = $00501; // Disable data transfer in the DS
  MSG_ENABLEDS           = $00502; // Enable data transfer in the DS
  MSG_ENABLEDSUIONLY     = $00503; // Enable for saving DS state only.

  // Messages used with a pointer to a DAT_EVENT structure
  MSG_PROCESSEVENT       = $00601; //

  // Messages used with a pointer to a DAT_PENDINGXFERS structure
  MSG_ENDXFER            = $00701;
  MSG_STOPFEEDER         = $00702;

  // Added 1.8
  // Messages used with a pointer to a DAT_FILESYSTEM structure
  MSG_CHANGEDIRECTORY    = $00801;
  MSG_CREATEDIRECTORY    = $00802;
  MSG_DELETE             = $00803;
  MSG_FORMATMEDIA        = $00804;
  MSG_GETCLOSE           = $00805;
  MSG_GETFIRSTFILE       = $00806;
  MSG_GETINFO            = $00807;
  MSG_GETNEXTFILE        = $00808;
  MSG_RENAME             = $00809;
  MSG_COPY               = $0080A;
  MSG_AUTOMATICCAPTUREDIRECTORY = $0080B;

  // Messages used with a pointer to a DAT_PASSTHRU structure
  MSG_PASSTHRU           = $00901;

  // used with DAT_CALLBACK 
  MSG_REGISTER_CALLBACK  = $00902; //

  // used with DAT_CAPABILITY 
  MSG_RESETALL           = $00A01; //
  
//------------------------------------------------------------------------------
// Capabilities
//------------------------------------------------------------------------------
const
  CAP_CUSTOMBASE             = $08000; // Base of custom capabilities

  // All data sources are REQUIRED to support these caps
  CAP_XFERCOUNT              = $00001;

  // Image data sources are REQUIRED to support these caps
  ICAP_COMPRESSION           = $00100;
  ICAP_PIXELTYPE             = $00101;
  ICAP_UNITS                 = $00102; // default is TWON_INCHES
  ICAP_XFERMECH              = $00103;

  // All data sources MAY support these caps
  CAP_AUTHOR                 = $01000;
  CAP_CAPTION                = $01001;
  CAP_FEEDERENABLED          = $01002;
  CAP_FEEDERLOADED           = $01003;
  CAP_TIMEDATE               = $01004;
  CAP_SUPPORTEDCAPS          = $01005;
  CAP_EXTENDEDCAPS           = $01006;
  CAP_AUTOFEED               = $01007;
  CAP_CLEARPAGE              = $01008;
  CAP_FEEDPAGE               = $01009;
  CAP_REWINDPAGE             = $0100A;
  // Added 1.1
  CAP_INDICATORS             = $0100B;
  // Added 1.6
  CAP_SUPPORTEDCAPSEXT       = $0100C; // Depricated
  CAP_PAPERDETECTABLE        = $0100D;
  CAP_UICONTROLLABLE         = $0100E;
  CAP_DEVICEONLINE	     = $0100F;
  CAP_AUTOSCAN               = $01010;
  // Added 1.7
  CAP_THUMBNAILSENABLED      = $01011;
  CAP_DUPLEX                 = $01012;
  CAP_DUPLEXENABLED          = $01013;
  CAP_ENABLEDSUIONLY         = $01014;
  CAP_CUSTOMDSDATA           = $01015;
  CAP_ENDORSER               = $01016;
  CAP_JOBCONTROL             = $01017;
  // Added 1.8
  CAP_ALARMS                 = $01018;
  CAP_ALARMVOLUME            = $01019;
  CAP_AUTOMATICCAPTURE       = $0101a;
  CAP_TIMEBEFOREFIRSTCAPTURE = $0101b;
  CAP_TIMEBETWEENCAPTURES    = $0101c;
  CAP_CLEARBUFFERS           = $0101d;
  CAP_MAXBATCHBUFFERS        = $0101e;
  CAP_DEVICETIMEDATE         = $0101f;
  CAP_POWERSUPPLY            = $01020;
  CAP_CAMERAPREVIEWUI        = $01021;
  CAP_DEVICEEVENT            = $01022;
  //CAP_PAGEMULTIPLEACQUIRE    = $01023;
  CAP_SERIALNUMBER           = $01024;
  //CAP_FILESYSTEM             = $01025;
  CAP_PRINTER                = $01026;
  CAP_PRINTERENABLED         = $01027;
  CAP_PRINTERINDEX           = $01028;
  CAP_PRINTERMODE            = $01029;
  CAP_PRINTERSTRING          = $0102a;
  CAP_PRINTERSUFFIX          = $0102b;
  CAP_LANGUAGE               = $0102c;
  CAP_FEEDERALIGNMENT        = $0102d;
  CAP_FEEDERORDER            = $0102e;
  CAP_REACQUIREALLOWED       = $01030;
  CAP_PASSTHRU               = $01031;
  CAP_BATTERYMINUTES         = $01032;
  CAP_BATTERYPERCENTAGE      = $01033;
  CAP_CAMERASIDE             = $01034;
  CAP_SEGMENTED              = $01035;
  CAP_CAMERAENABLED          = $01036;
  CAP_CAMERAORDER            = $01037;
  CAP_MICRENABLED            = $01038;
  CAP_FEEDERPREP             = $01039;
  CAP_FEEDERPOCKET           = $0103a;
  CAP_AUTOMATICSENSEMEDIUM   = $0103b;
  CAP_CUSTOMINTERFACEGUID    = $0103c;
  CAP_SUPPORTEDCAPSSEGMENTUNIQUE   = $0103d;
  CAP_SUPPORTEDDATS          = $0103e;
  CAP_DOUBLEFEEDDETECTION    = $0103f;
  CAP_DOUBLEFEEDDETECTIONLENGTH = $01040;
  CAP_DOUBLEFEEDDETECTIONSENSITIVITY = $01041;
  CAP_DOUBLEFEEDDETECTIONRESPONSE    = $01042;
  CAP_PAPERHANDLING          = $01043;
  CAP_INDICATORSMODE         = $01044;
  CAP_PRINTERVERTICALOFFSET  = $01045;
  CAP_POWERSAVETIME          = $01046;
  CAP_PRINTERCHARROTATION	   = $01047;
  CAP_PRINTERFONTSTYLE       = $01048;
  CAP_PRINTERINDEXLEADCHAR   = $01049;
  CAP_PRINTERINDEXMAXVALUE   = $0104A;
  CAP_PRINTERINDEXNUMDIGITS  = $0104B;
  CAP_PRINTERINDEXSTEP       = $0104C;
  CAP_PRINTERINDEXTRIGGER    = $0104D;
  CAP_PRINTERSTRINGPREVIEW   = $0104E;

  // image data sources MAY supposrt these caps
  ICAP_AUTOBRIGHT        = $01100;
  ICAP_BRIGHTNESS        = $01101;
  ICAP_CONTRAST          = $01103;
  ICAP_CUSTHALFTONE      = $01104;
  ICAP_EXPOSURETIME      = $01105;
  ICAP_FILTER            = $01106;
  ICAP_FLASHUSED         = $01107;
  ICAP_GAMMA             = $01108;
  ICAP_HALFTONES         = $01109;
  ICAP_HIGHLIGHT         = $0110A;
  ICAP_IMAGEFILEFORMAT   = $0110C;
  ICAP_LAMPSTATE         = $0110D;
  ICAP_LIGHTSOURCE       = $0110E;
  ICAP_ORIENTATION       = $01110;
  ICAP_PHYSICALWIDTH     = $01111;
  ICAP_PHYSICALHEIGHT    = $01112;
  ICAP_SHADOW            = $01113;
  ICAP_FRAMES            = $01114;
  ICAP_XNATIVERESOLUTION = $01116;
  ICAP_YNATIVERESOLUTION = $01117;
  ICAP_XRESOLUTION       = $01118;
  ICAP_YRESOLUTION       = $01119;
  ICAP_MAXFRAMES         = $0111A;
  ICAP_TILES             = $0111B;
  ICAP_BITORDER          = $0111C;
  ICAP_CCITTKFACTOR      = $0111D;
  ICAP_LIGHTPATH         = $0111E;
  ICAP_PIXELFLAVOR       = $0111F;
  ICAP_PLANARCHUNKY      = $01120;
  ICAP_ROTATION          = $01121;
  ICAP_SUPPORTEDSIZES    = $01122;
  ICAP_THRESHOLD         = $01123;
  ICAP_XSCALING          = $01124;
  ICAP_YSCALING          = $01125;
  ICAP_BITORDERCODES     = $01126;
  ICAP_PIXELFLAVORCODES  = $01127;
  ICAP_JPEGPIXELTYPE     = $01128;
  ICAP_TIMEFILL          = $0112A;
  ICAP_BITDEPTH          = $0112B;
  // Added 1.5
  ICAP_BITDEPTHREDUCTION = $0112C;
  // Added 1.6
  ICAP_UNDEFINEDIMAGESIZE= $0112D;
  // Added 1.7
  ICAP_IMAGEDATASET      = $0112E;
  ICAP_EXTIMAGEINFO      = $0112F;
  ICAP_MINIMUMHEIGHT     = $01130;
  ICAP_MINIMUMWIDTH      = $01131;
  // Added 1.8
  ICAP_FLIPROTATION                 = $01136;
  ICAP_BARCODEDETECTIONENABLED      = $01137;
  ICAP_SUPPORTEDBARCODETYPES        = $01138;
  ICAP_BARCODEMAXSEARCHPRIORITIES   = $01139;
  ICAP_BARCODESEARCHPRIORITIES      = $0113a;
  ICAP_BARCODESEARCHMODE            = $0113b;
  ICAP_BARCODEMAXRETRIES            = $0113c;
  ICAP_BARCODETIMEOUT               = $0113d;
  ICAP_ZOOMFACTOR                   = $0113e;
  ICAP_PATCHCODEDETECTIONENABLED    = $0113f;
  ICAP_SUPPORTEDPATCHCODETYPES      = $01140;
  ICAP_PATCHCODEMAXSEARCHPRIORITIES = $01141;
  ICAP_PATCHCODESEARCHPRIORITIES    = $01142;
  ICAP_PATCHCODESEARCHMODE          = $01143;
  ICAP_PATCHCODEMAXRETRIES          = $01144;
  ICAP_PATCHCODETIMEOUT             = $01145;
  ICAP_FLASHUSED2                   = $01146;
  ICAP_IMAGEFILTER                  = $01147;
  ICAP_NOISEFILTER                  = $01148;
  ICAP_OVERSCAN                     = $01149;
  ICAP_AUTOMATICBORDERDETECTION     = $01150;
  ICAP_AUTOMATICDESKEW              = $01151;
  ICAP_AUTOMATICROTATE              = $01152;
  // Added 1.9
  ICAP_JPEGQUALITY                  = $01153;
  ICAP_FEEDERTYPE                   = $01154;
  ICAP_ICCPROFILE                   = $01155;
  ICAP_AUTOSIZE                     = $01156;
  ICAP_AUTOMATICCROPUSESFRAME       = $01157;
  ICAP_AUTOMATICLENGTHDETECTION     = $01158;
  ICAP_AUTOMATICCOLORENABLED        = $01159;
  ICAP_AUTOMATICCOLORNONCOLORPIXELTYPE = $0115a;
  ICAP_COLORMANAGEMENTENABLED       = $0115b;
  ICAP_IMAGEMERGE                   = $0115c;
  ICAP_IMAGEMERGEHEIGHTTHRESHOLD    = $0115d;
  ICAP_SUPPORTEDEXTIMAGEINFO        = $0115e;
  ICAP_FILMTYPE                     = $0115f;
  ICAP_MIRROR                       = $01160;
  ICAP_JPEGSUBSAMPLING              = $01161;

  // Audio - image data sources MAY support these audio caps
  // Added 1.8
  ACAP_AUDIOFILEFORMAT    = $01201;
  ACAP_XFERMECH           = $01202;


//------------------------------------------------------------------------------
//  Version 1.7: Following is Extended Image Info Attributes.
//------------------------------------------------------------------------------
const
  TWEI_BARCODEX               = $01200;
  TWEI_BARCODEY               = $01201;
  TWEI_BARCODETEXT            = $01202;
  TWEI_BARCODETYPE            = $01203;
  TWEI_DESHADETOP             = $01204;
  TWEI_DESHADELEFT            = $01205;
  TWEI_DESHADEHEIGHT          = $01206;
  TWEI_DESHADEWIDTH           = $01207;
  TWEI_DESHADESIZE            = $01208;
  TWEI_SPECKLESREMOVED        = $01209;
  TWEI_HORZLINEXCOORD         = $0120A;
  TWEI_HORZLINEYCOORD         = $0120B;
  TWEI_HORZLINELENGTH         = $0120C;
  TWEI_HORZLINETHICKNESS      = $0120D;
  TWEI_VERTLINEXCOORD         = $0120E;
  TWEI_VERTLINEYCOORD         = $0120F;
  TWEI_VERTLINELENGTH         = $01210;
  TWEI_VERTLINETHICKNESS      = $01211;
  TWEI_PATCHCODE              = $01212;
  TWEI_ENDORSEDTEXT           = $01213;
  TWEI_FORMCONFIDENCE         = $01214;
  TWEI_FORMTEMPLATEMATCH      = $01215;
  TWEI_FORMTEMPLATEPAGEMATCH  = $01216;
  TWEI_FORMHORZDOCOFFSET      = $01217;
  TWEI_FORMVERTDOCOFFSET      = $01218;
  TWEI_BARCODECOUNT           = $01219;
  TWEI_BARCODECONFIDENCE      = $0121A;
  TWEI_BARCODEROTATION        = $0121B;
  TWEI_BARCODETEXTLENGTH      = $0121C;
  TWEI_DESHADECOUNT           = $0121D;
  TWEI_DESHADEBLACKCOUNTOLD   = $0121E;
  TWEI_DESHADEBLACKCOUNTNEW   = $0121F;
  TWEI_DESHADEBLACKRLMIN      = $01220;
  TWEI_DESHADEBLACKRLMAX      = $01221;
  TWEI_DESHADEWHITECOUNTOLD   = $01222;
  TWEI_DESHADEWHITECOUNTNEW   = $01223;
  TWEI_DESHADEWHITERLMIN      = $01224;
  TWEI_DESHADEWHITERLAVE      = $01225;
  TWEI_DESHADEWHITERLMAX      = $01226;
  TWEI_BLACKSPECKLESREMOVED   = $01227;
  TWEI_WHITESPECKLESREMOVED   = $01228;
  TWEI_HORZLINECOUNT          = $01229;
  TWEI_VERTLINECOUNT          = $0122A;
  TWEI_DESKEWSTATUS           = $0122B;
  TWEI_SKEWORIGINALANGLE      = $0122C;
  TWEI_SKEWFINALANGLE         = $0122D;
  TWEI_SKEWCONFIDENCE         = $0122E;
  TWEI_SKEWWINDOWX1           = $0122F;
  TWEI_SKEWWINDOWY1           = $01230;
  TWEI_SKEWWINDOWX2           = $01231;
  TWEI_SKEWWINDOWY2           = $01232;
  TWEI_SKEWWINDOWX3           = $01233;
  TWEI_SKEWWINDOWY3           = $01234;
  TWEI_SKEWWINDOWX4           = $01235;
  TWEI_SKEWWINDOWY4           = $01236;
  TWEI_BOOKNAME               = $01238; // added 1.9
  TWEI_CHAPTERNUMBER          = $01239; // added 1.9
  TWEI_DOCUMENTNUMBER         = $0123A; // added 1.9
  TWEI_PAGENUMBER             = $0123B; // added 1.9
  TWEI_CAMERA                 = $0123C; // added 1.9
  TWEI_FRAMENUMBER            = $0123D; // added 1.9
  TWEI_FRAME	              = $0123E; // added 1.9
  TWEI_PIXELFLAVOR            = $0123F; // added 1.9
  TWEI_ICCPROFILE             = $01240;
  TWEI_LASTSEGMENT            = $01241;
  TWEI_SEGMENTNUMBER          = $01242;
  TWEI_MAGDATA                = $01243;
  TWEI_MAGTYPE                = $01244;
  TWEI_PAGESIDE               = $01245;
  TWEI_FILESYSTEMSOURCE       = $01246;
  TWEI_IMAGEMERGED            = $01247;
  TWEI_MAGDATALENGTH          = $01248;
  TWEI_PAPERCOUNT             = $01249;
  TWEI_PRINTERTEXT            = $0124A; 

  TWEJ_NONE                   = $00000;
  TWEJ_MIDSEPARATOR           = $00001;
  TWEJ_PATCH1                 = $00002;
  TWEJ_PATCH2                 = $00003;
  TWEJ_PATCH3                 = $00004;
  TWEJ_PATCH4                 = $00005;
  TWEJ_PATCH6                 = $00006;
  TWEJ_PATCHT                 = $00007;

  // Added 1.8
  // TW_PASSTHRU.Direction values
  TWDR_GET                    = 1;
  TWDR_SET                    = 2;

//------------------------------------------------------------------------------
// Return Codes and Condition Codes section
//------------------------------------------------------------------------------

const
  // Return Codes: DSM_Entry and DS_Entry may return any one of these values.
  TWRC_CUSTOMBASE        = $08000;

  TWRC_SUCCESS           = 0;  //
  TWRC_FAILURE           = 1;  // App may get TW_STATUS for info on failure
  TWRC_CHECKSTATUS       = 2;  // "tried hard"; get status
  TWRC_CANCEL            = 3;  // Transfer was canceled. 
  TWRC_DSEVENT           = 4;  // A Data Source event.
  TWRC_NOTDSEVENT        = 5;  // Not a Data Source event.
  TWRC_XFERDONE          = 6;  // Data transfer completed.
  TWRC_ENDOFLIST         = 7;  // After MSG_GETNEXT if nothing left
  TWRC_INFONOTSUPPORTED  = 8;  //
  TWRC_DATANOTAVAILABLE  = 9;  //
  TWRC_BUSY              = 10;
  TWRC_SCANNERLOCKED     = 11;

  TWRC_EXCEPTION         = TWRC_CUSTOMBASE +
                           900; // (MCM) The source raised an exception.
                                // Not part of TWAIN's definition.
  TWRC_TIMEOUT           = TWRC_CUSTOMBASE +
                           901; // (MCM) The source didn't responde in a timely manor.
                                // Not part of TWAIN's definition.

  // Condition Codes: App gets these by doing DG_CONTROL DAT_STATUS MSG_GET.
  TWCC_CUSTOMBASE        = $08000;

  TWCC_SUCCESS           =  0; // Well - it worked!
  TWCC_BUMMER            =  1; // Failure due to unknown causes
  TWCC_LOWMEMORY         =  2; // Not enough memory to perform operation
  TWCC_NODS              =  3; // No Data Source
  TWCC_MAXCONNECTIONS    =  4; // Data Source is connected to max possible applications
  TWCC_OPERATIONERROR    =  5; // Operational error. DS or DSM reported error, app shouldn't
  TWCC_BADCAP            =  6; // Unknown capability
  TWCC_BADPROTOCOL       =  9; // Unrecognized DG DAT MSG combination.
  TWCC_BADVALUE          = 10; // Data parameter out of range.
  TWCC_SEQERROR          = 11; // DG DAT MSG out of expected sequence.
  TWCC_BADDEST           = 12; // Unknown destination App/Src in DSM_Entry
  TWCC_CAPUNSUPPORTED    = 13; // Capability not supported by source.
  TWCC_CAPBADOPERATION   = 14; // Operation not supported by capability
  TWCC_CAPSEQERROR       = 15; // Capability has dependancy on other capability
  // Added 1.8
  TWCC_DENIED            = 16; // File System operation is denied
                               // (file is protected)
  TWCC_FILEEXISTS        = 17; // Operation failed because file already exists.
  TWCC_FILENOTFOUND      = 18; // File not found
  TWCC_NOTEMPTY          = 19; // Operation failed because directory is not
                               // empty
  TWCC_PAPERJAM          = 20; // The feeder is jammed
  TWCC_PAPERDOUBLEFEED   = 21; // The feeder detected multiple pages
  TWCC_FILEWRITEERROR    = 22; // Error writing the file (meant for things like
                               // disk full conditions)
  TWCC_CHECKDEVICEONLINE = 23; // The device went off-line prior to or during
                               // this operation

  TWCC_INTERLOCK         = 24;
  TWCC_DAMAGEDCORNER     = 25;
  TWCC_FOCUSERROR        = 26;
  TWCC_DOCTOOLIGHT       = 27;
  TWCC_DOCTOODARK        = 28;
  TWCC_NOMEDIA           = 29;

//------------------------------------------------------------------------------
// bit patterns: for query the operation that are supported by the data source
// on a capability
// App gets these through DG_CONTROL/DAT_CAPABILITY/MSG_QUERYSUPPORT
// Added 1.6
//------------------------------------------------------------------------------

const
  TWQC_GET               = $00001;
  TWQC_SET               = $00002;
  TWQC_GETDEFAULT        = $00004;
  TWQC_GETCURRENT        = $00008;
  TWQC_RESET             = $00010;
  TWQC_ALL               = $0001F; // MCM - Not part of TWAIN's definition.
  TWQC_SETCONSTRAINT     = $00020;
  TWQC_CONSTRAINABLE     = $00040;
  TWQC_GETHELP           = $00100;
  TWQC_GETLABEL          = $00200;
  TWQC_GETLABELENUM      = $00400;


function GlobalEntryPoint : pTW_ENTRYPOINT;
procedure ClearGlobalEntryPoint;
function  DSMMemAlloc(Size : TW_UINT32) : TW_HANDLE; 
procedure DSMMemFree(Handle : TW_HANDLE);
function  DSMMemLock(Handle : TW_HANDLE) : TW_MEMREF;
procedure DSMMemUnlock(Handle : TW_HANDLE);

  
implementation

{$IFDEF GE_DXE2}
uses System.SysUtils;
{$ELSE}
uses SysUtils;
{$ENDIF}

var G_EntryPoint : TW_ENTRYPOINT; 

procedure ClearGlobalEntryPoint;
begin
  G_EntryPoint.Size := SizeOf(TW_ENTRYPOINT);
  G_EntryPoint.DSM_MemAllocate := Nil;
  G_EntryPoint.DSM_MemFree     := Nil;
  G_EntryPoint.DSM_MemLock     := Nil;
  G_EntryPoint.DSM_MemUnlock   := Nil;
end; // ClearGlobalEntryPoint.


function GlobalEntryPoint : pTW_ENTRYPOINT;
begin
  Result := @G_EntryPoint;
end; // GlobalEntryPoint.


function DSMMemAlloc(Size : TW_UINT32) : TW_HANDLE; 
begin
  if (G_EntryPoint.DSM_MemAllocate <> Nil)
  then Result := TDSM_MemAllocate(G_EntryPoint.DSM_MemAllocate)(Size)
  else Result := GlobalAlloc(GHND, Size);
end; // DSMMemAlloc.


procedure DSMMemFree(Handle : TW_HANDLE);
begin
  if (G_EntryPoint.DSM_MemFree <> Nil)
  then TDSM_MemFree(G_EntryPoint.DSM_MemFree)(Handle)
  else GlobalFree(Handle);
end; // DSMMemFree.


function DSMMemLock(Handle : TW_HANDLE) : TW_MEMREF;
begin
  if (G_EntryPoint.DSM_MemLock <> Nil)
  then Result := TDSM_MemLock(G_EntryPoint.DSM_MemLock)(Handle)
  else Result := GlobalLock(Handle);
end; // DSMMemLock.


procedure DSMMemUnlock(Handle : TW_HANDLE);
begin
  if (G_EntryPoint.DSM_MemUnlock <> Nil)
  then TDSM_MemUnlock(G_EntryPoint.DSM_MemUnlock)(Handle)
  else GlobalUnlock(Handle);
end; // DSMMemUnlock.



initialization
  ClearGlobalEntryPoint;
  
{$IFDEF MCMDEMO}
  CR_LF := StrPas(pCR_LF);
 {$IFOPT J-}{$DEFINE TYPED_CONSTANTS_ON}{$J+}{$ENDIF}
  pCR_LF[0] := chr($FF - byte(pCR_LF[0]));
  {$IFDEF TYPED_CONSTANTS_ON}{$UNDEF TYPED_CONSTANTS_ON}{$J-}{$ENDIF}
{$ELSE}
  CR_LF := StrPas(pCR_LF);
{$ENDIF}
end.

