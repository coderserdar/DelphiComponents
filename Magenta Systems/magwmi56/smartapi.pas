unit smartapi ;

{ ****************************************************************************
*                                                                           *
* THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY     *
* KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE       *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR     *
* PURPOSE.                                                                  *
*                                                                           *
* Copyright 1993-98  Microsoft Corporation.  All Rights Reserved.           *
*                                                                           *
****************************************************************************/

/ ****************************************************************************
*
* PROGRAM: SMART.H
*
* PURPOSE: Structure definitions for an application that calls SMART Ioctls

Converted from C to pascal by Angus Robertson, Magenta Systems Ltd
18th July 2004
29 Jul 2008 - Byte instead of Char where necessary for compability with unicode in Delphi 2009
23 Jan 2013 - bIDEDeviceMap only reports maximum four IDE drives and not reliabily, ignore it
              renamed various constants and structures to names currently used on MSDN
              expanded TIdentifyDeviceData to add numerous missing fields including LBA48 drive size
              added IOCTL_STORAGE_QUERY_PROPERTY to get basic drive info
              added IOCTL_DISK_GET_DRIVE_GEOMETRY_EX literals to get drive size
              added SCSI DeviceTypes, IOCTL_SCSI_MINIPORT structures and literals


 

SMART is documnented by Microsoft in MSDN under Windows Drivers:

Dev Center - Hardware > Docs > Drivers > Windows Driver Development > Device and Driver Technologies >
Storage Devices > Reference > I/O Requests for Mass Storage Drivers > Disk I/O Control Codes

http://msdn.microsoft.com/en-us/library/windows/hardware/ff561595%28v=vs.85%29.aspx

This code was originally converted from an MSDN command line sample SmartApp.exe



****************************************************************************  }

interface

uses
  Windows, Messages, SysUtils ;

// Miscellaneous


type
    USHORT = Word ;   // Angus   16 unsigned bits
    UCHAR = Byte ;

const
    READ_ATTRIBUTE_BUFFER_SIZE = 512 ;
    IDENTIFY_BUFFER_SIZE = 512 ;
    READ_THRESHOLD_BUFFER_SIZE = 512 ;
//
// IOCTL commands, DFP (disk fault protection) is old name, SMART current name
//
    DFP_GET_VERSION        = $00074080 ;
    DFP_SEND_DRIVE_COMMAND = $0007c084 ;
    DFP_RECEIVE_DRIVE_DATA = $0007c088 ;
 // 15 Jan 2013, same commands, newer names
    SMART_GET_VERSION                   = $00074080 ;
    SMART_SEND_DRIVE_COMMAND            = $0007c084 ;
    SMART_RECEIVE_DRIVE_DATA            = $0007c088 ;
    IOCTL_STORAGE_QUERY_PROPERTY        = $002D1400;
    IOCTL_DISK_GET_DRIVE_GEOMETRY_EX    = $000700A0;

// SCSI IO Device Control Codes
const
//    IOCTL_SCSI_PASS_THROUGH                         = $0004d004;
//    IOCTL_SCSI_PASS_THROUGH_DIRECT                  = $0004d014;
//    IOCTL_SCSI_MINIPORT                             = $0004D008;
    IOCTL_SCSI_EXECUTE_IN                           = $001b0011;
    IOCTL_SCSI_EXECUTE_OUT                          = $001b0012;
    IOCTL_SCSI_EXECUTE_NONE                         = $001b0013;

    IOCTL_STORAGE_BASE = $0000002d;  // FILE_DEVICE_MASS_STORAGE
    IOCTL_SCSI_BASE    = $00000004;  // FILE_DEVICE_CONTROLLER;

// Define the method codes for how buffers are passed for I/O and FS controls
    METHOD_BUFFERED   = 0;
    METHOD_IN_DIRECT  = 1;
    METHOD_OUT_DIRECT = 2;
    METHOD_NEITHER    = 3;

// Define the access check value for any access
  FILE_ANY_ACCESS     = 0;
  FILE_SPECIAL_ACCESS = FILE_ANY_ACCESS;
  FILE_READ_ACCESS    = $0001; // file & pipe
  FILE_WRITE_ACCESS   = $0002; // file & pipe

    IOCTL_STORAGE_GET_MEDIA_TYPES = (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or ($0300 shl 2) or (METHOD_BUFFERED);
    IOCTL_STORAGE_GET_MEDIA_TYPES_EX = (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or ($0301 shl 2) or (METHOD_BUFFERED);
    IOCTL_STORAGE_GET_DEVICE_NUMBER = (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or ($0420 shl 2) or (METHOD_BUFFERED);

    IOCTL_SCSI_PASS_THROUGH = (IOCTL_SCSI_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or ($0401 shl 2) or (METHOD_BUFFERED);
    IOCTL_SCSI_MINIPORT = (IOCTL_SCSI_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or ($0402 shl 2) or (METHOD_BUFFERED);
    IOCTL_SCSI_GET_INQUIRY_DATA = (IOCTL_SCSI_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or ($0403 shl 2) or (METHOD_BUFFERED);
    IOCTL_SCSI_GET_CAPABILITIES = (IOCTL_SCSI_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or ($0404 shl 2) or (METHOD_BUFFERED);
    IOCTL_SCSI_PASS_THROUGH_DIRECT = (IOCTL_SCSI_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or ($0405 shl 2) or (METHOD_BUFFERED);
    IOCTL_SCSI_GET_ADDRESS = (IOCTL_SCSI_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or ($0406 shl 2) or (METHOD_BUFFERED);
    IOCTL_SCSI_RESCAN_BUS = (IOCTL_SCSI_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or ($0407 shl 2) or (METHOD_BUFFERED);
    IOCTL_SCSI_GET_DUMP_POINTERS = (IOCTL_SCSI_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or ($0408 shl 2) or (METHOD_BUFFERED);
    IOCTL_SCSI_GET_FREE_DUMP_POINTERS = (IOCTL_SCSI_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or ($0409 shl 2) or (METHOD_BUFFERED);
    IOCTL_IDE_PASS_THROUGH = (IOCTL_SCSI_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or ($040a shl 2) or (METHOD_BUFFERED);

// SMART support in atapi
    IOCTL_SCSI_MINIPORT_SMART_VERSION               = $001b0500;
    IOCTL_SCSI_MINIPORT_IDENTIFY                    = $001b0501;
    IOCTL_SCSI_MINIPORT_READ_SMART_ATTRIBS          = $001b0502;
    IOCTL_SCSI_MINIPORT_READ_SMART_THRESHOLDS       = $001b0503;
    IOCTL_SCSI_MINIPORT_ENABLE_SMART                = $001b0504;
    IOCTL_SCSI_MINIPORT_DISABLE_SMART               = $001b0505;
    IOCTL_SCSI_MINIPORT_RETURN_STATUS               = $001b0506;
    IOCTL_SCSI_MINIPORT_ENABLE_DISABLE_AUTOSAVE     = $001b0507;
    IOCTL_SCSI_MINIPORT_SAVE_ATTRIBUTE_VALUES       = $001b0508;
    IOCTL_SCSI_MINIPORT_EXECUTE_OFFLINE_DIAGS       = $001b0509;
    IOCTL_SCSI_MINIPORT_ENABLE_DISABLE_AUTO_OFFLINE = $001b050a;

// SCSI inquiry command literals
    CDB6GENERIC_LENGTH  = 6;
    SCSI_IOCTL_DATA_IN  = 1;
    SCSIOP_INQUIRY      = $12;

// SCSI DeviceType from Inquiry, also
    DIRECT_ACCESS_DEVICE            = $00;   // disks
    SEQUENTIAL_ACCESS_DEVICE        = $01;   // tapes
    PRINTER_DEVICE                  = $02;   // printers
    PROCESSOR_DEVICE                = $03;   // scanners, printers, etc
    WRITE_ONCE_READ_MULTIPLE_DEVICE = $04;   // worms
    READ_ONLY_DIRECT_ACCESS_DEVICE  = $05;   // cdroms
    SCANNER_DEVICE                  = $06;   // scanners
    OPTICAL_DEVICE                  = $07;   // optical disks
    MEDIUM_CHANGER                  = $08;   // jukebox
    COMMUNICATION_DEVICE            = $09;   // network
// 0xA and 0xB are obsolete
    ARRAY_CONTROLLER_DEVICE         = $0C;   // RAID
    SCSI_ENCLOSURE_DEVICE           = $0D;
    REDUCED_BLOCK_DEVICE            = $0E;   // e.g., 1394 disk
    OPTICAL_CARD_READER_WRITER_DEVICE = $0F;
    BRIDGE_CONTROLLER_DEVICE        = $10;
    OBJECT_BASED_STORAGE_DEVICE     = $11;   // OSD
    LOGICAL_UNIT_NOT_PRESENT_DEVICE = $7F;
    DEVICE_TYPE_MAX                 = $11;

    DEVICE_QUALIFIER_ACTIVE         = $00;
    DEVICE_QUALIFIER_NOT_ACTIVE     = $01;
    DEVICE_QUALIFIER_NOT_SUPPORTED  = $03;

var
    DeviceTypeStr: array [0..DEVICE_TYPE_MAX] of string = (
        'Disk','Tape','Printer','Processor','Worm','CD/DVD','Scanner',
        'Optical','Jukebox','Network','','','RAID','Enclosure','RBC',
        'Card','Bridge','Other') ;

//---------------------------------------------------------------------
// GETVERSIONOUTPARAMS contains the data returned from the
// Get Driver Version function.
//---------------------------------------------------------------------
type
    PGetVersionOutParams = ^TGetVersionOutParams ;
    TGetVersionOutParams = Packed Record
        bVersion: byte ;        // Binary driver version.
        bRevision: byte ;       // Binary driver revision.
        bReserved: byte ;       // Not used.
        bIDEDeviceMap: byte ;   // Bit map of IDE devices, max four drives
        fCapabilities: DWORD ;  // Bit mask of driver capabilities.
        dwReserved: array [1..4] of DWORD ; // For future use.
     end ;

(* borrected from SmartTools, not not used yet, note supports more than eight devices

// 3ware specific versions of SMART ioctl structs

#define SMART_VENDOR_3WARE      0x13C1  // identifies 3ware specific parameters

#pragma pack(1)

typedef struct _GETVERSIONINPARAMS_EX {
  BYTE bVersion;
  BYTE bRevision;
  BYTE bReserved;
  BYTE bIDEDeviceMap;
  DWORD fCapabilities;
  DWORD dwDeviceMapEx;  // 3ware specific: RAID drive bit map
  WORD wIdentifier;     // Vendor specific identifier
  WORD wControllerId;   // 3ware specific: Controller ID (0,1,...)
  ULONG dwReserved[2];
} GETVERSIONINPARAMS_EX;

typedef struct _SENDCMDINPARAMS_EX {
  DWORD cBufferSize;
  IDEREGS irDriveRegs;
  BYTE bDriveNumber;
  BYTE bPortNumber;     // 3ware specific: port number
  WORD wIdentifier;     // Vendor specific identifier
  DWORD dwReserved[4];
  BYTE bBuffer[1];
} SENDCMDINPARAMS_EX;
*)

//
// Bits returned in the fCapabilities member of GETVERSIONOUTPARAMS
//
const
    CAP_IDE_ID_FUNCTION = 1 ;            // ATA ID command supported
    CAP_IDE_ATAPI_ID = 2 ;               // ATAPI ID command supported
    CAP_IDE_EXECUTE_SMART_FUNCTION = 4 ; // SMART commannds supported

//---------------------------------------------------------------------
// IDE registers
//---------------------------------------------------------------------

type
    PIDERegs = ^TIDERegs ;
    TIDERegs = Packed Record
        bFeaturesReg: byte ;        // Used for specifying SMART "commands".
        bSectorCountReg: byte ;     // IDE sector count register
        bSectorNumberReg: byte ;    // IDE sector number register
        bCylLowReg: byte ;          // IDE low order cylinder value
        bCylHighReg: byte ;         // IDE high order cylinder value
        bDriveHeadReg: byte ;       // IDE drive/head register
        bCommandReg: byte ;         // Actual IDE command.
        bReserved: byte ;           // reserved for future use.  Must be zero.
    end ;

//---------------------------------------------------------------------
// SENDCMDINPARAMS contains the input parameters for the
// Send Command to Drive function.
//---------------------------------------------------------------------

    PSendCmdInParams = ^TSendCmdInParams ;
    TSendCmdInParams = Packed Record
        cBufferSize: DWORD ;        // Buffer size in bytes
        irDriveRegs: TIDERegs ;     // Structure with drive register values.
        bDriveNumber: byte;         // Physical drive number to send command to (0,1,2,3).
        bReserved: array [1..3] of byte ;   // Reserved for future expansion.
        dwReserved: array [1..4] of DWORD ; // For future use.
        bBuffer: array [1..1] of byte ;     // Input buffer.
    end ;

//
// Valid values for the bCommandReg member of IDEREGS.
//
const
    IDE_ATAPI_ID                = $A1 ; // Returns ID sector for ATAPI.
    IDE_ID_FUNCTION             = $EC ; // Returns ID sector for ATA.
    IDE_EXECUTE_SMART_FUNCTION  = $B0 ; // Performs SMART cmd.
                                            // Requires valid bFeaturesReg,
                                            // bCylLowReg, and bCylHighReg
 // 15 Jan 2013, same commands, newer names
    ATA_IDENTIFY_PACKET_DEVICE  = $A1 ; // Returns ID sector for ATAPI.
    ATA_IDENTIFY_DEVICE         = $EC ; // Returns ID sector for ATA.
    ATA_SMART_CMD               = $B0 ; // Performs SMART cmd.
//
// Cylinder register values required when issuing SMART command
//
    SMART_CYL_LOW   = $4F ;
    SMART_CYL_HI    = $C2 ;

//---------------------------------------------------------------------
// Status returned from driver
//---------------------------------------------------------------------

type
    PDriverStatus = ^TDriverStatus ;
    TDriverStatus = Packed Record
        bDriverError: byte ;    // Error code from driver, or 0 if no error.
        bIDEStatus: byte ;      // Contents of IDE Error register.
                                // Only valid when bDriverError is SMART_IDE_ERROR.
        bReserved: array [1..2] of byte;        // Reserved for future expansion.
        dwReserved: array [1..2] of DWORD;      // Reserved for future expansion.
    end ;

//
// bDriverError values
//
const
    SMART_NO_ERROR         = 0 ;    // No error
    SMART_IDE_ERROR        = 1 ;    // Error from IDE controller
    SMART_INVALID_FLAG     = 2 ;    // Invalid command flag
    SMART_INVALID_COMMAND  = 3 ;    // Invalid command byte
    SMART_INVALID_BUFFER   = 4 ;    // Bad buffer (null, invalid addr..)
    SMART_INVALID_DRIVE    = 5 ;    // Drive number not valid
    SMART_INVALID_IOCTL    = 6 ;    // Invalid IOCTL
    SMART_ERROR_NO_MEM     = 7 ;    // Could not lock user's buffer
    SMART_INVALID_REGISTER = 8 ;    // Some IDE Register not valid
    SMART_NOT_SUPPORTED    = 9 ;    // Invalid cmd flag set
    SMART_NO_IDE_DEVICE    = 10;    // Cmd issued to device not present
                                    // although drive number is valid
// 11-255 reserved

var
    DriverErrorStr: array [0..10] of string = (
        'No error', 'Error from IDE controller', 'Invalid command flag',
        'Invalid command byte', 'Bad buffer (null, invalid addr..)',
        'Drive number not valid', 'Invalid IOCTL', 'Could not lock user"s buffer',
        'Some IDE Register not valid', 'Invalid cmd flag set',
        'Cmd issued to device not present although drive number is valid') ;

//---------------------------------------------------------------------
// Structure returned by SMART IOCTL for several commands
//---------------------------------------------------------------------

type
    PSendCmdOutParams = ^TSendCmdOutParams ;
    TSendCmdOutParams = Packed Record
        cBufferSize: DWORD ;            // Size of bBuffer in bytes
        DriverStatus: TDriverStatus ;   // Driver status structure.
        bBuffer: array [1..1] of byte ;     // Buffer of arbitrary length in which to store the data read from the drive.
    end ;

//---------------------------------------------------------------------
// Feature register defines for SMART "sub commands"
//---------------------------------------------------------------------

const
    SMART_READ_ATTRIBUTE_VALUES             = $D0 ; // ATA4: Renamed SMART READ DATA
    SMART_READ_ATTRIBUTE_THRESHOLDS         = $D1 ; // Obsoleted in ATA4!
    SMART_ENABLE_DISABLE_ATTRIBUTE_AUTOSAVE = $D2 ;
    SMART_SAVE_ATTRIBUTE_VALUES             = $D3 ;
    SMART_EXECUTE_OFFLINE_IMMEDIATE         = $D4 ; // ATA4
    SMART_READ_LOG_SECTOR                   = $D5 ; // ATA5
    SMART_WRITE_LOG_SECTOR                  = $D6 ; // ATA5
    SMART_WRITE_THRESHOLDS                  = $D7 ; // Obsoleted ??
// Vendor specific commands:
    SMART_ENABLE_SMART_OPERATIONS           = $D8 ;
    SMART_DISABLE_SMART_OPERATIONS          = $D9 ;
    SMART_RETURN_SMART_STATUS               = $DA ;

//---------------------------------------------------------------------
// The following structure defines the structure of a Drive Attribute
//---------------------------------------------------------------------

type
    PDriveAttribute = ^TDriveAttribute ;
    TDriveAttribute = Packed Record
        bAttrID: byte ;     // Identifies which attribute
        wStatusFlags: word ;    // see bit definitions below
        bAttrValue: byte ;      // Current normalized value
        bWorstValue: byte;      // How bad has it ever been?
        bRawValue: array [1..6] of byte ;   // Un-normalized value
        bReserved: byte ;       // ...
    end ;

//---------------------------------------------------------------------
// The following structure defines the structure of a Warranty Threshold
// Obsoleted in ATA4!
//---------------------------------------------------------------------
    PAttrThreshold = ^TAttrThreshold ;
    TAttrThreshold = Packed Record
        bAttrID: byte ;         // Identifies which attribute
        bWarrantyThreshold: byte ;  // Triggering value
        bReserved: array [1..10] of byte;   // ...
    end ;

//---------------------------------------------------------------------
// The following defines the IDENTIFY_DEVICE_DATA structure (Windows Drivers)
// buffer, note this has been extended over the past 20 years for larger drives
//  warning - this translation is not all tested for conversion errors!!!
//---------------------------------------------------------------------
type
    PIdentifyDeviceData = ^TIdentifyDeviceData ;
    TIdentifyDeviceData = Packed Record
        wGenConfig: USHORT ;   // bitmap
        wNumCyls: USHORT ;
        wReserved: USHORT ;
        wNumHeads: USHORT ;
        wRetired1: ULONG ;
        wSectorsPerTrack: USHORT ;
        wVendorUnique: array [1..3] of USHORT ;
        sSerialNumber: array [1..20] of UCHAR ;
        wRetired2: ULONG ;
        wObsolete: USHORT ;
        sFirmwareRev: array [1..8] of UCHAR ;
        sModelNumber: array [1..40] of UCHAR ;
        sMaximumBlockTransfer: UCHAR ;
        sVendorUnique2: UCHAR ;
        wReservedWord48: USHORT ;
        wCapabilities: USHORT ;   // bitmap
        wReservedWord50: USHORT ;
        uObsoleteWords51: ULONG ;
        wTranslationFieldsValid: USHORT ;
        wNumCurrentCyls: USHORT ;
        wNumCurrentHeads: USHORT ;
        wNumCurrentSectorsPerTrack: USHORT ;
        ulCurrentSectorCapacity: ULONG ;
        sCurrentMultiSectorSetting: UCHAR ;
        sMultiSectorSettingValid: UCHAR ;
        ulTotalAddressableSectors: ULONG ;
        ObsoleteWord62: USHORT ;
        sMultiWordDMASupport: UCHAR ;
        sMultiWordDMAActive: UCHAR ;
        sAdvancedPIOModes: UCHAR ;
        sReservedByte64: UCHAR ;
        wMinimumMWXferCycleTime: USHORT ;
        wRecommendedMWXferCycleTime: USHORT ;
        wMinimumPIOCycleTime: USHORT ;
        wMinimumPIOCycleTimeIORDY: USHORT ;
        wReservedWords69: array [1..6] of USHORT ;
        wQueueDepth: USHORT ;
        wSataMaxSpeed: USHORT ;             // undocumented on MSDN
        wSataCurSpeed: USHORT ;             // undocumented on MSDN
        wReservedWords78: array [1..2] of USHORT ;
        wMajorRevision: USHORT ;
        wMinorRevision: USHORT ;
        wCommandSetSupport1: USHORT ;
        wCommandSetSupport2: USHORT ;
        wCommandSetSupport3: USHORT ;
        wCommandSetActive1: USHORT ;
        wCommandSetActive2: USHORT ;
        wCommandSetActive3: USHORT ;
        sUltraDMASupport: UCHAR ;
        sUltraDMAActive: UCHAR ;
        wReservedWord89: array [1..4] of USHORT ;
        wHardwareResetResult: USHORT ;
        sCurrentAcousticValue: UCHAR ;
        sRecommendedAcousticValue: UCHAR ;
        wReservedWord95: array [1..5] of USHORT ;
        ullMax48BitLBA: ULONGLONG;
        wStreamingTransferTime: USHORT ;
        wReservedWord105: USHORT ;
        wPhysicalLogicalSectorSize: USHORT ;   // warning, mix of integer and flags
        wInterSeekDelay: USHORT ;
        wWorldWideName: array [1..4] of USHORT ;
        wReservedForWorldWideName128: array [1..4] of USHORT ;
        wReservedForTlcTechnicalReport: USHORT ;
        ulWordsPerLogicalSector: ULONG ;
        wCommandSetSupportExt: USHORT ;
        wCommandSetActiveExt: USHORT ;
        wReservedForExpandedSupportandActive: array [1..6] of USHORT ;
        wMsnSupport: USHORT ;
        wSecurityStatus: USHORT ;
        wReservedWord129: array [1..31] of USHORT ;
        wCfaPowerModel: USHORT ;
        wReservedForCfaWord161: array [1..8] of USHORT ;
        wDataSetManagementFeature: USHORT ;
        wReservedForCfaWord170: array [1..6] of USHORT ;
        wCurrentMediaSerialNumber: array [1..30] of USHORT ;
        wReservedWord206: USHORT ;
        wReservedWord207: array [1..2] of USHORT ;
        wBlockAlignment: USHORT ;
        ulWriteReadVerifySectorCountMode3Only: ULONG ;
        ulWriteReadVerifySectorCountMode2Only: ULONG ;
        wNVCacheCapabilities: USHORT ;
        wNVCacheSizeLSW: USHORT ;
        wNVCacheSizeMSW: USHORT ;
        wNominalMediaRotationRate: USHORT ;
        wReservedWord218: USHORT ;
        wVCacheOptions: USHORT ;
//        wReservedWord220: array [1..30] of USHORT ;
        wReservedWord220: USHORT ;
        wReservedWord221: USHORT ;
        wSataVersion: USHORT ;         // undocumented on MSDN
        wReservedWord223: array [1..27] of USHORT ;
        sSignature: UCHAR ;
        sCheckSum: UCHAR ;
    end ;

const
    GeneralConfigReserved1           = $00000001;
    GeneralConfigRetired3            = $00000002;
    GeneralConfigResponseIncomplete  = $00000004;
    GeneralConfigRetired2            = $00000008;
    GeneralConfigFixedDevice         = $00000010;
    GeneralConfigRemovableMedia      = $00000020;
    GeneralConfigRetired1            = $00000040;
    GeneralConfigDeviceType          = $00008000;

    CapabilitiesReservedByte49       = $00000010;
    CapabilitiesDmaSupported         = $00000020;
    CapabilitiesLbaSupported         = $00000040;
    CapabilitiesIordyDisable         = $00000080;
    CapabilitiesIordySupported       = $00000100;

    CmdSet1SmartCommands             = $00000001;
    CmdSet1SecurityMode              = $00000002;
    CmdSet1RemovableMediaFeature     = $00000004;
    CmdSet1PowerManagement           = $00000008;
    CmdSet1Reserved1                 = $00000010;
    CmdSet1WriteCache                = $00000020;
    CmdSet1LookAhead                 = $00000040;
    CmdSet1ReleaseInterrupt          = $00000080;
    CmdSet1ServiceInterrupt          = $00000100;
    CmdSet1DeviceReset               = $00000200;
    CmdSet1HostProtectedArea         = $00000400;
    CmdSet1Obsolete1                 = $00000800;
    CmdSet1WriteBuffer               = $00001000;
    CmdSet1ReadBuffer                = $00002000;
    CmdSet1Nop                       = $00004000;

    CmdSet2DownloadMicrocode        = $00000001;
    CmdSet2DmaQueued                = $00000002;
    CmdSet2Cfa                      = $00000004;
    CmdSet2AdvancedPm               = $00000008;
    CmdSet2Msn                      = $00000010;
    CmdSet2PowerUpInStandby         = $00000020;
    CmdSet2ManualPowerUp            = $00000040;
    CmdSet2Reserved2                = $00000080;
    CmdSet2SetMax                   = $00000100;
    CmdSet2Acoustics                = $00000200;
    CmdSet2BigLba                   = $00000400;   // LBA48 supported
    CmdSet2DeviceConfigOverlay      = $00000800;
    CmdSet2FlushCache               = $00001000;
    CmdSet2FlushCacheExt            = $00002000;

    CmdSet3SmartErrorLog            = $00000001;
    CmdSet3SmartSelfTest            = $00000002;
    CmdSet3MediaSerialNumber        = $00000004;
    CmdSet3MediaCardPassThrough     = $00000008;
    CmdSet3StreamingFeature         = $00000010;
    CmdSet3GpLogging                = $00000020;
    CmdSet3WriteFua                 = $00000040;
    CmdSet3WriteQueuedFua           = $00000080;
    CmdSet3WWN64Bit                 = $00000100;
    CmdSet3URGReadStream            = $00000200;
    CmdSet3URGWriteStream           = $00000400;
    CmdSet3ReservedForTechReport    = $00000800;
    CmdSet3IdleWithUnloadFeature    = $00002000;

    PhysLogSectSizeLogicalSectorsPerPhysicalSector          = $0000000F;
    PhysLogSectSizeReserved0                                = $00000010;
    PhysLogSectSizeLogicalSectorLongerThan256Words          = $00001000;
    PhysLogSectSizeMultipleLogicalSectorsPerPhysicalSector  = $00002000;

    BlockAlignmentOfLogicalWithinPhysical    = $00003FFF;
    BlockAlignWord209Supported               = $00004000;


//---------------------------------------------------------------------
// Valid Attribute IDs
//---------------------------------------------------------------------

const
    ATTR_INVALID                = 0 ;
    ATTR_READ_ERROR_RATE        = 1 ;
    ATTR_THROUGHPUT_PERF        = 2 ;
    ATTR_SPIN_UP_TIME           = 3 ;
    ATTR_START_STOP_COUNT       = 4 ;
    ATTR_REALLOC_SECTOR_COUNT   = 5 ;
    ATTR_READ_CHANNEL_MARGIN    = 6 ;
    ATTR_SEEK_ERROR_RATE        = 7 ;
    ATTR_SEEK_TIME_PERF         = 8 ;
    ATTR_POWER_ON_COUNT         = 9 ;
    ATTR_SPIN_RETRY_COUNT       = 10 ;
    ATTR_CALIBRATION_RETRY_COUNT   = 11 ;
    ATTR_POWER_CYCLE_COUNT         = 12 ;
    Attr_Emergency_Retract_Cycle   = 192 ; // C0
    Attr_Load_Cycle_Count          = 193 ; // C1
    Attr_Temperature_Celcius       = 194 ; // C2
    Attr_Hardware_ECC              = 195 ; // C3
    Attr_Reallocation_Event_Count  = 196 ; // C4
    Attr_Current_Pending_Sector    = 197 ; // C5
    Attr_Off_line_Uncorrectable    = 198 ; // C6
    Attr_Ultra_ATA_CRC_Error_Rate  = 199 ; // C7
    Attr_Multi_Zone_Error_Rate     = 200 ; // C8
    Attr_Soft_Read_Error_Rate      = 201 ; // C9
    Attr_Off_Track_Errors          = 201 ; // C9 - alternative Maxtor
    Attr_TA_Increase_Count         = 202 ; // CA
    Attr_Run_Out_Cancel            = 203 ; // CB
    Attr_ECC_Errors                = 203 ; // CB - alternative Maxtor
    Attr_Shock_Count_Write_Ops     = 204 ; // CC
    Attr_Shock_Rate_Write_Ops      = 205 ; // CD
    Attr_Unknown_Attribute         = 206 ; // CE
    Attr_Spin_High_Current         = 207 ; // CE
    Attr_Spin_Buzz                 = 208 ; // CF
    Attr_Offline_Seek_Perf         = 209 ; // D0


//---------------------------------------------------------------------
// Status Flags Values
//---------------------------------------------------------------------
    PRE_FAILURE_WARRANTY        = $1 ;
    ON_LINE_COLLECTION          = $2 ;
    PERFORMANCE_ATTRIBUTE       = $4 ;
    ERROR_RATE_ATTRIBUTE        = $8 ;
    EVENT_COUNT_ATTRIBUTE       = $10 ;
    SELF_PRESERVING_ATTRIBUTE   = $20 ;

    NUM_ATTRIBUTE_STRUCTS       = 30 ;


// Declare a global structure to help print the data.
// NOTE: Per ATA3 and ATA4 specs, these attribute definitions are defined by the drive vendor
// and hence their attributes may vary between vendors.
const
    MAX_KNOWN_ATTRIBUTES = 12 ;

var
    pAttrNames: array [0..13] of string = (
        'No Attribute Here','Raw Read Error Rate','Throughput Performance',
        'Spin Up Time','Start/Stop Count','Reallocated Sector Count',
        'Read Channel Margin','Seek Error Rate','Seek Time Performance',
        'Power On Count','Spin Retry Count','Calibration Retry Count',
        'Power Cycle Count','(Unknown attribute)') ;
    pAttrNames2: array [0..17] of string = (
        'Emergency Retract Cycle','Load Cycle Count','Temperature Celsius',
        'Hardware ECC','Reallocation Event Count','Current Pending Sector',
        'Off-line Uncorrectable','Ultra ATA CRC Error Rate','Multi Zone Error Rate',
        'Off Track Errors','TA Increase Count','ECC Errors',
        'Shock Count Write Ops','Shock Rate Write Ops','Unknown Attribute',
        'Spin High Current','Spin Buzz','Offline Seek Perf') ;

// buffer sizes
    AttrOutLen: integer = sizeof (TSendCmdOutParams) + READ_ATTRIBUTE_BUFFER_SIZE  ;
    ThreshOutLen: integer = sizeof (TSendCmdOutParams) + READ_THRESHOLD_BUFFER_SIZE ;
    IdOutLen: integer = sizeof (TSendCmdOutParams) + IDENTIFY_BUFFER_SIZE ;

// SCSI Pass Through stuff
{$ALIGN ON}
type
    TScsiPassThrough = record
        Length             : Word;
        ScsiStatus         : Byte;
        PathId             : Byte;
        TargetId           : Byte;
        Lun                : Byte;
        CdbLength          : Byte;
        SenseInfoLength    : Byte;
        DataIn             : Byte;
        DataTransferLength : ULONG;
        TimeOutValue       : ULONG;
        DataBufferOffset   : DWORD;
        SenseInfoOffset    : ULONG;
        Cdb                : Array[0..15] of Byte;
    end;
    TScsiPassThroughWithBuffers = record
        spt : TScsiPassThrough;
        bSenseBuf : Array[0..31] of Byte;
        bDataBuf : Array[0..191] of Byte;    // TInquiryData for SCSIOP_INQUIRY with flags=0
    end;

// IOCTL_SCSI_MINIPORT

// Define header for I/O control SRB. }
type
    TSrbIoControl = packed record
        HeaderLength : ULONG;  // Is sizeof(SRB_IO_CONTROL).
        // Identifies the application-dedicated, target HBA for this request.
        // This signature is used to prevent conflicts in ControlCode values between vendors.
        // It should be a string of ASCII characters.
        // If a miniport driver does not recognize the input Signature value,
        // it must complete the request with a status of SRB_STATUS_INVALID_REQUEST.
        Signature    : Array[0..7] of AnsiChar;
        // Indicates the interval in seconds that the request can execute
        // before the OS-specific port driver might consider it timed out.
        // Miniport drivers are not required to time requests because the port driver does.
        Timeout      : ULONG;
        // Indicates the operation to be performed. There are no system-defined operations.
        // Values must be defined by the driver as a set of private I/O control codes
        // with which the application can make requests by calling the Win32R DeviceIoControl function.
        ControlCode  : ULONG;
        // Returns a status code for examination by the requesting application.
        ReturnCode   : ULONG;
        // Indicates the size in bytes of the immediately following data area.
        // This area can be divided for the particular operation into input and output areas.
        // For input requests, the contents of the DataBuffer will be copied
        // to the requestor up to the returned value of DataTransferLength.
        Length       : ULONG;
    end;
    SRB_IO_CONTROL = TSrbIoControl;
    PSrbIoControl = ^TSrbIoControl;

//=============================================================
// Used with the IOCTL_SCSI_GET_INQUIRY_DATA IOCTL.
type
    // Define SCSI adapter bus information.
    TScsiBusData = record  // warning! dword alignment
        NumberOfLogicalUnits : Byte;
        InitiatorBusId       : Byte;
        InquiryDataOffset    : ULONG;
    end;
    SCSI_BUS_DATA = TScsiBusData;
    PScsiBusData = ^TScsiBusData;

    // Define SCSI adapter bus information structure..
    TScsiAdapterBusInfo = record // warning! dword alignment
        NumberOfBuses : Byte;
        BusData : Array[0..0] of SCSI_BUS_DATA;
        Buffer: Array [0..2047] of byte ;
    end;
    SCSI_ADAPTER_BUS_INFO = TScsiAdapterBusInfo;
    PScsiAdapterBusInfo = ^TScsiAdapterBusInfo;

// the SCSI buffer is documented at http://en.wikipedia.org/wiki/SCSI_Inquiry_Command
    TInquiryData = record
        DeviceType: UCHAR;   // Peripheral Device Type, only low 5- bits
        DeviceTypeModifier: UCHAR;   // Device Type Qualifier &  Removable Media Bit
        Versions: UCHAR;
        Flags1: UCHAR;
        Addlen : UCHAR;     // Additional Length
        Reserved: array [0..1] of UCHAR;    // Reserved
        Flags2: UCHAR ;
        sVendorId: array [1..8] of UCHAR;   // Vendor ID (ASCII)
        sProductId: array[1..16] of UCHAR;  // Product ID (ASCII)
        sProductRevisionLevel: array [1..4] of UCHAR;   // Revision level (ASCII)
        sVendorSpecific: array [1..20] of UCHAR;
    end;
    PInquiryData = ^TInquiryData;

    TScsiInquiryData = packed record // warning! dword alignment
        PathId                : Byte;
        TargetId              : Byte;
        Lun                   : Byte;
        DeviceClaimed         : Boolean;
        InquiryDataLength     : ULONG;
        NextInquiryDataOffset : ULONG;
        InquiryData           : TInquiryData;
    end;
    SCSI_INQUIRY_DATA = TScsiInquiryData;
    PScsiInquiryData = ^TScsiInquiryData;

{$ALIGN OFF}

// some STORAGE device structures for IOCTL_STORAGE_QUERY_PROPERT
type

    TStorageQueryType = (PropertyStandardQuery, PropertyExistsQuery,
              PropertyMaskQuery, PropertyQueryMaxDefined) ;

    TStoragePropertyId = (StorageDeviceProperty, StorageAdapterProperty,
              StorageDeviceIdProperty, StorageDeviceUniqueIdProperty,
              StorageDeviceWriteCacheProperty, StorageMiniportProperty,
              StorageAccessAlignmentProperty) ;

    TStoragePropertyQuery = packed record    // STORAGE_PROPERTY_QUERY
        PropertyId: DWORD ;
        QueryType: DWORD ;
        AdditionalParameters: array[0..3] of UChar ;
    end;

    TStorageDeviceDescr = packed record     //  STORAGE_DEVICE_DESCRIPTOR
        Version: ULONG;
        Size: ULONG;
        DeviceType: UChar;
        DeviceTypeModifier: UChar;
        RemovableMedia: Boolean;
        CommandQueueing: Boolean;
        VendorIdOffset: ULONG;
        ProductIdOffset: ULONG;
        ProductRevisionOffset: ULONG;
        SerialNumberOffset: ULONG;
        BusType: DWORD;
        RawPropertiesLength: ULONG;
        RawDeviceProperties: array[0..511] of UChar;
    end;

    // STORAGE_BUS_TYPE, note reported not very reliable, ie Ata instead of Sas/Sata
const
    BusTypeUnknown            = 0 ;
    BusTypeScsi               = 1 ;
    BusTypeAtapi              = 2 ;
    BusTypeAta                = 3 ;
    BusType1394               = 4 ;
    BusTypeSsa                = 5 ;
    BusTypeFibre              = 6 ;
    BusTypeUsb                = 7 ;
    BusTypeRAID               = 8 ;
    BusTypeiScsi              = 9 ;
    BusTypeSas                = 10 ;
    BusTypeSata               = 11 ;
    BusTypeSd                 = 12 ;
    BusTypeMmc                = 13 ;
    BusTypeVirtual            = 14 ;
    BusTypeFileBackedVirtual  = 15 ;
    BusTypeSpaces             = 16 ;
    BusTypeMax                = 17 ;
    BusTypeMaxReserved        = 127 ;
var
    BusTypeNames: array [0..BusTypeSpaces] of string = (
        'Unknown', 'SCSI', 'ATAPI', 'ATA', '1394', 'SSA', 'Fibre', 'USB', 'RAID',
        'iSCSI', 'SAS', 'SATA', 'SD', 'MMC', 'Virtual', 'eFileBackedVirtual', 'Spaces') ;

const


// Following are defined in ntdddisk.h in the MEDIA_TYPE enum
    Unknown            =  0; // Format is unknown
    F5_1Pt2_512        =  1; // 5.25", 1.2MB,  512 bytes/sector
    F3_1Pt44_512       =  2; // 3.5",  1.44MB, 512 bytes/sector
    F3_2Pt88_512       =  3; // 3.5",  2.88MB, 512 bytes/sector
    F3_20Pt8_512       =  4; // 3.5",  20.8MB, 512 bytes/sector
    F3_720_512         =  5; // 3.5",  720KB,  512 bytes/sector
    F5_360_512         =  6; // 5.25", 360KB,  512 bytes/sector
    F5_320_512         =  7; // 5.25", 320KB,  512 bytes/sector
    F5_320_1024        =  8; // 5.25", 320KB,  1024 bytes/sector
    F5_180_512         =  9; // 5.25", 180KB,  512 bytes/sector
    F5_160_512         = 10; // 5.25", 160KB,  512 bytes/sector
    RemovableMedia     = 11; // Removable media other than floppy
    FixedMedia         = 12; // Fixed hard disk media
    F3_120M_512        = 13; // 3.5", 120M Floppy
    F3_640_512         = 14; // 3.5" ,  640KB,  512 bytes/sector
    F5_640_512         = 15; // 5.25",  640KB,  512 bytes/sector
    F5_720_512         = 16; // 5.25",  720KB,  512 bytes/sector
    F3_1Pt2_512        = 17; // 3.5" ,  1.2Mb,  512 bytes/sector
    F3_1Pt23_1024      = 18; // 3.5" ,  1.23Mb, 1024 bytes/sector
    F5_1Pt23_1024      = 19; // 5.25",  1.23MB, 1024 bytes/sector
    F3_128Mb_512       = 20; // 3.5" MO 128Mb   512 bytes/sector
    F3_230Mb_512       = 21; // 3.5" MO 230Mb   512 bytes/sector
    F8_256_128         = 22; // 8",     256KB,  128 bytes/sector

// STORAGE_MEDIA_TYPE
    DDS_4mm            = 32; // Tape - DAT DDS1,2,... (all vendors)
    MiniQic            = 33; // Tape - miniQIC Tape
    Travan             = 34; // Tape - Travan TR-1,2,3,...
    QIC                = 35; // Tape - QIC
    MP_8mm             = 36; // Tape - 8mm Exabyte Metal Particle
    AME_8mm            = 37; // Tape - 8mm Exabyte Advanced Metal Evap
    AIT1_8mm           = 38; // Tape - 8mm Sony AIT1
    DLT                = 39; // Tape - DLT Compact IIIxt, IV
    NCTP               = 40; // Tape - Philips NCTP
    IBM_3480           = 41; // Tape - IBM 3480
    IBM_3490E          = 42; // Tape - IBM 3490E
    IBM_Magstar_3590   = 43; // Tape - IBM Magstar 3590
    IBM_Magstar_MP     = 44; // Tape - IBM Magstar MP
    STK_DATA_D3        = 45; // Tape - STK Data D3
    SONY_DTF           = 46; // Tape - Sony DTF
    DV_6mm             = 47; // Tape - 6mm Digital Video
    DMI                = 48; // Tape - Exabyte DMI and compatibles
    SONY_D2            = 49; // Tape - Sony D2S and D2L
    CLEANER_CARTRIDGE  = 50; // Cleaner - All Drive types that support Drive Cleaners
    CD_ROM             = 51; // Opt_Disk - CD
    CD_R               = 52; // Opt_Disk - CD-Recordable (Write Once)
    CD_RW              = 53; // Opt_Disk - CD-Rewriteable
    DVD_ROM            = 54; // Opt_Disk - DVD-ROM
    DVD_R              = 55; // Opt_Disk - DVD-Recordable (Write Once)
    DVD_RW             = 56; // Opt_Disk - DVD-Rewriteable
    MO_3_RW            = 57; // Opt_Disk - 3.5" Rewriteable MO Disk
    MO_5_WO            = 58; // Opt_Disk - MO 5.25" Write Once
    MO_5_RW            = 59; // Opt_Disk - MO 5.25" Rewriteable (not LIMDOW)
    MO_5_LIMDOW        = 60; // Opt_Disk - MO 5.25" Rewriteable (LIMDOW)
    PC_5_WO            = 61; // Opt_Disk - Phase Change 5.25" Write Once Optical
    PC_5_RW            = 62; // Opt_Disk - Phase Change 5.25" Rewriteable
    PD_5_RW            = 63; // Opt_Disk - PhaseChange Dual Rewriteable
    ABL_5_WO           = 64; // Opt_Disk - Ablative 5.25" Write Once Optical
    PINNACLE_APEX_5_RW = 65; // Opt_Disk - Pinnacle Apex 4.6GB Rewriteable Optical
    SONY_12_WO         = 66; // Opt_Disk - Sony 12" Write Once
    PHILIPS_12_WO      = 67; // Opt_Disk - Philips/LMS 12" Write Once
    HITACHI_12_WO      = 68; // Opt_Disk - Hitachi 12" Write Once
    CYGNET_12_WO       = 69; // Opt_Disk - Cygnet/ATG 12" Write Once
    KODAK_14_WO        = 70; // Opt_Disk - Kodak 14" Write Once
    MO_NFR_525         = 71; // Opt_Disk - Near Field Recording (Terastor)
    NIKON_12_RW        = 72; // Opt_Disk - Nikon 12" Rewriteable
    IOMEGA_ZIP         = 73; // Mag_Disk - Iomega Zip
    IOMEGA_JAZ         = 74; // Mag_Disk - Iomega Jaz
    SYQUEST_EZ135      = 75; // Mag_Disk - Syquest EZ135
    SYQUEST_EZFLYER    = 76; // Mag_Disk - Syquest EzFlyer
    SYQUEST_SYJET      = 77; // Mag_Disk - Syquest SyJet
    AVATAR_F2          = 78; // Mag_Disk - 2.5" Floppy
    MP2_8mm            = 79; // Tape - 8mm Hitachi
    DST_S              = 80; // Ampex DST Small Tapes
    DST_M              = 81; // Ampex DST Medium Tapes
    DST_L              = 82; // Ampex DST Large Tapes
    VXATape_1          = 83; // Ecrix 8mm Tape
    VXATape_2          = 84; // Ecrix 8mm Tape
    STK_EAGLE          = 85; // STK Eagle
    LTO_Ultrium        = 86; // IBM, HP, Seagate LTO Ultrium
    LTO_Accelis        = 87; // IBM, HP, Seagate LTO Accelis

// IOCTL_DISK_GET_DRIVE_GEOMETRY_EX
type
    TDiskGeometry = packed record
        Cylinders: int64 ;
        MediaType: ULONG ;
        TracksPerCylinder: ULONG ;
        SectorsPerTrack: ULONG ;
        BytesPerSector: ULONG ;
    end;

    TDiskGeometryEx = packed record
        Geometry : TDiskGeometry;
        DiskSize : int64;
        Data : array [0..255] of UCHAR;  //  DISK_PARTITION_INFO followed by DISK_DETECTION_INFO
    end;


implementation

end .
