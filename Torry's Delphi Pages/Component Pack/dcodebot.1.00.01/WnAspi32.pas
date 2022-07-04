
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit WnAspi32;

interface

{$I STD.INC}

uses
  Windows;

  { TARGET STATUS VALUES }

const
  STATUS_GOOD     = $00;    // Status Good
  STATUS_CHKCOND  = $02;    // Check Condition
  STATUS_CONDMET  = $04;    // Condition Met
  STATUS_BUSY     = $08;    // Busy
  STATUS_INTERM   = $10;    // Intermediate
  STATUS_INTCDMET = $14;    // Intermediate-condition met
  STATUS_RESCONF  = $18;    // Reservation conflict
  STATUS_COMTERM  = $22;    // Command Terminated
  STATUS_QFULL    = $28;    // Queue full

  { SCSI MISCELLANEOUS EQUATES }

  MAXLUN          =  7;    // Maximum Logical Unit Id
  MAXTARG         =  7;    // Maximum Target Id
  MAX_SCSI_LUNS   = 64;    // Maximum Number of SCSI LUNs
  MAX_NUM_HA      =  8;    // Maximum Number of SCSI HA's

  { SCSI COMMAND OPCODES }

  { Commands for all Device Types }

  SCSI_CHANGE_DEF = $40;    // Change Definition (Optional)
  SCSI_COMPARE    = $39;    // Compare (O)
  SCSI_COPY       = $18;    // Copy (O)
  SCSI_COP_VERIFY = $3A;    // Copy and Verify (O)
  SCSI_INQUIRY    = $12;    // Inquiry (MANDATORY)
  SCSI_LOG_SELECT = $4C;    // Log Select (O)
  SCSI_LOG_SENSE  = $4D;    // Log Sense (O)
  SCSI_MODE_SEL6  = $15;    // Mode Select 6-byte (Device Specific)
  SCSI_MODE_SEL10 = $55;    // Mode Select 10-byte (Device Specific)
  SCSI_MODE_SEN6  = $1A;    // Mode Sense 6-byte (Device Specific)
  SCSI_MODE_SEN10 = $5A;    // Mode Sense 10-byte (Device Specific)
  SCSI_READ_BUFF  = $3C;    // Read Buffer (O)
  SCSI_REQ_SENSE  = $03;    // Request Sense (MANDATORY)
  SCSI_SEND_DIAG  = $1D;    // Send Diagnostic (O)
  SCSI_TST_U_RDY  = $00;    // Test Unit Ready (MANDATORY)
  SCSI_WRITE_BUFF = $3B;    // Write Buffer (O)

  { Commands Unique to Direct Access Devices }

  SCSI_FORMAT     = $04;    // Format Unit (MANDATORY)
  SCSI_LCK_UN_CAC = $36;    // Lock Unlock Cache (O)
  SCSI_PREFETCH   = $34;    // Prefetch (O)
  SCSI_MED_REMOVL = $1E;    // Prevent/Allow medium Removal (O)
  SCSI_READ6      = $08;    // Read 6-byte (MANDATORY)
  SCSI_READ10     = $28;    // Read 10-byte (MANDATORY)
  SCSI_RD_CAPAC   = $25;    // Read Capacity (MANDATORY)
  SCSI_RD_DEFECT  = $37;    // Read Defect Data (O)
  SCSI_READ_LONG  = $3E;    // Read Long (O)
  SCSI_REASS_BLK  = $07;    // Reassign Blocks (O)
  SCSI_RCV_DIAG   = $1C;    // Receive Diagnostic Results (O)
  SCSI_RELEASE    = $17;    // Release Unit (MANDATORY)
  SCSI_REZERO     = $01;    // Rezero Unit (O)
  SCSI_SRCH_DAT_E = $31;    // Search Data Equal (O)
  SCSI_SRCH_DAT_H = $30;    // Search Data High (O)
  SCSI_SRCH_DAT_L = $32;    // Search Data Low (O)
  SCSI_SEEK6      = $0B;    // Seek 6-Byte (O)
  SCSI_SEEK10     = $2B;    // Seek 10-Byte (O)
  SCSI_SET_LIMIT  = $33;    // Set Limits (O)
  SCSI_START_STP  = $1B;    // Start/Stop Unit (O)
  SCSI_SYNC_CACHE = $35;    // Synchronize Cache (O)
  SCSI_VERIFY     = $2F;    // Verify (O)
  SCSI_WRITE6     = $0A;    // Write 6-Byte (MANDATORY)
  SCSI_WRITE10    = $2A;    // Write 10-Byte (MANDATORY)
  SCSI_WRT_VERIFY = $2E;    // Write and Verify (O)
  SCSI_WRITE_LONG = $3F;    // Write Long (O)
  SCSI_WRITE_SAME = $41;    // Write Same (O)

  { Commands Unique to Sequential Access Devices }

  SCSI_ERASE      = $19;    // Erase (MANDATORY)
  SCSI_LOAD_UN    = $1B;    // Load/Unload (O)
  SCSI_LOCATE     = $2B;    // Locate (O)
  SCSI_RD_BLK_LIM = $05;    // Read Block Limits (MANDATORY)
  SCSI_READ_POS   = $34;    // Read Position (O)
  SCSI_READ_REV   = $0F;    // Read Reverse (O)
  SCSI_REC_BF_DAT = $14;    // Recover Buffer Data (O)
  SCSI_RESERVE    = $16;    // Reserve Unit (MANDATORY)
  SCSI_REWIND     = $01;    // Rewind (MANDATORY)
  SCSI_SPACE      = $11;    // Space (MANDATORY)
  SCSI_VERIFY_T   = $13;    // Verify (Tape) (O)
  SCSI_WRT_FILE   = $10;    // Write Filemarks (MANDATORY)

  { Commands Unique to Printer Devices }

  SCSI_PRINT      = $0A;    // Print (MANDATORY)
  SCSI_SLEW_PNT   = $0B;    // Slew and Print (O)
  SCSI_STOP_PNT   = $1B;    // Stop Print (O)
  SCSI_SYNC_BUFF  = $10;    // Synchronize Buffer (O)

  { Commands Unique to Processor Devices }

  SCSI_RECEIVE    = $08    ;    // Receive (O)
  SCSI_SEND       = $0A    ;    // Send (O)

  { Commands Unique to Write-Once Devices }

  SCSI_MEDIUM_SCN = $38;    // Medium Scan (O)
  SCSI_SRCHDATE10 = $31;    // Search Data Equal 10-Byte (O)
  SCSI_SRCHDATE12 = $B1;    // Search Data Equal 12-Byte (O)
  SCSI_SRCHDATH10 = $30;    // Search Data High 10-Byte (O)
  SCSI_SRCHDATH12 = $B0;    // Search Data High 12-Byte (O)
  SCSI_SRCHDATL10 = $32;    // Search Data Low 10-Byte (O)
  SCSI_SRCHDATL12 = $B2;    // Search Data Low 12-Byte (O)
  SCSI_SET_LIM_10 = $33;    // Set Limits 10-Byte (O)
  SCSI_SET_LIM_12 = $B3;    // Set Limits 10-Byte (O)
  SCSI_VERIFY10   = $2F;    // Verify 10-Byte (O)
  SCSI_VERIFY12   = $AF;    // Verify 12-Byte (O)
  SCSI_WRITE12    = $AA;    // Write 12-Byte (O)
  SCSI_WRT_VER10  = $2E;    // Write and Verify 10-Byte (O)
  SCSI_WRT_VER12  = $AE;    // Write and Verify 12-Byte (O)

  { Commands Unique to CD-ROM Devices }

  SCSI_PLAYAUD_10 = $45;    // Play Audio 10-Byte (O)
  SCSI_PLAYAUD_12 = $A5;    // Play Audio 12-Byte 12-Byte (O)
  SCSI_PLAYAUDMSF = $47;    // Play Audio MSF (O)
  SCSI_PLAYA_TKIN = $48;    // Play Audio Track/Index (O)
  SCSI_PLYTKREL10 = $49;    // Play Track Relative 10-Byte (O)
  SCSI_PLYTKREL12 = $A9;    // Play Track Relative 12-Byte (O)
  SCSI_READCDCAP  = $25;    // Read CD-ROM Capacity (MANDATORY)
  SCSI_READHEADER = $44;    // Read Header (O)
  SCSI_SUBCHANNEL = $42;    // Read Subchannel (O)
  SCSI_READ_TOC   = $43;    // Read TOC (O)

  { Commands Unique to Scanner Devices }

  SCSI_GETDBSTAT  = $34;    // Get Data Buffer Status (O)
  SCSI_GETWINDOW  = $25;    // Get Window (O)
  SCSI_OBJECTPOS  = $31;    // Object Postion (O)
  SCSI_SCAN       = $1B;    // Scan (O)
  SCSI_SETWINDOW  = $24;    // Set Window (MANDATORY)

  { Commands Unique to Optical Memory Devices }

  SCSI_UpdateBlk  = $3D;    // Update Block (O)

  { Commands Unique to Medium Changer Devices }

  SCSI_EXCHMEDIUM = $A6;    // Exchange Medium (O)
  SCSI_INITELSTAT = $07;    // Initialize Element Status (O)
  SCSI_POSTOELEM  = $2B;    // Position to Element (O)
  SCSI_REQ_VE_ADD = $B5;    // Request Volume Element Address (O)
  SCSI_SENDVOLTAG = $B6;    // Send Volume Tag (O)

   { Commands Unique to Communication Devices }

  SCSI_GET_MSG_6  = $08;    // Get Message 6-Byte (MANDATORY)
  SCSI_GET_MSG_10 = $28;    // Get Message 10-Byte (O)
  SCSI_GET_MSG_12 = $A8;    // Get Message 12-Byte (O)
  SCSI_SND_MSG_6  = $0A;    // Send Message 6-Byte (MANDATORY)
  SCSI_SND_MSG_10 = $2A;    // Send Message 10-Byte (O)
  SCSI_SND_MSG_12 = $AA;    // Send Message 12-Byte (O)

  { END OF SCSI COMMAND OPCODES }

  { Request Sense Data Format }

type
  PSenseDataFmt = ^TSRBSenseDataFmt;
  SENSE_DATA_FMT = record
    ErrorCode: Byte;          // Error Code (70H or 71H)
    SegmentNum: Byte;         // Number of current segment descriptor
    SenseKey: Byte;           // Sense Key(See bit definitions too)
    InfoByte0: Byte;          // Information MSB
    InfoByte1: Byte;          // Information MID
    InfoByte2: Byte;          // Information MID
    InfoByte3: Byte;          // Information LSB
    AddSenLen: Byte;          // Additional Sense Length
    ComSpecInf0: Byte;        // Command Specific Information MSB
    ComSpecInf1: Byte;        // Command Specific Information MID
    ComSpecInf2: Byte;        // Command Specific Information MID
    ComSpecInf3: Byte;        // Command Specific Information LSB
    AddSenseCode: Byte;       // Additional Sense Code
    AddSenQual: Byte;         // Additional Sense Code Qualifier
    FieldRepUCode: Byte;      // Field Replaceable Unit Code
    SenKeySpec15: Byte;       // Sense Key Specific 15th byte
    SenKeySpec16: Byte;       // Sense Key Specific 16th byte
    SenKeySpec17: Byte;       // Sense Key Specific 17th byte
    AddSenseBytes: Byte;      // Additional Sense Bytes
  end;
  TSRBSenseDataFmt = SENSE_DATA_FMT;

  { REQUEST SENSE ERROR CODE }

const
  SERROR_CURRENT  = $70;    // Current Errors
  SERROR_DEFERED  = $71;    // Deferred Errors

  { REQUEST SENSE BIT DEFINITIONS }

  SENSE_VALID     = $80;    // Byte 0 Bit 7
  SENSE_FILEMRK   = $80;    // Byte 2 Bit 7
  SENSE_EOM       = $40;    // Byte 2 Bit 6
  SENSE_ILI       = $20;    // Byte 2 Bit 5

  { REQUEST SENSE SENSE KEY DEFINITIONS }

  KEY_NOSENSE     = $00;    // No Sense
  KEY_RECERROR    = $01;    // Recovered Error
  KEY_NOTREADY    = $02;    // Not Ready
  KEY_MEDIUMERR   = $03;    // Medium Error
  KEY_HARDERROR   = $04;    // Hardware Error
  KEY_ILLGLREQ    = $05;    // Illegal Request
  KEY_UNITATT     = $06;    // Unit Attention
  KEY_DATAPROT    = $07;    // Data Protect
  KEY_BLANKCHK    = $08;    // Blank Check
  KEY_VENDSPEC    = $09;    // Vendor Specific
  KEY_COPYABORT   = $0A;    // Copy Abort
  KEY_EQUAL       = $0C;    // Equal (Search)
  KEY_VOLOVRFLW   = $0D;    // Volume Overflow
  KEY_MISCOMP     = $0E;    // Miscompare (Search)
  KEY_RESERVED    = $0F;    // Reserved

  { PERIPHERAL DEVICE TYPE DEFINITIONS }

  DTYPE_DASD      = $00;    // Disk Device
  DTYPE_SEQD      = $01;    // Tape Device
  DTYPE_PRNT      = $02;    // Printer
  DTYPE_PROC      = $03;    // Processor
  DTYPE_WORM      = $04;    // Write-once read-multiple
  DTYPE_CROM      = $05;    // CD-ROM device
  DTYPE_CDROM     = $05;    // CD-ROM device
  DTYPE_SCAN      = $06;    // Scanner device
  DTYPE_OPTI      = $07;    // Optical memory device
  DTYPE_JUKE      = $08;    // Medium Changer device
  DTYPE_COMM      = $09;    // Communications device
  DTYPE_RESL      = $0A;    // Reserved (low)
  DTYPE_RESH      = $1E;    // Reserved (high)
  DTYPE_UNKNOWN   = $1F;    // Unknown or no device type

  { ANSI APPROVED VERSION DEFINITIONS }

  ANSI_MAYBE      = $0 ;    // Device may or may not be ANSI approved stand
  ANSI_SCSI1      = $1 ;    // Device complies to ANSI X3.131-1986 (SCSI-1)
  ANSI_SCSI2      = $2 ;    // Device complies to SCSI-2
  ANSI_RESLO      = $3 ;    // Reserved (low)
  ANSI_RESHI      = $7 ;    // Reserved (high)

  SENSE_LEN                   =  14;        // Default sense buffer length
  SRB_DIR_SCSI                = $00;        // Direction determined by SCSI
  SRB_POSTING                 = $01;        // Enable ASPI posting
  SRB_ENABLE_RESIDUAL_COUNT   = $04;        // Enable residual byte count reporting
  SRB_DIR_IN                  = $08;        // Transfer from SCSI target to host
  SRB_DIR_OUT                 = $10;        // Transfer from host to SCSI target
  SRB_EVENT_NOTIFY            = $40;        // Enable ASPI event notification

  RESIDUAL_COUNT_SUPPORTED    = $02;        // Extended buffer flag
  MAX_SRB_TIMEOUT          = 108000;        // 30 hour maximum timeout in s
  DEFAULT_SRB_TIMEOUT      = 108000;        // Max timeout by default

  { ASPI Command Definitions }

  SC_HA_INQUIRY               = $00;        // Host adapter inquiry
  SC_GET_DEV_TYPE             = $01;        // Get device type
  SC_EXEC_SCSI_CMD            = $02;        // Execute SCSI command
  SC_ABORT_SRB                = $03;        // Abort an SRB
  SC_RESET_DEV                = $04;        // SCSI bus device reset
  SC_SET_HA_PARMS             = $05;        // Set HA parameters
  SC_GET_DISK_INFO            = $06;        // Get Disk information
  SC_RESCAN_SCSI_BUS          = $07;        // ReBuild SCSI device map
  SC_GETSET_TIMEOUTS          = $08;        // Get/Set target timeouts

  { SRB Status }

  SS_PENDING                  = $00;        // SRB being processed
  SS_COMP                     = $01;        // SRB completed without error
  SS_ABORTED                  = $02;        // SRB aborted
  SS_ABORT_FAIL               = $03;        // Unable to abort SRB
  SS_ERR                      = $04;        // SRB completed with error

  SS_INVALID_CMD              = $80;        // Invalid ASPI command
  SS_INVALID_HA               = $81;        // Invalid host adapter number
  SS_NO_DEVICE                = $82;        // SCSI device not installed

  SS_INVALID_SRB              = $E0;        // Invalid parameter set in SRB
  SS_OLD_MANAGER              = $E1;        // ASPI manager doesn't support Windows
  SS_BUFFER_ALIGN             = $E1;        // Buffer not aligned (replaces OLD_MANAGER in Win32)
  SS_ILLEGAL_MODE             = $E2;        // Unsupported Windows mode
  SS_NO_ASPI                  = $E3;        // No ASPI managers resident
  SS_FAILED_INIT              = $E4;        // ASPI for windows failed init
  SS_ASPI_IS_BUSY             = $E5;        // No resources available to execute cmd
  SS_BUFFER_TO_BIG            = $E6;        // Buffer size to big to handle!
  SS_MISMATCHED_COMPONENTS    = $E7;        // The DLLs/EXEs of ASPI don't version check
  SS_NO_ADAPTERS              = $E8;        // No host adapters to manage
  SS_INSUFFICIENT_RESOURCES   = $E9;        // Couldn't allocate resources needed to init
  SS_ASPI_IS_SHUTDOWN         = $EA;        // Call came to ASPI after PROCESS_DETACH
  SS_BAD_INSTALL              = $EB;        // The DLL or other components are installed wrong

  { Host Adapter Status }

  HASTAT_OK                   = $00;        // Host adapter did not detect an
  HASTAT_SEL_TO               = $11;        // Selection Timeout
  HASTAT_DO_DU                = $12;        // Data overrun data underrun
  HASTAT_BUS_FREE             = $13;        // Unexpected bus free
  HASTAT_PHASE_ERR            = $14;        // Target bus phase sequence
  HASTAT_TIMEOUT              = $09;        // Timed out while SRB was
  HASTAT_COMMAND_TIMEOUT      = $0B;        // Adapter timed out processing SRB.
  HASTAT_MESSAGE_REJECT       = $0D;        // While processing SRB, the
  HASTAT_BUS_RESET            = $0E;        // A bus reset was detected.
  HASTAT_PARITY_ERROR         = $0F;        // A parity error was detected.
  HASTAT_REQUEST_SENSE_FAILED = $10;        // The adapter failed in issuing

type
  PSRB = Pointer;

  { SRB - HOST ADAPTER INQUIRY - SC_HA_INQUIRY (0) }

  PSRBHAInquiry = ^TSRBHAInquiry;
  SRB_HAInquiry = record
    Cmd: Byte;                          // 00/000 ASPI command code = SC_HA_INQUIRY
    Status: Byte;                       // 01/001 ASPI command status byte
    HaId: Byte;                         // 02/002 ASPI host adapter number
    Flags: Byte;                        // 03/003 ASPI request flags
    Rsvd: Cardinal;                     // 04/004 Reserved, MUST = 0
    HaCount: Byte;                      // 08/008 Number of host adapters present
    HaSCSIId: Byte;                     // 09/009 SCSI ID of host adapter
    HaManagerId: array[0..15] of Byte;  // 0A/010 String describing the manager
    HaIdentifier: array[0..15] of Byte; // 1A/026 String describing the host adapter
    HaUnique: array[0..15] of Byte;     // 2A/042 Host Adapter Unique parameters
    HaRsvd1: Word;                      // 3A/058 Reserved, MUST = 0
  end;
  TSRBHAInquiry = SRB_HAInquiry;

  { SRB - GET DEVICE TYPE - SC_GET_DEV_TYPE (1) }

  PSRBGDEVBlock = ^TSRBGDEVBlock;
  SRB_GDEVBlock = record
    Cmd: Byte;                          // 00/000 ASPI command code = SC_GET_DEV_TYPE
    Status: Byte;                       // 01/001 ASPI command status byte
    HaId: Byte;                         // 02/002 ASPI host adapter number
    Flags: Byte;                        // 03/003 Reserved, MUST = 0
    Rsvd: Cardinal;                     // 04/004 Reserved, MUST = 0
    Target: Byte;                       // 08/008 Target's SCSI ID
    Lun: Byte;                          // 09/009 Target's LUN number
    DeviceType: Byte;                   // 0A/010 Target's peripheral device type
    Rsvd1: Byte;                        // 0B/011 Reserved, MUST = 0
  end;
  TSRBGDEVBlock = SRB_GDEVBlock;

  { SRB - EXECUTE SCSI COMMAND - SC_EXEC_SCSI_CMD (2) }

  PSRBExecSCSICmd = ^TSRBExecSCSICmd;
  SRB_ExecSCSICmd = record
    Cmd: Byte;                          // 00/000 ASPI command code = SC_EXEC_SCSI_CMD
    Status: Byte;                       // 01/001 ASPI command status byte
    HaId: Byte;                         // 02/002 ASPI host adapter number
    Flags: Byte;                        // 03/003 ASPI request flags
    Rsvd: Cardinal;                     // 04/004 Reserved
    Target: Byte;                       // 08/008 Target's SCSI ID
    Lun: Byte;                          // 09/009 Target's LUN number
    Rsvd1: Word;                        // 0A/010 Reserved for Alignment
    BufLen: Cardinal;                   // 0C/012 Data Allocation Length
    BufPointer: PByte;                  // 10/016 Data Buffer Pointer
    SenseLen: Byte;                     // 14/020 Sense Allocation Length
    CDBLen: Byte;                       // 15/021 CDB Length
    HaStat: Byte;                       // 16/022 Host Adapter Status
    TargStat: Byte;                     // 17/023 Target Status
    PostProc: TFarProc;                 // 18/024 Post routine
    Rsvd2: array[0..19] of Byte;        // 1C/028 Reserved, MUST = 0
    CDBByte: array[0..15] of Byte;      // 30/048 SCSI CDB
    SenseArea: array[0.. SENSE_LEN + 1] of Byte;  // 50/064 Request Sense buffer
  end;
  TSRBExecSCSICmd = SRB_ExecSCSICmd;

  { SRB - ABORT AN SRB - SC_ABORT_SRB (3) }

  PSRBAbort = ^TSRBAbort;
  SRB_Abort = record
    Cmd: Byte;                          // 00/000 ASPI command code = SC_ABORT_SRB
    Status: Byte;                       // 01/001 ASPI command status byte
    HaId: Byte;                         // 02/002 ASPI host adapter number
    Flags: Byte;                        // 03/003 Reserved
    Rsvd: Cardinal;                     // 04/004 Reserved
    ToAbort: PSRB;                      // 08/008 Pointer to SRB to abort
  end;
  TSRBAbort = SRB_Abort;

  { SRB - BUS DEVICE RESET - SC_RESET_DEV (4) }

  PSRBBusDeviceReset = ^TSRBBusDeviceReset;
  SRB_BusDeviceReset = record
    Cmd: Byte;                          // 00/000 ASPI command code = SC_RESET_DEV
    Status: Byte;                       // 01/001 ASPI command status byte
    HaId: Byte;                         // 02/002 ASPI host adapter number
    Flags: Byte;                        // 03/003 ASPI request flags
    Rsvd: Cardinal;                     // 04/004 Reserved
    Target: Byte;                       // 08/008 Target's SCSI ID
    Lun: Byte;                          // 09/009 Target's LUN number
    Rsvd1: array[0..11] of Byte;        // 0A/010 Reserved for Alignment
    HaStat: Byte;                       // 16/022 Host Adapter Status
    TargStat: Byte;                     // 17/023 Target Status
    PostProc: TFarProc;                 // 18/024 Post routine
    Rsvd2: array[0..35] of Byte;        // 1C/028 Reserved, MUST = 0
  end;
  TSRBBusDeviceReset = SRB_BusDeviceReset;

  { SRB - GET DISK INFORMATION - SC_GET_DISK_INFO }

  PSRBGetDiskInfo = ^TSRBGetDiskInfo;
  SRB_GetDiskInfo = record
    Cmd: Byte;                          // 00/000 ASPI command code = SC_GET_DISK_INFO
    Status: Byte;                       // 01/001 ASPI command status byte
    HaId: Byte;                         // 02/002 ASPI host adapter number
    Flags: Byte;                        // 03/003 Reserved, MUST = 0
    Rsvd: Cardinal;                     // 04/004 Reserved, MUST = 0
    Target: Byte;                       // 08/008 Target's SCSI ID
    Lun: Byte;                          // 09/009 Target's LUN number
    DriveFlags: Byte;                   // 0A/010 Driver flags
    Int13HDriveInfo: Byte;              // 0B/011 Host Adapter Status
    Heads: Byte;                        // 0C/012 Preferred number of heads translation
    Sectors: Byte;                      // 0D/013 Preferred number of sectors translation
    Rsvd1: array[0..9] of Byte;         // 0E/014 Reserved, MUST = 0
  end;
  TSRBGetDiskInfo = SRB_GetDiskInfo;

  {  SRB - RESCAN SCSI BUS(ES) ON SCSIPORT }

  PSRBRescanPort = ^TSRBRescanPort;
  SRB_RescanPort = record
    Cmd: Byte;                          // 00/000 ASPI command code = SC_RESCAN_SCSI_BUS
    Status: Byte;                       // 01/001 ASPI command status byte
    HaId: Byte;                         // 02/002 ASPI host adapter number
    Flags: Byte;                        // 03/003 Reserved, MUST = 0
    Rsvd: Cardinal;                     // 04/004 Reserved, MUST = 0
  end;
  TSRBRescanPort = SRB_RescanPort;

  { SRB - GET/SET TARGET TIMEOUTS }

  PSRBGetSetTimeouts = ^TSRBGetSetTimeouts;
  SRB_GetSetTimeouts = record
    Cmd: Byte;                          // 00/000 ASPI command code = SC_GETSET_TIMEOUTS
    Status: Byte;                       // 01/001 ASPI command status byte
    HaId: Byte;                         // 02/002 ASPI host adapter number
    Flags: Byte;                        // 03/003 ASPI request flags
    Rsvd: Cardinal;                     // 04/004 Reserved, MUST = 0
    Target: Byte;                       // 08/008 Target's SCSI ID
    Lun: Byte;                          // 09/009 Target's LUN number
    Timeout: Cardinal;                  // 0A/010 Timeout in half seconds
  end;
  TSRBGetSetTimeouts = SRB_GetSetTimeouts;

  { ASPIBUFF - Structure For Controllng I/O Buffers }

  PASPI32Buff = ^TASPI32Buff;
  ASPI32BUFF = record
    BufPointer: PByte;                  // 00/000 Pointer to the ASPI allocated buffer
    BufLen: Integer;                    // 04/004 Length in bytes of the buffer
    ZeroFill: Cardinal;                 // 08/008 Flag set to 1 if buffer should be zeroed
    Reserved: Cardinal;                 // 0C/012 Reserved
  end;
  TASPI32Buff = ASPI32BUFF;

{ PROTOTYPES - User Callable ASPI for Win32 Functions }

  TASPI32SupportInfo = record
    HaCount: Byte;
    ASPIStatus: Byte;
    Reserved: Word;
  end;

var
  GetASPI32SupportInfo: function: TASPI32SupportInfo; cdecl = nil;
  SendASPI32Command: function(SRB: PSRB): Cardinal; cdecl = nil;
  GetASPI32Buffer: function(var Buffer: TASPI32Buff): Boolean; cdecl = nil;
  FreeASPI32Buffer: function(const Buffer: TASPI32Buff): Boolean; cdecl = nil;
  TranslateASPI32Address: function(var Path, DevNode: Cardinal): Boolean; cdecl = nil;

implementation

var
  WnAspi32DLL: THandle;

initialization
  WnAspi32DLL := LoadLibrary('wnaspi32.dll');
  if WnAspi32DLL <> 0 then
  begin
    @GetASPI32SupportInfo := GetProcAddress(WnAspi32DLL, 'GetASPI32SupportInfo');
    @SendASPI32Command := GetProcAddress(WnAspi32DLL, 'SendASPI32Command');
    @GetASPI32Buffer := GetProcAddress(WnAspi32DLL, 'GetASPI32Buffer');
    @FreeASPI32Buffer := GetProcAddress(WnAspi32DLL, 'FreeASPI32Buffer');
    @TranslateASPI32Address := GetProcAddress(WnAspi32DLL,'TranslateASPI32Address');
  end;
finalization
  if WnAspi32DLL <> 0 then
    FreeLibrary(WnAspi32DLL);
end.
