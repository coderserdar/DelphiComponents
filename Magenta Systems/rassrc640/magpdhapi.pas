unit magpdhapi;

// Magenta Performance Data Helper (PDH) definitions, Magenta Systems Ltd
// 9th August 2011 - Release 5.70, Copyright 2011, Magenta Systems Ltd

// Copyright by Angus Robertson, Magenta Systems Ltd, England
// delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/

// Changes in 5.40
// Made compatible with Delphi 2009, but still using ANSI RAS functions, not Unicode

interface

uses Windows, MagRasApi, MagSubs1;

// version info
const
  PDH_CVERSION_WIN40 = $0400;
// v1.1 revision of PDH -- basic log functions
// v1.2 of the PDH -- adds variable instance counters
// v1.3 of the PDH -- adds log service control & stubs for NT5/PDH v2 fn's
  PDH_VERSION = PDH_CVERSION_WIN40 + 3;

{$IFDEF VER100} { seems to be missing from Delphi 3 }
type
  { File System time stamps are represented with the following structure: }
  PFileTime = ^TFileTime;
  _FILETIME = record
    dwLowDateTime: DWORD;
    dwHighDateTime: DWORD;
  end;
  TFileTime = _FILETIME;
  FILETIME = _FILETIME;
{$ENDIF}

const
//
//  Time value constants
//
  MAX_TIME_VALUE = $7FFFFFFF;
  MIN_TIME_VALUE = 0;

//
// dwFormat flag values
//
  PDH_FMT_RAW     = $00000010;
  PDH_FMT_ANSI    = $00000020;
  PDH_FMT_UNICODE = $00000040;
  PDH_FMT_LONG    = $00000100;
  PDH_FMT_DOUBLE  = $00000200;
  PDH_FMT_LARGE   = $00000400;
  PDH_FMT_NOSCALE = $00001000;
  PDH_FMT_1000    = $00002000;
  PDH_FMT_NODATA  = $00004000;

  PERF_DETAIL_COSTLY   = $00010000;
  PERF_DETAIL_STANDARD = $0000FFFF;

  PDH_MAX_SCALE    = 7;
  PDH_MIN_SCALE    = -7;

  PDH_LOG_READ_ACCESS     = $00010000;
  PDH_LOG_WRITE_ACCESS    = $00020000;
  PDH_LOG_ACCESS_MASK     = $000F0000;

  PDH_LOG_CREATE_NEW      = $00000001;
  PDH_LOG_CREATE_ALWAYS   = $00000002;
  PDH_LOG_OPEN_ALWAYS     = $00000003;
  PDH_LOG_OPEN_EXISTING   = $00000004;
  PDH_LOG_CREATE_MASK     = $0000000F;

  PDH_LOG_OPT_USER_STRING = $01000000;
  PDH_LOG_OPT_MASK        = $0F000000;

  PDH_LOG_TYPE_UNDEFINED      = 0;
  PDH_LOG_TYPE_CSV            = 1;
  PDH_LOG_TYPE_TSV            = 2;
  PDH_LOG_TYPE_BINARY         = 3;
  PDH_LOG_TYPE_TRACE_KERNEL   = 4;
  PDH_LOG_TYPE_TRACE_GENERIC  = 5;

  PDH_FLAGS_CLOSE_QUERY = $00000001;
//
//  Data source selection dialog
//
  PDH_FLAGS_FILE_BROWSER_ONLY = $00000001;

// flags for the log service api's
  PDH_LOGSVC_NO_WAIT          = $80000000;
  PDH_LOGSVC_ALL_QUERIES      = $00000001;
  PDH_LOGSVC_TRACE_LOG        = $00000002;

  PDH_LOGSVC_CMD_START        = $00000010;
  PDH_LOGSVC_CMD_STOP         = $00000020;

  PDH_LOGSVC_CTRL_ADD         = $00000100;
  PDH_LOGSVC_CTRL_REMOVE      = $00000200;
  PDH_LOGSVC_CTRL_INFO        = $00000400;

  PDH_LOGSVC_STATUS_RUNNING   = $00001000;
  PDH_LOGSVC_STATUS_STOPPED   = $00002000;
  PDH_LOGSVC_STATUS_PAUSED    = $00004000;
  PDH_LOGSVC_STATUS_ERROR     = $00008000;
  PDH_LOGSVC_STATUS_PENDING   = $00010000;

  PDH_LOGSVC_NAME_UNDEFINED  = -1;
  PDH_LOGSVC_NAME_MMDDHH     = 0;
  PDH_LOGSVC_NAME_NNNNNN     = 1;
  PDH_LOGSVC_NAME_YYDDD      = 2;
  PDH_LOGSVC_NAME_YYMM       = 3;
  PDH_LOGSVC_NAME_YYMMDD     = 4;
  PDH_LOGSVC_NAME_YYMMDDHH   = 5;

  PDH_LOGSVC_RENAME_UNDEFINED = -1;
  PDH_LOGSVC_RENAME_HOURS     = 0;
  PDH_LOGSVC_RENAME_DAYS      = 1;
  PDH_LOGSVC_RENAME_MONTHS    = 2;
  PDH_LOGSVC_RENAME_KBYTES    = 3;
  PDH_LOGSVC_RENAME_MBYTES    = 4;

  PDH_CSTATUS_VALID_DATA           = $00000000;
  PDH_CSTATUS_NEW_DATA             = $00000001;
  PDH_CSTATUS_NO_MACHINE           = $800007D0;
  PDH_CSTATUS_NO_INSTANCE          = $800007D1;
  PDH_MORE_DATA                    = $800007D2;
  PDH_CSTATUS_ITEM_NOT_VALIDATED   = $800007D3;
  PDH_RETRY                        = $800007D4;
  PDH_NO_DATA                      = $800007D5;
  PDH_CALC_NEGATIVE_DENOMINATOR    = $800007D6;
  PDH_CALC_NEGATIVE_TIMEBASE       = $800007D7;
  PDH_CALC_NEGATIVE_VALUE          = $800007D8;
  PDH_DIALOG_CANCELLED             = $800007D9;
  PDH_END_OF_LOG_FILE              = $800007DA;
  PDH_CSTATUS_NO_OBJECT            = $C0000BB8;
  PDH_CSTATUS_NO_COUNTER           = $C0000BB9;
  PDH_CSTATUS_INVALID_DATA         = $C0000BBA;
  PDH_MEMORY_ALLOCATION_FAILURE    = $C0000BBB;
  PDH_INVALID_HANDLE               = $C0000BBC;
  PDH_INVALID_ARGUMENT             = $C0000BBD;
  PDH_FUNCTION_NOT_FOUND           = $C0000BBE;
  PDH_CSTATUS_NO_COUNTERNAME       = $C0000BBF;
  PDH_CSTATUS_BAD_COUNTERNAME      = $C0000BC0;
  PDH_INVALID_BUFFER               = $C0000BC1;
  PDH_INSUFFICIENT_BUFFER          = $C0000BC2;
  PDH_CANNOT_CONNECT_MACHINE       = $C0000BC3;
  PDH_INVALID_PATH                 = $C0000BC4;
  PDH_INVALID_INSTANCE             = $C0000BC5;
  PDH_INVALID_DATA                 = $C0000BC6;
  PDH_NO_DIALOG_DATA               = $C0000BC7;
  PDH_CANNOT_READ_NAME_STRINGS     = $C0000BC8;
  PDH_LOG_FILE_CREATE_ERROR        = $C0000BC9;
  PDH_LOG_FILE_OPEN_ERROR          = $C0000BCA;
  PDH_LOG_TYPE_NOT_FOUND           = $C0000BCB;
  PDH_NO_MORE_DATA                 = $C0000BCC;
  PDH_ENTRY_NOT_IN_LOG_FILE        = $C0000BCD;
  PDH_DATA_SOURCE_IS_LOG_FILE      = $C0000BCE;
  PDH_DATA_SOURCE_IS_REAL_TIME     = $C0000BCF;
  PDH_UNABLE_READ_LOG_HEADER       = $C0000BD0;
  PDH_FILE_NOT_FOUND               = $C0000BD1;
  PDH_FILE_ALREADY_EXISTS          = $C0000BD2;
  PDH_NOT_IMPLEMENTED              = $C0000BD3;
  PDH_STRING_NOT_FOUND             = $C0000BD4;
  PDH_UNABLE_MAP_NAME_FILES        = $80000BD5;
  PDH_UNKNOWN_LOG_FORMAT           = $C0000BD6;
  PDH_UNKNOWN_LOGSVC_COMMAND       = $C0000BD7;
  PDH_LOGSVC_QUERY_NOT_FOUND       = $C0000BD8;
  PDH_LOGSVC_NOT_OPENED            = $C0000BD9;

type
// data type definitions
  PDH_STATUS = LongInt;
  HCOUNTER = THandle;
  HQUERY = THandle;
  HLOG = THandle;

  TPDH_RAW_COUNTER = record
    CStatus : DWORD;
    TimeStamp : FILETIME;
    FirstValue : LONGLONG;
    SecondValue : LONGLONG;
    MultiCount : DWORD
  end;
  PPDH_RAW_COUNTER = ^TPDH_RAW_COUNTER;

  TPDH_RAW_COUNTER_ITEM_A = record
    szName : PAnsiChar;
    RawValue : TPDH_RAW_COUNTER
  end;
  PPDH_RAW_COUNTER_ITEM_A = ^TPDH_RAW_COUNTER_ITEM_A;
  TPDH_RAW_COUNTER_ITEM = TPDH_RAW_COUNTER_ITEM_A;
  PPDH_RAW_COUNTER_ITEM = ^TPDH_RAW_COUNTER_ITEM;

  TPDH_RAW_COUNTER_ITEM_W = record
    szName : PWideChar;
    RawValue : TPDH_RAW_COUNTER
  end;
  PPDH_RAW_COUNTER_ITEM_W = ^TPDH_RAW_COUNTER_ITEM_W;

  TPDH_FMT_COUNTERVALUE = record
  CStatus : DWORD;
  dummy : DWORD;
  case DWORD of
    0 : (longValue : LongInt);
    1 : (doubleValue : double);
    2 : (largeValue : LONGLONG);
    3 : (AnsiStringValue : PAnsiChar);
    4 : (WideStringValue : PWideChar)
  end;
  PPDH_FMT_COUNTERVALUE = ^TPDH_FMT_COUNTERVALUE;

  TPDH_FMT_COUNTERVALUE_ITEM_A = record
    szName : PAnsiChar;
    FmtValue : TPDH_FMT_COUNTERVALUE
  end;
  PPDH_FMT_COUNTERVALUE_ITEM_A = ^TPDH_FMT_COUNTERVALUE_ITEM_A;
  TPDH_FMT_COUNTERVALUE_ITEM = TPDH_FMT_COUNTERVALUE_ITEM_A;
  PPDH_FMT_COUNTERVALUE_ITEM = ^TPDH_FMT_COUNTERVALUE_ITEM;

  TPDH_FMT_COUNTERVALUE_ITEM_W = record
    szName : PWideChar;
    FmtValue : TPDH_FMT_COUNTERVALUE
  end;
  PPDH_FMT_COUNTERVALUE_ITEM_W = ^TPDH_FMT_COUNTERVALUE_ITEM_W;

  TPDH_STATISTICS  = record
    dwFormat : DWORD;
    count : DWORD;
    min : TPDH_FMT_COUNTERVALUE;
    max : TPDH_FMT_COUNTERVALUE;
    mean : TPDH_FMT_COUNTERVALUE
  end;
  PPDH_STATISTICS = ^TPDH_STATISTICS;

  TPDH_COUNTER_PATH_ELEMENTS_A = record
    szMachineName : PAnsiChar;
    szObjectName : PAnsiChar;
    szInstanceName : PAnsiChar;
    szParentInstance : PAnsiChar;
    dwInstanceIndex : DWORD;
    szCounterName : PAnsiChar
  end;
  PPDH_COUNTER_PATH_ELEMENTS_A = ^TPDH_COUNTER_PATH_ELEMENTS_A;
  TPDH_COUNTER_PATH_ELEMENTS = TPDH_COUNTER_PATH_ELEMENTS_A;
  PPDH_COUNTER_PATH_ELEMENTS = ^TPDH_COUNTER_PATH_ELEMENTS;

  TPDH_COUNTER_PATH_ELEMENTS_W = record
    szMachineName : PWideChar;
    szObjectName : PWideChar;
    szInstanceName : PWideChar;
    szParentInstance : PWideChar;
    dwInstanceIndex : DWORD;
    szCounterName : PWideChar
  end;
  PPDH_COUNTER_PATH_ELEMENTS_W = ^TPDH_COUNTER_PATH_ELEMENTS_W;


  TPDH_DATA_ITEM_PATH_ELEMENTS_A = record
    szMachineName : PAnsiChar;
    ObjectGUID : TGUID;
    dwItemId : DWORD;
    szInstanceName : PAnsiChar
  end;
  PPDH_DATA_ITEM_PATH_ELEMENTS_A = ^TPDH_DATA_ITEM_PATH_ELEMENTS_A;
  TPDH_DATA_ITEM_PATH_ELEMENTS = TPDH_DATA_ITEM_PATH_ELEMENTS_A;
  PPDH_DATA_ITEM_PATH_ELEMENTS = ^TPDH_DATA_ITEM_PATH_ELEMENTS;

  TPDH_DATA_ITEM_PATH_ELEMENTS_W = record
    szMachineName : PWideChar;
    ObjectGUID : TGUID;
    dwItemId : DWORD;
    szInstanceName : PWideChar
  end;
  PPDH_DATA_ITEM_PATH_ELEMENTS_W = ^TPDH_DATA_ITEM_PATH_ELEMENTS_W;

  TPathDetailsA = record
    case byte of
      0 : (
            DataItemPath : TPDH_DATA_ITEM_PATH_ELEMENTS_A;
          );
      1 : (
            CounterPath : TPDH_COUNTER_PATH_ELEMENTS_A;
          );

      2 : (
            szMachineName : PAnsiChar;
            szObjectName : PAnsiChar;
            szInstanceName : PAnsiChar;
            szParentInstance : PAnsiChar;
            dwInstanceIndex : DWORD;
            szCounterName : PAnsiChar;
          );
  end;

  TPathDetailsW = record
    case byte of
      0 : (
            DataItemPath : TPDH_DATA_ITEM_PATH_ELEMENTS_W;
          );
      1 : (
            CounterPath : TPDH_COUNTER_PATH_ELEMENTS_W;
          );

      2 : (
            szMachineName : PWideChar;
            szObjectName : PWideChar;
            szInstanceName : PWideChar;
            szParentInstance : PWideChar;
            dwInstanceIndex : DWORD;
            szCounterName : PWideChar;
          );
  end;

  TPDH_COUNTER_INFO_A = record
    dwLength : DWORD;
    dwType : DWORD;
    CVersion : DWORD;
    CStatus : DWORD;
    lScale : LongInt;
    lDefaultScale : LongInt;
    dwUserData : DWORD;
    dwQueryUserData : DWORD;
    szFullPath : PAnsiChar;
    pathDetails : TPathDetailsA;
    szExplainText : PAnsiChar;
    DataBuffer : array [0..0] of DWORD
  end;
  PPDH_COUNTER_INFO_A = ^TPDH_COUNTER_INFO_A;
  TPDH_COUNTER_INFO = TPDH_COUNTER_INFO_A;
  PPDH_COUNTER_INFO = ^TPDH_COUNTER_INFO;

  TPDH_COUNTER_INFO_W = record
    dwLength : DWORD;
    dwType : DWORD;
    CVersion : DWORD;
    CStatus : DWORD;
    lScale : LongInt;
    lDefaultScale : LongInt;
    dwUserData : DWORD;
    dwQueryUserData : DWORD;
    szFullPath : PWideChar;
    pathDetails : TPathDetailsW;
    szExplainText : PWideChar;
    DataBuffer : array [0..0] of DWORD
  end;
  PPDH_COUNTER_INFO_W = ^TPDH_COUNTER_INFO_W;

  TPDH_TIME_INFO = record
    StartTime : LONGLONG;
    EndTime : LONGLONG;
    SampleCount : DWORD
  end;
  PPDH_TIME_INFO = ^TPDH_TIME_INFO;

  TPDH_RAW_LOG_RECORD = record
    dwStructureSize : DWORD;
    dwRecordType : DWORD;
    dwItems : DWORD;
    RawBytes : array [0..0] of byte;
  end;
  PPDH_RAW_LOG_RECORD = ^TPDH_RAW_LOG_RECORD;

  TPDH_LOG_SERVICE_QUERY_INFO_A = record
    dwSize : DWORD;
    dwFlags : DWORD;
    dwLogQuota : DWORD;
    szLogFileCaption : PAnsiChar;
    szDefaultDir : PAnsiChar;
    szBaseFileName : PAnsiChar;
    dwFileType : DWORD;
    dwReserved : DWORD;
    case byte of
      0 : (
            PdlAutoNameInterval : DWORD;
            PdlAutoNameUnits : DWORD;
            PdlCommandFilename : PAnsiChar;
            PdlCounterList : PAnsiChar;
            PdlAutoNameFormat : DWORD;
            PdlSampleInterval : DWORD;
            PdlLogStartTime : FILETIME;
            PdlLogEndTime : FILETIME;
          );
      1 : (
            TlNumberOfBuffers : DWORD;
            TlMinimumBuffers : DWORD;
            TlMaximumBuffers : DWORD;
            TlFreeBuffers : DWORD;
            TlBufferSize : DWORD;
            TlEventsLost : DWORD;
            TlLoggerThreadId : DWORD;
            TlBuffersWritten : DWORD;
            TlLogHandle : DWORD;
            TlLogFileName : PAnsiChar;
          );
  end;
  PPDH_LOG_SERVICE_QUERY_INFO_A = ^TPDH_LOG_SERVICE_QUERY_INFO_A;
  TPDH_LOG_SERVICE_QUERY_INFO = TPDH_LOG_SERVICE_QUERY_INFO_A;
  PPDH_LOG_SERVICE_QUERY_INFO = ^TPDH_LOG_SERVICE_QUERY_INFO;

  TPDH_LOG_SERVICE_QUERY_INFO_W = record
    dwSize : DWORD;
    dwFlags : DWORD;
    dwLogQuota : DWORD;
    szLogFileCaption : PWideChar;
    szDefaultDir : PWideChar;
    szBaseFileName : PWideChar;
    dwFileType : DWORD;
    dwReserved : DWORD;
    case byte of
      0 : (
            PdlAutoNameInterval : DWORD;
            PdlAutoNameUnits : DWORD;
            PdlCommandFilename : PWideChar;
            PdlCounterList : PWideChar;
            PdlAutoNameFormat : DWORD;
            PdlSampleInterval : DWORD;
            PdlLogStartTime : FILETIME;
            PdlLogEndTime : FILETIME;
          );
      1 : (
            TlNumberOfBuffers : DWORD;
            TlMinimumBuffers : DWORD;
            TlMaximumBuffers : DWORD;
            TlFreeBuffers : DWORD;
            TlBufferSize : DWORD;
            TlEventsLost : DWORD;
            TlLoggerThreadId : DWORD;
            TlBuffersWritten : DWORD;
            TlLogHandle : DWORD;
            TlLogFileName : PWideChar;
          );
  end;
  PPDH_LOG_SERVICE_QUERY_INFO_W = ^TPDH_LOG_SERVICE_QUERY_INFO_W;

  CounterPathCallBack = function (dw : DWORD) : PDH_STATUS; stdcall;

  TBrowseDlgConfigFlag = (
    bIncludeInstanceIndex,
    bSingleCounterPerAdd,
    bSingleCounterPerDialog,
    bLocalCountersOnly,
    bWildCardInstances,
    bHideDetailBox,
    bInitializePath,
    bDisableMachineSelection,
    bIncludeCostlyObjects);

  TBrowseDlgConfigFlags = set of TBrowseDlgConfigFlag;

  TPDH_BrowseDlgConfig_W = record
    flags : TBrowseDlgConfigFlags;
    hWndOwner : HWND;
    szDataSource : PWideChar;
    szReturnPathBuffer : PWideChar;
    cchReturnPathLength : DWORD;
    pCallBack : CounterPathCallBack;
    dwCallBackArg : DWORD;
    CallBackStatus : PDH_STATUS;
    dwDefaultDetailLevel : DWORD;
    szDialogBoxCaption : PWideChar;
  end;
  PPDH_BROWSE_DLG_CONFIG_W = ^TPDH_BrowseDlgConfig_W;

  TPDH_BrowseDlgConfig_A = record
    flags : TBrowseDlgConfigFlags;
    hWndOwner : HWND;
    szDataSource : PAnsiChar;
    szReturnPathBuffer : PAnsiChar;
    cchReturnPathLength : DWORD;
    pCallBack : CounterPathCallBack;
    dwCallBackArg : DWORD;
    CallBackStatus : PDH_STATUS;
    dwDefaultDetailLevel : DWORD;
    szDialogBoxCaption : PAnsiChar;
  end;
  PPDH_BROWSE_DLG_CONFIG_A = ^TPDH_BrowseDlgConfig_A;
  TPDH_BrowseDlgConfig = TPDH_BrowseDlgConfig_A;
  PPDH_BrowseDlgConfig = ^TPDH_BrowseDlgConfig;

// function definitions

var

PdhGetDllVersion: Function (
    var lpdwVersion : DWORD
) : PDH_STATUS; stdcall;

//
//  Query Functions
//

PdhOpenQuery: Function (
    szDataSource : PAnsiChar;
    dwUserData : DWORD;
    var phQuery : HQUERY
) : PDH_STATUS; stdcall;


PdhAddCounter: Function (
    hQuery : HQUERY;
    szFullCounterPath : PAnsiChar;
    dwUserData : DWORD;
    var phCounter : HCOUNTER
) : PDH_STATUS; stdcall;

PdhRemoveCounter: Function (
    hCounter : HCOUNTER
) : PDH_STATUS; stdcall;

PdhCollectQueryData: Function (
    hQuery: HQUERY
) : PDH_STATUS; stdcall;

PdhCloseQuery: Function (
    hQuery : HQUERY
) : PDH_STATUS; stdcall;

//
//  Counter Functions
//

PdhGetFormattedCounterValue: Function (
    hCounter : HCOUNTER;
    dwFormat : DWORD;
    lpdwType : PWORD;
    var pValue : TPDH_FMT_COUNTERVALUE
) : PDH_STATUS; stdcall;

PdhGetFormattedCounterArray: Function (
    hCounter : HCOUNTER;
    dwFormat : DWORD;
    var lpdwBufferSize : DWORD;
    var lpdwItemCount : DWORD;
    ItemBuffer : PPDH_FMT_COUNTERVALUE_ITEM
) : PDH_STATUS; stdcall;

PdhGetRawCounterValue: Function (
    hCounter : HCOUNTER;
    var lpdwType : DWORD;
    var pValue : TPDH_RAW_COUNTER
) : PDH_STATUS; stdcall;

PdhGetRawCounterArray: Function (
    hCounter : HCOUNTER;
    var lpdwBufferSize : DWORD;
    var lpdwItemCount : DWORD;
    ItemBuffer : PPDH_RAW_COUNTER_ITEM
) : PDH_STATUS; stdcall;

PdhCalculateCounterFromRawValue: Function (
    hCounter : HCOUNTER;
    dwFormat : DWORD;
    rawValue1 : PPDH_RAW_COUNTER;
    rawValue2 : PPDH_RAW_COUNTER;
    fmtValue : PPDH_FMT_COUNTERVALUE
) : PDH_STATUS; stdcall;

PdhComputeCounterStatistics: Function (
    hCounter : HCounter;
    dwFormat : DWORD;
    dwFirstEntry : DWORD;
    dwNumEntries : DWORD;
    lpRawValueArray : PPDH_RAW_COUNTER;
    data : PPDH_STATISTICS
) : PDH_STATUS; stdcall;

PdhGetCounterInfo: Function (
    hCounter : HCOUNTER;
    bRetrieveExplainText : BOOL;
    var pdwBufferSize : DWORD;
    lpBuffer : PPDH_COUNTER_INFO
) : PDH_STATUS; stdcall;

PdhSetCounterScaleFactor: Function (
    hCounter : HCOUNTER;
    lFactor : longint
) : PDH_STATUS; stdcall;
//
//   Browsing and enumeration functions
//
PdhConnectMachine: Function (
    szMachineName : PAnsiChar
) : PDH_STATUS; stdcall;

PdhEnumMachines: Function (
    szDataSource : PAnsiChar;
    mszMachineList : PAnsiChar;
    var pcchBufferSize : DWORD
) : PDH_STATUS; stdcall;

PdhEnumObjects: Function (
    szDataSource : PAnsiChar;
    szMachineName : PAnsiChar;
    mszObjectList : PAnsiChar;
    var pcchBufferSize : DWORD;
    dwDetailLevel : DWORD;
    bRefresh : BOOL
) : PDH_STATUS; stdcall;

PdhEnumObjectItems: Function (
    szDataSource : PAnsiChar;
    szMachineName : PAnsiChar;
    szObjectName : PAnsiChar;
    mszCounterList : PAnsiChar;
    var pcchCounterListLength : DWORD;
    mszInstanceList : PAnsiChar;
    var pcchInstanceListLength : DWORD;
    dwDetailLevel : DWORD;
    dwFlags : DWORD
) : PDH_STATUS; stdcall;

PdhMakeCounterPath: Function (
    var pCounterPathElements : PPDH_COUNTER_PATH_ELEMENTS;
    szFullPathBuffer : PAnsiChar;
    var pcchBufferSize : DWORD;
    dwFlags : DWORD
) : PDH_STATUS; stdcall;

PdhParseCounterPath: Function (
    szFullPathBuffer : PAnsiChar;
    var pCounterPathElements : PPDH_COUNTER_PATH_ELEMENTS;
    var pdwBufferSize : DWORD;
    dwFlags : DWORD
) : PDH_STATUS; stdcall;

PdhParseInstanceName: Function (
    szInstanceString : PAnsiChar;
    szInstanceName : PAnsiChar;
    var pcchInstanceNameLength : DWORD;
    szParentName : PAnsiChar;
    var pcchParentNameLength : DWORD;
    var lpIndex : DWORD
) : PDH_STATUS; stdcall;

PdhValidatePath: Function (
    szFullPathBuffer : PAnsiChar
) : PDH_STATUS; stdcall;

PdhGetDefaultPerfObject: Function (
    szDataSource : PAnsiChar;
    szMachineName : PAnsiChar;
    szDefaultObjectName : PAnsiChar;
    var pcchBufferSize : DWORD
) : PDH_STATUS; stdcall;

PdhGetDefaultPerfCounter: Function (
    szDataSource : PAnsiChar;
    szMachineName : PAnsiChar;
    szObjectName : PAnsiChar;
    szDefaultCounterName : PAnsiChar;
    var pcchBufferSize : DWORD
) : PDH_STATUS; stdcall;

PdhBrowseCounters: Function (
  const data : TPDH_BrowseDlgConfig
) : PDH_STATUS; stdcall;

PdhExpandCounterPath: Function (
    szWildCardPath : PAnsiChar;
    mszExpandedPathList : PAnsiChar;
    var pcchPathListLength : DWORD
) : PDH_STATUS; stdcall;

//
//  v2.0 functions
//
PdhLookupPerfNameByIndex: Function (
    szMachineName : PAnsiChar;
    dwNameIndex : DWORD;
    szNameBuffer : PAnsiChar;
    var pcchNameBufferSize : DWORD
) : PDH_STATUS; stdcall;


PdhLookupPerfIndexByName: Function (
    szMachineName : PAnsiChar;
    szNameBuffer : PAnsiChar;
    var pdwIndex : DWORD
) : PDH_STATUS; stdcall;

//
//   Logging Functions
//

PdhOpenLog: Function (
    szLogFileName : PAnsiChar;
    dwAccessFlags : DWORD;
    var lpdwLogType : DWORD;
    hQuery : HQUERY;
    dwMaxRecords : DWORD;
    szUserCaption : PAnsiChar;
    var phLog : HLOG
) : PDH_STATUS; stdcall;

PdhUpdateLog: Function (
    hLog : HLOG;
    szUserString : PAnsiChar
) : PDH_STATUS; stdcall;

PdhGetLogFileSize: Function (
    hLog : HLOG;
    var llSize : LONGLONG
) : PDH_STATUS; stdcall;

PdhCloseLog: Function (
    hLog : HLOG;
    dwFlags : DWORD
) : PDH_STATUS; stdcall;

PdhSelectDataSource: Function (
    hWndOwner : HWND;
    dwFlags : DWORD;
    szDataSource : PAnsiChar;
    var pcchBufferLength : DWORD
) : PDH_STATUS; stdcall;

PdhIsRealTimeQuery: Function (
    hQuery : HQUERY
) : BOOL; stdcall;

PdhSetQueryTimeRange: Function (
    hQuery : HQUERY;
    pInfo : PPDH_TIME_INFO
) : PDH_STATUS; stdcall;

PdhGetDataSourceTimeRange: Function (
    szDataSource : PAnsiChar;
    var pdwNumEntries : DWORD;
    pInfo : PPDH_TIME_INFO;
    var pdwBufferSize : DWORD
) : PDH_STATUS; stdcall;

PdhCollectQueryDataEx: Function (
    hQuery : HQUERY;
    dwIntervalTime : DWORD;
    hNewDataEvent : THandle
) : PDH_STATUS; stdcall;

PdhFormatFromRawValue: Function (
    dwCounterType : DWORD;
    dwFormat : DWORD;
    var pTimeBase : LONGLONG;
    pRawValue1 : PPDH_RAW_COUNTER;
    pRawValue2 : PPDH_RAW_COUNTER;
    pFmtValue : PPDH_FMT_COUNTERVALUE
) : PDH_STATUS; stdcall;

PdhGetCounterTimeBase: Function (
    hCounter : HCOUNTER;
    var pTimeBase : LONGLONG
) : PDH_STATUS; stdcall;

PdhEncodeWmiPath: Function (
    szFullPathBufer : PAnsiChar;
    pDataItemPathElements : PPDH_DATA_ITEM_PATH_ELEMENTS;
    var pdwBuferSize : DWORD;
    langID : LANGID;
    dwFlags : DWORD
) : PDH_STATUS; stdcall;

PdhDecodeWmiPath: Function (
    pDataItemPathElements : PPDH_DATA_ITEM_PATH_ELEMENTS;
    pCounterPathElements : PPDH_COUNTER_PATH_ELEMENTS;
    var pdwBufferSize : DWORD;
    longID : LANGID;
    dwFlags : DWORD
) : PDH_STATUS; stdcall;


PdhReadRawLogRecord: Function (
    hLog : HLOG;
    ftRecord : FILETIME;
    pRawLogRecord : PPDH_RAW_LOG_RECORD;
    var pdwBufferLength : DWORD
) : PDH_STATUS; stdcall;

PdhLogServiceCommand: Function (
    szMachineName : PAnsiChar;
    szQueryName : PAnsiChar;
    dwFlags : DWORD;
    var pdwStatus : DWORD
) : PDH_STATUS; stdcall;

PdhLogServiceControl: Function (
    szMachineName : PAnsiChar;
    szQueryName : PAnsiChar;
    dwFlags : DWORD;
    pInfoBuffer : PPDH_LOG_SERVICE_QUERY_INFO;
    var pdwBufferSize : DWORD
) : PDH_STATUS; stdcall;

{function IsSuccessSeverity (ErrorCode : DWORD) : boolean;
function IsInformationalSeverity (ErrorCode : DWORD) : boolean;
function IsWarningSeverity (ErrorCode : DWORD) : boolean;
function IsErrorSeverity (ErrorCode : DWORD) : boolean; }

const pdhapi = 'pdh.dll';

var
  MagPdhLib: THandle = 0 ;
  MagPdhAPI_Loaded: Boolean = false ;   // See if DLL functions are loaded

function MagLoadPdhApi: boolean ;

implementation

// Try and load various PDH DLL functions. Returns false if failed

function MagLoadPdhApi: boolean ;
begin
    result := Assigned (PdhOpenQuery) ;
    if MagPdhAPI_Loaded then exit ;
    result := false ;
    if NOT MagLoadRasApi then exit ;
    if MagRasOSVersion <> OSNT4 then exit ;

// open libraries - only come here once
    result := false ;
    MagPdhAPI_Loaded := True ;
    MagPdhLib := LoadLibrary (pdhapi) ;
    If MagPdhLib = 0 then exit ;

// WARNING - currently only loading functions used by MagRasPer !!!!!    

// set function addresses to ASCII versions in DLL
    PdhOpenQuery := GetProcAddress (MagPdhLib, 'PdhOpenQuery') ;
    PdhAddCounter := GetProcAddress (MagPdhLib, 'PdhAddCounterA') ;
    PdhEnumObjects := GetProcAddress (MagPdhLib, 'PdhEnumObjectsA') ;
    PdhEnumObjectItems := GetProcAddress (MagPdhLib, 'PdhEnumObjectItemsA') ;
    PdhGetCounterInfo := GetProcAddress (MagPdhLib, 'PdhGetCounterInfoA') ;
    PdhGetRawCounterValue := GetProcAddress (MagPdhLib, 'PdhGetRawCounterValue') ;
    PdhGetRawCounterArray := GetProcAddress (MagPdhLib, 'PdhGetRawCounterArrayA') ;
    PdhCollectQueryData := GetProcAddress (MagPdhLib, 'PdhCollectQueryData') ;
    PdhCloseQuery := GetProcAddress (MagPdhLib, 'PdhCloseQuery') ;
    result := Assigned (PdhOpenQuery) ;
end ;

Initialization
    MagPdhLib := 0 ;
    MagPdhAPI_Loaded := false ;
finalization
    if MagPdhAPI_Loaded then FreeLibrary (MagPdhLib) ;
end.
