{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2020 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnHardWareInfo;
{* |<PRE>
================================================================================
* ������ƣ�CnPack �����
* ��Ԫ���ƣ�Ӳ����Ϣ��Ԫ
* ��Ԫ���ߣ�SkyJacker
*           LiuXiao
*           Yock
*           Bahamut
* ��    ע��Ӳ����Ϣ��Ԫ��Ŀǰֻʵ�ֻ�ȡ��ˡ���CPUϵͳ��ָ��CPU�����к���ռ����
*           �Լ����� BIOS �� ID������Ӳ�̵��������к�.
* ����ƽ̨��WindowsXP sp2 + Delphi 6.0 up2
* ���ݲ��ԣ�Win2000/XP + Delphi 5��6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2014.12.04 V1.5
*               �����ȡ����������Լ����кŵĹ��ܣ���δ֧�� Win9X
*               �����ȡ�߼����������Լ����Ĺ���
*           2012.05.10 V1.4
*               ���� 64 λ�µı�������
*           2008.08.01 V1.3
*               ���� Bahamut �Ļ�ȡ BIOS ID �Ĺ��̣���ֻ֧��С���� BIOS
*           2008.04.12 V1.2
*               LiuXiao ����� CPU �����������Ķ�ȡ���Ƿ�֧�� cpuid ָ�������к�
*               �����ԣ���л Yock��
*           2008.01.12 V1.1
*               LiuXiao ����� CPU ռ���ʵĶ�ȡ
*           2007.01.23 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, Windows, SysUtils, ExtCtrls, CnNativeDecl, CnCommon;

const
  RelationCache = 2;
  RelationNumaNode = 1;
  RelationProcessorCore = 0;
  RelationProcessorPackage = 3;

type
  TCacheType = (CacheUnified, CacheInstruction, CacheData, CacheTrace);

  TCacheDescriptor = packed record
    Level: Byte;
    Associativity: Byte;
    LineSize: Word;
    Size: DWORD;
    CacheType: TCacheType;
  end;

  TSystemLogicalProcessorInformation = packed record
    ProcessorMask: TCnNativeUInt;
    Relationship: DWORD;
    case Integer of
      0: (ProcessorCoreFlag: Byte);
      1: (NumaModeNumber: DWORD);
      2: (Cache: TCacheDescriptor);
      3: (Reserved: array[0..1] of Int64);
  end;
  PSystemLogicalProcessorInformation = ^TSystemLogicalProcessorInformation;

  TGetLogicalProcessorInformation = function(Buffer: PSystemLogicalProcessorInformation;
    out ReturnLength: DWORD): BOOL; stdcall;

  TCnCPUIdFormat = (ifContinuous, ifDashed);
  {* CPU���к�����Ϣ����ʾ��ʽ
   |<PRE>
     ifContinuous:  -������
     ifDashed:      -ʹ�÷ָ��'-'�ָ�
   |</PRE>
  }

  TCnCpuId = class(TPersistent)
  {CPU ��Ϣ��}
  private
    FTimer: TTimer;
    FCPUCount: Integer;
    FCPUIds: TStrings;
    FCPUInfos: TStrings;
    FSupportCpuIds: TList;
    FSupportCpuSns: TList;
    FCPUOems: TStrings;
    FCPUIdFormat: TCnCPUIdFormat;
    FCPUUsageRead: Boolean;
    FCPUUsage: array[0..255] of Integer; // �ܲ��ᳬ�� 256 �� CPU �ɣ�
    FCurCnt, FLastCnt: array[0..255] of Integer;
    FAverageCPUUsage: Integer;
    FProcessorCoreCount: Integer;
    FL1CacheCount: Integer;
    FNumaNodeCount: Integer;
    FL3CacheCount: Integer;
    FProcessorPackageCount: Integer;
    FLogicalProcessorCount: Integer;
    FL2CacheCount: Integer;
    function GetFirstCPUId: string;
    function GetCPUId(Index: Integer): string;
    procedure SetCPUIdFormat(ACPUIdFormat: TCnCPUIdFormat);
    function GetAverageCPUUsage: Integer;
    function GetCPUUsage(Index: Integer): Integer;
    function GetFirstCPUUsage: Integer;

    function RefreshCPUUsages: Cardinal; // ֻ����ʱ����
    procedure CpuUsageTimer(Sender: TObject);
    function GetCPUOem(Index: Integer): string;
    function GetFirstCPUOem: string;
    function GetSupportCPUId(Index: Integer): Boolean;
    function GetSupportCPUSn(Index: Integer): Boolean;
    function GetFirstSupportCPUId: Boolean;
    function GetFirstSupportCPUSn: Boolean;
    function GetCPUInfoString(Index: Integer): string;
    function GetFirstCPUInfoString: string;

    function GetCnCPUOem: AnsiString;
    function GetCnCPUInfoString: AnsiString;
    function GetCnCPUID: AnsiString;

    procedure ClearLocalCPUInfo;
  public
    constructor Create;
    {* ���캯�������� FCPUIds ������ ReadCPUId}
    destructor Destroy; override;

    procedure ReadCPUInfo;
    {* ������� CPU �ں˵����кź�������Ϣ������������б�}

    procedure ReadLogicalCPUInfo;
    {* ͨ�� GetLogicalProcessorInformation API ��ȡ������߼� CPU ��Ϣ}

    property CPUIdFormat: TCnCPUIdFormat read FCPUIdFormat write SetCPUIdFormat;
    {* CPU ���к���ʾ��ʽ}
    property CPUCount: Integer read FCPUCount;
    {* ϵͳ�е��߼� CPU ����}
    property FirstCPUId: string read GetFirstCPUId;
    {* ��ȡ�׸� CPU �� ID�����ڵ� CPU ϵͳ}
    property FirstCPUInfoString: string read GetFirstCPUInfoString;
    {* ��ȡ�׸� CPU ����Ϣ�ַ��������ڵ� CPU ϵͳ}
    property FirstCPUOem: string read GetFirstCPUOem;
    {* ��ȡ�׸� CPU ���������̣����ڵ� CPU ϵͳ}
    property FirstSupportCPUId: Boolean read GetFirstSupportCPUId;
    {* ��ȡ�׸� CPU �Ƿ�֧�� CPUID ָ����ڵ� CPU ϵͳ}
    property FirstSupportCPUSn: Boolean read GetFirstSupportCPUSn;
    {* ��ȡ�׸� CPU �Ƿ�֧�ֶ�ȡ CPU ���кţ����ڵ� CPU ϵͳ}
    property SupportCPUId[Index: Integer]: Boolean read GetSupportCPUId;
    {* ��ȡָ�� CPU �Ƿ�֧�� CPUID ָ��}
    property SupportCPUSn[Index: Integer]: Boolean read GetSupportCPUSn;
    {* ��ȡָ�� CPU �Ƿ�֧�ֶ�ȡ CPU ���к�}
    property CPUId[Index: Integer]: string read GetCPUId;
    {* ���ָ�� CPU �����кš����� Index �� 0 ��ʼ��
       ��Ҫ˵�����ǣ����кźܶ� CPU ����ֹ��ȡ����˴�����ȫ 0����������������
       CPU ���кŶ�ȡ���ܶ�����¶���������ġ���Ϣ�ַ�����������}
    property CPUInfoString[Index: Integer]: string read GetCPUInfoString;
    {* ���ָ�� CPU ����Ϣ�ַ��������� Index �� 0 ��ʼ��
       ����Ϣ�ַ��������� CPU ��һЩ����˵������Ψһ���ܶ�����±����� CPU ID��}
    property CPUOem[Index: Integer]: string read GetCPUOem;
     {* ���ָ�� CPU ���������̡����� Index �� 0 ��ʼ}
    property CPUUsage[Index: Integer]: Integer read GetCPUUsage;
    {* ���ָ�� CPU ��ռ���ʣ�0 �� 100
       ��Ҫ˵�����ǣ������� NT ϵͳ�ϲ��ö�ʱ������� CPU ��æ�������ټ��������
       ����ڸ�ʵ������δ�������ʱ���õ��� CPU ռ���ʿ�����������ͬ��
    }
    property AverageCPUUsage: Integer read GetAverageCPUUsage;
    {* ���ƽ�� CPU ռ���ʣ�0 �� 100}
    property FirstCPUUsage: Integer read GetFirstCPUUsage;
    {* ����׸� CPU ��ռ���ʣ�0 �� 100�����ڵ� CPU ϵͳ}

    // ������Ϣʹ�� API GetLogicalProcessorInformation ���
    property ProcessorPackageCount: Integer read FProcessorPackageCount;
    {* ����������װ����}
    property ProcessorCoreCount: Integer read FProcessorCoreCount;
    {* ������������}
    property LogicalProcessorCount: Integer read FLogicalProcessorCount;
    {* �߼��������������� CPUCount ֵ}
    property NumaNodeCount: Integer read FNumaNodeCount;
    {* ��ͳһ�ڴ���ʼܹ��Ľڵ����}
    property L1CacheCount: Integer read FL1CacheCount;
    {* ��һ�����ٻ������}
    property L2CacheCount: Integer read FL2CacheCount;
    {* �ڶ������ٻ������}
    property L3CacheCount: Integer read FL3CacheCount;
    {* ���������ٻ������}
  end;

  TCnHardDiskInfo = class(TPersistent)
  private
    FHardDiskCount: Integer;
    FHardDiskSns: TStrings;
    FVolumnCount: Integer;
    FVolumnLetters: TStrings;
    FVolumnNames: TStrings;

    function GetVolumnLetter(Index: Integer): string;
    function GetVolumnName(Index: Integer): string;
    function GetDiskSerialNo(Index: Integer): string;

    procedure ReadPhysicalDriveInNTWithZeroRights;
    procedure ReadVolumnInfo;
  public
    constructor Create;
    {* ���캯���������б����� ReadHardDisk}
    destructor Destroy; override;

    procedure ReadHardDisk;
    {* ��ȡ����Ӳ���Լ���������Ϣ}
    
    property HardDiskCount: Integer read FHardDiskCount;
    {* ����Ӳ�̸���}
    property DiskSerialNo[Index: Integer]: string read GetDiskSerialNo;
    {* ����Ӳ�̵����к�}

    property VolumnCount: Integer read FVolumnCount;
    {* �߼�����������}
    property VolumnLetter[Index: Integer]: string read GetVolumnLetter;
    {* �߼��������̷�}
    property VolumnName[Index: Integer]: string read GetVolumnName;
    {* �߼����������}
  end;

function CnGetBiosID: string;
{* ��� BIOS �� ID��ֻ֧��С���� BIOS�����Ҿ�ʽ���������ڲ��淶���޷���ȡ ID}

implementation

const
  BiosOffset: array[0..2] of DWORD = ($6577, $7196, $7550);
  SCN_CPUID_BIT = $200000; //CPU ID λ���

type
  TCnCPUIDResult = array[0..3] of Longint;

  PUNICODE_STRING = ^TUNICODE_STRING;
  _UNICODE_STRING = record
    Length: Word;
    MaximumLength: Word;
    Buffer: PWChar;
  end;
  TUNICODE_STRING = _UNICODE_STRING;

  POBJECT_ATTRIBUTES = ^TOBJECT_ATTRIBUTES;
  _OBJECT_ATTRIBUTES = record
    Length: ULONG;
    RootDirectory: THandle;
    ObjectName: PUNICODE_STRING;
    Attributes: ULONG;
    SecurityDescriptor: Pointer;
    SecurityQualityOfService: Pointer;
  end;
  TOBJECT_ATTRIBUTES = _OBJECT_ATTRIBUTES;

  PLARGE_INTEGER = ^LARGE_INTEGER;
  PPByte = ^PByte;

  _SYSTEM_BASIC_INFORMATION = record
    Unknown: ULONG;
    MaximumIncrement: ULONG;
    PhysicalPageSize: ULONG;
    NumberOfPhysicalPages: ULONG;
    LowestPhysicalPage: ULONG;
    HighestPhysicalPage: ULONG;
    AllocationGranularity: ULONG;
    LowestUserAddress: ULONG;
    HighestUserAddress: ULONG;
    ActiveProcessors: ULONG;
    NumberProcessors: UCHAR;
  end;
  SYSTEM_BASIC_INFORMATION = _SYSTEM_BASIC_INFORMATION;
  PSYSTEM_BASIC_INFORMATION = ^SYSTEM_BASIC_INFORMATION;
  TSystemBasicInformation = SYSTEM_BASIC_INFORMATION;
  PSystemBasicInformation = ^TSystemBasicInformation;

  SYSTEM_PROCESSOR_TIMES = packed record
    IdleTime: LARGE_INTEGER;
    KernelTime: LARGE_INTEGER;
    UserTime: LARGE_INTEGER;
    DpcTime: LARGE_INTEGER;
    InterruptTime: LARGE_INTEGER;
    InterruptCount: ULONG;
  end;
  TSystemProcessorTimes = SYSTEM_PROCESSOR_TIMES;
  PSystemProcessorTimes = ^TSystemProcessorTimes;

  SYSTEM_INFORMATION_CLASS = (
          SystemBasicInformation,
          SystemProcessorInformation,
          SystemPerformanceInformation,
          SystemTimeOfDayInformation,
          SystemNotImplemented1,
          SystemProcessesAndThreadsInformation,
          SystemCallCounts,
          SystemConfigurationInformation,
          SystemProcessorTimes,
          SystemGlobalFlag,
          SystemNotImplemented2,
          SystemModuleInformation,
          SystemLockInformation,
          SystemNotImplemented3,
          SystemNotImplemented4,
          SystemNotImplemented5,
          SystemHandleInformation,
          SystemObjectInformation,
          SystemPagefileInformation,
          SystemInstructionEmulationCounts,
          SystemInvalidInfoClass1,
          SystemCacheInformation,
          SystemPoolTagInformation,
          SystemProcessorStatistics,
          SystemDpcInformation,
          SystemNotImplemented6,
          SystemLoadImage,
          SystemUnloadImage,
          SystemTimeAdjustment,
          SystemNotImplemented7,
          SystemNotImplemented8,
          SystemNotImplemented9,
          SystemCrashDumpInformation,
          SystemExceptionInformation,
          SystemCrashDumpStateInformation,
          SystemKernelDebuggerInformation,
          SystemContextSwitchInformation,
          SystemRegistryQuotaInformation,
          SystemLoadAndCallImage,
          SystemPrioritySeparation,
          SystemNotImplemented10,
          SystemNotImplemented11,
          SystemInvalidInfoClass2,
          SystemInvalidInfoClass3,
          SystemTimeZoneInformation,
          SystemLookasideInformation,
          SystemSetTimeSlipEvent,
          SystemCreateSession,
          SystemDeleteSession,
          SystemInvalidInfoClass4,
          SystemRangeStartInformation,
          SystemVerifierInformation,
          SystemAddVerifier,
          SystemSessionProcessesInformation);
  TSystemInformationClass = SYSTEM_INFORMATION_CLASS;

  TNativeQuerySystemInformation = function(SystemInformationClass:
    TSystemInformationClass; SystemInformation: Pointer; SystemInformationLength:
    Cardinal; ReturnLength: PDWORD): Cardinal; stdcall;

  TZwOpenSection = function (var hWnd: THandle; dwMask: DWORD; PObject: POBJECT_ATTRIBUTES): DWORD; stdcall;

  TZwMapViewOfSection = function (hWnd: THandle; ViewHandle: THandle; PBaseAddr: Pointer;
    dwLength: ULONG; dwAllocLen: ULONG; PRealAddr: PLARGE_INTEGER; PReadLen: PDWORD;
    dwInherite: DWORD; dwAllocType: ULONG; dwProtectType: ULONG): DWORD; stdcall;

  TZwUnmapViewOfSection = function (hWnd: THandle; PBaseAddr: Pointer): DWORD; stdcall;

  // Hard Disk begin

  TStorageQueryType = ( PropertyStandardQuery, PropertyExistsQuery,
    PropertyMaskQuery, PropertyQueryMaxDefined);

  TStoragePropertyId = (StorageDeviceProperty, StorageAdapterProperty);

  TStoragePropertyQuery = packed record
    PropertyId: TStoragePropertyId;
    QueryType: TStorageQueryType;
    AdditionalParameters: array[0..9] of Char;
  end;

  TStorageBusType = (
    BusTypeUnknown,
    BusTypeScsi,
    BusTypeAtapi,
    BusTypeAta,
    BusType1394,
    BusTypeSsa,
    BusTypeFibre,
    BusTypeUsb,
    BusTypeRAID,
    BusTypeiScsi,
    BusTypeSas,
    BusTypeSata,
    BusTypeSD,
    BusTypeMmc,
    BusTypeVirtual,
    BusTypeFileBackedVirtual,
    BusTypeSpaces,
    BusTypeMax,
    BusTypeMaxReserved);

  TStorageDeviceDescriptor = packed record
    Version: DWORD;
    Size: DWORD;
    DeviceType: Byte;
    DeviceTypeModifier: Byte;
    RemovableMedia: Boolean;
    CommandQueueing: Boolean;
    VendorIdOffset: DWORD;
    ProductIdOffset: DWORD;
    ProductRevisionOffset: DWORD;
    SerialNumberOffset: DWORD;
    BusType: TStorageBusType;
    RawPropertiesLength: DWORD;
    RawDeviceProperties: array [0..0] of AnsiChar;
  end;
  PStorageDeviceDescriptor = ^TStorageDeviceDescriptor;

  TIDERegs = packed record
    bFeaturesReg: BYTE;
    bSectorCountReg: BYTE;
    bSectorNumberReg: BYTE;
    bCylLowReg: BYTE;
    bCylHighReg: BYTE;
    bDriveHeadReg: BYTE;
    bCommandReg: BYTE;
    bReserved: BYTE;
  end;

  TSendCmdInParams = packed record
    cBufferSize: DWORD;
    irDriveRegs: TIDERegs;
    bDriveNumber: BYTE;
    bReserved: array[0..2] of Byte;
    dwReserved: array[0..3] of DWORD;
    bBuffer: array[0..0] of Byte;
  end;

  TIdSector = packed record
    wGenConfig: Word;
    wNumCyls: Word;
    wReserved: Word;
    wNumHeads: Word;
    wBytesPerTrack: Word;
    wBytesPerSector: Word;
    wSectorsPerTrack: Word;
    wVendorUnique: array[0..2] of Word;
    sSerialNumber: array[0..19] of CHAR;
    wBufferType: Word;
    wBufferSize: Word;
    wECCSize: Word;
    sFirmwareRev: array[0..7] of Char;
    sModelNumber: array[0..39] of Char;
    wMoreVendorUnique: Word;
    wDoubleWordIO: Word;
    wCapabilities: Word;
    wReserved1: Word;
    wPIOTiming: Word;
    wDMATiming: Word;
    wBS: Word;
    wNumCurrentCyls: Word;
    wNumCurrentHeads: Word;
    wNumCurrentSectorsPerTrack: Word;
    ulCurrentSectorCapacity: DWORD;
    wMultSectorStuff: Word;
    ulTotalAddressableSectors: DWORD;
    wSingleWordDMA: Word;
    wMultiWordDMA: Word;
    bReserved: array[0..127] of BYTE;
  end;
  PIdSector = ^TIdSector;

  TDriverStatus = packed record
    bDriverError: Byte;
    bIDEStatus: Byte;
    bReserved: array[0..1] of Byte;
    dwReserved: array[0..1] of DWORD;
  end;

  TSendCmdOutParams = packed record
    cBufferSize: DWORD;
    DriverStatus: TDriverStatus;
    bBuffer: array[0..0] of BYTE;
  end;
  // Hard Disk end.

const
  STATUS_SUCCESS = $00000000;
  IOCTL_STORAGE_QUERY_PROPERTY = $002D1400; // ($2D shl 16) or ($0500 shl 2); //

var
  NtDllHandle: THandle = 0;
  NtDllNeedFree: Boolean = False;

  NtQuerySystemInformation: TNativeQuerySystemInformation = nil;

  ZwOpenSection: TZwOpenSection = nil;

  ZwMapViewOfSection: TZwMapViewOfSection = nil;

  ZwUnmapViewOfSection: TZwUnmapViewOfSection = nil;

  GetLogicalProcessorInformation: TGetLogicalProcessorInformation = nil;

function GetNtNativeAPIs: Boolean;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    NtDllHandle := GetModuleHandle('NTDLL.DLL');
    if NtDllHandle = 0 then
    begin
      NtDllHandle := LoadLibrary('NTDLL.DLL');
      NtDllNeedFree := NtDllHandle <> 0;
    end;

    if NtDllHandle <> 0 then
    begin
//      @NtQueryInformationToken:=GetProcAddress(NtDllHandle,'NtQueryInformationToken');
//      @NtOpenProcessToken := GetProcAddress(NtDllHandle,'NtOpenProcessToken');
//      @NtOpenSection := GetProcAddress(NtDllHandle,'NtOpenSection');
//      @NtClose := GetProcAddress(NtDllHandle,'NtClose');
//      @NtOpenProcess := GetProcAddress(NtDllHandle,'NtOpenProcess');
      @NtQuerySystemInformation := GetProcAddress(NtDllHandle, 'NtQuerySystemInformation');
//      @NtCreateSection := GetProcAddress(NtDllHandle,'NtCreateSection');
//      @NtCreateToken := GetProcAddress(NtDllHandle,'NtCreateToken');
//      @NtMapViewOfSection := GetProcAddress(NtDllHandle,'NtMapViewOfSection');
//      @NtUnmapViewOfSection := GetProcAddress(NtDllHandle,'NtUnmapViewOfSection');
//      @NtOpenFile := GetProcAddress(NtDllHandle,'NtOpenFile');
//      @NtCreateFile := GetProcAddress(NtDllHandle,'NtCreateFile');
//      @NtQueryObject := GetProcAddress(NtDllHandle,'NtQueryObject');
//      @NtQueryInformationProcess := GetProcAddress(NtDllHandle,'NtQueryInformationProcess');
//      @NtQueryInformationThread := GetProcAddress(NtDllHandle,'NtQueryInformationThread');
//      @NtQueryInformationFile := GetProcAddress(NtDllHandle,'NtQueryInformationFile');
//      @NtDuplicateObject := GetProcAddress(NtDllHandle,'NtDuplicateObject');
//      @NtDeviceIoControlFile := GetProcAddress(NtDllHandle,'NtDeviceIoControlFile');

      @ZwOpenSection := GetProcAddress(NtDllHandle, 'ZwOpenSection');
      @ZwMapViewOfSection := GetProcAddress(NtDllHandle, 'ZwMapViewOfSection');
      @ZwUnmapViewOfSection := GetProcAddress(NtDllHandle, 'ZwUnmapViewOfSection');
    end;
  end;

  @GetLogicalProcessorInformation := GetProcAddress(GetModuleHandle('KERNEL32.DLL'), 'GetLogicalProcessorInformation');
  Result := NtDllHandle <> 0;
end;

procedure FreeNtNativeAPIs;
begin
  if (NtDllHandle <> 0) and NtDllNeedFree then
  begin
    FreeLibrary(NtDllHandle);
    NtDllHandle := 0;
  end;
end;

{$IFDEF WIN64}

function GetCPUInfo64: TCnCPUIDResult; assembler; register;
asm
        PUSH    RBX
        PUSH    RDI
        MOV     RDI, RCX
        MOV     EAX, 1
        CPUID
        MOV     [RDI], EAX;
        MOV     [RDI + 4], EBX;
        MOV     [RDI + 8], ECX;
        MOV     [RDI + 12], EDX;
        POP     RDI
        POP     RBX
end;

function GetCPUID64: TCnCPUIDResult; assembler; register;
asm
        PUSH    RBX
        PUSH    RDI
        MOV     RDI, RCX
        MOV     EAX, 1
        CPUID
        MOV     [RDI], EAX;
        MOV     [RDI + 4], EDX;
        MOV EAX, $3
        CPUID
        MOV     [RDI], EAX;
        MOV     [RDI + 8], ECX;
        MOV     [RDI + 12], EDX;
        POP     RDI
        POP     RBX
end;

function GetCPUOem64: TCnCPUIDResult; assembler; register;
asm
        PUSH    RBX
        PUSH    RDI
        MOV     RDI, RCX
        MOV     EAX, 0
        CPUID
        MOV     [RDI], EAX;
        MOV     [RDI + 4], EBX;
        MOV     [RDI + 8], ECX;
        MOV     [RDI + 12], EDX;
        POP     RDI
        POP     RBX
end;

{$ENDIF}

constructor TCnCpuId.Create;
begin
  FSupportCpuIds := TList.Create;
  FSupportCpuSns := TList.Create;
  FCPUIds := TStringList.Create;
  FCPUInfos := TStringList.Create;
  FCPUOems := TStringList.Create;
  FCPUIdFormat := ifContinuous;

  ReadCPUInfo;
  ReadLogicalCPUInfo;

  FTimer := TTimer.Create(nil);
  FTimer.Interval := 1000;
  FTimer.OnTimer := CpuUsageTimer;
  FTimer.Enabled := True;
  RefreshCPUUsages;

  if Win32Platform = VER_PLATFORM_WIN32_NT then // NT ����Ҫ�����ж�
    FCPUUsageRead := False;
end;

destructor TCnCpuId.Destroy;
begin
  FTimer.Free;
  FCPUOems.Free;
  FCPUInfos.Free;
  FCPUIds.Free;
  FSupportCpuSns.Free;
  FSupportCpuIds.Free;
end;

// ��ȡ CPU ��Ϣ�ַ���
function TCnCpuId.GetCnCPUInfoString: AnsiString;
const
  cnIFContinuous = '%.8x%.8x%.8x%.8x';
  cnIFDashed = '%.8x-%.8x-%.8x-%.8x';
var
  iEax,iEbx,iEcx,iEdx: Integer;
{$IFDEF WIN64}
  Rec64: TCnCPUIDResult;
{$ENDIF}
begin
{$IFDEF WIN64}
  Rec64 := GetCPUInfo64;
  iEax := Rec64[0];
  iEbx := Rec64[1];
  iEcx := Rec64[2];
  iEdx := Rec64[3];
{$ELSE}
  asm
    PUSH EBX
    PUSH ECX
    PUSH EDX
    MOV  EAX, $1
    DW $A20F      //CPUID
    MOV IEAX, EAX
    MOV IEBX, EBX
    MOV IECX, ECX
    MOV IEDX, EDX
    POP EDX
    POP ECX
    POP EBX
  end;
{$ENDIF}

  if FCPUIdFormat = ifContinuous then
    Result := Format(cnIFContinuous, [iEax, iEbx, iEcx, iEdx])
  else
    Result := Format(cnIFDashed, [iEax, iEbx, iEcx, iEdx])
end;

// ��ȡ CPU ���к�
function TCnCpuId.GetCnCPUID: AnsiString;
const
  SCnIFContinuous = '%.4x%.4x%.4x%.4x%.4x%.4x';
  SCnIFDashed = '%.4x-%.4x-%.4x-%.4x-%.4x-%.4x';
var
  SFmt: string;
  iEax, iEcx, iEdx, iEdx1: Integer;
{$IFDEF WIN64}
  Rec64: TCnCPUIDResult;
{$ENDIF}
begin
{$IFDEF WIN64}
  Rec64 := GetCPUID64;
  iEax := Rec64[0];
  iEdx1 := Rec64[1];
  iEcx := Rec64[2];
  iEdx := Rec64[3];
{$ELSE}
  asm
    PUSH EBX
    PUSH ECX
    PUSH EDX
    MOV  EAX, $1
    DW $A20F      //CPUID
    MOV IEAX, EAX
    MOV IEDX1, EDX
    MOV EAX, $3
    DW $A20F
    MOV IECX, ECX
    MOV IEDX, EDX
    POP EDX
    POP ECX
    POP EBX
  end;
{$ENDIF}

  if FCPUIdFormat = ifContinuous then
    SFmt := SCnIFContinuous
  else
    SFmt := SCnIFDashed;

  if iEdx1 and (1 shr 18) = 0 then // Cpu ���кŲ��ܶ�������ȫ0
  begin
    Result := Format(SFmt, [0, 0, 0, 0, 0, 0]);
    FSupportCpuSns.Add(nil); // �� False
  end
  else
  begin
    FSupportCpuSns.Add(Pointer(True));
    Result := Format(SFmt,
      [(iEax and $FFFF0000) shr 16, iEax and $FFFF,
       (iEcx and $FFFF0000) shr 16, iEcx and $FFFF,
       (iEdx and $FFFF0000) shr 16, iEdx and $FFFF]);
  end;
end;

// ��� CPU OEM ������
function TCnCpuId.GetCnCPUOem: AnsiString;
var
  iEax, iEbx, iEcx, iEdx: Integer;
{$IFDEF WIN64}
  Rec64: TCnCPUIDResult;
{$ENDIF}
begin
{$IFDEF WIN64}
  Rec64 := GetCPUOem64;
  iEax := Rec64[0];
  iEbx := Rec64[1];
  iEcx := Rec64[2];
  iEdx := Rec64[3];
{$ELSE}
  asm
    PUSH EBX
    PUSH ECX
    PUSH EDX
    MOV  EAX, $0
    DW $A20F      //CPUID
    MOV IEAX, EAX
    MOV IEBX, EBX
    MOV IECX, ECX
    MOV IEDX, EDX
    POP EDX
    POP ECX
    POP EBX
  end;
{$ENDIF}
  SetLength(Result, 3 * SizeOf(Integer));
  CopyMemory(@Result[1], @iEbx, SizeOf(Integer));
  CopyMemory(@Result[1 + SizeOf(Integer)], @iEdx, SizeOf(Integer));
  CopyMemory(@Result[1 + 2 * SizeOf(Integer)], @iEcx, SizeOf(Integer));
end;

function GetCnCpuIdSupport: Boolean;
asm
{$IFDEF WIN64}
  MOV     AL, 1  // 64 λ�ݷ���֧��
{$ELSE}
  PUSHFD   //������ֱ�Ӵ�ȡ������ͨ����ջ
  POP     EAX
  MOV     EDX, EAX
  XOR     EAX, SCN_CPUID_BIT
  PUSH    EAX
  POPFD
  PUSHFD
  POP     EAX
  XOR     EAX,EDX  // ��� ID λ�Ƿ���Ӱ��
  JZ      @exit    // CPUID ��Ч
  MOV     AL, 1  // CPUID ��Ч
@exit:
{$ENDIF}
end;

// ��ȡ���� CPU �����к�
procedure TCnCpuId.ReadCPUInfo;
var
  I: Integer;
  Mask: Integer;
  CurrProc: THandle;
  SysInfo: TSystemInfo;
  ProcessAffinityOld: TCnNativeUInt;
  ProcessAffinity: TCnNativeUInt;
  SystemAffinity: TCnNativeUInt;
begin
  FCPUCount := 0;
  FSupportCpuIds.Clear;
  FSupportCpuSns.Clear;
  FCPUIds.Clear;
  FCPUOems.Clear;
  FCPUInfos.Clear;
  
  // ��ȡ CPU ����
  GetSystemInfo(SysInfo);
  FCPUCount := SysInfo.dwNumberOfProcessors;

  // ��ȡ���� CPU �����к�
  Mask := $1;
  CurrProc := GetCurrentProcess;
  if not GetProcessAffinityMask(CurrProc, ProcessAffinityOld, SystemAffinity) then
    Exit;

  try
    for I := 0 to FCpuCount - 1 do
    begin
      ProcessAffinity := Mask shl I;
      if not SetProcessAffinityMask(CurrProc, ProcessAffinity) then
        Break;

      FSupportCpuIds.Add(Pointer(GetCnCpuIdSupport));
      if FSupportCpuIds[FSupportCpuIds.Count - 1] <> nil then
      begin
        FCPUIds.Add(GetCnCPUID);
        FCPUInfos.Add(GetCnCPUInfoString);
        FCPUOems.Add(GetCnCPUOem);
      end
      else
      begin
        FCPUIds.Add('');
        FCPUInfos.Add('');
        FCPUOems.Add('');
      end;
    end;
  finally
    //�ָ�Ĭ��
    SetProcessAffinityMask(CurrProc, ProcessAffinityOld);
  end;
end;

procedure TCnCpuId.SetCPUIdFormat(ACPUIdFormat: TCnCPUIdFormat);
begin
  if FCPUIdFormat <> ACPUIdFormat then
  begin
    FCPUIdFormat := ACPUIdFormat;
    ReadCPUInfo;
  end;
end;

// ��õ� CPU ϵͳ ID
function TCnCpuId.GetFirstCPUId: string;
begin
  if FCPUIds.Count > 0 then
    Result := FCPUIds.Strings[0];
end;

// �õ��ڼ��� CPU �����к�
function TCnCpuId.GetCPUId(Index: Integer): string;
begin
  Result := '';
  // ��֤ FCPUIds �����ĺϷ���
  if (Index < 0) or (Index > FCPUIds.Count - 1) then
    Exit;

  Result := FCPUIds.Strings[Index];
end;

function TCnCpuId.GetAverageCPUUsage: Integer;
begin
  if not FCPUUsageRead then
    Result := -1
  else
    Result := FAverageCPUUsage;
end;

function TCnCpuId.GetCPUUsage(Index: Integer): Integer;
begin
  if not FCPUUsageRead or (Index > FCPUCount - 1) then
    Result := -1
  else
    Result := FCPUUsage[Index];
end;

function TCnCpuId.GetFirstCPUUsage: Integer;
begin
  if FCPUUsageRead and (FCPUCount > 0) then
    Result := FCPUUsage[0]
  else
    Result := -1;
end;

function TCnCpuId.RefreshCPUUsages: Cardinal;
var
  CpuCnt: Cardinal;
  I: integer;
  Spt: Pointer;
  Sbi: TSystemBasicInformation;

  dwType, cbData: Cardinal;
  hOpen: HKEY;
  Buffer: Cardinal;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    for I := 0 to FCPUCount - 1 do // �����ֵ
      FLastCnt[I] := FCurCnt[I];

    if NtQuerySystemInformation(SystemBasicInformation, @Sbi, SizeOf(Sbi), nil)
      <> STATUS_SUCCESS then
      CpuCnt := 1
    else
      CpuCnt := Sbi.NumberProcessors;

    Spt := AllocMem(CpuCnt * (SizeOf(TSystemProcessorTimes) + 4));
    NtQuerySystemInformation(SystemProcessorTimes, Spt, CpuCnt * (SizeOf(TSystemProcessorTimes) + 4), @Result);

    for I := 0 to CpuCnt - 1  do
    begin
      with PSystemProcessorTimes(PChar(Spt) + I * (sizeof(TSystemProcessorTimes) + 4))^ do
        FCurCnt[I] := IdleTime.QuadPart;
        
      // ���������
      try
        FCPUUsage[I] := Round((10000000 - (FCurCnt[I] - FLastCnt[I]) / (FTimer.Interval / 1000)) / 10000000 * 100);
      except
        FCPUUsage[I] := 0;
      end;
    end;
    FreeMem(spt);
  end
  else
  begin
    if RegOpenKeyEx(HKEY_DYN_DATA,'PerfStats\StatData', 0, KEY_READ, hOpen) = ERROR_SUCCESS then
    begin
      cbData:=sizeof(Cardinal);
      if RegQueryValueEx(hOpen, 'KERNEL\CPUUsage', nil, @dwType, PBYTE(@Buffer),
        @cbData) = ERROR_SUCCESS then
        FCPUUsage[0] := Buffer;
      RegCloseKey(hOpen);
    end
    else
      FCPUUsage[0] := -1;
  end;

  FCPUUsageRead := True;
end;

procedure TCnCpuId.CpuUsageTimer(Sender: TObject);
var
  I: Integer;
begin
  RefreshCPUUsages;
  
  FAverageCPUUsage := 0;
  for I := 0 to FCPUCount - 1 do
  begin
    if FCPUUsage[I] <> -1 then
      FAverageCPUUsage := FAverageCPUUsage + FCPUUsage[I];
  end;

  if FCPUCount > 0 then
    FAverageCPUUsage := Round(FAverageCPUUsage / FCPUCount)
  else
    FAverageCPUUsage := -1;
end;

function TCnCpuId.GetCPUOem(Index: Integer): string;
begin
  Result := '';
  // ��֤ FCPUIds �����ĺϷ���
  if (Index < 0) or (Index > FCPUOems.Count - 1) then
    Exit;

  Result := FCPUOems.Strings[Index];
end;

function TCnCpuId.GetFirstCPUOem: string;
begin
  if FCPUOems.Count > 0 then
    Result := FCPUOems.Strings[0];
end;

function TCnCpuId.GetSupportCPUId(Index: Integer): Boolean;
begin
  Result := False;
  // ��֤ FSupportCpuIds �����ĺϷ���
  if (Index < 0) or (Index > FSupportCpuIds.Count - 1) then
    Exit;

  Result := Boolean(FSupportCpuIds[Index]);
end;

function TCnCpuId.GetSupportCPUSn(Index: Integer): Boolean;
begin
  Result := False;
  // ��֤ FSupportCpuIds �����ĺϷ���
  if (Index < 0) or (Index > FSupportCpuSns.Count - 1) then
    Exit;

  Result := Boolean(FSupportCpuSns[Index]);
end;

function TCnCpuId.GetFirstSupportCPUId: Boolean;
begin
  Result := False;
  if FSupportCpuIds.Count > 0 then
    Result := Boolean(FSupportCpuIds[0]);
end;

function TCnCpuId.GetFirstSupportCPUSn: Boolean;
begin
  Result := False;
  if FSupportCpuSns.Count > 0 then
    Result := Boolean(FSupportCpuSns[0]);
end;

function TCnCpuId.GetCPUInfoString(Index: Integer): string;
begin
  Result := '';
  if (Index < 0) or (Index > FCPUInfos.Count - 1) then
    Exit;

  Result := FCPUInfos.Strings[Index];
end;

function TCnCpuId.GetFirstCPUInfoString: string;
begin
  if FCPUInfos.Count > 0 then
    Result := FCPUInfos[0];
end;

function FindAwardBios(var BiosAddr: PByte): UINT;
var
  ABiosAddr: PByte;
  szBiosData: array [0..127] of Char;
  Len: Integer;
  Loop: Byte;
begin
  Result:= 0;
  ABiosAddr:= PByte(DWORD(BiosAddr) + $EC71);

  CopyMemory(@szBiosData[0], ABiosAddr, 127);
  szBiosData[127]:= #0;

  Len:= StrLen(PChar(@szBiosData[0]));
  if (Len <= 0) or (Len >= 128) then
    Exit;

  //AWard:         07/08/2002-i845G-ITE8712-JF69VD0CC-00
  //Phoenix-Award: 03/12/2002-sis645-p4s333
  if (szBiosData[2] <> '/') or (szBiosData[5] <> '/') then
    Exit;

  Loop:= 0;
  while szBiosData[Loop] <> #0 do
  begin
    if (szBiosData[Loop] < ' ') or (szBiosData[Loop] >= Chr(127)) then
      Break;
    Inc(Loop);
  end;

  if szBiosData[Loop] = #0 then
  begin
    BiosAddr:= ABiosAddr;
    Result:= Len;
  end;
end;

function FindAmiBios(var BiosAddr: PByte): UINT;
var
  ABiosAddr: PByte;
  szBiosData: array [0..127] of Char;
  Len: Integer;
  Loop: Byte;
begin
  Result:= 0;
  ABiosAddr:= PByte(DWORD(BiosAddr) + $F478);

  CopyMemory(@szBiosData[0], ABiosAddr, 127);
  szBiosData[127]:= #0;

  Len:= StrLen(PChar(@szBiosData[0]));
  if (Len <= 0) or (Len >= 128) then
    Exit;

  // Example: "AMI: 51-2300-000000-00101111-030199-"
  if (szBiosData[2] <> '-') or (szBiosData[7] <> '-') then
    Exit;

  Loop:= 0;
  while szBiosData[Loop] <> #0 do
  begin
    if (szBiosData[Loop] < ' ') or (szBiosData[Loop] >= Chr(127)) then
      Break;
    Inc(Loop);
  end;

  if szBiosData[Loop] = #0 then
  begin
    BiosAddr:= ABiosAddr;
    Result:= Len;
  end;
end;

function FindPhoenixBios(var BiosAddr: PByte): UINT;
var
  ABiosAddr: PByte;
  szBiosData: array [0..127] of Char;
  Len: Integer;
  I, Loop: Byte;
begin
  for I := 0 to 2 do
  begin
    ABiosAddr:= PByte(DWORD(BiosAddr) + BiosOffset[I]);
    CopyMemory(@szBiosData[0], ABiosAddr, 127);
    szBiosData[127]:= #0;
    Len:= StrLen(PChar(@szBiosData[0]));
    if (Len <= 0) or (Len >= 128) then
      Continue;

    // Example: Phoenix "NITELT0.86B.0044.P11.9910111055"
    if (szBiosData[7] <> '.') or (szBiosData[11] <> '.') then
      Continue;

    Loop:= 0;
    while szBiosData[Loop] <> #0 do
    begin
      if (szBiosData[Loop] < ' ') or (szBiosData[Loop] >= Chr(127)) then
        Break;
      Inc(Loop);
    end;

    if szBiosData[Loop] = #0 then
    begin
      BiosAddr:= ABiosAddr;
      Result:= Len;
      Exit;
    end;
  end;
  Result:= 0;
end;

function CnGetBiosID: string;
var
  Size: DWORD;
  RealAddr: LARGE_INTEGER;
  Path: PWCHAR;
  BaseAddr: DWORD;
  UniString: TUNICODE_STRING;
  Obj: TOBJECT_ATTRIBUTES;
  hSection: THandle;
  PBiosSerial: PByte;
  uBiosSerialLen: UINT;
  szSystemInfo: array [0..4095] of Char;
  ReturnLen: UINT;
begin
  FillChar(szSystemInfo, 4096, 0);
  ReturnLen:= 0;

  RealAddr.LowPart:= $000F0000;
  RealAddr.HighPart:= $00000000;
  Size:= $FFFF;
  Path:= '\device\physicalmemory';
  BaseAddr:= 0;
  UniString.Buffer:= Path;
  UniString.Length:= $2C;
  UniString.MaximumLength:= $2E;

  Obj.Attributes:= 64;
  Obj.Length:= 24;
  Obj.ObjectName:= @UniString;
  Obj.RootDirectory:= 0;
  Obj.SecurityDescriptor:= nil;
  Obj.SecurityQualityOfService:= nil;

  //���ú������������ڴ����ӳ��
  if (ZwOpenSection(hSection, 4, @Obj) = 0) and
     (ZwMapViewOfSection(hSection, $FFFFFFFF, @BaseAddr, 0, $FFFF, @RealAddr, @Size, 1, 0, 2) = 0) then
  begin
    //ִ�к���ڵ�ǰ���̵Ŀռ俪��һ��64k�Ŀռ䣬����f000:0000��f000:ffff��������ӳ�䵽����
    //ӳ��Ļ�ַ��BaseAddr����,���ӳ�䲻������,Ӧ����ZwUnmapViewOfSection�Ͽ�ӳ��
    PBiosSerial:= PByte(BaseAddr);
    uBiosSerialLen:= FindAwardBios(PBiosSerial);
    if uBiosSerialLen = 0 then
    begin
      uBiosSerialLen:= FindAmiBios(PBiosSerial);
      if uBiosSerialLen = 0 then
        uBiosSerialLen:= FindPhoenixBios(PBiosSerial);
    end;

    if uBiosSerialLen <> 0 then
    begin
      CopyMemory(Pointer(szSystemInfo + ReturnLen), PBiosSerial, uBiosSerialLen);
      Inc(ReturnLen, uBiosSerialLen);
    end;
    ZwUnmapViewOfSection($FFFFFFFF, Pointer(BaseAddr));
  end;

  if ReturnLen <> 0 then
  begin
    SetLength(Result, ReturnLen);
    MoveMemory(@Result[1], @szSystemInfo[0], ReturnLen);
  end;
end;

{ TCnHardDiskInfo }

constructor TCnHardDiskInfo.Create;
begin
  inherited;
  FHardDiskSns := TStringList.Create;
  FVolumnLetters := TStringList.Create;
  FVolumnNames := TStringList.Create;
  ReadHardDisk;
end;

destructor TCnHardDiskInfo.Destroy;
begin
  FVolumnNames.Free;
  FVolumnLetters.Free;
  FHardDiskSns.Free;
  inherited;
end;

function TCnHardDiskInfo.GetDiskSerialNo(Index: Integer): string;
begin
  if (Index >= 0) or (Index < FHardDiskSns.Count) then
    Result := FHardDiskSns[Index]
  else
    Result := '';
end;

function TCnHardDiskInfo.GetVolumnLetter(Index: Integer): string;
begin
  if (Index >= 0) or (Index < FVolumnLetters.Count) then
    Result := FVolumnLetters[Index]
  else
    Result := '';
end;

function TCnHardDiskInfo.GetVolumnName(Index: Integer): string;
begin
  if (Index >= 0) or (Index < FVolumnNames.Count) then
    Result := FVolumnNames[Index]
  else
    Result := '';
end;

procedure TCnHardDiskInfo.ReadHardDisk;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    ReadPhysicalDriveInNTWithZeroRights;
  end
  else
  begin
    // TODO: Win9x
  end;

  ReadVolumnInfo;
end;

function        LoCase( ch : AnsiChar ) : AnsiChar;
begin
//asm
//{ ->    AL      Character       }
//{ <-    AL      Result          }
//
//        CMP     AL,'A'
//        JB      @@exit
//        CMP     AL,'Z'
//        JA      @@exit
//        ADD     AL,'a' - 'A'
//@@exit:
  if Ch in ['A'..'Z'] then
    Result := AnsiChar(Chr(Ord(Ch) + (Ord('a') - Ord('A'))))
  else
    Result := Ch;
end;

procedure TCnHardDiskInfo.ReadPhysicalDriveInNTWithZeroRights;
var
  I: Integer;
  H: THandle;
  Drive: string;
  BytesReturned: Cardinal;
  Query: TStoragePropertyQuery;
  Buffer: array[0..8191] of Byte;
  Descriptor: PStorageDeviceDescriptor;
  Sn: AnsiString;

  function flipAndCodeBytes(Str: PAnsiChar; Offset: Integer): AnsiString;
  var
    I, J, K: Integer;
    P: Integer;
    C, T: AnsiChar;
    Buf: PAnsiChar;
  begin
    if Offset <= 0 then // If offset 0, no serial number.
    begin
      Result := '';
      Exit;
    end;

    SetLength(Result, 1024);
    ZeroMemory(@(Result[1]), Length(Result));

    K := 0;
    Buf := @(Result[1]);

    P := 0;
    J := 1;
    I := Offset;
    while (J > 0) and (Str[I] <> #0) do  // ʮ�����ƽ���
    begin
      C := LoCase(Str[I]);
      if C = ' ' then
        C := '0';

      Inc(P);
      Buf[K] := AnsiChar(Chr((Ord(Buf[K]) shl 4)));
      if C in ['0'..'9'] then
        Buf[K] := AnsiChar(Chr(Ord(Buf[K]) or (Ord(C) - Ord('0'))))
      else if C in ['a'..'f'] then
        Buf[K] := AnsiChar(Chr(Ord(Buf[K]) or (Ord(C) - Ord('a') + 10)))
      else
      begin
        J := 0;
        Break;
      end;

      if P = 2 then
      begin
        if (Buf[K] <> #0) and (Ord(Buf[K]) <= $1F) then
        begin
          J := 0;
          Break;
        end;
        Inc(K);
        P := 0;
        Buf[K] := #0;
      end;

      Inc(I);
    end;

    if J = 0 then
    begin
      J := 1;
      K := 0;
      I := Offset;
      while (J <> 0) and (Str[I] <> #0) do
      begin
        if Ord(Str[I]) <= $1F then
        begin
          J := 0;
          Break;
        end;
        Buf[K] := Str[I];
        Inc(K);
        Inc(I);
      end;
    end;

    if J = 0 then
      K := 0;

    // ��תǰ���ֽ�
    Buf[K] := #0;
    J := 0;
    while J < K do
    begin
      T := Buf[J];
      Buf[J] := Buf[J + 1];
      Buf[J + 1] := T;
      Inc(J, 2);
    end;

    Result := Trim(Result);
  end;

begin
  for I := 0 to 15 do  // Max Disk number is 16
  begin
    Drive := '\\.\PhysicalDrive' + IntToStr(I);
    H := CreateFile(PChar(Drive), 0, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
      OPEN_EXISTING, 0, 0);
    if H = INVALID_HANDLE_VALUE then
      Exit;

    try
      FillChar(Buffer, SizeOf(Buffer), 0);
      FillChar(Query, SizeOf(Query), 0);
      Query.PropertyId := StorageDeviceProperty;
      Query.QueryType := PropertyStandardQuery;

      BytesReturned := 0;
      if DeviceIoControl(H, IOCTL_STORAGE_QUERY_PROPERTY, @Query, SizeOf(Query),
        @(Buffer[0]), SizeOf(Buffer), BytesReturned, nil) then
      begin
        Descriptor := PStorageDeviceDescriptor(@(Buffer[0]));
        SetLength(Sn, 1024);
        ZeroMemory(@(Sn[1]), Length(Sn));

        StrCopy(PAnsiChar(@(Sn[1])), PAnsiChar(flipAndCodeBytes(@(Buffer[0]), Descriptor^.SerialNumberOffset)));
        Sn := Trim(Sn);

        FHardDiskSns.Add(Trim(Sn));
        Inc(FHardDiskCount);
      end;
    finally
      CloseHandle(H);
    end
  end;
end;

procedure TCnHardDiskInfo.ReadVolumnInfo;
var
  Letter: Char;
  Drive: string;
  NotUsed: DWORD;
  VolumeFlags: DWORD;
  VSNumber: DWORD;
  PType: array[0..63] of Char;
  VName: array[0..63] of Char;
begin
  Letter := 'C';
  while Ord(Letter) <= Ord('Z') do
  begin
    Drive := Letter + ':\';
    if GetVolumeInformation(PChar(Drive), @VName, SizeOf(VName),
      @VSNumber, NotUsed, VolumeFlags, PType, 64) then
    begin
      FVolumnLetters.Add(Drive);
      FVolumnNames.Add(VName);
      Inc(FVolumnCount);
    end;

    Inc(Letter);
  end;
end;

procedure TCnCpuId.ReadLogicalCPUInfo;
var
  Buf: array of TSystemLogicalProcessorInformation;
  RetLen: DWORD;
  I: Integer;
begin
  ClearLocalCPUInfo;
  if not Assigned(GetLogicalProcessorInformation) then
    Exit;

  RetLen := 0;
  if GetLogicalProcessorInformation(nil, RetLen) then
    Exit;

  if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
    Exit;

  SetLength(Buf, (RetLen div SizeOf(TSystemLogicalProcessorInformation)) + 1);
  if not GetLogicalProcessorInformation(@Buf[0], RetLen) then
    Exit;

  for I := Low(Buf) to High(Buf) do
  begin
    case Buf[I].Relationship of
      RelationCache:
        begin
          case Buf[I].Cache.Level of
            1: Inc(FL1CacheCount);
            2: Inc(FL2CacheCount);
            3: Inc(FL3CacheCount);
          end;
        end;
      RelationNumaNode:
        begin
          Inc(FNumaNodeCount);
        end;
      RelationProcessorCore:
        begin
          Inc(FProcessorCoreCount);
          Inc(FLogicalProcessorCount, CountSetBits(Buf[I].ProcessorMask));
        end;
      RelationProcessorPackage:
        begin
          Inc(FProcessorPackageCount);
        end;
    end;
  end;
end;

procedure TCnCpuId.ClearLocalCPUInfo;
begin
  FProcessorCoreCount := 0;
  FL1CacheCount := 0;
  FNumaNodeCount := 0;
  FL3CacheCount := 0;
  FProcessorPackageCount := 0;
  FLogicalProcessorCount := 0;
  FL2CacheCount := 0;
end;

initialization
  GetNtNativeAPIs;

finalization
  FreeNtNativeAPIs;

end.
