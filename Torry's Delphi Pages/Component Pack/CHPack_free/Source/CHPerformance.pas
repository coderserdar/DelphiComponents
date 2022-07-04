unit CHPerformance;

{ ##############################################################################
  TCHPerformance

  Version   		:   1.0.1
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 04.10.2002    - First Release
  1.0.1 - 09.03.2003    - reorganize "uses" for more performance and less memory needed

  ############################################################################ }

interface

uses
  Windows, SysUtils, Classes;

type
  TPerformanceResult = (prMillisecond, prSecond);
  
  TCHPerformance = class;

  TPDWord = ^DWORD;

  TCHPerformance = class(TComponent)
  private
    NtQuerySystemInformation: function(infoClass: DWORD; buffer: Pointer; bufSize: DWORD; returnSize: TPDword): DWORD; stdcall;
    FOldIdleTime: LARGE_INTEGER;
    FOldSystemTime: LARGE_INTEGER;
    FFrequence : TLargeInteger;
    FBeginCount : TLargeInteger;
    FEndCount : TLargeInteger;
    FPerformanceResult: TPerformanceResult;
    function Li2Double(x: LARGE_INTEGER): Double;
  public
    function GetCPUUsage : string;
    procedure SetPerformanceCounter;
    function GetPerformanceCounter : Extended;
  published
    property PerformanceResult : TPerformanceResult read FPerformanceResult Write FPerformanceResult;
  end;

const
  SystemBasicInformation = 0;
  SystemPerformanceInformation = 2;
  SystemTimeInformation = 3;

type
  TSystem_Basic_Information = packed record
    dwUnknown1: DWORD; 
    uKeMaximumIncrement: ULONG; 
    uPageSize: ULONG;
    uMmNumberOfPhysicalPages: ULONG;
    uMmLowestPhysicalPage: ULONG; 
    uMmHighestPhysicalPage: ULONG; 
    uAllocationGranularity: ULONG; 
    pLowestUserAddress: Pointer; 
    pMmHighestUserAddress: Pointer; 
    uKeActiveProcessors: ULONG; 
    bKeNumberProcessors: byte; 
    bUnknown2: byte; 
    wUnknown3: word; 
  end; 

type 
  TSystem_Performance_Information = packed record 
    liIdleTime: LARGE_INTEGER;
    dwSpare: array[0..75] of DWORD; 
  end; 

type
  TSystem_Time_Information = packed record
    liKeBootTime: LARGE_INTEGER; 
    liKeSystemTime: LARGE_INTEGER; 
    liExpTimeZoneBias: LARGE_INTEGER; 
    uCurrentTimeZoneId: ULONG; 
    dwReserved: DWORD; 
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHPerformance]);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHPerformance.GetCPUUsage: string;
var 
  SysBaseInfo: TSystem_Basic_Information; 
  SysPerfInfo: TSystem_Performance_Information; 
  SysTimeInfo: TSystem_Time_Information; 
  nStatus: Longint;
  nSystemTime, nIdleTime: Double;
begin 
  if @NtQuerySystemInformation = nil then
    NtQuerySystemInformation := GetProcAddress(GetModuleHandle('ntdll.dll'), 'NtQuerySystemInformation');

  // get number of processors in the system
  nStatus := NtQuerySystemInformation(SystemBasicInformation, @SysBaseInfo, SizeOf(SysBaseInfo), nil);
  if nStatus <> 0 then
    exit;

  // get new system time
  nStatus := NtQuerySystemInformation(SystemTimeInformation, @SysTimeInfo, SizeOf(SysTimeInfo), nil);
  if nStatus <> 0 then
    Exit;

  // get new CPU's idle time
  nStatus := NtQuerySystemInformation(SystemPerformanceInformation, @SysPerfInfo, SizeOf(SysPerfInfo), nil);
  if nStatus <> 0 then
    Exit;

  // if it's a first call - skip it
  if (FOldIdleTime.QuadPart <> 0) then
  begin
    // CurrentValue = NewValue - OldValue
    nIdleTime := Li2Double(SysPerfInfo.liIdleTime) - Li2Double(FOldIdleTime);
    nSystemTime := Li2Double(SysTimeInfo.liKeSystemTime) - Li2Double(FOldSystemTime);

    // CurrentCpuIdle = IdleTime / SystemTime
    nIdleTime := nIdleTime / nSystemTime;

    // CurrentCpuUsage% = 100 - (CurrentCpuIdle * 100) / NumberOfProcessors
    nIdleTime := 100.0 - nIdleTime * 100.0 / SysBaseInfo.bKeNumberProcessors + 0.5;

    Result := FormatFloat('0.0', nIdleTime);
  end;

  // store new CPU's idle and system time
  FOldIdleTime := SysPerfInfo.liIdleTime;
  FOldSystemTime := SysTimeInfo.liKeSystemTime;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHPerformance.Li2Double(x: LARGE_INTEGER): Double;
begin
  Result := x.HighPart * 4.294967296E9 + x.LowPart;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHPerformance.GetPerformanceCounter: Extended	;
begin
  QueryPerformanceCounter(FEndCount);

  if FPerformanceResult = prMillisecond then
    Result := ((FEndCount - FBeginCount) / FFrequence) * 1000
  else
    Result := ((FEndCount - FBeginCount) / FFrequence);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPerformance.SetPerformanceCounter;
begin
  QueryPerformanceFrequency(FFrequence);
  QueryPerformanceCounter(FBeginCount);
end;



end.
