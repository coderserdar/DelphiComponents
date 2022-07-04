{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       TgtProcessManager                               }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}
unit o_ProcessManager;

interface
uses
   Classes
  ,Windows
  ,Messages
  ,Contnrs
  ,TLHelp32
  ,PSApi
  ,o_FileInfo
  ;

const
  ABOVE_NORMAL_PRIORITY_CLASS   = $00008000;
  BELOW_NORMAL_PRIORITY_CLASS   = $00004000;
  PROCESS_MODE_BACKGROUND_BEGIN = $00100000;
  PROCESS_MODE_BACKGROUND_END   = $00200000;

type

{------------------------------------------------------------------------------}
  TgtProcessManager  = class;
{------------------------------------------------------------------------------}
  TgtProcessPriority = (
                           ppAboveNormal
                          ,ppBelowNormal
                          ,ppHigh
                          ,ppIdle
                          ,ppNormal
                          ,ppBackgroundBegin
                          ,ppBackgroundEnd
                          ,ppRealTime
                        );

{------------------------------------------------------------------------------}
  TgtRunningProcessMemoryInfo = class(TPersistent)
  private
    FPagefileUsage: Double;
    FPeakWorkingSetSize: Double;
    FQuotaPeakPagedPoolUsage: Double;
    FQuotaNonPagedPoolUsage: Double;
    FQuotaPagedPoolUsage: Double;
    FPeakPagefileUsage: Double;
    FWorkingSetSize: Double;
    FRecordSize: Double;
    FQuotaPeakNonPagedPoolUsage: Double;
    FPageFaultCount: Integer;
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Assign(Source : TPersistent);override;
    procedure AssignProcessInfo(MemoryInfo : PPROCESS_MEMORY_COUNTERS);
    destructor  Destroy;override;
  published
    { Published declarations}
    property  RecordSize                  : Double  read FRecordSize;//in bytes
    property  PageFaultCount              : Integer read FPageFaultCount;
    property  PeakWorkingSetSize          : Double  read FPeakWorkingSetSize;//in bytes
    property  WorkingSetSize              : Double  read FWorkingSetSize;//in bytes
    property  QuotaPeakPagedPoolUsage     : Double  read FQuotaPeakPagedPoolUsage;//in bytes
    property  QuotaPagedPoolUsage         : Double  read FQuotaPagedPoolUsage;//in bytes
    property  QuotaPeakNonPagedPoolUsage  : Double  read FQuotaPeakNonPagedPoolUsage;//in bytes
    property  QuotaNonPagedPoolUsage      : Double  read FQuotaNonPagedPoolUsage;//in bytes
    property  PagefileUsage               : Double  read FPagefileUsage;//in bytes
    property  PeakPagefileUsage           : Double  read FPeakPagefileUsage;
  end;
{------------------------------------------------------------------------------}
  TgtRunningProcess = class(TPersistent)
  private
    FSize       : Cardinal;
    FModuleID   : Cardinal;
    FFlags      : Cardinal;
    FUsage      : Single;
    FThreads    : Cardinal;
    FHeapID     : Cardinal;
    FPPID       : Cardinal;
    FPID        : Cardinal;
    FClassBase  : Integer;
    FExeName    : string;
    FMemoryInfo : TgtRunningProcessMemoryInfo;
    FPriority   : TgtProcessPriority;
    FMemoryUse  : Cardinal;
    FProcessManager : TgtProcessManager;
    FReadingData: Boolean;
    { Private declarations }
  protected
    { Protected declarations }
      FHandle        : Cardinal;
      property ReadingData : Boolean read FReadingData;
      procedure WndProc(var Message: TMessage);
  public
    { Public declarations }
    constructor Create(ProcessManager : TgtProcessManager);
    destructor  Destroy;override;
    procedure Assign(Source : TPersistent);override;
    procedure AssignProcessInfo(ProcessInfo : TProcessEntry32);
  published
    { Published declarations}
    property ExeName   : string                        read FExeName;
    property Size      : Cardinal                      read FSize;
    property Usage     : Single                        read FUsage      write FUsage;
    property PID       : Cardinal                      read FPID;
    property HeapID    : Cardinal                      read FHeapID;
    property ModuleID  : Cardinal                      read FModuleID;
    property Threads   : Cardinal                      read FThreads;
    property PPID      : Cardinal                      read FPPID;
    property ClassBase : Integer                       read FClassBase;
    property Flags     : Cardinal                      read FFlags;
    property MemoryInfo: TgtRunningProcessMemoryInfo   read FMemoryInfo;
    property Priority  : TgtProcessPriority            read FPriority;
    property MemoryUse : Cardinal                      read FMemoryUse;
  end;
{------------------------------------------------------------------------------}
  TgtProcessManager = class(TComponent)
  private
    FHostApp              : string;
    FHostAppLoadedModules : TStrings;
    FFileInfo             : TgtFileInfo;
    FRunningProcessList: TObjectList;
    FOnAfterLaunch: TNotifyEvent;
    procedure SetHostApp(const Value: string);
    function GetRunningProcess(Index: Integer): TgtRunningProcess;
    { Private declarations }
  protected
    { Protected declarations }
    function  GetProcessId(ProcessName : string):Cardinal;
    function  GetModuleLoaded(ModuleName : string):Boolean;
    function  InternalTerminateProcess(ExeName : string):Integer;
    procedure InternalLaunch(ExeFileName: string;WaitTimeOut:Integer=5000;ExeParams:string='';Wait: Boolean = False);
    procedure GetHostAppLoadedModules;
    function  GetMemUsageForProcess(ProcId:Cardinal):Cardinal;
    procedure GetCpuUsage(AProcess : TgtRunningProcess);
  public
    { Public declarations }
    constructor Create(AOwner : TComponent);override;
    destructor  Destroy;override;
    procedure   UpdateRunningProcessList;
    function    IsModuleLoaded(ModuleName:string;LookInCache : Boolean = False):Boolean;
    function    TerminateProcess(ExeName : string):Integer;overload;
    function    TerminateProcess(RunningProcess : TgtRunningProcess):Integer;overload;
    function    TerminateProcess(RunningProcessIndex : Integer):Integer;overload;
    procedure   Launch(ExeFileName: string;WaitTimeOut:Integer=5000;ExeParams:string='');
    procedure   LaunchAndWait(ExeFileName: string;WaitTimeOut:Integer=5000;ExeParams:string='');
  public
    property  RunningProcesses[Index : Integer] : TgtRunningProcess read GetRunningProcess;
  published
    { Published declarations}
    property HostApp              : string          read FHostApp               write SetHostApp;
    property HostAppLoadedModules : TStrings        read FHostAppLoadedModules;
    property ModuleInfo           : TgtFileInfo     read FFileInfo;
    property RunningProcessList   : TObjectList     read FRunningProcessList;
  published
    property OnAfterLaunch : TNotifyEvent read FOnAfterLaunch write FOnAfterLaunch;
  end;


implementation

uses
    SysUtils
   ,SyncObjs
  ;


{ THDLLChecker }
{------------------------------------------------------------------------------}
constructor TgtProcessManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHostAppLoadedModules := TStringList.Create;
  FFileInfo             := TgtFileInfo.Create(Self);
  FRunningProcessList   := TObjectList.Create(True);
end;
{------------------------------------------------------------------------------}
destructor TgtProcessManager.Destroy;
begin
  FreeAndNil(FHostAppLoadedModules);
  FRunningProcessList.Clear;
  FRunningProcessList.Free;
  inherited;
end;
{------------------------------------------------------------------------------}
function TgtProcessManager.GetProcessId(ProcessName: string): Cardinal;
var
    Handle  : THandle;
    Process : TProcessEntry32;
begin
  Result := 0;
  Handle:=CreateToolHelp32SnapShot(TH32CS_SNAPALL,0);
  Process.dwSize := SizeOf(TProcessEntry32);
  if Process32First(Handle,Process) then
  begin
    if SameText(Process.szExeFile,ProcessName) then
    begin
      Result := Process.th32ProcessID;
      Exit;
    end
    else
    begin
      while Process32Next(Handle,Process) do
      begin
        if SameText(Process.szExeFile,ProcessName) then
        begin
          Result := Process.th32ProcessID;
          Break;
        end;
      end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtProcessManager.GetHostAppLoadedModules;
var
  DllHandle   : THandle;
  DllProcInfo : TModuleEntry32;
  PID         : Cardinal;
begin
  PID    := GetProcessId(HostApp);
  DllHandle := CreateToolHelp32SnapShot(TH32CS_SNAPMODULE,PID);
  DllProcInfo.dwSize := Sizeof(TModuleEntry32);
  FHostAppLoadedModules.Clear;
  if Module32First(DllHandle,DllProcInfo) then
  begin
    FHostAppLoadedModules.Add(DllProcInfo.szExePath);
    while Module32Next(DllHandle,DllProcInfo) do
    begin
      FHostAppLoadedModules.Add(DllProcInfo.szExePath);
    end;
  end;
end;
{------------------------------------------------------------------------------}
function TgtProcessManager.GetModuleLoaded(ModuleName: string): Boolean;
var
  DllHandle   : THandle;
  DllProcInfo : TModuleEntry32;
  PID         : Cardinal;
begin
  Result := False;
  PID    := GetProcessId(HostApp);
  DllHandle := CreateToolHelp32SnapShot(TH32CS_SNAPMODULE,PID);
  DllProcInfo.dwSize := Sizeof(TModuleEntry32);
  if Module32First(DllHandle,DllProcInfo) then
  begin
    if SameText(DllProcInfo.szModule,ModuleName) then
    begin
      Result := True;
      FFileInfo.FileName := DllProcInfo.szExePath;
      Exit;
    end;
    while Module32Next(DllHandle,DllProcInfo) do
    begin
      if SameText(DllProcInfo.szModule,ModuleName) then
      begin
        FFileInfo.FileName := DllProcInfo.szExePath;
        Result := True;
        Exit;
      end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
function TgtProcessManager.GetMemUsageForProcess(ProcId: Cardinal): Cardinal;
var Hwnd : THandle;
    MemCounters : PPROCESS_MEMORY_COUNTERS;
    Size : Cardinal;
begin
  Hwnd   := 0;
  MemCounters := nil;
  try
    Result := 0;
    Size   := SizeOf(PROCESS_MEMORY_COUNTERS);
    GetMem(MemCounters,Size);
    MemCounters.cb := Size;
    Hwnd := OpenProcess(PROCESS_ALL_ACCESS, False, ProcId);
    if GetProcessMemoryInfo(Hwnd,MemCounters,Size) then
      Result := MemCounters.WorkingSetSize;
  finally
    CloseHandle(Hwnd);
    FreeMem(MemCounters);
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtProcessManager.GetCpuUsage(AProcess : TgtRunningProcess);
begin
  //ToDo
end;
{------------------------------------------------------------------------------}
procedure TgtProcessManager.UpdateRunningProcessList;
var
    Handle         : THandle;
    Process        : TProcessEntry32;
    RunProcess     : TgtRunningProcess;
begin
  FRunningProcessList.Clear;
  Handle         := CreateToolHelp32SnapShot(TH32CS_SNAPALL,0);
  Process.dwSize := SizeOf(TProcessEntry32);
  try
    if Process32First(Handle,Process) then
    begin
      RunProcess := TgtRunningProcess.Create(Self);
      RunProcess.AssignProcessInfo(Process);
      FRunningProcessList.Add(RunProcess);
      while Process32Next(Handle,Process) do
      begin
        RunProcess := TgtRunningProcess.Create(Self);
        RunProcess.AssignProcessInfo(Process);
        FRunningProcessList.Add(RunProcess);
      end;
    end;
  finally
    CloseHandle(Handle);
  end;
end;
{------------------------------------------------------------------------------}
function TgtProcessManager.IsModuleLoaded(ModuleName: string;LookInCache: Boolean): Boolean;
begin
  if LookInCache then
    Result := FHostAppLoadedModules.IndexOf(ModuleName) <> -1
  else
    Result := GetModuleLoaded(ModuleName);
end;
{------------------------------------------------------------------------------}
function TgtProcessManager.InternalTerminateProcess(ExeName: string): Integer;
const
  PROCESS_TERMINATE = $0001;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
  FRunningProcessIndex : Cardinal;
begin
  Result := 0;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);

  while Integer(ContinueLoop) <> 0 do
  begin
    if SameText(ExeName,ExtractFileName(FProcessEntry32.szExeFile)) then
    begin
      FRunningProcessIndex :=OpenProcess(PROCESS_TERMINATE,BOOL(0),FProcessEntry32.th32ProcessID);
      Result := Integer(TerminateProcess(FRunningProcessIndex));
    end;
     ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;
{------------------------------------------------------------------------------}
function TgtProcessManager.TerminateProcess(ExeName: string): Integer;
begin
  Result := InternalTerminateProcess(ExeName);
end;
{------------------------------------------------------------------------------}
function TgtProcessManager.TerminateProcess(RunningProcess: TgtRunningProcess): Integer;
begin
  Result := InternalTerminateProcess(RunningProcess.ExeName);
end;
{------------------------------------------------------------------------------}
function TgtProcessManager.TerminateProcess(RunningProcessIndex: Integer): Integer;
begin
  Result := InternalTerminateProcess(RunningProcesses[RunningProcessIndex].ExeName);
end;
{------------------------------------------------------------------------------}
procedure TgtProcessManager.InternalLaunch(ExeFileName:string;WaitTimeOut:Integer;ExeParams : string;Wait: Boolean);
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  if FileExists(ExeFileName) then
  begin
    FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
    StartUpInfo.cb      := SizeOf(TStartupInfo);
    StartUpInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
    StartUpInfo.wShowWindow := SW_SHOW;
    if CreateProcess(nil,PChar(ExeFileName + ExeParams ),nil,nil,False
                        ,NORMAL_PRIORITY_CLASS,nil,nil,StartUpInfo,ProcessInfo) then
    begin
      if Assigned(FOnAfterLaunch) then
        FOnAfterLaunch(Self);
      if Wait then
        WaitForSingleObject(ProcessInfo.hProcess, WaitTimeOut);
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtProcessManager.Launch(ExeFileName:string;WaitTimeOut:Integer;ExeParams:string);
begin
  InternalLaunch(ExeFileName,WaitTimeOut,ExeParams,False);
end;
{------------------------------------------------------------------------------}
procedure TgtProcessManager.LaunchAndWait(ExeFileName:string;WaitTimeOut:Integer;ExeParams:string);
begin
  InternalLaunch(ExeFileName,WaitTimeOut,ExeParams,True);
end;
{------------------------------------------------------------------------------}




// Getters - Setters \\
{------------------------------------------------------------------------------}
procedure TgtProcessManager.SetHostApp(const Value: string);
begin
  FHostApp := Value;
  GetHostAppLoadedModules;

end;
{------------------------------------------------------------------------------}
function TgtProcessManager.GetRunningProcess(Index: Integer): TgtRunningProcess;
begin
  Result := TgtRunningProcess(FRunningProcessList[Index]);
end;
{------------------------------------------------------------------------------}




{ TgtRunningProcess }
{------------------------------------------------------------------------------}
constructor TgtRunningProcess.Create(ProcessManager : TgtProcessManager);
begin
   FMemoryInfo     := TgtRunningProcessMemoryInfo.Create;
   FProcessManager := ProcessManager;
   //FProcCpuThread  := TgtProcessCpuUsageThread.Create(True);
   FHandle         := Classes.AllocateHWnd(WndProc);
end;
{------------------------------------------------------------------------------}
destructor TgtRunningProcess.Destroy;
begin
 { FProcCpuThread.Terminate;
  FProcCpuThread := nil;}
  FMemoryInfo.Free;
  Classes.DeallocateHWnd(FHandle);
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtRunningProcess.WndProc(var Message: TMessage);
begin
//
end;
{------------------------------------------------------------------------------}
procedure TgtRunningProcess.Assign(Source: TPersistent);
begin
  if Source is TgtRunningProcess then
  begin
    FExeName   := TgtRunningProcess(Source).ExeName;
    FSize      := TgtRunningProcess(Source).Size;
    FUsage     := TgtRunningProcess(Source).Usage;
    FModuleID  := TgtRunningProcess(Source).ModuleID;
    FPID       := TgtRunningProcess(Source).PID;
    FPPID      := TgtRunningProcess(Source).PPID;
    FHeapID    := TgtRunningProcess(Source).HeapID;
    FThreads   := TgtRunningProcess(Source).Threads;
    FClassBase := TgtRunningProcess(Source).ClassBase;
    FFlags     := TgtRunningProcess(Source).Flags;
  end
  else
  inherited Assign(Source);
end;
{------------------------------------------------------------------------------}
procedure TgtRunningProcess.AssignProcessInfo(ProcessInfo : TProcessEntry32);
var
  MemInfo  : _PROCESS_MEMORY_COUNTERS;
  Priority : Cardinal;
begin

  MemInfo.cb := SizeOf(TProcessMemoryCounters);
  FExeName   := ProcessInfo.szExeFile;
  FSize      := ProcessInfo.dwSize;
  FModuleID  := ProcessInfo.th32ModuleID;
  FPID       := ProcessInfo.th32ProcessID;
  FMemoryUse := FProcessManager.GetMemUsageForProcess(FPID);
  FPPID      := ProcessInfo.th32ParentProcessID;
  FHeapID    := ProcessInfo.th32DefaultHeapID;
  FThreads   := ProcessInfo.cntThreads;
  FClassBase := ProcessInfo.pcPriClassBase;
  FFlags     := ProcessInfo.dwFlags;

  {try
    FReadingData := True;
    while FProcCpuThread.Executing do
      WaitForSingleObject(FHandle,INFINITE);
    FProcCpuThread.RunningProcess := Self;
  finally
    FReadingData := False;
  end;
  {while FProcCpuThread.Executing do
    WaitForSingleObject(FHandle,INFINITE);

  FProcCpuThread.RunningProcess := Self;

{  try
    FProcCpuThread := TgtProcessCpuUsageThread.Create(Self);
    FProcCpuThread.Resume;
    {while not FProcCpuThread.Terminated do
      WaitForSingleObject(FProcCpuThread.Handle,1000);
  finally
    FProcCpuThread := nil;
  end;}


  GetProcessMemoryInfo(FPID,@MemInfo,MemInfo.cb);
  FMemoryInfo.AssignProcessInfo(@MemInfo);

  Priority := Windows.GetPriorityClass(FPID);
  case Priority of
    NORMAL_PRIORITY_CLASS           : FPriority := ppNormal;
    IDLE_PRIORITY_CLASS             : FPriority := ppIdle;
    HIGH_PRIORITY_CLASS             : FPriority := ppHigh;
    REALTIME_PRIORITY_CLASS         : FPriority := ppRealTime;
    ABOVE_NORMAL_PRIORITY_CLASS     : FPriority := ppAboveNormal;
    BELOW_NORMAL_PRIORITY_CLASS     : FPriority := ppBelowNormal;
    PROCESS_MODE_BACKGROUND_BEGIN   : FPriority := ppBackgroundBegin;
    PROCESS_MODE_BACKGROUND_END     : FPriority := ppBackgroundEnd;
  end;
end;
{------------------------------------------------------------------------------}






{ TgtRunningProcesseMemoryInfo }
{------------------------------------------------------------------------------}
procedure TgtRunningProcessMemoryInfo.Assign(Source: TPersistent);
begin
  if Source is  TgtRunningProcessMemoryInfo then
  begin
    FRecordSize                 := TgtRunningProcessMemoryInfo(Source).RecordSize;
    FPageFaultCount             := TgtRunningProcessMemoryInfo(Source).PageFaultCount;
    FPeakWorkingSetSize         := TgtRunningProcessMemoryInfo(Source).PeakWorkingSetSize;
    FWorkingSetSize             := TgtRunningProcessMemoryInfo(Source).WorkingSetSize;
    FQuotaPeakPagedPoolUsage    := TgtRunningProcessMemoryInfo(Source).QuotaPeakPagedPoolUsage;
    FQuotaPagedPoolUsage        := TgtRunningProcessMemoryInfo(Source).QuotaPagedPoolUsage;
    FQuotaPeakNonPagedPoolUsage := TgtRunningProcessMemoryInfo(Source).QuotaPeakNonPagedPoolUsage;
    FQuotaNonPagedPoolUsage     := TgtRunningProcessMemoryInfo(Source).QuotaNonPagedPoolUsage;
    FPagefileUsage              := TgtRunningProcessMemoryInfo(Source).PagefileUsage;
    FPeakPagefileUsage          := TgtRunningProcessMemoryInfo(Source).PeakPagefileUsage;
  end
  else
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtRunningProcessMemoryInfo.AssignProcessInfo(MemoryInfo: PPROCESS_MEMORY_COUNTERS);
begin
  FRecordSize                 := MemoryInfo.cb;
  FPageFaultCount             := MemoryInfo.PageFaultCount;
  FPeakWorkingSetSize         := MemoryInfo.PeakWorkingSetSize;
  FWorkingSetSize             := MemoryInfo.WorkingSetSize;
  FQuotaPeakPagedPoolUsage    := MemoryInfo.QuotaPeakPagedPoolUsage;
  FQuotaPagedPoolUsage        := MemoryInfo.QuotaPagedPoolUsage;
  FQuotaPeakNonPagedPoolUsage := MemoryInfo.QuotaPeakNonPagedPoolUsage;
  FQuotaNonPagedPoolUsage     := MemoryInfo.QuotaNonPagedPoolUsage;
  FPagefileUsage              := MemoryInfo.PagefileUsage;
  FPeakPagefileUsage          := MemoryInfo.PeakPagefileUsage;
end;
{------------------------------------------------------------------------------}
destructor TgtRunningProcessMemoryInfo.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}








end.
