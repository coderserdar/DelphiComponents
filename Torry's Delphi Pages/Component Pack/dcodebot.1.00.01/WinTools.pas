
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit WinTools;

interface

{$I STD.INC}

uses
  Classes, Windows, Messages, StrTools, SysUtils, ShellAPI;

{ The GetWindowClassName function }

function GetWindowClassName(Wnd: HWND): string;

{ The GetWindowCaption function }

function GetWindowCaption(Wnd: HWND): string;

{ The IsWindowClass function }

function IsWindowClass(const ClassName: string; const Module: string = ''): Boolean;

{ The GetDesktopWindows procedure }

type
  TWindowStringFormat = set of (sfCaption, sfClassName, sfHandle, sfVisibility);

procedure GetDesktopWindows(Windows: TStrings; Format: TWindowStringFormat);

{ The GetChildWindows procedure }

procedure GetChildWindows(Parent: HWND; Windows: TStrings; Format: TWindowStringFormat);

{ The FindSimilarChild function }

function FindSimilarChild(Wnd: HWND; const ClassName: string): HWND;

{ The FindNestedChild function }

function FindNestedChild(Wnd: HWND; const ClassName: string): HWND;

{ The HideTaskbarIcon procedure }

procedure HideTaskbarIcon(Wnd: HWND);

{ The window position routines are used to query and modify the dimensions of
  a window using the TWindowPosition structure }

type
  TWindowPosition = record
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
  end;

procedure GetWindowPosition(Wnd: HWND; var Pos: TWindowPosition);
procedure SetWindowPosition(Wnd: HWND; const Pos: TWindowPosition);
function IsWindowPosition(Wnd: HWND; const Pos: TWindowPosition): Boolean;

{ The GetEnvironmentVariable function }

function GetEnvironmentVariable(const Name: string): string;

{ The TerminateProcess function }

function TerminateProcess(Process: THandle): Boolean;

{ The IsProcessWindow function }

function IsProcessWindow(Wnd: HWND): Boolean;

{ The IsProcess function }

function IsProcess(Process: THandle): Boolean;

{ The WindowFromPoint function returns any window, child or not, below the
  Point parameter. This function ignores hidden windows }

function WindowFromPoint(const Point: TPoint): HWND;

{ The GetDialogParent function }

function GetDialogParent(Wnd: HWND): HWND;

{ The ShutdownWindows procedure exits the current window session }

procedure ShutdownWindows;

{ Create process routines }

type
  TReadProc = procedure(const S: string; Data: Pointer);
  TWaitProc = procedure(Interval: Integer; Data: Pointer);

function CreateProcessAndReturn(const AppName: string; ShowState: Integer): THandle;

procedure CreateProcessAndWait(const AppName: string; ShowState: Integer;
  Data: Pointer; WaitProc: TWaitProc);

function CreateProcessAndRedirect(const AppName: string; Data: Pointer;
  ReadProc: TReadProc; WaitProc: TWaitProc): Boolean;

{ The ScanProcessMemory procedure }

type
  TScanProc = procedure(Memory: Pointer; Size: Integer);

procedure ScanProcessMemory(ScanProc: TScanProc);

{ Hooks routines }

type
  TKeyboardHook = procedure(Key: Word; State: Cardinal; var Remove: Boolean) of object;
  TMouseHook = procedure(Msg: Cardinal; const HookStruct: TMouseHookStruct;
    var Remove: Boolean) of object;

procedure HookKeyboard(Hook: TKeyboardHook);
procedure UnhookKeyboard(Hook: TKeyboardHook);
procedure HookMouse(Hook: TMouseHook);
procedure UnhookMouse(Hook: TMouseHook);

{ TLauncher class }

type
  TShowState = (ssHide, ssNormal, ssMinimized, ssMaximized);

  TLaunchOperation = (loOpen, loPrint, loExplore);

  TLaunchInfo = record
    ProcessInfo: TProcessInformation;
    StartupInfo: TStartupInfo;
    ProcessMask: DWORD;
    AfinityMask: DWORD;
  end;

  ELauncherError = class(Exception);

  TLauncher = class
  private
    FLaunchInfo: TLaunchInfo;
    FRunning: Boolean;
    FStartTick: Cardinal;
    FFileName: string;
    FInterval: Integer;
    FParams: string;
    FOperation: TLaunchOperation;
    FShowState: TShowState;
    FOnWait: TNotifyEvent;
    FOnTerminate: TNotifyEvent;
    FOnLaunch: TNotifyEvent;
    procedure SetFileName(const Value: string);
    procedure SetInterval(const Value: Integer);
    procedure SetParams(const Value: string);
    function GetRunning: Boolean;
    function GetElapsedTime: Cardinal;
  public
    procedure Launch;
    procedure Terminate;
    procedure Wait;
    property Running: Boolean read GetRunning;
    property ElapsedTime: Cardinal read GetElapsedTime;
    property FileName: string read FFileName write SetFileName;
    property Params: string read FParams write SetParams;
    property Operation: TLaunchOperation read FOperation write FOperation;
    property ShowState: TShowState read FShowState write FShowState;
    property Interval: Integer read FInterval write SetInterval;
    property OnLaunch: TNotifyEvent read FOnLaunch write FOnLaunch;
    property OnWait: TNotifyEvent read FOnWait write FOnWait;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
  end;

{ TRedirector class }

  TPriorityClass = (pcDefault, pcIdle, pcNormal, pcHigh, pcRealtime);

  TDataEvent = procedure(Sender: TObject; Buffer: Pointer; Size: Integer) of object;

  TPipeError = record
    hRead: DWORD;
    hWrite: DWORD;
  end;

  TRedirector = class
  private
    FAvailable: Integer;
    FProcessInfo: TProcessInformation;
    FExitCode: Integer;
    FExecutable: string;
    FCommandline: string;
    FDefaultErrorMode: Boolean;
    FStartSuspended: Boolean;
    FKillOnDestroy: Boolean;
    FDirectory: string;
    FEnvironment: Pointer;
    FInitialPriority: TPriorityClass;
    FPipeInput: TPipeError;
    FPipeOutput: TPipeError;
    FPipeError: TPipeError;
    FThread: TThread;
    FShowState: TShowState;
    FOnData: TDataEvent;
    FOnErrorData: TDataEvent;
    FOnTerminated: TNotifyEvent;
    procedure ReadStdOutput;
    procedure ReadStdError;
    procedure ProcessTerminated;
  protected
    procedure Error(Msg: string);
    procedure WinError(Msg: string);
    procedure CreatePipes;
    procedure ClosePipes;
    function GetRunning: Boolean;
    function GetExitCode: Integer;
    function GetProcessID: Integer;
    function GetThreadID: Integer;
    function GetProcessHandle: Integer;
    procedure SetShowState(Value: TShowState);
    function GetThreadHandle: Integer;
    procedure SetExecutable(Value: string);
    function GetCommandLine: string;
    procedure SetCommandLine(Value: string);
    procedure SetDefaultErrorMode(Value: Boolean);
    procedure SetStartSuspended(Value: Boolean);
    procedure SetInitialPriority(Value: TPriorityClass);
    procedure SetDirectory(Value: string);
    procedure SetEnvironment(Value: Pointer);
  public
    destructor Destroy; override;
    procedure Terminate;
    function Execute: THandle;
    procedure SendData(Buffer: Pointer; BufferSize: Integer);
    procedure SendText(S: string);
    property Running: Boolean read GetRunning;
    property ExitCode: Integer read GetExitCode;
    property ProcessID: Integer read GetProcessID;
    property ProcessHandle: Integer read GetProcessHandle;
    property ThreadID: Integer read GetThreadID;
    property ThreadHandle: Integer read GetThreadHandle;
    property Environment: Pointer read FEnvironment write SetEnvironment;
    property KillOnDestroy: Boolean read FKillOnDestroy write FKillOnDestroy;
    property Executable: string read FExecutable write SetExecutable;
    property CommandLine: string read GetCommandLine write SetCommandLine;
    property ShowState: TShowState read FShowState write SetShowState;
    property DefaultErrorMode: Boolean read FDefaultErrorMode write SetDefaultErrorMode;
    property StartSuspended: Boolean read FStartSuspended write SetStartSuspended;
    property InitialPriority: TPriorityClass read FInitialPriority write SetInitialPriority;
    property Directory: string read FDirectory write SetDirectory;
    property OnData: TDataEvent read FOnData write FOnData;
    property OnErrorData: TDataEvent read FOnErrorData write FOnErrorData;
    property OnTerminated: TNotifyEvent read FOnTerminated write FOnTerminated;
  end;

{ TPerformanceTimer class }

  EPerformanceError = class(Exception);

  TPerformanceTimer = class(TObject)
  private
    FResolution: Int64;
    FStart: Int64;
    FStop: Int64;
    FTiming: Boolean;
    function GetElapsedTime: LongWord;
  public
    constructor Create;
    procedure Start;
    procedure Stop;
    property ElapsedTime: LongWord read GetElapsedTime;
  end;

{ TUtilityWindow class }

  TUtilityWindow = class(TObject)
  private
    FOwner: TObject;
    FHandle: HWND;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    property Handle: HWND read FHandle;
  end;

{ TGlobalData class }

function GlobalDataExists(const Name: string): Boolean;

type
  TGlobalData = class(TObject)
  private
    FSize: Integer;
    FLocked: Boolean;
    FMap: THandle;
    FMutex: THandle;
    FData: Pointer;
    FCreator: Boolean;
    FName: string;
    function GetData: Pointer;
  public
    constructor Create(const Name: string; Size: Integer);
    destructor Destroy; override;
    procedure Lock;
    procedure UnLock;
    property Data: Pointer read GetData;
    property Creator: Boolean read FCreator;
    property Locked: Boolean read FLocked;
    property Name: string read FName;
    property Size: Integer read FSize;
  end;

{ TMemoryMappedFile class }

  EFileMappingError = class(Exception);

  TMemoryMappedFile = class(TObject)
  private
    FFileName: string;
    FFile: HFile;
    FMap: THandle;
    FViewStart: PChar;
    FViewEnd: PChar;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    property ViewStart: PChar read FViewStart;
    property ViewEnd: PChar read FViewEnd;
    property FileName: string read FFileName;
  end;

{ TBasePipe class }

  TPipeMode = (pmRead, pmWrite);

  TBasePipe = class(TObject)
  private
    FHandle: THandle;
    FConnected: Boolean;
    FMode: TPipeMode;
    FName: string;
  public
    constructor Create(const AName: string; AMode: TPipeMode); virtual;
    procedure Connect; virtual; abstract;
    procedure Disconnect; virtual; abstract;
    function Read(var Buffer; Count: LongWord): LongWord;
    function Write(const Buffer; Count: LongWord): LongWord;
    property Connected: Boolean read FConnected;
    property Handle: THandle read FHandle;
    property Mode: TPipeMode read FMode;
    property Name: string read FName;
  end;

{ TServerPipe class }

  TServerPipe = class(TBasePipe)
  public
    constructor Create(const AName: string; AMode: TPipeMode); override;
    destructor Destroy; override;
    procedure Connect; override;
    procedure Disconnect; override;
  end;

{ TClientPipe class }

  TClientPipe = class(TBasePipe)
  private
    FRemoteMachine: string;
  public
    destructor Destroy; override;
    procedure Connect; override;
    procedure Disconnect; override;
    property RemoteMachine: string read FRemoteMachine write FRemoteMachine;
  end;

{ TPipeThread class }

  TPipeThreadParams = record
    Thread: TThread;
    Pipe: TBasePipe;
    Instance: Pointer;
    Data: LongWord;
  end;

  TPipeProc = procedure(const Params: TPipeThreadParams);

  TPipeThread = class(TThread)
  private
    FParams: TPipeThreadParams;
    FPipeProc: TPipeProc;
  public
    constructor Create(const Params: TPipeThreadParams; PipeProc: TPipeProc);
    destructor Destroy; override;
    procedure Execute; override;
    property Terminated;
  end;

implementation

uses
  StrConst;

function GetWindowClassName(Wnd: HWND): string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if IsWindow(Wnd) and (GetClassName(Wnd, Buffer, MAX_PATH) <> 0) then
    Result := Buffer
  else
    Result := '';
end;

function GetWindowCaption(Wnd: HWND): string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if IsWindow(Wnd) and (GetWindowText(Wnd, Buffer, MAX_PATH) <> 0) then
    Result := Buffer
  else
    Result := '';
end;

procedure GetWindowPosition(Wnd: HWND; var Pos: TWindowPosition);
begin
  GetWindowRect(Wnd, TRect(Pos));
  with Pos do
  begin
    Dec(Width, Left);
    Dec(Height, Top);
  end;
  MapWindowPoints(GetDesktopWindow, GetParent(Wnd), TRect(Pos).TopLeft, 1);
end;

procedure SetWindowPosition(Wnd: HWND; const Pos: TWindowPosition);
begin
  with Pos do
    MoveWindow(Wnd, Left, Top, Width, Height, True);
end;

var
  WindowsStringFormat: TWindowStringFormat;

function EnumWindowsCallback(Wnd: HWND; Windows: TStrings): BOOL; stdcall;
const
  WindowStatus: array[Boolean] of string = ('hidden', 'visible');
var
  S: string;
begin
  S := '';
  if sfCaption in WindowsStringFormat then
    S := '"' + GetWindowCaption(Wnd) + '"';
  if sfClassName in WindowsStringFormat then
    S := Trim(S + ' ' + GetWindowClassName(Wnd));
  if sfHandle in WindowsStringFormat then
    S := Trim(S + ' [' + IntToHex(Wnd, 8) + ']');
  if sfVisibility in WindowsStringFormat then
    S := Trim(S + ' (' + WindowStatus[IsWindowVisible(Wnd)] + ')');
  if  WindowsStringFormat = [sfCaption] then
    S := Copy(S, 2, Length(S) - 2);
  Windows.AddObject(S, TObject(Wnd));
  Result := True;
end;

procedure GetDesktopWindows(Windows: TStrings; Format: TWindowStringFormat);
begin
  Windows.BeginUpdate;
  try
    Windows.Clear;
    WindowsStringFormat := Format;
    EnumWindows(@EnumWindowsCallback, Integer(Windows));
  finally
    Windows.EndUpdate;
  end;
end;

procedure GetChildWindows(Parent: HWND; Windows: TStrings; Format: TWindowStringFormat);
begin
  Windows.BeginUpdate;
  try
    Windows.Clear;
    WindowsStringFormat := Format;
    EnumChildWindows(Parent, @EnumWindowsCallback, Integer(Windows));
  finally
    Windows.EndUpdate;
  end;
end;

type
  TSimilarChild = record
    ClassName: PChar;
    Wnd: HWND;
  end;
  PSimilarChild = ^TSimilarChild;

function EnumSimilarChild(Wnd: HWND; SimilarChild: PSimilarChild): BOOL; stdcall;
var
  SearchClass: string;
  ChildClass: string;
  P: PChar;
begin
  Result := True;
  SearchClass := SimilarChild.ClassName;
  ChildClass := GetWindowClassName(Wnd);
  P := PChar(ChildClass);
  if SearchToken(P, SearchClass) then
  begin
    SimilarChild.Wnd := Wnd;
    Result := False;
  end;
end;

function FindSimilarChild(Wnd: HWND; const ClassName: string): HWND;
var
  Child: TSimilarChild;
begin
  Result := 0;
  if ClassName <> '' then
  begin
    Child.ClassName := PChar(ClassName);
    Child.Wnd := 0;
    EnumChildWindows(Wnd, @EnumSimilarChild, Integer(@Child));
    Result := Child.Wnd;
  end;
end;

function EnumNestedChild(Wnd: HWND; SimilarChild: PSimilarChild): BOOL; stdcall;
var
  SearchClass: string;
  ChildClass: string;
begin
  Result := True;
  SearchClass := UpperCase(SimilarChild.ClassName);
  ChildClass := UpperCase(GetWindowClassName(Wnd));
  if UpperCase(SearchClass) = UpperCase(ChildClass) then
  begin
    SimilarChild.Wnd := Wnd;
    Result := False;
  end;
  {if Result then
  begin
    EnumChildWindows(Wnd, @EnumNestedChild, Integer(@Child));
    Result :=
  end;}
end;

function FindNestedChild(Wnd: HWND; const ClassName: string): HWND;
var
  Child: TSimilarChild;
begin
  Result := 0;
  if ClassName <> '' then
  begin
    Child.ClassName := PChar(ClassName);
    Child.Wnd := 0;
    EnumChildWindows(Wnd, @EnumNestedChild, Integer(@Child));
    Result := Child.Wnd;
  end;
end;

procedure HideTaskbarIcon(Wnd: HWND);
begin
  SetWindowLong(Wnd, GWL_EXSTYLE, GetWindowLong(Wnd, GWL_EXSTYLE
    and not WS_EX_APPWINDOW) or WS_EX_TOOLWINDOW);
  ShowWindow(Wnd, SW_HIDE);
end;

function IsWindowClass(const ClassName: string;
  const Module: string = ''): Boolean;
var
  WndClass: TWndClass;
  Handle: HMODULE;
begin
  FillChar(WndClass, SizeOf(WndClass), #0);
  if Module <> '' then
  begin
    Handle := GetModuleHandle(PChar(Module));
    if Handle <> 0 then
      Result := GetClassInfo(Handle, PChar(ClassName), WndClass)
    else
      Result := False;
  end
  else
    Result := GetClassInfo(MainInstance, PChar(ClassName), WndClass);
end;

function GetEnvironmentVariable(const Name: string): string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if Windows.GetEnvironmentVariable(PChar(Name), Buffer, MAX_PATH) > 0 then
    Result := Buffer
  else
    Result := '';
end;

function TerminateProcess(Process: THandle): Boolean;
begin
  Process := OpenProcess(PROCESS_ALL_ACCESS, TRUE, Process);
  if (Process <> 0) then
  begin
   Result := Windows.TerminateProcess(Process, 0);
   CloseHandle(Process);
  end
  else
    Result := False;
end;

function IsProcessWindow(Wnd: HWND): Boolean;
var
  Process: THandle;
begin
  Result := IsWindow(Wnd);
  if Result then
  begin
    GetWindowThreadProcessId(Wnd, @Process);
    Result := Process = GetCurrentProcessID;
  end;
end;

function IsProcess(Process: THandle): Boolean;
var
  ExitCode: DWORD;
begin
  Result := GetExitCodeProcess(Process, ExitCode) and (ExitCode = STILL_ACTIVE);
end;

function IsWindowPosition(Wnd: HWND; const Pos: TWindowPosition): Boolean;
var
  CurrentPos: TWindowPosition;
begin
  GetWindowPosition(Wnd, CurrentPos);
  Result := CompareMem(@CurrentPos, @Pos, SizeOf(TWindowPosition));
end;

function WindowFromPoint(const Point: TPoint): HWND;
var
  Wnd: HWND;
  P: TPoint;
begin
  Result := 0;
  Wnd := GetDesktopWindow;
  while (Wnd <> Result) and IsWindow(Wnd) do
  begin
    Result := Wnd;
    P := Point;
    ScreenToClient(Result, P);
    Wnd := ChildWindowFromPointEx(Result, P, CWP_SKIPINVISIBLE);
  end;
end;

function GetDialogParent(Wnd: HWND): HWND;
begin
  Result := Wnd;
  if IsWindow(Result) then
    while GetWindowLong(Result, GWL_STYLE) and WS_CHILD = WS_CHILD do
      Result := GetParent(Result)
  else
    Result := 0;
end;

function CreateProcessAndReturn(const AppName: string; ShowState: Integer): THandle;
var
  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfo;
begin
  FillChar(StartupInfo, SizeOf(TStartupInfo), #0);
  with StartupInfo do
  begin
    cb := SizeOf(TStartupInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := ShowState;
  end;
  if CreateProcess(nil, PChar(AppName), nil, nil, False,
    CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo,
    ProcessInfo) then
    Result := ProcessInfo.dwProcessId
  else
    Result := 0;
end;

procedure CreateProcessAndWait(const AppName: string; ShowState: Integer;
  Data: Pointer; WaitProc: TWaitProc);
var
  Handle: THandle;
  Interval: Integer;
  State: Integer;
begin
  Handle := CreateProcessAndReturn(AppName, ShowState);
  if Handle = 0 then Exit;
  Interval := 0;
  repeat
    State := WaitForSingleObject(Handle, 100);
    if (State = WAIT_TIMEOUT) and Assigned(WaitProc) then
    begin
      Inc(Interval, 100);
      WaitProc(Interval, Data);
    end;
  until State <> WAIT_TIMEOUT;
end;

function CreateProcessAndRedirect(const AppName: string; Data: Pointer;
  ReadProc: TReadProc; WaitProc: TWaitProc): Boolean;

type
  TPipeHandles = record
    Read: THandle;
    Write: THandle;
  end;

  procedure InitializePipes(var Pipes: TPipeHandles; Inherit: PHandle);
  var
    SecAttr: TSecurityAttributes;
  begin
    SecAttr.nLength := SizeOf(SecAttr);
    SecAttr.lpSecurityDescriptor := nil;
    SecAttr.bInheritHandle := True;
    with Pipes do
    begin
      if not CreatePipe(Read, Write, @SecAttr, 1024) then RaiseLastWin32Error;
      if not DuplicateHandle(GetCurrentProcess, Read, GetCurrentProcess,
        @Inherit, 0, True, DUPLICATE_CLOSE_SOURCE or DUPLICATE_SAME_ACCESS) then
        RaiseLastWin32Error;
    end;
  end;

  procedure ReadData(Pipe: THandle);
  var
    Available: Cardinal;
    Bytes: Cardinal;
    S: string;
  begin
    if PeekNamedPipe(Pipe, nil, 0, nil, @Available, nil) and
      (Available > 0) then
    begin
      SetLength(S, Available);
      if ReadFile(Pipe, PChar(S)^, Available, Bytes, nil) then
      begin
        SetLength(S, StrLen(PChar(S)));
        if Assigned(ReadProc) then
          ReadProc(S, Data);
      end;
    end;
  end;

  procedure ClosePipes(Pipes: array of TPipeHandles);
  var
    I: Integer;
  begin
    for I := Low(Pipes) to High(Pipes) do
    begin
      if Pipes[I].Read <> 0 then CloseHandle(Pipes[I].Read);
      if Pipes[I].Write <> 0 then CloseHandle(Pipes[I].Write);
    end;
  end;

var
  InputPipes, OutputPipes, ErrorPipes: TPipeHandles;
  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfo;
  StartTime: Cardinal;
begin
  FillChar(InputPipes, SizeOf(TPipeHandles), #0);
  FillChar(OutputPipes, SizeOf(TPipeHandles), #0);
  FillChar(ErrorPipes, SizeOf(TPipeHandles), #0);
  try
    InitializePipes(InputPipes, @InputPipes.Read);
    InitializePipes(OutputPipes, @OutputPipes.Write);
    InitializePipes(ErrorPipes, @ErrorPipes.Write);
    FillChar(StartupInfo, SizeOf(StartupInfo), 0);
    with StartupInfo do
    begin
      cb := SizeOf(TStartupInfo);
      wShowWindow :=  SW_HIDE;
      hStdInput := InputPipes.Read;
      hStdOutput := OutputPipes.Write;
      hStdError := ErrorPipes.Write;
      dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    end;
    Result := CreateProcess(nil, PChar(AppName), nil, nil, True,
      CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo,
      ProcessInfo);
    if Result then
    try
      StartTime := GetTickCount;
      while IsProcess(ProcessInfo.hProcess) do
      begin
        ReadData(OutputPipes.Read);
        ReadData(ErrorPipes.Read);
        if Assigned(WaitProc) then
          WaitProc(GetTickCount - StartTime, Data);
      end;
    finally
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
    end;
  finally
    ClosePipes([InputPipes, OutputPipes, ErrorPipes]);
  end;
end;

procedure ScanProcessMemory(ScanProc: TScanProc);
var
  SystemInfo: TSystemInfo;
  Process: THandle;
  Memory: Cardinal;
  MemoryInformation: TMemoryBasicInformation;
begin
  GetSystemInfo(SystemInfo);
  Process := GetCurrentProcess;
  Memory := 0;
  while Memory < Cardinal(SystemInfo.lpMaximumApplicationAddress) do
  begin
    MemoryInformation.RegionSize := 0;
    VirtualQueryEx(Process, Pointer(Memory), MemoryInformation,
      SizeOf(TMemoryBasicInformation));
    with MemoryInformation do
    begin
      Memory := Cardinal(BaseAddress) + Cardinal(RegionSize);
      if (AllocationProtect and PAGE_READWRITE = PAGE_READWRITE) and
        (Type_9 = MEM_PRIVATE) and (State = MEM_COMMIT) then
        ScanProc(BaseAddress, RegionSize);
    end;
  end;
end;

{ Hook support routines }

type
  PMethod = ^TMethod;
  THookList = record
    Hook: HHOOK;
    Callbacks: TList;
  end;

procedure SetHook(var HookList: THookList; Kind: Integer; HookProc: TFarProc;
  const Method: TMethod);
var
  DynamicMethod: PMethod;
begin
  with HookList do
    if Hook = 0 then
    begin
      Hook := SetWindowsHookEx(Kind, HookProc, 0, GetCurrentThreadId);
      Callbacks := TList.Create;
    end;
  New(DynamicMethod);
  DynamicMethod^ := Method;
  HookList.Callbacks.Add(DynamicMethod);
end;

procedure ReleaseHook(var HookList: THookList; const Method: TMethod);
var
  DyanmicMethod: PMethod;
  I: Integer;
begin
  with HookList do
    if Hook <> 0 then
    begin
      for I := 0 to Callbacks.Count do
      begin
        DyanmicMethod := Callbacks[I];
        if (DyanmicMethod.Code = Method.Code) and
          (DyanmicMethod.Data = Method.Data) then
        begin
          Dispose(DyanmicMethod);
          Callbacks.Delete(I);
          Break;
        end;
      end;
      if Callbacks.Count = 0 then
      begin
        UnhookWindowsHookEx(Hook);
        Hook := 0;
        Callbacks.Free;
        Callbacks := nil;
      end;
    end
end;

var
  InternalKeyboardHooks: THookList;

function KeyboardHook(Code: Integer; wParam: LongInt; lParam: LongInt): LongInt; stdcall;
var
  Remove: Boolean;
  Method: TMethod;
  Callback: TKeyboardHook absolute Method;
  I: Integer;
begin
  with InternalKeyboardHooks do
    if Code < 0 then
      Result := CallNextHookEx(Hook, Code, wParam, lParam)
    else
    begin
      Remove := False;
      for I := 0 to Callbacks.Count - 1 do
      begin
        Method := PMethod(Callbacks[I])^;
        Callback(wParam, lParam, Remove);
      end;
      if Remove then Result := 1 else Result := 0;
    end;
end;

procedure HookKeyboard(Hook: TKeyboardHook);
var
  Method: TMethod absolute Hook;
begin
  SetHook(InternalKeyboardHooks, WH_KEYBOARD, @KeyboardHook, Method);
end;

procedure UnhookKeyboard(Hook: TKeyboardHook);
var
  Method: TMethod absolute Hook;
begin
  ReleaseHook(InternalKeyboardHooks, Method);
end;

var
  InternalMouseHooks: THookList;

function MouseHook(Code: Integer; Msg: Cardinal;
  HookStruct: PMouseHookStruct): Integer; stdcall;
var
  Remove: Boolean;
  Method: TMethod;
  Callback: TMouseHook absolute Method;
  I: Integer;
begin
  with InternalMouseHooks do
    if Code < 0 then
      Result := CallNextHookEx(Hook, Code, Msg, Integer(HookStruct))
    else
    begin
      Remove := False;
      for I := 0 to Callbacks.Count - 1 do
      begin
        Method := PMethod(Callbacks[I])^;
        Callback(Msg, HookStruct^, Remove);
      end;
      if Remove then Result := 1 else Result := 0;
    end;
end;

procedure HookMouse(Hook: TMouseHook);
var
  MethodParam: TMethod absolute Hook;
  Method: PMethod;
begin
  with InternalMouseHooks do
    if Hook = 0 then
    begin
      Hook := SetWindowsHookEx(WH_MOUSE, @MouseHook, 0, GetCurrentThreadId);
      Callbacks := TList.Create;
    end;
  New(Method);
  Method^ := MethodParam;
  InternalMouseHooks.Callbacks.Add(Method);
end;

procedure UnhookMouse(Hook: TMouseHook);
var
  MethodParam: TMethod absolute Hook;
  Method: PMethod;
  I: Integer;
begin
  with InternalMouseHooks do
    if Hook <> 0 then
    begin
      for I := 0 to Callbacks.Count do
      begin
        Method := Callbacks[I];
        if (Method.Code = MethodParam.Code) and
          (Method.Data = MethodParam.Data) then
        begin
          Dispose(Method);
          Callbacks.Delete(I);
          Break;
        end;
      end;
      if Callbacks.Count = 0 then
      begin
        UnhookWindowsHookEx(Hook);
        Hook := 0;
        Callbacks.Free;
        Callbacks := nil;
      end;
    end;
end;

procedure ReleaseAllHooks;

  procedure ReleaseHooks(var Hooks: THookList);
  var
    I: Integer;
  begin
  with Hooks do
    if Hook <> 0 then
    begin
      UnhookWindowsHookEx(Hook);
      Hook := 0;
      for I := 0 to Callbacks.Count - 1 do
        Dispose(Callbacks[I]);
      Callbacks.Free;
      Callbacks := nil;
    end;
  end;

begin
  ReleaseHooks(InternalKeyboardHooks);
  ReleaseHooks(InternalMouseHooks);
end;

procedure ShutdownWindows;
var
  ProcessHandle: Integer;
  TokenHandle: THandle;
  TokenPrivileges: TTokenPrivileges;
  Dummy: TTokenPrivileges;
  Buffer: DWORD;
begin
  with TokenPrivileges, Privileges[0] do
  begin
    PrivilegeCount := 1;
    LookupPrivilegeValue(nil, 'SeShutdownPrivilege', LUID);
    Attributes := SE_PRIVILEGE_ENABLED;
  end;
  ProcessHandle := GetCurrentProcess;
  OpenProcessToken(ProcessHandle, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, TokenHandle);
  AdjustTokenPrivileges(TokenHandle, False, TokenPrivileges, SizeOf(TokenPrivileges),
    Dummy, Buffer);
  ExitWindowsEx(EWX_SHUTDOWN or EWX_FORCE or EWX_REBOOT, 0);
end;

{ TLauncher }

const
  ShowStates: array [TShowState] of Integer = (SW_HIDE, SW_SHOW,
    SW_SHOWMINIMIZED, SW_SHOWMAXIMIZED);

procedure TLauncher.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TLauncher.SetInterval(const Value: Integer);
begin
  FInterval := Value;
end;

procedure TLauncher.SetParams(const Value: string);
begin
  FParams := Value;
end;

procedure TLauncher.Launch;

  procedure Succeeded(Value: Boolean);
  begin
    if Value then
      if Assigned(FOnLaunch) then
        FOnLaunch(Self)
      else
    else
      raise Exception.Create(SLauncherFileError);
  end;

const
  LaunchOps: array [TLaunchOperation] of PChar = ('open', 'print', 'explore');
var
  ShortFileName: array [0..MAX_PATH] of Char;
  FileStr: string;
begin
  Terminate;
  if Trim(FFileName) = '' then
    Exit;
  GetShortPathName(PChar(FFileName), @ShortFileName, SizeOf(ShortFileName));
  if Operation = loOpen then
    with FLaunchInfo  do
    begin
      FillChar(FLaunchInfo, SizeOf(TLaunchInfo), #0);
      FileStr := '';
      if UpperCase(ExtractFileExt(FFileName)) <> '.EXE' then
      begin
        SetLength(FileStr, MAX_PATH);
        FindExecutable(ShortFileName, nil, PChar(FileSTr));
        SetLength(FileStr, StrLen(PChar(FileStr)));
        FileStr := FileStr + ' ' + Trim(StrPas(ShortFileName) + ' ' + FParams);
      end
      else
        FileStr := Trim(StrPas(ShortFileName) + ' ' + FParams);
      StartupInfo.cb := SizeOf(TStartupInfo);
      StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
      StartupInfo.wShowWindow := ShowStates[FShowState];
      Succeeded(CreateProcess(nil, PChar(FileStr), nil, nil, False, CREATE_NEW_CONSOLE or
        NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInfo));
      FStartTick := GetTickCount;
      FRunning := True;
    end
  else
    Succeeded(ShellExecute(0, LaunchOps[FOperation], ShortFileName, PChar(FParams),
      nil, ShowStates[FShowState]) > 32);
end;

procedure TLauncher.Terminate;
begin
  if GetRunning then
    with FLaunchInfo.ProcessInfo do
    begin
      try
        if TerminateProcess(hProcess) then
          if Assigned(FOnTerminate) then
            FOnTerminate(Self)
          else
        else
          raise Exception.Create(SLauncherTerminateError);
      finally
        FRunning := False;
      end;
    end;
end;

procedure TLauncher.Wait;
var
  TimeOut: Cardinal;
  WaitObject: Cardinal;
begin
  if GetRunning then
  begin
    TimeOut := FInterval;
    if TimeOut = 0 then
      TimeOut := INFINITE;
    repeat
      WaitObject := WaitForSingleObject(FLaunchInfo.ProcessInfo.hProcess, TimeOut);
      if Assigned(FOnWait) then
        FOnWait(Self);
    until WaitObject = WAIT_OBJECT_0;
    if Assigned(FOnTerminate) then
      FOnTerminate(Self);
    FRunning := False;
  end;
end;

function TLauncher.GetRunning: Boolean;
begin
  if FRunning then
    FRunning := IsProcess(FLaunchInfo.ProcessInfo.hProcess);
  Result := FRunning;
end;

function TLauncher.GetElapsedTime: Cardinal;
begin
  if GetRunning then
    Result := GetTickCount - FStartTick
  else
    Result := 0;
end;

{ TRedirector }

const
  DUPLICATE_CLOSE_SOURCE = 1;
  DUPLICATE_SAME_ACCESS  = 2;

type
  TRedirectorThread = class(TThread)
  protected
    FRedirector: TRedirector;
    procedure Execute; override;
  public
    constructor Create(ARedirector: TRedirector);
  end;

procedure TRedirector.Error(Msg: string);
begin
  TerminateProcess(ProcessHandle);
  raise Exception.Create(Msg);
end;

procedure TRedirector.WinError(Msg: string);
begin
  Error(Msg + IntToStr(GetLastError));
end;

procedure TRedirector.CreatePipes;
var
  SecAttr: TSecurityAttributes;
begin
  SecAttr.nLength := SizeOf(SecAttr);
  SecAttr.lpSecurityDescriptor := nil;
  SecAttr.bInheritHandle := True;
  with FPipeInput do
  begin
    if not CreatePipe(hRead, hWrite, @SecAttr, 1024)
      then WinError('Error on STDIN pipe creation: ');
    if not DuplicateHandle(GetCurrentProcess, hRead, GetCurrentProcess,
      @hRead, 0, True, DUPLICATE_CLOSE_SOURCE or DUPLICATE_SAME_ACCESS) then
      WinError('Error on STDIN pipe duplication: ');
  end;
  with FPipeOutput do
  begin
    if not CreatePipe(hRead, hWrite, @SecAttr, 1024) then
      WinError('Error on STDOUT pipe creation: ');
    if not DuplicateHandle(GetCurrentProcess, hWrite, GetCurrentProcess,
      @hWrite, 0, True, DUPLICATE_CLOSE_SOURCE or DUPLICATE_SAME_ACCESS) then
      WinError('Error on STDOUT pipe duplication: ');
  end;
  with FPipeError do
  begin
    if not CreatePipe(hRead, hWrite, @SecAttr, 1024)
      then WinError('Error on STDERR pipe creation: ');
    if not DuplicateHandle(GetCurrentProcess, hWrite, GetCurrentProcess,
             @hWrite, 0, True, DUPLICATE_CLOSE_SOURCE or DUPLICATE_SAME_ACCESS)
      then WinError('Error on STDERR pipe duplication: ');
  end;
end;

procedure TRedirector.ClosePipes;
begin
  with FPipeInput do
  begin
    if hRead <> 0 then CloseHandle(hRead);
    if hWrite <> 0 then CloseHandle(hWrite);
    hRead := 0;
    hWrite := 0;
  end;
  with FPipeOutput do
  begin
    if hRead <> 0 then CloseHandle(hRead);
    if hWrite <> 0 then CloseHandle(hWrite);
    hRead := 0;
    hWrite := 0;
  end;
  with FPipeError do
  begin
    if hRead <> 0 then CloseHandle(hRead);
    if hWrite <> 0 then CloseHandle(hWrite);
    hRead := 0;
    hWrite := 0;
  end;
end;

function TRedirector.GetRunning: Boolean;
begin
  Result := ProcessHandle <> 0;
  if (Result) and (not IsProcess(ProcessHandle)) then
    Terminate;
end;

function TRedirector.GetExitCode: Integer;
begin
  if Running then
    Result := STILL_ACTIVE
  else
    Result := FExitCode;
end;

function TRedirector.GetProcessID: Integer;
begin
  Result := FProcessInfo.dwProcessID;
end;

function TRedirector.GetThreadID: Integer;
begin
  Result := FProcessInfo.dwThreadID;
end;

function TRedirector.GetProcessHandle: Integer;
begin
  Result := FProcessInfo.hProcess;
  if Result <> 0 then
    if not IsProcess(FProcessInfo.hProcess) then
    begin
      CloseHandle(FProcessInfo.hProcess);
      FillChar(FProcessInfo, SizeOf(TProcessInformation), #0);
    end;
end;

function TRedirector.GetThreadHandle: Integer;
begin
  Result := FProcessInfo.hThread;
end;

procedure TRedirector.SetExecutable(Value: string);
begin
  if (ANSICompareText(Value, Executable) = 0) or not Running then
    FExecutable := Value
  else if Running then
    Error('Cannot change Executable while process is active');
end;

procedure TRedirector.SetCommandLine(Value: string);
begin
  if (ANSICompareText(Value, Commandline) = 0) or (not Running) then
    FCommandline := Value
  else if Running then
    Error('Cannot change Commandline while process is active');
end;

function TRedirector.GetCommandLine: string;
begin
  Result := FExecutable;
  if Result = '' then
    Result := FCommandline
  else
    Result := FExecutable + ' ' + FCommandline;
end;

procedure TRedirector.SetDefaultErrorMode(Value: Boolean);
begin
  if (Value = DefaultErrorMode) or (not Running) then
    FDefaultErrorMode := Value
  else if Running then
    Error('Cannot change DefaultErrorMode while process is active');
end;

procedure TRedirector.SetStartSuspended(Value: Boolean);
begin
  if (Value = DefaultErrorMode) or not Running then FStartSuspended := Value
  else if Running then Error('Cannot change StartSuspended while process is active');
end;

procedure TRedirector.SetInitialPriority(Value: TPriorityClass);
begin
  if (Value = InitialPriority) or not Running then FInitialPriority := Value
  else if Running then Error('Cannot change InititalPriority while process is active');
end;

procedure TRedirector.SetDirectory(Value: string);
begin
  if (ANSICompareText(Value, Directory) = 0) or(not Running) then FDirectory := Value
  else if Running then Error('Cannot change Directory while process is active');
end;

procedure TRedirector.SetEnvironment(Value: Pointer);
begin
  if (Value = Environment) or not Running then FEnvironment := Value
  else if Running then Error('Cannot change Environment while process is active');
end;

procedure TRedirector.SetShowState(Value: TShowState);
begin
  if (Value = FShowState) or not Running then FShowState := Value
  else if Running then Error('Cannot change ShowWindow while process is active');
end;

procedure TRedirector.ReadStdOutput;
var
  BytesRead: DWORD;
  Buffer: Pointer;
begin
  GetMem(Buffer, FAvailable);
  try
    if not ReadFile(FPipeOutput.hRead, Buffer^, FAvailable, BytesRead, nil) then
    begin
      FThread.Terminate;
      WinError('Error reading STDOUT pipe: ');
    end;
    if Assigned(FOnData) then
      FOnData(Self, Buffer, BytesRead);
  finally
    FreeMem(Buffer);
  end;
end;

procedure TRedirector.ReadStdError;
var
  BytesRead: DWORD;
  Buffer: Pointer;
begin
  GetMem(Buffer, FAvailable);
  try
    if not ReadFile(FPipeError.hRead, Buffer^, FAvailable, BytesRead, nil) then
    begin
      FThread.Terminate;
      WinError('Error reading STDERR pipe: ');
    end;
    if Assigned(FOnErrorData) then
      FOnErrorData(Self, Buffer, BytesRead);
  finally
    FreeMem(Buffer);
  end;
end;

procedure TRedirector.ProcessTerminated;
begin
  if FThread <> nil then
    FThread.Terminate;
  FThread := nil;
  if Assigned(FOnTerminated) then FOnTerminated(Self);
  ClosePipes;
  TerminateProcess(FProcessInfo.dwProcessId);
  CloseHandle(FProcessInfo.hProcess);
  CloseHandle(FProcessInfo.hThread);
  FillChar(FProcessInfo, SizeOf(FProcessInfo), #0);
end;

procedure TRedirector.Terminate;
begin
  ProcessTerminated;
end;

function TRedirector.Execute: THandle;
var
  StartupInfo: TStartupInfo;
  szExecutable: PChar;
  szCommandline: PChar;
  szDirectory: PChar;
begin
  Result := 0;
  if Running then
    Error('Process is already active');
  if Trim(CommandLine)='' then
    Error('No commandline to run');
  try
    CreatePipes;
    FillChar(StartupInfo, SizeOf(StartupInfo), 0);
    with StartupInfo do
    begin
      cb := SizeOf(StartupInfo);
      wShowWindow :=  ShowStates[FShowState];
      hStdInput := FPipeInput.hRead;
      hStdOutput := FPipeOutput.hWrite;
      hStdError := FPipeError.hWrite;
      dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    end;
    if Trim(Executable) = '' then
      szExecutable := nil
    else
      szExecutable := PChar(FExecutable);
    if Trim(Commandline) = '' then
      szCommandline := nil
    else
      szCommandline := PChar(FCommandline);
    if Trim(Directory) = '' then
      szDirectory := nil
    else
      szDirectory := PChar(FDirectory);
    if CreateProcess(szExecutable, szCommandline, nil, nil, True,
     (CREATE_DEFAULT_ERROR_MODE and Integer(FDefaultErrorMode))
      or(CREATE_SUSPENDED and Integer(FStartSuspended)), Environment,
      szDirectory, StartupInfo, FProcessInfo) then
    begin
      Result := FProcessInfo.hProcess;
      // WaitForSingleObject(FProcessInfo.hProcess, 1500);
      FThread := TRedirectorThread.Create(Self);
    end
      else WinError('Error creating process: ');
  except
    on Exception do
    begin
      ClosePipes;
      CloseHandle(FProcessInfo.hProcess);
      CloseHandle(FProcessInfo.hThread);
      FillChar(FProcessInfo, SizeOf(FProcessInfo), 0);
      raise;
    end;
  end;
end;

procedure TRedirector.SendData(Buffer: Pointer; BufferSize: Integer);
var
  BytesWritten: DWORD;
begin
  if not Running then
    Error('Can''t send data to an inactive process');
  if not WriteFile(FPipeInput.hWrite, Buffer^, BufferSize, BytesWritten, nil) then
    WinError('Error writing to STDIN pipe: ');
end;

procedure TRedirector.SendText(S: string);
begin
  SendData(PChar(S), Length(S));
end;

destructor TRedirector.Destroy;
begin
  Terminate;
  inherited Destroy;
end;

constructor TRedirectorThread.Create(ARedirector: TRedirector);
begin
  FRedirector := ARedirector;
  inherited Create(False);
end;

procedure TRedirectorThread.Execute;
var
  Idle: Boolean;
begin
  FreeOnTerminate := True;
  while not Terminated do
  begin
    Idle := True;
    if PeekNamedPipe(FRedirector.FPipeOutput.hRead, nil, 0, nil,
      @FRedirector.FAvailable, nil) and (FRedirector.FAvailable>0) then
    begin
      Synchronize(FRedirector.ReadStdOutput);
      Idle := False;
    end;
    if PeekNamedPipe(FRedirector.FPipeError.hRead, nil, 0, nil,
      @FRedirector.FAvailable, nil) and (FRedirector.FAvailable>0) then
    begin
      Synchronize(FRedirector.ReadStdError);
      Idle := False;
    end;
    if Idle and (WaitForSingleObject(FRedirector.ProcessHandle,
      100) = WAIT_OBJECT_0) then
    begin
      {if not Terminated then
        Synchronize(FRedirector.ProcessTerminated);}
    end;
  end;
end;

{ TPerformanceTimer }

constructor TPerformanceTimer.Create;
begin
  if not QueryPerformanceFrequency(FResolution) then
    RaiseLastWin32Error;
  {$IFDEF D5_UP}
     FResolution := FResolution div 1000;
  {$ELSE}
     FResolution := FResolution / 1000;
  {$ENDIF}
end;

function TPerformanceTimer.GetElapsedTime: LongWord;
begin
  if FTiming then
    raise EPerformanceError.Create(SElapsedTime)
  else
  {$IFDEF D5}
    Result := (FStop-FStart) div FResolution;
  {$ELSE}
    Result := Trunc((FStop - FStart) / FResolution);
  {$ENDIF}
end;

procedure TPerformanceTimer.Start;
begin
  if not QueryPerformanceCounter(FStart) then
    raise EPerformanceError.CreateFmt(STimerError, ['start']);
  FTiming := True;
end;

procedure TPerformanceTimer.Stop;
begin
  if not QueryPerformanceCounter(FStop) then
    raise EPerformanceError.CreateFmt(STimerError, ['stop']);
  FTiming := False;
end;

{ TUtilityWindow }

threadvar
  CreationWindow: TUtilityWindow;

function UtilityProc(Wnd: HWND; uMsg: Cardinal; wParam: LongInt; lParam: LongInt): Integer; stdcall;
var
  UtilityWindow: TUtilityWindow;
  Msg: TMessage;
begin
  if CreationWindow <> nil then
  begin
    UtilityWindow := CreationWindow;
    UtilityWindow.FHandle := Wnd;
    CreationWindow := nil;
    SetWindowLong(Wnd, GWL_USERDATA, Integer(UtilityWindow));
  end
  else
    UtilityWindow := TUtilityWindow(GetWindowLong(Wnd, GWL_USERDATA));
  Result := DefWindowProc(Wnd, uMsg, wParam, lParam);
  if UtilityWindow <> nil then
  try
    Msg.Msg := uMsg;
    Msg.wParam := wParam;
    Msg.lParam := lParam;
    Msg.Result := Result;
    UtilityWindow.FOwner.Dispatch(Msg);
    if Msg.Msg = WM_DESTROY then
      UtilityWindow.FHandle := 0;
  except
    on E: Exception do
      MessageBox(0, PChar(E.ClassName + ': ' + E.Message), 'Error',
        MB_ICONERROR or MB_OK or MB_TASKMODAL);
  end;
end;

constructor TUtilityWindow.Create(AOwner: TObject);
var
  WindowClass: string;
  WindowName: string;
  WndClass: TWndClass;
begin
  inherited Create;
  FOwner := AOwner;
  WindowClass := ClassName;
  WindowName := FOwner.ClassName;
  if not IsWindowClass(PChar(WindowClass)) then
    with WndClass do
    begin
      FillChar(WndClass, SizeOf(TWndClass), #0);
      lpfnWndProc := @UtilityProc;
      lpszClassName := PChar(WindowClass);
      hInstance := MainInstance;
      if Windows.RegisterClass(WndClass) = 0 then
        RaiseLastWin32Error;
    end;
  CreationWindow := Self;
  try
    CreateWindow(PChar(WindowClass), PChar(WindowName), 0, 0, 0, 0, 0, 0, 0, 0, nil);
  except
    CreationWindow := nil;
  end;
  if FHandle = 0 then
    RaiseLastWin32Error;
end;

destructor TUtilityWindow.Destroy;
begin
  if FHandle <> 0 then
    DestroyWindow(FHandle);
end;

{ TGlobalData }

function GlobalDataExists(const Name: string): Boolean;
var
  Mutex: THandle;
begin
  Mutex := CreateMutex(nil, False, PChar('Mutex' + Name));
  Result := GetLastError = ERROR_ALREADY_EXISTS;
  CloseHandle(Mutex);
end;

constructor TGlobalData.Create(const Name: string; Size: Integer);
begin
  FName := Name;
  FSize := Size;
  FMap := CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0, FSize,
    PChar('Map' + Name));
  if FMap <> 0 then
  begin
    FCreator := GetLastError <> ERROR_ALREADY_EXISTS;
    FMutex := CreateMutex(nil, False, PChar('Mutex' + Name));
    if FMutex = 0 then
      raise Exception.Create(SMutexCreateError)
  end
  else
    raise Exception.Create(SMapppingCreateError);
end;

destructor TGlobalData.Destroy;
begin
  UnLock;
  if FMap <> 0 then
    CloseHandle(FMap);
  if FMutex <> 0 then
    CloseHandle(FMutex);
end;

function TGlobalData.GetData: Pointer;
begin
  if FLocked then
    Result := FData
  else
    raise Exception.Create(SNotLocked);
end;

procedure TGlobalData.Lock;
begin
  if not FLocked then
  begin
    WaitForSingleObject(FMutex, INFINITE);
    FData := MapViewOfFile(FMap, FILE_MAP_ALL_ACCESS, 0, 0, FSize);
    FLocked  := True;
  end;
end;

procedure TGlobalData.UnLock;
begin
  if FLocked then
  begin
    UnmapViewOfFile(FData);
    ReleaseMutex(FMutex);
    FLocked  := False;
  end;
end;

{ TMemoryMappedFile }

constructor TMemoryMappedFile.Create(const AFileName: string);
begin
  FFileName := AFileName;
  FFile := FileOpen(FFileName, fmOpenRead);
  if FFile <> 0 then
  begin
    FMap := CreateFileMapping(FFile, nil, PAGE_READONLY, 0, 0, nil);
    if FMap <> 0 then
    begin
      FViewStart := MapViewOfFile(FMap, FILE_MAP_READ, 0, 0, 0);
      if Assigned(FViewStart) then
      begin
        FViewEnd := FViewStart;
        Inc(FViewEnd, GetFileSize(FFile, nil) + 1);
      end
      else
        raise EFileMappingError.Create(SViewMapError)
    end
    else
      raise EFileMappingError.Create(SMapppingCreateError)
  end
  else
    raise EFileMappingError.Create(SFileOpenError);
end;

destructor TMemoryMappedFile.Destroy;
begin
 if Assigned(FViewStart) then
   UnmapViewOfFile(FViewStart);
 if FMap <> 0 then
   CloseHandle(FMap);
 if FFile <> 0 then
   CloseHandle(FFile);
end;

{ TBasePipe }

const
  PIPE_BUFFER  = 1024*4;
  PIPE_TIMEOUT = 5000;

constructor TBasePipe.Create(const AName: string; AMode: TPipeMode);
begin
  inherited Create;
  FName := AName;
  FMode := AMode;
end;

function TBasePipe.Read(var Buffer; Count: LongWord): LongWord;
begin
  if FConnected then
    if FMode = pmRead then
      ReadFile(FHandle, Buffer, Count, Result, nil)
    else
      raise Exception.Create(SInvalidMode)
  else
    raise Exception.Create(SNotConnected);
end;

function TBasePipe.Write(const Buffer; Count: LongWord): LongWord;
begin
  if FConnected then
    if FMode = pmWrite then
      WriteFile(FHandle, Buffer, Count, Result, nil)
    else
      raise Exception.Create(SInvalidMode)
  else
    raise Exception.Create(SNotConnected);
end;

constructor TServerPipe.Create(const AName: string; AMode: TPipeMode);
const
  Modes: array [TPipeMode] of LongWord = (PIPE_ACCESS_INBOUND, PIPE_ACCESS_OUTBOUND);
begin
  inherited Create(AName, AMode);
  FHandle := CreateNamedPipe(PChar('\\.\pipe\' + FName), Modes[FMode], 0,
    PIPE_UNLIMITED_INSTANCES, PIPE_BUFFER, PIPE_BUFFER, PIPE_TIMEOUT, nil);
  if FHandle = INVALID_HANDLE_VALUE then
    RaiseLastWin32Error;
end;

destructor TServerPipe.Destroy;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
  begin
    Disconnect;
    CloseHandle(FHandle);
  end;
  inherited Destroy;
end;

procedure TServerPipe.Connect;
begin
  if not FConnected then
  begin
    FConnected := ConnectNamedPipe(FHandle, nil);
    if not FConnected then
    begin
      FConnected := GetLastError = ERROR_PIPE_CONNECTED;
      if not FConnected then
        RaiseLastWin32Error;
    end;
  end;
end;

procedure TServerPipe.Disconnect;
begin
  if FConnected then
  begin
    FlushFileBuffers(FHandle);
    DisconnectNamedPipe(FHandle);
    FConnected := False;
  end;
end;

{ TClientPipe }

destructor TClientPipe.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TClientPipe.Connect;
const
  Modes: array [TPipeMode] of LongWord = (GENERIC_READ, GENERIC_WRITE);
var
  ClientPipeName: string;
begin
    if RemoteMachine <> '' then
      ClientPipeName := '\\' + FRemoteMachine + '\pipe\' + FName
    else
      ClientPipeName := '\\.\pipe\' + FName;
    if WaitNamedPipe(PChar(ClientPipeName), PIPE_TIMEOUT) then
    begin
      FHandle := CreateFile(PChar(ClientPipeName), Modes[FMode], 0, nil,
        OPEN_EXISTING, 0, 0);
      if Handle <> INVALID_HANDLE_VALUE then
        FConnected := True
      else
      begin
        FHandle := 0;
        RaiseLastWin32Error;
      end;
    end;
end;

procedure TClientPipe.Disconnect;
begin
  if FConnected then
  begin
    CloseHandle(FHandle);
    FHandle := 0;
    FConnected := False;
  end;
end;

{ TPipeThread }

constructor TPipeThread.Create(const Params: TPipeThreadParams; PipeProc: TPipeProc);
begin
  FParams := Params;
  FParams.Thread := Self;
  FPipeProc := PipeProc;
  inherited Create(False);
end;

destructor TPipeThread.Destroy;
begin
  with FParams do
  begin
    Pipe.Free;
    if Assigned(Instance) then
      PInteger(Instance)^ := 0;
  end;
  inherited Destroy;
end;

procedure TPipeThread.Execute;
begin
  FreeOnTerminate := True;
  while not Terminated do
    FPipeProc(FParams);
end;

initialization
  InternalMouseHooks.Hook := 0;
finalization
  ReleaseAllHooks;
end.
