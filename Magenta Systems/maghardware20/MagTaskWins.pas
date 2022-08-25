unit MagTaskWins;

{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}

(* 
Unit to handle tasks and windows, relating to starting and stopping programs
This is not a visual component and may not be installed

// Tasks and Windows Unit

// Copyright by Angus Robertson, Magenta Systems Ltd, England
// delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/

// Designed for Delphi 4 and later, tested under Windows 95, 98 and NT 4.0, W2K, XP
// not knowlingly using anything not compatible with Delphi 2 or 3,

Includes Window List Component 1.5 by Jerry Ryle (gryle@calpoly.edu)
Modified by Angus, 4th April 1999 to correct threadid being returned
  instead of processid, now get both

Includes Process List, from usenet, converted to build list of all processes

Includes various Shell related functions to run programs and get process
and windows ids for running programs.

Warning - under WinNT process list needs PSAPI.DLL (from SDK)

Release 1.2
Added GetExeDesc (from version info)

18 May 2000
Added fileShellOpenEx

8 Nov 2000
Set current directory path in StartExe

20 June 2001
Fixed StartExe to minimise and not hide window

9 July 2001
Changed StartExe again for both minimum and hide, not backward compatible

27 Dec 2001
Added CheckExePID

2 April 2002
Added TermExe, CheckExe, CloseExe, TermPID

19 August 2002
Removed Forms, use Nil for owner

16 August 2002
Added WinClass to TWindowObject

30 Dec 2004 - added  GetWorkingSetSize

12 Aug 2005 - added GetConsoleOutputWait

25 July 2008 - renamed unit MagTaskWins so it can be found more easily
               unicode support for Delphi 2009
               WinExecAndWait32 passes string not pointer

20 Oct 2008   - works on Delphi 7/2007 again
                TStartupInfoW missing from D7 and CreateProcessW wrong  

*)


interface

uses
  Windows, Messages, SysUtils, Classes, TlHelp32, Psapi, ShellAPI, MagSubs1 ;

type

  TWindowObject = record
                    WinHandle  : HWnd;    {Window Handle}
                    WinCaption : String;  {Window Caption Text (If any)}
                    ProcessID  : DWord;   {Process the window belongs to}
                    IsVisible  : Boolean; {Is the window visible?}
                    IsEnabled  : Boolean; {Is the window enabled for mouse/keyboard input?}
                    IsIconic   : Boolean; {Is the window minimized?}
                    WindowRect : TRect;   {Window Dimensions}
                    ThreadId   : DWord; {thread that created the window - Angus }
                    WinClass   : String;  {Window Class (If any)}
                    {Add more properties here if you like,
                     then fill them in at the WindowCallback
                     function.}
                  end;
  PTWindowObject = ^TWindowObject;

  TProcessObject = record
                    ProcessID: DWORD;       // this process
                    DefaultHeapID: DWORD;
                    ModuleID: DWORD;        // associated exe
                    CountThreads: DWORD;
                    ParentProcessID: DWORD; // this process's parent process
                    PriClassBase: Longint;  // Base priority of process's threads
                    Flags: DWORD;
                    ExeFile: string ;       // Path
                end ;
  PTProcessObject = ^TProcessObject;

    _STARTUPINFOW = record  // misssing from Delphi 7
    cb: DWORD;
    lpReserved: PWideChar;
    lpDesktop: PWideChar;
    lpTitle: PWideChar;
    dwX: DWORD;
    dwY: DWORD;
    dwXSize: DWORD;
    dwYSize: DWORD;
    dwXCountChars: DWORD;
    dwYCountChars: DWORD;
    dwFillAttribute: DWORD;
    dwFlags: DWORD;
    wShowWindow: Word;
    cbReserved2: Word;
    lpReserved2: PByte;
    hStdInput: THandle;
    hStdOutput: THandle;
    hStdError: THandle;
  end;
  TStartupInfoW = _STARTUPINFOW;



// build list of windows
  TWindowList = class(TComponent)
  private
    WindowLst : TList;
    FCount : Integer;
  protected
    Function GetAWindow(Index : Integer) : TWindowObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    Procedure Refresh;
    Property Windows[Index : Integer]: TWindowObject read GetAWindow;
    Property Count : Integer read FCount;
  published
    { Published declarations }
  end;

// build list of processes
  TProcessList = class(TComponent)
  private
    ProcessLst : TList;
    FCount : Integer;
    FOnlyExe : boolean ;
  protected
    Function GetAProcess(Index : Integer) : TProcessObject;
    procedure GetWin9x ;
    procedure GetWinNT ;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;

    Procedure Refresh;
    Property Process[Index : Integer]: TProcessObject read GetAProcess;
    Property Count : Integer read FCount;
    Property OnlyExe : boolean read FOnlyExe write FOnlyExe ;
  published
    { Published declarations }
  end;

// various functions for running programs and checking if they are running

function WinExecAndWait32(const aCmdLine: String; Visibility: Word): integer;
function StartExe (const aCmdLine, aWorkDir: String;
                                        ShowState: Word): TProcessInformation ;
function fileExec(const aCmdLine, aWorkDir: String; ShowState: Word;
                                                aWait: Boolean): Boolean ;
function GetExePID (const AppName: string): DWORD ;
function GetPIDWin (PID: DWORD): DWORD ;
function GetExeWin (const AppName: string): DWORD ;
function GetExeDesc (const AppName: string): string ;
function fileShellOpenEx (aFile: String; var PID: longword): boolean ;
function fileGetOpen (aFile: String; var ExeName: string): boolean ;
//function fileShellOpenWait(const FileName, Params, StartDir: string;
//                                InitialState: Integer): Integer;
function CheckExePID (PID: DWORD): boolean ;
function TermExe (hProcess: THandle; exitcode: integer): boolean ;
function CheckExe (hProcess: THandle): boolean ;
function CloseExe (PID: DWORD): boolean ;
function TermPID (PID: DWORD; exitcode: integer): boolean ;
function GetWorkingSetSize : DWORD;
procedure GetConsoleOutputWait (const CommandLine: string; const WaitSecs: integer;
                                                      var Output : TStringList);

// 20 Oct 2008 D7 version has nonW startup
function CreateProcessWW(lpApplicationName: PWideChar; lpCommandLine: PWideChar;
  lpProcessAttributes, lpThreadAttributes: PSecurityAttributes;
  bInheritHandles: BOOL; dwCreationFlags: DWORD; lpEnvironment: Pointer;
  lpCurrentDirectory: PWideChar; const lpStartupInfo: TStartupInfoW;
  var lpProcessInformation: TProcessInformation): BOOL; stdcall;
function CreateProcessWW; external kernel32 name 'CreateProcessW';

implementation

{Note that this function is not a member of WindowList.
 Therefore, the list to be filled needs to be passed
 as a pointer. Note that this is passed as a VAR. if you
 don't do this, bad things happen in memory.}

Function WindowCallback(WHandle : HWnd; Var Parm : Pointer) : Boolean; stdcall;
{This function is called once for each window}
Var  Buffer: array[0..255] of WideChar ;
     MyLongWord : DWord ;
     MyWindowPtr : ^TWindowObject;
     MyLen : integer ;
begin
    New(MyWindowPtr);

    {Window Handle (Passed by the enumeration)}
    MyWindowPtr.WinHandle := WHandle;

    {Window title caption text}
    GetWindowTextW (WHandle,Buffer,255);    // unicode
    MyWindowPtr.WinCaption := Buffer;

    {Window class text}
    MyLen := GetClassNameW (WHandle,Buffer,255);    // unicode
    if MyLen <> 0 then
        MyWindowPtr.WinClass := Buffer
    else
        MyWindowPtr.WinClass := '' ;

    {Process ID - Angus function returned thread not process }
    MyLongWord := 0;
    MyWindowPtr.ThreadID := GetWindowThreadProcessId(WHandle,@MyLongWord);
    MyWindowPtr.ProcessID := MyLongWord ;

    {Visiblity}
    MyWindowPtr.IsVisible := IsWindowVisible(WHandle);

    {Enabled}
    MyWindowPtr.IsEnabled := IsWindowEnabled(WHandle);

    {Iconic}
    MyWindowPtr.IsIconic := IsIconic(WHandle);

    {Window Dimensions}
    MyWindowPtr.WindowRect := Rect(0,0,0,0);
    GetWindowRect(WHandle,MyWindowPtr.WindowRect);

    {Add the structure to the list. Do not dereference Parm...
     once again, bad things happen.}
    TList(Parm).Add(MyWindowPtr);
    Result := True; {Everything's okay. Continue to enumerate windows}
end;

constructor TWindowList.Create(AOwner: TComponent);
begin
    inherited;
    WindowLst := TList.Create;
    try

{Thanks Serge, I should've done this from the start :)
 Sloppy me. }
        if not ( csDesigning in ComponentState ) then
        begin
            EnumWindows(@WindowCallback,Longint(@WindowLst));
            FCount := WindowLst.Count;
        end
    else
        FCount := 0;
    except
        FCount := 0;
    end ;
end;

destructor TWindowList.Destroy;
    var I : Integer;
begin
    try
        if WindowLst.Count > 0 then
        begin
            for I := 0 to (WindowLst.Count - 1) do
                Dispose(PTWindowObject(WindowLst[I]));
        end;
        WindowLst.Free;
    except
    end ;
    inherited;
end;

procedure TWindowList.Refresh;
begin
    try
        WindowLst.Clear; {Clear the list!}
        EnumWindows (@WindowCallback,Longint(@WindowLst));
        FCount := WindowLst.Count;
    except
        FCount := 0;
    end ;
end;

function TWindowList.GetAWindow(Index : Integer) : TWindowObject;
begin
    Result := PTWindowObject(WindowLst[Index])^;
end;


// ==============================================================================================

constructor TProcessList.Create (AOwner: TComponent) ;
begin
    inherited;
    ProcessLst := TList.Create;
    FOnlyExe := true ;
    if not ( csDesigning in ComponentState ) then
        Refresh
    Else
        FCount := 0;
end;

destructor TProcessList.Destroy;
    var I : Integer;
begin
    If ProcessLst.Count > 0 Then
    Begin
        For I := 0 To (ProcessLst.Count - 1) Do
            Dispose(PTProcessObject(ProcessLst[I]));
    End;
    ProcessLst.Free;
    inherited;
end;

procedure TProcessList.GetWin9x ;
var
    hSnapShot: THandle;
    pe: TProcessEntry32W;  // unicode
    res: BOOL;
    ProcObj: PTProcessObject;
    FileName: string ;
begin
    hSnapShot := CreateToolhelp32Snapshot (TH32CS_SNAPPROCESS, 0);
    if integer (hSnapShot) = -1 then exit ;
    ZeroMemory (@pe, SizeOf (pe)) ;
    pe.dwSize := SizeOf (pe);
    res := Process32FirstW (hSnapShot, pe);
    while res do
    begin
        FileName := pe.szExeFile ;
        if (NOT FOnlyExe) or
                    (CompareText (ExtractFileExt (FileName), '.exe') = 0) then
        begin
            New(ProcObj);
            with ProcObj^ do
            begin
                ProcessID := pe.th32ProcessID ;
                DefaultHeapID := pe.th32DefaultHeapID ;
                ModuleID := pe.th32ModuleID ;
                CountThreads := pe.cntThreads ;
                ParentProcessID := pe.th32ParentProcessID ;
                PriClassBase := pe.pcPriClassBase ;
                Flags := pe.dwFlags ;
                ExeFile := FileName ;
            end ;
            ProcessLst.Add (ProcObj) ;
        end ;
        res := Process32NextW (hSnapShot, pe);
    end;
    CloseHandle (hSnapShot);
end ;

procedure TProcessList.GetWinNT ;
var
    Processes, Modules: array [0..1023] of DWORD;
    cbNeededP,cbNeededM: Cardinal;
    i,j: integer;
    hProcess: THandle;
    szProcessName: array [0..MAX_PATH-1] of WideChar;
    ProcObj: PTProcessObject;
    FileName: string ;
begin
    if EnumProcesses (@Processes, SizeOf(Processes), cbNeededP) then
    begin
        for i := 0 to ((cbNeededP div SizeOf (DWORD)) - 1) do
        begin
            hProcess := OpenProcess (PROCESS_QUERY_INFORMATION or
                                    PROCESS_VM_READ, FALSE, Processes [i] );
            if hProcess <> 0 then
            begin
                if EnumProcessModules (hProcess, @Modules,
                                 sizeof (Modules), cbNeededM) then
                begin
                    for j := 0 to ((cbNeededM div SizeOf (DWORD)) - 1) do
                    begin
                        if GetModuleFileNameExW (hProcess, Modules [j],                // unicode
                                 szProcessName, Length(szProcessName)) > 0 then
                        begin
                            FileName := szProcessName ;
                            if (NOT FOnlyExe) or (CompareText (ExtractFileExt
                                                 (FileName), '.exe') = 0) then
                            begin
                                New(ProcObj);
                                with ProcObj^ do
                                begin
                                    ProcessID := Processes [i] ;
                                    DefaultHeapID := 0 ;
                                    ModuleID := Modules [j] ;
                                    CountThreads := 0 ;
                                    ParentProcessID := 0 ;
                                    PriClassBase := 0 ;
                                    Flags := 0 ;
                                    ExeFile := FileName ;
                                end ;
                                ProcessLst.Add (ProcObj) ;
                            end ;                                
                        end;
                    end;
                    CloseHandle (hProcess);
                end;
            end;
        end ;
    end ;
end ;


procedure TProcessList.Refresh;
begin
    ProcessLst.Clear; {Clear the list!}
    FCount := 0 ;
    if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
        GetWin9x
    else if Win32Platform = VER_PLATFORM_WIN32_NT then
        GetWinNT ;
    FCount := ProcessLst.Count;
end;

function TProcessList.GetAProcess (Index: Integer): TProcessObject;
begin
  Result := PTProcessObject (ProcessLst[Index])^;
end;

// ==============================================================================================

function WinExecAndWait32(const aCmdLine: String; Visibility: Word): integer;
var
    Msg: TMsg;
    lpExitCode : longword ;
    StartupInfo: TStartupInfoW;
    ProcessInfo: TProcessInformation;
    lpCommandLine: WideString;  // Unicode - CreateProcessW may modify this string, don't use constant
begin
    lpCommandLine := aCmdLine ;
    FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
    with StartupInfo do
    begin
        cb := SizeOf(TStartupInfo);
        dwFlags := STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
        wShowWindow := visibility;
                 {you could pass sw_show or sw_hide as parameter}
    end;
// 20 Oct 2008 D7 has bad declaration 
    if CreateProcessWW (nil,PWideChar (lpCommandLine),nil, nil, False,
        NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInfo) then
    begin
        repeat
            while PeekMessage(Msg, 0, 0, 0, pm_Remove) do
            begin
                if Msg.Message = wm_Quit then Halt(Msg.WParam);
                TranslateMessage(Msg);
                DispatchMessage(Msg);
            end;
            GetExitCodeProcess(ProcessInfo.hProcess,lpExitCode);
        until lpExitCode<>Still_Active;

        with ProcessInfo do {not sure this is necessary but seen in in some code elsewhere}
        begin
            CloseHandle(hThread);
            CloseHandle(hProcess);
        end;
        result := 0; {sucess}
    end
    else
        result := GetLastError;
            {error occurs during CreateProcess see help for details}
end;

// ==============================================================================================

// run a program and return immediately with all process information

function StartExe (const aCmdLine, aWorkDir: String;
                                        ShowState: Word): TProcessInformation ;
var
    StartupInfo : TStartupInfoW;
    ProcessInfo : TProcessInformation;
    lpCommandLine: WideString;  // Unicode - CreateProcessW may modify this string, don't use constant
    lpCurrentDirectory: WideString ; // Unicode
    lpDirPtr: PWideChar ;           // Unicode, nil needed for blank directory
begin
  {setup the startup information for the application }
    lpCommandLine := aCmdLine ;
    lpCurrentDirectory := aWorkDir ;
    lpDirPtr := Nil ;
    if Length (lpCurrentDirectory) > 2 then lpDirPtr := @lpCurrentDirectory [1] ;
    FillChar (StartupInfo, SizeOf (TStartupInfo), 0);
    FillChar (Result, SizeOf (Result), 0);
    with StartupInfo do
    begin
        cb := SizeOf (TStartupInfo);
        dwFlags := STARTF_FORCEONFEEDBACK ;
        if ShowState <> 0 then
        begin
            dwFlags := dwFlags OR STARTF_USESHOWWINDOW ;
            wShowWindow := ShowState ;  //  SW_SHOWNORMAL, SW_SHOWMINIMIZED or SW_HIDE
        end ;
    end;
    if CreateProcessWW (nil, PWideChar (lpCommandLine), nil, nil, False,
             NORMAL_PRIORITY_CLASS, nil, lpDirPtr, StartupInfo,
                                 ProcessInfo) then Result := ProcessInfo ;
end;

// ==============================================================================================

// run a program and optionally return immediately

function fileExec(const aCmdLine, aWorkDir: String; ShowState: Word;
                                                aWait: Boolean): Boolean ;
var
    ProcessInfo : TProcessInformation;
begin
    result := false ;
    ProcessInfo := StartExe (aCmdLine, aWorkDir, ShowState) ;
    if ProcessInfo.hProcess = 0 then exit ;
    result := true ;
    if NOT aWait then exit ;
    WaitForInputIdle (ProcessInfo.hProcess, INFINITE);     // handle not PID
    WaitForSingleObject (ProcessInfo.hProcess, INFINITE);
end ;

// ==============================================================================================

// run a program, URL or document, returning process handle

function fileShellOpenEx (aFile: String; var PID: longword): boolean ;
var
    shellinfo: TShellExecuteInfoW ;       // unicode
    WideFileName: WideString ;            // unicode
begin
    WideFileName := aFile ;
    FillChar (shellinfo, SizeOf (shellinfo), 0);
    PID := 0 ;
    with shellinfo do
    begin
        cbSize := SizeOf (TShellExecuteInfo) ;
        fmask := SEE_MASK_NOCLOSEPROCESS OR
                         SEE_MASK_FLAG_DDEWAIT OR  SEE_MASK_FLAG_NO_UI ;
        Wnd := hInstance ;
        lpVerb := 'open' ;
        lpFile := PWideChar(WideFileName) ;
        nShow :=  SW_NORMAL ;
    end ;
    result := NOT ShellExecuteExW (@shellinfo) ;
    if NOT result then PID := shellinfo.hProcess ;
end;

// ==============================================================================================

// run a program, URL or document, returning process handle

function fileGetOpen (aFile: String; var ExeName: string): boolean ;
var
    NewFile: PWideChar ;
    retcode: integer ;
    WideFileName: WideString ;   // unicode
begin
    ExeName := '' ;
    WideFileName := aFile ;
    NewFile := nil ;
    result := true ;
    retcode := FindExecutableW (PWideChar(WideFileName), Nil, @NewFile) ;    // unicode
    if (retcode > 32) and (NewFile <> Nil) then
    begin
        result := false ;
        ExeName := NewFile ;
    end ;
end;

// ==============================================================================================

// gets the processid for a running application, zero means not running

function GetExePID (const AppName: string): DWORD ;
var
    ProcList: TProcessList ;
    item: integer ;
    ProcFile: string ;
begin
    result := 0 ;
    ProcList := TProcessList.Create (Nil) ;
    try
        if ProcList.Count = 0 then exit ;
        for item := 0 to ProcList.Count - 1 do
        begin
            ProcFile := ProcList.Process [item].ExeFile ;
            if (pos ('\', AppName) = 0) then
                                    ProcFile := ExtractFileName (ProcFile) ;
            if CompareText (AppName, ProcFile) = 0 then
            begin
                result := ProcList.Process [item].ProcessID ;
                exit ;
            end ;
        end ;
    finally
        if Assigned (ProcList) then ProcList.Destroy ;
    end ;
end ;


// ==============================================================================================

// check if processid is for a running application

function CheckExePID (PID: DWORD): boolean ;
var
    ProcList: TProcessList ;
    item: integer ;
begin
    result := false ;
    ProcList := TProcessList.Create (Nil) ;
    try
        if ProcList.Count = 0 then exit ;
        for item := 0 to Pred (ProcList.Count) do
        begin
            if PID = ProcList.Process [item].ProcessID then
            begin
                result := true ;
                exit ;
            end ;
        end ;
    finally
        if Assigned (ProcList) then ProcList.Destroy ;
    end ;
end ;


// ==============================================================================================


// get the windows handle for a process identifier

function GetPIDWin (PID: DWORD): DWORD ;
var
    WinList: TWindowList ;
    item: integer ;
begin
    result := 0 ;
    WinList := TWindowList.Create (Nil) ;
    try
        if WinList.Count = 0 then exit ;
        for item := 0 to WinList.Count - 1 do
        begin
            if (WinList.Windows [item].ProcessId = PID) then
            begin
                if (WinList.Windows [item].IsIconic) or
                                (WinList.Windows [item].IsVisible) then
                begin
                    Result := WinList.Windows [item].WinHandle ;
                    exit ;
                end ;
            end ;
        end ;
    finally
        if Assigned (WinList) then WinList.Destroy ;
    end ;
end ;

// ==============================================================================================

// get the windows handle for a running application, zero means not running

function GetExeWin (const AppName: string): DWORD ;
var
    procid: DWORD ;
begin
    result := 0 ;
    procid := GetExePID (AppName) ;
    if procid = 0 then exit ;
    result := GetPIDWin (procid) ;
end ;

// ==============================================================================================

// This function converts a pointer to a wide char string into a pascal string

function WideCharToStr(WStr: PWChar; Len: Integer): AnsiString;  // unicode
begin
    if Len = 0 then Len := -1 ;
    Len := WideCharToMultiByte (CP_ACP, 0, WStr, Len, nil, 0, nil, nil) ;
    SetLength (Result, Len) ;
    WideCharToMultiByte (CP_ACP, 0, WStr, Len, PAnsiChar(Result), Len, nil, nil) ;
end;

// ==============================================================================================

// This function gets the program description from the string resources
// 22 July 2008 call version in magsubs1 to avoid duplicating code 

function GetExeDesc (const AppName: string): string ;
begin
    result := GetFileVerInfo (AppName, 'FileDescription') ;
end ;

// ==============================================================================================

// terminate a process using handle from CreateProcess or OpenProcess, not Process ID
// this is brutal, crashes program

function TermExe (hProcess: THandle; exitcode: integer): boolean ;
begin
    result := TerminateProcess (hProcess, exitcode) ;
    CloseHandle (hProcess) ;
end ;

// ==============================================================================================

// check if program running using handle from CreateProcess or OpenProcess, not Process ID

function CheckExe (hProcess: THandle): boolean ;
var
    lpExitCode: longword ;
begin
    GetExitCodeProcess (hProcess, lpExitCode) ;
    result := (lpExitCode = Still_Active) ;
end ;

// ==============================================================================================

// close a process using the Process ID
// the proper way to close a program,

function CloseExe (PID: DWORD): boolean ;
var
    winhandle: THandle ;
begin
    result := false ;
    winhandle := GetPIDWin (PID) ;
    if winhandle = 0 then exit ;
    PostMessage (winhandle, WM_CLOSE, 0, 0) ;        // or WM_QUIT
    result := true ;
end ;

// ==============================================================================================

// Terminate a process using the Process ID
// this is brutal, crashes program

function TermPID (PID: DWORD; exitcode: integer): boolean ;
var
    hProcess: THandle ;
begin
    result := false ;
    hProcess := OpenProcess (PROCESS_TERMINATE, false, PID) ;
    if hProcess = 0 then exit ;
    result := TerminateProcess (hProcess, exitcode) ;
    CloseHandle (hProcess) ;
end ;

// ==============================================================================================


function GetWorkingSetSize : DWORD;
var
    MemCounters   : PROCESS_MEMORY_COUNTERS;  // Defined in PsAPI
    ProcessHandle : THandle;
begin
    ProcessHandle := OpenProcess(PROCESS_ALL_ACCESS, FALSE, GetCurrentProcessID);
    FillChar(MemCounters, SizeOf(MemCounters), 0);
    MemCounters.cb := SizeOf(MemCounters);
    GetProcessMemoryInfo(ProcessHandle, @MemCounters, SizeOf(MemCounters));
    CloseHandle(ProcessHandle);
    Result := MemCounters.WorkingSetSize;
end;

// ==============================================================================================

procedure GetConsoleOutputWait (const CommandLine: string; const WaitSecs: integer;
                                                      var Output : TStringList);
var
    SA: TSecurityAttributes;
    SI: TStartupInfoW;
    PI: TProcessInformation;
    StdOutFile, AppProcess, AppThread : THandle;
    RootDir, StdOutFileName: WideString;  // Unicode 
    ret, exitcode, waitticks: integer ;
    lpCommandLine: WideString;  // Unicode - CreateProcessW may modify this string, don't use constant
    pCurrentDirectory: WideString ; // Unicode
const
  FUNC_NAME = 'GetConsoleOuput';
begin
    StdOutFile:=0;
    AppProcess:=0;
    AppThread:=0;
    lpCommandLine := CommandLine ; // unicode
    try

    // Initialize dirs
    RootDir:=ExtractFilePath(ParamStr(0));
    pCurrentDirectory:=ExtractFilePath(CommandLine);

    // Check WorkDir
    if not (FileSearch(ExtractFileName(CommandLine),pCurrentDirectory)<>'') then
      pCurrentDirectory:=RootDir;

    // Initialize output file security attributes
    FillChar(SA,SizeOf(SA),#0);
    SA.nLength:=SizeOf(SA);
    SA.lpSecurityDescriptor:=nil;
    SA.bInheritHandle:=True;

    // Create Output File
    StdOutFileName:=pCurrentDirectory+'output.tmp';
    StdOutFile:=CreateFileW(PWideChar(StdOutFileName),
                   GENERIC_READ or GENERIC_WRITE,
                   FILE_SHARE_READ or FILE_SHARE_WRITE,
                   @SA,
                   CREATE_ALWAYS, // Always create it
                   FILE_ATTRIBUTE_TEMPORARY or // Will cache in memory
                                               // if possible
                   FILE_FLAG_WRITE_THROUGH,
                   0);

    // Check Output Handle
    if StdOutFile = INVALID_HANDLE_VALUE then
      raise Exception.CreateFmt('Function %s() failed!' + #10#13 +
        'Command line = %s',[FUNC_NAME,CommandLine]);

    // Initialize Startup Info
    FillChar(SI,SizeOf(SI),#0);
    with SI do begin
      cb:=SizeOf(SI);
      dwFlags:=STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      wShowWindow:=SW_HIDE;
      hStdInput:=GetStdHandle(STD_INPUT_HANDLE);
      hStdError:=StdOutFile;
      hStdOutput:=StdOutFile;
    end;

    // Create the process
    if CreateProcessWW (nil, PWideChar(lpCommandLine), nil, nil,
                     True, 0, nil, PWideChar(pCurrentDirectory), SI, PI) then
    begin
      waitticks := WaitSecs * 1000 ;
      if waitticks < 2000 then waitticks := 2000 ;
      ret := WaitForSingleObject(PI.hProcess, waitticks);
      AppProcess:=PI.hProcess;
      AppThread:=PI.hThread;
      exitcode := 0 ;
      if ret = WAIT_TIMEOUT then
      begin
          TerminateProcess (AppProcess, exitcode) ;
          Sleep (2000);  // wait for program to die 
      end;
    end
    else
      raise Exception.CreateFmt('CreateProcess() in function %s() failed!'
                   + #10#13 + 'Command line = %s',[FUNC_NAME,CommandLine]);

    CloseHandle(StdOutFile);
    StdOutFile:=0;

    Output.Clear;
    Output.LoadFromFile (StdOutFileName);
    if ret = WAIT_TIMEOUT then Output.Add ('Abandoned Waiting for Program to Finish') ;

  finally
    // Close handles
    if StdOutFile <> 0 then CloseHandle(StdOutFile);
    if AppProcess <> 0 then CloseHandle(AppProcess);
    if AppThread <> 0 then CloseHandle(AppThread);

    // Delete Output file
    if FileExists(StdOutFileName) then
      SysUtils.DeleteFile(StdOutFileName);
  end;
end;


end.
