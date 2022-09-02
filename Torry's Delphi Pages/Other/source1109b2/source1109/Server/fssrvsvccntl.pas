// *************
// defines
// *************

{.$DEFINE FixExplorerBug}{on some Win2000 systems Explorer doesn't work if
                           interact with Desktop is enabled but
                           SUP won't work (on Win2000) unless this
                           define is turned off}
{.$DEFINE TESTING}{use this for testing Win9X mode on NT}
{$DEFINE ffc_TCPInterfaceExists} {this was introduced in FF1.55 (I think)}

{$I FSDEFINE.INC}

{$IFDEF FFServerDebugLog}

Whilst you * can * have this define On - it makes FSSQL run at a snails pace!!

!! please turn off the FFServerDebugLog define In FSDEFINE.INC

(*   you need to find this line: {$DEFINE FFServerDebugLog}
     and change it so it looks like this: {.$DEFINE FFServerDebugLog}*)

{$ENDIF}

{

Usage:

      1. Define alias and Startup parameters:

         A. Place a Scripts file in the same folder as the
         FFSvc.exe named "FSull.sc$" (see the define
         const FullScriptFileName = 'FFull.sc$';
         below if you want to change it)

         here is a sample FFull.sc$:

         MyAlias=E:\FFData\
         SERVERNAME=MyServer
         MAXRAMPAGES=30
         USELOGIN=FALSE
         USEBROADCASTS=TRUE
         ALLOWENCRYPT=FALSE
         NOAUTOSAVECFG=FALSE
         LASTMSGINTVAL=120000
         ALIVEINTERVAL=120000
         ALIVERETRIES=4
         PRIORITY=NORMAL

         B. Alternately put the standard FF control tables
         in the same folder as the FFSvc.exe:


      2. to Install, run: "FFSvc.exe Install"
      3. to Stop, run: "FFSvc.exe Stop"
      4. to Restart, run: "FFSvc.exe Run"
      5. to Un-Install, run: "FFSvc.exe Uninstall"

      This will get you a multi-protocol FF1 or FF2 Server -
      depending whether you have FF1 or FF2 installed
      when you compile

Multiple NIC support:

      If you use a version of FF1 prior to the version which
      introduced multiple NIC support via const ffc_TCPInterface
      in FFllProt.pas (FF 1.55 I think) then you can get
      multi-protocol support by making the MULTIPLETCPIPSERVERS
      code fix (below)

      then FF1 will support two NICs on TCP/IP
      otherwise - just one (the 1st one bound)

      You'll need to comment out the line:

      ffc_TCPInterface:= -1;

      in function Start: boolean; // FF1

Exceptional Magic Support:

      Exceptional Magic is a system that provides added
      support for exception handling:

          unit file name and line number reports
          Call Stack reports

      this is amazingly helpful in debugging off site code

      the Home Page is: http://dimus.virtualave.net/delphi/excmagic/index.html

      If you have Exceptional Magic then enable the define at the top
      of the unit: UseExceptionMagic

}

Unit fssrvsvccntl;

Interface

Uses Windows,
  SysUtils,
  Classes,
  Messages;

Procedure RunNT;
Procedure RunWin9x;

Var
  IsNT: boolean;
  IsWin2000XP: boolean;
  OSIs: String = '';
  LastError: Integer = 0;
  LastErrorMessage: String = '';

Const
  CExeName: String = 'fssvcserver.exe';
  FullScriptFileName = 'FSSQL.sc$';
  // the exe and needed files are moved to %WinSysDir%\System32SubDir
  // this requires admin priveleges on NT/2000
  // but then, installing a NT Service also requires admin priviledges
  System32SubDir = '\FSSQL10\';
  CServiceName = 'FSSQL';
  CDisplayName = 'FSSQL Server';
  AliasTable = 'TAB0001.FSD';
  ServerInfoTable = 'TAB0003.FSD';
  UserTable = 'TAB0003.FSD';
  IndexTable = 'TAB0002.FSD';

Const
  // Win95 Messages
  WM_SERVICE_CONTROL_STOP = WM_USER;
  WM_SERVICE_CONTROL_PAUSE = WM_USER + 1;
  WM_SERVICE_CONTROL_CONTINUE = WM_USER + 2;
  WM_SERVICE_CONTROL_INTERROGATE = WM_USER + 3;
  SERVICE_ACCEPTED_MESSAGE = 8;
  SERVICE_NOT_INSTALLED = 10;
  SERVICE_WINDOW_NAME = 'FSSQL 1.0';

  FFInterrogate = WM_SERVICE_CONTROL_INTERROGATE;

Var
  FSSQLWindow: HWND = 0;

Const

  AdvAPI32 = 'advapi32.dll';

  SERVICE_KERNEL_DRIVER = $00000001;
  SERVICE_FILE_SYSTEM_DRIVER = $00000002;
  SERVICE_ADAPTER = $00000004;
  SERVICE_RECOGNIZER_DRIVER = $00000008;

  SERVICE_DRIVER =
    (SERVICE_KERNEL_DRIVER Or
    SERVICE_FILE_SYSTEM_DRIVER Or
    SERVICE_RECOGNIZER_DRIVER);

  SERVICE_WIN32_OWN_PROCESS = $00000010;
  SERVICE_WIN32_SHARE_PROCESS = $00000020;
  SERVICE_WIN32 =
    (SERVICE_WIN32_OWN_PROCESS Or
    SERVICE_WIN32_SHARE_PROCESS);

  SERVICE_INTERACTIVE_PROCESS = $00000100;

  SERVICE_TYPE_ALL =
    (SERVICE_WIN32 Or
    SERVICE_ADAPTER Or
    SERVICE_DRIVER Or
    SERVICE_INTERACTIVE_PROCESS);

  //
  // Start Type
  //

  SERVICE_BOOT_START = $00000000;
  SERVICE_SYSTEM_START = $00000001;
  SERVICE_AUTO_START = $00000002;
  SERVICE_DEMAND_START = $00000003;
  SERVICE_DISABLED = $00000004;

  //
  // Error control type
  //
  SERVICE_ERROR_IGNORE = $00000000;
  SERVICE_ERROR_NORMAL = $00000001;
  SERVICE_ERROR_SEVERE = $00000002;
  SERVICE_ERROR_CRITICAL = $00000003;

  //
  // Service State -- for Enum Requests (Bit Mask)
  //
Const
  SERVICE_ACTIVE = 1;
  SERVICE_INACTIVE = 2;
  SERVICE_STATE_ALL = SERVICE_ACTIVE + SERVICE_INACTIVE;

  //
  // Controls
  //
Const
  SERVICE_CONTROL_STOP = 1;
  SERVICE_CONTROL_PAUSE = 2;
  SERVICE_CONTROL_CONTINUE = 3;
  SERVICE_CONTROL_INTERROGATE = 4;
  SERVICE_CONTROL_SHUTDOWN = 5;

  //SERVICE_CONTROL_REBUILD             = 128; added see above

//
// Service State -- for CurrentState
//
Const
  SERVICE_STOPPED = 1;
  SERVICE_START_PENDING = 2;
  SERVICE_STOP_PENDING = 3;
  SERVICE_RUNNING = 4;
  SERVICE_CONTINUE_PENDING = 5;
  SERVICE_PAUSE_PENDING = 6;
  SERVICE_PAUSED = 7;

  //
  // Controls Accepted  (Bit Mask)
  //
Const
  SERVICE_ACCEPT_STOP = 1;
  SERVICE_ACCEPT_PAUSE_CONTINUE = 2;
  SERVICE_ACCEPT_SHUTDOWN = 4;

  //
  // Service Control Manager object specific access types
  //
Const
  SC_MANAGER_CONNECT = $0001;
  SC_MANAGER_CREATE_SERVICE = $0002;
  SC_MANAGER_ENUMERATE_SERVICE = $0004;
  SC_MANAGER_LOCK = $0008;
  SC_MANAGER_QUERY_LOCK_STATUS = $0010;
  SC_MANAGER_MODIFY_BOOT_CONFIG = $0020;

  SC_MANAGER_ALL_ACCESS =
    STANDARD_RIGHTS_REQUIRED Or
    SC_MANAGER_CONNECT Or
    SC_MANAGER_CREATE_SERVICE Or
    SC_MANAGER_ENUMERATE_SERVICE Or
    SC_MANAGER_LOCK Or
    SC_MANAGER_QUERY_LOCK_STATUS Or
    SC_MANAGER_MODIFY_BOOT_CONFIG;

  //
  // Service object specific access type
  //
Const
  SERVICE_QUERY_CONFIG = $0001;
  SERVICE_CHANGE_CONFIG = $0002;
  SERVICE_QUERY_STATUS = $0004;
  SERVICE_ENUMERATE_DEPENDENTS = $0008;
  SERVICE_START = $0010;
  SERVICE_STOP = $0020;
  SERVICE_PAUSE_CONTINUE = $0040;
  SERVICE_INTERROGATE = $0080;
  SERVICE_USER_DEFINED_CONTROL = $0100;

  SERVICE_ALL_ACCESS =
    (STANDARD_RIGHTS_REQUIRED Or
    SERVICE_QUERY_CONFIG Or
    SERVICE_CHANGE_CONFIG Or
    SERVICE_QUERY_STATUS Or
    SERVICE_ENUMERATE_DEPENDENTS Or
    SERVICE_START Or
    SERVICE_STOP Or
    SERVICE_PAUSE_CONTINUE Or
    SERVICE_INTERROGATE Or
    SERVICE_USER_DEFINED_CONTROL);

  //
  // Handle Types
  //
Type
  TSCHandle = THandle;
  PSCHandle = ^TSCHandle;
  TServiceStatusHandle = Integer;

  //
  // Service Status Structure
  //
  PServiceStatus = ^TServiceStatus;
  TServiceStatus =
    Packed Record
    dwServiceType: Integer;
    dwCurrentState: Integer;
    dwControlsAccepted: Integer;
    dwWin32ExitCode: Integer;
    dwServiceSpecificExitCode: Integer;
    dwCheckpoint: Integer;
    dwWaitHint: Integer;
  End;

  //
  // Service Status Enumeration Structure
  //
Type
  PEnumServiceStatusA = ^TEnumServiceStatusA;
  TEnumServiceStatusA =
    Packed Record
    lpServiceName: PChar;
    lpDisplayName: PChar;
    ServiceStatus: TServiceStatus;
  End;

  TEnumServiceStatus = TEnumServiceStatusA;
  PEnumServiceStatus = ^TEnumServiceStatus;

  //
  // Structures for the Lock API functions
  //
Type
  TSCLock = POINTER;

  PQueryServiceLockStatus = ^TQueryServiceLockStatus;
  TQueryServiceLockStatus =
    Packed Record
    fIsLocked: Integer;
    lpLockOwner: PChar;
    dwLockDuration: Integer;
  End;

  //
  // Query Service Configuration Structure
  //
  PQueryServiceConfig = ^TQueryServiceConfig;
  TQueryServiceConfig =
    Packed Record
    dwServiceType: Integer;
    dwStartType: Integer;
    dwErrorControl: Integer;
    lpBinaryPathName: PChar;
    lpLoadOrderGroup: PChar;
    dwTagID: Integer;
    lpDependencies: PChar;
    lpServiceStartName: PChar;
    lpDisplayName: PChar;
  End;

  //
  // Function Prototype for the Service Main Function
  //
Type
  TServiceMainFunction = Procedure(dwNumServicesArgs: Integer; Var lpServiceArgVectors: PChar); stdcall;

  //
  // Service Start Table
  //
Type
  PServiceTableEntry = ^TServiceTableEntry;
  TServiceTableEntry =
    Record
    lpServiceName: PChar;
    lpServiceProc: TServiceMainFunction;
  End;

  //
  // Prototype for the Service Control Handler Function
  //
Type
  THandlerFunction = Procedure(dwControl: Integer); stdcall;

  ///////////////////////////////////////////////////////////////////////////
  // API Function Prototypes
  ///////////////////////////////////////////////////////////////////////////

Function ChangeServiceConfig
  (
  hService: TSCHandle;
  dwServiceType,
  dwStartType,
  dwErrorControl: Integer;
  lpBinaryPathName,
  lpLoadOrderGroup: PChar;
  Var dwTagID: Integer;
  lpDependencies,
  lpServiceStartName,
  lpPassword,
  lpDisplayName: PChar
  ): LONGBOOL; stdcall;

Function CloseServiceHandle
  (
  hSCObject: TSCHandle
  ): LONGBOOL; stdcall;

Function ControlService
  (
  hService: TSCHandle;
  dwControl: Integer;
  Var lpServiceStatus: tServiceStatus
  ): LONGBOOL; stdcall;

Function CreateService
  (
  hSZManager: TSCHandle;
  lpServiceName,
  lpDisplayName: PChar;
  dwDesiredAccess,
  dwServiceType,
  dwStartType,
  dwErrorControl: Integer;
  lpBinaryPathName,
  lpLoadOrderGroup: PChar;
  dwTagID: PINTEGER;
  lpDependencies,
  lpServiceStartName,
  lpPassword: PChar
  ): TSCHandle; stdcall;

Function DeleteService
  (
  hService: TSCHandle
  ): LONGBOOL; stdcall;

Function EnumDependentServices
  (
  hService: TSCHandle;
  dwServiceState: Integer;
  Var lpServices: TEnumServiceStatus;
  cbBufSize: Integer;
  Var pcbBytesNeeded: Integer;
  Var lpServicesReturned: Integer
  ): LONGBOOL; stdcall;

Function EnumServicesStatus
  (
  hSCManager: TSCHandle;
  dwServiceType,
  dwServiceState: Integer;
  lPServices: POINTER;
  cbBufSize: Integer;
  Var pcbBytesNeeded: Integer;
  Var lpServicesReturned: Integer;
  Var lpResumeHandle: Integer
  ): LONGBOOL; stdcall;

Function GetServiceKeyName
  (
  hSCManager: TSCHandle;
  lpDisplayName,
  lpServiceName: PChar;
  Var lpcchBuffer: Integer
  ): LONGBOOL; stdcall;

Function GetServiceDisplayName
  (
  hSCManager: TSCHandle;
  lpServiceName,
  lpDisplayName: PChar;
  Var lpcchBuffer: Integer
  ): LONGBOOL; stdcall;

Function LockServiceDatabase
  (
  hSCManager: TSCHandle
  ): TSCLock; stdcall;

Function NotifyBootConfigStatus
  (
  BootAcceptable: LONGBOOL
  ): LONGBOOL; stdcall;

Function OpenSCManager
  (
  lpMachineName,
  lpDatabaseName: PChar;
  dwDesiredAccess: Integer
  ): TSCHandle; stdcall;

Function OpenService
  (
  hSCManager: TSCHandle;
  lpServiceName: PChar;
  dwDesiredAccess: Integer
  ): TSCHandle; stdcall;

Function QueryServiceConfig
  (
  hService: TSCHandle;
  lpServiceConfig: PQueryServiceConfig;
  cbBufSize: Integer;
  Var pcbBytesNeeded: Integer
  ): LONGBOOL; stdcall;

Function QueryServiceLockStatus
  (
  hSCManager: TSCHandle;
  Var lpLockStatus: TQueryServiceLockStatus;
  cbBufSize: Integer;
  Var pcbBytesNeeded: Integer
  ): LONGBOOL; stdcall;

(*
function QueryServiceObjectSecurity
  (
    hService               : TSCHandle;
    dwSecurityInformation  : TSecurityInformation;
    var SecurityDescriptor : TSecurityDescriptor;
    cbBufSize              : INTEGER;
    var pcbBytesNeeded     : INTEGER
  ) : LONGBOOL; stdcall;
*)

Function QueryServiceStatus
  (
  hService: TSCHandle;
  Var lpServiceStatus: TServiceStatus
  ): LONGBOOL; stdcall;

Function RegisterServiceCtrlHandler
  (
  lpServiceName: PChar;
  lpHandlerProc: THandlerFunction
  ): TServiceStatusHandle; stdcall;

(*
function SetServiceObjectSecurity
  (
    hService               : TSCHandle;
    dwSecurityInformation  : TSecurityInformation;
    var lpSecurityDescriptor : TSecurityDescriptor
  ) : LONGBOOL; stdcall;
*)

Function SetServiceStatus
  (
  hServiceStatus: TServiceStatusHandle;
  lpServiceStatus: PServiceStatus
  ): LONGBOOL; stdcall;

Function StartServiceCtrlDispatcher
  (
  lpServiceStartTable: PServiceTableEntry
  ): LONGBOOL; stdcall;

Function StartService
  (
  hService: TSCHandle;
  dwNumServiceArgs: Integer;
  lpServiceArgVectors: PChar
  ): LONGBOOL; stdcall;

Function UnlockServiceDatabase
  (
  scLock: TSCLock
  ): LONGBOOL; stdcall;

Var
  hSCM: TSCHandle;
  hService: TSCHandle;
  ss: TServiceStatus;
  FStartTime: TDateTime;
  sshStatusHandle: TServiceStatusHandle;
  ssStatus: TServiceStatus;
  Stopped: BOOLEAN;
  EngineIsDone: BOOLEAN;

Implementation


Uses fssrvegmgr,
  fsllcomm,
  fsllcomp,
  fsllprot,
  fsllbase;


Const
  InstallServiceKey =
    'Software\Microsoft\Windows\CurrentVersion\RunServices';
  InstallRootKey = HKEY_LOCAL_MACHINE;

Procedure LocalProcessMessages; Forward;
Function InstallService(p: String): boolean; Forward;
Function UninstallService: boolean; Forward;
Function RunService: boolean; Forward;
Function StopService: boolean; Forward;
Function QueryService: boolean; Forward;
Function RegWrite(Root: HKEY; Key, Name, s: String;
  OpenAlready, LeaveOpen: Boolean): Boolean; Forward;
Procedure RegDelete(Root: HKEY; Key, Name: String); Forward;
Procedure RegClose; Forward;
Function OpenManager(AccessLevelNeeded: Integer): boolean; Forward;
Function GetServicePath: String; Forward;

// these are the common FF management routines ...
Function InitializeFF: boolean; Forward;
{starts things up}
Procedure FFProcessMessages; Forward;
{allows things to run}
Procedure StopFF; Forward;
{Stops the Engine}
Procedure FinalizeFF; Forward;
{finalizes, frees etc}

Function ChangeServiceConfig; External AdvAPI32 Name 'ChangeServiceConfigA';
Function CloseServiceHandle; External AdvAPI32;
Function ControlService; External AdvAPI32;
Function CreateService; External AdvAPI32 Name 'CreateServiceA';
Function DeleteService; External AdvAPI32;
Function EnumDependentServices; External AdvAPI32 Name 'EnumDependentServicesA';
Function EnumServicesStatus; External AdvAPI32 Name 'EnumServicesStatusA';
Function GetServiceKeyName; External AdvAPI32 Name 'GetServiceKeyNameA';
Function GetServiceDisplayName; External AdvAPI32 Name 'GetServiceDisplayNameA';
Function LockServiceDatabase; External AdvAPI32;
Function NotifyBootConfigStatus; External AdvAPI32;
Function OpenSCManager; External AdvAPI32 Name 'OpenSCManagerA';
Function OpenService; External AdvAPI32 Name 'OpenServiceA';
Function QueryServiceConfig; External AdvAPI32 Name 'QueryServiceConfigA';
Function QueryServiceLockStatus; External AdvAPI32 Name 'QueryServiceLockStatusA';
Function QueryServiceStatus; External AdvAPI32;
Function RegisterServiceCtrlHandler; External AdvAPI32 Name 'RegisterServiceCtrlHandlerA';
Function SetServiceStatus; External AdvAPI32;
Function StartServiceCtrlDispatcher; External AdvAPI32 Name 'StartServiceCtrlDispatcherA';
Function StartService; External AdvAPI32 Name 'StartServiceA';
Function UnlockServiceDatabase; External AdvAPI32;

// ****************************************************
// Message Logging Support ...
// ****************************************************

Var
  FFLogFileName: String = 'FSSQLLog.txt';
  EventLogName: String = 'FSSQLService';
  ServerLogCS: TRTLCriticalSection;

Const
  EVENTLOG_SUCCESS: Word = $0000;
  EVENTLOG_ERROR_TYPE: Word = $0001;
  EVENTLOG_WARNING_TYPE: Word = $0002;
  EVENTLOG_INFORMATION_TYPE: Word = $0004;
  EVENTLOG_AUDIT_SUCCESS: Word = $0008;
  EVENTLOG_AUDIT_FAILURE: Word = $0010;

Procedure WriteLnToServerLog(Text: String);
Var
  FFLog: textfile;
  s: String;
  a: Array[0..MAX_PATH] Of char;
  CN: Array[0..99] Of char;
  Sz: DWORD;
Begin
  If FFLogFileName = '' Then
    Exit;
  EnterCriticalSection(ServerLogCS);
  Try
    TTextRec(FFLog).Handle := 0;
    GetSystemDirectory(a, MAX_PATH);
    StrCat(a, System32SubDir);
    StrCat(a, pChar('\' + ExtractFileName(FFLogFileName)));
    s := String(a);
    If Not FileExists(s) Then
      Begin
        {$I-}
        Mkdir(ExtractFilePath(s));
        IOResult;
        {$I+}
      End;
    Try
      assignfile(FFLog, s);
      If Not FileExists(s) Then
        rewrite(FFLog)
      Else
        Begin
          Append(FFLog);
          writeln(FFLog, '');
        End;
      Sz := 99;
      GetComputerName(CN, Sz);
      s := String(CN);
      writeln(FFLog, FormatDateTime('yyyy/mm/dd hh:nn:ss', now) +
        ' Process: ' + IntToStr(GetCurrentProcessId) +
        ' Thread: ' + IntToStr(GetCurrentThreadId) +
        ' Computer: ' + s);
      writeln(FFLog, Text);
      closefile(FFLog);
    Except If TTextRec(FFLog).Handle <> 0 Then
        Begin
          Try
            CloseFile(FFLog);
          Except
          End;
        End;
    End;
  Except
  End;
  LeaveCriticalSection(ServerLogCS);
End;

Procedure LogMessage(msg: String; Severity: Word);
Var
  P: Pointer;
  EventLog: Integer;
  Win9XMode: String;
Begin
  Win9XMode := '';
  If (Not IsNT) And (Win32Platform = VER_PLATFORM_WIN32_NT) Then
    Win9XMode := ' - Running in Simulated Win9X Mode'
  Else
    Win9XMode := '';
  Try
    If Win32Platform = VER_PLATFORM_WIN32_NT Then
      Begin //NT --> Event Viewer -Application- Log
        P := PChar(msg + ' [' + OSIs + Win9XMode + ']'#0);
        EventLog := RegisterEventSource(Nil, PChar(EventLogName));
        If EventLog <> 0 Then
          Begin
            ReportEvent(EventLog, Severity, 0, 1, Nil, 1, 0, @P, Nil);
          End;
        If EventLog <> 0 Then
          DeRegisterEventSource(EventLog);
      End;
    WriteLnToServerLog(msg + ' [' + OSIs + Win9XMode + ']');
  Except
  End;
End;

Function GetExceptionDetails: String;
{$IFDEF UseExceptionMagic}
Var
  ModuleDebugInfo: TModuleDebugInfo;
  MName, FName, PName: String;
  LineNumber: Integer;
  {$ENDIF}
Begin
  Result := '';
  {$IFDEF UseExceptionMagic}
  Try
    ExceptionHook.Options := [excDlgDetailed, excDlgCallStack];
    If ExceptionHook.GetAddressSourceInfo(ExceptAddr, ModuleDebugInfo,
      MName, FName, PName, LineNumber) Then
      Result := ' {Address: ' + IntToStr(Integer(ExceptAddr)) +
        ' ModuleName: ' + MName +
        ' FileName: ' + FName +
        ' ProcName: ' + PName +
        ' Line Number: ' + IntToStr(LineNumber) + '}';
  Except
  End;
  {$ENDIF}
End;

Function GetExceptionCallStackDetails: String;
{$IFDEF UseExceptionMagic}
Var
  SL: TStringList;
  i: Integer;
  {$ENDIF}
Begin
  Result := '';
  {$IFDEF UseExceptionMagic}
  Try
    ExceptionHook.Options := [excDlgDetailed, excDlgCallStack];
    With ExceptionHook.CallStack Do
      Begin
        GenerateFromAddr(ExceptAddr, ExceptionHook.ExceptionRec.ExceptEBP,
          100, True);
        SL := TStringList.Create;
        Try
          Dump(SL);
          Result := '{{';
          For i := 0 To pred(SL.Count) Do
            If Trim(SL[i]) <> '' Then
              Result := Result + #13#10 + SL[i];
          Result := Result + '}}';
        Finally SL.free;
        End;
      End;
  Except
  End;
  {$ENDIF}
End;

Function GetCallStackDetails: String;
{$IFDEF UseExceptionMagic}
Var
  ModuleDebugInfo: TModuleDebugInfo;
  MName, FName, PName: String;
  i, LineNumber: Integer;
  SL: TStringList;
  {$ENDIF}
Begin
  Result := '';
  {$IFDEF UseExceptionMagic}
  Try
    ExceptionHook.Options := [excDlgDetailed, excDlgCallStack];
    Result := '';
    ExceptionHook.Options := [excDlgDetailed, excDlgCallStack];
    If ExceptionHook.GetAddressSourceInfo(
      ExceptAddr, ModuleDebugInfo,
      MName, FName, PName, LineNumber) Then
      Result := Result + ' {Address: ' + IntToStr(Integer(ExceptAddr)) +
        ' ModuleName: ' + MName +
        ' FileName: ' + FName +
        ' ProcName: ' + PName +
        ' Line Number: ' + IntToStr(LineNumber) + '}';

    // Call Stack
    With ExceptionHook.CallStack Do
      Begin
        Generate(100, True);
        SL := TStringList.Create;
        Try
          Dump(SL);
          Result := Result + '{{';
          For i := 0 To pred(SL.Count) Do
            Begin
              If Trim(SL[i]) <> '' Then
                Result := Result + #13#10 + SL[i];
            End;
          Result := Result + '}}';
        Finally SL.free;
        End;
      End;
  Except
  End;
  {$ENDIF}
End;

Procedure AddCallStackToServerLog;
{$IFDEF UseExceptionMagic}
Var
  s: String;
  {$ENDIF}
Begin
  {$IFDEF UseExceptionMagic}
  s := GetCallStackDetails;
  If s <> '' Then
    WriteLnToServerLog('Call Stack: ' + s);
  {$ENDIF}
End;

Procedure AddBlockToServerLog(Msg: String; Buf: pointer; BufLen: Integer);
Const
  HexPos: Array[0..15] Of Byte =
  (1, 4, 7, 10, 14, 17, 20, 23, 27, 30, 33, 36, 40, 43, 46, 49);
  HexChar: Array[0..15] Of char = '0123456789abcedf';
Var
  B: PByteArray Absolute Buf;
  ThisWidth, i, j, Res: Integer;
  Line: String[70];
  a: Array[0..MAX_PATH] Of char;
  Work: Byte;
  FFLog: textfile;
  SearchRec: TSearchRec;
  s: String;
Begin
  If FFLogFileName = '' Then
    Exit;
  Try
    EnterCriticalSection(ServerLogCS);
    TTextRec(FFLog).Handle := 0;
    GetSystemDirectory(a, MAX_PATH);
    StrCat(a, pChar(System32SubDir + ExtractFileName(FFLogFileName)));
    s := String(a);
    If Not FileExists(s) Then
      Begin
        GetSystemDirectory(a, MAX_PATH);
        StrCat(a, System32SubDir);
        {$I-}
        Mkdir(ExtractFilePath(String(a)));
        IOResult;
        {$I+}
      End;
    assignfile(FFLog, s);
    Res := FindFirst(s, faAnyFile, SearchRec);
    If Res <> 0 Then
      rewrite(FFLog)
    Else
      Begin
        // not if log > 4 Mb!!
        If SearchRec.Size > (4 * 1024 * 1024) Then
          Begin
            FindClose(SearchRec);
            Exit;
          End;
        Append(FFLog);
        writeln(FFLog, '');
      End;
    FindClose(SearchRec);
    writeln(FFLog, ' ': 13, Msg, ' (Size: ', BufLen, ')');
    If (BufLen = 0) Or (Buf = Nil) Then
      writeln(FFLog, ' ': 13, 'buffer is nil')
    Else
      Begin
        If BufLen > 33000 Then
          Begin
            BufLen := 33000;
          End;
        For i := 0 To ((BufLen - 1) Shr 4) Do
          Begin
            FillChar(Line, 70, ' ');
            Line[0] := #70;
            Line[53] := '[';
            Line[70] := ']';
            If (BufLen >= 16) Then
              ThisWidth := 16
            Else
              ThisWidth := BufLen;
            For j := 0 To ThisWidth - 1 Do
              Begin
                Work := B^[(i Shl 4) + j];
                Line[HexPos[j]] := HexChar[Work Shr 4];
                Line[HexPos[j] + 1] := HexChar[Work And $F];
                If (Work < 32) Or (Work >= $80) Then
                  Work := ord('.');
                Line[54 + j] := char(Work);
              End;
            writeln(FFLog, ' ': 13, Line);
            dec(BufLen, ThisWidth);
          End;
      End;
    If TTextRec(FFLog).Handle <> 0 Then
      Begin
        Try
          CloseFile(FFLog);
        Except
        End;
      End;
  Except If TTextRec(FFLog).Handle <> 0 Then
      Begin
        Try
          CloseFile(FFLog);
        Except
        End;
      End;
  End;
  LeaveCriticalSection(ServerLogCS);
End;

// ****************************************************
// Combined Win NT/2000 AND Win 9X Interface support
// ****************************************************

Procedure SetErrors;
Begin
  LastError := GetLastError;
  LastErrorMessage := SysErrorMessage(LastError);
End;

Procedure CloseManager;
Begin
  If IsNT And (hSCM <> 0) Then
    CloseServiceHandle(hSCM);
  hSCM := 0;
End;

Procedure CloseTheService;
Begin
  If IsNT And (hService <> 0) Then
    CloseServiceHandle(hService);
  hService := 0;
End;

Function OpenManager(AccessLevelNeeded: Integer): boolean;
Begin
  If IsNT Then
    Begin
      hSCM := OpenSCManager(Nil, Nil, AccessLevelNeeded);
      Result := hSCM <> 0;
    End
  Else
    Begin
      hSCM := FindWindow(SERVICE_WINDOW_NAME, Nil); // find Win95 window...
      Result := (hSCM <> 0) And IsWindow(hSCM);
      If Not Result Then
        ss.dwCurrentState := SERVICE_STOPPED;
    End;

  If Not Result Then
    SetErrors;
End;

Function OpenTheService: boolean;
Begin
  If IsNT Then
    Begin
      hService := OpenService(hSCM, PChar(CServiceName), SERVICE_ALL_ACCESS);
      Result := hService <> 0;
      If Not Result Then
        Begin
          SetErrors;
          CloseManager;
        End;
    End
  Else
    Result := True;
End;

Function InstallService(p: String): boolean;
Var
  a: Array[0..MAX_PATH] Of char;
  s: String;
Begin
  Result := False;
  If IsNT And (Not OpenManager(SC_MANAGER_CREATE_SERVICE)) Then
    Exit;
  GetSystemDirectory(a, MAX_PATH);
  StrCat(a, System32SubDir);
  s := String(a);
  StrCat(a, pChar(CExeName));
  {$I-}Mkdir(ExtractFilePath(String(a)));
  IOResult;
  {$I+}
  If p = '' Then
    p := ExtractFilePath(paramstr(0))
  Else
    Begin
      If p[length(p)] <> '\' Then
        p := p + '\';
    End;
  CopyFile(pChar(p + CExeName), pChar(s + CExeName), False);
  CopyFile(pChar(p + FullScriptFileName), pChar(s + FullScriptFileName), False);
  CopyFile(pChar(p + AliasTable), pChar(s + AliasTable), False);
  CopyFile(pChar(p + ServerInfoTable), pChar(s + ServerInfoTable), False);
  CopyFile(pChar(p + UserTable), pChar(s + UserTable), False);
  CopyFile(pChar(p + IndexTable), pChar(s + IndexTable), False);
  If IsNT Then
    Begin
      If IsWin2000XP Then
        hService := CreateService(hSCM, PChar(CServiceName), PChar(CDisplayName),
          SERVICE_ALL_ACCESS,

          SERVICE_WIN32_OWN_PROCESS
          {$IFNDEF FixExplorerBug}
          Or SERVICE_INTERACTIVE_PROCESS
          {$ENDIF},
          SERVICE_AUTO_START, SERVICE_ERROR_NORMAL,
          a, Nil, Nil, Nil, Nil, Nil)

      Else
        hService := CreateService(hSCM, PChar(CServiceName), PChar(CDisplayName),
          SERVICE_ALL_ACCESS,
          SERVICE_WIN32_OWN_PROCESS,
          SERVICE_AUTO_START, SERVICE_ERROR_NORMAL,
          a, Nil, Nil, Nil, Nil, Nil);

    End
  Else
    Begin // Win9X
      If FileExists(s + CExeName) Then
        Begin
          hService := 1;
          {$IFNDEF TESTING}
          RegWrite(InstallRootKey, InstallServiceKey,
            CServiceName, String(a), False, False);
          {$ENDIF}
        End
      Else
        hService := 0;
    End;
  Result := hService <> 0;
  CloseManager;
  CloseTheService;
  If Result Then
    LogMessage('FSSQL Server - installed', EVENTLOG_SUCCESS)
  Else
    Begin
      SetErrors;
      LogMessage('FSSQL Server - attempted install failed', EVENTLOG_ERROR_TYPE);
    End;
End;

Function UninstallService: boolean;
Var
  a: Array[0..MAX_PATH] Of char;
  s: String;
Begin
  GetSystemDirectory(a, MAX_PATH);
  StrCat(a, System32SubDir);
  s := String(a);
  StrCat(a, pChar(CExeName));
  Result := OpenManager(SC_MANAGER_ALL_ACCESS) And OpenTheService;
  If IsNT Then
    Begin
      If Result Then
        Begin
          DeleteService(hService);
          If Not Result Then
            SetErrors;
        End;
    End
  Else
    Begin
      If Result Then
        StopService; // service is running - stop it
      // remove registry settings
      RegDelete(InstallRootKey, InstallServiceKey, CServiceName);
      Result := True;
    End;
  sleep(2000);
  DeleteFile(s + FullScriptFileName);
  DeleteFile(s + AliasTable);
  DeleteFile(s + ServerInfoTable);
  DeleteFile(s + UserTable);
  DeleteFile(s + IndexTable);
  DeleteFile(a);
  If Result Then
    LogMessage('FSSQL Server - uninstalled', EVENTLOG_SUCCESS)
  Else
    LogMessage('FSSQL Server - attempted uninstall failed',
      EVENTLOG_ERROR_TYPE);
  CloseTheService;
  CloseManager;
End;

Function RunService: boolean;
Var
  p: pChar;
  a: Array[0..MAX_PATH] Of char;
Begin
  p := Nil;
  Result := OpenManager(SC_MANAGER_CONNECT) And OpenTheService;
  If IsNT Then
    Begin
      If Not Result Then
        SetErrors
      Else
        Begin
          StartService(hService, 0, p);
          If Not Result Then
            SetErrors;
        End;
    End
  Else
    Begin
      GetSystemDirectory(a, MAX_PATH);
      StrCat(a, pChar(System32SubDir + CExeName));
      Result := WinExec(a, SW_SHOWNORMAL) > 31;
      If Not Result Then
        SetErrors;
    End;
  CloseManager;
  CloseTheService;
End;

Function StopService: boolean;
Begin
  Result := OpenManager(SC_MANAGER_ALL_ACCESS) And OpenTheService;
  If Result Then
    Begin
      If IsNT Then
        ControlService(hService, SERVICE_CONTROL_STOP, ss)
      Else {result:= SendMessage(hSCM, WM_SERVICE_CONTROL_STOP, 0, 0)=
        SERVICE_ACCEPTED_MESSAGE;}
        Result := boolean(SendMessageTimeout(hSCM, WM_SERVICE_CONTROL_STOP,
          0, 0, SMTO_ABORTIFHUNG Or SMTO_NORMAL,
          2000, DWORD(ss.dwCurrentState)));
      If Not Result Then
        SetErrors;
      If Not IsNT Then
        LastErrorMessage := 'Engine not running';
    End
  Else
    LastErrorMessage := 'Engine not running';
  CloseManager;
  CloseTheService;
End;

Function QueryService: boolean;
Begin // called by client
  Result := OpenManager(SC_MANAGER_CONNECT) And OpenTheService;
  If Result Then
    Begin
      If IsNT Then
        Begin
          Result := ControlService(hService, SERVICE_CONTROL_INTERROGATE, ss);
          If Not Result Then
            SetErrors;
        End
      Else If Not boolean(SendMessageTimeout(hSCM, WM_SERVICE_CONTROL_INTERROGATE,
        0, 0, SMTO_ABORTIFHUNG Or SMTO_NORMAL,
        250, DWORD(ss.dwCurrentState))) Then
        Begin
          ss.dwCurrentState := -1;
          Result := False;
        End
    End
  Else
    Begin
      If Not IsNT Then
        LastErrorMessage := 'Engine not running';
    End;
  CloseManager;
  CloseTheService;
End;

// *******************************
// Win NT/2000 support
// *******************************

Function GetServicePath: String;
Var
  a: Array[0..MAX_PATH] Of char;
Begin
  GetSystemDirectory(a, MAX_PATH);
  StrCat(a, System32SubDir);
  StrCat(a, pChar(CExeName));
  Result := String(a);
End;

Procedure ServiceHandler(fdwControl: Integer); Stdcall;

Var
  Countdown: Integer;

Begin
  If Not (fdwControl In [SERVICE_CONTROL_INTERROGATE,
    SERVICE_CONTROL_STOP,
      SERVICE_CONTROL_SHUTDOWN]) Then
    LogMessage('Unexpected FF Server Request Received ' +
      IntToStr(fdwControl), EVENTLOG_SUCCESS);
  If fdwControl = SERVICE_CONTROL_INTERROGATE Then
    Begin
      If Not Stopped Then
        Begin
          If IsWindow(FSSQLWindow) Then
            PostMessage(FSSQLWindow, FFInterrogate, 0, 0);
        End;
    End;
  If fdwControl In [SERVICE_CONTROL_STOP, SERVICE_CONTROL_SHUTDOWN] Then
    Begin
      LogMessage('FSSQL Server Stop Request Received', EVENTLOG_SUCCESS);
      EngineIsDone := False;
      Stopped := True;
      ssStatus.dwCurrentState := SERVICE_STOP_PENDING;
      SetServiceStatus(sshStatusHandle, @ssStatus);
      If IsWindow(FSSQLWindow) Then
        PostMessage(FSSQLWindow, 0, 0, 0);
      Try
        StopFF;
      Except
      End;
      ssStatus.dwCurrentState := SERVICE_STOPPED; {the code should have been here}
      SetServiceStatus(sshStatusHandle, @ssStatus);
    End;

End;

Procedure ServiceProc(dwArgC: Integer; Var lpszArgV: PChar); Stdcall;
Begin
  sshStatusHandle := RegisterServiceCtrlHandler(PChar(CServiceName),
    @ServiceHandler);
  If sshStatusHandle <> 0 Then
    Begin
      Stopped := False;
      FillChar(ssStatus, SizeOf(ssStatus), #0);
      ssStatus.dwServiceType := SERVICE_WIN32_OWN_PROCESS Or
        SERVICE_INTERACTIVE_PROCESS;
      ssStatus.dwCurrentState := SERVICE_START_PENDING;
      ssStatus.dwControlsAccepted := SERVICE_CONTROL_INTERROGATE Or
        SERVICE_ACCEPT_STOP Or
        SERVICE_ACCEPT_SHUTDOWN;
      ssStatus.dwWaitHint := 30000;
      SetServiceStatus(sshStatusHandle, @ssStatus);
      If InitializeFF Then
        Begin
          ssStatus.dwCurrentState := SERVICE_RUNNING;
          ssStatus.dwWin32ExitCode := NO_ERROR;
          SetServiceStatus(sshStatusHandle, @ssStatus);
          FFProcessMessages;
        End
      Else
        Begin
          ssStatus.dwWin32ExitCode := ERROR_SERVICE_SPECIFIC_ERROR;
          ssStatus.dwServiceSpecificExitCode := 1;
          LogMessage('InitializeFF failed: ', EVENTLOG_ERROR_TYPE);
        End;
      EngineIsDone := True;
      Try
        FinalizeFF;
      Except On E: Exception Do
          Begin
            LogMessage('FinalizeFF Failure: ' + E.message, EVENTLOG_ERROR_TYPE);
          End;
      End;
    End;
  ssStatus.dwCurrentState := SERVICE_STOPPED;
  SetServiceStatus(sshStatusHandle, @ssStatus);
  LogMessage('FSSQL Server Stopped', EVENTLOG_SUCCESS);
End;

Procedure RunNT;
Var
  dispatchTable: Array[0..1] Of TServiceTableEntry;
  s: String;
Begin
  CExeName := ExtractFileName(ParamStr(0));
  // handle Install/UnInstall requests in the Param List ...
  If ParamCount > 0 Then
    Begin
      s := ParamStr(1);
      If s <> '' Then
        Begin
          Case UpCase(s[1]) Of
            'I':
              Begin
                InstallService('');
                RunService;
              End;
            'R':
              Begin
                RunService;
              End;
            'S':
              Begin
                StopService;
              End;
            'U':
              Begin
                StopService;
                UninstallService;
              End;
            Else
              Begin
                LogMessage('Unknown Startup Parameter: ' + s, EVENTLOG_ERROR_TYPE);
              End;
          End;
          Exit;
        End;
    End;

  dispatchTable[0].lpServiceName := PChar(CServiceName);
  dispatchTable[0].lpServiceProc := @ServiceProc;
  dispatchTable[1].lpServiceName := Nil;
  dispatchTable[1].lpServiceProc := Nil;
  StartServiceCtrlDispatcher(@dispatchTable);
End;

// *******************************
// Win 9X support
// *******************************

// ****************************************************
// Registry handling code
// ****************************************************

Var
  TempKey: HKEY = 0;

Function RegWrite(Root: HKEY; Key, Name, s: String;
  OpenAlready, LeaveOpen: Boolean): Boolean;
Var
  Disposition: Integer;
Begin
  If Not OpenAlready Then
    Begin
      RegClose;
      {try to open key}
      Result := (RegOpenKeyEx(Root, pchar(Key), 0,
        KEY_QUERY_VALUE Or KEY_SET_VALUE,
        TempKey) = ERROR_SUCCESS);
      {if cannot open key try to create it}
      If Not Result Then
        Result := RegCreateKeyEx(Root, pchar(Key), 0, Nil,
          REG_OPTION_NON_VOLATILE,
          KEY_EXECUTE Or KEY_SET_VALUE,
          Nil, TempKey, @Disposition) = ERROR_SUCCESS;
      {if cannot open or create key then exit with error}
      If Not Result Then
        Exit;
    End;
  Result := RegSetValueEx(TempKey, PChar(Name), 0,
    REG_SZ, pchar(s), length(s)) = ERROR_SUCCESS;
  If Not LeaveOpen Then
    RegClose;
End;

Procedure RegDelete(Root: HKEY; Key, Name: String);
Begin
  RegClose;
  If RegOpenKeyEx(Root, pchar(Key), 0,
    _DELETE Or KEY_WRITE, TempKey) <> ERROR_SUCCESS Then
    Exit;
  If Name = '' Then
    RegDeleteKey(TempKey, Nil) // fails on NT if TempKey is not empty
  Else
    RegDeleteValue(TempKey, pchar(Name));
  RegClose;
End;

Procedure RegClose;
Begin
  If TempKey <> 0 Then
    Begin
      RegFlushKey(TempKey);
      RegCloseKey(TempKey);
      TempKey := 0;
    End;
End;

Function WndProc(Window: HWND; Message, wParam, lParam: Longint): Longint; Stdcall;
Begin
  Result := 0;
  Case Message Of
    WM_DESTROY:
      Begin
        stopped := True;
        PostMessage(FSSQLWindow, 0, 0, 0);
        Result := DefWindowProc(Window, Message, wParam, lParam);
      End;
    WM_SERVICE_CONTROL_STOP:
      Begin
        Stopped := True;
        ReplyMessage(SERVICE_ACCEPTED_MESSAGE);
      End;
    WM_SERVICE_CONTROL_INTERROGATE:
      Begin
        SendMessage(FSSQLWindow, FFInterrogate, 0, 0);
        ReplyMessage(ssStatus.dwCurrentState);
      End;
    Else
      Result := DefWindowProc(Window, Message, wParam, lParam);
  End;
End;

Var
  WinHandle: HWND = 0;
  WindowClass: TWndClass = (style: 0;
    lpfnWndProc: @WndProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: Nil;
    lpszClassName: SERVICE_WINDOW_NAME);

Function MakeServiceWindow: boolean;
Begin
  Result := Windows.RegisterClass(WindowClass) <> 0;
  If Result Then
    Begin
      WinHandle := CreateWindowEx(WS_EX_TOOLWINDOW, WindowClass.lpszClassName,
        '', WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, Nil);
    End
  Else
    Begin
      LogMessage(SysErrorMessage(GetLastError), EVENTLOG_ERROR_TYPE);
    End;
End;

Function RegisterTheService: boolean;
Const
  RSP_SIMPLE_SERVICE: DWORD = 1; // Registers the process as a service
  RSP_UNREGISTER_SERVICE: DWORD = 0; // Unregisters the process as a service
Type
  TRegisterServiceProcess =
    Function(dwProcessId, dwServiceType: DWORD): DWORD; stdcall;
Var
  Kernal32Module: THandle;
  RegisterServiceProcess: TRegisterServiceProcess;
Begin
  Result := False;
  Kernal32Module := LoadLibrary(kernel32);
  If Kernal32Module = 0 Then
    Begin
      LogMessage('Unable to Load "' + kernel32 + '". Error Message: ' +
        SysErrorMessage(GetLastError), EVENTLOG_ERROR_TYPE);
      Exit;
    End;
  @RegisterServiceProcess := GetProcAddress(Kernal32Module, 'RegisterServiceProcess');
  If @RegisterServiceProcess <> Nil Then
    Result := RegisterServiceProcess(0, RSP_SIMPLE_SERVICE) = 1
  Else
    Result := True;
  FreeLibrary(Kernal32Module);
  If Not Result Then
    Begin
      LogMessage('Unable to Register as a Service with the non NT/2000 OS: ',
        EVENTLOG_ERROR_TYPE);
    End;
End;

Procedure LocalProcessMessages;
Var
  Msg: TMsg;
  s: String;
Begin
  While PeekMessage(Msg, 0, 0, 0, PM_REMOVE) Do
    Begin
      If Msg.Message = WM_QUIT Then
        STOPPED := True
      Else
        Begin
          TranslateMessage(Msg); // are there keystrokes here? - not sure!
          Try
            DispatchMessage(Msg);
          Except On E: Exception Do
              Begin
                s := 'Unexpected Error!: ' + E.message +
                  ' [Message: ' + IntToStr(Msg.message) +
                  ' wParam: ' + IntToStr(Msg.wParam) +
                  ' lParam: ' + IntToStr(Msg.lParam) + ']' +
                  ' HInstance: ' + IntToStr(Integer(HInstance)) +
                  ' MapBase: ' + IntToStr(Integer(HInstance + 4104));

                s := s + GetExceptionDetails + GetExceptionCallStackDetails;
                LogMessage(s, EVENTLOG_ERROR_TYPE);
              End;
          End;
        End;
    End;
End;

Procedure StartTheService;
Begin
  Stopped := False;
  ssStatus.dwCurrentState := SERVICE_RUNNING;
  If InitializeFF Then
    FFProcessMessages
  Else
    LogMessage('InitializeFF failed: ', EVENTLOG_ERROR_TYPE);
  FinalizeFF;
End;

Procedure RunWin9x;
Begin
  CExeName := ExtractFileName(ParamStr(0));
  If MakeServiceWindow Then
    Begin
      RegisterTheService;
      StartTheService;
    End;
End;

// ***************************
// FS start, run, end code
// ***************************

Procedure FFProcessMessage(ForMessage: DWORD);
Var
  Msg: TMsg;
  s: String;
Begin 
  If Stopped Then
    Exit;
  Try
    While PeekMessage(Msg, 0, ForMessage, ForMessage, PM_REMOVE) Do
      Begin
        // TranslateMessage(Msg); - no keystrokes here
        Try
          DispatchMessage(Msg);
        Except On E: Exception Do
            Begin
              s := 'Unexpected Error!: ' + E.message +
                ' [Message: ' + IntToStr(Msg.message) +
                ' wParam: ' + IntToStr(Msg.wParam) +
                ' lParam: ' + IntToStr(Msg.lParam) + ']' +
                ' HInstance: ' + IntToStr(Integer(HInstance)) +
                ' MapBase: ' + IntToStr(Integer(HInstance + 4104));

              s := s + GetExceptionDetails + GetExceptionCallStackDetails;
              LogMessage(s, EVENTLOG_ERROR_TYPE);
            End;
        End;
      End;
  Except On E: Exception Do
      Begin
        s := 'Unexpected Error!: ' + E.message +
          ' [Message: ' + IntToStr(Msg.message) +
          ' wParam: ' + IntToStr(Msg.wParam) +
          ' lParam: ' + IntToStr(Msg.lParam) + ']' +
          ' HInstance: ' + IntToStr(Integer(HInstance)) +
          ' MapBase: ' + IntToStr(Integer(HInstance + 4104));

        s := s + GetExceptionDetails + GetExceptionCallStackDetails;
        LogMessage(s, EVENTLOG_ERROR_TYPE);
      End;
  End;
End;

Procedure FFProcessMessages;
Begin // FF1 & FF2 - common code
  Try
    If FSSQLWindow = 0 Then
      Exit; // no comms engine window???

    // sleep until any message arrives
    // note: this means that we MUST send a message
    // to get it to respond - eg, to STOP it
    Repeat WaitMessage;
      FFProcessMessage(0);
    Until Stopped;
  Except On E: Exception Do
      Begin
        LogMessage('FSSQL ProcessMessages Failure: ' + E.message, EVENTLOG_ERROR_TYPE);
      End;
  End;
End;

Var
  fsSvcEngineMgr: TFSEngineManager;

Function InitializeFF: boolean;
Var
  Msg: String;
Begin // from ffllsvc.pas: procedure InitEngineMgr  & procedure ServiceMain
  // enable all transports, accept broadcasts, listen
  Result := False;
  // Start the engine manager.
  Try
    IsMultiThread := True;
    fsSvcEngineMgr := TFSEngineManager.Create(Nil);
    fsSvcEngineMgr.EventLogEnabled := False;
    fsSvcEngineMgr.ScriptFile := ExtractFilePath(ParamStr(0)) + FullScriptFileName;
    Msg := Format( '32-bit Version %5.3f', [fsVersionNumber / 1000.0] ) + ' Started:';
    fsc_TCPInterface := -1; // enable ALL NICS
    With fsSvcEngineMgr.ServerEngine.Configuration.GeneralInfo^ Do
      Begin
        If fsSvcEngineMgr.SUPTransport.Supported Then
          Begin
            fsSvcEngineMgr.SUPTransport.Enabled := True;
            fsSvcEngineMgr.SUPTransport.BeginUpdate;
            Try
              fsSvcEngineMgr.SUPTransport.ServerName := giServerName;
              fsSvcEngineMgr.SUPTransport.Mode := fstmListen;
              fsSvcEngineMgr.SUPTransport.EndUpdate;
              Msg := Msg + ' SUP';
            Except fsSvcEngineMgr.SUPTransport.CancelUpdate;
            End;
          End;

        If fsSvcEngineMgr.IPXSPXTransport.Supported Then
          Begin
            fsSvcEngineMgr.IPXSPXTransport.Enabled := True;
            fsSvcEngineMgr.IPXSPXTransport.BeginUpdate;
            Try
              fsSvcEngineMgr.IPXSPXTransport.ServerName := giServerName;
              fsSvcEngineMgr.IPXSPXTransport.RespondToBroadcasts := True;
              fsSvcEngineMgr.IPXSPXTransport.Mode := fstmListen;
              fsSvcEngineMgr.IPXSPXTransport.EndUpdate;
              Msg := Msg + ' IPX';
            Except fsSvcEngineMgr.IPXSPXTransport.CancelUpdate;
            End;
          End;

        If fsSvcEngineMgr.TCPIPTransport.Supported Then
          Begin
            fsSvcEngineMgr.TCPIPTransport.Enabled := True;
            fsSvcEngineMgr.TCPIPTransport.BeginUpdate;
            Try
              fsSvcEngineMgr.TCPIPTransport.ServerName := giServerName;
              fsSvcEngineMgr.TCPIPTransport.RespondToBroadcasts := True;
              fsSvcEngineMgr.TcpIPTransport.Mode := fstmListen;
              fsSvcEngineMgr.TCPIPTransport.EndUpdate;
              Msg := Msg + ' TCP';
            Except fsSvcEngineMgr.TCPIPTransport.CancelUpdate;
            End;
          End;
      End;

    fsSvcEngineMgr.StartUp;
    FSSQLWindow := 1;
    Result := True;
    LogMessage(Msg, EVENTLOG_SUCCESS);
  Except On E: Exception Do
      Begin
        LogMessage('Failure Starting Engine: ' + E.Message,
          EVENTLOG_ERROR_TYPE);
      End;
  End;
End;

Procedure StopFF;
Begin
  fsSvcEngineMgr.Stop;
End;

Procedure FinalizeFF;
Begin
  Try
    fsSvcEngineMgr.ShutDown;
  Except
  End;
  Try
    fsSvcEngineMgr.Free;
  Except
  End;
End;

Initialization
  {$IFDEF TESTING}
  IsNT := False; {use this for testing Win95 mode on NT}
  {$ELSE}
  IsNT := Win32Platform = VER_PLATFORM_WIN32_NT;
  {$ENDIF}

  IsWin2000XP := False;
  Case Win32Platform Of
    VER_PLATFORM_WIN32_WINDOWS:
      Case Win32MinorVersion Of
        0: If Win32CSDVersion = 'B' Then
            OSIs := 'Win95OSR2'
          Else
            OSIs := 'Win95';
        10: If Win32CSDVersion = 'A' Then
            OSIs := 'Win98SE'
          Else
            OSIs := 'Win98';
        90..255: OSIs := 'WinME';
      End;
    VER_PLATFORM_WIN32_NT:
      Case Win32MajorVersion Of
        3: OSIs := 'WinNT3';
        4: OSIs := 'WinNT4';
        5:
          Begin
            OSIs := 'Win2000XP';
            IsWin2000XP := True;
          End;
      End;
  End;
  InitializeCriticalSection(ServerLogCS);

Finalization
  DeleteCriticalSection(ServerLogCS);

End.

