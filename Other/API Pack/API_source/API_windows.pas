unit API_windows;

//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
//
// r1.02, 29062009, ari pikivirta
//  * exported most of the functions, because they're actually quite handy
//  * added function AppIsResponding(Classname)
//  * added function GetUserPrivileges
//  * added event OnSessionEnd to detect Windows shutdown
//  * added function KillTask(Exename)
//  * added function KillProcess(Handle)
//
// r1.01, 26022007, ari pikivirta
//  * changed a bit function names
//  * added possibility to get module filename
//  * made functions more efficient
//
// r1.00, 25022007, ari pikivirta
//  * created

interface

uses
  Windows, Messages, SysUtils, Classes, API_base;

type
  TAPI_windows = class(TAPI_Custom_Component)
  private
    { Private declarations }
    fList: tstringlist;
    fEndSessionEvent: tnotifyevent;
    procedure dummys(s: string);
    procedure dummysl(sl: tstringlist);
    function  ActiveWindowCaption: string;
    function  GetList: tstringlist;
    function  GetListColumn(
                  const index: integer;
                  var caption: string;
                  var wclassname: string;
                  var whandle: cardinal;
                  var memorysize: cardinal;
                  var filename: string ): boolean;
    procedure WMEndSession(var Msg: TWMEndSession); message WM_ENDSESSION;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: tcomponent); override;
    destructor  Destroy; override;
    procedure   UpdateList;                                     // update(s) cache
    function    ForceForeground(handle: hwnd): boolean;         // show
    function    HandleFromCaption(Caption: string): cardinal;   // handle
    function    HandleFromFilename(Filename: string): cardinal; // handle
    function    Filename(Handle: hwnd): string;                 // filename
    function    Caption(Handle: hwnd): string;                  // caption
    function    MemoryUsage(Handle: hwnd): cardinal;            // bytes
    function    HandleFromIndex(index: integer): cardinal;
    function    FilenameFromIndex(index: integer): string;
    function    CaptionFromIndex(index: integer): string;
    function    MemoryUsageFromIndex(index: integer): cardinal;
    function    ClassNameFromIndex(index: integer): string;
  published
    { Published declarations }
    property ActiveCaption: string read ActiveWindowCaption write dummys stored false;
    property List: tstringlist read getlist write dummysl stored FALSE;
    property OnSessionEnd: tnotifyevent read fendsessionevent write fendsessionevent;
  end;

procedure Register;

//------------------------------------------------------------------------------
// exported functions ----------------------------------------------------------
//------------------------------------------------------------------------------
function KillTask(Const ExeFileName: string): Integer;
procedure KillProcess(hWindowHandle: HWND);
function GetUserPrivileges(var List: TStrings): boolean;
function ForceForeground(handle1: THandle): boolean;
function AppIsResponding(ClassName: string): Boolean;
function IsWinXP: Boolean;
function IsWin2k: Boolean;
function IsWinNT4: Boolean;
function IsWin3X: Boolean;
function GetProcessMemorySize(l_nWndHandle: hwnd; var _nMemSize: Cardinal): Boolean;
function GetWindowModule(hwnd: THandle; var Fname: string): boolean;
function GetComputerName: string;
function SetComputerName(AComputerName: string): Boolean;
function GetCurrentUserName(var CurrentUserName: string): Boolean;
//------------------------------------------------------------------------------

implementation

{$include '..\API_source\inc\CompilerVersions.INC'}

uses
  PsAPI, TlHelp32;

const
  ComponmentRevisionString = '1.02/ari.pikivirta[at]kolumbus.fi';
  RsSystemIdleProcess = 'System Idle Process';
  RsSystemProcess = 'System Process';

//------------------------------------------------------------------------------
function GetCurrentUserName(var CurrentUserName: string): Boolean;
var
  BufferSize: DWORD;
  pUser: PChar;
begin
  BufferSize := 0;
  GetUserName(nil, BufferSize);
  pUser := StrAlloc(BufferSize);
  try
    Result := GetUserName(pUser, BufferSize);
    CurrentUserName := StrPas(pUser);
  finally
    StrDispose(pUser);
  end;
end;

//------------------------------------------------------------------------------
function GetComputerName: string;
var
  buffer: array[0..MAX_COMPUTERNAME_LENGTH + 1] of Char;
  Size: Cardinal;
begin
  Size := MAX_COMPUTERNAME_LENGTH + 1;
  Windows.GetComputerName(@buffer, Size);
  Result := StrPas(buffer);
end;

function SetComputerName(AComputerName: string): Boolean;
var
  ComputerName: array[0..MAX_COMPUTERNAME_LENGTH + 1] of Char;
begin
  StrPCopy(ComputerName, AComputerName);
  Result := Windows.SetComputerName(ComputerName);
end;

//------------------------------------------------------------------------------
function KillTask(Const ExeFileName: string): Integer;
const
  PROCESS_TERMINATE = $0001;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  Result := 0;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  try
    FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
    ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
    while Integer(ContinueLoop) <> 0 do
    begin
      if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
        UpperCase(ExeFileName)) or (UpperCase(FProcessEntry32.szExeFile) =
        UpperCase(ExeFileName))) then
          Result := Integer(TerminateProcess(OpenProcess(PROCESS_TERMINATE, BOOL(0), FProcessEntry32.th32ProcessID), 0));
     ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
    end;
  finally
    CloseHandle(FSnapshotHandle);
  end;
end;

procedure KillProcess(hWindowHandle: HWND);
var
  hprocessID: INTEGER;
  processHandle: THandle;
  DWResult: DWORD;
begin
  SendMessageTimeout(hWindowHandle, WM_CLOSE, 0, 0, SMTO_ABORTIFHUNG or SMTO_NORMAL, 5000, DWResult);
  if isWindow(hWindowHandle) then
  begin
    { Get the process identifier for the window}
    GetWindowThreadProcessID(hWindowHandle, @hprocessID);
    if hprocessID <> 0 then
    begin
      { Get the process handle }
      processHandle := OpenProcess(PROCESS_TERMINATE or PROCESS_QUERY_INFORMATION, False, hprocessID);
      if processHandle <> 0 then
      begin
        { Terminate the process }
        TerminateProcess(processHandle, 0);
        CloseHandle(ProcessHandle);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_windows.WMEndSession(var Msg: TWMEndSession);
begin
  if Msg.EndSession = TRUE then
    if assigned(fendsessionevent) then
      fendsessionevent(self);
  inherited;
end;

//------------------------------------------------------------------------------
function GetUserPrivileges(var List: TStrings): boolean;
const
  TokenSize = 800; //  (SizeOf(Pointer)=4 *200)
var
  hToken: THandle;
  pTokenInfo: PTOKENPRIVILEGES;
  ReturnLen: Cardinal;
  i: Integer;
  PrivName: PChar;
  DisplayName: PChar;
  NameSize: Cardinal;
  DisplSize: Cardinal;
  LangId: Cardinal;
begin
  Result:= FALSE;
  List.Clear;
  GetMem(pTokenInfo, TokenSize);
  try
    if not OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then exit;
    if not GetTokenInformation(hToken, TokenPrivileges, pTokenInfo, TokenSize, ReturnLen) then exit;
    GetMem(PrivName, 255);
    GetMem(DisplayName, 255);
    try
      for i:= 0 to pTokenInfo.PrivilegeCount-1 do
      begin
        DisplSize := 255;
        NameSize  := 255;
        LookupPrivilegeName(nil, pTokenInfo.Privileges[i].Luid, PrivName, Namesize);
        LookupPrivilegeDisplayName(nil, PrivName, DisplayName, DisplSize, LangId);
        List.add(PrivName +^I + DisplayName);
      end;
    finally
      FreeMem(PrivName);
      FreeMem(DisplayName);
    end;
  finally
    FreeMem(pTokenInfo);
  end;
end;

//------------------------------------------------------------------------------
function AppIsResponding(ClassName: string): Boolean;
const
  TIMEOUT = 50; { Specifies the duration, in milliseconds, of the time-out period }
var
  Res: DWORD;
  h: HWND;
begin
  h:= FindWindow(PChar(ClassName), nil);
  if (h<>0) then
    Result := SendMessageTimeOut(H, WM_NULL, 0, 0, SMTO_NORMAL or SMTO_ABORTIFHUNG, TIMEOUT, Res) <> 0
  else
    // Classname was not found!
    raise exception.create(Classname+' was not found.');
end;

//------------------------------------------------------------------------------
constructor TAPI_windows.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Version:= ComponmentRevisionString;
  flist:= tstringlist.create;
  UpdateList;
end;

//------------------------------------------------------------------------------
destructor TAPI_windows.Destroy;
begin
  flist.free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
function IsWinXP: Boolean;
begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and
    (Win32MajorVersion = 5) and (Win32MinorVersion = 1);
end;

function IsWin2k: Boolean;
begin
  Result := (Win32MajorVersion >= 5) and
    (Win32Platform = VER_PLATFORM_WIN32_NT);
end;

function IsWinNT4: Boolean;
begin
  Result := Win32Platform = VER_PLATFORM_WIN32_NT;
  Result := Result and (Win32MajorVersion = 4);
end;

function IsWin3X: Boolean;
begin
  Result := Win32Platform = VER_PLATFORM_WIN32_NT;
  Result := Result and (Win32MajorVersion = 3) and
    ((Win32MinorVersion = 1) or (Win32MinorVersion = 5) or
    (Win32MinorVersion = 51));
end;

//------------------------------------------------------------------------------
procedure TAPI_windows.dummys(s: string);
begin
  // does absolutely nothing
end;

procedure TAPI_windows.dummysl(sl: tstringlist);
begin
  // does absolutely nothing
end;

//------------------------------------------------------------------------------
function GetProcessMemorySize(l_nWndHandle: hwnd; var _nMemSize: Cardinal): Boolean;
var
  l_nProcID, l_nTmpHandle: HWND;
  l_pPMC: PPROCESS_MEMORY_COUNTERS;
  l_pPMCSize: Cardinal;
begin
  result:= false;
  if l_nWndHandle = 0 then exit;

  l_pPMCSize:= SizeOf(PROCESS_MEMORY_COUNTERS);
  GetMem(l_pPMC, l_pPMCSize);
  l_pPMC^.cb := l_pPMCSize;
  GetWindowThreadProcessId(l_nWndHandle, @l_nProcID);
  l_nTmpHandle := OpenProcess(PROCESS_ALL_ACCESS, False, l_nProcID);
  if (GetProcessMemoryInfo(l_nTmpHandle, l_pPMC, l_pPMCSize)) then _nMemSize := l_pPMC^.WorkingSetSize
    else _nMemSize := 0;
  FreeMem(l_pPMC);
  Result := True;
end;

//------------------------------------------------------------------------------
(*
    todo:
      * get this list same time processing the whole list..
*)
function GetWindowModule(hwnd: THandle; var Fname: string): boolean;
var
  //pe32: TProcessEntry32;
  //hSnapshot: integer;
  Pid: dword;
  //fr: bool;
  Handle: THandle;
begin
  result:= false;
  fname:= '';

  GetWindowThreadProcessId(hwnd,@Pid);

  Handle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, PID);
  if Handle <> 0 then
  try
    SetLength(Fname, MAX_PATH);
    (*
    if FullPath then
    begin
      if GetModuleFileNameEx(Handle, 0, PChar(Result), MAX_PATH) > 0 then
        SetLength(Result, StrLen(PChar(Result)))
      else
        Result := '';
      end
      else
    *)
    {$ifdef DELPHI2009UP}
    if GetModuleBaseNameW(Handle, 0, pwidechar(fname), MAX_PATH) > 0 then
    {$else}
    if GetModuleBaseName(Handle, 0, pansichar(fname), MAX_PATH) > 0 then
    {$endif}
  finally
    CloseHandle(Handle);
  end;

  (*
  hSnapshot:= CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if hSnapshot = -1 then exit;

  FillChar(pe32,sizeof(TProcessEntry32),#0);
  pe32.dwSize:= sizeof(TProcessEntry32);
  fr:= Process32First(hSnapshot, pe32);
  while fr do
  begin
    if Pid = pe32.th32ProcessID then
    begin
       fname:= strpas(pe32.szExeFile);
       result:= true;
       break;
    end;
    fr:= Process32Next(hSnapshot, pe32);
  end;
  CloseHandle( hSnapshot);
  *)
end;

//------------------------------------------------------------------------------
(*
  EnumWindows(lpEnumFunc: TFNWndEnumProc;   {the address of the enumeration callback function}
            lParam: LPARAM                  {a 32-bit application-defined value}
            ): BOOL;                        {returns TRUE or FALSE}

  EnumWindowsProc(hWnd: HWND;               {a handle to a top-level window}
                lParam: LPARAM              {the application-defined data}
                ): BOOL;                    {returns TRUE or FALSE}
*)

function EnumWindowsProc(wHandle: HWND; lb: tstringlist): Bool; stdcall; export;
var
  Title, ClassName: array[0..255] of char;
  MemorySize: cardinal;
  Fname: string;
begin
  Result:= True;
  GetWindowText(wHandle, Title, 255);
  GetClassName(wHandle, ClassName, 255);
  if not GetProcessMemorySize(wHandle, MemorySize) then MemorySize:= 0;
  GetWindowModule(wHandle, FName);
  lb.add( string(Title)+'|'        // 1. caption
    +string(ClassName)+'|'         // 2. class name
    +inttostr(wHandle)+'|'         // 3. handle
    +inttostr(MemorySize)+'|'      // 4. memory size
    +Fname+'|'                     // 5. filename
    );
end;

procedure TAPI_windows.UpdateList;
begin
  flist.clear;
  windows.EnumWindows(@EnumWindowsProc, Integer(flist));
end;

//------------------------------------------------------------------------------
function TAPI_windows.ActiveWindowCaption: string;
var
  Handle: THandle;
  Len: LongInt;
  Title: string;
begin
  Result:= '';
  {$IFDEF Win32}
  Handle:= GetForegroundWindow;
  {$ELSE}
  Handle:= GetActiveWindow;
  {$ENDIF}
  if Handle<>0 then
  begin
    Len:= GetWindowTextLength(Handle) + 1;
    SetLength(Title, Len);
    GetWindowText(Handle, PChar(Title), Len);
    ActiveCaption:= TrimRight(Title);
  end;
end;

//------------------------------------------------------------------------------
function ForceForeground(handle1: THandle): boolean;
const
  SPI_GETFOREGROUNDLOCKTIMEOUT = $2000;
  SPI_SETFOREGROUNDLOCKTIMEOUT = $2001;
var
  ForegndThreadID: DWORD;
  TheThreadID: DWORD;
  timeout: DWORD;
  OSVersionInfo: TOSVersionInfo;
  hParent: THandle;
  AniInfo: TAnimationInfo;
  Animate: Boolean;
begin
  // 1. check if just minimized
  if IsIconic(Handle1) then windows.ShowWindow(Handle1, SW_RESTORE);
  result:= (windows.getforegroundwindow=handle1);
  if result then exit;

  // 2. try getting handle again (minimized)
  hParent:= GetWindowLong(Handle1, GWL_HWNDPARENT);
  if hParent>0 then
    if IsIconic(hParent) then windows.ShowWindow(hParent, SW_RESTORE);
  result:= (windows.getforegroundwindow=handle1);
  if result then exit;

  // 3. might be platform specific..
  OSVersionInfo.dwOSVersionInfoSize:= SizeOf(OSVersionInfo);
  GetVersionEx(OSVersionInfo);
  if ((OSVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT) and (OSVersionInfo.dwMajorVersion > 4))
    or ((OSVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS) and ((OSVersionInfo.dwMajorVersion > 4)
    or ((OSVersionInfo.dwMajorVersion = 4) and (OSVersionInfo.dwMinorVersion > 0)))) then
  begin
    // OS is above win 95

    // 3.1. attach threadinput and force then foreground
    ForegndThreadID:= GetWindowThreadProcessID(GetForegroundWindow,nil);
    TheThreadID:= GetWindowThreadProcessId(Handle1,nil);
    if AttachThreadInput(TheThreadID, ForegndThreadID, true) then
    begin
      SetForegroundWindow(Handle1);
      AttachThreadInput(TheThreadID, ForegndThreadID, false);
    end;
    Result:= (windows.GetForegroundWindow=Handle1);
    if result then exit;

    // 3.2. play with foreground lockings
    SystemParametersInfo(SPI_GETFOREGROUNDLOCKTIMEOUT, 0, @timeout, 0);
    SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, nil, SPIF_SENDCHANGE);
    windows.SetForegroundWindow(Handle1);
    SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, @timeout, SPIF_SENDCHANGE);
    Result:= (windows.GetForegroundWindow = Handle1);
    if Result then Exit;

  end else
  begin

    // 3.3. in win 95 this should be all needed
    windows.SetForegroundWindow(Handle1);
    Result := (windows.GetForegroundWindow = Handle1);
    if Result then Exit;

  end;

  // 4. playing tricks here then!
  AniInfo.cbSize:= SizeOf(TAnimationInfo);
  if SystemParametersInfo(SPI_GETANIMATION, SizeOf(AniInfo), @AniInfo, 0) then
  begin
    Animate:= AniInfo.iMinAnimate<>0;
    if Animate then
    begin
      AniInfo.iMinAnimate:= 0;
      SystemParametersInfo(SPI_SETANIMATION, SizeOf(AniInfo), @AniInfo, 0);
    end;
  end else
    Animate:= False;

  SendMessage(Handle1,WM_SYSCOMMAND,SC_MINIMIZE,0);
  Sleep(40);
  if hParent>0 then SendMessage(hParent,WM_SYSCOMMAND,SC_RESTORE,0)
    else SendMessage(Handle1,WM_SYSCOMMAND,SC_RESTORE,0);

  if Animate then
  begin
    AniInfo.iMinAnimate:= 1;
    SystemParametersInfo(SPI_SETANIMATION, SizeOf(AniInfo), @AniInfo, 0);
  end;
  Result:= (windows.GetForegroundWindow=Handle1);
end;

function TAPI_windows.ForceForeground(handle: hwnd): boolean;
begin
  result:= api_windows.forceforeground(handle);
end;

//------------------------------------------------------------------------------
function TAPI_windows.GetListColumn( const index: integer;
  var caption: string;
  var wclassname: string;
  var whandle: cardinal;
  var memorysize: cardinal;
  var filename: string ): boolean;
var
  tmp: string;
  p: integer;
begin
  result:= false;
  if (index>-1) and (index<flist.count) then
  begin
    tmp:= flist[index];

    // caption
    p:= pos( '|', tmp );
    if p>0 then
    begin
      caption:= copy(tmp, 1, p-1);
      delete(tmp, 1, p);
    end else
      exit;

    // class name
    p:= pos( '|', tmp );
    if p>0 then
    begin
      wclassname:= copy(tmp, 1, p-1);
      delete(tmp, 1, p);
    end else
      exit;

    // handle
    p:= pos( '|', tmp );
    if p>0 then
    try
      whandle:= strtoint( copy(tmp, 1, p-1) );
      delete(tmp, 1, p);
    except
      whandle:= 0;
      exit;
    end;

    // memory size
    p:= pos( '|', tmp );
    if p>0 then
    try
      memorysize:= strtoint( copy(tmp, 1, p-1) );
      delete(tmp, 1, p);
    except
      memorysize:= 0;
      exit;
    end;

    // file name
    p:= pos( '|', tmp );
    if p>0 then
    begin
      filename:= copy(tmp, 1, p-1);
      delete(tmp, 1, p);

      result:= true;
    end;

  end;
end;

//------------------------------------------------------------------------------
function TAPI_windows.HandleFromIndex(index: integer): cardinal;
var
  ca, cl, fn: string;
  wh, me: cardinal;
begin
  if getlistcolumn( index, ca, cl, wh, me, fn ) then result:= wh
    else result:= 0;
end;

function TAPI_windows.FilenameFromIndex(index: integer): string;
var
  ca, cl, fn: string;
  wh, me: cardinal;
begin
  if getlistcolumn( index, ca, cl, wh, me, fn ) then result:= fn
    else result:= '';
end;

function TAPI_windows.CaptionFromIndex(index: integer): string;
var
  ca, cl, fn: string;
  wh, me: cardinal;
begin
  if getlistcolumn( index, ca, cl, wh, me, fn ) then result:= ca
    else result:= '';
end;

function TAPI_windows.MemoryUsageFromIndex(index: integer): cardinal;
var
  ca, cl, fn: string;
  wh, me: cardinal;
begin
  if getlistcolumn( index, ca, cl, wh, me, fn ) then result:= me
    else result:= 0;
end;

function TAPI_windows.ClassNameFromIndex(index: integer): string;
var
  ca, cl, fn: string;
  wh, me: cardinal;
begin
  if getlistcolumn( index, ca, cl, wh, me, fn ) then result:= cl
    else result:= '';
end;

//------------------------------------------------------------------------------
function TAPI_windows.HandleFromCaption(Caption: string): cardinal;     // handle
var
  i: integer;
begin
  UpdateList;
  result:= 0;
  for i:=0 to flist.count-1 do
    if ansipos( caption, captionfromindex( i ) )>0 then
    begin
      result:= handlefromindex( i );
      break;
    end;
end;

function TAPI_windows.HandleFromFilename(Filename: string): cardinal;     // handle
var
  i: integer;
begin
  UpdateList;
  result:= 0;
  for i:=0 to flist.count-1 do
    if ansipos( filename, filenamefromindex( i ) )>0 then
    begin
      result:= handlefromindex( i );
      break;
    end;
end;

function TAPI_windows.Caption(Handle: hwnd): string;      // caption
var
  i: integer;
begin
  UpdateList;
  result:= '';
  for i:=0 to flist.count-1 do
    if (handlefromindex(i) = handle) then
    begin
      result:= captionfromindex( i );
      break;
    end;
end;

function TAPI_windows.Filename(Handle: hwnd): string;                 // filename
var
  i: integer;
begin
  UpdateList;
  result:= '';
  for i:=0 to flist.count-1 do
    if (handlefromindex(i) = handle) then
    begin
      result:= filenamefromindex(i);
      break;
    end;
end;

function TAPI_windows.MemoryUsage(Handle: hwnd): cardinal;         // bytes
var
  i: integer;
begin
  UpdateList;
  result:= 0;
  for i:=0 to flist.count-1 do
    if (handlefromindex(i) = handle) then
    begin
      result:= memoryusagefromindex( i );
      break;
    end;
end;

//------------------------------------------------------------------------------
function TAPI_windows.GetList: tstringlist;
begin
  result:= tstringlist.create;
  result.clear;
  UpdateList;
  result.Text:= flist.text;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Comm', [TAPI_windows]);
end;

end.
