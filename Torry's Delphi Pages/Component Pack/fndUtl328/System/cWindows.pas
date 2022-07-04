{$INCLUDE ..\cDefines.inc}
unit cWindows;

{                                                                              }
{                          Windows functions v3.07                             }
{                                                                              }
{             This unit is copyright © 2000-2004 by David J Butler             }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                    Its original file name is cWindows.pas                    }
{       The latest version is available from the Fundamentals home page        }
{                     http://fundementals.sourceforge.net/                     }
{                                                                              }
{                I invite you to use this unit, free of charge.                }
{        I invite you to distibute this unit, but it must be for free.         }
{             I also invite you to contribute to its development,              }
{             but do not distribute a modified copy of this file.              }
{                                                                              }
{          A forum is available on SourceForge for general discussion          }
{             http://sourceforge.net/forum/forum.php?forum_id=2117             }
{                                                                              }
{ Description:                                                                 }
{   MS Windows specific functions.                                             }
{                                                                              }
{ Revision history:                                                            }
{   2000/10/01  1.01  Initial version created from cUtils.                     }
{   2001/12/12  2.02  Added AWindowHandle.                                     }
{   2002/03/15  2.03  Added GetWinOSType.                                      }
{   2002/06/26  3.04  Refactored for Fundamentals 3.                           }
{   2002/09/22  3.05  Moved Registry functions to unit cRegistry.              }
{   2003/01/04  3.06  Added Reboot function.                                   }
{   2003/10/01  3.07  Updated GetWindowsVersion function.                      }
{                                                                              }

interface

uses
  { Delphi }
  Windows,
  Messages,
  SysUtils,
  Classes,

  { Fundamentals }
  cUtils;



{                                                                              }
{ Windows Version                                                              }
{                                                                              }
type
  TWindowsVersion = (
      // 16-bit Windows
      Win16_31,
      // 32-bit Windows
      Win32_95, Win32_95R2, Win32_98, Win32_98SE, Win32_ME, Win32_Future,
      // Windows NT 3
      WinNT_31, WinNT_35, WinNT_351,
      // Windows NT 4
      WinNT_40,
      // Windows NT 5
      WinNT5_2000, WinNT5_XP, WinNT5_2003, WinNT5_Future,
      // Windows NT 6+
      WinNT_Future,
      // Windows Post-NT
      Win_Future);
  TWindowsVersions = Set of TWindowsVersion;

function  GetWindowsVersion: TWindowsVersion;
function  IsWinPlatform95: Boolean;
function  IsWinPlatformNT: Boolean;
function  GetWindowsProductID: String;



{                                                                              }
{ Windows Paths                                                                }
{                                                                              }
function  GetWindowsTemporaryPath: String;
function  GetWindowsPath: String;
function  GetWindowsSystemPath: String;
function  GetProgramFilesPath: String;
function  GetCommonFilesPath: String;
function  GetApplicationPath: String;



{                                                                              }
{ Identification                                                               }
{                                                                              }
function  GetUserName: String;
function  GetLocalComputerName: String;



{                                                                              }
{ Application Version Info                                                     }
{                                                                              }
type
  TVersionInfo = (viFileVersion, viFileDescription, viLegalCopyright,
                  viComments, viCompanyName, viInternalName,
                  viLegalTrademarks, viOriginalFilename, viProductName,
                  viProductVersion);

function  GetAppVersionInfo(const VersionInfo: TVersionInfo): String;



{                                                                              }
{ Windows Processes                                                            }
{                                                                              }
function  WinExecute(const ExeName, Params: String;
          const ShowWin: Word = SW_SHOWNORMAL;
          const Wait: Boolean = True): Boolean;



{                                                                              }
{ Exit Windows                                                                 }
{                                                                              }
{$IFNDEF FREEPASCAL}
type
  TExitWindowsType = (exitLogOff, exitPowerOff, exitReboot, exitShutDown);

function  ExitWindows(const ExitType: TExitWindowsType;
          const Force: Boolean = False): Boolean;

function  LogOff(const Force: Boolean = False): Boolean;
function  PowerOff(const Force: Boolean = False): Boolean;
function  Reboot(const Force: Boolean = False): Boolean;
function  ShutDown(const Force: Boolean = False): Boolean;
{$ENDIF}



{                                                                              }
{ Windows Fibers                                                               }
{   These functions are redeclared because Delphi 7's Windows.pas declare      }
{   them incorrectly.                                                          }
{                                                                              }
{$IFDEF FREEPASCAL}
type
  TFNFiberStartRoutine = TFarProc;
{$ENDIF}

function  ConvertThreadToFiber(lpParameter: Pointer): Pointer; stdcall;
function  CreateFiber(dwStackSize: DWORD; lpStartAddress: TFNFiberStartRoutine;
          lpParameter: Pointer): Pointer; stdcall;



{                                                                              }
{ Windows Shell                                                                }
{                                                                              }
procedure ShellLaunch(const S: String);



{                                                                              }
{ Miscelleaneous Windows API                                                   }
{                                                                              }
function  GetEnvironmentStrings: StringArray;

function  ContentTypeFromExtention(const Extention: String): String;
function  FileClassFromExtention(const Extention: String): String;
function  GetFileClass(const FileName: String): String;
function  GetFileAssociation(const FileName: String): String;

function  IsApplicationAutoRun(const Name: String): Boolean;
procedure SetApplicationAutoRun(const Name: String; const AutoRun: Boolean);

{$IFNDEF FREEPASCAL}
function  GetWinPortNames: StringArray;
{$ENDIF}

function  GetKeyPressed(const VKeyCode: Integer): Boolean;

function  GetHardDiskSerialNumber(const DriveLetter: Char): String;



{                                                                              }
{ WinInet API                                                                  }
{                                                                              }
type
  TIEProxy = (iepHTTP, iepHTTPS, iepFTP, iepGOPHER, iepSOCKS);

{$IFNDEF FREEPASCAL}
function  GetIEProxy(const Protocol: TIEProxy): String;
{$ENDIF}



{                                                                              }
{ Window Handle                                                                }
{   Base class for allocation of a new Window handle that can process its own  }
{   messages.                                                                  }
{                                                                              }
type
  TWindowHandleMessageEvent = function (const Msg: Cardinal; const wParam, lParam: Integer;
      var Handled: Boolean): Integer of object;
  TWindowHandle = class;
  TWindowHandleEvent = procedure (const Sender: TWindowHandle) of object;
  TWindowHandleErrorEvent = procedure (const Sender: TWindowHandle;
      const E: Exception) of object;
  TWindowHandle = class(TComponent)
  protected
    FWindowHandle    : HWND;
    FTerminated      : Boolean;
    FOnMessage       : TWindowHandleMessageEvent;
    FOnException     : TWindowHandleErrorEvent;
    FOnBeforeMessage : TWindowHandleEvent;
    FOnAfterMessage  : TWindowHandleEvent;

    procedure RaiseError(const Msg: String);
    function  AllocateWindowHandle: HWND; virtual;
    function  MessageProc(const Msg: Cardinal; const wParam, lParam: Integer): Integer;
    function  HandleWM(const Msg: Cardinal; const wParam, lParam: Integer): Integer; virtual;

  public
    destructor Destroy; override;

    procedure DestroyWindowHandle; virtual;
    property  WindowHandle: HWND read FWindowHandle;
    function  GetWindowHandle: HWND;

    function  ProcessMessage: Boolean;
    procedure ProcessMessages;
    function  HandleMessage: Boolean;
    procedure MessageLoop;

    property  OnMessage: TWindowHandleMessageEvent read FOnMessage write FOnMessage;
    property  OnException: TWindowHandleErrorEvent read FOnException write FOnException;
    property  OnBeforeMessage: TWindowHandleEvent read FOnBeforeMessage write FOnBeforeMessage;
    property  OnAfterMessage: TWindowHandleEvent read FOnAfterMessage write FOnAfterMessage;

    property  Terminated: Boolean read FTerminated;
    procedure Terminate; virtual;
  end;
  EWindowHandle = class(Exception);

  { TfndWindowHandle                                                           }
  {   Published Window Handle component.                                       }
  TfndWindowHandle = class(TWindowHandle)
  published
    property  OnMessage;
    property  OnException;
  end;



{                                                                              }
{ TTimerHandle                                                                 }
{                                                                              }
type
  TTimerHandle = class;
  TTimerEvent = procedure (const Sender: TTimerHandle) of object;
  TTimerHandle = class(TWindowHandle)
  protected
    FTimerInterval : Integer;
    FTimerActive   : Boolean;
    FOnTimer       : TTimerEvent;

    function  HandleWM(const Msg: Cardinal; const wParam, lParam: Integer): Integer; override;
    function  DoSetTimer: Boolean;
    procedure TriggerTimer; virtual;
    procedure SetTimerActive(const TimerActive: Boolean); virtual;
    procedure Loaded; override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure DestroyWindowHandle; override;

    property  TimerInterval: Integer read FTimerInterval write FTimerInterval default 1000;
    property  TimerActive: Boolean read FTimerActive write SetTimerActive default False;
    property  OnTimer: TTimerEvent read FOnTimer write FOnTimer;
  end;

  { TfndTimerHandle                                                            }
  {   Published Timer Handle component.                                        }
  TfndTimerHandle = class(TTimerHandle)
  published
    property  OnMessage;
    property  OnException;
    property  TimerInterval;
    property  TimerActive;
    property  OnTimer;
  end;



{$IFNDEF DELPHI6_UP}
{                                                                              }
{ RaiseLastOSError                                                             }
{                                                                              }
procedure RaiseLastOSError;
{$ENDIF}



implementation

uses
  { Delphi }
  {$IFNDEF FREEPASCAL}
  WinSpool,
  WinInet,
  {$ENDIF}
  ShellApi,

  { Fundamentals }
  cStrings,
  cRegistry;



{$IFNDEF DELPHI6_UP}
{                                                                              }
{ RaiseLastOSError                                                             }
{                                                                              }
procedure RaiseLastOSError;
begin
  {$IFDEF FREEPASCAL}
  raise Exception.Create('OS Error');
  {$ELSE}
  RaiseLastWin32Error;
  {$ENDIF}
end;
{$ENDIF}



{                                                                              }
{ Windows Version Info                                                         }
{                                                                              }
{$IFDEF FREEPASCAL}
var
  Win32Platform     : Integer;
  Win32MajorVersion : Integer;
  Win32MinorVersion : Integer;
  Win32CSDVersion   : String;

procedure InitPlatformId;
var OSVersionInfo : TOSVersionInfo;
begin
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
  if GetVersionEx(OSVersionInfo) then
    with OSVersionInfo do
      begin
        Win32Platform := dwPlatformId;
        Win32MajorVersion := dwMajorVersion;
        Win32MinorVersion := dwMinorVersion;
        Win32CSDVersion := szCSDVersion;
      end;
end;
{$ENDIF}

function GetWindowsVersion: TWindowsVersion;
begin
  Case Win32Platform of
    VER_PLATFORM_WIN32s :
      Result := Win16_31;
    VER_PLATFORM_WIN32_WINDOWS :
      if Win32MajorVersion <= 4 then
        Case Win32MinorVersion of
          0..9   : if Trim(Win32CSDVersion, csWhiteSpace) = 'B' then
                     Result := Win32_95R2 else
                     Result := Win32_95;
          10..89 : if Trim(Win32CSDVersion, csWhiteSpace) = 'A' then
                     Result := Win32_98SE else
                     Result := Win32_98;
          90..99 : Result := Win32_ME;
        else
          Result := Win32_Future;
        end
      else
        Result := Win32_Future;
    VER_PLATFORM_WIN32_NT :
      Case Win32MajorVersion of
        3 : Case Win32MinorVersion of
              1, 10..19 : Result := WinNT_31;
              5, 50     : Result := WinNT_35;
              51..99    : Result := WinNT_351;
            else
              Result := WinNT_31;
            end;
        4 : Result := WinNT_40;
        5 : Case Win32MinorVersion of
               0 : Result := WinNT5_2000;
               1 : Result := WinNT5_XP;
               2 : Result := WinNT5_2003;
            else
              Result := WinNT5_Future;
            end;
      else
        Result := WinNT_Future;
      end;
  else
    Result := Win_Future;
  end;
end;

function IsWinPlatform95: Boolean;
begin
  Result := Win32Platform = VER_PLATFORM_WIN32_WINDOWS;
end;

function IsWinPlatformNT: Boolean;
begin
  Result := Win32Platform = VER_PLATFORM_WIN32_NT;
end;

function GetWindowsProductID: String;
begin
  Result := GetRegistryString(HKEY_LOCAL_MACHINE,
         'Software\Microsoft\Windows\CurrentVersion', 'ProductId');
end;



{                                                                              }
{ Windows Paths                                                                }
{                                                                              }
function GetWindowsTemporaryPath: String;
const MaxTempPathLen = MAX_PATH + 1;
var I : LongWord;
begin
  SetLength(Result, MaxTempPathLen);
  I := GetTempPath(MaxTempPathLen, PChar(Result));
  if I > 0 then
    SetLength(Result, I) else
    Result := '';
end;

function GetWindowsPath: String;
const MaxWinPathLen = MAX_PATH + 1;
var I : LongWord;
begin
  SetLength(Result, MaxWinPathLen);
  I := GetWindowsDirectory(PChar(Result), MaxWinPathLen);
  if I > 0 then
    SetLength(Result, I) else
    Result := '';
end;

function GetWindowsSystemPath: String;
const MaxWinSysPathLen = MAX_PATH + 1;
var I : LongWord;
begin
  SetLength(Result, MaxWinSysPathLen);
  I := GetSystemDirectory(PChar(Result), MaxWinSysPathLen);
  if I > 0 then
    SetLength(Result, I) else
    Result := '';
end;

const
  CurrentVersionRegistryKey = 'SOFTWARE\Microsoft\Windows\CurrentVersion';

function GetProgramFilesPath: String;
begin
  Result := GetRegistryString(HKEY_LOCAL_MACHINE, CurrentVersionRegistryKey,
      'ProgramFilesDir');
end;

function GetCommonFilesPath: String;
begin
  Result := GetRegistryString(HKEY_LOCAL_MACHINE, CurrentVersionRegistryKey,
      'CommonFilesDir');
end;

function GetApplicationPath: String;
begin
  Result := ExtractFilePath(ParamStr(0));
  StrEnsureSuffix(Result, '\');
end;



{                                                                              }
{ Identification                                                               }
{                                                                              }
function GetUserName: String;
const MAX_USERNAME_LENGTH = 256;
var L : LongWord;
begin
  L := MAX_USERNAME_LENGTH + 2;
  SetLength(Result, L);
  if Windows.GetUserName(PChar(Result), L) and (L > 0) then
    SetLength(Result, StrLen(PChar(Result))) else
    Result := '';
end;

function GetLocalComputerName: String;
var L : LongWord;
begin
  L := MAX_COMPUTERNAME_LENGTH + 2;
  SetLength(Result, L);
  if Windows.GetComputerName(PChar(Result), L) and (L > 0) then
    SetLength(Result, StrLen(PChar(Result))) else
    Result := '';
end;



{                                                                              }
{ Application Version Info                                                     }
{                                                                              }
var
  VersionInfoBuf : Pointer = nil;
  VerTransStr    : String;

// Returns True if VersionInfo is available
function LoadAppVersionInfo: Boolean;
type TTransBuffer = Array [1..4] of SmallInt;
     PTransBuffer = ^TTransBuffer;
var InfoSize : Integer;
    Size, H  : LongWord;
    EXEName  : String;
    Trans    : PTransBuffer;
begin
  Result := Assigned(VersionInfoBuf);
  if Result then
    exit;
  EXEName := ParamStr(0);
  InfoSize := GetFileVersionInfoSize(PChar(EXEName), H);
  if InfoSize = 0 then
    exit;
  GetMem(VersionInfoBuf, InfoSize);
  if not GetFileVersionInfo(PChar(EXEName), H, InfoSize, VersionInfoBuf) then
    begin
      FreeMem(VersionInfoBuf);
      VersionInfoBuf := nil;
      exit;
    end;
  VerQueryValue(VersionInfoBuf, PChar('\VarFileInfo\Translation'),
                 Pointer(Trans), Size);
  VerTransStr := IntToHex(Trans^ [1], 4) + IntToHex(Trans^ [2], 4);
  Result := True;
end;

const
  VersionInfoStr: Array [TVersionInfo] of String =
    ('FileVersion', 'FileDescription', 'LegalCopyright', 'Comments',
     'CompanyName', 'InternalName', 'LegalTrademarks',
     'OriginalFilename', 'ProductName', 'ProductVersion');

function GetAppVersionInfo(const VersionInfo: TVersionInfo): String;
var S     : String;
    Size  : LongWord;
    Value : PChar;
begin
  Result := '';
  if not LoadAppVersionInfo then
    exit;
  S := 'StringFileInfo\' + VerTransStr + '\' + VersionInfoStr[VersionInfo];
  if VerQueryvalue(VersionInfoBuf, PChar(S), Pointer(Value), Size) then
    Result := Value;
end;



{                                                                              }
{ Windows Processes                                                            }
{                                                                              }
function WinExecute(const ExeName, Params: String; const ShowWin: Word;
    const Wait: Boolean): Boolean;
var StartUpInfo : TStartupInfo;
    ProcessInfo	: TProcessInformation;
    Cmd         : String;
begin
  if Params = '' then
    Cmd := ExeName else
    Cmd := ExeName + ' ' + Params;
  FillChar(StartUpInfo, SizeOf(StartUpInfo), #0);
  StartUpInfo.cb := SizeOf(StartUpInfo);
  StartUpInfo.dwFlags := STARTF_USESHOWWINDOW; // STARTF_USESTDHANDLES
  StartUpInfo.wShowWindow := ShowWin;
  Result := CreateProcess(
           nil, PChar(Cmd), nil, nil, False,
           CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil,
           PChar(ExtractFilePath(ExeName)), StartUpInfo, ProcessInfo);
  if Wait then
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
end;



{                                                                              }
{ Exit Windows                                                                 }
{                                                                              }
{$IFNDEF FREEPASCAL}
function ExitWindows(const ExitType: TExitWindowsType; const Force: Boolean): Boolean;
const SE_SHUTDOWN_NAME = 'SeShutDownPrivilege';
      ExitTypeFlags : Array[TExitWindowsType] of Cardinal =
          (EWX_LOGOFF, EWX_POWEROFF, EWX_REBOOT, EWX_SHUTDOWN);
var hToken : Cardinal;
    tkp    : TTokenPrivileges;
    retval : Cardinal;
    uFlags : Cardinal;
begin
  if IsWinPlatformNT then
    if OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then
      begin
        LookupPrivilegeValue(nil, SE_SHUTDOWN_NAME, tkp.Privileges[0].Luid);
        tkp.PrivilegeCount := 1;
        tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
        AdjustTokenPrivileges(hToken, false, tkp, 0, tkp, retval);
      end;
  uFlags := ExitTypeFlags[ExitType];
  if Force then
    uFlags := uFlags or EWX_FORCE;
  Result := Windows.ExitWindowsEx(uFlags, 0);
end;

function LogOff(const Force: Boolean = False): Boolean;
begin
  Result := ExitWindows(exitLogOff, Force);
end;

function PowerOff(const Force: Boolean = False): Boolean;
begin
  Result := ExitWindows(exitPowerOff, Force);
end;

function Reboot(const Force: Boolean): Boolean;
begin
  Result := ExitWindows(exitReboot, Force);
end;

function ShutDown(const Force: Boolean = False): Boolean;
begin
  Result := ExitWindows(exitShutDown, Force);
end;
{$ENDIF}



{                                                                              }
{ Windows Fibers                                                               }
{                                                                              }
function ConvertThreadToFiber; external kernel32 name 'ConvertThreadToFiber';
function CreateFiber(dwStackSize: DWORD; lpStartAddress: TFNFiberStartRoutine;
         lpParameter: Pointer): Pointer; external kernel32 name 'CreateFiber';



{                                                                              }
{ Windows Shell                                                                }
{                                                                              }
procedure ShellLaunch(const S: String);
begin
  ShellExecute(0, 'open', PChar(S), '', '', SW_SHOWNORMAL);
end;



{                                                                              }
{ Miscelleaneous Windows API                                                   }
{                                                                              }
function GetEnvironmentStrings: StringArray;
var P, Q : PChar;
    I : Integer;
    S : String;
begin
  P := PChar(Windows.GetEnvironmentStrings);
  try
    if P^ <> #0 then
      Repeat
        Q := P;
        I := 0;
        While Q^ <> #0 do
          begin
            Inc(Q);
            Inc(I);
          end;
        SetLength(S, I);
        if I > 0 then
          Move(P^, Pointer(S)^, I);
        Append(Result, S);
        P := Q;
        Inc(P);
      Until P^ = #0;
  finally
    FreeEnvironmentStrings(P);
  end;
end;

function ContentTypeFromExtention(const Extention: String): String;
begin
  Result := GetRegistryString(HKEY_CLASSES_ROOT, Extention, 'Content Type');
end;

function FileClassFromExtention(const Extention: String): String;
begin
  Result := GetRegistryString(HKEY_CLASSES_ROOT, Extention, '');
end;

function GetFileClass(const FileName: String): String;
begin
  Result := FileClassFromExtention(ExtractFileExt(FileName));
end;

function GetFileAssociation(const FileName: String): String;
var S : String;
begin
  S := FileClassFromExtention(ExtractFileExt(FileName));
  if S = '' then
    Result := ''
  else
    Result := GetRegistryString(HKEY_CLASSES_ROOT, S + '\Shell\Open\Command', '');
end;

const
  AutoRunRegistryKey = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Run';

function IsApplicationAutoRun(const Name: String): Boolean;
var S : String;
begin
  S := ParamStr(0);
  Result := (S <> '') and (Name <> '') and
      StrEqualNoCase(GetRegistryString(HKEY_LOCAL_MACHINE, AutoRunRegistryKey, Name), S);
end;

procedure SetApplicationAutoRun(const Name: String; const AutoRun: Boolean);
begin
  if Name = '' then
    exit;
  if AutoRun then
    SetRegistryString(HKEY_LOCAL_MACHINE, AutoRunRegistryKey, Name, ParamStr(0))
  else
    DeleteRegistryValue(HKEY_LOCAL_MACHINE, AutoRunRegistryKey, Name);
end;

{$IFNDEF FREEPASCAL}
function GetWinPortNames: StringArray;
var BytesNeeded, N, I : LongWord;
    Buf : Pointer;
    InfoPtr : PPortInfo1;
    TempStr : String;
begin
  Result := nil;
  if EnumPorts(nil, 1, nil, 0, BytesNeeded, N) then
    exit;
  if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
    RaiseLastOSError;
  GetMem(Buf, BytesNeeded);
  try
    if not EnumPorts(nil, 1, Buf, BytesNeeded, BytesNeeded, N) then
      RaiseLastOSError;
    For I := 0 to N - 1 do
      begin
        InfoPtr := PPortInfo1(LongWord(Buf) + I * SizeOf(TPortInfo1));
        TempStr := InfoPtr^.pName;
        Append(Result, TempStr);
      end;
  finally
    FreeMem(Buf);
  end;
end;
{$ENDIF}

function GetKeyPressed(const VKeyCode: Integer): Boolean;
begin
  Result := GetKeyState(VKeyCode) and $80 <> 0;
end;



{                                                                              }
{ WinInet API                                                                  }
{                                                                              }
const
  IEProtoPrefix : Array[TIEProxy] of String =
      ('http=', 'https=', 'ftp=', 'gopher=', 'socks=');

{$IFNDEF FREEPASCAL}
function GetIEProxy(const Protocol: TIEProxy): String;
var ProxyInfo : PInternetProxyInfo;
    Len       : LongWord;
    Proxies   : StringArray;
    I         : Integer;
begin
  Proxies := nil;
  Result := '';
  Len := 4096;
  GetMem(ProxyInfo, Len);
  try
    if InternetQueryOption(nil, INTERNET_OPTION_PROXY, ProxyInfo, Len) then
      if ProxyInfo^.dwAccessType = INTERNET_OPEN_TYPE_PROXY then
        begin
          Result := ProxyInfo^.lpszProxy;
          if PosChar('=', Result) = 0 then // same proxy for all protocols
            exit;
          // Find proxy for Protocol
          Proxies := StrSplitChar(Result, ' ');
          For I := 0 to Length(Proxies) - 1 do
            if StrMatchLeft(Proxies[I], IEProtoPrefix[Protocol], False) then
              begin
                Result := StrAfterChar(Proxies[I], '=');
                exit;
              end;
          // No proxy for Protocol
          Result := '';
        end;
  finally
    FreeMem(ProxyInfo);
  end;
end;
{$ENDIF}

function GetHardDiskSerialNumber(const DriveLetter: Char): String;
var N, F, S : DWORD;
begin
  S := 0;
  GetVolumeInformation(PChar(DriveLetter + ':\'), nil, MAX_PATH + 1, @S,
      N, F, nil, 0);
  Result := LongWordToHex(S, 8);
end;



{                                                                              }
{ TWindowHandle                                                                }
{                                                                              }
function WindowHandleMessageProc(const WindowHandle: HWND; const Msg: Cardinal;
    const wParam, lParam: Integer): Integer; stdcall;
var V : TObject;
begin
  V := TObject(GetWindowLong(WindowHandle, 0)); // Get user data
  if V is TWindowHandle then
    Result := TWindowHandle(V).MessageProc(Msg, wParam, lParam) else
    Result := DefWindowProc(WindowHandle, Msg, wParam, lParam); // Default handler
end;

var
  WindowClass: TWndClass = (
      style         : 0;
      lpfnWndProc   : @WindowHandleMessageProc;
      cbClsExtra    : 0;
      cbWndExtra    : SizeOf(Pointer); // Size of extra user data
      hInstance     : 0;
      hIcon         : 0;
      hCursor       : 0;
      hbrBackground : 0;
      lpszMenuName  : nil;
      lpszClassName : 'FundamentalsWindowClass');

Destructor TWindowHandle.Destroy;
begin
  DestroyWindowHandle;
  inherited Destroy;
end;

procedure TWindowHandle.RaiseError(const Msg: String);
begin
  raise EWindowHandle.Create(Msg);
end;

function TWindowHandle.AllocateWindowHandle: HWND;
var C : TWndClass;
begin
  WindowClass.hInstance := HInstance;
  // Register class
  if not GetClassInfo(HInstance, WindowClass.lpszClassName, C) then
    if Windows.RegisterClass(WindowClass) = 0 then
      RaiseError('Window class registration failed: Windows error #' + IntToStr(GetLastError));

  // Allocate handle
  Result := CreateWindowEx(WS_EX_TOOLWINDOW,
                           WindowClass.lpszClassName,
                           '',        { Window name   }
                           WS_POPUP,  { Window Style  }
                           0, 0,      { X, Y          }
                           0, 0,      { Width, Height }
                           0,         { hWndParent    }
                           0,         { hMenu         }
                           HInstance, { hInstance     }
                           nil);      { CreateParam   }
  if Result = 0 then
    RaiseError('Window handle allocation failed: Windows error #' + IntToStr(GetLastError));

  // Set user data
  SetWindowLong(Result, 0, Integer(self));
end;

function TWindowHandle.HandleWM(const Msg: Cardinal; const wParam, lParam: Integer): Integer;
var Handled : Boolean;
begin
  Result := 0;
  Handled := False;
  if Assigned(FOnMessage) then
    Result := FOnMessage(Msg, wParam, lParam, Handled);
  if not Handled then
    Result := DefWindowProc(FWindowHandle, Msg, wParam, lParam); // Default handler
end;

function TWindowHandle.MessageProc(const Msg: Cardinal; const wParam, lParam: Integer): Integer;
var R : Boolean;
begin
  if Assigned(FOnBeforeMessage) then
    FOnBeforeMessage(self);
  R := Assigned(FOnAfterMessage);
  try
    try
      Result := HandleWM(Msg, wParam, lParam);
    except
      on E : Exception do
        begin
          if Assigned(FOnException) then
            FOnException(self, E);
          Result := 0;
        end;
    end;
  finally
    if R then
      if Assigned(FOnAfterMessage) then
        FOnAfterMessage(self);
  end;
end;

function TWindowHandle.GetWindowHandle: HWND;
begin
  Result := FWindowHandle;
  if Result = 0 then
    begin
      FWindowHandle := AllocateWindowHandle;
      Result := FWindowHandle;
    end;
end;

procedure TWindowHandle.DestroyWindowHandle;
begin
  if FWindowHandle = 0 then
    exit;

  // Clear user data
  SetWindowLong(FWindowHandle, 0, 0);

  DestroyWindow(FWindowHandle);
  FWindowHandle := 0;
end;

function TWindowHandle.ProcessMessage: Boolean;
var Msg : Windows.TMsg;
begin
  if FTerminated then
    begin
      Result := False;
      exit;
    end;
  Result := PeekMessage(Msg, 0, 0, 0, PM_REMOVE);
  if Result then
    if Msg.Message = WM_QUIT then
      FTerminated := True else
      if FTerminated then
        Result := False else
        begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end;
end;

procedure TWindowHandle.ProcessMessages;
begin
  While ProcessMessage do ;
end;

function TWindowHandle.HandleMessage: Boolean;
var Msg : Windows.TMsg;
begin
  if FTerminated then
    begin
      Result := False;
      exit;
    end;
  Result := GetMessage(Msg, 0, 0, 0);
  if not Result then
    FTerminated := True else
    if FTerminated then
      Result := False else
      begin
        TranslateMessage(Msg);
        DispatchMessage(Msg)
      end;
end;

procedure TWindowHandle.MessageLoop;
begin
  While HandleMessage do ;
end;

procedure TWindowHandle.Terminate;
begin
  FTerminated := True;
end;



{                                                                              }
{ TTimerHandle                                                                 }
{                                                                              }
Constructor TTimerHandle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimerInterval := 1000;
end;

procedure TTimerHandle.DestroyWindowHandle;
begin
  if not (csDesigning in ComponentState) and (FWindowHandle <> 0) and
      FTimerActive then
    KillTimer(FWindowHandle, 1);
  inherited DestroyWindowHandle;
end;

function TTimerHandle.DoSetTimer: Boolean;
begin
  if FTimerInterval <= 0 then
    Result := False
  else
    Result := SetTimer (GetWindowHandle, 1, FTimerInterval, nil) = 0;
end;

procedure TTimerHandle.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) and FTimerActive then
    DoSetTimer;
end;

procedure TTimerHandle.TriggerTimer;
begin
  if Assigned(FOnTimer) then
    FOnTimer(self);
end;

procedure TTimerHandle.SetTimerActive(const TimerActive: Boolean);
begin
  if FTimerActive = TimerActive then
    exit;
  if [csDesigning, csLoading] * ComponentState = [] then
    if TimerActive then
      begin
        if not DoSetTimer then
          exit;
      end else
      KillTimer(FWindowHandle, 1);
  FTimerActive := TimerActive;
end;

function TTimerHandle.HandleWM(const Msg: Cardinal; const wParam, lParam: Integer): Integer;
begin
  if Msg = WM_TIMER then
    try
      Result := 0;
      TriggerTimer;
    except
      on E: Exception do
        begin
          Result := 0;
          if Assigned(FOnException) then
            FOnException(self, E);
          exit;
        end;
    end
  else
    Result := inherited HandleWM(Msg, wParam, lParam);
end;



initialization
finalization
  if Assigned(VersionInfoBuf) then
    FreeMem(VersionInfoBuf);
end.

