{$INCLUDE ..\cDefines.inc}
unit cWindows;

{                                                                              }
{                          Windows functions v3.06                             }
{                                                                              }
{      This unit is copyright © 2000-2003 by David Butler (david@e.co.za)      }
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
{ Windows Version Info                                                         }
{                                                                              }
type
  TWinOSType = (win31,
                win32_95, win32_98, win32_ME,
                win32_NT, win32_2000, win32_XP,
                win_UnknownPlatform);

function  GetWinOSType: TWinOSType;
function  IsWinNTFamily: Boolean;
function  IsWin95Family: Boolean;



{                                                                              }
{ Windows Paths                                                                }
{                                                                              }
function  GetWindowsTemporaryPath: String;
function  GetWindowsPath: String;
function  GetWindowsSystemPath: String;
function  GetProgramFilesPath: String;
function  GetApplicationPath: String;



{                                                                              }
{ Identification                                                               }
{                                                                              }
function  GetUserName: String;
function  GetLocalComputerName: String;
function  GetLocalHostName: String;



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
{ Windows Fibers                                                               }
{   These functions are redeclared because Delphi 7's Windows.pas declare      }
{   them incorrectly.                                                          }
{                                                                              }
function ConvertThreadToFiber(lpParameter: Pointer): Pointer; stdcall;
function CreateFiber(dwStackSize: DWORD; lpStartAddress: TFNFiberStartRoutine;
         lpParameter: Pointer): Pointer; stdcall;



{                                                                              }
{ Miscelleaneous Windows API                                                   }
{                                                                              }
function  GetEnvironmentStrings: StringArray;

function  ContentTypeFromExtention (Extention: String): String;

function  IsApplicationAutoRun(const Name: String): Boolean;
procedure SetApplicationAutoRun(const Name: String; const AutoRun: Boolean);

function  GetWinPortNames: StringArray;

function  GetKeyPressed(const VKeyCode: Integer): Boolean;

function  GetHardDiskSerialNumber(const DriveLetter: Char): String;
function  GetWindowsProductID: String;

function  Reboot: Boolean;



{                                                                              }
{ WinInet API                                                                  }
{                                                                              }
type
  TIEProxy = (iepHTTP, iepHTTPS, iepFTP, iepGOPHER, iepSOCKS);

function GetIEProxy(const Protocol: TIEProxy): String;



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
  TTimerEvent = procedure(const Sender: TTimerHandle) of object;
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
  WinSock,
  WinSpool,
  WinInet,

  { Fundamentals }
  cStrings,
  cRegistry;



{$IFNDEF DELPHI6_UP}
{                                                                              }
{ RaiseLastOSError                                                             }
{                                                                              }
procedure RaiseLastOSError;
begin
  RaiseLastWin32Error;
end;
{$ENDIF}



{                                                                              }
{ Windows Version Info                                                         }
{                                                                              }
function GetWinOSType: TWinOSType;
begin
  Case Win32Platform of
    VER_PLATFORM_WIN32s :
      Result := win31;
    VER_PLATFORM_WIN32_WINDOWS :
      begin
        Result := win32_95;
        if Win32MajorVersion = 4 then
          if Win32MinorVersion >= 90 then
            Result := win32_ME else
          if Win32MinorVersion >= 10 then
            Result := win32_98;
      end;
    VER_PLATFORM_WIN32_NT :
      begin
        Result := win32_nt;
        if Win32MajorVersion = 5 then
          if Win32MinorVersion >= 1 then
            Result := win32_xp else
            Result := win32_2000;
      end;
  else
    Result := win_UnknownPlatform;
  end;
end;

function IsWinNTFamily: Boolean;
begin
  Result := Win32Platform = VER_PLATFORM_WIN32_NT;
end;

function IsWin95Family: Boolean;
begin
  Result := Win32Platform = VER_PLATFORM_WIN32_WINDOWS;
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

function GetProgramFilesPath: String;
begin
  Result := GetRegistryString(HKEY_LOCAL_MACHINE,
         'Software\Microsoft\Windows\CurrentVersion', 'ProgramFilesDir');
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

function GetLocalHostName: String;
const MAX_HOST_LENGTH = MAX_PATH;
var WSAData : TWSAData;
    L       : LongWord;
begin
  if WSAStartup($0101, WSAData) = 0 then
    try
      L := MAX_HOST_LENGTH + 2;
      SetLengthAndZero(Result, L);
      if GetHostName(PChar(Result), L) = 0 then
        SetLength(Result, StrLen(PChar(Result))) else
        Result := '';
    finally
      WSACleanup;
    end;
end;



{                                                                              }
{ Application Version Info                                                     }
{                                                                              }
var
  VersionInfoBuf : Pointer = nil;
  VerTransStr    : String;

procedure LoadAppVersionInfo;
type TTransBuffer = Array [1..4] of SmallInt;
     PTransBuffer = ^TTransBuffer;
var InfoSize : Integer;
    Size, H : LongWord;
    EXEName : String;
    Trans : PTransBuffer;
begin
  if Assigned(VersionInfoBuf) then
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
end;

const
  VersionInfoStr: Array [TVersionInfo] of String =
    ('FileVersion', 'FileDescription', 'LegalCopyright', 'Comments',
     'CompanyName', 'InternalName', 'LegalTrademarks',
     'OriginalFilename', 'ProductName', 'ProductVersion');

function GetAppVersionInfo(const VersionInfo: TVersionInfo): String;
var S : String;
    Size : LongWord;
    Value : PChar;
begin
  LoadAppVersionInfo;
  S := 'StringFileInfo\' + VerTransStr + '\' + VersionInfoStr [VersionInfo];
  if not VerQueryvalue(VersionInfoBuf, PChar(S), Pointer(Value), Size) then
    Result := '' else
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
{ Windows Fibers                                                               }
{                                                                              }
function ConvertThreadToFiber; external kernel32 name 'ConvertThreadToFiber';
function CreateFiber(dwStackSize: DWORD; lpStartAddress: TFNFiberStartRoutine;
         lpParameter: Pointer): Pointer; external kernel32 name 'CreateFiber';



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

function ContentTypeFromExtention(Extention: String): String;
begin
  Result := GetRegistryString(HKEY_CLASSES_ROOT, '\' + Extention, 'Content Type');
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
    SetRegistryString(HKEY_LOCAL_MACHINE, AutoRunRegistryKey, Name, ParamStr(0)) else
    DeleteRegistryValue(HKEY_LOCAL_MACHINE, AutoRunRegistryKey, Name);
end;

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

function GetKeyPressed(const VKeyCode: Integer): Boolean;
begin
  Result := GetKeyState(VKeyCode) and $80 <> 0;
end;

function Reboot: Boolean;
const SE_SHUTDOWN_NAME = 'SeShutDownPrivilege';
var VersionInfo: TOSVersionInfo;
    hToken: Cardinal;
    tkp: TTokenPrivileges;
    retval: Cardinal;
begin
  VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
  GetVersionEx(VersionInfo);
  if IsWinNTFamily then
    if OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then
      begin
        LookupPrivilegeValue(nil, SE_SHUTDOWN_NAME, tkp.Privileges[0].Luid);
        tkp.PrivilegeCount := 1;
        tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
        AdjustTokenPrivileges(hToken, false, tkp, 0, tkp, retval);
      end;
  Result := ExitWindowsEx(EWX_FORCE or EWX_REBOOT, 0);
end;



{                                                                              }
{ WinInet API                                                                  }
{                                                                              }
const
  IEProtoPrefix : Array[TIEProxy] of String =
      ('http=', 'https=', 'ftp=', 'gopher=', 'socks=');

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
            if StrMatchLeft(IEProtoPrefix[Protocol], Proxies[I], False) then
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

function GetHardDiskSerialNumber(const DriveLetter: Char): String;
var N, F, S : DWORD;
begin
  S := 0;
  GetVolumeInformation(PChar(DriveLetter + ':\'), nil, MAX_PATH + 1, @S,
      N, F, nil, 0);
  Result := LongWordToHex(S, 8);
end;

function GetWindowsProductID: String;
var S : String;
begin
  if IsWinNTFamily then
    S := 'Software\Microsoft\Windows NT\CurrentVersion' else
    S := 'Software\Microsoft\Windows\CurrentVersion';
  Result := GetRegistryString(HKEY_LOCAL_MACHINE, S, 'ProductID');
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
begin
  if Assigned(FOnBeforeMessage) then
    FOnBeforeMessage(self);
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
var Msg : TMsg;
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
var Msg : TMsg;
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
    Result := False else
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
    end else
    Result := inherited HandleWM(Msg, wParam, lParam);
end;



initialization
finalization
  if Assigned(VersionInfoBuf) then
    FreeMem(VersionInfoBuf);
end.

