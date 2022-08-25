{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Description:  Service utilities
Creation:     2006
Version:      1.0
EMail:        arno.garrels@gmx.de
Support:      None
Legal issues: Copyright (C) 2006-2011 by Arno Garrels, Berlin

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.


History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit DDSvcUtils;

{$I DDCompilers.inc}

interface

{$IFDEF COMPILER16_UP}
uses
  Winapi.Windows, System.Win.Registry, System.SysUtils;
{$ELSE}
uses
  Windows, Registry, SysUtils;
{$ENDIF}

procedure RegisterEventLogSource(const ServiceName, Module: string);
procedure UnRegisterEventLogSource(const ServiceName: string);
function IsCharInSysCharSet(Ch: WideChar; const ASet: TSysCharSet) : Boolean; overload;
function IsCharInSysCharSet(Ch: AnsiChar; const ASet: TSysCharSet) : Boolean; overload;
//function FindCmdLineParam(const Name: string; const SwitchChars: TSysCharSet;
  //const ParamDelim: TSysCharSet; IgnoreCase: Boolean; var Param: string): Boolean; overload;
function FindCmdLineParam(const Name: string; var Param: string): Boolean; overload;
procedure SetServiceImagePath(const ServiceName, ImagePath: string);
function IsServiceRunning(const SvcName: string): Boolean;
function GetParentProcessFileName(const ChildPid: DWORD): string; overload;
function GetParentProcessFileName: string; overload;
function GetWinSys32Dir: string;
function IsWow64: Boolean;
function GetProcessFileName(hProcess: THandle): string;

implementation

{$IFDEF COMPILER16_UP}
uses
  Winapi.WinSvc, Winapi.TlHelp32, Winapi.PsApi;
{$ELSE}
uses
  WinSvc, TlHelp32, PsApi;
{$ENDIF}

const
  KeyServices    = 'SYSTEM\CurrentControlSet\Services\';
  KeyEventLogApp = 'SYSTEM\CurrentControlSet\Services\EventLog\Application\';

procedure SetServiceImagePath(const ServiceName, ImagePath: string);
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    OpenKey(KeyServices + ServiceName, False);
    WriteString('ImagePath', ImagePath);
  finally
    Free;
  end
end;

procedure RegisterEventLogSource(const ServiceName, Module: string);
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    OpenKey(KeyEventLogApp + ServiceName, True);
    WriteString('EventMessageFile', Module);
    WriteInteger('TypesSupported', EVENTLOG_ERROR_TYPE or
                 EVENTLOG_WARNING_TYPE or EVENTLOG_INFORMATION_TYPE);
  finally
    Free;
  end
end;

procedure UnRegisterEventLogSource(const ServiceName: string);
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    DeleteKey(KeyEventLogApp + ServiceName);
  finally
    Free;
  end
end;

function IsCharInSysCharSet(Ch: WideChar; const ASet: TSysCharSet) : Boolean;
begin
  Result := (Ord(Ch) < 256) and (AnsiChar(Ch) in ASet);
end;

function IsCharInSysCharSet(Ch: AnsiChar; const ASet: TSysCharSet) : Boolean;
begin
  Result := Ch in ASet;
end;

function FindCmdLineParam(const Name: string; const SwitchChars: TSysCharSet;
  const ParamDelim: TSysCharSet; IgnoreCase: Boolean; var Param: string): Boolean;  overload;
var
  I, J, K, L: Integer;
  S: string;
begin
  for I := 1 to ParamCount do
  begin
    S := ParamStr(I);
    if (SwitchChars = []) or IsCharInSysCharSet(S[1], SwitchChars) then
    begin
      K := 0;
      L := Length(S);
      for J := 1 to L do
      begin
        if IsCharInSysCharSet(S[J], ParamDelim) then
        begin
          K := J;
          Break;
        end;
      end;
      if K < 2 then
        Continue;
      if IgnoreCase then
      begin
        if (AnsiCompareText(Copy(S, 2, K - 2), Name) = 0) then
        begin
          Param := AnsiLowerCase(Copy(S, K + 1, MaxInt));
          Result := True;
          Exit;
        end;
      end
      else begin
        if (AnsiCompareStr(Copy(S, 2, K - 2), Name) = 0) then
        begin
          Param := Copy(S, K + 1, MaxInt);
          Result := True;
          Exit;
        end;
      end;
    end;
  end;
  Param := '';
  Result := False;
end;

function FindCmdLineParam(const Name: string; var Param: string): Boolean;
begin
  Result := FindCmdLineParam(Name, ['-', '/', ' '], [':'], TRUE, Param);
end;

function GetSvcStatus(const SvcName: string): Integer;
var
 Scm: SC_HANDLE;
 Svc: SC_HANDLE;
 Status: TServiceStatus;
begin
  Result := 0;
  Scm := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if Scm <> 0 then
  begin
    try
      Svc := OpenService(Scm, PChar(SvcName), SERVICE_QUERY_STATUS);
      if Svc <> 0 then
      begin
        try
          if QueryServiceStatus(Svc, Status) then
            Result := Status.dwCurrentState;
        finally
          CloseServiceHandle(Svc);
        end;
      end;
    finally
      CloseServiceHandle(Scm);
    end;
  end;
end;

function IsServiceRunning(const SvcName: string): Boolean;
begin
  Result := GetSvcStatus(SvcName) = SERVICE_RUNNING;
end;

function DevicePathToFileName(const DevPath: string): string;
const
  BUFCHARS = 128;
var
  szTemp: array[0..BUFCHARS - 1] of Char;
  szName: array[0..MAX_PATH -1] of Char;
  szDrive: array[0..2] of Char;
  bFound: Boolean;
  p: PChar;
  Len: Integer;
begin
  // Translate path with device name to drive letters.
  Result := '';
  if DevPath = '' then
    Exit;
  szTemp[0] := #0;
  Len := GetLogicalDriveStrings(BUFCHARS -1, szTemp);
  if (Len > 0) and (Len < BUFCHARS -1) then
  begin
    szDrive[1] := ':';
    szDrive[2] := #0;
    bFound := FALSE;
    p := szTemp;
    repeat
      // Copy the drive letter to the template string
      szDrive[0] := p^;
      // Look up each device name
      if QueryDosDevice(szDrive, szName, MAX_PATH) <> 0 then
      begin
        Len := StrLen(PChar(@szName[0]));

        if Len < MAX_PATH then
        begin
          bFound := (StrLiComp(PChar(DevPath), PChar(@szName[0]), Len) = 0) and
                    (StrLiComp(PChar(@DevPath[Len + 1]), '\', 1) = 0);
          if bFound then
            Result := string(szDrive) + string(PChar(@DevPath[Len + 1]));
        end;
      end;
      Inc(p, 4);
    until bFound or (p^ = #0);
  end;
end;

type
  TQueryFullProcessImageName = function(hProcess: THandle; dwFlags: DWORD;
              lpExeName: PChar; var lpdwSize: DWORD): LongBool; stdcall;
  TGetProcessImageFileName = function(hProcess: THandle; lpImageFileName: PChar;
              nSize: DWORD): DWORD; stdcall;
var
  f_QueryFullProcessImageName: TQueryFullProcessImageName = nil;
  hPSAPI: THandle = 0;
  f_GetProcessImageFileName: TGetProcessImageFileName = nil;

function GetProcessFileName(hProcess: THandle): string;
var
  Len: DWORD;
  hMod: HMODULE;
begin
  if @f_QueryFullProcessImageName = nil then
    f_QueryFullProcessImageName := GetProcAddress(GetModuleHandle('kernel32'),
                                            {$IFDEF UNICODE}
                                                'QueryFullProcessImageNameW'
                                            {$ELSE}
                                                'QueryFullProcessImageNameA'
                                            {$ENDIF}
                                                );
  if @f_QueryFullProcessImageName <> nil then { Vista+ }
  begin
    Len := MAX_PATH;
    SetLength(Result, Len);
    if not f_QueryFullProcessImageName(hProcess, 0, PChar(Result), Len) then
      Result := ''
    else
      SetLength(Result, Len);
  end
  else begin //
    if hPSAPI = 0 then
      hPSAPI := LoadLibrary('psapi.dll');
    if (hPSAPI >= 32) then
    begin
      if @f_GetProcessImageFileName = nil then
        f_GetProcessImageFileName := GetProcAddress(hPSAPI,
                                            {$IFDEF UNICODE}
                                                'GetProcessImageFileNameW'
                                            {$ELSE}
                                                'GetProcessImageFileNameA'
                                            {$ENDIF}
                                                );
    end;
    if @f_GetProcessImageFileName <> nil then { XP }
    begin
      Len := MAX_PATH;
      SetLength(Result, Len);
      if f_GetProcessImageFileName(hProcess, PChar(Result), Len) = 0 then
        Result := ''
      else
        SetLength(Result, Len);
      if Result <> '' then
        Result := DevicePathToFileName(Result);
    end
    else begin { W2K }
      Result := '';
      if not EnumProcessModules(hProcess, @hMod, SizeOf(hMod), Len) then
        Exit;
      SetLength(Result, MAX_PATH);
      Len := GetModuleFileNameEx(hProcess, hMod, PChar(Result), MAX_PATH);
      SetLength(Result, Len);
    end;
  end;
end;

function GetParentProcessFileName(const ChildPid: DWORD): string;
var
  hSnapShot: THandle;
  ProcEntry: TProcessEntry32;
  hProcess: THandle;
  PID: DWORD;
begin
  Result := '';
  PID := 0;
  hSnapShot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if hSnapShot = INVALID_HANDLE_VALUE then
    RaiseLastOSError;
  try
    ProcEntry.dwSize := SizeOf(ProcEntry);
    if not Process32First(hSnapShot, ProcEntry) then
      RaiseLastOSError;
    repeat
      if ChildPid = ProcEntry.th32ProcessID then
        PID := ProcEntry.th32ParentProcessID;
    until
      (PID <> 0) or not Process32Next(hSnapShot, ProcEntry);
    if PID = 0 then
      Exit;//RaiseLastOSError;
    hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, FALSE, PID);
    if hProcess = 0 then
      RaiseLastOSError;
    try
      Result := GetProcessFileName(hProcess);
    finally
      CloseHandle(hProcess);
    end;
  finally
    CloseHandle(hSnapShot);
  end;
end;

function GetParentProcessFileName: string;
begin
  Result := GetParentProcessFileName(GetCurrentProcessID);
end;

function GetWinSys32Dir: string;
var
  Required: Cardinal;
begin
  Required := GetSystemDirectory(nil, 0);
  SetLength(Result, Required);
  if Required > 0 then
  begin
    Required := GetSystemDirectory(PChar(Result), Required);
    SetLength(Result, Required);
  end;
end;

function IsWow64: Boolean;
type
  TIsWow64Process = function(hProcess: THandle; out Wow64Process: LongBool): LongBool; stdcall;
var
  f_IsWow64Process: TIsWow64Process;
  bIsWow64: LongBool;
begin
  bIsWow64 := FALSE;
  f_IsWow64Process := GetProcAddress(GetModuleHandle('kernel32'), 'IsWow64Process');
  if @f_IsWow64Process <> nil then
  begin
    if not f_IsWow64Process(GetCurrentProcess, bIsWow64) then
      RaiseLastOSError;
  end;
  Result := bIsWow64;
end;


end.
