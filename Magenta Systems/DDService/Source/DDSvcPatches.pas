{**************************************************************************************************}
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ Copyright (C) 2013 Arno Garrels. All Rights Reserved.                                            }
{                                                                                                  }
{**************************************************************************************************}

{
Change log
  v1.1 Added function VirtualAllocNearBy() in order to be able to allocate 'near by' memory in x64.
       Compiles from D7 to XE5 now.

 Usage
 =====
   Add this unit to the .dpr file's uses clause.

 Example
 =======
   uses
     FastMM4, // optional memory manager
     DDSvcPatches,
     Forms,
     Unit1 in 'Unit1.pas';

 ***************************************************************************** }

{$A8,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N-,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

{$WARN SYMBOL_PLATFORM OFF}

{$IFNDEF MSWINDOWS}
 {$MESSAGE FATAL 'This unit requires MS Windows'};
{$ENDIF}

{$DEFINE ServiceLocaleFix}

unit DDSvcPatches;

interface

const
  DDSvcPatchesVersion  = '1.1';
  DDSvcPatchesDate     = '2013/01/12';  // yyyy/dd/mm

implementation

uses
  Windows, SysUtils, Math;

const
  DelphiXE2 = 23;

type
  PPointer = ^Pointer;

  PAbsoluteIndirectJmp = ^TAbsoluteIndirectJmp;
  TAbsoluteIndirectJmp = packed record
    OpCode: Word;   //$FF25(Jmp, FF /4)
    Addr: Integer;
  end;

{$IFDEF WIN64}
  TJump = packed record
    AIJ   : TAbsoluteIndirectJmp;
    Pad1  : Byte;
    Pad2  : Byte;
  end;

  TJumpItem = packed record
    Code    : TJump;
    Jump    : Word;
    Offset  : Integer;
    OrgProc : Pointer;
    NewProc : Pointer;
  end;
{$ELSE}
  TJump = packed record
    Jump    : Byte;
    Offset  : Integer;
  end;
  TJumpItem = packed record
    Code    : TJump;
    Jump    : Byte;
    Offset  : Integer;
  end;
{$ENDIF}
  PJumpItem = ^TJumpItem;

{$IFDEF WIN64}
var
  GSysInfo : TSystemInfo;
{$ENDIF}

procedure GlobalInit;
begin
{$IFDEF WIN64}
  GetSystemInfo(GSysInfo);
{$ENDIF}
end;

function VirtualAllocNearBy(AOrigProc: Pointer; ASize: LongWord): Pointer;
{$IFDEF WIN64}
var
  LMinAddr  : Pointer;
  LMaxAddr  : Pointer;
  LMini     : NativeInt;
  LMaxi     : NativeInt;
  I         : Integer;
  LRel      : Integer;
  LMemInfo  : TMemoryBasicInformation;
  LCurAddr  : Pointer;
begin
  LRel      := 0;
  Result    := nil;
  LMinAddr  := Pointer(Max(NativeInt(GSysInfo.lpMinimumApplicationAddress), NativeInt(AOrigProc) - $20000000));
  LMaxAddr  := Pointer(Min(NativeInt(GSysInfo.lpMaximumApplicationAddress), NativeInt(AOrigProc) + $20000000));
  LMini     := NativeInt(LMinAddr) div GSysInfo.dwAllocationGranularity;
  LMaxi     := NativeInt(LMaxAddr) div GSysInfo.dwAllocationGranularity;
  for I := 0 to (LMaxi - LMini) -1 do
  begin
    LRel := -LRel + (I and 1);
    LCurAddr := Pointer((((LMini + LMaxi) div 2) + LRel) * GSysInfo.dwAllocationGranularity);
    if (VirtualQuery(LCurAddr, LMemInfo, SizeOf(LMemInfo)) = SizeOf(LMemInfo)) and
       (LMemInfo.State = MEM_FREE) then
    begin
      Result := VirtualAlloc(LCurAddr, ASize, MEM_COMMIT or MEM_RESERVE, PAGE_EXECUTE_READWRITE);
      if Result <> nil then
        Exit;
    end;
  end;
{$ELSE}
begin
  Result := VirtualAlloc(nil, ASize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
{$ENDIF}
end;

function GetActualAddr(AProc: Pointer): Pointer;
begin
  if AProc <> nil then
  begin
    if PAbsoluteIndirectJmp(AProc)^.OpCode = $25FF then
    {$IFDEF WIN64}
      Result := PPointer(PAnsiChar(AProc) + 6 + PAbsoluteIndirectJmp(AProc)^.Addr)^
    {$ELSE}
      Result := PPointer(PAbsoluteIndirectJmp(AProc)^.Addr)^
    {$ENDIF WIN64}
    else
      Result := AProc;
  end
  else
    Result := nil;
end;

procedure DebugLog(const S: string);
begin
  if DebugHook <> 0 then
    OutputDebugString(PChar('DDPatchPack: ' + S));
end;

procedure PatchWinAPI(AProc, ADest: Pointer; var AJumpItem: TJumpItem);
var
  LBytesWritten : {$IF COMPILERVERSION < DelphiXE2} Cardinal {$ELSE} NativeUInt {$IFEND};
  LCode : TJump;
begin
  Assert(AProc <> nil);
  if ReadProcessMemory(GetCurrentProcess, AProc, @AJumpItem.Code, SizeOf(AJumpItem.Code), LBytesWritten) then
  begin
  {$IFDEF WIN64}
    AJumpItem.Jump      := $25FF;
    AJumpItem.Offset    := 0;
    AJumpItem.OrgProc   := Pointer(NativeInt(AProc) + SizeOf(AJumpItem.Code));

    LCode.AIJ.OpCode    := $25FF;
    LCode.AIJ.Addr      := Integer(NativeInt(@AJumpItem.NewProc) - NativeInt(AProc) - SizeOf(LCode.AIJ));
    LCode.Pad1          := $CC; // Just for the looking
    LCode.Pad2          := $CC; // Just for the looking
    AJumpItem.NewProc   := ADest;
  {$ELSE}
    AJumpItem.Jump      := $E9;
    AJumpItem.Offset    := Integer(PAnsiChar(AProc) - PAnsiChar(@AJumpItem) - SizeOf(AJumpItem.Code));

    LCode.Jump          := $E9;
    LCode.Offset        := Integer(PAnsiChar(ADest) - PAnsiChar(AProc) - SizeOf(LCode));
  {$ENDIF}
    WriteProcessMemory(GetCurrentProcess, AProc, @LCode, SizeOf(LCode), LBytesWritten);
  end;
end;

procedure UnpatchWinAPI(AProc: Pointer; const AJumpItem: TJumpItem);
var
  LBytesWritten: {$IF COMPILERVERSION < DelphiXE2} Cardinal {$ELSE} NativeUInt {$IFEND};
begin
{$IFDEF WIN64}
  if AJumpItem.Code.AIJ.OpCode <> 0 then
{$ELSE}
  if AJumpItem.Code.Jump <> 0 then
{$ENDIF}
  begin
    AProc := GetActualAddr(AProc);
    Assert(AProc <> nil);
    WriteProcessMemory(GetCurrentProcess, AProc, @AJumpItem.Code, SizeOf(AJumpItem.Code), LBytesWritten);
  end;
end;

{ **************************************************************************** }
{$IFDEF ServiceLocaleFix}
var
  GJumpItem0: PJumpItem;
  GJumpItem1: PJumpItem;
  ServiceLocaleFixRequired: Boolean;

  CompareStringAProc: function(Locale: LCID; dwCmpFlags: DWORD; lpString1: PAnsiChar;
    cchCount1: Integer; lpString2: PAnsiChar; cchCount2: Integer): Integer; stdcall;
  CompareStringWProc: function(Locale: LCID; dwCmpFlags: DWORD; lpString1: PWideChar;
    cchCount1: Integer; lpString2: PWideChar; cchCount2: Integer): Integer; stdcall;

function ReplacementCompareStringW(Locale: LCID; dwCmpFlags: DWORD; lpString1: PWideChar;
  cchCount1: Integer; lpString2: PWideChar; cchCount2: Integer): Integer; stdcall;
begin
  if Locale = LOCALE_USER_DEFAULT then
    Locale := LOCALE_SYSTEM_DEFAULT;
  Result := CompareStringWProc(Locale, dwCmpFlags, lpString1, cchCount1, lpString2, cchCount2);
end;

function ReplacementCompareStringA(Locale: LCID; dwCmpFlags: DWORD; lpString1: PAnsiChar;
  cchCount1: Integer; lpString2: PAnsiChar; cchCount2: Integer): Integer; stdcall;
begin
  if Locale = LOCALE_USER_DEFAULT then
    Locale := LOCALE_SYSTEM_DEFAULT;
  Result := CompareStringAProc(Locale, dwCmpFlags, lpString1, cchCount1, lpString2, cchCount2);
end;

procedure InitializeServiceLocaleFix;
var
  LProc: Pointer;
begin
  { Vista+ NT services are affected }
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6) then
  begin
    ServiceLocaleFixRequired := True; // Do we run as a service?
    if ServiceLocaleFixRequired then
    begin
      DebugLog('Installing ServiceLocaleFix for Windows Vista+');
      LProc := GetActualAddr(@CompareStringA);
      GJumpItem0 := VirtualAllocNearBy(LProc, SizeOf(TJumpItem));
      Assert(GJumpItem0 <> nil);
      PatchWinAPI(LProc, @ReplacementCompareStringA, GJumpItem0^);
      CompareStringAProc := @GJumpItem0^;

      LProc := GetActualAddr(@CompareStringW);
      GJumpItem1 := VirtualAllocNearBy(LProc, SizeOf(TJumpItem));
      Assert(GJumpItem1 <> nil);
      PatchWinAPI(LProc, @ReplacementCompareStringW, GJumpItem1^);
      CompareStringWProc := @GJumpItem1^;
    end;
  end;
end;

procedure FinalizeServiceLocaleFix;
begin
  if ServiceLocaleFixRequired then
  begin
    UnpatchWinAPI(@CompareStringA, GJumpItem0^);
    UnpatchWinAPI(@CompareStringW, GJumpItem1^);
    VirtualFree(GJumpItem0, 0, MEM_RELEASE);
    VirtualFree(GJumpItem1, 0, MEM_RELEASE);
  end;
end;
{$ENDIF ServiceLocaleFix}

{ **************************************************************************** }
initialization
  GlobalInit;
{$IFDEF ServiceLocaleFix}
  InitializeServiceLocaleFix;
{$ENDIF ServiceLocaleFix}

finalization
  // In revers order
{$IFDEF ServiceLocaleFix}
  FinalizeServiceLocaleFix;
{$ENDIF ServiceLocaleFix}

end.
