unit TB2Hook;

{
  Toolbar2000
  Copyright (C) 1998-2006 by Jordan Russell
  All rights reserved.

  The contents of this file are subject to the "Toolbar2000 License"; you may
  not use or distribute this file except in compliance with the
  "Toolbar2000 License". A copy of the "Toolbar2000 License" may be found in
  TB2k-LICENSE.txt or at:
    http://www.jrsoftware.org/files/tb2k/TB2k-LICENSE.txt

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License (the "GPL"), in which case the provisions of the
  GPL are applicable instead of those in the "Toolbar2000 License". A copy of
  the GPL may be found in GPL-LICENSE.txt or at:
    http://www.jrsoftware.org/files/tb2k/GPL-LICENSE.txt
  If you wish to allow use of your version of this file only under the terms of
  the GPL and not to allow others to use your version of this file under the
  "Toolbar2000 License", indicate your decision by deleting the provisions
  above and replace them with the notice and other provisions required by the
  GPL. If you do not delete the provisions above, a recipient may use your
  version of this file under either the "Toolbar2000 License" or the GPL.

  $jrsoftware: tb2k/Source/TB2Hook.pas,v 1.17 2006/03/12 23:11:59 jr Exp $
}

interface

uses
  Windows;

type
  THookProcCode = (hpSendActivate, hpSendActivateApp, hpSendWindowPosChanged,
    hpPreDestroy, hpGetMessage);
  THookProcCodes = set of THookProcCode;

  THookProc = procedure(Code: THookProcCode; Wnd: HWND; WParam: WPARAM; LParam: LPARAM);

procedure InstallHookProc(AUser: TObject; AProc: THookProc; ACodes: THookProcCodes);
procedure UninstallHookProc(AUser: TObject; AProc: THookProc);

implementation

uses
  {$IFDEF CLR} System.Runtime.InteropServices, {$ENDIF} 
  SysUtils, Classes, Messages, TB2Common;

type
  THookType = (htCallWndProc, htCBT, htGetMessage);
  THookTypes = set of THookType;

  THookUserData = class
    Prev: THookUserData;
    User: TObject;
    InstalledHookTypes: THookTypes;
  end;

  THookProcData = class
    Proc: THookProc;
    Codes: THookProcCodes;
    LastUserData: THookUserData;
  end;

  THookInfo = class
    Handles: array[THookType] of HHOOK;
    Counts: array[THookType] of Longint;
  end;

threadvar
  HookInfo: THookInfo;
  HookProcList: TList;


function CallWndProcHook(Code: Integer; WParam: WPARAM; LParam: LPARAM): LRESULT;
{$IFNDEF CLR} stdcall; {$ENDIF}
type
  THookProcCodeMsgs = hpSendActivate..hpSendWindowPosChanged;
const
  MsgMap: array[THookProcCodeMsgs] of UINT =
    (WM_ACTIVATE, WM_ACTIVATEAPP, WM_WINDOWPOSCHANGED);
var
  J: THookProcCodeMsgs;
  I: Integer;
  CWPStruct: {$IFNDEF CLR} PCWPStruct {$ELSE} TCWPStruct {$ENDIF};
begin
  if Assigned(HookProcList) and (Code = HC_ACTION) then begin
    {$IFNDEF CLR}
    CWPStruct := PCWPStruct(LParam);
    {$ELSE}
    CWPStruct := TCWPStruct(Marshal.PtrToStructure(IntPtr(LParam), TypeOf(TCWPStruct)));
    {$ENDIF}
    for J := Low(J) to High(J) do
      if CWPStruct.Message = MsgMap[J] then begin
        for I := 0 to HookProcList.Count-1 do
          try
            with THookProcData(HookProcList.List[I]) do
              if J in Codes then
                Proc(J, CWPStruct.hwnd, CWPStruct.WParam, CWPStruct.LParam);
          except
          end;
        Break;
      end;
  end;
  Result := CallNextHookEx(HookInfo.Handles[htCallWndProc], Code, WParam, LParam);
end;

function CBTHook(Code: Integer; WParam: WPARAM; LParam: LPARAM): LRESULT;
{$IFNDEF CLR} stdcall; {$ENDIF}
var
  I: Integer;
begin
  if Assigned(HookProcList) and (Code = HCBT_DESTROYWND) then
    for I := 0 to HookProcList.Count-1 do
      try
        with THookProcData(HookProcList.List[I]) do
          if hpPreDestroy in Codes then
            Proc(hpPreDestroy, HWND(WParam), 0, 0);
      except
      end;
  Result := CallNextHookEx(HookInfo.Handles[htCBT], Code, WParam, LParam);
end;

function GetMessageHook(Code: Integer; WParam: WPARAM; LParam: LPARAM): LRESULT;
{$IFNDEF CLR} stdcall; {$ENDIF}
var
  I: Integer;
begin
  if Assigned(HookProcList) and (Code = HC_ACTION) then
    for I := 0 to HookProcList.Count-1 do
      try
        with THookProcData(HookProcList.List[I]) do
          if hpGetMessage in Codes then
            Proc(hpGetMessage, 0, WParam, LParam);
      except
      end;
  Result := CallNextHookEx(HookInfo.Handles[htGetMessage], Code, WParam, LParam);
end;

function HookCodesToTypes(Codes: THookProcCodes): THookTypes;
const
  HookCodeToType: array[THookProcCode] of THookType =
    (htCallWndProc, htCallWndProc, htCallWndProc, htCBT, htGetMessage);
var
  J: THookProcCode;
begin
  Result := [];
  for J := Low(J) to High(J) do
    if J in Codes then
      Include(Result, HookCodeToType[J]);
end;

var
  HookProcs: array[THookType] of TFNHookProc;
const
  HookIDs: array[THookType] of Integer =
    (WH_CALLWNDPROC, WH_CBT, WH_GETMESSAGE);

procedure InstallHooks(ATypes: THookTypes; var InstalledTypes: THookTypes);
var
  T: THookType;
begin
  if HookInfo = nil then
    HookInfo := THookInfo.Create;

  { Don't increment reference counts for hook types that were already
    installed previously } 
  ATypes := ATypes - InstalledTypes;

  { Increment reference counts first. This should never raise an exception. }
  for T := Low(T) to High(T) do
    if T in ATypes then begin
      Inc(HookInfo.Counts[T]);
      Include(InstalledTypes, T);
    end;

  { Then install the hooks }
  for T := Low(T) to High(T) do
    if T in InstalledTypes then begin
      if HookInfo.Handles[T] = 0 then begin
        { On Windows NT platforms, SetWindowsHookExW is used to work around an
          apparent bug in Windows NT/2000/XP: if an 'ANSI' WH_GETMESSAGE hook
          is called *before* a 'wide' WH_GETMESSAGE hook, then WM_*CHAR
          messages passed to the 'wide' hook use ANSI character codes.
          This is needed for compatibility with the combination of Tnt Unicode
          Controls and Keyman. See "Widechar's and tb2k" thread on the
          newsgroup from 2003-09-23 for more information. }
        if Win32Platform = VER_PLATFORM_WIN32_NT then
          HookInfo.Handles[T] := SetWindowsHookExW(HookIDs[T], HookProcs[T],
            0, GetCurrentThreadId)
        else
          HookInfo.Handles[T] := SetWindowsHookEx(HookIDs[T], HookProcs[T],
            0, GetCurrentThreadId);
        { .NET note: A reference to the delegate passed to SetWindowsHookEx
          must exist for as long as the hook is installed, otherwise the GC
          will collect it and the app will crash. Hence we always pass a
          global variable (HookProcs[]) to SetWindowsHookEx. }
      end;
    end;
end;

procedure UninstallHooks(const ATypes: THookTypes; const Force: Boolean);
var
  T: THookType;
begin
  { HookInfo can be nil if InstallHooks was never called previously (e.g. when
    we're being called with Force=True), or if it was called but failed with
    an exception }
  if HookInfo = nil then
    Exit;

  { Decrement reference counts first. This should never raise an exception. }
  if not Force then
    for T := Low(T) to High(T) do
      if T in ATypes then
        Dec(HookInfo.Counts[T]);

  { Then uninstall the hooks }
  for T := Low(T) to High(T) do
    if T in ATypes then begin
      if (Force or (HookInfo.Counts[T] = 0)) and (HookInfo.Handles[T] <> 0) then begin
        UnhookWindowsHookEx(HookInfo.Handles[T]);
        HookInfo.Handles[T] := 0;
      end;
    end;

  { If all hooks are uninstalled, free HookInfo }
  for T := Low(T) to High(T) do
    if (HookInfo.Counts[T] <> 0) or (HookInfo.Handles[T] <> 0) then
      Exit;
  FreeAndNil(HookInfo);
end;

procedure InstallHookProc(AUser: TObject; AProc: THookProc; ACodes: THookProcCodes);
var
  Found: Boolean;
  I: Integer;
  UserData: THookUserData;
  ProcData: THookProcData;
label 1;
begin
  if HookProcList = nil then
    HookProcList := TList.Create;
  Found := False;
  UserData := nil;  { avoid warning }
  for I := 0 to HookProcList.Count-1 do begin
    ProcData := THookProcData(HookProcList[I]);
    if @ProcData.Proc = @AProc then begin
      UserData := ProcData.LastUserData;
      while Assigned(UserData) do begin
        if UserData.User = AUser then begin
          { InstallHookProc was already called for AUser/AProc. Go ahead and
            call InstallHooks again just in case the hooks weren't successfully
            installed last time. }
          goto 1;
        end;
        UserData := UserData.Prev;
      end;
      UserData := THookUserData.Create;
      UserData.Prev := ProcData.LastUserData;
      UserData.User := AUser;
      UserData.InstalledHookTypes := [];
      ProcData.LastUserData := UserData;
      Found := True;
      Break;
    end;
  end;
  if not Found then begin
    UserData := THookUserData.Create;
    try
      UserData.Prev := nil;
      UserData.User := AUser;
      UserData.InstalledHookTypes := [];
      HookProcList.Expand;
      ProcData := THookProcData.Create;
    except
      UserData.Free;
      raise;
    end;
    ProcData.Proc := AProc;
    ProcData.Codes := ACodes;
    ProcData.LastUserData := UserData;
    HookProcList.Add(ProcData);
  end;
1:InstallHooks(HookCodesToTypes(ACodes), UserData.InstalledHookTypes);
end;

procedure UninstallHookProc(AUser: TObject; AProc: THookProc);
var
  I: Integer;
  ProcData: THookProcData;
  NextUserData, UserData: THookUserData;
  T: THookTypes;
begin
  if HookProcList = nil then Exit;
  for I := 0 to HookProcList.Count-1 do begin
    ProcData := THookProcData(HookProcList[I]);
    if @ProcData.Proc = @AProc then begin
      { Locate the UserData record }
      NextUserData := nil;
      UserData := ProcData.LastUserData;
      while Assigned(UserData) and (UserData.User <> AUser) do begin
        NextUserData := UserData;
        UserData := UserData.Prev;
      end;
      if UserData = nil then
        Exit;

      { Remove record from linked list }
      if NextUserData = nil then begin
        { It's the last item in the list }
        if UserData.Prev = nil then begin
          { It's the only item in the list, so destroy the ProcData record }
          HookProcList.Delete(I);
          ProcData.Free;
        end
        else
          ProcData.LastUserData := UserData.Prev;
      end
      else
        NextUserData.Prev := UserData.Prev;

      T := UserData.InstalledHookTypes;
      UserData.Free;
      UninstallHooks(T, False);
      Break;
    end;
  end;
  if HookProcList.Count = 0 then
    FreeAndNil(HookProcList);
end;


initialization
  { Work around Delphi.NET 2005 bug: declaring a constant array of procedural
    types crashes the compiler (see QC #10381; 2006 fixes it). So we instead
    declare HookProcs as a variable, and initialize the elements here. }
  HookProcs[htCallWndProc] := CallWndProcHook;
  HookProcs[htCBT] := CBTHook;
  HookProcs[htGetMessage] := GetMessageHook;
finalization
  UninstallHooks([Low(THookType)..High(THookType)], True);
end.
