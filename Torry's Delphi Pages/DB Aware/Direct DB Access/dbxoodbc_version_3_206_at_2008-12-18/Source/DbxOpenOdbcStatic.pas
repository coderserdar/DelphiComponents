{
  Kylix / Delphi open source DbExpress driver for ODBC
  Version 3.200, 2008-09-18

  Copyright (c) 2001-2009 Vadim V.Lopushansky

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public License
  as published by the Free Software Foundation; either version 2.1
  of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License for more details.
}
unit DbxOpenOdbcStatic;

{$i DbxOpenOdbc.inc}

interface

function IsStatic(): Boolean;

implementation

uses
  {$IFDEF LINUX}
    DbxOpenOdbc,
  {$ELSE}
    Windows,
    DbxOpenOdbcApiHook,
    DbxOpenOdbc,
    {$IFDEF _DBX30_}
    DbxOpenOdbc3,
    {$ENDIF}
    {$IFDEF _ASA_MESSAGE_CALLBACK_}
    DbxOpenOdbcASA,
    {$ENDIF}
  {$ENDIF}
  SysUtils;

{$IFDEF LINUX}

function IsStatic(): Boolean;
begin
  Result := False;
end;

{$ELSE}

const
  dbxoodbc = 'dbxoodbc.dll';

var
  {$IFDEF MSWINDOWS}
  IATPatch: TIATPatch =  nil;
  {$ENDIF}
  DllHandle: HMODULE = 0;

function IsStatic(): Boolean;
begin
  Result := DllHandle <> 0;
end;

var
  _LoadLibraryA: function (lpLibFileName: PAnsiChar): HMODULE; stdcall;

function NewLoadLibraryA(lpLibFileName: PAnsiChar): HMODULE; stdcall;
begin
  if (DllHandle <> 0) and SameText(ExtractFileName(string(StrPas(lpLibFileName))), dbxoodbc) then
    Result := DllHandle
  else
    Result := _LoadLibraryA(lpLibFileName);
end;

var
  _LoadLibraryW: function(lpLibFileName: PWideChar): HMODULE; stdcall;

function NewLoadLibraryW(lpLibFileName: PWideChar): HMODULE; stdcall;
begin
  if (DllHandle <> 0) and SameText(ExtractFileName(WideString(lpLibFileName)), dbxoodbc) then
    Result := DllHandle
  else
    Result := _LoadLibraryW(lpLibFileName);
end;

var
  _FreeLibrary: function (hLibModule: HMODULE): BOOL; stdcall;

function NewFreeLibrary(hLibModule: HMODULE): BOOL; stdcall;
begin
  if (DllHandle = 0) or (hLibModule <> DllHandle) then
    Result := _FreeLibrary(hLibModule)
  else
    Result := True;
end;

var
  _GetProcAddress: function (hModule: HMODULE; lpProcName: LPCSTR): FARPROC; stdcall;

function NewGetProcAddress(hModule: HMODULE; lpProcName: LPCSTR): FARPROC; stdcall;

  procedure LDoGetProcAddress;
  var
    S: AnsiString;
  begin
    S := AnsiString(UpperCase(Trim(string(StrPas(lpProcName)))));
    {$IFDEF _DBX30_}
    if S = 'GETSQLDRIVERODBCW' then
    begin
      Result := @getSQLDriverODBCW;
    end
    else
    if S = 'GETSQLDRIVERODBCWA' then
    begin
      Result := @getSQLDriverODBCWA;
    end
    else
    {$ENDIF}
    if S = 'GETSQLDRIVERODBC' then
    begin
      Result := @getSQLDriverODBC;
    end
    else
    if S = 'GETSQLDRIVERODBCAW' then
    begin
      Result := @getSQLDriverODBCAW;
    end
    else
    {$IFDEF _ASA_MESSAGE_CALLBACK_}
    if S = 'ASA_MESSAGE_INIT' then
    begin
      Result := @ASA_MESSAGE_INIT;
    end
    else
    if S = 'ASA_MESSAGE_CALLBACK' then
    begin
      Result := @ASA_MESSAGE_CALLBACK;
    end
    else
    {$ENDIF}
      Result := nil;
  end;

begin
  if (DllHandle <> 0) and (DllHandle = hModule) then
  begin
    LDoGetProcAddress();
    if Assigned(Result) then
      Exit;
  end;
  Result := _GetProcAddress(hModule, lpProcName);
end;

procedure UnHookIAT_LoadLibrary();
begin
  {$IFDEF MSWINDOWS}
  if Assigned(IATPatch) then
  begin
    DllHandle := 0;
    IATPatch.Clear(True);
    IATPatch.Free;
    IATPatch := nil;
  end
  else if Win32Platform = VER_PLATFORM_WIN32_NT then
  {$ENDIF}
  begin
    UnHookCode(@_LoadLibraryA);
    UnHookCode(@_LoadLibraryW);
    UnHookCode(@_FreeLibrary);
    UnHookCode(@_GetProcAddress);
  end;
end;

procedure HookIAT_LoadLibrary();
begin
  {.$IFDEF _DBX30_}
  {$IFDEF MSWINDOWS}
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  {$ENDIF}
  begin
    if HookProc(kernel32, 'LoadLibraryA', @NewLoadLibraryA, @_LoadLibraryA)
      and
      HookProc(kernel32, 'LoadLibraryW', @NewLoadLibraryW, @_LoadLibraryW)
      and
      HookProc(kernel32, 'FreeLibrary', @NewFreeLibrary, @_FreeLibrary)
      and
      HookProc(kernel32, 'GetProcAddress', @NewGetProcAddress, @_GetProcAddress)
    then
    begin
      DllHandle := GetModuleHandle(kernel32);
      Exit;
    end
    else
    begin
      UnHookIAT_LoadLibrary();
    end;
  end;
  {.$ENDIF}
  {$IFDEF MSWINDOWS}
  IATPatch := TIATPatch.Create;
  if IATPatch.Patch(kernel32, 'LoadLibraryA', @NewLoadLibraryA, @_LoadLibraryA)
    and
    IATPatch.Patch(kernel32, 'LoadLibraryW', @NewLoadLibraryW, @_LoadLibraryW)
    and
    IATPatch.Patch(kernel32, 'FreeLibrary', @NewFreeLibrary, @_FreeLibrary)
    and
    IATPatch.Patch(kernel32, 'GetProcAddress', @NewGetProcAddress, @_GetProcAddress)
  then
  begin
    DllHandle := GetModuleHandle(kernel32);
  end
  else
  begin
    UnHookIAT_LoadLibrary();
  end;
  {$ENDIF}
end;

initialization
    HookIAT_LoadLibrary();
finalization
    UnHookIAT_LoadLibrary();
{$ENDIF ~LINUX}
end.
