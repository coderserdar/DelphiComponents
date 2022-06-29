unit ATxRegistry;

interface

uses
  Windows;

function SetRegKeyStr(RootKey: HKEY; const SubKey, Name, Value: WideString): Boolean;
function GetRegKeyStr(RootKey: HKEY; const SubKey, Name, Default: WideString): WideString;

     
implementation

uses
  SysUtils;

function CreateRegKeyA(RootKey: HKEY; const SubKey: string): HKEY;
var
  Disposition: DWORD;
begin
  if RegCreateKeyExA(RootKey, PAnsiChar(AnsiString(SubKey)), 0, nil,
                    REG_OPTION_NON_VOLATILE, KEY_WRITE, nil,
                    Result, @Disposition) <> ERROR_SUCCESS
    then Result:= 0;
end;

function CreateRegKeyW(RootKey: HKEY; const SubKey: WideString): HKEY;
var
  Disposition: DWORD;
begin
  if RegCreateKeyExW(RootKey, PWChar(SubKey), 0, nil,
                    REG_OPTION_NON_VOLATILE, KEY_WRITE, nil,
                    Result, @Disposition) <> ERROR_SUCCESS
    then Result:= 0;
end;

function OpenRegKeyA(RootKey: HKEY; const SubKey: string): HKEY;
begin
  if RegOpenKeyExA(RootKey, PAnsiChar(AnsiString(SubKey)), 0, KEY_QUERY_VALUE, Result) <> ERROR_SUCCESS
    then Result:= 0;
end;

function OpenRegKeyW(RootKey: HKEY; const SubKey: WideString): HKEY;
begin
  if RegOpenKeyExW(RootKey, PWChar(SubKey), 0, KEY_QUERY_VALUE, Result) <> ERROR_SUCCESS
    then Result:= 0;
end;


function SetRegKeyStrA(RootKey: HKEY; const SubKey, Name, Value: string): Boolean;
var
  h: HKEY;
begin
  Result:= False;
  h:= CreateRegKeyA(RootKey, SubKey);
  if h <> 0 then
    begin
    Result:= RegSetValueExA(h, PAnsiChar(AnsiString(Name)), 0, REG_SZ, PChar(Value), Length(Value)+1) = ERROR_SUCCESS;
    RegCloseKey(h);
    end;
end;

function SetRegKeyStrW(RootKey: HKEY; const SubKey, Name, Value: WideString): Boolean;
var
  h: HKEY;
begin
  Result:= False;
  h:= CreateRegKeyW(RootKey, SubKey);
  if h <> 0 then
    begin
    Result:= RegSetValueExW(h, PWChar(Name), 0, REG_SZ, PWChar(Value), (Length(Value)+1)*2) = ERROR_SUCCESS;
    RegCloseKey(h);
    end;
end;

function SetRegKeyStr(RootKey: HKEY; const SubKey, Name, Value: WideString): Boolean;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result:= SetRegKeyStrW(RootKey, SubKey, Name, Value)
  else
    Result:= SetRegKeyStrA(RootKey, SubKey, Name, Value);
end;


function SetRegKeyInt(RootKey: HKEY; const SubKey, Name: WideString; Value: DWORD): Boolean;
var
  h: HKEY;
begin
  Result:= False;
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
    h:= CreateRegKeyW(RootKey, SubKey);
    if h <> 0 then
      Result:= RegSetValueExW(h, PWChar(Name), 0, REG_DWORD, @Value, SizeOf(DWORD)) = ERROR_SUCCESS;
    end
  else
    begin
    h:= CreateRegKeyA(RootKey, SubKey);
    if h <> 0 then
      Result:= RegSetValueExA(h, PAnsiChar(AnsiString(Name)), 0, REG_DWORD, @Value, SizeOf(DWORD)) = ERROR_SUCCESS;
    end;
  RegCloseKey(h);
end;


function SetRegKeyBin(RootKey: HKEY; const SubKey, Name: WideString; DataPtr: pointer; DataSize: DWORD): Boolean;
var
  h: HKEY;
begin
  Result:= False;
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
    h:= CreateRegKeyW(RootKey, SubKey);
    if h <> 0 then
      Result:= RegSetValueExW(h, PWChar(Name), 0, REG_BINARY, DataPtr, DataSize) = ERROR_SUCCESS;
    end
  else
    begin
    h:= CreateRegKeyA(RootKey, SubKey);
    if h <> 0 then
      Result:= RegSetValueExA(h, PAnsiChar(AnsiString(Name)), 0, REG_BINARY, DataPtr, DataSize) = ERROR_SUCCESS;
    end;
  RegCloseKey(h);
end;


function GetRegKeyStrA(RootKey: HKEY; const SubKey, Name, Default: string): string;
var
  h: HKEY;
  Buffer: PChar;
  DataType, DataSize: DWORD;
begin
  Result:= Default;

  h:= OpenRegKeyA(RootKey, SubKey);
  if (RegQueryValueExA(h, PAnsiChar(AnsiString(Name)), nil, @DataType, nil, @DataSize)<>ERROR_SUCCESS)
    or (DataType<>REG_SZ) then
    begin RegCloseKey(h); Exit end;

  GetMem(Buffer, DataSize);
  if (RegQueryValueExA(h, PAnsiChar(AnsiString(Name)), nil, @DataType, PByte(Buffer), @DataSize)<>ERROR_SUCCESS)
    or (DataType<>REG_SZ) then
    begin RegCloseKey(h); Exit end;

  Result:= Buffer;
  FreeMem(Buffer, DataSize);
  RegCloseKey(h);
end;

function GetRegKeyStrW(RootKey: HKEY; const SubKey, Name, Default: WideString): WideString;
var
  h: HKEY;
  Buffer: PWChar;
  DataType, DataSize: DWORD;
begin
  Result:= Default;

  h:= OpenRegKeyW(RootKey, SubKey);
  if (RegQueryValueExW(h, PWChar(Name), nil, @DataType, nil, @DataSize)<>ERROR_SUCCESS)
    or (DataType<>REG_SZ) then
    begin RegCloseKey(h); Exit end;

  GetMem(Buffer, DataSize);
  if (RegQueryValueExW(h, PWChar(Name), nil, @DataType, PByte(Buffer), @DataSize)<>ERROR_SUCCESS)
    or (DataType<>REG_SZ) then
    begin RegCloseKey(h); Exit end;

  Result:= Buffer;
  FreeMem(Buffer, DataSize);
  RegCloseKey(h);
end;

function GetRegKeyStr(RootKey: HKEY; const SubKey, Name, Default: WideString): WideString;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result:= GetRegKeyStrW(RootKey, SubKey, Name, Default)
  else
    Result:= GetRegKeyStrA(RootKey, SubKey, Name, Default);
end;


(*
//ANSI versions:

function GetRegKeyInt(RootKey: HKEY; SubKey: PChar; Name: PChar; const Default: DWORD): DWORD;
var
  h: HKEY;
  DataType, DataSize: DWORD;
begin
  DataSize:= SizeOf(DWORD);
  h:= OpenRegKey(RootKey, SubKey);
  if (RegQueryValueEx(h, Name, nil, @DataType, PByte(@Result), @DataSize)<>ERROR_SUCCESS)
    or (DataType<>REG_DWORD)
    then Result:= Default;
  RegCloseKey(h);
end;

function GetRegKeyBin(RootKey: HKEY; SubKey: PChar; Name: PChar; var DataPtr: pointer; var DataSize: DWORD): boolean;
var
  h: HKEY;
  DataType: DWORD;
begin
  h:= OpenRegKey(RootKey, SubKey);
  Result:= (RegQueryValueEx(h, Name, nil, @DataType, PByte(DataPtr), @DataSize)=ERROR_SUCCESS)
    and (DataType=REG_BINARY);
  if not Result then
    begin DataPtr:= nil; DataSize:= 0 end;
  RegCloseKey(h);
end;
*)


end.
