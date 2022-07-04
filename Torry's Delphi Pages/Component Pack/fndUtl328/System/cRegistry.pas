{$INCLUDE ..\cDefines.inc}
unit cRegistry;

{                                                                              }
{                      Windows Registry functions v3.01                        }
{                                                                              }
{             This unit is copyright © 2002-2004 by David J Butler             }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                   Its original file name is cRegistry.pas                    }
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
{   Windows Registry functions.                                                }
{                                                                              }
{ Revision history:                                                            }
{   2002/09/22  3.00  Created cRegistry unit from cWindows.                    }
{   2002/12/08  3.01  Small revisions.                                         }
{                                                                              }

interface

uses
  { Delphi }
  Windows,

  { Fundamentals }
  cUtils;



{                                                                              }
{ Registry functions                                                           }
{                                                                              }
procedure SplitRegName(const Name: String; var Key, ValueName: String);

{ Exists                                                                       }
function  RegKeyExists(const RootKey: HKEY; const Key: String): Boolean;
function  RegValueExists(const RootKey: HKEY; const Key, Name: String): Boolean;

{ Set                                                                          }
function  RegSetValue(const RootKey: HKEY; const Key, Name: String;
          const ValueType: Cardinal; const Value: Pointer;
          const ValueSize: Integer): Boolean; overload;
function  RegSetValue(const RootKey: HKEY; const Name: String;
          const ValueType: Cardinal; const Value: Pointer;
          const ValueSize: Integer): Boolean; overload;

function  SetRegistryString(const RootKey: HKEY; const Key: String;
          const Name: String; const Value: String): Boolean; overload;
function  SetRegistryString(const RootKey: HKEY; const Name: String;
          const Value: String): Boolean; overload;

function  SetRegistryDWord(const RootKey: HKEY; const Name: String;
          const Value: LongWord): Boolean;

function  SetRegistryBinary(const RootKey: HKEY; const Name: String;
          const Value; const ValueSize: Integer): Boolean;

{ Get                                                                          }
function  RegGetValue(const RootKey: HKEY; const Key, Name: String;
          const ValueType: Cardinal; var RegValueType: Cardinal;
          var ValueBuf: Pointer; var ValueSize: Integer): Boolean; overload;
function  RegGetValue(const RootKey: HKEY; const Name: String;
          const ValueType: Cardinal; var RegValueType: Cardinal;
          var ValueBuf: Pointer; var ValueSize: Integer): Boolean; overload;

function  GetRegistryString(const RootKey: HKEY; const Key, Name: String): String; overload;
function  GetRegistryString(const RootKey: HKEY; const Name: String): String; overload;

function  GetRegistryDWord(const RootKey: HKEY; const Key, Name: String): LongWord;

{ Delete                                                                       }
function  DeleteRegistryValue(const RootKey: HKEY; const Key, Name: String): Boolean;
function  DeleteRegistryKey(const RootKey: HKEY; const Key: String): Boolean;

{ Remote Registries                                                            }
function  ConnectRegistry(const MachineName: String; const RootKey: HKEY;
          var RemoteKey: HKEY): Boolean;
function  DisconnectRegistry(const RemoteKey: HKEY): Boolean;

{ Enumerate                                                                    }
function  EnumRegistryValues(const RootKey: HKEY; const Name: String;
          var ValueList: StringArray): Boolean;
function  EnumRegistryKeys(const RootKey: HKEY; const Name: String;
          var KeyList: StringArray): Boolean;



implementation

uses
  { Delphi }
  SysUtils,

  { Fundamentals }
  cStrings;



{                                                                              }
{ Registry                                                                     }
{                                                                              }
procedure SplitRegName(const Name: String; var Key, ValueName: String);
var S : String;
    I : Integer;
begin
  S := StrExclSuffix(StrExclPrefix(Name, '\'), '\');
  I := PosChar('\', S);
  if I <= 0 then
    begin
      Key := S;
      ValueName := '';
      exit;
    end;
  Key := CopyLeft(S, I - 1);
  ValueName := CopyFrom(S, I + 1);
end;

{ Exists                                                                       }
function RegKeyExists(const RootKey: HKEY; const Key: String): Boolean;
var Handle : HKEY;
begin
  if RegOpenKeyEx(RootKey, PChar(Key), 0, KEY_READ, Handle) = ERROR_SUCCESS then
    begin
      Result := True;
      RegCloseKey(Handle);
    end else
    Result := False;
end;

function RegValueExists(const RootKey: HKEY; const Key, Name: String): Boolean;
var Handle : HKEY;
begin
  if RegOpenKeyEx(RootKey, PChar(Key), 0, KEY_READ, Handle) = ERROR_SUCCESS then
    begin
      Result := RegQueryValueEx(Handle, Pointer(Name), nil, nil, nil, nil) = ERROR_SUCCESS;
      RegCloseKey(Handle);
    end else
    Result := False;
end;

{ Set                                                                          }
function RegSetValue(const RootKey: HKEY; const Key, Name: String;
         const ValueType: Cardinal; const Value: Pointer;
         const ValueSize: Integer): Boolean;
var D : DWORD;
    Handle : HKEY;
begin
  Result := False;
  if ValueSize < 0 then
    exit;
  if RegCreateKeyEx(RootKey, PChar(Key), 0, nil, REG_OPTION_NON_VOLATILE,
      KEY_WRITE, nil, Handle, @D) <> ERROR_SUCCESS then
    exit;
  Result := RegSetValueEx(Handle, Pointer(Name), 0, ValueType, Value, ValueSize) = ERROR_SUCCESS;
  RegCloseKey(Handle);
end;

function RegSetValue(const RootKey: HKEY; const Name: String;
         const ValueType: Cardinal; const Value: Pointer;
         const ValueSize: Integer): Boolean;
var K, N : String;
begin
  SplitRegName(Name, K, N);
  Result := RegSetValue(RootKey, K, N, ValueType, Value, ValueSize);
end;

function SetRegistryString(const RootKey: HKEY; const Key: String;
    const Name: String; const Value: String): Boolean;
begin
  Result := RegSetValue(RootKey, Key, Name, REG_SZ, PChar(Value), Length(Value) + 1);
end;

function SetRegistryString(const RootKey: HKEY; const Name: String;
    const Value: String): Boolean;
begin
  Result := RegSetValue(RootKey, Name, REG_SZ, PChar(Value), Length(Value) + 1);
end;

function SetRegistryDWord(const RootKey: HKEY; const Name: String;
    const Value: LongWord): Boolean;
begin
  Result := RegSetValue(RootKey, Name, REG_DWORD, @Value, Sizeof(LongWord));
end;

function SetRegistryBinary(const RootKey: HKEY; const Name: String; const Value;
    const ValueSize: Integer): Boolean;
begin
  Result := RegSetValue(RootKey, Name, REG_BINARY, @Value, ValueSize);
end;

{ Get                                                                          }
function RegGetValue(const RootKey: HKEY; const Key, Name: String;
         const ValueType: Cardinal; var RegValueType: Cardinal;
         var ValueBuf: Pointer; var ValueSize: Integer): Boolean;
var Handle  : HKEY;
    Buf     : Pointer;
    BufSize : Cardinal;
begin
  Result := False;
  ValueSize := 0;
  ValueBuf := nil;
  if RegOpenKeyEx(RootKey, PChar(Key), 0, KEY_READ, Handle) <> ERROR_SUCCESS then
    exit;
  BufSize := 0;
  RegQueryValueEx(Handle, Pointer(Name), nil, @RegValueType, nil, @BufSize);
  if BufSize <= 0 then
    exit;
  GetMem(Buf, BufSize);
  if RegQueryValueEx(Handle, Pointer(Name), nil, @RegValueType, Buf, @BufSize) = ERROR_SUCCESS then
    begin
      ValueBuf := Buf;
      ValueSize := Integer(BufSize);
      Result := True;
    end;
  if not Result then
    FreeMem(Buf);
  RegCloseKey(Handle);
end;

function RegGetValue(const RootKey: HKEY; const Name: String;
         const ValueType: Cardinal; var RegValueType: Cardinal;
         var ValueBuf: Pointer; var ValueSize: Integer): Boolean;
var K, N : String;
begin
  SplitRegName(Name, K, N);
  Result := RegGetValue(RootKey, K, N, ValueType, RegValueType, ValueBuf, ValueSize);
end;

function GetRegistryString(const RootKey: HKEY; const Key, Name: String): String;
var Buf   : Pointer;
    Size  : Integer;
    VType : Cardinal;
begin
  Result := '';
  if not RegGetValue(RootKey, Key, Name, REG_SZ, VType, Buf, Size) then
    exit;
  if (VType = REG_DWORD) and (Size >= Sizeof(LongWord)) then
    Result := IntToStr(PLongWord(Buf)^) else
  if Size > 0 then
    begin
      SetLength(Result, Size - 1);
      MoveMem(Buf^, Pointer(Result)^, Size - 1);
    end;
  FreeMem(Buf);
end;

function GetRegistryString(const RootKey: HKEY; const Name: String): String;
var K, N : String;
begin
  SplitRegName(Name, K, N);
  Result := GetRegistryString(RootKey, K, N);
end;

function GetRegistryDWord(const RootKey: HKEY; const Key, Name: String): LongWord;
var Buf   : Pointer;
    Size  : Integer;
    VType : Cardinal;
begin
  Result := 0;
  if not RegGetValue(RootKey, Key, Name, REG_DWORD, VType, Buf, Size) then
    exit;
  if (VType = REG_DWORD) and (Size >= Sizeof(LongWord)) then
    Result := PLongWord(Buf)^;
  FreeMem(Buf);
end;

{ Delete                                                                       }
function DeleteRegistryValue(const RootKey: HKEY; const Key, Name: String): Boolean;
var Handle : HKEY;
begin
  if RegOpenKeyEx(RootKey, PChar(Key), 0, KEY_WRITE, Handle) = ERROR_SUCCESS then
    begin
      Result := RegDeleteValue(Handle, Pointer(Name)) = ERROR_SUCCESS;
      RegCloseKey(Handle);
    end else
    Result := False;
end;

function DeleteRegistryKey(const RootKey: HKEY; const Key: String): Boolean;
var Handle : HKEY;
    K, N   : String;
begin
  SplitRegName(Key, K, N);
  if RegOpenKeyEx(RootKey, PChar(K), 0, KEY_WRITE, Handle) = ERROR_SUCCESS then
    begin
      Result := RegDeleteKey(Handle, Pointer(N)) = ERROR_SUCCESS;
      RegCloseKey(Handle);
    end else
    Result := False;
end;

{ Remote Registries                                                            }
function ConnectRegistry(const MachineName: String; const RootKey: HKEY;
         var RemoteKey: HKEY): Boolean;
begin
  Result := RegConnectRegistry(PChar(MachineName), RootKey, RemoteKey) = ERROR_SUCCESS;
end;

function DisconnectRegistry(const RemoteKey: HKEY): Boolean;
begin
  Result := RegCloseKey(RemoteKey) = ERROR_SUCCESS;
end;

{ Enumerate                                                                    }
function RegEnum(const RootKey: HKEY; const Name: String;
         var ResultList: StringArray; const DoKeys: Boolean): Boolean;
var Buf     : Array[0..2047] of Char;
    BufSize : Cardinal;
    I       : Integer;
    Res     : Integer;
    S       : String;
    Handle  : HKEY;
begin
  ResultList := nil;
  Result := RegOpenKeyEx(RootKey, PChar(Name), 0, KEY_READ, Handle) = ERROR_SUCCESS;
  if not Result then
    exit;
  I := 0;
  Repeat
    BufSize := Sizeof(Buf);
    if DoKeys then
      Res := RegEnumKeyEx(Handle, I, @Buf[0], BufSize, nil, nil, nil, nil)
    else
      Res := RegEnumValue(Handle, I, @Buf[0], BufSize, nil, nil, nil, nil);
    if Res = ERROR_SUCCESS then
      begin
        SetLength(S, BufSize);
        if BufSize > 0 then
          MoveMem(Buf[0], Pointer(S)^, BufSize);
        Append(ResultList, S);
        Inc(I);
      end;
  Until Res <> ERROR_SUCCESS;
  RegCloseKey(Handle);
end;

function EnumRegistryValues(const RootKey: HKEY; const Name: String;
    var ValueList: StringArray): Boolean;
begin
  Result := RegEnum(RootKey, Name, ValueList, False);
end;

function EnumRegistryKeys(const RootKey: HKEY; const Name: String;
    var KeyList: StringArray): Boolean;
begin
  Result := RegEnum(RootKey, Name, KeyList, True);
end;



end.

