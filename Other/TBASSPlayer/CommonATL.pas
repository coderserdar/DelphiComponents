{ *************************************************************************** }
{                                                                             }
{ Audio Tools Library                                                         }
{ Global Unicode functions                                                    }
{                                                                             }
{ http://mac.sourceforge.net/atl/                                             }
{ e-mail: macteam@users.sourceforge.net                                       }
{                                                                             }
{ Copyright (c) 2003-2005 by The MAC Team                                     }
{                                                                             }
{ Version 1.0 (24 March 2005)                                                 }
{                                                                             }
{ This library is free software; you can redistribute it and/or               }
{ modify it under the terms of the GNU Lesser General Public                  }
{ License as published by the Free Software Foundation; either                }
{ version 2.1 of the License, or (at your option) any later version.          }
{                                                                             }
{ This library is distributed in the hope that it will be useful,             }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of              }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU           }
{ Lesser General Public License for more details.                             }
{                                                                             }
{ You should have received a copy of the GNU Lesser General Public            }
{ License along with this library; if not, write to the Free Software         }
{ Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA   }
{                                                                             }
{ *************************************************************************** }

unit CommonATL;

interface

uses
  Classes;

function UTF8ToWString(const S: ansistring): WideString;
function WStringToUTF8(const S: WideString): ansistring;
function UpCaseW(const C: WideChar): WideChar;
function LowCaseW(const C: WideChar): WideChar;
function UpperCaseW(const S: WideString): WideString;
function LowerCaseW(const S: WideString): WideString;
function GetCurDir: WideString;
procedure SetCurDir(const Dir: WideString);
function FileCreateW(const FileName: WideString): Integer;
function FileOpenW(const FileName: WideString; Mode: LongWord): Integer;
function WideFileSetDate(const FileName: WideString; Age: Integer): Integer;

{type
  TFileStreamW = class(THandleStream)
  public
    constructor Create(const FileName: WideString; Mode: Word); overload;
    destructor Destroy; override;
  end; }

implementation

uses
  SysUtils, Windows, TntCollection{, TntClasses};

(* -------------------------------------------------------------------------- *)

function UTF8ToWString(const S: ansiString): WideString;
var
  Len: Integer;
begin
  Len := MultiByteToWideChar(CP_UTF8, 0, PAnsiChar(S), Length(S), nil, 0);
  SetLength(Result, Len);
  MultiByteToWideChar(CP_UTF8, 0, PAnsiChar(S), Length(S), PWideChar(Result), Len);
end;

(* -------------------------------------------------------------------------- *)

function WStringToUTF8(const S: WideString): ansiString;
var
  Len: Integer;
begin
  Len := WideCharToMultiByte(CP_UTF8, 0, PWideChar(S), Length(S), nil, 0, nil, nil);
  SetLength(Result, Len);
  WideCharToMultiByte(CP_UTF8, 0, PWideChar(S), Length(S), PAnsiChar(Result), Len, nil, nil);
end;

(* -------------------------------------------------------------------------- *)

function UpCaseW(const C: WideChar): WideChar;
begin
  Result := C;
  CharUpperW(PWideChar(Result));
end;

(* -------------------------------------------------------------------------- *)

function LowCaseW(const C: WideChar): WideChar;
begin
  Result := C;
  CharLowerW(PWideChar(Result));
end;

(* -------------------------------------------------------------------------- *)

function UpperCaseW(const S: WideString): WideString;
var
  Len: Integer;
begin
  Len := Length(S);
  Result := S;
  if Len > 0 then
    CharUpperBuffW(Pointer(Result), Len);
end;

(* -------------------------------------------------------------------------- *)

function LowerCaseW(const S: WideString): WideString;
var
  Len: Integer;
begin
  Len := Length(S);
  Result := S;
  if Len > 0 then
    CharLowerBuffW(Pointer(Result), Len);
end;

(* -------------------------------------------------------------------------- *)

function GetCurDir: WideString;
var
  Len: Integer;
begin
  SetLength(Result, 512);
  Len := GetCurrentDirectoryW(512, PWideChar(Result));
  if Len <> 0 then
  begin
    SetLength(Result, Len);
    GetCurrentDirectoryW(Len, PWideChar(Result));
  end;
end;

(* -------------------------------------------------------------------------- *)

procedure SetCurDir(const Dir: WideString);
begin
  if SetCurrentDirectoryW(PWideChar(Dir)) then
  begin
    FormatMessageW(FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM,
      nil, GetLastError, 0, PWideChar(Error), 0, nil);
    raise EInOutError.CreateFmt('Cannot change to directory "%s". %s',
      [ExpandFilename(Dir), SysErrorMessage(GetLastError)]);
  end;
end;

(* -------------------------------------------------------------------------- *)

function FileCreateW(const FileName: WideString): Integer;
begin
  Result := Integer(CreateFileW(PWideChar(FileName), GENERIC_READ or
    GENERIC_WRITE,
    0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0));
end;

(* -------------------------------------------------------------------------- *)

function FileOpenW(const FileName: WideString; Mode: LongWord): Integer;
const
  AccessMode: array[0..2] of LongWord = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of LongWord = (
    0,
    0,
    FILE_SHARE_READ,
    FILE_SHARE_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE);
begin
  Result := -1;
  if ((Mode and 3) <= fmOpenReadWrite) and
    ((Mode and $F0) <= fmShareDenyNone) then
    Result := Integer(CreateFileW(PWideChar(FileName), AccessMode[Mode and 3],
      ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL, 0));
end;

(* -------------------------------------------------------------------------- *)

function WideFileSetDate(const FileName: WideString; Age: Integer): Integer;
var
  handle: THandle;
begin
  handle := WideFileOpen(FileName, fmOpenWrite);
  if handle = THandle(-1) then
    Result := GetLastError
  else
  begin
    Result := FileSetDate(handle, Age);
    FileClose(handle);
  end;
end;

(* -------------------------------------------------------------------------- *)
{ TFileStreamW }

{constructor TFileStreamW.Create(const FileName: WideString; Mode: Word);
begin
  if Mode = fmCreate then
  begin
    inherited Create(FileCreateW(FileName));
    if FHandle < 0 then
      raise EFCreateError.CreateResFmt(@SFCreateErrorEx,
        [ExpandFileName(FileName), SysErrorMessage(GetLastError)]);
  end
  else
  begin
    inherited Create(FileOpenW(FileName, Mode));
    if FHandle < 0 then
      raise EFOpenError.CreateResFmt(@SFOpenErrorEx, [ExpandFileName(FileName),
        SysErrorMessage(GetLastError)]);
  end;
end;

(* -------------------------------------------------------------------------- *)

destructor TFileStreamW.Destroy;
begin
  if FHandle >= 0 then
    FileClose(FHandle);
  inherited Destroy;
end; }

(* -------------------------------------------------------------------------- *)

end.

