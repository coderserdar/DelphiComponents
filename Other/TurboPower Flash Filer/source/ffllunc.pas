{*********************************************************}
{* FlashFiler: Conversion of drive:path to UNC names     *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit ffllunc;

interface

uses
  Windows,
  Messages,
  SysUtils,
  ffllbase;

function FFExpandUNCFileName(const FN : TffFullFileName) : TffFullFileName;

implementation

{===Win32 Helper routines============================================}
function GetUniversalNameNT(const EFN : TffFullFileName) : TffFullFileName;
var
  BufSize : DWORD;
  EFNZ    : TffStringZ;
  Buffer  : array [0..1023] of byte;
begin
  FFStrPCopy(EFNZ, EFN);
  BufSize := sizeof(Buffer);
  if WNetGetUniversalName(EFNZ, UNIVERSAL_NAME_INFO_LEVEL,
                          @Buffer, BufSize) = NO_ERROR then
    Result := FFStrPasLimit(PUniversalNameInfo(@Buffer).lpUniversalName,
                            pred(sizeof(TffFullFileName)))
  else
    Result := EFN;
end;
{--------}
function GetUniversalName95(const EFN : TffFullFileName;
                              var UNC : TffFullFileName) : boolean;
type
  PNetResArray = ^TNetResArray;
  TNetResArray = array [0..127] of TNetResource;
var
  chLocal     : AnsiChar;
  hEnum       : THandle;
  dwResult    : DWORD;
  cbBuffer    : DWORD;
  NetResource : PNetResArray;
  dwSize      : DWORD;
  cEntries    : DWORD;
  i           : integer;
begin
  {Note: according to Microsoft's article Q131416, the Windows 95
         version of WNetGetUniversalName is broken, hence the funny
         code (a pretty direct translation of MS's workaround using
         length byte strings and try..finallys)}
  Result := false;
  // cursory validation
  if (length(EFN) < 3) then
    Exit;
  // get the local drive letter
  chLocal := UpCase(EFN[1]);
  // more cursory validation
  if (chLocal < 'A') or (chLocal > 'Z') or
     (EFN[2] <> ':') or (EFN[3] <> '\' ) then
    Exit;
  {open a network enumeration}
  if (WNetOpenEnum(RESOURCE_CONNECTED, RESOURCETYPE_DISK,
                   0, nil, hEnum) <> NO_ERROR) then
    Exit;
  try
    // start with a reasonable buffer size
    cbBuffer := 50 * sizeof(TNetResource);
    GetMem(NetResource, cbBuffer);
    try
      while true do begin
        dwSize := cbBuffer;
        cEntries := $7FFFFFFF;
        dwResult := WNetEnumResource(hEnum, cEntries, NetResource, dwSize);
        if (dwResult = ERROR_MORE_DATA) then begin
          // the buffer was too small, enlarge
          cbBuffer := dwSize;
          ReallocMem(NetResource, cbBuffer);
          continue;
        end;
        if (dwResult <> NO_ERROR) then
          Exit;
        // search for the specified drive letter
        for i := 0 to pred(cEntries) do
          with NetResource^[i] do
            if (lpLocalName <> nil) and
               (chLocal = UpCase(lpLocalName[0])) then begin
              // match
              Result := true;
              // build a UNC name
              UNC := FFStrPasLimit(lpRemoteName, pred(sizeof(TffFullFileName)));
              FFShStrConcat(UNC, Copy(EFN, 3, 255));
              Exit;
            end;
      end;
    finally
      FreeMem(NetResource, cbBuffer);
    end;{try..finally}
  finally
    WNetCloseEnum(hEnum);
  end;{try..finally}
end;
{--------}
function GetUniversalName(const EFN : TffFullFileName) : TffFullFileName;
begin
  if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) then begin
    if not GetUniversalName95(EFN, Result) then
      Result := EFN;
  end
  else
    Result := GetUniversalNameNT(EFN);
end;
{====================================================================}

function FFExpandUNCFileName(const FN : TffFullFileName) : TffFullFileName;
begin
  Result := GetUniversalName(FFExpandFileName(FN));
end;

end.
