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

{$I fsdefine.inc}

Unit fsllunc;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  fsllbase;

Function FFExpandUNCFileName(Const FN: TffFullFileName): TffFullFileName;

Implementation

{===Win32 Helper routines============================================}

Function GetUniversalNameNT(Const EFN: TffFullFileName): TffFullFileName;
Var
  BufSize: DWORD;
  EFNZ: TffStringZ;
  Buffer: Array[0..1023] Of Byte;
Begin
  FFStrPCopy(EFNZ, EFN);
  BufSize := sizeof(Buffer);
  If WNetGetUniversalName(EFNZ, UNIVERSAL_NAME_INFO_LEVEL,
    @Buffer, BufSize) = NO_ERROR Then
    Result := FFStrPasLimit(PUniversalNameInfo(@Buffer).lpUniversalName,
      pred(sizeof(TffFullFileName)))
  Else
    Result := EFN;
End;
{--------}

Function GetUniversalName95(Const EFN: TffFullFileName;
  Var UNC: TffFullFileName): boolean;
Type
  PNetResArray = ^TNetResArray;
  TNetResArray = Array[0..127] Of TNetResource;
Var
  chLocal: AnsiChar;
  hEnum: THandle;
  dwResult: DWORD;
  cbBuffer: DWORD;
  NetResource: PNetResArray;
  dwSize: DWORD;
  cEntries: DWORD;
  i: Integer;
Begin
  {Note: according to Microsoft's article Q131416, the Windows 95
         version of WNetGetUniversalName is broken, hence the funny
         code (a pretty direct translation of MS's workaround using
         length byte strings and try..finallys)}
  Result := False;
  // cursory validation
  If (length(EFN) < 3) Then
    Exit;
  // get the local drive letter
  chLocal := UpCase(EFN[1]);
  // more cursory validation
  If (chLocal < 'A') Or (chLocal > 'Z') Or
    (EFN[2] <> ':') Or (EFN[3] <> '\') Then
    Exit;
  {open a network enumeration}
  If (WNetOpenEnum(RESOURCE_CONNECTED, RESOURCETYPE_DISK,
    0, Nil, hEnum) <> NO_ERROR) Then
    Exit;
  Try
    // start with a reasonable buffer size
    cbBuffer := 50 * sizeof(TNetResource);
    GetMem(NetResource, cbBuffer);
    Try
      While True Do
        Begin
          dwSize := cbBuffer;
          cEntries := $7FFFFFFF;
          dwResult := WNetEnumResource(hEnum, cEntries, NetResource, dwSize);
          If (dwResult = ERROR_MORE_DATA) Then
            Begin
              // the buffer was too small, enlarge
              cbBuffer := dwSize;
              ReallocMem(NetResource, cbBuffer);
              continue;
            End;
          If (dwResult <> NO_ERROR) Then
            Exit;
          // search for the specified drive letter
          For i := 0 To pred(cEntries) Do
            With NetResource^[i] Do
              If (lpLocalName <> Nil) And
                (chLocal = UpCase(lpLocalName[0])) Then
                Begin
                  // match
                  Result := True;
                  // build a UNC name
                  UNC := FFStrPasLimit(lpRemoteName, pred(sizeof(TffFullFileName)));
                  FFShStrConcat(UNC, Copy(EFN, 3, 255));
                  Exit;
                End;
        End;
    Finally
      FreeMem(NetResource, cbBuffer);
    End; {try..finally}
  Finally
    WNetCloseEnum(hEnum);
  End; {try..finally}
End;
{--------}

Function GetUniversalName(Const EFN: TffFullFileName): TffFullFileName;
Begin
  If (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) Then
    Begin
      If Not GetUniversalName95(EFN, Result) Then
        Result := EFN;
    End
  Else
    Result := GetUniversalNameNT(EFN);
End;
{====================================================================}

Function FFExpandUNCFileName(Const FN: TffFullFileName): TffFullFileName;
Begin
  Result := GetUniversalName(FFExpandFileName(FN));
End;

End.

