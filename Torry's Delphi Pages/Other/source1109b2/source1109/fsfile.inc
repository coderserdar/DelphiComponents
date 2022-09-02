{*********************************************************}
{* FlashFiler: 32-bit file access routines include file  *}
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

{===File access routines (primitives)================================}

Procedure FFCloseFilePrim32(aFI: PffFileInfo);
Var
  WinError: TffWord32;
Begin
  {$IFDEF Tracing}
  FFAddTrace(foClose, aFI^.fiHandle, sizeof(aFI^.fiHandle));
  {$ENDIF}
  {close the file handle}
  If Not CloseHandle(aFI^.fiHandle) Then
    Begin
      WinError := GetLastError;
      {$IFDEF Tracing}
      FFAddTrace(foUnknown, WinError, sizeof(WinError));
      {$ENDIF}
      FSRaiseException(EfsServerException, fsStrResServer, fserrCloseFailed,
        [aFI.fiName^, WinError, SysErrorMessage(WinError)]);
    End;
End;
{--------}

Procedure FFFlushFilePrim32(aFI: PffFileInfo);
Var
  WinError: TffWord32;
Begin
  {$IFDEF Tracing}
  FFAddTrace(foFlush, aFI^.fiHandle, sizeof(aFI^.fiHandle));
  {$ENDIF}
  If Not FlushFileBuffers(aFI^.fiHandle) Then
    Begin
      WinError := GetLastError;
      {$IFDEF Tracing}
      FFAddTrace(foUnknown, WinError, sizeof(WinError));
      {$ENDIF}
      FSRaiseException(EfsServerException, fsStrResServer, fserrFlushFailed,
        [aFI.fiName^, WinError, SysErrorMessage(WinError)]);
    End;
End;
{--------}

Function FFGetPositionFilePrim32(aFI: PffFileInfo): TffInt64;
Var
  WinError: TffWord32;
  HighWord: TffWord32;
  {$IFDEF Tracing}
  Params: Array[0..1] Of Longint;
  {$ENDIF}
Begin
  {$IFDEF Tracing}
  Params[0] := aFI^.fiHandle;
  Params[1] := 0;
  FFAddTrace(foSeek, Params, sizeof(Params));
  {$ENDIF}
  HighWord := 0;
  Result.iLow := SetFilePointer(aFI^.fiHandle, 0, @HighWord, FILE_CURRENT);
  If (Result.iLow = $FFFFFFFF) Then
    Begin
      WinError := GetLastError;
      {$IFDEF Tracing}
      FFAddTrace(foUnknown, WinError, sizeof(WinError));
      {$ENDIF}
      FSRaiseException(EfsServerException, fsStrResServer, fserrSeekFailed,
        [aFI.fiName^, 0, 0, WinError, SysErrorMessage(WinError)]);
    End;
  Result.ihigh := HighWord;
  {$IFDEF Tracing}
  FFAddTrace(foUnknown, Result, sizeof(Result));
  {$ENDIF}
End;
{--------}

Function FFOpenFilePrim32(aName: PAnsiChar;
  aOpenMode: TffOpenMode;
  aShareMode: TffShareMode;
  aWriteThru: Boolean;
  aCreateFile: Boolean): THandle;
Var
  AttrFlags: TffWord32;
  CreateMode: TffWord32;
  OpenMode: TffWord32;
  ShareMode: TffWord32;
  WinError: TffWord32;
Begin
  {$IFDEF Tracing}
  FFAddTrace(foOpen, aName^, succ(StrLen(aName)));
  {$ENDIF}
  {initialise parameters to CreateFile}
  If (aOpenMode = omReadOnly) Then
    OpenMode := GENERIC_READ
  Else
    OpenMode := GENERIC_READ Or GENERIC_WRITE;
  If (aShareMode = smExclusive) Then
    ShareMode := 0
  Else If (aShareMode = smShareRead) Then {!!.06}
    ShareMode := FILE_SHARE_READ {!!.06}
  Else
    ShareMode := FILE_SHARE_READ Or FILE_SHARE_WRITE;
  If aCreateFile Then
    CreateMode := CREATE_ALWAYS
  Else
    CreateMode := OPEN_EXISTING;
  If aWriteThru Then
    AttrFlags := FILE_ATTRIBUTE_NORMAL Or FILE_FLAG_WRITE_THROUGH
  Else
    AttrFlags := FILE_ATTRIBUTE_NORMAL;
  {open the file}
  Result := CreateFile(aName,
    OpenMode,
    ShareMode,
    Nil, {!! Security attrs}
    CreateMode,
    AttrFlags,
    0);
  If (Result = INVALID_HANDLE_VALUE) Then
    Begin
      WinError := GetLastError;
      {$IFDEF Tracing}
      FFAddTrace(foUnknown, WinError, sizeof(WinError));
      {$ENDIF}
      FSRaiseException(EfsServerException, fsStrResServer, fserrOpenFailed,
        [aName, WinError, SysErrorMessage(WinError)]);
    End;
  {$IFDEF Tracing}
  FFAddTrace(foUnknown, Result, sizeof(Result));
  {$ENDIF}
End;
{--------}

Procedure FFPositionFilePrim32(aFI: PffFileInfo;
  Const aOffset: TffInt64);
Var
  SeekResult: TffWord32;
  WinError: TffWord32;
  {$IFDEF Tracing}
  Params: Array[0..1] Of Longint;
  {$ENDIF}
Begin
  {$IFDEF Tracing}
  Params[0] := aFI^.fiHandle;
  Params[1] := aOffset.iLow;
  FFAddTrace(foSeek, Params, sizeof(Params));
  {$ENDIF}
  SeekResult := SetFilePointer(aFI^.fiHandle, aOffset.iLow, @aOffset.iHigh,
    FILE_BEGIN);
  If (SeekResult = $FFFFFFFF) Then
    Begin
      WinError := GetLastError;
      {$IFDEF Tracing}
      FFAddTrace(foUnknown, WinError, sizeof(WinError));
      {$ENDIF}
      FSRaiseException(EfsServerException, fsStrResServer, fserrSeekFailed,
        [aFI.fiName^, aOffset.iLow, aOffset.iHigh, WinError,
        SysErrorMessage(WinError)]);
    End;
  {$IFDEF Tracing}
  FFAddTrace(foUnknown, SeekResult, sizeof(SeekResult));
  {$ENDIF}
End;
{--------}

Function FFPositionFileEOFPrim32(aFI: PffFileInfo): TffInt64;
Var
  WinError: TffWord32;
  highWord: TffWord32;
  {$IFDEF Tracing}
  Params: Array[0..1] Of Longint;
  {$ENDIF}
Begin
  {$IFDEF Tracing}
  Params[0] := aFI^.fiHandle;
  Params[1] := -1;
  FFAddTrace(foSeek, Params, sizeof(Params));
  {$ENDIF}
  highWord := 0;
  Result.iLow := SetFilePointer(aFI^.fiHandle, 0, @highWord, FILE_END);
  If (Result.iLow = $FFFFFFFF) Then
    Begin
      WinError := GetLastError;
      {$IFDEF Tracing}
      FFAddTrace(foUnknown, WinError, sizeof(WinError));
      {$ENDIF}
      FSRaiseException(EfsServerException, fsStrResServer, fserrSeekFailed,
        [aFI.fiName^, 0, 0, WinError, SysErrorMessage(WinError)]);
    End;
  Result.iHigh := HighWord;
  {$IFDEF Tracing}
  FFAddTrace(foUnknown, Result, sizeof(Result));
  {$ENDIF}
End;
{--------}

Function FFReadFilePrim32(aFI: PffFileInfo;
  aToRead: TffWord32;
  Var aBuffer): TffWord32;
Var
  WinError: TffWord32;
  BytesRead: DWORD;
  {$IFDEF Tracing}
  Params: Array[0..1] Of Longint;
  {$ENDIF}
Begin
  {$IFDEF Tracing}
  Params[0] := aFI^.fiHandle;
  Params[1] := aToRead;
  FFAddTrace(foRead, Params, sizeof(Params));
  {$ENDIF}
  If Not ReadFile(aFI^.fiHandle, aBuffer, aToRead, BytesRead, Nil) Then
    Begin
      WinError := GetLastError;
      {$IFDEF Tracing}
      FFAddTrace(foUnknown, WinError, sizeof(WinError));
      {$ENDIF}
      FSRaiseException(EfsServerException, fsStrResServer, fserrReadFailed,
        [aFI.fiName^, WinError, SysErrorMessage(WinError)]);
    End;
  Result := BytesRead;
  {$IFDEF Tracing}
  FFAddTrace(foUnknown, Result, sizeof(Result));
  {$ENDIF}
End;
{--------}

Procedure FFSetEOFPrim32(aFI: PffFileInfo;
  Const aOffset: TffInt64);
Var
  WinError: TffWord32;
  {$IFDEF Tracing}
  Params: Array[0..1] Of Longint;
  {$ENDIF}
Begin
  {$IFDEF Tracing}
  Params[0] := aFI^.fiHandle;
  Params[1] := aOffset.iLow;
  FFAddTrace(foSetEOF, Params, sizeof(Params));
  {$ENDIF}
  FFPositionFilePrim(aFI, aOffset);
  If Not Windows.SetEndOfFile(aFI^.fiHandle) Then
    Begin
      WinError := GetLastError;
      {$IFDEF Tracing}
      FFAddTrace(foUnknown, WinError, sizeof(WinError));
      {$ENDIF}
      FSRaiseException(EfsServerException, fsStrResServer, fserrSetEOFFailed,
        [aFI.fiName^, WinError, SysErrorMessage(WinError)]);
    End;
End;
{--------}

Function FFWriteFilePrim32(aFI: PffFileInfo;
  aToWrite: TffWord32;
  Const aBuffer): TffWord32;
Var
  WinError: TffWord32;
  BytesWritten: DWORD;
  {$IFDEF Tracing}
  Params: Array[0..2] Of Longint;
  {$ENDIF}
Begin
  {$IFDEF Tracing}
  Params[0] := aFI^.fiHandle;
  Params[1] := aToWrite;
  FFAddTrace(foWrite, Params, sizeof(Params));
  {$ENDIF}
  If Not WriteFile(aFI^.fiHandle, aBuffer, aToWrite, BytesWritten, Nil) Then
    Begin
      WinError := GetLastError;
      {$IFDEF Tracing}
      FFAddTrace(foUnknown, WinError, sizeof(WinError));
      {$ENDIF}
      FSRaiseException(EfsServerException, fsStrResServer, fserrWriteFailed,
        [aFI.fiName^, WinError, SysErrorMessage(WinError)]);
    End;
  Result := BytesWritten;
  {$IFDEF Tracing}
  FFAddTrace(foUnknown, Result, sizeof(Result));
  {$ENDIF}
End;
{====================================================================}

{===Default Sleep routine============================================}

Procedure FFSleepPrim32(MilliSecs: Longint);
Begin
  Windows.Sleep(MilliSecs);
End;
{====================================================================}

