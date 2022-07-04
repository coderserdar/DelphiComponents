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

procedure FFCloseFilePrim32(aFI : PffFileInfo);
var
  WinError : TffWord32;
begin
  {$IFDEF Tracing}
  FFAddTrace(foClose, aFI^.fiHandle, sizeof(aFI^.fiHandle));
  {$ENDIF}
  {close the file handle}
  if not CloseHandle(aFI^.fiHandle) then begin
    WinError := GetLastError;
    {$IFDEF Tracing}
    FFAddTrace(foUnknown, WinError, sizeof(WinError));
    {$ENDIF}
    FFRaiseException(EffServerException, ffStrResServer, fferrCloseFailed,
                     [aFI.fiName^, WinError, SysErrorMessage(WinError)]);
  end;
end;
{--------}
procedure FFFlushFilePrim32(aFI : PffFileInfo);
var
  WinError : TffWord32;
begin
  {$IFDEF Tracing}
  FFAddTrace(foFlush, aFI^.fiHandle, sizeof(aFI^.fiHandle));
  {$ENDIF}
  if not FlushFileBuffers(aFI^.fiHandle) then begin
    WinError := GetLastError;
    {$IFDEF Tracing}
    FFAddTrace(foUnknown, WinError, sizeof(WinError));
    {$ENDIF}
    FFRaiseException(EffServerException, ffStrResServer, fferrFlushFailed,
                     [aFI.fiName^, WinError, SysErrorMessage(WinError)]);
  end;
end;
{--------}
function FFGetPositionFilePrim32(aFI : PffFileInfo) : TffInt64;
var
  WinError : TffWord32;
  HighWord : TffWord32;
  {$IFDEF Tracing}
  Params     : array [0..1] of Longint;
  {$ENDIF}
begin
  {$IFDEF Tracing}
  Params[0] := aFI^.fiHandle;
  Params[1] := 0;
  FFAddTrace(foSeek, Params, sizeof(Params));
  {$ENDIF}
  HighWord := 0;
  result.iLow := SetFilePointer(aFI^.fiHandle, 0, @HighWord , FILE_CURRENT);
  if (Result.iLow = $FFFFFFFF) then begin
    WinError := GetLastError;
    {$IFDEF Tracing}
    FFAddTrace(foUnknown, WinError, sizeof(WinError));
    {$ENDIF}
    FFRaiseException(EffServerException, ffStrResServer, fferrSeekFailed,
                     [aFI.fiName^, 0, 0, WinError, SysErrorMessage(WinError)]);
  end;
  result.ihigh := HighWord;
  {$IFDEF Tracing}
  FFAddTrace(foUnknown, Result, sizeof(Result));
  {$ENDIF}
end;
{--------}
function FFOpenFilePrim32(aName       : PAnsiChar;
                          aOpenMode   : TffOpenMode;
                          aShareMode  : TffShareMode;
                          aWriteThru  : Boolean;
                          aCreateFile : Boolean) : THandle;
var
  AttrFlags  : TffWord32;
  CreateMode : TffWord32;
  OpenMode   : TffWord32;
  ShareMode  : TffWord32;
  WinError   : TffWord32;
begin
  {$IFDEF Tracing}
  FFAddTrace(foOpen, aName^, succ(StrLen(aName)));
  {$ENDIF}
  {initialise parameters to CreateFile}
  if (aOpenMode = omReadOnly) then
    OpenMode := GENERIC_READ
  else
    OpenMode := GENERIC_READ or GENERIC_WRITE;
  if (aShareMode = smExclusive) then
    ShareMode := 0
  else if (aShareMode = smShareRead) then                              {!!.06}
    ShareMode := FILE_SHARE_READ                                       {!!.06}
  else
    ShareMode := FILE_SHARE_READ or FILE_SHARE_WRITE;
  if aCreateFile then
    CreateMode := CREATE_ALWAYS
  else
    CreateMode := OPEN_EXISTING;
  if aWriteThru then
    AttrFlags := FILE_ATTRIBUTE_NORMAL or FILE_FLAG_WRITE_THROUGH
  else
    AttrFlags := FILE_ATTRIBUTE_NORMAL;
  {open the file}
  Result := CreateFile(aName,
                       OpenMode,
                       ShareMode,
                       nil,            {!! Security attrs}
                       CreateMode,
                       AttrFlags,
                       0);
  if (Result = INVALID_HANDLE_VALUE) then begin
    WinError := GetLastError;
    {$IFDEF Tracing}
    FFAddTrace(foUnknown, WinError, sizeof(WinError));
    {$ENDIF}
    FFRaiseException(EffServerException, ffStrResServer, fferrOpenFailed,
                     [aName, WinError, SysErrorMessage(WinError)]);
  end;
  {$IFDEF Tracing}
  FFAddTrace(foUnknown, Result, sizeof(Result));
  {$ENDIF}
end;
{--------}
procedure FFPositionFilePrim32(aFI     : PffFileInfo;
                         const aOffset : TffInt64);
var
  SeekResult : TffWord32;
  WinError   : TffWord32;
  {$IFDEF Tracing}
  Params     : array [0..1] of Longint;
  {$ENDIF}
begin
  {$IFDEF Tracing}
  Params[0] := aFI^.fiHandle;
  Params[1] := aOffset.iLow;
  FFAddTrace(foSeek, Params, sizeof(Params));
  {$ENDIF}
  SeekResult := SetFilePointer(aFI^.fiHandle, aOffset.iLow, @aOffset.iHigh,
                               FILE_BEGIN);
  if (SeekResult = $FFFFFFFF) then begin
    WinError := GetLastError;
    {$IFDEF Tracing}
    FFAddTrace(foUnknown, WinError, sizeof(WinError));
    {$ENDIF}
    FFRaiseException(EffServerException, ffStrResServer, fferrSeekFailed,
                     [aFI.fiName^, aOffset.iLow, aOffset.iHigh, WinError,
                      SysErrorMessage(WinError)]);
  end;
  {$IFDEF Tracing}
  FFAddTrace(foUnknown, SeekResult, sizeof(SeekResult));
  {$ENDIF}
end;
{--------}
function FFPositionFileEOFPrim32(aFI : PffFileInfo) : TffInt64;
var
  WinError : TffWord32;
  highWord : TffWord32;
  {$IFDEF Tracing}
  Params   : array [0..1] of Longint;
  {$ENDIF}
begin
  {$IFDEF Tracing}
  Params[0] := aFI^.fiHandle;
  Params[1] := -1;
  FFAddTrace(foSeek, Params, sizeof(Params));
  {$ENDIF}
  highWord := 0;
  Result.iLow := SetFilePointer(aFI^.fiHandle, 0, @highWord, FILE_END);
  if (Result.iLow = $FFFFFFFF) then begin
    WinError := GetLastError;
    {$IFDEF Tracing}
    FFAddTrace(foUnknown, WinError, sizeof(WinError));
    {$ENDIF}
    FFRaiseException(EffServerException, ffStrResServer, fferrSeekFailed,
                     [aFI.fiName^, 0, 0, WinError, SysErrorMessage(WinError)]);
  end;
  result.iHigh := HighWord;
  {$IFDEF Tracing}
  FFAddTrace(foUnknown, Result, sizeof(Result));
  {$ENDIF}
end;
{--------}
function FFReadFilePrim32(aFI : PffFileInfo;
                          aToRead : TffWord32;
                      var aBuffer) : TffWord32;
var
  WinError  : TffWord32;
  BytesRead : DWORD;
  {$IFDEF Tracing}
  Params    : array [0..1] of Longint;
  {$ENDIF}
begin
  {$IFDEF Tracing}
  Params[0] := aFI^.fiHandle;
  Params[1] := aToRead;
  FFAddTrace(foRead, Params, sizeof(Params));
  {$ENDIF}
  if not ReadFile(aFI^.fiHandle, aBuffer, aToRead, BytesRead, nil) then begin
    WinError := GetLastError;
    {$IFDEF Tracing}
    FFAddTrace(foUnknown, WinError, sizeof(WinError));
    {$ENDIF}
    FFRaiseException(EffServerException, ffStrResServer, fferrReadFailed,
                     [aFI.fiName^, WinError, SysErrorMessage(WinError)]);
  end;
  Result := BytesRead;
  {$IFDEF Tracing}
  FFAddTrace(foUnknown, Result, sizeof(Result));
  {$ENDIF}
end;
{--------}
procedure FFSetEOFPrim32(aFI : PffFileInfo;
                         const aOffset : TffInt64);
var
  WinError : TffWord32;
  {$IFDEF Tracing}
  Params   : array [0..1] of Longint;
  {$ENDIF}
begin
  {$IFDEF Tracing}
  Params[0] := aFI^.fiHandle;
  Params[1] := aOffset.iLow;
  FFAddTrace(foSetEOF, Params, sizeof(Params));
  {$ENDIF}
  FFPositionFilePrim(aFI, aOffset);
  if not Windows.SetEndOfFile(aFI^.fiHandle) then begin
    WinError := GetLastError;
    {$IFDEF Tracing}
    FFAddTrace(foUnknown, WinError, sizeof(WinError));
    {$ENDIF}
    FFRaiseException(EffServerException, ffStrResServer, fferrSetEOFFailed,
                     [aFI.fiName^, WinError, SysErrorMessage(WinError)]);
  end;
end;
{--------}
function FFWriteFilePrim32(aFI : PffFileInfo;
                           aToWrite : TffWord32;
                     const aBuffer) : TffWord32;
var
  WinError : TffWord32;
  BytesWritten : DWORD;
  {$IFDEF Tracing}
  Params   : array [0..2] of Longint;
  {$ENDIF}
begin
  {$IFDEF Tracing}
  Params[0] := aFI^.fiHandle;
  Params[1] := aToWrite;
  FFAddTrace(foWrite, Params, sizeof(Params));
  {$ENDIF}
  if not WriteFile(aFI^.fiHandle, aBuffer, aToWrite, BytesWritten, nil) then begin
    WinError := GetLastError;
    {$IFDEF Tracing}
    FFAddTrace(foUnknown, WinError, sizeof(WinError));
    {$ENDIF}
    FFRaiseException(EffServerException, ffStrResServer, fferrWriteFailed,
                     [aFI.fiName^, WinError, SysErrorMessage(WinError)]);
  end;
  Result := BytesWritten;
  {$IFDEF Tracing}
  FFAddTrace(foUnknown, Result, sizeof(Result));
  {$ENDIF}
end;
{====================================================================}


{===Default Sleep routine============================================}
procedure FFSleepPrim32(MilliSecs : Longint);
begin
  Windows.Sleep(MilliSecs);
end;
{====================================================================}

