{*********************************************************}
{* FlashFiler: 32-bit file management routines inc file  *}
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


{===File management routines=========================================}
procedure FFCopyFile(const FromFileName, ToFileName : TffFullFileName);
  var
    WinError : TffWord32;
    FromZ    : TffMaxPathZ;
    ToZ      : TffMaxPathZ;
  begin
    if not Windows.CopyFile(FFStrPCopy(FromZ, FromFileName),
                            FFStrPCopy(ToZ, ToFileName), True) then
      begin
        WinError := GetLastError;
        FFRaiseException(EffServerException, ffStrResServer, fferrCopyFile,
                         [WinError, SysErrorMessage(WinError)]);
      end;
  end;
{--------}
procedure FFDeleteFile(const FileName : TffFullFileName);
  var
    WinError : TffWord32;
    FZ       : TffMaxPathZ;
  begin
    if not Windows.DeleteFile(FFStrPCopy(FZ, FileName)) then
      begin
        WinError := GetLastError;
        FFRaiseException(EffServerException, ffStrResGeneral, fferrDeleteFile,     {!!.10}
                         [WinError, SysErrorMessage(WinError)]);
      end;
  end;
{--------}
procedure FFRenameFile(const OldFileName, NewFileName : TffFullFileName);
  var
    WinError : TffWord32;
    OldZ     : TffMaxPathZ;
    NewZ     : TffMaxPathZ;
  begin
    if not Windows.MoveFile(FFStrPCopy(OldZ, OldFileName), FFStrPCopy(NewZ, NewFileName)) then
      begin
        WinError := GetLastError;
        FFRaiseException(EffServerException, ffStrResServer, fferrRenameFile,
                         [WinError, SysErrorMessage(WinError)]);
      end;
  end;
{====================================================================}


