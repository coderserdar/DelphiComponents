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

Procedure FFCopyFile(Const FromFileName, ToFileName: TffFullFileName);
Var
  WinError: TffWord32;
  FromZ: TffMaxPathZ;
  ToZ: TffMaxPathZ;
Begin
  If Not Windows.CopyFile(FFStrPCopy(FromZ, FromFileName),
    FFStrPCopy(ToZ, ToFileName), True) Then
    Begin
      WinError := GetLastError;
      FSRaiseException(EfsServerException, fsStrResServer, fserrCopyFile,
        [WinError, SysErrorMessage(WinError)]);
    End;
End;
{--------}

Procedure FFDeleteFile(Const FileName: TffFullFileName);
Var
  WinError: TffWord32;
  FZ: TffMaxPathZ;
Begin
  If Not Windows.DeleteFile(FFStrPCopy(FZ, FileName)) Then
    Begin
      WinError := GetLastError;
      FSRaiseException(EfsServerException, fsStrResGeneral, fserrDeleteFile, {!!.10}
        [WinError, SysErrorMessage(WinError)]);
    End;
End;
{--------}

Procedure FFRenameFile(Const OldFileName, NewFileName: TffFullFileName);
Var
  WinError: TffWord32;
  OldZ: TffMaxPathZ;
  NewZ: TffMaxPathZ;
Begin
  If Not Windows.MoveFile(FFStrPCopy(OldZ, OldFileName), FFStrPCopy(NewZ, NewFileName)) Then
    Begin
      WinError := GetLastError;
      FSRaiseException(EfsServerException, fsStrResServer, fserrRenameFile,
        [WinError, SysErrorMessage(WinError)]);
    End;
End;
{====================================================================}

