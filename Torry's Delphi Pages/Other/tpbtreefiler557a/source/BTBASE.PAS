{********************************************************************}
{* BTBase.PAS - B-Tree Filer basic types & constants                *}
{********************************************************************}

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
 * The Original Code is TurboPower B-Tree Filer
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Based in part on code written by Ralf Nagel
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I BTDEFINE.INC}

unit BTBase;

{Notes: This unit is designed so that all compilers have the same
        types and constants as each other. This in turn makes the
        code easier}

interface

{$IFDEF Win32}                                                 {!!.54}
uses                                                           {!!.54}
  Windows;                                                     {!!.54}
{$ENDIF}                                                       {!!.54}

type
  {$IFDEF VER70}
  SmallInt = integer;
  Cardinal = word;
  {$ENDIF}

  {$IFDEF Win32}
  TbtfErrorCode = integer;
  TbtfHandle = DWORD;
  TbtfMemSize = DWORD;
  {$ELSE}
  DWORD = longint;                                             {!!.54}
  TbtfErrorCode = word;
  TbtfHandle = word;
  TbtfMemSize = word;
  PAnsiChar = PChar;
  {$ENDIF}

{$IFNDEF Win32}
const
  INVALID_HANDLE_VALUE = $FFFF;
{$ENDIF}

implementation

end.
