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
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

program BldFiler;                                               {!!.51}
  {-Simple program to build all B-Tree Filer units via Make}

{$I BTDEFINE.INC}

uses
{$IFDEF Win32}
  Filer,
  Vrec,
  BufRecIO,
  NumKey32,
  Rebuild,
  Reorg,
  ReIndex,
  VRebuild,
  VReorg,
  FixToVar,
  Restruct;
{$ELSE}
{$IFDEF UseFilerDLL}
  RexCHook,
{$ELSE}
  {$IFDEF MSDOS}
  EMSHeap,
  EMSSupp,
  {$ENDIF}
{$ENDIF}
  BaseSupp,
{$IFDEF VER80}
  DosSupp,
{$ENDIF}
  Filer,
  Vrec,
  BufRecIO,
  NumKeys,
  Rebuild,
  Reorg,
  ReIndex,
  VRebuild,
  VReorg,
  FixToVar,
  Restruct;
{$ENDIF}

begin
end.
