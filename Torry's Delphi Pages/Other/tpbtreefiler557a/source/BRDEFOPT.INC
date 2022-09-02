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

{--Definition of to use B-Tree Shell or B-Tree Isam for Browser}
  (********************************************************************)
  (******************)   {$DEFINE BRUseIsam}    (**********************)
  (******************)   { $DEFINE BRUseShell}   (**********************)
  (********************************************************************)
  {-Either BRUseShell or BRUseIsam may be defined, not both. At least one
    of them must be defined to determine whether B-Tree Filer or B-Tree
    Shell is used for the browser}


(**********************************************************************)
(**********************************************************************)
{Don't change anything beyond this point}

{--Test the BRUseXXX defines}
  {$IFDEF BRUseShell}
    {$IFDEF BRUseIsam}
      ** ERROR **  You can only define one of BRUseShell or BRUseIsam
    {$ENDIF}
  {$ELSE}
    {$IFNDEF BRUseIsam}
      ** ERROR **  One of BRUseShell or BRUseIsam *must* be defined
    {$ENDIF}
  {$ENDIF}

{--Conditional defines that affect this unit}
  {$IFDEF BRUseShell}
    {$I SHDefine.Inc}
  {$ENDIF}
  {$IFDEF BRUseIsam}
    {$I BTDefine.Inc}
  {$ENDIF}
