{*********************************************************}
{* Compiler options/directives include file              *}
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

{NOTE: FFCRDEFN.INC is included in all Crystal Reports driver units;
       hence you can specify global compiler options here.
       FFCRDEFN.INC is included *before* each unit's own required
       compiler options, so options specified here could be
       overridden by hardcoded options in the unit source file.}

{$I ffdefine.inc}

{.$DEFINE Debug}

{====Global fixed compiler options (do NOT change)====}
{$A- Force alignment on byte boundaries}
{$Z2 Enumerations in Crystal Reports are word sized}
