{*********************************************************}
{* FlashFiler: Interface to the FlashFiler 1 DLL that is *}
{* used in the conversion utility to converte FlashFiler *}
{* 1.5x tables to 2.x                                    *}
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

library FF1Intfc;

uses
  SysUtils,
  Classes,
  uFF1Data;

{$R *.RES}

exports
  FF1DirOpen,
  FF1IsFileBLOB,
  FF1TableOpen,
  FF1TableClose,
  FF1TableDataDictionary,
  FF1TableFirst,
  FF1TableNext,
  FF1TableFieldValue,
  FF1TableEOF,
  FF1TableRecordCount,
  FF1GetMem,
  FF1FreeMem,
  FF1ReallocMem,
  FF1GetAutoInc;

begin

end.
