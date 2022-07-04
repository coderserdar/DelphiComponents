{*********************************************************}
{* FlashFiler: Register InfoPower-compatible FF table    *}
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

{$I ffdefine.inc}

{Notes: Based on information provided by Woll2Woll Software.
        Requires InfoPower 3.01 or later
        InfoPower is Copyright (c) 1995-1997 by Woll2Woll Software}

unit ffwwreg;

interface

procedure Register;

implementation

uses
  {$IFDEF DCC6OrLater}
  DesignIntf, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  Classes,
  Controls,
  DB,
  ffwwtabl,
  ffclver;

procedure Register;
begin
  RegisterComponents('FlashFiler Client', [TffwwTable, TffwwQuery]);
  RegisterPropertyEditor(TypeInfo(AnsiString), TffwwTable, 'Version', TffVersionProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TffwwQuery, 'Version', TffVersionProperty);
end;

end.
