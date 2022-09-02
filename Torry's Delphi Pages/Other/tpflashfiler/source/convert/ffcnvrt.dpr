{*********************************************************}
{* FlashFiler: GUI FF1->FF2 conversion utility           *}
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

program FFCnvrt;

uses
  FFMemMgr in 'FFMemMgr.pas',
  {$IFDEF USETeDEBUG}
  TeDebug,
  {$ENDIF}
  Forms,
  uFF2Cnv in 'uFF2Cnv.pas' {frmFF2Conv},
  uFFNet in 'uFFNet.pas' {frmFFransport};

{$R *.RES}

begin
  Application.Initialize;
  Application.HelpFile := 'ffcnvrt.hlp';
  Application.CreateForm(TfrmFF2Conv, frmFF2Conv);
  Application.Run;
end.

