{*********************************************************}
{* FlashFiler Server project file                        *}
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

program FFServer;

{$I FFDEFINE.INC}

uses
  {$IFDEF USETeDEBUG}
  TeDebug,
  {$ENDIF}
  Forms,
  Uffsmain in 'Uffsmain.pas' {frmFFServer},
  Uffsalas in 'Uffsalas.pas' {FFAliasForm},
  uFFSBrws in 'UFFSBRWS.PAS' {DirBrowseForm},
  Uffsuser in 'Uffsuser.pas' {FFUserForm},
  Uffspwd in 'Uffspwd.pas' {PwdForm},
  Uffsgenl in 'Uffsgenl.pas' {FFGenConfigForm},
  UFFSINDX in 'UFFSINDX.PAS' {FFIndexForm},
  uFFEgMgr in 'uFFEgMgr.pas' {FFEngineMgr: TffBaseEngineManager},
  Uffsnet in 'Uffsnet.pas' {FFNetConfigForm},
  uFFSRJrn in '..\uffsrjrn.pas' {JournalForm};

{$R FFSERVER.RES}

begin
  Application.HelpFile := 'ffserver.hlp';
  Application.CreateForm(TfrmFFServer, frmFFServer);
  Application.Run;
end.
