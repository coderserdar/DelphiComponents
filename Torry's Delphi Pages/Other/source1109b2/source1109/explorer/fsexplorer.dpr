{*********************************************************}
{* Project source file                                   *}
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

{$I fsdefine.inc}

Program fsexplorer;

uses
  Forms,
  fmmain in 'fmmain.pas' {frmMain},
  fmstruct in 'fmstruct.pas' {frmTableStruct},
  fmprog in 'fmprog.pas' {frmRebuildStatus},
  dgselidx in 'dgselidx.pas' {dlgSelectIndex},
  dgprintg in 'dgprintg.pas' {dlgPrinting},
  dgaddals in 'dgaddals.pas' {dlgAddAlias},
  dgimport in 'dgimport.pas' {dlgImport},
  dgimpdo in 'dgimpdo.pas' {dlgImportProgress},
  uelement in 'uelement.pas',
  uconsts in 'uconsts.pas',
  ubase in 'ubase.pas',
  uentity in 'uentity.pas',
  uconfig in 'uconfig.pas',
  dgregsrv in 'dgregsrv.pas' {dlgRegisteredServers},
  dgimpdef in 'dgimpdef.pas' {dlgImportDefinition},
  dgquery in 'dgquery.pas' {dlgQuery},
  dgautoin in 'dgautoin.pas' {dlgAutoInc},
  usqlcfg in 'usqlcfg.pas',
  dgSQLOps in 'dgSQLOps.pas' {frmSQLOps},
  dgSetRng in 'dgSetRng.pas' {dlgSetRange},
  dgServSt in 'dgServSt.pas' {dlgServerStats},
  dgtable in 'dgtable.pas' {dlgTable},
  dgtabletrigger in 'dgtabletrigger.pas' {dlgTableTrigger},
  dgtableprocedures in 'dgtableprocedures.pas' {dlgTableProcedures},
  dgtableconstraints in 'dgtableconstraints.pas' {dlgTableConstraints},
  ufscomms in '..\fscomms\ufscomms.pas' {frmFFCommsMain},
  dgmrec in 'dgmrec.pas' {dlgMaxRecords},
  dgpropfieldidx in 'dgpropfieldidx.pas' {frmPropFieldIdx},
  dgmdict in 'dgmdict.pas' {frmDict};

{$R *.RES}

Begin
  Application.Title := 'FSSQL Explorer';
  //Application.HelpFile := 'FSE.HLP';
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmPropFieldIdx, frmPropFieldIdx);
  Application.CreateForm(TfrmDict, frmDict);
  frmMain.Show;
  Application.ProcessMessages;
  frmMain.Initialize;
  Application.Run;
End.
