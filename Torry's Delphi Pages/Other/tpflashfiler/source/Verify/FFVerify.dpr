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

program FFVerify;

uses
  Forms,
  frMain in 'frMain.pas' {frmMain},
  ffrepair in 'ffrepair.pas',
  ffv2file in 'ffv2file.pas',
  ffFileInt in 'ffFileInt.pas',
  ffrepcnst in 'ffrepcnst.pas',
  frmBlock in 'frmBlock.pas' {frmBlockNum},
  FFChain in 'FFChain.pas',
  frmOptions in 'frmOptions.pas' {frmOptionsConfig};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'FlashFiler Table Repair';
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmOptionsConfig, frmOptionsConfig);
  Application.Run;
end.
