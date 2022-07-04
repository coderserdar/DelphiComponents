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
 * The Original Code is TurboPower XMLPartner
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

program EXmlPro;

uses
  Forms,
  Registry,
  Windows,
  ExMain in 'ExMain.pas' {MainForm},
  ExChildW in 'ExChildW.pas' {XmlChild},
  ExAttr in 'ExAttr.pas' {AttributeForm},
  ExElemnt in 'ExElemnt.pas' {ElementForm},
  ExText in 'ExText.pas' {TextForm},
  ExCommnt in 'ExCommnt.pas' {CommentForm},
  ExProcIn in 'ExProcIn.pas' {PIForm},
  ExPrefs in 'ExPrefs.pas' {PrefsForm},
  ExURL in 'ExURL.pas' {URLForm},
  ExSelAtt in 'ExSelAtt.pas' {SelAttrsForm},
  XpDom in '..\..\..\XpDOM.pas',
  ExErr in 'ExErr.pas' {frmErrors},
  ExUtil in 'ExUtil.pas',
  xpAboutw in '..\..\..\XpAboutw.pas' {XpAboutBox},
  ExView in 'ExView.pas' {frmPreview};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TElementForm, ElementForm);
  Application.CreateForm(TTextForm, TextForm);
  Application.CreateForm(TCommentForm, CommentForm);
  Application.CreateForm(TSelAttrsForm, SelAttrsForm);
  Application.CreateForm(TfrmErrors, frmErrors);
  Application.CreateForm(TXpAboutBox, XpAboutBox);
  Application.CreateForm(TfrmPreview, frmPreview);
  Application.Run;
end.
