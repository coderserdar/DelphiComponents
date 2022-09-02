{*********************************************************}
{* User password maintenance for server                  *}
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

Unit fssrvpwd;

{$I FsDEFINE.INC}

Interface

Uses
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  Stdctrls,
  Buttons;

Type
  TfsPwdForm = Class(TForm)
    edtFirstTry: TEdit;
    lblFisrtTry: TLabel;
    lblSecondTry: TLabel;
    edtSecondTry: TEdit;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    Procedure btnOKClick(Sender: TObject);
  Private
    { Private declarations }
  Public
    { Public declarations }
  End;

Var
  fsPwdForm: TfsPwdForm;

Implementation

{$R *.DFM}

Procedure TfsPwdForm.btnOKClick(Sender: TObject);
Begin
  If (edtFirstTry.Text = edtSecondTry.Text) Then
    If (edtFirstTry.Text = '') Then
      Showmessage('Must write password')
    Else
      ModalResult := mrOK
  Else
    Showmessage('Bad retype');
End;


End.

