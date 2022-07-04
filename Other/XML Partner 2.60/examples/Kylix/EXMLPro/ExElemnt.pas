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

{*********************************************************}
{* XMLPartner: ExElemnt.PAS 2.57                         *}
{*********************************************************}
{* XMLPartner: XML Editor Element Edit form              *}
{*********************************************************}
unit ExElemnt;

interface

uses
{$IFDEF WIN32}
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Grids,
{$ENDIF}
{$IFDEF LINUX}
  QStdCtrls,
  QControls,
  QForms,
  QDialogs,
{$ENDIF}
  SysUtils,
  Classes;

type
  TElementForm = class(TForm)
    Label1: TLabel;
    NameEdit: TEdit;
    OkBtn: TButton;
    CancelBtn: TButton;
    procedure OkBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ElementForm: TElementForm;

implementation

{$R *.dfm}

procedure TElementForm.OkBtnClick(Sender: TObject);
begin
  NameEdit.Text := Trim(NameEdit.Text);
  if NameEdit.Text = '' then begin
    MessageDlg('Element name cannot be blank!', mtError, [mbOk], 0);
    ModalResult := mrNone;
  end;
end;

procedure TElementForm.FormShow(Sender: TObject);
begin
  NameEdit.SetFocus;
  NameEdit.SelectAll;
end;

end.
