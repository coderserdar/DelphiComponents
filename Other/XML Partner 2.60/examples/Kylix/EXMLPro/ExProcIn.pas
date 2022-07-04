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
{* XMLPartner: ExProcIn.PAS 2.57                         *}
{*********************************************************}
{* XMLPartner: XML Editor Processing Instruction form    *}
{*********************************************************}
unit ExProcIn;

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
{$ENDIF}
{$IFDEF LINUX}
  QForms,
  QDialogs,
  QStdCtrls,
  QControls,
{$ENDIF}
  SysUtils,
  Classes;

type
  TPIForm = class(TForm)
    Label1: TLabel;
    PINameEdit: TEdit;
    Label2: TLabel;
    PIValueEdit: TEdit;
    OkBtn: TButton;
    CancelBtn: TButton;
    procedure OkBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PIForm: TPIForm;

implementation

{$R *.dfm}

procedure TPIForm.OkBtnClick(Sender: TObject);
begin
  PINameEdit.Text := Trim(PINameEdit.Text);
  if PINameEdit.Text = '' then
  begin
    MessageDlg('Processing instruction name cannot be blank!', mtError, [mbOk], 0);
    ModalResult := mrNone;
  end;
end;

end.
