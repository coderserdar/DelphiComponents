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

unit uFFSPwd;

{$I FFDEFINE.INC}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type
  TPwdForm = class(TForm)
    edtFirstTry: TEdit;
    lblFisrtTry: TLabel;
    lblSecondTry: TLabel;
    edtSecondTry: TEdit;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    pnlBottom: TPanel;
    bvlUpper: TBevel;
    procedure btnOKClick(Sender: TObject);
    procedure edtFirstTryKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    PanelVisible : boolean;
    OldPanelColor : TColor;
    OldPanelFontColor : TColor;
  public
    { Public declarations }
    procedure ShowError(const Msg : string);
  end;

var
  PwdForm: TPwdForm;

implementation

{$R *.DFM}

procedure TPwdForm.btnOKClick(Sender: TObject);
begin
  if (edtFirstTry.Text = edtSecondTry.Text) then
    if (edtFirstTry.Text = '') then
      ShowError('The password cannot be blank, please re-enter it')
    else
      ModalResult := mrOK
  else
    ShowError('The two passwords are not the same, please re-enter them');
end;

procedure TPwdForm.edtFirstTryKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if PanelVisible then begin
    PanelVisible := false;
    with pnlBottom do begin
      Caption := '';
      Font.Color := OldPanelFontColor;
      Color := OldPanelColor;
    end;
  end;
end;

procedure TPwdForm.ShowError(const Msg : string);
begin
  if not PanelVisible then begin
    PanelVisible := true;
    with pnlBottom do begin
      OldPanelFontColor := Font.Color;
      OldPanelColor := Color;
      Font.Color := clWhite;
      Color := clGreen;
    end;
  end;
  pnlBottom.Caption := Msg;
  edtFirstTry.Text := '';
  edtSecondTry.Text := '';
  ActiveControl := edtFirstTry;
end;

end.
