{*********************************************************}
{* FlashFiler: Client Login Dialog                       *}
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

unit fflogdlg;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Buttons,
  ffllbase;

type
  TFFLoginDialog = class(TForm)
    lblUserName: TLabel;
    edtUserName: TEdit;
    edtPassword: TEdit;
    lblPassword: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetPassowrd: string;
    procedure SetPassword(const Value: string);
    function GetUserName: string;
    procedure SetUserName(const Value: string);
  public
    property UserName : string
       read GetUserName
       write SetUserName;
    property Password : string
       read GetPassowrd
       write SetPassword;
  end;

var
  FFLoginDialog: TFFLoginDialog;

implementation

{$R *.DFM} 

procedure TFFLoginDialog.btnOKClick(Sender: TObject);
begin
  if Length(edtUserName.Text) = 0 then begin
    edtUserName.SetFocus;
    MessageBeep(0);
    Exit;
  end;
  if Length(edtPassword.Text ) = 0 then begin
    edtPassword.SetFocus;
    MessageBeep(0);
    Exit;
  end;
  ModalResult := mrOK;
end;
{--------}
function TFFLoginDialog.GetPassowrd: string;
begin
  Result := edtPassword.Text;
end;
{--------}
function TFFLoginDialog.GetUserName: string;
begin
  Result := edtUserName.Text;
end;
{--------}
procedure TFFLoginDialog.SetPassword(const Value: string);
begin
  edtPassword.Text := Value;
end;
{--------}
procedure TFFLoginDialog.SetUserName(const Value: string);
begin
  edtUserName.Text := Value;
end;
{--------}
procedure TFFLoginDialog.FormCreate(Sender: TObject);
begin
  edtUserName.MaxLength := ffcl_UserNameSize;
  edtPassword.MaxLength := ffcl_GeneralNameSize;
end;

procedure TFFLoginDialog.FormShow(Sender: TObject);
begin
  if edtUserName.Text <> '' then
    edtPassword.SetFocus;
end;

end.
