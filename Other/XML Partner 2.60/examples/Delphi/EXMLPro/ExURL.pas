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
{* XMLPartner: ExURL.PAS 2.57                            *}
{*********************************************************}
{* XMLPartner: XML Editor URL Edit form                  *}
{*********************************************************}
unit ExURL;

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
  QStdCtrls,
  QControls,
  QDialogs,
  QForms,
{$ENDIF}
  SysUtils,
  Classes;


type
  TURLForm = class(TForm)
    Label1: TLabel;
    UrlEdit: TEdit;
    Label2: TLabel;
    FtpUserIdEdit: TEdit;
    Label3: TLabel;
    FtpPasswordEdit: TEdit;
    OkBtn: TButton;
    CancelBtn: TButton;
    Label4: TLabel;
    procedure OkBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    function GetFTPPassword : string;
    function GetFTPUser : string;
    function GetURL : string;

    procedure SetFTPPassword(const aPassword : string);
    procedure SetFTPUser(const aUser : string);
    procedure SetURL(const aURL : string);
  public
    { Public declarations }
    property FTPPassword : string read GetFTPPassword write SetFTPPassword;
    property FTPUser : string read GetFTPUser write SetFTPUser;
    property URL : string read GetURL write SetURL;
  end;

var
  URLForm: TURLForm;

implementation

{$R *.dfm}

procedure TURLForm.OkBtnClick(Sender: TObject);
begin
  if UrlEdit.Text = '' then
  begin
    MessageDlg('The URL cannot be blank.', mtError, [mbOk], 0);
    ModalResult := mrNone;
  end;
end;
{--------}
function TURLForm.GetFTPPassword : string;
begin
  Result := FTPPasswordEdit.Text;
end;
{--------}
function TURLForm.GetFTPUser : string;
begin
  Result := FTPUserIDEdit.Text;
end;
{--------}
function TURLForm.GetURL : string;
begin
  Result := URLEdit.Text;
end;
{--------}
procedure TURLForm.SetFTPPassword(const aPassword : string);
begin
  FTPPasswordEdit.Text := aPassword;
end;
{--------}
procedure TURLForm.SetFTPUser(const aUser : string);
begin
  FTPUserIDEdit.Text := aUser;
end;
{--------}
procedure TURLForm.SetURL(const aURL : string);
begin
  URLEdit.Text := aURL;
end;

procedure TURLForm.FormShow(Sender: TObject);
begin
  URLEdit.SetFocus;
end;

end.
