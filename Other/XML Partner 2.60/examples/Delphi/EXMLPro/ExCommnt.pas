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
{* XMLPartner: ExCommnt.PAS 2.57                         *}
{*********************************************************}
{* XMLPartner: XML Editor Comment Edit form              *}
{*********************************************************}
unit ExCommnt;

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
  QForms,
{$ENDIF}
  SysUtils,
  Classes;
  
type
  TCommentForm = class(TForm)
    Label1: TLabel;
    CommentEdit: TEdit;
    OkBtn: TButton;
    CancelBtn: TButton;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CommentForm: TCommentForm;

implementation

{$IFDEF WIN32}
{$R *.dfm}
{$ENDIF}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}

procedure TCommentForm.FormShow(Sender: TObject);
begin
  CommentEdit.SetFocus;
end;

end.
