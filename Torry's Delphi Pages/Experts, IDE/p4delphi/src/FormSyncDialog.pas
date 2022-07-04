{**************************************************************************************************}
{                                                                                                  }
{ Perforce for Delphi plugin (P4Delphi)                                                            }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Chris Fairall. Portions created by                 }
{ Chris Fairall are Copyright (C) Chris Fairall (cfairall at bigpond dot net dot au)               }
{ All rights reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit FormSyncDialog;

{----------------------------------------------------------------------------

   Unit Name     :  FormSyncDialog
   Date Created  :  21 May 2002
   Author        :  Chris Fairall
   Description   :  Dialogue used to Sync to a given revision.

   $File: //depot/Delphi/Toolkit/Experts/Perforce/FormSyncDialog.pas $
   $Revision: #1 $
   $DateTime: 2003/09/18 17:59:41 $
   $Author: fairallc $

 ----------------------------------------------------------------------------}

interface

{$I P4Define.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmSyncDialog = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    rbtnHead: TRadioButton;
    rbtnRevNbr: TRadioButton;
    rbtnOther: TRadioButton;
    edtRevNbr: TEdit;
    edtOther: TEdit;
    Label1: TLabel;
    chbxForce: TCheckBox;
    procedure edtRevNbrKeyPress(Sender: TObject; var Key: Char);
    procedure edtOtherKeyPress(Sender: TObject; var Key: Char);
    procedure RadioButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function Execute(out psAddOn : String; out pbForce : Boolean) : Boolean;
  end;

var
  frmSyncDialog: TfrmSyncDialog;

implementation

{$R *.dfm}

{ TfrmSyncDialog }

function TfrmSyncDialog.Execute(out psAddOn: String;
  out pbForce: Boolean): Boolean;
begin
  Result := (ShowModal = mrOK);
  if Result then
    begin
      psAddOn := '';
      if rbtnRevNbr.Checked and (edtRevNbr.Text <> '') then
        psAddOn := '#' + edtRevNbr.Text;
      if rbtnOther.Checked and (edtOther.Text <> '') then
        psAddOn := '@' + edtOther.Text;

      pbForce := chbxForce.Checked;
    end;
end;

procedure TfrmSyncDialog.edtRevNbrKeyPress(Sender: TObject; var Key: Char);
begin
  {$IFDEF UNICODE}
  if not (CharInSet(Key , [#8, '0'..'9'])) then
  {$ELSE}
  if not (Key in [#8, '0'..'9']) then
  {$ENDIF}
    Key := #0;
end;

procedure TfrmSyncDialog.edtOtherKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ' ' then
    Key := #0;
end;

procedure TfrmSyncDialog.RadioButtonClick(Sender: TObject);
begin
  edtRevNbr.Enabled := rbtnRevNbr.Checked;
  with edtRevNbr do
    if Enabled then
      Color := clWindow
    else
      Color := clBtnFace;

  edtOther.Enabled := rbtnOther.Checked;
  with edtOther do
    if Enabled then
      Color := clWindow
    else
      Color := clBtnFace;

  if (Sender = rbtnRevNbr) and edtRevNbr.Enabled then
    edtRevNbr.SetFocus;

  if (Sender = rbtnOther) and edtOther.Enabled then
    edtOther.SetFocus;
end;

end.
