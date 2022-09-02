{*********************************************************}
{* Dialog to rename a database/table                     *}
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

unit dgautoin;

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
  Buttons,
  ExtCtrls,
  Mask,
  ffllbase;

type
  TdlgAutoInc = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    edtSeed: TEdit;
    lblSeed: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
  protected
    FTableName : string;
    FNewSeed : TffWord32;                                           {!!.10}
  public
    property NewSeed : TffWord32 read FNewSeed write FNewSeed;      {!!.10}

    property TableName : string read FTableName write FTableName;
    
  end;

function ShowAutoIncDlg(const aTableName : string;
                          var aNewSeed: TffWord32): TModalResult;   {!!.10}


var
  dlgAutoInc: TdlgAutoInc;

implementation

{$R *.DFM}

function ShowAutoIncDlg(const aTableName : string;
                          var aNewSeed: TffWord32): TModalResult;   {!!.10}
begin
  with TdlgAutoInc.Create(nil) do
  try
    FTableName := aTableName;
    NewSeed := aNewSeed;
    Result := ShowModal;
    if Result = mrOK then
      aNewSeed := NewSeed;
  finally
    Free;
  end;
end;

procedure TdlgAutoInc.FormShow(Sender: TObject);
begin
  Caption := Format(Caption, [FTableName]);
  edtSeed.Text := intToStr(FNewSeed);
end;

procedure TdlgAutoInc.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  Value : TffWord32;                                                {!!.10}
  Code : Integer;
begin
  Val(edtSeed.Text, Value, Code);
  NewSeed := Value;
  CanClose := (Code = 0) or (ModalResult <> mrOK);
  if not CanClose then begin
    MessageBeep(0);
    MessageDlg('A valid seed must be entered.', mtWarning, [mbOK], 0);
  end;
end;

end.
