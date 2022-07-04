{*********************************************************}
{* FlashFiler: Input form for block to be viewed         *}
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

unit frmBlock;

interface

uses
  {$IFDEF DCC6OrLater}
  Variants,
  {$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FFLLBase;

type
  TfrmBlockNum = class(TForm)
    edtBlockNum: TEdit;
    pbOK: TButton;
    pbCancel: TButton;
    lblBlockNum: TLabel;
    lblValidRange: TLabel;
    procedure FormShow(Sender: TObject);
    procedure edtBlockNumKeyPress(Sender: TObject; var Key: Char);
    procedure pbOKClick(Sender: TObject);
    procedure pbCancelClick(Sender: TObject);
    procedure edtBlockNumChange(Sender: TObject);
  private
    { Private declarations }
    FBlockNum : TffWord32;
    FMaxBlockNum : TffWord32;
  public
    { Public declarations }
    procedure SetCtrlStates;

    property BlockNum : TffWord32 read FBlockNum write FBlockNum;
    property MaxBlockNum : TffWord32 read FMaxBlockNum write FMaxBlockNum;
  end;

var
  frmBlockNum: TfrmBlockNum;

implementation

{$R *.dfm}

procedure TfrmBlockNum.FormShow(Sender: TObject);
begin
  FBlockNum := ffc_W32NoValue;
  edtBlockNum.SetFocus;
  lblValidRange.Caption := Format('Valid range is 0 to %d', [FMaxBlockNum]);
  SetCtrlStates;
end;

procedure TfrmBlockNum.edtBlockNumKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key <> Char(8)) and ((Key < '0') or (Key > '9')) then begin
    Key := Char(0);
    Beep;
  end;
end;

procedure TfrmBlockNum.pbOKClick(Sender: TObject);
begin
  FBlockNum := StrToInt(edtBlockNum.Text);
  Close;
end;

procedure TfrmBlockNum.pbCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmBlockNum.SetCtrlStates;
var
  BlockNum : TffWord32;
begin
  if edtBlockNum.Text <> '' then begin
    BlockNum := StrToInt(edtBlockNum.Text);
    pbOK.Enabled := (edtBlockNum.Text <> '') and
                    (BlockNum <= FMaxBlockNum);
  end
  else
    pbOK.Enabled := False;
end;

procedure TfrmBlockNum.edtBlockNumChange(Sender: TObject);
begin
  SetCtrlStates;
end;

end.
