{*********************************************************}
{* FlashFiler: Error Plugin Demo Main Form               *}
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

unit ExPlMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ffllcomm, ffLLLgcy, FFLLBase, ffllcomp, ExErrPlg, StdCtrls;

type
  TfrmErrPlugMain = class(TForm)
    efErrMsg: TEdit;
    pbSend: TButton;
    tpMain: TffLegacyTransport;
    efErrCode: TEdit;
    lblErrMsg: TLabel;
    lblErrCode: TLabel;
    pbClose: TButton;
    lblUserName: TLabel;
    efUserName: TEdit;
    errPlugin: TffRemoteErrorPlugin;
    procedure pbSendClick(Sender: TObject);
    procedure pbCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmErrPlugMain: TfrmErrPlugMain;

implementation

{$R *.DFM}

procedure TfrmErrPlugMain.pbSendClick(Sender: TObject);
begin
  try
    errPlugin.WriteError(0, StrToInt(efErrCode.Text), efUserName.Text,
                         efErrMsg.Text);
  finally
    efErrMsg.SetFocus;
  end;
end;

procedure TfrmErrPlugMain.pbCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmErrPlugMain.FormCreate(Sender: TObject);
begin
  tpMain.State := ffesStarted;
  errPlugin.State := ffesStarted;
end;

procedure TfrmErrPlugMain.FormDestroy(Sender: TObject);
begin
  errPlugin.State := ffesInactive;
  tpMain.State := ffesInactive;
end;

end.
