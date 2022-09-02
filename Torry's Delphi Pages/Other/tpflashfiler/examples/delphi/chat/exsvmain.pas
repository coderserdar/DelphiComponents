{*********************************************************}
{* FlashFiler: Example Chat Server program               *}
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
 
unit ExSvMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, FFLLBase, ffllcomp, ffllcomm, ffLLLgcy, ExChtMsg,
  fflllog;

type
  TfrmServerMain = class(TForm)
    tpMain: TffLegacyTransport;
    pnlMain: TPanel;
    lblServerLog: TLabel;
    memChat: TMemo;
    lblServerName: TLabel;
    efSrvName: TEdit;
    pbSrvCtrl: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure pbSrvCtrlClick(Sender: TObject);
  private
    { Private declarations }
    FChatHandler : TffChatSrvHandler;
  public
    { Public declarations }
  end;

var
  frmServerMain: TfrmServerMain;

implementation

{$R *.DFM}

{====================================================================}
procedure TfrmServerMain.FormDestroy(Sender: TObject);
begin
  if assigned(FChatHandler) then
    FChatHandler.Free;
end;
{--------}
procedure TfrmServerMain.pbSrvCtrlClick(Sender: TObject);
begin
  if tpMain.State = ffesInactive then begin
    efSrvName.Enabled := False;
    pbSrvCtrl.Caption := '&Stop';

    { Create a command handler. }
    FChatHandler := TffChatSrvHandler.Create(Self);
    FChatHandler.Memo := memChat;

    { Connect the command handler to the transport. }
    tpMain.CommandHandler := FChatHandler;
    tpMain.OnAddClient := FChatHandler.OnAddClient;
    tpMain.OnRemoveClient := FChatHandler.OnRemoveClient;

    { Start the transport. }
    tpMain.ServerName := efSrvName.Text;
    tpMain.State := ffesStarted;
  end
  else begin
    tpMain.State := ffesInactive;
    efSrvName.Enabled := True;
    pbSrvCtrl.Caption := '&Start';
  end;
end;
{====================================================================}

end.
