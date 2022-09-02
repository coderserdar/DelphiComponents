{*********************************************************}
{* FlashFiler: Example Chat Client program               *}
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
 
unit ExClMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, FFLLBase, ffllcomp, ffllcomm, ffLLLgcy, ExChtMsg,
  ffllReq, fflllog;

type
  TfrmCltMain = class(TForm)
    bvMain: TBevel;
    lblUserName: TLabel;
    lblConnect: TLabel;
    pbSend: TButton;
    efMessage: TEdit;
    pbConnect: TButton;
    pbDisconnect: TButton;
    efUserName: TEdit;
    lbUsers: TListBox;
    chkPrivate: TCheckBox;
    lbOutput: TListBox;
    pbExit: TButton;
    tpClient: TffLegacyTransport;
    cmbServers: TComboBox;
    pbRefreshServers: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure pbSendClick(Sender: TObject);
    procedure efMessageKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbOutputDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure chkPrivateClick(Sender: TObject);
    procedure pbExitClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pbConnectClick(Sender: TObject);
    procedure pbDisconnectClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pbRefreshServersClick(Sender: TObject);
  private
    { Private declarations }
    FChatHandler : TffChatClntHandler;
    FClientID : TffClientID;

    procedure RefreshServers;
    procedure SetCtrlStates;
  public
    { Public declarations }
  end;

var
  frmCltMain: TfrmCltMain;

implementation

{$R *.DFM}

{====================================================================}
procedure TfrmCltMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  { Make sure we are inactive. }
  tpClient.State := ffesInactive;
end;
{--------}
procedure TfrmCltMain.pbSendClick(Sender: TObject);
var
  Request : TffnmChatText;
begin
  if efMessage.Text <> '' then begin

    Request.IsPrivate := chkPrivate.Checked;
    Request.UserName := '';

    { Private? }
    if chkPrivate.Checked then begin
      { Yes.  Verify a recipient is checked. }
      if lbUsers.ItemIndex < 0 then begin
        showMessage('No one is selected.');
        exit;
      end
      else
        Request.UserName := lbUsers.Items.Strings[lbUsers.ItemIndex];
    end;

    Request.Text := efMessage.Text;
    efMessage.Text := '';

    tpClient.Post(0, FClientID, ffnmChatText, @Request, sizeOf(Request),
                  1000, ffrmNoReplyExpected);

  end;
end;
{--------}
procedure TfrmCltMain.efMessageKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  { This is here to stop that really annoying bell. }
  if Key = 13 then Key := 0;
end;
{--------}
procedure TfrmCltMain.lbOutputDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  TempStr : string;
begin
  { color different messages }
  if lbOutput.Items.Count <> 0 then begin
    TempStr := lbOutput.Items.Strings[Index];
    if Pos(efUserName.Text + ' :', TempStr) <> 0 then begin
      lbOutput.Canvas.Font.Color := clGreen;
    end else if Pos(ffc_Private, TempStr) <> 0 then begin
      lbOutput.Canvas.Font.Color := clRed;
    end;
    lbOutput.Canvas.TextRect(Rect, Rect.Left, Rect.Top, TempStr);
  end;
end;
{--------}
procedure TfrmCltMain.RefreshServers;
var
  Index : longInt;
  ServerList : TStringList;
begin
  { Obtain a list of available chat servers. }
  ServerList := TStringList.Create;
  try
    tpClient.GetServerNames(ServerList, 1000);

    { Populate the combobox. }
    cmbServers.Items.Clear;
    cmbServers.Text := '';
    for Index := 0 to pred(ServerList.Count) do begin
      cmbServers.Items.Add(ServerList.Strings[Index]);
    end;

    if ServerList.Count > 0 then
      cmbServers.ItemIndex := 0
    else
      cmbServers.Text := '<No servers found>';

  finally
    ServerList.Free;
  end;
end;
{--------}
procedure TfrmCltMain.chkPrivateClick(Sender: TObject);
begin
  { make a visual for private message }
  if chkPrivate.Checked then
    efMessage.Color := clAqua
  else
    efMessage.Color := clWindow;
end;
{--------}
procedure TfrmCltMain.pbExitClick(Sender: TObject);
begin
  tpClient.State := ffesInactive;
  Close;
end;
{--------}
procedure TfrmCltMain.FormShow(Sender: TObject);
var
  aSize : DWORD;
  Buffer : array[0..255] of Char;
begin

  { Get the user's login and use that as their default chat name. }
  if GetUserName(@Buffer, aSize) then
    efUserName.Text := Buffer
  else
    efUserName.Text := 'me';

  { Create a command handler & connect it to the transport. }
  FChatHandler := TffChatClntHandler.Create(nil);
  FChatHandler.Output := lbOutput;
  FChatHandler.UserList := lbUsers;
  tpClient.CommandHandler := FChatHandler;

  RefreshServers;

  SetCtrlStates;

end;
{--------}
procedure TfrmCltMain.pbConnectClick(Sender: TObject);
begin
  { Requirement: Must have specified a user name. }
  if efUserName.Text = '' then begin
    ShowMessage('You must specify a user name.');
    exit;
  end;

  { Requirement: Must have selected a server. }
  if cmbServers.ItemIndex < 0 then begin
    ShowMessage('You must select a chat server.');
    exit;
  end;

  tpClient.ServerName := cmbServers.Items[cmbServers.ItemIndex];
  tpClient.State := ffesStarted;
  tpClient.EstablishConnection(efUserName.Text, 0, 1000, FClientID);
  SetCtrlStates;
  if tpClient.IsConnected then begin
    lbOutput.Items.Add('Connected to ' + tpClient.ServerName);
    lbOutput.ItemIndex := pred(lbOutput.Items.Count);
    lbOutput.ItemIndex := -1;
    efMessage.SetFocus;
  end;
end;
{--------}
procedure TfrmCltMain.SetCtrlStates;
var
  IsConnected : boolean;
begin
  IsConnected := tpClient.IsConnected;
  pbConnect.Enabled := (not IsConnected);
  pbConnect.Default := pbConnect.Enabled;
  pbSend.Enabled := IsConnected;
  pbSend.Default := pbSend.Enabled;
  efMessage.Enabled := IsConnected;
  pbDisconnect.Enabled := IsConnected;
  efUserName.Enabled := (not IsConnected);

  if IsConnected then
    lblConnect.Caption := 'Connected to ' + tpClient.ServerName
  else
    lblConnect.Caption := 'Not connected';

end;
{--------}
procedure TfrmCltMain.pbDisconnectClick(Sender: TObject);
begin
  tpClient.State := ffesInactive;
  lbOutput.Items.Add('Disconnected from ' + tpClient.ServerName);
  lbOutput.ItemIndex := pred(lbOutput.Items.Count);
  lbOutput.ItemIndex := -1;
  SetCtrlStates;
end;
{--------}
procedure TfrmCltMain.FormDestroy(Sender: TObject);
begin
  FChatHandler.Free;
end;
{--------}
procedure TfrmCltMain.pbRefreshServersClick(Sender: TObject);
begin
  RefreshServers;
end;
{====================================================================}

end.
