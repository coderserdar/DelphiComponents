(*

eICQ: the free ICQ for Microsoft(tm) Windows(tm)

Copyright 2003-2004 eICQ ICQ project,
all portions of this codebase are copyrighted to the people
listed in contributors.txt.

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*)

unit AddUser;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Main;

type
  TAddUserForm = class(TForm)
    Label1: TLabel;
    mNick: TMemo;
    gbOptions: TGroupBox;
    cbYouWereAdded: TCheckBox;
    cbAuhtorize: TCheckBox;
    gbAuthReq: TGroupBox;
    mAuthReq: TMemo;
    btnAdd: TButton;
    btnCancel: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbAuhtorizeClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure mNickKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure mNickKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    dlgHandle: HWND;
    FUIN: DWORD;
    FNick: String;
    FDBData: _DBCONTACTSETTINGS;
  end;

var
  AddUserForm: TAddUserForm;

implementation

uses
  ICQWorks;
  
{$R *.dfm}

procedure TAddUserForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  EnableWindow(dlgHandle, True);          //API ruleZZZ!!!
  Action := caFree;
end;

procedure TAddUserForm.cbAuhtorizeClick(Sender: TObject);
begin
  if cbAuhtorize.Checked then mAuthReq.Enabled := True
    else mAuthReq.Enabled := False;
end;

procedure TAddUserForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TAddUserForm.btnAddClick(Sender: TObject);
var
  ListItem: TListItem;
begin
  if cbYouWereAdded.Checked then
    MainForm.ICQClient1.SendYouWereAdded(FUIN);
  if cbAuhtorize.Checked then
    MainForm.ICQClient1.SendAuthRequest(FUIN, mAuthReq.Text);

  if MainForm.ICQClient1.AddContact(FUIN) then
  begin
    //Add if user in the contact list ListView1 in the MainForm
    ListItem := MainForm.ListView1.Items.Add;
    ListItem.ImageIndex := ICON_OFFLINE;
    ListItem.Caption := IntToStr(FUIN);
    //STATUS
    ListItem.SubItems.Add(IntToStr(S_OFFLINE));
    //Internal IP
    ListItem.SubItems.Add('');
    //External IP
    ListItem.SubItems.Add('');
    //Port
    ListItem.SubItems.Add('0');
    //ProtoVer
    ListItem.SubItems.Add('0');
    //UserCaps
    ListItem.SubItems.Add('0');
    //Online since
    ListItem.SubItems.Add('0');
    //ICQ Client
    ListItem.SubItems.Add('0');
    //Miranda version
    ListItem.SubItems.Add('0');
    //Nick
    if mNick.Text = '' then FNick := IntToStr(FUIN)
      else FNick := mNick.Text;
    ListItem.SubItems.Add(FNick);
    //Idle
    ListItem.SubItems.Add('0');

    FDBData.dwUIN := FUIN;
    FDBData.dwStatus := S_OFFLINE;
    FDBData.sInternalIP := '';
    FDBData.sExternalIP := '';
    FDBData.wPort := 0;
    FDBData.byProtoVer := 0;
    FDBData.byUserCaps := 0;
    FDBData.dtOnlineTime := 0;
    FDBData.dwClient := 0;
    FDBData.dwMirandaVer := 0;
    FDBData.sNick := FNick;

    MainForm.WriteToDB(FUIN, FDBData);
  end;
  Close;
end;

procedure TAddUserForm.mNickKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = [ssCtrl]) then
    mNick.ReadOnly := True;
end;

procedure TAddUserForm.mNickKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  mNick.ReadOnly := False;
end;

end.
