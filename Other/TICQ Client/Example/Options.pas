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

unit Options;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Passwd, Main;

type
  TOptionsForm = class(TForm)
    Notebook: TNotebook;
    TreeView: TTreeView;
    btnOK: TButton;
    btnCancel: TButton;
    btnApply: TButton;
    gbWindow: TGroupBox;
    gbTrans: TGroupBox;
    cbOnTop: TCheckBox;
    edTitleText: TEdit;
    Label1: TLabel;
    cbHide: TCheckBox;
    cbTransparent: TCheckBox;
    tbTransparent: TTrackBar;
    lblTransp: TLabel;
    Label2: TLabel;
    gbProxy: TGroupBox;
    lblProxyType: TLabel;
    cbProxyType: TComboBox;
    lblProxyHost: TLabel;
    edProxyHost: TEdit;
    lblProxyPort1: TLabel;
    edProxyPort: TEdit;
    lblProxyPort2: TLabel;
    cbProxyAuth: TCheckBox;
    lblProxyUserID: TLabel;
    edProxyUserID: TEdit;
    lblProxyPass: TLabel;
    edProxyPass: TPasswordEdit;
    cbProxyResolve: TCheckBox;
    gbICQ: TGroupBox;
    lblUIN: TLabel;
    lblPasswd: TLabel;
    edUIN: TEdit;
    edPasswd: TPasswordEdit;
    lblCreateUIN: TLabel;
    lblRetPasswd: TLabel;
    gbExpert: TGroupBox;
    lblICQServer: TLabel;
    edICQServer: TEdit;
    lblICQPort: TLabel;
    edICQPort: TEdit;
    btnReset: TButton;
    cbKeepAlive: TCheckBox;
    gbAutoAway: TGroupBox;
    cbOnSaver: TCheckBox;
    cbOnWLock: TCheckBox;
    cbOnMouse: TCheckBox;
    cbSetNA: TCheckBox;
    lblAwayMin: TLabel;
    lblNaMin: TLabel;
    edAwayTime: TEdit;
    edNATime: TEdit;
    gbStatusMsgs: TGroupBox;
    cbAwayMsgs: TComboBox;
    mAwayMsg: TMemo;
    Label5: TLabel;
    Label6: TLabel;
    lblICQChange: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure btnResetClick(Sender: TObject);
    procedure tbTransparentChange(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure cbTransparentClick(Sender: TObject);
    procedure AssignSettings(Settings: _DBSETTINGS);
    procedure cbAwayMsgsChange(Sender: TObject);
    procedure mAwayMsgExit(Sender: TObject);
    procedure cbProxyTypeChange(Sender: TObject);
    procedure cbOnMouseClick(Sender: TObject);
    procedure cbSetNAClick(Sender: TObject);
    procedure lblCreateUINClick(Sender: TObject);
    procedure lblRetPasswdClick(Sender: TObject);
    procedure edUINChange(Sender: TObject);
    procedure edPasswdChange(Sender: TObject);
    procedure edICQServerChange(Sender: TObject);
    procedure edICQPortChange(Sender: TObject);
  private
  protected
    procedure CreateParams(var Params : TCreateParams); override;
  public
    FMsgAway: String;
    FMsgNA: String;
    FMsgDND: String;
    FMsgOccupied: String;
    FMsgFFC: String;
  end;

var
  OptionsForm: TOptionsForm;

implementation

uses
  ICQWorks;

{$R *.dfm}

procedure TOptionsForm.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params); //Don't ever forget to do this!!!
  Params.WindowClass.hIcon := Icon.Handle;  
  Params.WndParent := GetDesktopWindow;
end;

procedure TOptionsForm.AssignSettings(Settings: _DBSETTINGS);
begin
  // Window
  cbOnTop.Checked := Settings.bOnTop;
  cbHide.Checked := Settings.bHide;
  edTitleText.Text := Settings.sTitleText;
  cbTransparent.Checked := Settings.bTransparent;
  tbTransparent.Position := Settings.iBlendValue;

  if cbTransparent.Checked then begin
    tbTransparent.Enabled := True;
    lblTransp.Enabled := True;
  end
  else begin
    tbTransparent.Enabled := False;
    lblTransp.Enabled := False;
  end;

  lblTransp.Caption := Format('%d%%', [tbTransparent.Position]);

  // Network
  case Settings.ProxyType of
    P_SOCKS4: cbProxyType.ItemIndex := 0;
    P_SOCKS5: cbProxyType.ItemIndex := 1;
      P_HTTP: cbProxyType.ItemIndex := 2;
     P_HTTPS: cbProxyType.ItemIndex := 3;
      P_NONE: cbProxyType.ItemIndex := 4;
  else
    cbProxyType.ItemIndex := -1;
  end;

  if cbProxyType.ItemIndex = 4 then begin
    edProxyHost.Enabled := False;
    edProxyPort.Enabled := False;
    cbProxyAuth.Enabled := False;
    edProxyUserID.Enabled := False;
    edProxyPass.Enabled := False;
    cbProxyResolve.Enabled := False;

    lblProxyHost.Enabled := False;
    lblProxyPort1.Enabled  := False;
    lblProxyPort2.Enabled  := False;
    lblProxyUserID.Enabled  := False;
    lblProxyPass.Enabled  := False;
  end
  else begin
    edProxyHost.Enabled := True;
    edProxyPort.Enabled := True;
    cbProxyAuth.Enabled := True;
    edProxyUserID.Enabled := True;
    edProxyPass.Enabled := True;
    cbProxyResolve.Enabled := True;

    lblProxyHost.Enabled  := True;
    lblProxyPort1.Enabled  := True;
    lblProxyPort2.Enabled  := True;
    lblProxyUserID.Enabled  := True;
    lblProxyPass.Enabled  := True;
  end;

  edProxyHost.Text := Settings.ProxyHost;
  edProxyPort.Text := IntToStr(Settings.ProxyPort);
  cbProxyAuth.Checked := Settings.ProxyAuth;
  edProxyUserID.Text := Settings.ProxyUserID;
  edProxyPass.Text := Settings.ProxyPass;
  cbProxyResolve.Checked := Settings.ProxyResolve;

  // ICQ
  edUIN.Text := IntToStr(Settings.UIN);
  edPasswd.Text := Settings.Password;
  edICQServer.Text := Settings.ICQServer;
  edICQPort.Text := IntToStr(Settings.ICQPort);
  cbKeepAlive.Checked := Settings.KeepAlive;

  // Auto Away
  cbOnSaver.Checked := Settings.OnSaver;
  cbOnWLock.Checked := Settings.OnWLock;
  cbOnMouse.Checked := Settings.OnMouse;
  cbSetNA.Checked := Settings.SetNA;
  edAwayTime.Text := IntToStr(Settings.AwayTime);
  edNATime.Text := IntToStr(Settings.NATime);

  if cbOnMouse.Checked then begin
    edAwayTime.Enabled := True;
    lblAwayMin.Enabled := True;
  end
  else begin
    edAwayTime.Enabled := False;
    lblAwayMin.Enabled := False;
  end;

  if cbSetNA.Checked then begin
    edNATime.Enabled := True;
    lblNaMin.Enabled := True;
  end
  else begin
    edNATime.Enabled := False;
    lblNaMin.Enabled := False;
  end;

  // Status Messages
  mAwayMsg.Lines.Text := Settings.MsgAway;
  FMsgAway := MsgAway;
  FMsgNA := MsgNA;
  FMsgDND := MsgDND;
  FMsgOccupied := MsgOccupied;
  FMsgFFC := MsgFFC;
end;

procedure TOptionsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  OptionsForm := nil;  
end;

procedure TOptionsForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TOptionsForm.btnOKClick(Sender: TObject);
begin
  btnApply.Click;
  Close;
end;

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  AssignSettings(MainForm.ReadSettings());

  TreeView.Select(TreeView.Items.Item[3], []);
  TreeView.Items.Item[0].Expand(True);
  TreeView.Items.Item[2].Expand(True);
  TreeView.Items.Item[4].Expand(True);

  MainForm.OnlyNumbers(edProxyPort);
  MainForm.OnlyNumbers(edUIN);
  MainForm.OnlyNumbers(edICQPort);
  MainForm.OnlyNumbers(edAwayTime);
  MainForm.OnlyNumbers(edNATime);

 //System HandPoint Cursor
  if IsWinVer2000Plus then begin
    Screen.Cursors[NIDC_HAND] := LoadCursor(0, IDC_HAND);
    lblCreateUIN.Cursor := NIDC_HAND;
    lblRetPasswd.Cursor := NIDC_HAND;
  end
  else begin
    lblCreateUIN.Cursor := crHandPoint;
    lblRetPasswd.Cursor := crHandPoint;
  end;
  lblICQChange.Visible := False;  
end;

procedure TOptionsForm.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  case Node.AbsoluteIndex of
    1 : Notebook.PageIndex := 0;
    2 : Notebook.PageIndex := 1;
    3 : Notebook.PageIndex := 2;
    5 : Notebook.PageIndex := 3;
    6 : Notebook.PageIndex := 4;
  else
    Notebook.PageIndex := 5;
  end;
end;

procedure TOptionsForm.btnResetClick(Sender: TObject);
begin
  edICQServer.Text := 'login.icq.com';
  edICQPort.Text := '5190';
end;

procedure TOptionsForm.tbTransparentChange(Sender: TObject);
begin
  lblTransp.Caption := Format('%d%%', [tbTransparent.Position]);
end;

procedure TOptionsForm.btnApplyClick(Sender: TObject);
var
  DBSet: _DBSETTINGS;
begin
  // Window
  DBSet.bOnTop := cbOnTop.Checked;
  DBSet.bHide := cbHide.Checked;
  DBSet.sTitleText := edTitleText.Text;
  DBSet.bTransparent := cbTransparent.Checked;
  DBSet.iBlendValue := tbTransparent.Position;

  // Network
  case cbProxyType.ItemIndex of
    0: DBSet.ProxyType := P_SOCKS4;
    1: DBSet.ProxyType := P_SOCKS5;
    2: DBSet.ProxyType := P_HTTP;
    3: DBSet.ProxyType := P_HTTPS;
    4: DBSet.ProxyType := P_NONE;
  else
    DBSet.ProxyType := P_NONE;
  end;

  DBSet.ProxyHost := edProxyHost.Text;
  if MainForm.ValidateRange(edProxyPort.Text, 1) then
    DBSet.ProxyPort := StrToInt64(edProxyPort.Text)
  else
    DBSet.ProxyPort := 1080;
  DBSet.ProxyAuth := cbProxyAuth.Checked;
  DBSet.ProxyUserID := edProxyUserID.Text;
  DBSet.ProxyPass := edProxyPass.Text;
  DBSet.ProxyResolve := cbProxyResolve.Checked;

  // ICQ
  if MainForm.ValidateRange(edUIN.Text, 2) then
    DBSet.UIN := StrToInt64(edUIN.Text)
  else
    DBSet.UIN := 0;
  DBSet.Password := edPasswd.Text;
  DBSet.ICQServer := edICQServer.Text;
  if MainForm.ValidateRange(edICQPort.Text, 1) then
    DBSet.ICQPort := StrToInt64(edICQPort.Text)
  else
    DBSet.ICQPort := 5190;
  DBSet.KeepAlive := cbKeepAlive.Checked;

  // Auto Away
  DBSet.OnSaver := cbOnSaver.Checked;
  DBSet.OnWLock := cbOnWLock.Checked;
  DBSet.OnMouse := cbOnMouse.Checked;
  DBSet.SetNA := cbSetNA.Checked;
  if MainForm.ValidateRange(edAwayTime.Text, 1) then
    DBSet.AwayTime := StrToInt64(edAwayTime.Text)
  else
    DBSet.AwayTime := 5;
  if MainForm.ValidateRange(edNATime.Text, 1) then
    DBSet.NATime := StrToInt64(edNATime.Text)
  else
    DBSet.NATime := 20;
  // Status Messages
  MsgAway := FMsgAway;
  MsgNA := FMsgNA;
  MsgDND := FMsgDND;
  MsgOccupied := FMsgOccupied;
  MsgFFC := FMsgFFC;

  DBSet.MsgAway := MsgAway;
  DBSet.MsgNA := MsgNA;
  DBSet.MsgDND := MsgDND;
  DBSet.MsgOccupied := MsgOccupied;
  DBSet.MsgFFC := MsgFFC;

  // Misc
  DBSet.iLeft := MainForm.Left;
  DBSet.iTop := MainForm.Top;
  DBSet.iHeight := MainForm.Height;
  DBSet.iWidth := MainForm.Width;
  DBSet.dwLastStatus := dwLastStatus;

  if not MainForm.ICQClient1.LoggedIn then
    MainForm.SetCriticalSettings(DBSet);
  MainForm.SetSettings(DBSet);
  MainForm.WriteSettings(DBSet);

  // Apply Status Msg change
  if MainForm.ICQClient1.LoggedIn then
    MainForm.DoStatusChange(MainForm.ICQClient1.Status  or fDCFlag);
end;

procedure TOptionsForm.cbTransparentClick(Sender: TObject);
begin
  if cbTransparent.Checked then begin
    tbTransparent.Enabled := True;
    lblTransp.Enabled := True;
  end
  else begin
    tbTransparent.Enabled := False;
    lblTransp.Enabled := False;
  end;
end;

procedure TOptionsForm.cbAwayMsgsChange(Sender: TObject);
begin
  case cbAwayMsgs.ItemIndex of
    0: mAwayMsg.Lines.Text := MsgAway;
    1: mAwayMsg.Lines.Text := MsgNA;
    2: mAwayMsg.Lines.Text := MsgOccupied;
    3: mAwayMsg.Lines.Text := MsgDND;
    4: mAwayMsg.Lines.Text := MsgFFC;
  end;
end;

procedure TOptionsForm.mAwayMsgExit(Sender: TObject);
begin
  case cbAwayMsgs.ItemIndex of
    0: FMsgAway := mAwayMsg.Lines.Text;
    1: FMsgNA := mAwayMsg.Lines.Text;
    2: FMsgOccupied := mAwayMsg.Lines.Text;
    3: FMsgDND := mAwayMsg.Lines.Text;
    4: FMsgFFC := mAwayMsg.Lines.Text;
  end;
end;

procedure TOptionsForm.cbProxyTypeChange(Sender: TObject);
begin
  if cbProxyType.ItemIndex = 4 then begin
    edProxyHost.Enabled := False;
    edProxyPort.Enabled := False;
    cbProxyAuth.Enabled := False;
    edProxyUserID.Enabled := False;
    edProxyPass.Enabled := False;
    cbProxyResolve.Enabled := False;

    lblProxyHost.Enabled  := False;
    lblProxyPort1.Enabled  := False;
    lblProxyPort2.Enabled  := False;
    lblProxyUserID.Enabled  := False;
    lblProxyPass.Enabled  := False;
  end
  else begin
    edProxyHost.Enabled := True;
    edProxyPort.Enabled := True;
    cbProxyAuth.Enabled := True;
    edProxyUserID.Enabled := True;
    edProxyPass.Enabled := True;
    cbProxyResolve.Enabled := True;

    lblProxyHost.Enabled  := True;
    lblProxyPort1.Enabled  := True;
    lblProxyPort2.Enabled  := True;
    lblProxyUserID.Enabled  := True;
    lblProxyPass.Enabled  := True;
  end;
end;

procedure TOptionsForm.cbOnMouseClick(Sender: TObject);
begin
  if cbOnMouse.Checked then begin
    edAwayTime.Enabled := True;
    lblAwayMin.Enabled := True;
  end
  else begin
    edAwayTime.Enabled := False;
    lblAwayMin.Enabled := False;
  end;
end;

procedure TOptionsForm.cbSetNAClick(Sender: TObject);
begin
  if cbSetNA.Checked then begin
    edNATime.Enabled := True;
    lblNaMin.Enabled := True;
  end
  else begin
    edNATime.Enabled := False;
    lblNaMin.Enabled := False;
  end;
end;

procedure TOptionsForm.lblCreateUINClick(Sender: TObject);
begin
  MainForm.OpenURL('http://lite.icq.com/register', False);
end;

procedure TOptionsForm.lblRetPasswdClick(Sender: TObject);
begin
  MainForm.OpenURL('https://web.icq.com/secure/password', False);
end;

procedure TOptionsForm.edUINChange(Sender: TObject);
begin
  lblICQChange.Visible := True;
end;

procedure TOptionsForm.edPasswdChange(Sender: TObject);
begin
  lblICQChange.Visible := True;
end;

procedure TOptionsForm.edICQServerChange(Sender: TObject);
begin
  lblICQChange.Visible := True;
end;

procedure TOptionsForm.edICQPortChange(Sender: TObject);
begin
  lblICQChange.Visible := True;
end;

end.
