unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Grids, Spin,
  NMMCustomClient, Menus, NMMVoiceClient, NMMVoiceInClient;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    StatusBar: TStatusBar;
    Client: TNMMVoiceClient;
    MainMenu1: TMainMenu;
    mnuSettings: TMenuItem;
    mnuStatistics: TMenuItem;
    cbActivate: TCheckBox;
    mnuAbout: TMenuItem;
    mnuExit: TMenuItem;
    procedure cbActivateClick(Sender: TObject);
    procedure ImageClientConnectionTeminated(Sender: TObject;
      ErrorMsg: String);
    procedure ImageClientStatusChanged(Sender: TObject; Connected: Boolean;
      ExtendedStatusInfo: String);
    procedure ImageClientConnectionRefused(Sender: TObject;
      RefuseReason: TRefuseReason; ExtendedInfo: String);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ClientPeriodChangedEvent(Sender: TObject;
      NewPeriod: Integer);
    procedure mnuSettingsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuStatisticsClick(Sender: TObject);
    procedure ClientTextMessage(Sender: TObject; Msg: String);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
  private
    { Private declarations }
    FirstData: Boolean;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation
uses IdSocks, NMMCommon, NMMAudioCommon, Settings, Stat, NoCodecDlg,
  Messags, About;

{$R *.dfm}

procedure TfrmMain.cbActivateClick(Sender: TObject);
begin
 with Client,frmSettings do
 begin
   if Client.Active = (Sender As TCheckBox).Checked then
     exit;

   if (Sender As TCheckBox).Checked then
   begin
     InOutDevice:= frmSettings.SelectAudioInput.GetDeviceID;

     ServerHost:= edHost.Text;
     ServerPort:= sedPort.Value;
     SocksProxyHost:= edProxyHost.Text;
     SocksProxyPort:= sedProxyPort.Value;

     with cbSocksProxyVersion do
       SocksProxyVersion:= TSocksVersion(Items.Objects[ItemIndex]);

     SocksProxyUserName:= edProxyUserName.Text;
     SocksProxyPassword:= edProxyPassword.Text;

     if Trim(SocksProxyUserName)<>'' then
       SocksProxyAuthentication:= saUsernamePassword
     else
       SocksProxyAuthentication:= saNoAuthentication;

     User:= edUser.Text;
     Password:= edPassword.Text;
     try
       Connect
     except
       cbActivate.Checked:= false;
       raise;
     end;
   end
   else
   begin
     Disconnect;
   end;
 end;
end;

procedure TfrmMain.ImageClientConnectionTeminated(Sender: TObject;
  ErrorMsg: String);
begin
 if cbActivate.Checked<>false then
   cbActivate.Checked:= false;
end;

procedure TfrmMain.ImageClientStatusChanged(Sender: TObject;
  Connected: Boolean; ExtendedStatusInfo: String);
begin
 if cbActivate.Checked<>Client.Active then
   cbActivate.Checked:= Client.Active;

 if Connected then
   StatusBar.Panels[0].Text:= 'Connected'
 else
   StatusBar.Panels[0].Text:= 'Disconnected';
 StatusBar.Panels[1].Text:= ExtendedStatusInfo;

 with frmStat do
 begin
   lbDeltasReceived.Caption:= 'Frames Received= ' +
     Format( '%d', [Client.Statistics.DeltasReceivedCount] );
   lbDeltaBytesReceived.Caption:= 'Frame Bytes Received= ' +
     Format( '%d', [Client.Statistics.DeltaBytesReceived] );
   lbTotalBytesReceived.Caption:= 'Total Bytes Received= ' +
     Format( '%d', [Client.Statistics.IniBytesReceived +
                    Client.Statistics.DeltaBytesReceived +
                    Client.Statistics.TextBytesReceived] );
   lbCodec.Caption:= 'Audio codec: ' + StrPas(Client.AudioDataParams.szFormatTag);
   lbCodecParams.Caption:= 'Codec format: ' + StrPas(Client.AudioDataParams.szFormat);
 end;

end;

procedure TfrmMain.ImageClientConnectionRefused(Sender: TObject;
  RefuseReason: TRefuseReason; ExtendedInfo: String);
begin
 with frmSettings do
 begin
   case RefuseReason of
     rrInvalidUserName:
       ShowMessage('Connection refused:'+char(VK_RETURN)+'Invalid user name');
     rrInvalidPassword:
       ShowMessage('Connection refused:'+char(VK_RETURN)+'Invalid password');
     rrUserLoginExpired:
       ShowMessage('Connection refused:'+char(VK_RETURN)+'User login expired');
     rrUserChangedLocationTo:
       ShowMessage('Connection refused:'+char(VK_RETURN)+
                   'Another user "'+edUser.Text+
                   '" just logged in from IP address: '+
                   ExtendedInfo);
   end;
 end;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
{ Client.Disconnect;
 Sleep(10);}
 CanClose:= true;
end;

procedure TfrmMain.ClientPeriodChangedEvent(Sender: TObject;
  NewPeriod: Integer);
begin
  raise Exception.Create('Connection is too slow');
end;

procedure TfrmMain.mnuSettingsClick(Sender: TObject);
begin
  frmSettings.ShowModal;
end;

procedure TfrmMain.FormShow(Sender: TObject);
var LSL: TStringList;
    i: Integer;
    LSpeexFound: Boolean;
begin
 with frmSettings do
 begin
   pcParams.TabIndex:= 0;
   FirstData:= True;

   cbSocksProxyVersion.Items.AddObject('svNoSocks',Pointer(svNoSocks));
   cbSocksProxyVersion.Items.AddObject('svSocks4',Pointer(svSocks4));
   cbSocksProxyVersion.Items.AddObject('svSocks4A',Pointer(svSocks4A));
   cbSocksProxyVersion.Items.AddObject('svSocks5',Pointer(svSocks5));
   cbSocksProxyVersion.ItemIndex:= 0;

   sedPort.Value:= Client.ServerPort;
   IniFileName:= 'VoiceClient.ini';
   Load;
 end;

 LSL:= TStringList.Create;
 try
   GetCodecs(LSL);
   LSpeexFound:= false;
   for i:=0 to LSL.Count-1 do
   begin
     if Pos('Speex',LSL[i])<>0 then
        LSpeexFound:= true;
   end;
   if not LSpeexFound then
      frmNoCodec.ShowModal;
 finally
   LSL.Free;
 end;
end;

procedure TfrmMain.mnuStatisticsClick(Sender: TObject);
begin
  frmStat.Show;
end;

procedure TfrmMain.ClientTextMessage(Sender: TObject; Msg: String);
begin
  frmMessages.RichEdit.Lines.Add(Msg);
  frmMessages.Show;
end;

procedure TfrmMain.mnuAboutClick(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TfrmMain.mnuExitClick(Sender: TObject);
begin
  if Client.Active then
     Client.Disconnect;
  Close;
end;

end.
