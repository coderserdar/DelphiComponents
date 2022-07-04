unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, NMMRCClient, StdCtrls, ExtCtrls, ComCtrls, Grids, Spin,
  NMMCustomClient, ActnList, Menus;

type
  TfrmMain = class(TForm)
    Client: TNMMRCClient;
    StatusBar: TStatusBar;
    MainMenu: TMainMenu;
    mnuOptions: TMenuItem;
    mnuRemoteControl: TMenuItem;
    mnuEnabled: TMenuItem;
    mnuRunTaskManager: TMenuItem;
    mnuPaste: TMenuItem;
    mnuExit: TMenuItem;
    ActionList1: TActionList;
    acEnableRC: TAction;
    acRunTaskManager: TAction;
    acPaste: TAction;
    mnuConnection: TMenuItem;
    sbPicture: TScrollBox;
    Image: TImage;
    mnuStatistics: TMenuItem;
    mnuConnect: TMenuItem;
    N1: TMenuItem;
    acActive: TAction;
    mnuAbout: TMenuItem;
    procedure cbActivateClick(Sender: TObject);
    procedure ClientConnectionTeminated(Sender: TObject;
      ErrorMsg: String);
    procedure FormCreate(Sender: TObject);
    procedure ClientPeriodChangedEvent(Sender: TObject;
      NewPeriod: Integer);
    procedure ClientStatusChanged(Sender: TObject; Connected: Boolean;
      ExtendedStatusInfo: String);
    procedure ClientConnectionRefused(Sender: TObject;
      RefuseReason: TRefuseReason; ExtendedInfo: String);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ClientPictureWndUpdate(Sender: TObject; R: TRect);
    procedure mnuExitClick(Sender: TObject);
    procedure acEnableRCExecute(Sender: TObject);
    procedure acRunTaskManagerExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuOptionsClick(Sender: TObject);
    procedure mnuStatisticsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure acPasteExecute(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
  private
    { Private declarations }
    FirstData: Boolean;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation
uses IdSocks, NMMCommon, NMMRCCommon, Options, ConDlg, Stat, Clipbrd,
  About;

{$R *.dfm}

procedure TfrmMain.cbActivateClick(Sender: TObject);
begin
{
 if Client.Active = (Sender As TAction).Checked then
   exit;
 }
 if not (Sender As TAction).Checked then
 begin
   if frmConDlg.ShowModal=mrOk then
   begin
     with frmOptions,frmConDlg do
     begin
       Client.ServerHost:= edHost.Text;
       Client.ServerPort:= sedPort.Value;

       Client.SocksProxyHost:= edProxyHost.Text;
       Client.SocksProxyPort:= sedProxyPort.Value;
       with cbSocksProxyVersion do
         Client.SocksProxyVersion:= TSocksVersion(Items.Objects[ItemIndex]);

       Client.SocksProxyUserName:= edProxyUserName.Text;
       Client.SocksProxyPassword:= edProxyPassword.Text;

       if Trim(Client.SocksProxyUserName)<>'' then
         Client.SocksProxyAuthentication:= saUsernamePassword
       else
         Client.SocksProxyAuthentication:= saNoAuthentication;

       Client.User:= edUser.Text;
       Client.Password:= edPassword.Text;
       Client.Period:= StrToInt(frmOptions.edPeriod.Text);
       try
         if Client.Active then Client.Disconnect;
         Client.Connect
       except
         acActive.Checked:= false;
         raise;
       end;
     end;
   end;
 end
 else
 begin
   Client.Disconnect;
 end;
 mnuRemoteControl.Enabled:= Client.Active;
end;

procedure TfrmMain.ClientConnectionTeminated(Sender: TObject;
  ErrorMsg: String);
begin
 if acActive.Checked<>false then
    acActive.Checked:= false;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
 FirstData:= True;
end;

procedure TfrmMain.ClientPeriodChangedEvent(Sender: TObject;
  NewPeriod: Integer);
begin
 with frmOptions do
   edPeriod.Text:= IntToStr(NewPeriod);
end;

procedure TfrmMain.ClientStatusChanged(Sender: TObject;
  Connected: Boolean; ExtendedStatusInfo: String);
begin
 if acActive.Checked<>Client.Active then
    acActive.Checked:= Client.Active;

 if Connected then
   StatusBar.Panels[0].Text:= 'Connected'
 else
   StatusBar.Panels[0].Text:= 'Disconnected';
 StatusBar.Panels[1].Text:= ExtendedStatusInfo;
end;

procedure TfrmMain.ClientConnectionRefused(Sender: TObject;
  RefuseReason: TRefuseReason; ExtendedInfo: String);
begin
 with frmConDlg do
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

procedure TfrmMain.ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Client.Active and acEnableRC.Checked then
    if (Image.Picture.Bitmap.Width >0) and
       (Image.Picture.Bitmap.Height > 0) then
    begin
      X:= ((X * 65535) div Image.Picture.Bitmap.Width);
      Y:= ((Y * 65535) div Image.Picture.Bitmap.Height);
      Client.SendCustomCommand(cmdMouseMove + 'X=' + IntToStr(X) + ';Y=' + IntToStr(Y) + ';');
    end;
end;

procedure TfrmMain.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Client.Active and acEnableRC.Checked then
    if (Image.Picture.Bitmap.Width >0) and
       (Image.Picture.Bitmap.Height > 0) then
    begin
      X:= ((X * 65535) div Image.Picture.Bitmap.Width);
      Y:= ((Y * 65535) div Image.Picture.Bitmap.Height);
      Client.SendCustomCommand(cmdMouseDown + 'X=' + IntToStr(X) +
                                           ';Y=' + IntToStr(Y) +
                                           ';S=' + PackShiftState(Shift) +
                                           ';');
    end;
end;

procedure TfrmMain.ImageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Client.Active and acEnableRC.Checked then
    if (Image.Picture.Bitmap.Width >0) and
       (Image.Picture.Bitmap.Height > 0) then
    begin
      X:= ((X * 65535) div Image.Picture.Bitmap.Width);
      Y:= ((Y * 65535) div Image.Picture.Bitmap.Height);
      Client.SendCustomCommand(cmdMouseUp + 'X=' + IntToStr(X) +
                                           ';Y=' + IntToStr(Y) +
                                           ';S=' + PackShiftState(Shift) +
                                           ';');
    end;
end;

procedure TfrmMain.ClientPictureWndUpdate(Sender: TObject; R: TRect);
begin
 sbPicture.VertScrollBar.Range:= Image.Height;
 sbPicture.HorzScrollBar.Range:= Image.Width;
 R.Left:= R.Left - sbPicture.HorzScrollBar.Position;
 R.Right:= R.Right - sbPicture.HorzScrollBar.Position;
 R.Top:= R.Top - sbPicture.VertScrollBar.Position;
 R.Bottom:= R.Bottom - sbPicture.VertScrollBar.Position;
 InvalidateRect(sbPicture.Handle,@R,False);

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
 end;
end;

procedure TfrmMain.mnuExitClick(Sender: TObject);
begin
  Client.Disconnect;
  Close;
end;

procedure TfrmMain.acEnableRCExecute(Sender: TObject);
begin
  acRunTaskManager.Enabled:= acEnableRC.Checked;
  acPaste.Enabled:= acEnableRC.Checked;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
 with frmOptions,frmConDlg do
 begin
   cbSocksProxyVersion.Items.AddObject('svNoSocks',Pointer(svNoSocks));
   cbSocksProxyVersion.Items.AddObject('svSocks4',Pointer(svSocks4));
   cbSocksProxyVersion.Items.AddObject('svSocks4A',Pointer(svSocks4A));
   cbSocksProxyVersion.Items.AddObject('svSocks5',Pointer(svSocks5));
   cbSocksProxyVersion.ItemIndex:= 0;

   sedPort.Value:= DefaultServerPort;
 end;
end;

procedure TfrmMain.mnuOptionsClick(Sender: TObject);
begin
 frmOptions.ShowModal;
end;

procedure TfrmMain.mnuStatisticsClick(Sender: TObject);
begin
  frmStat.Show;
end;

procedure TfrmMain.acRunTaskManagerExecute(Sender: TObject);
begin
  if Client.Active and acEnableRC.Checked then
  begin
    Client.SendCustomCommand(cmdShellExec + 'P=' + 'taskmgr' + ';');
  end;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Client.Disconnect;
  CanClose:= true;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Client.Active and acEnableRC.Checked then
  begin
    Client.SendCustomCommand(cmdKeyDown + 'K=' + IntToStr(Key) +
                                         ';S=' + PackShiftState(Shift) +
                                         ';');
  end;
end;

procedure TfrmMain.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Client.Active and acEnableRC.Checked then
  begin
    Client.SendCustomCommand(cmdKeyUp + 'K=' + IntToStr(Key) +
                                         ';S=' + PackShiftState(Shift) +
                                         ';');
  end;
end;

procedure TfrmMain.acPasteExecute(Sender: TObject);
var LClipBoard: TClipBoard;
begin
  LClipBoard:= TClipBoard.Create;
  try
    Client.SendCustomCommand(cmdClipBoard + LClipBoard.AsText);
  finally
    FreeAndNil(LClipBoard);
  end;
end;

procedure TfrmMain.mnuAboutClick(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

end.
