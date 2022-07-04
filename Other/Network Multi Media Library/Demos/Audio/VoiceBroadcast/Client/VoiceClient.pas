unit VoiceClient;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImageClient, StdCtrls, ExtCtrls, ComCtrls, Grids, Spin,
  CustomNMMClient;

type
  TfrmMain = class(TForm)
    ImageClient: TImageClient;
    Panel1: TPanel;
    cbActivate: TCheckBox;
    pcParams: TPageControl;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    edUser: TEdit;
    edPassword: TEdit;
    Label6: TLabel;
    TabSheet2: TTabSheet;
    Label5: TLabel;
    edHost: TEdit;
    sedPort: TSpinEdit;
    Label8: TLabel;
    TabSheet3: TTabSheet;
    Label9: TLabel;
    edProxyHost: TEdit;
    Label10: TLabel;
    sedProxyPort: TSpinEdit;
    Label3: TLabel;
    edProxyUserName: TEdit;
    Label4: TLabel;
    edProxyPassword: TEdit;
    cbSocksProxyVersion: TComboBox;
    Label7: TLabel;
    StatusBar: TStatusBar;
    pcData: TPageControl;
    tsPicture: TTabSheet;
    tsNetStatistics: TTabSheet;
    lbDeltaBytesReceived: TLabel;
    lbTotalBytesReceived: TLabel;
    lbDeltasReceived: TLabel;
    edPeriod: TEdit;
    Label2: TLabel;
    procedure cbActivateClick(Sender: TObject);
    procedure ImageClientConnectionTeminated(Sender: TObject;
      ErrorMsg: String);
    procedure FormCreate(Sender: TObject);
    procedure ImageClientPeriodChangedEvent(Sender: TObject;
      NewPeriod: Integer);
    procedure ImageClientStatusChanged(Sender: TObject; Connected: Boolean;
      ExtendedStatusInfo: String);
    procedure ImageClientConnectionRefused(Sender: TObject;
      RefuseReason: TRefuseReason; ExtendedInfo: String);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageClientPictureWndUpdate(Sender: TObject; R: TRect);
  private
    { Private declarations }
    FirstData: Boolean;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation
uses IdSocks, NMMCommon, NMMRCCommon;

{$R *.dfm}

procedure TfrmMain.cbActivateClick(Sender: TObject);
begin
 if ImageClient.Active = (Sender As TCheckBox).Checked then
   exit;
 if (Sender As TCheckBox).Checked then
 begin
   ImageClient.ServerHost:= edHost.Text;
   ImageClient.ServerPort:= sedPort.Value;

   ImageClient.SocksProxyHost:= edProxyHost.Text;
   ImageClient.SocksProxyPort:= sedProxyPort.Value;
   with cbSocksProxyVersion do
     ImageClient.SocksProxyVersion:= TSocksVersion(Items.Objects[ItemIndex]);

   ImageClient.SocksProxyUserName:= edProxyUserName.Text;
   ImageClient.SocksProxyPassword:= edProxyPassword.Text;

   if Trim(ImageClient.SocksProxyUserName)<>'' then
     ImageClient.SocksProxyAuthentication:= saUsernamePassword
   else
     ImageClient.SocksProxyAuthentication:= saNoAuthentication;

   ImageClient.User:= edUser.Text;
   ImageClient.Password:= edPassword.Text;
   ImageClient.Period:= StrToInt(edPeriod.Text);
   try
     ImageClient.Connect
   except
     cbActivate.Checked:= false;
     raise;
   end;
 end
 else
 begin
   ImageClient.Disconnect;
 end;
end;

procedure TfrmMain.ImageClientConnectionTeminated(Sender: TObject;
  ErrorMsg: String);
begin
 if cbActivate.Checked<>false then
   cbActivate.Checked:= false;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
 pcData.TabIndex:= 0;
 pcParams.TabIndex:= 0;
 FirstData:= True;

 cbSocksProxyVersion.Items.AddObject('svNoSocks',Pointer(svNoSocks));
 cbSocksProxyVersion.Items.AddObject('svSocks4',Pointer(svSocks4));
 cbSocksProxyVersion.Items.AddObject('svSocks4A',Pointer(svSocks4A));
 cbSocksProxyVersion.Items.AddObject('svSocks5',Pointer(svSocks5));
 cbSocksProxyVersion.ItemIndex:= 0;

 sedPort.Value:= DefaultServerPort;
end;

procedure TfrmMain.ImageClientPeriodChangedEvent(Sender: TObject;
  NewPeriod: Integer);
begin
 edPeriod.Text:= IntToStr(NewPeriod);
end;

procedure TfrmMain.ImageClientStatusChanged(Sender: TObject;
  Connected: Boolean; ExtendedStatusInfo: String);
begin
 if cbActivate.Checked<>ImageClient.Active then
   cbActivate.Checked:= ImageClient.Active;

 if Connected then
   StatusBar.Panels[0].Text:= 'Connected'
 else
   StatusBar.Panels[0].Text:= 'Disconnected';
 StatusBar.Panels[1].Text:= ExtendedStatusInfo;
end;

procedure TfrmMain.ImageClientConnectionRefused(Sender: TObject;
  RefuseReason: TRefuseReason; ExtendedInfo: String);
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

procedure TfrmMain.ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if cbEnableRC.Checked then
  begin
    X:= ((X * 65535) div Image.Picture.Bitmap.Width);
    Y:= ((Y * 65535) div Image.Picture.Bitmap.Height);
    ImageClient.SendCustomCommand(cmdMouseMove + 'X=' + IntToStr(X) + ';Y=' + IntToStr(Y) + ';');
  end;
end;

procedure TfrmMain.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if cbEnableRC.Checked then
  begin
    X:= ((X * 65535) div Image.Picture.Bitmap.Width);
    Y:= ((Y * 65535) div Image.Picture.Bitmap.Height);
    ImageClient.SendCustomCommand(cmdMouseDown + 'X=' + IntToStr(X) + ';Y=' + IntToStr(Y) + ';');
  end;
end;

procedure TfrmMain.ImageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if cbEnableRC.Checked then
  begin
    X:= ((X * 65535) div Image.Picture.Bitmap.Width);
    Y:= ((Y * 65535) div Image.Picture.Bitmap.Height);
    ImageClient.SendCustomCommand(cmdMouseUp + 'X=' + IntToStr(X) + ';Y=' + IntToStr(Y) + ';');
  end;
end;

procedure TfrmMain.ImageClientPictureWndUpdate(Sender: TObject; R: TRect);
begin
 sbPicture.VertScrollBar.Range:= Image.Height;
 sbPicture.HorzScrollBar.Range:= Image.Width;
 R.Left:= R.Left - sbPicture.HorzScrollBar.Position;
 R.Right:= R.Right - sbPicture.HorzScrollBar.Position;
 R.Top:= R.Top - sbPicture.VertScrollBar.Position;
 R.Bottom:= R.Bottom - sbPicture.VertScrollBar.Position;
 InvalidateRect(sbPicture.Handle,@R,False);

 lbDeltasReceived.Caption:= 'Frames Received= ' +
   Format( '%d', [ImageClient.Statistics.DeltasReceivedCount] );
 lbDeltaBytesReceived.Caption:= 'Frame Bytes Received= ' +
   Format( '%d', [ImageClient.Statistics.DeltaBytesReceived] );
 lbTotalBytesReceived.Caption:= 'Total Bytes Received= ' +
   Format( '%d', [ImageClient.Statistics.IniBytesReceived +
                  ImageClient.Statistics.DeltaBytesReceived +
                  ImageClient.Statistics.TextBytesReceived] );
end;

end.
