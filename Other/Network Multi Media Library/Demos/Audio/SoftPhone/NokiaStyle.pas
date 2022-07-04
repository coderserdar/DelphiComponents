unit NokiaStyle;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ActnList, ImgList, NMMCustomServer,
  IdTCPServer,
  NMMCustomBroadcastServer, NMMCustomSimpleBroadcastServer,
  NMMVoiceBroadcastServer, NMMP2PVoiceServer, NMMCustomClient,
  NMMVoiceInClient, NMMVoiceClient, StdCtrls, ExtCtrls;

type
  TPhoneState = (psIdle,psCalling,psCalled);

  TfrmNokiaStyle = class(TForm)
    Image: TImage;
    Label1: TLabel;
    cbNames: TComboBox;
    lbNum: TLabel;
    lbAddPhone: TLabel;
    imCall: TImage;
    imEndCall: TImage;
    ImUp: TImage;
    Client: TNMMVoiceClient;
    Server: TNMMP2PVoiceServer;
    imgsUp: TImageList;
    imgsDown: TImageList;
    imgsEndCall: TImageList;
    imgsCall: TImageList;
    ActionList: TActionList;
    acExit: TAction;
    acCall: TAction;
    acEndCall: TAction;
    acUp: TAction;
    acDown: TAction;
    acAudioSettings: TAction;
    acPhoneSettings: TAction;
    PopupMenu: TPopupMenu;
    mnuCall: TMenuItem;
    N1: TMenuItem;
    mnuAudioSettings: TMenuItem;
    mnuPhoneSettings: TMenuItem;
    mnuExit: TMenuItem;
    ImDown: TImage;
    lbEditPhone: TLabel;
    lbDelPhone: TLabel;
    N2: TMenuItem;
    mnuAbout: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure acCallExecute(Sender: TObject);
    procedure acEndCallExecute(Sender: TObject);
    procedure acAudioSettingsExecute(Sender: TObject);
    procedure acPhoneSettingsExecute(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure imCallClick(Sender: TObject);
    procedure imEndCallClick(Sender: TObject);
    procedure cbNamesChange(Sender: TObject);
    procedure mnuCallClick(Sender: TObject);
    procedure mnuEndCallClick(Sender: TObject);
    procedure ServerBeforeConnect(Sender: TObject; User: String;
      IdPeerThread: TIdPeerThread; var Accept: Boolean;
      var RefuseReason: String);
    procedure ServerDisconnect(Sender: TObject);
    procedure ClientConnectionRefused(Sender: TObject;
      RefuseReason: TRefuseReason; ExtendedInfo: String);
    procedure ClientConnectionTeminated(Sender: TObject; ErrorMsg: String);
    procedure ClientDisconnected(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure lbAddPhoneClick(Sender: TObject);
    procedure lbEditPhoneClick(Sender: TObject);
    procedure lbDelPhoneClick(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
  private
    { Private declarations }
    FBitmap: TBitmap;
    FIncommingCaller: String;
    FMissedCalls: TStringList;
    FStopCalling: Boolean;
    FTakeReceiver: Boolean;
    FDragX, FDragY: Integer;
    FPhoneState: TPhoneState;
    procedure LoadContactList;
    procedure SaveContactList;
  public
    { Public declarations }
  end;

var
  frmNokiaStyle: TfrmNokiaStyle;

implementation

{$R *.dfm}

uses MMSystem, IniFiles,
     NMMCommon, PhoneSettings, EditContact, About;

procedure TfrmNokiaStyle.FormCreate(Sender: TObject);
begin
  if Image.Picture<>nil then
  begin
    Image.Left:= 0;
    Image.Top:= 0;
    Width:= Image.Width;
    Height:= Image.Height;
    Image.SendToBack;
  end;
  Image.SendToBack;
  imCall.BringToFront;
  imEndCall.BringToFront;
  imUp.BringToFront;
  imDown.BringToFront;

  FBitmap:= TBitmap.Create;
  imgsCall.GetBitmap(0,FBitmap);
  imCall.Picture.Bitmap.Assign(FBitmap);
  imgsEndCall.GetBitmap(0,FBitmap);
  imEndCall.Picture.Bitmap.Assign(FBitmap);
  FMissedCalls:= TStringList.Create;

  cbNames.Color:= RGB(146,184,163);

  if cbNames.Items.Count > 0 then
  begin
    cbNames.ItemIndex:= 0;
    cbNamesChange(nil);
  end;

  Server.LoadSettings('audiosettings.dat');
  Client.InOutDevice:= Server.InOutDevice;
  FPhoneState:= psIdle;
  FTakeReceiver:= false;
  FStopCalling:= false;
  LoadContactList;
  if cbNames.Items.Count > 0 then
     cbNames.ItemIndex:= 0;
  cbNamesChange(nil);   
end;

procedure TfrmNokiaStyle.FormShow(Sender: TObject);
begin
  Server.Port:= StrToInt(frmPhoneSettings.edPort.Text);
  Server.Active:= true;
end;

procedure TfrmNokiaStyle.FormHide(Sender: TObject);
begin
  Server.Active:= false;
  if Client.Active then
     Client.Disconnect;
end;

procedure TfrmNokiaStyle.FormDestroy(Sender: TObject);
var i: Integer;
begin
  Server.Active:= false;
  for i:=0 to cbNames.Items.Count-1 do
    if Assigned(cbNames.Items.Objects[i]) then
    begin
      cbNames.Items.Objects[i].Free;
      cbNames.Items.Objects[i]:= nil;
    end;
  FreeAndNil(FBitmap);
  FreeAndNil(FMissedCalls);
end;

procedure TfrmNokiaStyle.acCallExecute(Sender: TObject);
var i: Integer;
    LServerHost: String;
    LServerPort: Integer;
begin
  acCall.Checked:= true;
  mnuCall.Checked:= true;
  imgsCall.GetBitmap(1,FBitmap);
  imCall.Picture.Bitmap.Assign(FBitmap);
  {
  imgsEndCall.GetBitmap(1,FBitmap);
  imEndCall.Picture.Bitmap.Assign(FBitmap);
  }

  if FPhoneState = psIdle then
  begin
   // if Client.User = '' then
    begin
      with cbNames do
      begin
        if ItemIndex > -1 then
        begin
          if not ParseHostAndPortString(TStringWrapper(Items.Objects[ItemIndex]).Value,
                                        LServerHost,LServerPort) then
          begin
            ShowMessage('Invalid IP address: "'+TStringWrapper(Items.Objects[ItemIndex]).Value+'"');
            acEndCallExecute(nil);
            exit;
          end;
        end;
      end;
      FPhoneState:= psCalling;
      Client.ServerHost:= LServerHost;
      Client.ServerPort:= LServerPort;
      Client.User:= frmPhoneSettings.edUser.Text;
      Client.Connect;
  //    FIncommingCaller:= Client.User;
      FStopCalling:= false;
      while not (Client.DataExchangeStarted or FStopCalling) and (FPhoneState=psCalling) do
      begin
        PlaySound('ringout.wav',0,SND_ASYNC);
        for i:=1 to 200 do
        begin
          Application.ProcessMessages;
          Sleep(10);
          if (Client.DataExchangeStarted or FStopCalling) then break;
        end;
      end;

      if not Client.DataExchangeStarted then
      begin
        if FPhoneState=psCalling then
          acEndCallExecute(nil);
        FStopCalling:= false;
      end
      else
        FPhoneState:= psCalling;
    end;
  end
  else
  if FPhoneState = psCalled then
  begin
    FTakeReceiver:= true;
  end;
end;

procedure TfrmNokiaStyle.acEndCallExecute(Sender: TObject);
begin
  FStopCalling:= true;
{
  acCall.Enabled:= true;
  acEndCall.Enabled:= false;
  mnuCall.Enabled:= true;
  mnuEndCall.Enabled:= false;
  }
  {
  imgsEndCall.GetBitmap(0,FBitmap);
  imEndCall.Picture.Bitmap.Assign(FBitmap);
  }
  FStopCalling:= true;
  imgsCall.GetBitmap(0,FBitmap);
  imCall.Picture.Bitmap.Assign(FBitmap);
  acCall.Checked:= false;
  mnuCall.Checked:= false;

  //if Client.User <> '' then
  if FPhoneState = psCalling then
  begin
    //if (Client.Active) or (Client.DataExchangeStarted) then
    FPhoneState:= psIdle;
    Client.Disconnect;
    Client.User:= '';
  end
  else
  if FPhoneState = psCalled then
  begin
    if Server.Active then
    begin
      FPhoneState:= psIdle;
      Server.Active:= false;
      Sleep(10);
      Server.Active:= true;
      FIncommingCaller:= '';
    end;
  end;
  FTakeReceiver:= false;
  FStopCalling:= false;
end;

procedure TfrmNokiaStyle.acAudioSettingsExecute(Sender: TObject);
begin
  if Server.RunAudioSetupDlg=mrOk then
  begin
    Server.SaveSettings('audiosettings.dat');
    Client.InOutDevice:= Server.InOutDevice;
    if Server.Active then
    begin
      Server.Active:= false;
      Server.Active:= true;
    end;
  end;
end;

procedure TfrmNokiaStyle.acPhoneSettingsExecute(Sender: TObject);
begin
  frmPhoneSettings.ShowModal;
  if Server.Active then
  begin
    Server.Active:= false;
    Server.Active:= true;
  end;
end;

procedure TfrmNokiaStyle.acExitExecute(Sender: TObject);
begin
  acEndCallExecute(nil);
  Sleep(5);
  Close;
end;

procedure TfrmNokiaStyle.FormMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDragX:= X;
  FDragY:= Y;
end;

procedure TfrmNokiaStyle.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragX:= 0;
  FDragY:= 0;
end;

procedure TfrmNokiaStyle.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    if FDragX<>0 then
       Left:= Left + (X-FDragX);
    if FDragY<>0 then
       Top:= Top + (Y-FDragY);
    FDragX:= X;
    FDragY:= Y;
  end;
end;

procedure TfrmNokiaStyle.imCallClick(Sender: TObject);
begin
  if acCall.Checked then
    acEndCallExecute(nil)
  else
    acCallExecute(nil);
end;

procedure TfrmNokiaStyle.imEndCallClick(Sender: TObject);
begin
//  acEndCallExecute(nil);
  PopupMenu.Popup(Left+imEndCall.Left,Top+imEndCall.Top);
end;

procedure TfrmNokiaStyle.cbNamesChange(Sender: TObject);
begin
  with cbNames do
    if ItemIndex > -1 then
       lbNum.Caption:= '(' + TStringWrapper(Items.Objects[ItemIndex]).Value + ')'
    else
       lbNum.Caption:= '';
end;

procedure TfrmNokiaStyle.mnuCallClick(Sender: TObject);
begin
  if acCall.Checked then
    acEndCallExecute(nil)
  else
    acCallExecute(nil);
end;

procedure TfrmNokiaStyle.mnuEndCallClick(Sender: TObject);
begin
  acEndCallExecute(nil);
end;

procedure TfrmNokiaStyle.mnuExitClick(Sender: TObject);
begin
  acExitExecute(nil);
end;

procedure TfrmNokiaStyle.ServerBeforeConnect(Sender: TObject; User: String;
  IdPeerThread: TIdPeerThread; var Accept: Boolean;
  var RefuseReason: String);
var i: Integer;
begin
  if FPhoneState = psIdle then
  begin
    FPhoneState:= psCalled;
    while IdPeerThread.Connection.Connected and not FStopCalling  and not FTakeReceiver do
    begin
      PlaySound('ringin.wav',0,SND_ASYNC);
      for i:=1 to 200 do
      begin
        Application.ProcessMessages;
        Sleep(10);
        if not ({acCall.Enabled and }IdPeerThread.Connection.Connected and not FStopCalling and not FTakeReceiver) then break;
      end;
    end;

    if FStopCalling then
    begin
      FMissedCalls.Add(DateTimeToStr(Now)+': '+User);
      Accept:= false;
      RefuseReason:= 'Refused by user';
      FStopCalling:= false;
      FIncommingCaller:= '';
      FPhoneState:= psIdle;
    end;

    if FTakeReceiver then
    begin
      Accept:= true;
      FIncommingCaller:= User;
    //  FPhoneState:= psCalled;
    end;
    FStopCalling:= false;
    FTakeReceiver:= false;
  end
  else
  begin
    Accept:= false;
    RefuseReason:= 'Busy';
    FMissedCalls.Add(DateTimeToStr(Now)+': '+User);
  end;
end;

procedure TfrmNokiaStyle.ServerDisconnect(Sender: TObject);
begin
  FIncommingCaller:= '';
  imgsCall.GetBitmap(0,FBitmap);
  imCall.Picture.Bitmap.Assign(FBitmap);
  acCall.Checked:= false;
  mnuCall.Checked:= false;
  {
  if Server.Active then
  begin
    Server.Active:= false;
    Server.Active:= true;
  end;
  }
  fStopCalling:= false;
  FPhoneState:= psIdle;
end;

procedure TfrmNokiaStyle.ClientConnectionRefused(Sender: TObject;
  RefuseReason: TRefuseReason; ExtendedInfo: String);
var i: Integer;
begin
  inherited;
  Client.User:= '';
  if ExtendedInfo='Busy' then
  begin
    while not FStopCalling do
    begin
      PlaySound('ringout.wav',0,SND_ASYNC);
      for i:=1 to 150 do
      begin
        Application.ProcessMessages;
        Sleep(10);
        if FStopCalling then
        begin
          break;
        end;
      end;
    end;
  end
  else
  begin
    acEndCallExecute(nil);
  end;
  FTakeReceiver:= false;
  FStopCalling:= false;
  FPhoneState:= psIdle;
end;

procedure TfrmNokiaStyle.ClientConnectionTeminated(Sender: TObject;
  ErrorMsg: String);
begin
  Client.User:= '';
  acEndCallExecute(nil);
end;

procedure TfrmNokiaStyle.ClientDisconnected(Sender: TObject);
begin
  Client.User:= '';
  if FPhoneState = psCalling then
     acEndCallExecute(nil);
end;

procedure TfrmNokiaStyle.lbAddPhoneClick(Sender: TObject);
begin
  frmEditContact.Left:= Left + lbAddPhone.Left;
  frmEditContact.Top:= Top + lbAddPhone.Top + lbAddPhone.Height;
  frmEditContact.edName.Text:= '';
  frmEditContact.edURL.Text:= '';
  with cbNames do
  begin
    frmEditContact.Caption:= 'New contact';
    if frmEditContact.ShowModal = mrOk then
    begin
      Items.AddObject(frmEditContact.edName.Text,TStringWrapper.Create(frmEditContact.edURL.Text));
      ItemIndex:= Items.Count - 1;
      SaveContactList;
    end;
  end;
end;

procedure TfrmNokiaStyle.lbEditPhoneClick(Sender: TObject);
var LItemIndex: integer;
begin
  frmEditContact.Left:= Left + lbEditPhone.Left;
  frmEditContact.Top:= Top + lbEditPhone.Top + lbEditPhone.Height;
  with cbNames do
  if ItemIndex >= 0 then
  begin
    frmEditContact.Caption:= 'Edit contact';
    frmEditContact.edName.Text:= Items[ItemIndex];
    frmEditContact.edURL.Text:= TStringWrapper(Items.Objects[ItemIndex]).Value;
    if frmEditContact.ShowModal = mrOk then
    begin
      TStringWrapper(Items.Objects[ItemIndex]).Value:= frmEditContact.edURL.Text;
      Items[ItemIndex]:= frmEditContact.edName.Text;
      LItemIndex:= ItemIndex;
      ItemIndex:= 0;
      ItemIndex:= LItemIndex;
      SaveContactList;
    end;
    cbNamesChange(nil);
  end;
end;

procedure TfrmNokiaStyle.lbDelPhoneClick(Sender: TObject);
var LItemIndex: integer;
begin
  with cbNames do
  if ItemIndex >= 0 then
  begin
    LItemIndex:= ItemIndex;
    if Assigned(Items.Objects[ItemIndex]) then
       Items.Objects[ItemIndex].Free;
    Items.Delete(ItemIndex);
    if Items.Count > 0 then
    begin
      ItemIndex:= 0;
      if LItemIndex > 0 then
         ItemIndex:= LItemIndex - 1;
    end
    else
    begin
      ItemIndex:= -1;
      Text:= '';
    end;
    cbNamesChange(nil);
  end;
  SaveContactList;
end;

procedure TfrmNokiaStyle.LoadContactList;
var LIni: TIniFile;
    LIniName: String;
    i: Integer;
    LSecs: TStringList;
begin
  LIniName:= IncludeTrailingPathDelimiter(
                           ExtractFilePath(Application.ExeName)) + 'ContactList.ini';
  LIni:= TIniFile.Create(LIniName);
  LSecs:= TStringList.Create;
  try
    LIni.ReadSections(LSecs);
    for i:= 0 to LSecs.Count-1 do
    begin
      cbNames.Items.AddObject(LSecs[i],TStringWrapper.Create(LIni.ReadString(LSecs[i],'URL','')));
    end;
  finally
    FreeAndNil(LSecs);
    FreeAndNil(LIni);
  end;
end;

procedure TfrmNokiaStyle.SaveContactList;
var LIni: TIniFile;
    LIniName: String;
    i: Integer;
begin
  LIniName:= IncludeTrailingPathDelimiter(
                           ExtractFilePath(Application.ExeName)) + 'ContactList.ini';
  LIni:= TIniFile.Create(LIniName);
  try
    for i:= 0 to cbNames.Items.Count-1 do
    begin
      LIni.WriteString(cbNames.Items[i],'URL',TStringWrapper(cbNames.Items.Objects[i]).Value);
    end;
  finally
    FreeAndNil(LIni);
  end;
end;

procedure TfrmNokiaStyle.mnuAboutClick(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

end.
