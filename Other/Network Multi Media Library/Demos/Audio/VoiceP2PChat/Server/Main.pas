unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  StdCtrls, Spin, ExtCtrls,
  IdBaseComponent, IdComponent, IdTCPServer,
  DB, Grids, DBGrids, ActnList, NMMCustomBroadcastServer,
  NMMCustomSimpleBroadcastServer, NMMVoiceBroadcastServer, Menus,
  NMMCustomServer, NMMP2PVoiceServer;

type
  TfrmMain = class(TForm)
    cbActive: TCheckBox;
    ActionList: TActionList;
    acShowThreadInfo: TAction;
    Server: TNMMP2PVoiceServer;
    edIP: TEdit;
    edPort: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    MainMenu: TMainMenu;
    mnuSettings: TMenuItem;
    mnuUsers: TMenuItem;
    mnuBroadcastMessage: TMenuItem;
    mnuAbout: TMenuItem;
    mnuExit: TMenuItem;
    procedure cbActiveClick(Sender: TObject);
    procedure acShowThreadInfoExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ServerAuthentication(Sender: TObject; User, Password: String;
      var Accept: Boolean);
    procedure mnuSettingsClick(Sender: TObject);
    procedure mnuUsersClick(Sender: TObject);
    procedure mnuBroadcastMessageClick(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
  private
    { Private declarations }
    function LocalIP: string;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses UserList, ThreadInfo, WinSock, NoCodecDlg, NMMAudioCommon,
  BroadcastMessage, About;

{$R *.dfm}

function TfrmMain.LocalIP: string;
type
   TaPInAddr = array [0..10] of PInAddr;
   PaPInAddr = ^TaPInAddr;
   PHostEnt= ^hostent;
var
    phe: PHostEnt;
    pptr: PaPInAddr;
    Buffer: array [0..63] of char;
    i: Integer;
    GInitData: TWSADATA;
begin
    WSAStartup($101, GInitData);
    Result := '';
    GetHostName(Buffer, SizeOf(Buffer));
    phe:= Pointer(GetHostByName(buffer));
    if phe = nil then Exit;
    pptr := PaPInAddr(Phe^.h_addr_list);
    i := 0;
    while pptr^[i] <> nil do
    begin
      result:=StrPas(inet_ntoa(pptr^[i]^));
      Inc(i);
    end;
    WSACleanup;
end;

procedure TfrmMain.cbActiveClick(Sender: TObject);
begin
 try
   if (Sender as TCheckBox).Checked = true then
   begin
     edIP.Text:= LocalIP;
     edPort.Text:= IntToStr(Server.Port);
     Server.Active:= true;
   end else
   begin
     Server.Active:= false;
     Server.SaveSettings('settings.dat');
   end;
 except
   (Sender as TCheckBox).Checked:= not (Sender as TCheckBox).Checked;
   raise;
 end;
end;

procedure TfrmMain.acShowThreadInfoExecute(Sender: TObject);
begin
 frmThreadInfo.Show;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Server.LoadSettings('settings.dat');
end;

procedure TfrmMain.FormShow(Sender: TObject);
var LSL: TStringList;
    i: Integer;
    LSpeexFound: Boolean;
begin
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

procedure TfrmMain.ServerAuthentication(Sender: TObject; User,
  Password: String; var Accept: Boolean);
begin
  Accept:= true;
end;

procedure TfrmMain.mnuSettingsClick(Sender: TObject);
begin
  if Server.RunAudioSetupDlg=mrOk then
     Server.SaveSettings('settings.dat');
end;

procedure TfrmMain.mnuUsersClick(Sender: TObject);
var LUserList: TStringList;
begin
 LUserList:= TStringList.Create;
 try
   Server.FillUserList(LUserList);
   frmUsers.lbUsers.Items.Assign(LUserList);
   frmUsers.Left:= Left;
   frmUsers.Top:= Top + Height div 2;
   frmUsers.ShowModal;
 finally
   LUserList.Free;
 end;
end;

procedure TfrmMain.mnuBroadcastMessageClick(Sender: TObject);
begin
  frmBroadcastMessage.Show;
end;

procedure TfrmMain.mnuAboutClick(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TfrmMain.mnuExitClick(Sender: TObject);
begin
  Server.Active:= false;
  Close;
end;

end.
