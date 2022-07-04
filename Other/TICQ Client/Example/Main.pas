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

{ *** EXAMPLE APPLICATION COMPLETELY IMPROVED AND UPDATED BY Dalibor Drzik (eraser@senior.cz) *** }

unit Main;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  ComCtrls, ImgList, Menus, ICQClient, ICQWorks, RecvMsg,
  SendMsg, UserInfo, StdCtrls, UserSearch, AutoAway, PktDump,
  UserReg, UserRegNew, ExtCtrls, InfoMsgs, CommCtrl,
  UserUnReg, ChangePasswd, SendURL, SendSMS, RecvURL, RecvSMS,
  About, ShellAPI, StrUtils;

const
  // Status Icons
  ICON_ONLINE = 0;
  ICON_AWAY = 1;
  ICON_DND = 2;
  ICON_NA = 3;
  ICON_INVISIBLE = 4;
  ICON_OFFLINE = 5;
  ICON_OCCUPIED = 6;
  ICON_FFC = 7;

  // Listview Indexes
  LV_INDEX_STATUS = 0;            //STATUS
  LV_INDEX_INTIP = 1;             //Internal IP
  LV_INDEX_EXTIP = 2;             //External IP
  LV_INDEX_PORT = 3;              //Port
  LV_INDEX_PROTOVER = 4;          //ProtoVer
  LV_INDEX_USERCAPS = 5;          //UserCaps
  LV_INDEX_ONLINETIME = 6;        //Online since
  LV_INDEX_CLIENT = 7;            //ICQ Client
  LV_INDEX_MIRANDAVER = 8;        //Miranda version
  LV_INDEX_NICK = 9;              //Nick
  LV_INDEX_IDLE = 10;

  //Not specified
  NA = '<not specified>';

  // Declare the cursor constant that contains the resource identifier
  // of the system Hand cursor.
  //  IDC_HAND = MakeIntResource(32649);
  // Declare the cursor constant for our own use. Constant value must
  // not conflict with any existing Delphi cursor constant.
  NIDC_HAND = 32649;

  USERINFO_ENTRIES = 6;

  WM_NOTIFYICON = wm_user + 400;

  MY_WS_EX_LAYERED = $00080000;
  MY_LWA_ALPHA = $00000002;

type
  _DBCONTACTSETTINGS = record
    dwUIN: DWORD;
    dwStatus: DWORD;
    sInternalIP, sExternalIP: String[15];
    wPort: Word;
    byProtoVer, byUserCaps: Byte;
    dtOnlineTime: TDateTime;
    dwClient, dwMirandaVer: DWORD;
    sNick: String[20];                  // Nick max. 20 chars
  end;

type
  _DBSETTINGS = record
    dwLastStatus: DWORD;                   // Last Status
    bOnTop: Boolean;                       // Window StayOnTop
    sTitleText: String[15];                // Titlebar Text
    bHide: Boolean;                        // Hide or Close main window
    bTransparent: Boolean;                 // Window Transparency
    iBlendValue: Integer;                  // Transparecy level
    iLeft, iTop, iHeight, iWidth: Integer; // Position & Size
    ProxyType: TProxyType;                 // Proxy type
    ProxyAuth: Boolean;                    // Proxy requires auth
    ProxyHost: String[50];                 // Proxy server
    ProxyPass: String[50];                 // Proxy passwd
    ProxyUserID: String[50];               // Username
    ProxyPort: Word;                       // Proxy port
    ProxyResolve: Boolean;                 // Resolve hostnames through proxy
    UIN: DWORD;                            // UIN
    Password: String[15];                  // Passwd
    ICQServer: String[50];                 // ICQ login server
    ICQPort: Word;                         // ICQ Port
    KeepAlive: Boolean;                    // Keep connection alive
    OnSaver, OnWLock: Boolean;             // AutoAway
    OnMouse, SetNA: Boolean;
    AwayTime, NATime: Word;
    MsgAway, MsgNA, MsgDND: String[100];   // AutoAway msgs
    MsgOccupied, MsgFFC: String[100];
  end;

type
  TMainForm = class(TForm)
    ListView1: TListView;
    IconList: TImageList;
    StatusBar1: TStatusBar;
    ICQClient1: TICQClient;
    popupListMenu: TPopupMenu;
    UserInfo1: TMenuItem;
    RemoveContact1: TMenuItem;
    ReadAwayMessage1: TMenuItem;
    AdvIconList: TImageList;
    N2: TMenuItem;
    popupMenu: TPopupMenu;
    sendsms: TMenuItem;
    N6: TMenuItem;
    findcontact: TMenuItem;
    mydetails: TMenuItem;
    N7: TMenuItem;
    options: TMenuItem;
    N8: TMenuItem;
    system: TMenuItem;
    N9: TMenuItem;
    help: TMenuItem;
    N10: TMenuItem;
    Exit2: TMenuItem;
    About1: TMenuItem;
    RegisterNewUIN2: TMenuItem;
    UnregisterUIN1: TMenuItem;
    N11: TMenuItem;
    SetMyAuthorization2: TMenuItem;
    UnsetMyAuthorization2: TMenuItem;
    N12: TMenuItem;
    PacketDump1: TMenuItem;
    LoadContactListfromServer1: TMenuItem;
    N13: TMenuItem;
    Message1: TMenuItem;
    WebPageAddressURL1: TMenuItem;
    N14: TMenuItem;
    ViewHistory1: TMenuItem;
    Rename1: TMenuItem;
    ChangeMyPassword1: TMenuItem;
    popupTray: TPopupMenu;
    HideShow1: TMenuItem;
    N3: TMenuItem;
    Exit3: TMenuItem;
    eICQWebsite1: TMenuItem;
    popupStatus: TPopupMenu;
    Offline2: TMenuItem;
    Online2: TMenuItem;
    Away2: TMenuItem;
    NA2: TMenuItem;
    Occupied2: TMenuItem;
    DND2: TMenuItem;
    Freeforchat1: TMenuItem;
    Invisible2: TMenuItem;
    AutoAwayTimer: TTimer;
    KeepAliveTimer: TTimer;
    N1: TMenuItem;
    GeneralLincence1: TMenuItem;
    Help1: TMenuItem;
    eICQWebsite2: TMenuItem;
    GNUGeneralPublicLicense1: TMenuItem;
    N4: TMenuItem;
    About2: TMenuItem;
    N5: TMenuItem;
    Support1: TMenuItem;
    IdleTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ICQClient1Login(Sender: TObject);
    procedure ICQClient1ConnectionFailed(Sender: TObject);
    procedure ICQClient1StatusChange(Sender: TObject; UIN: String;
      Status: Cardinal);
    procedure ICQClient1UserOffline(Sender: TObject; UIN: String);
    procedure popupListMenuPopup(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure UserInfo1Click(Sender: TObject);
    procedure ICQClient1UserInfoMore(Sender: TObject; UIN: String;
      Age: Word; Gender: Byte; HomePage: String; BirthYear, BirthMonth,
      BirthDay: Word; Lang1, Lang2, Lang3: String);
    procedure ICQClient1UserInfoAbout(Sender: TObject; UIN, About: String);
    procedure ICQClient1UserWorkInfo(Sender: TObject; UIN, WCity, WState,
      WPhone, WFax, FAddress, WZip, WCountry, WCompany, WDepartment,
      WPosition, WOccupation, WHomePage: String);
    procedure ICQClient1UserInfoInterests(Sender: TObject; UIN: String;
      Interests: TStringList);
    procedure ICQClient1UserFound(Sender: TObject; UIN, Nick, FirstName,
      LastName, Email: String; Status: Word; Gender, Age: Byte;
      SearchComplete: Boolean; Authorize: Boolean);
    procedure ICQClient1UserNotFound(Sender: TObject);
    procedure ICQClient1PktParse(Sender: TObject; Buffer: Pointer;
      BufLen: Cardinal; Incoming: Boolean);
    procedure ICQClient1UserInfoBackground(Sender: TObject; UIN: String;
      Pasts, Affiliations: TStringList);
    procedure ICQClient1UserGeneralInfo(Sender: TObject; UIN, NickName,
      FirstName, LastName, Email, City, State, Phone, Fax, Street,
      Cellular, Zip, Country: String; TimeZone: Byte;
      PublishEmail: Boolean);
    procedure RemoveContact1Click(Sender: TObject);
    procedure ICQClient1ServerListRecv(Sender: TObject;
      SrvContactList: TList);
    procedure ReadAwayMessage1Click(Sender: TObject);
    procedure ICQClient1AutoMsgResponse(Sender: TObject; UIN: String;
      ID: Word; RespStatus: Byte; Msg: String);
    procedure ICQClient1PktDirectParse(Sender: TObject; Buffer: Pointer;
      BufLen: Cardinal; Incoming: Boolean);
    procedure ICQClient1URLRecv(Sender: TObject; Description, URL,
      UIN: String);
    procedure ICQClient1Error(Sender: TObject; ErrorType: TErrorType;
      ErrorMsg: String);
    procedure ICQClient1NewUINRefused(Sender: TObject);
    procedure ICQClient1NewUINRegistered(Sender: TObject; UIN: String);
    procedure StatusBar1DrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; const Rect: TRect);
    procedure ICQClient1OfflineMsgRecv(Sender: TObject;
      DateTime: TDateTime; Msg, UIN: String);
    procedure ICQClient1OfflineURLRecv(Sender: TObject;
      DateTime: TDateTime; Description, URL, UIN: String);
    procedure ICQClient1MessageRecv(Sender: TObject; Msg, UIN: String);
    procedure ICQClient1AddedYou(Sender: TObject; UIN: String);
    procedure ICQClient1AuthResponse(Sender: TObject; UIN: String;
      Granted: Boolean; Reason: String);
    procedure ICQClient1AuthorizationChangedOk(Sender: TObject);
    procedure ListView1DrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    procedure findcontactClick(Sender: TObject);
    procedure mydetailsClick(Sender: TObject);
    procedure StatusBar1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Exit2Click(Sender: TObject);
    procedure SetMyAuthorization2Click(Sender: TObject);
    procedure UnsetMyAuthorization2Click(Sender: TObject);
    procedure PacketDump1Click(Sender: TObject);
    procedure LoadContactListfromServer1Click(Sender: TObject);
    procedure RegisterNewUIN2Click(Sender: TObject);
    procedure UnregisterUIN1Click(Sender: TObject);
    procedure ChangeMyPassword1Click(Sender: TObject);
    procedure ICQClient1UnregisterBadPassword(Sender: TObject);
    procedure ICQClient1UnregisterOk(Sender: TObject);
    procedure ICQClient1InfoChanged(Sender: TObject; InfoType: TInfoType;
      ChangedOk: Boolean);
    procedure Message1Click(Sender: TObject);
    procedure WebPageAddressURL1Click(Sender: TObject);
    procedure ICQClient1SMSAck(Sender: TObject; Source, Network,
      MsgId: String; Deliverable: Boolean);
    procedure ICQClient1SMSRefused(Sender: TObject);
    procedure ICQClient1SMSReply(Sender: TObject; Source, SmsSender, Time,
      Text: String);
    procedure sendsmsClick(Sender: TObject);
    procedure ListView1ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure eICQWebsite1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Exit3Click(Sender: TObject);
    procedure HideShow1Click(Sender: TObject);
    procedure StatusClick(Sender: TObject);
    procedure ListView1Edited(Sender: TObject; Item: TListItem;
      var S: String);
    procedure ListView1Editing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure Rename1Click(Sender: TObject);
    procedure AutoAwayTimerTimer(Sender: TObject);
    procedure optionsClick(Sender: TObject);
    procedure KeepAliveTimerTimer(Sender: TObject);
    procedure ICQClient1LogOff(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure GeneralLincence1Click(Sender: TObject);
    procedure ViewHistory1Click(Sender: TObject);
    procedure ListView1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ListView1Click(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure Support1Click(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ICQClient1AdvancedMsgAck(Sender: TObject; UIN: String;
      ID: Word; AcceptType: Byte; AcceptMsg: String);
    procedure ICQClient1AuthRequest(Sender: TObject; UIN, Reason: String);
    procedure ICQClient1OnlineInfo(Sender: TObject; UIN: String;
      Port: Word; InternalIP, ExternalIP: String; OnlineTime: TDateTime;
      Idle: Word; ICQVersion, MirandaVersion: Cardinal; ProtoVer,
      UserCaps: Byte);
    procedure IdleTimerTimer(Sender: TObject);
  private
    procedure ReCreateContactList;
    procedure RepaintOfflineCL;
    procedure ListViewWndProc(var Message: TMessage);
    procedure RefreshRowHeight;
    procedure WMMeasureItem(var Message: TWMMeasureItem); message WM_MEASUREITEM;
  protected
    procedure WMNotifyIcon(var Message : TMessage); message WM_NOTIFYICON;
  public
    FConnecting: Boolean;
    FInfoList: TList;
    bSetAuthorization: Boolean;
    FListViewWndProc: TWndMethod;
    hImlMenuIcons: HIMAGELIST;
    UserInfoReader: Byte;
    tnid: TNotifyIconData;
    bEditAble: Boolean;
    sUIN: String;
    function GetUserInfoIdx(Value: String): Integer;
    procedure DoCreateInfoQuery(UIN: String);
    procedure DoStatusChange(NewStatus: LongWord);
    procedure DeleteFromDB(dwUIN: DWORD);
    procedure WriteToDB(dwUIN: DWORD; DBD: _DBCONTACTSETTINGS);
    procedure ReadFromDB(DBD: _DBCONTACTSETTINGS);
    function ConvertDateTime(DateTime: TDateTime): String;
    procedure GetWindowsVersion;
    procedure OpenURL(const sURL: String; IsEmail: Boolean);
    procedure SetLabel(var lblLabel: TLabel; Enabled: Boolean; URL: Boolean; Text: String);
    function LocalTime(const DateTime: TDateTime): TDateTime;
    function ReadSettings(): _DBSETTINGS;
    procedure WriteSettings(Settings: _DBSETTINGS);
    procedure SetSettings(Settings: _DBSETTINGS);
    procedure SetCriticalSettings(Settings: _DBSETTINGS);
    procedure OnlyNumbers(var EditB: TEdit);
    function ValidateRange(Num: String; FType: Byte): Boolean;
    procedure LogToFile(const strUIN, Text, NickName: String; Sent: Boolean; const DateTime: String);
    function CryptPassword(Encrypt: Boolean; const Value: String): String;
  end;

type
  _DBCONTACTSETTINGSFILE = file of _DBCONTACTSETTINGS;

type
  _DBSETTINGSFILE = file of _DBSETTINGS;

type
  TSetLayeredWindowAttributes = function (Hwnd: THandle; crKey: COLORREF; bAlpha: Byte; dwFlags: DWORD): Boolean; stdcall;

var
  MainForm: TMainForm;
  fDCFlag: LongWord = S_ALLOWDCONN;  //S_ALLOWDAUTH;
  DBData: _DBCONTACTSETTINGS;
  F: _DBCONTACTSETTINGSFILE;
  D: _DBSETTINGSFILE;
  LVItemHeight: Integer = 17;
  IsWinVerNT: Boolean;
  IsWinVerNT4Plus: Boolean;
  IsWinVer98Plus: Boolean;
  IsWinVerMEPlus: Boolean;
  IsWinVer2000Plus: Boolean;
  IsWinVerXPPlus: Boolean;

  OnSaver, OnWLock, OnMouse, SetNA: Boolean;
  lastMousePos: TPoint;
  mouseStationaryTimer: Integer = 0;
  awayModeTimer: Integer = 0;
  awaySet: Integer = 0;
  naSet: Integer = 0;
  AwayTime: Word = 5;
  NATime: Word = 20;
  originalStatusMode: DWORD;
  OnTop: HWND = HWND_TOPMOST;
  bHide: Boolean = False;
  bTransparent: Boolean = True;
  iBlendValue: Byte = 70;
  dwLastStatus: DWORD = S_OFFLINE;
  bMyDetails: Boolean = False;

  OldTestHit: TListItem = nil;
  TestHit: TListItem = nil;

  MsgAway: String = '';
  MsgNA: String = '';
  MsgDND: String = '';
  MsgOccupied: String = '';
  MsgFFC: String = '';

  MyGetLastInputInfo: function(var plii: TLastInputInfo): LongBool; stdcall;

  MySetLayeredWindowAttributes: TSetLayeredWindowAttributes = nil;

implementation

uses SysUtils, DateUtils, Options;

{$R *.dfm}


function TMainForm.CryptPassword(Encrypt: Boolean; const Value: String): String;
var
  i: Word;
begin
  Result := '';
  if Length(Value) < 1 then Exit;
  case Encrypt of
    True: for i := 1 to Length(Value) do             // Encrypt
      Result := Result + Chr(Ord(Value[i]) + 5);

    False: begin
      for i := 1 to Length(Value) do
        Result := Result + Chr(Ord(Value[i]) - 5);   // Decrypt
           end;
  end;
end;


procedure TMainForm.LogToFile(const strUIN, Text, NickName: String; Sent: Boolean; const DateTime: String);
var
  FileName: String;
  txt: TextFile;

begin
  FileName := Format('%s%s%s%s', [ExtractFileDir(ParamStr(0)), '\History\', strUIN, '.txt']);

  try
    AssignFile(txt, FileName);
    if FileExists(FileName) then
      Append(txt)
    else
      Rewrite(txt);
      
    case Sent of
      True: Writeln(txt, Format('To: %s (%s) [%s]', [NickName, strUIN, DateTime]));

      False: Writeln(txt, Format('From: %s (%s) [%s]', [NickName, strUIN, DateTime]));
    end;
    Writeln(txt, '-----------------------------------------');
    Writeln(txt, Text);
    Writeln(txt, '');
    Flush(txt);
  finally
    CloseFile(txt);
  end;
end;

function TMainForm.ValidateRange(Num: String; FType: Byte): Boolean;
const
  MAX_BYTE = 255;
  MAX_WORD =  65535;
  MAX_DWORD = 4294967295;
var
  E: Integer;
  V: Int64;
begin
  Val(Num, V, E);

  if E = 0 then begin
    case FType of
      0: begin
          if V > MAX_BYTE then
            Result := False
          else
            Result := True;
         end;

      1: begin
          if V > MAX_WORD then
            Result := False
          else
            Result := True;
         end;

      2: begin
          if V > MAX_DWORD then
            Result := False
          else
            Result := True;
         end;
      else
        Result := False;
    end;
  end else
    Result := False;
end;

procedure TMainForm.OnlyNumbers(var EditB: TEdit);
begin
  SetWindowLong(EditB.Handle, GWL_STYLE, GetWindowLong(EditB.Handle, GWL_STYLE) or ES_NUMBER);
end;

function TMainForm.ReadSettings(): _DBSETTINGS;
var
  FileName: String;
  DBSet: _DBSETTINGS;
  bWasReaded: Boolean;
begin
  FileName := ExtractFilePath(ParamStr(0)) + 'settings.dat';
  bWasReaded := False;
  try
    AssignFile(D, FileName);
    if FileExists(FileName) then begin
      Reset(D);
      while not EOF(D) do
      begin
        Read(D, DBSet);
        DBSet.Password := CryptPassword(False, DBSet.Password);
        bWasReaded := True;
      end;
    end
    else
      Rewrite(D);

  if not bWasReaded then begin
    // Default settings
    DBSet.dwLastStatus := S_OFFLINE;     // Last Status
    DBSet.bOnTop := True;                // Window StayOnTop
    DBSet.sTitleText := 'eICQ';          // Titlebar Text
    DBSet.bHide := True;                 // Hide or Close main window
    DBSet.bTransparent := True;          // Window Transparency
    DBSet.iBlendValue := 70;             // Transparecy level
    DBSet.iLeft := 0;
    DBSet.iTop := 0;
    DBSet.iHeight := 292;
    DBSet.iWidth := 100;                 // Position & Size
    DBSet.ProxyType := P_NONE;           // Proxy type
    DBSet.ProxyAuth := False;            // Proxy requires auth
    DBSet.ProxyHost := '';               // Proxy server
    DBSet.ProxyPass := '';               // Proxy passwd
    DBSet.ProxyUserID := '';             // Username
    DBSet.ProxyPort := 1080;             // Proxy port
    DBSet.ProxyResolve := False;         // Resolve hostnames through proxy
    DBSet.UIN := 0;                      // UIN
    DBSet.Password := '';                // Passwd
    DBSet.ICQServer := 'login.icq.com';  // ICQ login server
    DBSet.ICQPort := 5190;               // ICQ Port
    DBSet.KeepAlive := False;            // Keep connection alive
    DBSet.OnSaver := True;
    DBSet.OnWLock := True;               // AutoAway
    DBSet.OnMouse := True;
    DBSet.SetNA := True;
    DBSet.AwayTime := 5;
    DBSet.NATime := 20;
                                          // AutoAway msgs
    DBSet.MsgAway := 'I''ve been away since %time%';
    DBSet.MsgNA := 'Give it up, I''m not in!' + #13#10 + #13#10 +
                   'N/A since %time% %date%';
    DBSet.MsgDND :=  'Give a guy some peace, would ya?';
    DBSet.MsgOccupied := 'Not right now.';
    DBSet.MsgFFC :=  'Well, I would talk to you if eICQ supported chat';
  end;

  finally
    CloseFile(D);
  end;

  Result := DBSet;
end;

procedure TMainForm.SetCriticalSettings(Settings: _DBSETTINGS);
begin
  if not ICQClient1.LoggedIn then begin
    dwLastStatus := Settings.dwLastStatus;             // Last Status
    ICQClient1.ProxyType := Settings.ProxyType ;       // Proxy type
    ICQClient1.ProxyAuth := Settings.ProxyAuth ;       // Proxy requires auth
    ICQClient1.ProxyHost := Settings.ProxyHost ;       // Proxy server
    ICQClient1.ProxyPass := Settings.ProxyPass ;       // Proxy passwd
    ICQClient1.ProxyUserID := Settings.ProxyUserID ;   // Username
    ICQClient1.ProxyPort := Settings.ProxyPort ;       // Proxy port
    ICQClient1.ProxyResolve := Settings.ProxyResolve ; // Resolve hostnames through proxy

    ICQClient1.UIN := Settings.UIN ;                   // UIN
    ICQClient1.Password := Settings.Password ;         // Passwd
    ICQClient1.ICQServer := Settings.ICQServer ;       // ICQ login server
    ICQClient1.ICQPort := Settings.ICQPort ;           // ICQ Port
  end;
end;

procedure TMainForm.SetSettings(Settings: _DBSETTINGS);
var
  AStyle: Integer;
begin
  if Settings.bOnTop then                        // Window StayOnTop
   OnTop := HWND_TOPMOST
  else
   OnTop := HWND_NOTOPMOST;

  Caption := Settings.sTitleText;               // Titlebar Text
  bHide := Settings.bHide;                      // Hide or Close main window
  bTransparent := Settings.bTransparent;        // Window Transparency
  iBlendValue := Settings.iBlendValue;          // Transparecy level

  Self.Left := Settings.iLeft;
  Self.Top := Settings.iTop;
  Self.Height := Settings.iHeight;
  Self.Width := Settings.iWidth;                     // Position & Size

  KeepAliveTimer.Enabled := Settings.KeepAlive; // Keep connection alive
                                                 // AutoAway
  OnSaver := Settings.OnSaver;
  OnWLock := Settings.OnWLock;
  OnMouse := Settings.OnMouse;
  SetNA := Settings.SetNA;

  AwayTime := Settings.AwayTime;
  NATime := Settings.NATime;
                                                 // AutoAway msgs
  MsgAway := Settings.MsgAway;
  MsgNA := Settings.MsgNA;
  MsgDND := Settings.MsgDND;
  MsgOccupied := Settings.MsgOccupied;
  MsgFFC := Settings.MsgFFC;

  //System Tray
  SetWindowLong(Application.Handle, GWL_EXSTYLE, WS_EX_TOOLWINDOW);

  AStyle := GetWindowLong(Handle, GWL_EXSTYLE);
  if (@MySetLayeredWindowAttributes <> nil) and bTransparent then begin
    if (AStyle and MY_WS_EX_LAYERED) = 0 then
      SetWindowLong(Handle, GWL_EXSTYLE, AStyle or MY_WS_EX_LAYERED);
    MySetLayeredWindowAttributes(Handle, RGB(0,0,0), Round(iBlendValue * 255 div 100), MY_LWA_ALPHA);
  end
  else begin
      SetWindowLong(Handle, GWL_EXSTYLE, AStyle and not MY_WS_EX_LAYERED);
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME or RDW_ALLCHILDREN);
  end;

  // FormStyle
  SetWindowPos(Handle, OnTop, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
end;


procedure TMainForm.WriteSettings(Settings: _DBSETTINGS);
var
  FileName: String;
begin
  FileName := ExtractFilePath(ParamStr(0)) + 'settings.dat';
  try
    AssignFile(D, FileName);
    if FileExists(FileName) then
      Reset(D)
    else
      Rewrite(D);

    Settings.Password := CryptPassword(True, Settings.Password);
    Write(D, Settings);
  finally
    CloseFile(D);
  end;
end;

procedure TMainForm.WMNotifyIcon(var Message : TMessage);
var
  uMouseMsg : Integer;
  CursorPos: TPoint;
begin
  uMouseMsg := Message.lParam;
  inherited;
  case uMouseMsg of
    WM_LBUTTONDBLCLK:
      HideShow1.Click;
      
    WM_RBUTTONUP:
    begin
      SetForegroundWindow(Handle);
      GetCursorPos(CursorPos);
      Self.popupTray.Popup(CursorPos.X, CursorPos.Y);
      PostMessage(Handle, WM_NULL, 0, 0);
    end;
  end;
end;

//Specifies the current bias, in minutes, for local time translation on this
//computer. The bias is the difference, in minutes, between Coordinated
//Universal Time (UTC) and local time. All translations between UTC and local
//time are based on the following formula: UTC = local time + bias

function TMainForm.LocalTime(const DateTime: TDateTime): TDateTime;
var
  TimeZoneInfo: TTimeZoneInformation;
begin
  case GetTimeZoneInformation(TimeZoneInfo) of
    TIME_ZONE_ID_STANDARD:
      Result := DateTime - (TimeZoneInfo.Bias / 60 / 24); 
    TIME_ZONE_ID_DAYLIGHT: 
      Result := DateTime - ((TimeZoneInfo.Bias + TimeZoneInfo.DaylightBias) / 60 / 24); 
    else 
      Result := 0; 
  end;
end;

procedure TMainForm.OpenURL(const sURL: String; IsEmail: Boolean);
begin
  if IsEmail then
    ShellExecute(Handle,'open',PChar('mailto:' + sURL),'','',sw_Normal)
  else
    ShellExecute(Handle,'open',PChar(sURL),'','',sw_Normal);
end;

{Convert datetime in format like 'Tue Mar 09 08:32:06 2004'}
function TMainForm.ConvertDateTime(DateTime: TDateTime): String;
var
  FormatSettings: TFormatSettings;
begin
  GetLocaleFormatSettings(LANG_ENGLISH, FormatSettings);
  Result := FormatDateTime('ddd mmm dd hh:nn:ss yyyy', DateTime, FormatSettings);
end;

procedure TMainForm.GetWindowsVersion;
var
  WinVerMajor: DWORD;
  WinVerMinor: DWORD;
begin
  WinVerMajor := (LoByte(LoWord(GetVersion)));
  WinVerMinor := (HiByte(LoWord(GetVersion)));

  // WinVerNT
  if GetVersion < $80000000 then IsWinVerNT := True;

  // WinVerNT4Plus
  if (WinVerMajor >= 5) or (WinVerMinor > 0) or IsWinVerNT
    then IsWinVerNT4Plus := True;

  // WinVer98Plus
  if LoWord(GetVersion) <> 4 then IsWinVer98Plus := True;

  // WinVerMEPlus
  if (WinVerMajor >= 5) or (WinVerMinor > 10) then IsWinVerMEPlus := True;

  // WinVer2000Plus
  if WinVerMajor >= 5 then IsWinVer2000Plus := True;

  // WinVerXPPlus
  if (WinVerMajor >= 5) and (LoWord(GetVersion) <> 5) then IsWinVerXPPlus := True;
end;

procedure TMainForm.RefreshRowHeight;
var
  wp: TWindowPos;
begin
  wp.hwnd := ListView1.Handle;
  wp.cx := ListView1.Width;
  wp.cy := ListView1.Height;
  wp.flags := SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOOWNERZORDER or SWP_NOZORDER;
  SendMessage(ListView1.Handle, WM_WINDOWPOSCHANGED, 0, Longint(@wp));
end;

procedure TMainForm.ListViewWndProc(var Message: TMessage);
begin
  ShowScrollBar(ListView1.Handle, SB_HORZ, False); // cacher la barre horizontale
  FListViewWndProc(Message); // process message
end;

procedure TMainForm.WMMeasureItem(var Message: TWMMeasureItem);
begin
  inherited;
  if Message.MeasureItemStruct.CtlType = ODT_LISTVIEW then
  begin
    Message.MeasureItemStruct.itemHeight := LVItemHeight;
    Message.Result := 1;
  end;
end;

function DoLoadIcons(FileName: String): Boolean;
var
  dllHandle: HMODULE;

  procedure DoSingleEntry(Name: String);
  var
    Icon: TIcon;
  begin
    Icon := TIcon.Create;
    Icon.Handle := LoadIcon(dllHandle, PChar(Name));
    MainForm.IconList.AddIcon(Icon);
  end;

begin
  Result := False;
  dllHandle := LoadLibrary(PChar(FileName));
  if dllHandle <> 0 then
  begin
    DoSingleEntry('#104');  //Online      0
    DoSingleEntry('#128');  //Away        1
    DoSingleEntry('#158');  //DND         2
    DoSingleEntry('#131');  //N/A         3
    DoSingleEntry('#130');  //Invisible   4
    DoSingleEntry('#105');  //Offline     5
    DoSingleEntry('#159');  //Occupied    6
    DoSingleEntry('#129');  //FFC         7
    FreeLibrary(dllHandle);
    Result := True;
  end;
end;

//Deleting user from ContactList
procedure TMainForm.DeleteFromDB(dwUIN: DWORD);
var
  FileName: String;
  podm: Longint;
  EmptyDBD: _DBCONTACTSETTINGS;

  function ContactExists: LongInt;
  var
    Position: Longint;
  begin
    Position := -1;
    while not EOF(F) do
    begin
      Read(F, DBData);
      if DBData.dwUIN = dwUIN then
      begin
        Position := FilePos(F)-1;
        Break;
      end;
    end;
    Result := Position;
  end;

begin
  FileName := ExtractFilePath(ParamStr(0)) + 'contacts.dat';
  try
    AssignFile(F, FileName);
    if FileExists(FileName) then
      Reset(F)
    else
      Rewrite(F);

    podm := ContactExists;

    if podm <> -1 then
    begin
      Seek(F, podm);         //Edit existing contact

      EmptyDBD.dwUIN := 0;
      EmptyDBD.dwStatus := S_OFFLINE;
      EmptyDBD.sInternalIP := '';
      EmptyDBD.sExternalIP := '';
      EmptyDBD.wPort := 0;
      EmptyDBD.byProtoVer := 0;
      EmptyDBD.byUserCaps := 0;
      EmptyDBD.dtOnlineTime := 0;
      EmptyDBD.dwClient := 0;
      EmptyDBD.dwMirandaVer := 0;
      EmptyDBD.sNick := '';

      Write(F, EmptyDBD);
    end;
  finally
    CloseFile(F);
  end;
end;

//Adding new user, UserInfo, Nick change
procedure TMainForm.WriteToDB(dwUIN: DWORD; DBD: _DBCONTACTSETTINGS);
var
  FileName: String;
  podm: Longint;

  function ContactExists: LongInt;
  var
    Position: Longint;
  begin
    Position := -1;
    while not EOF(F) do
    begin
      Read(F, DBData);
      if DBData.dwUIN = dwUIN then
      begin
        Position := FilePos(F)-1;
        Break;
      end;
    end;
    Result := Position;
  end;

begin
  FileName := ExtractFilePath(ParamStr(0)) + 'contacts.dat';
  try
    AssignFile(F, FileName);
    if FileExists(FileName) then
      Reset(F)
    else
      Rewrite(F);

    podm := ContactExists;

    if podm = -1 then
      Seek(F, FileSize(F))   //Add new contact
    else
      Seek(F, podm);         //Edit existing contact

    Write(F, DBD);
  finally
    CloseFile(F);
  end;
end;

procedure TMainForm.ReadFromDB(DBD: _DBCONTACTSETTINGS);
var
  FileName: String;
  ListItem: TListItem;
begin
  FileName := ExtractFilePath(ParamStr(0)) + 'contacts.dat';
  try
    AssignFile(F, FileName);
    if FileExists(FileName) then
      Reset(F)
    else
      Rewrite(F);

    while not EOF(F) do
    begin
      Read(F, DBD);

      if DBD.dwUIN <> 0 then
      begin

        ListItem := ListView1.Items.Add;

        ListItem.ImageIndex := ICON_OFFLINE;
        //UIN
        ListItem.Caption := IntToStr(DBD.dwUIN);
        ICQClient1.AddContact(DBD.dwUIN);
        //STATUS
        ListItem.SubItems.Add(IntToStr(DBD.dwStatus));
        //Internal IP
        ListItem.SubItems.Add(DBD.sInternalIP);
        //External IP
        ListItem.SubItems.Add(DBD.sExternalIP);
        //Port
        ListItem.SubItems.Add(IntToStr(DBD.wPort));
        //ProtoVer
        ListItem.SubItems.Add(IntToStr(DBD.byProtoVer));
        //UserCaps
        ListItem.SubItems.Add(IntToStr(DBD.byUserCaps));
        //Online since
        ListItem.SubItems.Add(FloatToStr(DBD.dtOnlineTime));
        //ICQ Client
        ListItem.SubItems.Add(IntToStr(DBD.dwClient));
        //Miranda version
        ListItem.SubItems.Add(IntToStr(DBD.dwMirandaVer));
        //Nick
        ListItem.SubItems.Add(DBD.sNick);
        //Idle
        ListItem.SubItems.Add('0');
      end;
    end;
  finally
    CloseFile(F);
  end;
end;

procedure TMainForm.RepaintOfflineCL;
var
  i: Integer;
begin
  if ListView1.Items.Count > 0 then
    for i := 0 to ListView1.Items.Count - 1 do
      ListView1.Items.Item[i].ImageIndex := ICON_OFFLINE;
  ListView1.Invalidate;
end;

procedure TMainForm.ReCreateContactList;
begin
  //ListView Filling
  ListView1.Items.Clear;
  ReadFromDB(DBData);
  ListView1.Invalidate;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  UserInfoReader := 0;

  bEditAble := False;

  FListViewWndProc := ListView1.WindowProc;
  ListView1.WindowProc := ListViewWndProc;

  //Load icons
  if not DoLoadIcons(ExtractFilePath(ParamStr(0)) + 'Icons/icons.dll') then
  begin
    MessageBox(Self.Handle,'Could not load icons! Application will be terminated.','Error',MB_OK);
    Application.Terminate;
  end;  

  GetWindowsVersion;


  if IsWinVer2000Plus then begin
    @MyGetLastInputInfo := GetProcAddress(GetModuleHandle('user32'), 'GetLastInputInfo');

    @MySetLayeredWindowAttributes := GetProcAddress(GetModuleHandle('user32.dll'), 'SetLayeredWindowAttributes');
  end;

  //Icons
  if IsWinVerXPPlus then  // IsWinVerXPPlus need 32-bit icons on XP for alpha channels
    hImlMenuIcons  := ImageList_Create(GetSystemMetrics(SM_CXSMICON),
      GetSystemMetrics(SM_CYSMICON), ILC_COLOR32 or ILC_MASK, IconList.Count, 20)
  else
  //Win2k won't blend icons with imagelist_drawex when color-depth>16-bit.
  //Don't know about WinME, but it certainly doesn't support alpha channels
		hImlMenuIcons := ImageList_Create(GetSystemMetrics(SM_CXSMICON),
      GetSystemMetrics(SM_CYSMICON), ILC_COLOR16 or ILC_MASK, IconList.Count, 20);

  for i := 0 to IconList.Count - 1 do
  begin
    IconList.GetIcon(i, Icon);
    ImageList_AddIcon(hImlMenuIcons, Icon.Handle);
  end;

  //Icon
  IconList.GetIcon(ICON_OFFLINE, Icon);
  StatusBar1.Panels[0].Text := 'Offline';

  tnid.cbSize:= sizeof(tnid);
  tnid.Wnd:= Handle;
  tnid.uID:= Application.Icon.Handle;
  tnid.uFlags:= NIF_MESSAGE or NIF_ICON or NIF_TIP;
  tnid.uCallbackMessage:= WM_NOTIFYICON;
  tnid.hIcon := Icon.Handle;
  StrPCopy(tnid.szTip, Format('ICQ - %s ', ['Offline']));
  Shell_NotifyIconA(NIM_ADD, @tnid);

  SetCriticalSettings(ReadSettings());
  SetSettings(ReadSettings());

  //List of avaible info query forms
  FInfoList := TList.Create;

  //Add users from contactlist to listview in offline mode
  ReCreateContactList;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if bHide then begin
    Action := caNone;
    Self.Hide;
  end
  else
    Application.Terminate;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  DBSet: _DBSETTINGS;
begin
  TestHit := nil;

  DBSet := ReadSettings();
  DBSet.iHeight := Height;
  DBSet.iWidth := Width;
  WriteSettings(DBSet);

  DBSet := ReadSettings();
  DBSet.iLeft := Left;
  DBSet.iTop := Top;
  WriteSettings(DBSet);
  
  ListView1.WindowProc := FListViewWndProc;
  FListViewWndProc := nil;
  ImageList_Destroy(hImlMenuIcons);
  Shell_NotifyIconA(NIM_DELETE, @tnid);
  FInfoList.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  RefreshRowHeight;
  if dwLastStatus <> S_OFFLINE then DoStatusChange(dwLastStatus or fDCFlag);
end;

procedure TMainForm.DoStatusChange(NewStatus: LongWord);
var
  img: Byte;

  function SetAutoAwayMsg(AnyMsg: String): String;
  begin
    while Pos('%time%', AnyMsg) <> 0 do
    begin
      Insert(TimeToStr(Now), AnyMsg, Pos('%time%', AnyMsg));
      Delete(AnyMsg, Pos('%time%', AnyMsg), Length('%time%'));
    end;
    while Pos('%date%', AnyMsg) <> 0 do
    begin
      Insert(DateToStr(Now), AnyMsg, Pos('%date%', AnyMsg));
      Delete(AnyMsg, Pos('%date%', AnyMsg), Length('%date%'));
    end;
    Result := AnyMsg;
  end;

begin
  if not ICQClient1.LoggedIn then
  begin
    if (ICQClient1.Password = '') or (ICQClient1.UIN = 0) then
    begin
      MessageBox(MainForm.Handle, 'Please set UIN & Password in Options dialog!', 'Error!', MB_OK);
      options.Click;
      Exit;
    end;
    StatusBar1.Panels[0].Text := 'Connecting...';
    ICQClient1.Login(NewStatus);
  end else
  begin
    ICQClient1.Status := NewStatus;
    StatusBar1.Panels[0].Text := StatusToStr(ICQClient1.Status);
  end;

  case StatusToInt(NewStatus) of
    S_ONLINE : begin
                 img := ICON_ONLINE;
               end;
    S_AWAY   : begin
                 img := ICON_AWAY;
                 ICQClient1.AutoAwayMessage := SetAutoAwayMsg(MsgAway);
               end;
    S_DND    : begin
                 img := ICON_DND;
                 ICQClient1.AutoAwayMessage := SetAutoAwayMsg(MsgDND);
               end;
    S_NA     : begin
                 img := ICON_NA;
                 ICQClient1.AutoAwayMessage := SetAutoAwayMsg(MsgNA);
               end;
    S_INVISIBLE : begin
                    img := ICON_INVISIBLE;
                  end;
    S_OCCUPIED : begin
                   img := ICON_OCCUPIED;
                   ICQClient1.AutoAwayMessage := SetAutoAwayMsg(MsgOccupied);
               end;
    S_FFC     : begin
                 img := ICON_FFC;
                 ICQClient1.AutoAwayMessage := SetAutoAwayMsg(MsgFFC);
               end;
  else
    img := ICON_OFFLINE;
  end;
  
  IconList.GetIcon(img, Icon);
  StatusBar1.Invalidate;

  tnid.hIcon := Icon.Handle;
  StrPCopy(tnid.szTip, Format('ICQ - %s ', [StatusToStr(NewStatus)]));
  Shell_NotifyIconA(NIM_MODIFY, @tnid);
end;

procedure TMainForm.ICQClient1Login(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := StatusToStr(ICQClient1.Status);
  ICQClient1.RequestOfflineMessages;
  AutoAwayTimer.Enabled := True;
end;

procedure TMainForm.ICQClient1ConnectionFailed(Sender: TObject);
begin
  IconList.GetIcon(ICON_OFFLINE, Icon);
  StatusBar1.Panels[0].Text := 'Connection failed';
  //ReCreateContactList;
  RepaintOfflineCL;

  if AutoAwayTimer.Enabled then AutoAwayTimer.Enabled := False;

  SetCriticalSettings(ReadSettings());
  AutoAwayTimer.Enabled := False;
  
  tnid.hIcon := Icon.Handle;
  StrPCopy(tnid.szTip, Format('ICQ - %s ', ['Connection failed']));
  Shell_NotifyIconA(NIM_MODIFY, @tnid);
end;

procedure TMainForm.ICQClient1StatusChange(Sender: TObject; UIN: String;
  Status: Cardinal);
var
  i, img: Integer;
begin
  if ListView1.Items.Count > 0 then
    for i := 0 to ListView1.Items.Count - 1 do
      if ListView1.Items.Item[i].Caption = UIN then
      begin
        case StatusToInt(Status) of
          S_ONLINE     : img := ICON_ONLINE;
          S_AWAY       : img := ICON_AWAY;
          S_DND        : img := ICON_DND;
          S_NA         : img := ICON_NA;
          S_INVISIBLE  : img := ICON_INVISIBLE;
          S_OCCUPIED   : img := ICON_OCCUPIED;
          S_FFC        : img := ICON_FFC;
        else
          img := ICON_OFFLINE;
        end;
        ListView1.Items.Item[i].ImageIndex := img;
        ListView1.Items.Item[i].SubItems[LV_INDEX_STATUS] := IntToStr(Status); //StatusToStr(Status);
        ListView1.Invalidate;
        Exit;
      end;
end;

procedure TMainForm.ICQClient1UserOffline(Sender: TObject; UIN: String);
var
  i: Integer;
  ListItem: TListItem;
begin
  //ListView Filling
  if ListView1.Items.Count > 0 then
    for i := 0 to ListView1.Items.Count - 1 do
      if ListView1.Items.Item[i].Caption = UIN then
      begin
        ListItem := ListView1.Items.Item[i];;
        ListItem.ImageIndex := ICON_OFFLINE;
        //STATUS
        ListItem.SubItems[LV_INDEX_STATUS] := IntToStr(S_OFFLINE);
        ListView1.Invalidate;
        Exit;
      end;
end;

procedure TMainForm.popupListMenuPopup(Sender: TObject);
var
  IsAwayStatus: Boolean;
  itemStatus: DWORD;
begin
  //If user not in Online or Invisible or Offline mode
  //then enable Read away message item in popup menu
  IsAwayStatus := True;
  itemStatus := StrToInt64(ListView1.Selected.SubItems[LV_INDEX_STATUS]);
  case StatusToInt(itemStatus) of
    S_AWAY       : ReadAwayMessage1.ImageIndex := 20;
    S_DND        : ReadAwayMessage1.ImageIndex := 23;
    S_NA         : ReadAwayMessage1.ImageIndex := 22;
    S_OCCUPIED   : ReadAwayMessage1.ImageIndex := 19;
    S_FFC        : ReadAwayMessage1.ImageIndex := 21;
  else  // !IsAwayStatus
    IsAwayStatus := False;
  end;

  if IsAwayStatus then begin
    ReadAwayMessage1.Enabled := True;
    ReadAwayMessage1.Visible := True;
  end
  else begin
    ReadAwayMessage1.Enabled := False;
    ReadAwayMessage1.Visible := False;
  end;
end;

function TMainForm.GetUserInfoIdx(Value: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  if FInfoList.Count > 0 then
    for i := 0 to FInfoList.Count - 1 do
      if TUserInfoForm(FInfoList.Items[i]).FSource = Value then
      begin
        Result := i;
        Exit;
      end;
end;

procedure TMainForm.SetLabel(var lblLabel: TLabel; Enabled: Boolean; URL: Boolean; Text: String);
begin
  if Enabled then
  begin
    case URL of
      True: begin
              lblLabel.Visible := True;
              lblLabel.Enabled := True;
              lblLabel.Caption := Text;
              lblLabel.Enabled := True;
              lblLabel.Font.Color := $00CB8B64;
              lblLabel.Font.Style := [fsUnderline];
              //System HandPoint Cursor
              if IsWinVer2000Plus then
              begin
                Screen.Cursors[NIDC_HAND] := LoadCursor(0, IDC_HAND);
                lblLabel.Cursor := NIDC_HAND;
              end else lblLabel.Cursor := crHandPoint;
            end;

      False: begin
              lblLabel.Caption := Text;
              lblLabel.Enabled := True;
              lblLabel.Font.Color := clWindowText;
              lblLabel.Cursor := crDefault;
             end;
    end;
  end else
  begin
    lblLabel.Caption := '<not specified>';
    lblLabel.Enabled := False;
    lblLabel.Font.Color := clWindowText;
    lblLabel.Cursor := crDefault;
  end;
end;

procedure TMainForm.DoCreateInfoQuery(UIN: String);
var
  i: Integer;
  UIForm: TUserInfoForm;
  ListItem: TListItem;

  function GetICQClientName(Code: Cardinal; ProtoVer: Byte): String;
  var
    sICQName: String;
  begin
    case ProtoVer of
       4: sICQName := 'ICQ98';
       6: sICQName := 'ICQ99 / licq';
       7: sICQName := 'Icq2Go or ICQ2000';
       8: sICQName := 'ICQ2001-2003a';
       9: sICQName := 'ICQ Lite';
      10: sICQName := 'ICQ 2003b';
    else
      sICQName := 'Unknown';
    end;

    case Code of
      $ffffffff : sICQName := 'Miranda IM'; //ff ff ff ff - Miranda
      $7D800404 : sICQName := 'licq';       //7D 80 04 04- licq
                                            //7d xx xx xx - licq; reminder wxxy decimal is version w.x.y.
                                            //00 80 00 00 - licq SSL flag
      $ffffff8f : sICQName := 'StrICQ';     //ff ff ff 8f - StrICQ
      $ffffff42 : sICQName := 'mICQ';       //ff ff ff 42 - mICQ
      $ffffff7f : sICQName := '&&RQ';       //ff ff ff 7f - &RQ
      $ffffffbe : sICQName := 'alicq';      //ff ff ff be - alicq
      $ffffffab : sICQName := 'YSM';        //ff ff ff ab - YSM (does not send version)
    end;

    Result := Format('%d: %s', [ProtoVer, sICQName]);
  end;

  function MirandaVersionToString(Code, ICQVer: Cardinal): String;
  var
    sICQVersion: String;
    aa, bb, cc, dd: Cardinal;
    sAlpha: String;
  begin
    case ICQver of
      $ffffffbe:
        begin
          aa := (Code shr 24) and $FF;
          bb := (Code shr 16) and $FF;
          cc := (Code shr 8) and $FF;
          dd := Code and $FF;
          sICQVersion := Format('%u.%u.%u.%u', [aa, bb, cc, dd]);
        end;

      $ffffffff:
        begin
          if Code = $00000001 then sICQVersion := '0.1.2.0 alpha'
          else begin
            aa := (Code shr 24) and $7F;
            bb := (Code shr 16) and $FF;
            cc := (Code shr 8) and $FF;
            dd := Code and $FF;
            if (Code) and ($80000000) = $80000000 then
              sAlpha := ' alpha'
            else
              sAlpha := '';

            sICQVersion := Format('%u.%u.%u.%u%s', [aa, bb, cc, dd, sAlpha]);
          end;
        end;
    else
      sICQVersion := '';
    end;

    Result := sICQVersion;
  end;

begin
  i := GetUserInfoIdx(UIN);
  if i > -1 then
  begin
    TUserInfoForm(FInfoList.Items[i]).Show;
    Exit;
  end;
  UIForm := TUserInfoForm.Create(nil);
  FInfoList.Add(UIForm);
  with UIForm do
  begin
    if bMyDetails then begin
      SetLabel(lblMyDetailsURL ,True, True, 'Change my details using the ICQ website' );
      bMyDetails := False;
    end;
    UINLabel.Caption := UIN;
    stHandle := stPing.Handle;
    Caption := 'Info about ' + UIN;
    FSource := UIN;
    LocalTimer.Enabled := True;
    if UIN <> IntToStr(ICQClient1.UIN) then
    begin
      if ListView1.Items.Count > 0 then
        for i := 0 to ListView1.Items.Count - 1 do
          if ListView1.Items.Item[i].Caption = UIN then
          begin
            ListItem := ListView1.Items.Item[i];

            sExternalIP := ListItem.SubItems[LV_INDEX_EXTIP];
            sInternalIP := ListItem.SubItems[LV_INDEX_INTIP];
            wPort := StrToInt(ListItem.SubItems[LV_INDEX_PORT]);
            byProtoVer := StrToInt(ListItem.SubItems[LV_INDEX_PROTOVER]);
            byUserCaps := StrToInt(ListItem.SubItems[LV_INDEX_USERCAPS]);
            dwMirandaVer := StrToInt64(ListItem.SubItems[LV_INDEX_MIRANDAVER]);
            dwClient := StrToInt64(ListItem.SubItems[LV_INDEX_CLIENT]);
            dtOnlineTime := StrToFloat(ListItem.SubItems[LV_INDEX_ONLINETIME]);

            dwIdle := StrToInt(ListItem.SubItems[LV_INDEX_IDLE]);

            lbIdle.Caption := Format('%.02u:%.02u',[dwIdle div 60, dwIdle mod 60]);

            UINLabel.Caption := UIN;

            if sExternalIP = '' then SetLabel(ExtIPLabel, False, False, NA)
              else SetLabel(ExtIPLabel, True, False, sExternalIP);

            if sInternalIP = '' then SetLabel(IntIPLabel, False, False, NA)
              else SetLabel(IntIPLabel, True, False, sInternalIP);

            if wPort = 0 then SetLabel(PortLabel, False, False, NA)
              else SetLabel(PortLabel, True, False, IntToStr(wPort));

            if GetICQClientName(dwClient, byProtoVer) = '' then SetLabel(IcqVerLabel, False, False, NA)
              else SetLabel(IcqVerLabel, True, False, GetICQClientName(dwClient, byProtoVer));

            if MirandaVersionToString(dwMirandaVer, dwClient) = '' then SetLabel(MiraVerLabel, False, False, NA)
              else SetLabel(MiraVerLabel, True, False, MirandaVersionToString(dwMirandaVer, dwClient));

            if (dtOnlineTime = 0) or (not ICQClient1.LoggedIn) then SetLabel(OnlineSinceLabel, False, False, NA)
              else SetLabel(OnlineSinceLabel, True, False, ConvertDateTime(dtOnlineTime));

            dwUIN := StrToInt64(UIN);
            DBData.dwUIN := dwUIN;
            DBData.dwStatus := S_OFFLINE;
            DBData.sInternalIP := sInternalIP;
            DBData.sExternalIP := sExternalIP;
            DBData.wPort := wPort;
            DBData.byProtoVer := byProtoVer;
            DBData.byUserCaps := byUserCaps;
            DBData.dtOnlineTime := dtOnlineTime;
            DBData.dwClient := dwClient;
            DBData.dwMirandaVer := dwMirandaVer;
            DBData.sNick := ListItem.SubItems[LV_INDEX_NICK];   //Nick

            MainForm.WriteToDB(dwUIN, DBData);
          end;
        end;
    if Self.ICQClient1.LoggedIn then btnUpdate.Enabled := True
      else btnUpdate.Enabled := False;
    Show;
  end;
end;

procedure TMainForm.ICQClient1UserGeneralInfo(Sender: TObject; UIN,
  NickName, FirstName, LastName, Email, City, State, Phone, Fax, Street,
  Cellular, Zip, Country: String; TimeZone: Byte; PublishEmail: Boolean);
var
  Form: TUserInfoForm;
  i: Integer;
begin
  i := GetUserInfoIdx(UIN);
  if i < 0 then Exit;
  Form := FInfoList.Items[i];
  with Form do
  begin
    UINLabel.Caption := UIN;

    if NickName <> '' then   //Update Nick
      SetLabel(NickNameLabel, True, False, NickName)
    else
      SetLabel(NickNameLabel, False, False, NA);

    if FirstName = '' then SetLabel(FirstNameLabel, False, False, NA)
      else SetLabel(FirstNameLabel, True, False, FirstName);

    if LastName = '' then SetLabel(LastNameLabel, False, False, NA)
      else SetLabel(LastNameLabel, True, False, LastName);

    if PublishEmail then
    begin
      if Email = '' then SetLabel(EmailLabel, False, False, NA)
        else SetLabel(EmailLabel, True, True, Email);
    end
    else begin
      if Email = '' then SetLabel(EmailLabel, False, False, '<not published>')
        else SetLabel(EmailLabel, True, True, Email);
    end;

    if Street = '' then SetLabel(StreetLabel, False, False, NA)
      else SetLabel(StreetLabel, True, False, Street);

    if City = '' then SetLabel(CityLabel, False, False, NA)
      else SetLabel(CityLabel, True, False, City);

    if State = '' then SetLabel(StateLabel, False, False, NA)
      else SetLabel(StateLabel, True, False, State);

    if Country = '' then SetLabel(CountryLabel, False, False, NA)
      else SetLabel(CountryLabel, True, False, Country);

    if Zip = '' then SetLabel(ZipLabel, False, False, NA)
      else SetLabel(ZipLabel, True, False, Zip);

    //
    if TimeZone <> 0 then
    begin
      TimeZoneDiff := ShortInt(TimeZone);
      TimeZoneLabel.Caption := Format(IfThen(TimeZoneDiff > 0, 'GTM -%d:%.02d', 'GTM +%d:%.02d'),[Abs(TimeZoneDiff div 2), (TimeZoneDiff and 1)*30]);
      SetLabel(TimeZoneLabel, True, False, TimeZoneLabel.Caption);
    end else
      SetLabel(TimeZoneLabel, False, False, NA);

    if Cellular = '' then SetLabel(CellularLabel, False, False, NA)
      else SetLabel(CellularLabel, True, False, Cellular);
  end;

  Inc(UserInfoReader);
end;

procedure TMainForm.ICQClient1UserInfoMore(Sender: TObject; UIN: String;
  Age: Word; Gender: Byte; HomePage: String; BirthYear, BirthMonth,
  BirthDay: Word; Lang1, Lang2, Lang3: String);
var
  Form: TUserInfoForm;
  i: Integer;
  nMonth: String[11];
begin
  i := GetUserInfoIdx(UIN);
  if i < 0 then Exit;
  Form := FInfoList.Items[i];
  with Form do
  begin
    if Age = 0 then SetLabel(AgeLabel, False, False, NA)
      else SetLabel(AgeLabel, True, False, IntToStr(Age));

    case Gender of
      GEN_MALE: SetLabel(GenderLabel, True, False, 'Male');

      GEN_FEMALE: SetLabel(GenderLabel, True, False, 'Female');

      else SetLabel(GenderLabel, False, False, NA);
    end;

    if HomePage = '' then SetLabel(HomePageLabel, False, False, NA)
      else SetLabel(HomePageLabel, True, True, HomePage);

    case BirthMonth of
      1  : nMonth := 'January ';
      2  : nMonth := 'February ';
      3  : nMonth := 'March ';
      4  : nMonth := 'April ';
      5  : nMonth := 'May ';
      6  : nMonth := 'June ';
      7  : nMonth := 'July ';
      8  : nMonth := 'August ';
      9  : nMonth := 'September ';
      10 : nMonth := 'October ';
      11 : nMonth := 'November ';
      12 : nMonth := 'December ';
    else
      nMonth := '';
    end;

    if (BirthDay = 0) and (BirthMonth = 0) and (BirthYear = 0) then
      SetLabel(DayOfBirthLabel, False, False, NA)
    else
      SetLabel(DayOfBirthLabel, True, False, nMonth + IntToStr(BirthDay) + ', ' +  IntToStr(BirthYear));

    if Lang1 = '' then SetLabel(Language1Label, False, False, NA)
      else SetLabel(Language1Label, True, False, Lang1);

    if Lang2 = '' then SetLabel(Language2Label, False, False, NA)
      else SetLabel(Language2Label, True, False, Lang2);

    if Lang3 = '' then SetLabel(Language3Label, False, False, NA)
      else SetLabel(Language3Label, True, False, Lang3);
  end;
  Inc(UserInfoReader);  
end;

procedure TMainForm.ICQClient1UserInfoAbout(Sender: TObject; UIN,
  About: String);
var
  Form: TUserInfoForm;
  i: Integer;
begin
  i := GetUserInfoIdx(UIN);
  if i < 0 then Exit;
  Form := FInfoList.Items[i];
  Form.AboutMemo.Text := About;
  Inc(UserInfoReader);  
end;

procedure TMainForm.ICQClient1UserWorkInfo(Sender: TObject; UIN, WCity,
  WState, WPhone, WFax, FAddress, WZip, WCountry, WCompany, WDepartment,
  WPosition, WOccupation, WHomePage: String);
var
  Form: TUserInfoForm;
  i: Integer;
begin
  i := GetUserInfoIdx(UIN);
  if i < 0 then Exit;
  Form := FInfoList.Items[i];
  with Form do
  begin
    if WCity = '' then SetLabel(WCityLabel, False, False, NA)
      else SetLabel(WCityLabel, True, False, WCity);

    if WState = '' then SetLabel(WStateLabel, False, False, NA)
      else SetLabel(WStateLabel, True, False, WState);

    if WPhone = '' then SetLabel(WPhoneLabel, False, False, NA)
      else SetLabel(WPhoneLabel, True, False, WPhone);

    if WFax = '' then SetLabel(WFaxLabel, False, False, NA)
      else SetLabel(WFaxLabel, True, False, WFax);

    if FAddress = '' then SetLabel(WAddressLabel, False, False, NA)
      else SetLabel(WAddressLabel, True, False, FAddress);

    if WZip = '' then SetLabel(WZipLabel, False, False, NA)
      else SetLabel(WZipLabel, True, False, WZip);

    if WCountry = '' then SetLabel(WCountryLabel, False, False, NA)
      else SetLabel(WCountryLabel, True, False, WCountry);

    if WCompany = '' then SetLabel(WCompanyLabel, False, False, NA)
      else SetLabel(WCompanyLabel, True, False, WCompany);

    if WDepartment = '' then SetLabel(WDepartmentLabel, False, False, NA)
      else SetLabel(WDepartmentLabel, True, False, WDepartment);

    if WPosition = '' then SetLabel(WPositionLabel, False, False, NA)
      else SetLabel(WPositionLabel, True, False, WPosition);

    if WOccupation = '' then SetLabel(WOccupationLabel, False, False, NA)
      else SetLabel(WOccupationLabel, True, False, WOccupation);

    if WHomePage = '' then SetLabel(WHomePageLabel, False, False, NA)
      else SetLabel(WHomePageLabel, True, True, WHomePage);
  end;
  Inc(UserInfoReader);
end;

procedure TMainForm.ICQClient1UserInfoInterests(Sender: TObject; UIN: String;
  Interests: TStringList);
var
  Form: TUserInfoForm;
  i: Integer;
  ListItem: TListItem;
begin
  i := GetUserInfoIdx(UIN);
  if i < 0 then Exit;
  Form := FInfoList.Items[i];
  Form.InterestsView.Items.Clear;
  if Interests.Count > 0 then
    for i := 0 to Interests.Count - 1 do
    begin
      if (ExtractName(Interests.Strings[i]) <> '') and (ExtractValue(Interests.Strings[i]) <> '') then
      begin
        ListItem := Form.InterestsView.Items.Add;
        ListItem.Caption := ExtractName(Interests.Strings[i]);
        ListItem.SubItems.Add(ExtractValue(Interests.Strings[i]));
      end;
    end;
  Interests.Free;
  Inc(UserInfoReader);  
end;

procedure TMainForm.UserInfo1Click(Sender: TObject);
begin
  SetCapture(ListView1.Handle);

  if ListView1.Selected <> nil then
    DoCreateInfoQuery(ListView1.Selected.Caption);
end;


procedure TMainForm.RemoveContact1Click(Sender: TObject);
var
  dwUIN: DWORD;
begin
  SetCapture(ListView1.Handle);
  if ListView1.Selected <> nil then
  begin
    dwUIN := StrToInt64(ListView1.Selected.Caption);
    ICQClient1.RemoveContact(dwUIN);
    ListView1.Items.Delete(ListView1.Selected.Index);

    DeleteFromDB(dwUIN);
  end;
end;

procedure TMainForm.ICQClient1UserFound(Sender: TObject; UIN, Nick, FirstName,
  LastName, Email: String; Status: Word; Gender, Age: Byte;
  SearchComplete, Authorize: Boolean);
var
  ListItem: TListItem;
  S: String;
begin
  if UserSearchForm <> nil then
  begin
    ListItem := UserSearchForm.ListView1.Items.Add;
    if Status = 1 then {Online}
      ListItem.ImageIndex := ICON_ONLINE
    else
      ListItem.ImageIndex := ICON_OFFLINE;
    ListItem.Caption := Nick;
    ListItem.SubItems.Add(FirstName);
    ListItem.SubItems.Add(LastName);
    ListItem.SubItems.Add(Email);
    ListItem.SubItems.Add(UIN);
    S := '';
    if Gender <> 0 then
      if Gender = GEN_FEMALE then
        S := 'F'
      else if Gender = GEN_MALE then
        S := 'M';
    if Age <> 0 then
      if S = '' then
        S := IntToStr(Age)
      else
        S := S + '-' + IntToStr(Age);
    ListItem.SubItems.Add(S);
    if Authorize then ListItem.SubItems.Add('Yes')
      else ListItem.SubItems.Add('No');

    if SearchComplete then
    begin
      UserSearchForm.Button1.Caption := 'Search';
      UserSearchForm.StatusBar1.Panels[0].Text := 'Search complete';
    end;
  end;
end;

procedure TMainForm.ICQClient1UserNotFound(Sender: TObject);
begin
  if UserSearchForm <> nil then
  begin
    UserSearchForm.Button1.Caption := 'Search';
    UserSearchForm.StatusBar1.Panels[0].Text := 'User not found';
  end;
end;

{Needed for debug only! It's not required implementing this event in your developments.}
procedure TMainForm.ICQClient1PktParse(Sender: TObject; Buffer: Pointer;
  BufLen: Cardinal; Incoming: Boolean);
var
  p: PRawPkt;
  ListItem: TListItem;
  ad: String;
  f, s: Word;
begin
  GetMem(p, SizeOf(TRawPkt));
  Move(Buffer^, p^, BufLen);
  p^.Len := 0;
  PktDumpForm.FPktList.Add(p);
  {ListItem}
  ListItem := PktDumpForm.ListView1.Items.Add;
  if Incoming then
    ListItem.ImageIndex := 5
  else
    ListItem.ImageIndex := 0;
  ListItem.SubItems.Add(IntToStr(PFlapHdr(Buffer)^.ChID));
  ListItem.SubItems.Add(IntToStr(Swap16(PFlapHdr(Buffer)^.DataLen)));
  ListItem.SubItems.Add('0x' + IntToHex(Swap16(PFlapHdr(Buffer)^.Seq), 4));

  if PFlapHdr(Buffer)^.ChID <> 2 then
  begin
    ListItem.SubItems.Add('');
    if (PFlapHdr(Buffer)^.ChID = 1) and (Swap16(PFlapHdr(Buffer)^.DataLen) > 35) then
    begin
      if PChar(LongWord(Buffer) + BufLen - 2)^ + PChar(LongWord(Buffer) + BufLen - 1)^ = 'us' then
        ListItem.SubItems.Add('CLI_IDENT')
      else if Swap16(PFlapHdr(Buffer)^.DataLen) = 264 then
          ListItem.SubItems.Add('CLI_COOKIE')
    end else
    if (PFlapHdr(Buffer)^.ChID = 4) and (Swap16(PFlapHdr(Buffer)^.DataLen) = 0) then
    begin
      if Incoming then
        ListItem.SubItems.Add('SRV_GOODBYE')
      else
        ListItem.SubItems.Add('CLI_GOODBYE');
    end else if (PFlapHdr(Buffer)^.ChID = 4) and Incoming then
    begin
      if (Swap16(PFlapHdr(Buffer)^.DataLen) > 270) then
        ListItem.SubItems.Add('SRV_COOKIE')
      else
        ListItem.SubItems.Add('SRV_GOODBYE')
    end
    else if Swap16(PFlapHdr(Buffer)^.DataLen) = 4 then
    begin
      if Incoming then
        ListItem.SubItems.Add('SRV_HELLO')
      else
        ListItem.SubItems.Add('CLI_HELLO');
    end else
      ListItem.SubItems.Add('none');
  end else
  begin
    ad := '';
    f := Swap16(PSnacHdr(LongWord(Buffer) + TFLAPSZ)^.Family);
    s := Swap16(PSnacHdr(LongWord(Buffer) + TFLAPSZ)^.SubType);
    if (f = $15) and ((s = $03) or (s = $02)) then
      ad := ' [' + SrvMetaToStr(PWord(LongWord(Buffer) + TFLAPSZ + TSNACSZ + 10)^, PWord(LongWord(Buffer) + TFLAPSZ + TSNACSZ + 14)^) + ']';
    ListItem.SubItems.Add('SNAC(' + IntToHex(f, 2) + ',' + IntToHex(s, 2) + ')' + ad);
    ListItem.SubItems.Add(SnacToStr(f, s));
  end;
end;

procedure TMainForm.ICQClient1UserInfoBackground(Sender: TObject;
  UIN: String; Pasts, Affiliations: TStringList);
var
  Form: TUserInfoForm;
  i: Integer;
  ListItem: TListItem;
begin
  i := GetUserInfoIdx(UIN);
  if i < 0 then Exit;
  Form := FInfoList.Items[i];
  if Form = nil then Exit;
  Form.PastsListView.Items.Clear;
  Form.AffiliationsListView.Items.Clear;
  if Pasts.Count > 0 then
    for i := 0 to Pasts.Count - 1 do
    begin
      if (ExtractName(Pasts.Strings[i]) <> '') and (ExtractName(Pasts.Strings[i]) <> '') then
      begin
        ListItem := Form.PastsListView.Items.Add;
        ListItem.Caption := ExtractName(Pasts.Strings[i]);
        ListItem.SubItems.Add(ExtractValue(Pasts.Strings[i]));
      end;
    end;
  Pasts.Free;
  if Affiliations.Count > 0 then
    for i := 0 to Affiliations.Count - 1 do
    begin
      if (ExtractName(Affiliations.Strings[i]) <> '') and (ExtractValue(Affiliations.Strings[i]) <> '') then
      begin
        ListItem := Form.AffiliationsListView.Items.Add;
        ListItem.Caption := ExtractName(Affiliations.Strings[i]);
        ListItem.SubItems.Add(ExtractValue(Affiliations.Strings[i]));
      end;
    end;
  Affiliations.Free;
  Inc(UserInfoReader);  
end;

procedure TMainForm.ICQClient1ServerListRecv(Sender: TObject;
  SrvContactList: TList);
var
  i: Word;
  UserInfo: TUINEntry;
  ListItem: TListItem;
begin
  if SrvContactList.Count > 0 then
    for i := 0 to SrvContactList.Count - 1 do
    begin
      UserInfo := PUINEntry(SrvContactList.Items[i])^;
      if UserInfo.CType = U_VISIBLE_LIST then
        ICQClient1.VisibleList.Add(IntToStr(UserInfo.UIN))
      else if UserInfo.CType = U_INVISIBLE_LIST then
        ICQClient1.InvisibleList.Add(IntToStr(UserInfo.UIN))
      else if UserInfo.CType = U_NORMAL then
      begin
        if ICQClient1.AddContact(UserInfo.UIN) then
        begin
          ListItem := ListView1.Items.Add;
          ListItem.ImageIndex := ICON_OFFLINE;
          ListItem.Caption := IntToStr(UserInfo.UIN);
        end;
      end;
    end;
  ICQClient1.DestroyUINList(SrvContactList);
end;

procedure TMainForm.ReadAwayMessage1Click(Sender: TObject);
var
  ReqStatus: Byte;
  itemStatus: DWORD;
begin
  SetCapture(ListView1.Handle);
  if ListView1.Selected = nil then Exit;

  itemStatus := StrToInt64(ListView1.Selected.SubItems[LV_INDEX_STATUS]);
  case StatusToInt(itemStatus) of
    S_AWAY       : ReqStatus := GET_AWAY;
    S_DND        : ReqStatus := GET_DND;
    S_NA         : ReqStatus := GET_NA;
    S_OCCUPIED   : ReqStatus := GET_OCCUPIED;
    S_FFC        : ReqStatus := GET_FFC;
  else  //Unknown status
    Exit;
  end;

  Randomize;
  ICQClient1.RequestAwayMsg(StrToInt64(ListView1.Selected.Caption), Random($FFFF), ReqStatus);
end;

procedure TMainForm.ICQClient1AutoMsgResponse(Sender: TObject; UIN: String;
  ID: Word; RespStatus: Byte; Msg: String);
begin
  with TAutoAwayForm.Create(Self) do
  begin
    Caption := UIN + ': away-message';
    AwayMemo.Lines.Text := Msg;
    Show;
  end;
end;

procedure TMainForm.ICQClient1AdvancedMsgAck(Sender: TObject; UIN: String;
  ID: Word; AcceptType: Byte; AcceptMsg: String);
begin
  with TAutoAwayForm.Create(Self) do
  begin
    Caption := UIN + ': adv-away-message';
    AwayMemo.Lines.Text := AcceptMsg;
    Show;
  end;
end;

{Needed for debug only! It's not required implementing this event in your developments.}
procedure TMainForm.ICQClient1PktDirectParse(Sender: TObject;
  Buffer: Pointer; BufLen: Cardinal; Incoming: Boolean);
var
  p: PRawPkt;
  ListItem: TListItem;
begin
  GetMem(p, SizeOf(TRawPkt));
  Move(Buffer^, p^, BufLen);
  p^.Len := BufLen;
  PktDumpForm.FPktList.Add(p);
  {ListItem}
  ListItem := PktDumpForm.ListView1.Items.Add;
  if Incoming then
    ListItem.ImageIndex := 5
  else
    ListItem.ImageIndex := 0;
  ListItem.SubItems.Add('DIRECT');
  ListItem.SubItems.Add(IntToStr(BufLen));
  ListItem.SubItems.Add('');
  ListItem.SubItems.Add('0x' + IntToHex(PByte(LongWord(Buffer) + 2)^, 2));
  ListItem.SubItems.Add(PeerCmdToStr(PByte(LongWord(Buffer) + 2)^));
  //Save incoming dumps to a file
  {if Incoming then LogText('dc.txt', 'Incoming packet! Command: ' + PeerCmdToStr(PByte(LongWord(Buffer) + 2)^) + ', length: ' + IntToStr(BufLen - 2) + #13#10 + DumpPacket(Buffer, BufLen) + #13#10#13#10)
  else LogText('dc.txt', 'Outgoing packet! Command: ' + PeerCmdToStr(PByte(LongWord(Buffer) + 2)^) + ', length: ' + IntToStr(BufLen - 2) + #13#10 + DumpPacket(Buffer, BufLen) + #13#10#13#10);}
end;

procedure TMainForm.ICQClient1Error(Sender: TObject; ErrorType: TErrorType;
  ErrorMsg: String);
begin
//  ShowMessage(ErrorMsg); {Handle as you need}
end;

procedure TMainForm.ICQClient1NewUINRefused(Sender: TObject);
begin
  with TUserRegForm.Create(Self) do
  begin
    Memo1.Lines.Text := 'Sorry, server cannot give you a new UIN, possible'
      + ' you are trying to register numbers too often.';
    Show;
  end;
end;

procedure TMainForm.ICQClient1NewUINRegistered(Sender: TObject;
  UIN: String);
begin
  with TUserRegForm.Create(Self) do
  begin
    Memo1.Lines.Text := 'Success! New UIN was granted: ' + #13#10 + UIN;
    Show;
  end;
end;

procedure TMainForm.ICQClient1UnregisterBadPassword(Sender: TObject);
begin
  with TUserRegForm.Create(Self) do
  begin
    Memo1.Lines.Text := 'Sorry, server can''t unregister UIN, possible'
      + ' you''ve entered a bad password.';
    Show;
  end;
end;

procedure TMainForm.ICQClient1UnregisterOk(Sender: TObject);
begin
  with TUserRegForm.Create(Self) do
  begin
    Memo1.Lines.Text := Format('Your UIN: %u was successfully unregistred!', [ICQClient1.UIN]);
    Show;
  end;
end;

procedure TMainForm.ICQClient1InfoChanged(Sender: TObject;
  InfoType: TInfoType; ChangedOk: Boolean);
begin
  with TUserRegForm.Create(Self) do
  begin
    if InfoType = INFO_PASSWORD then
    begin
      case ChangedOk of
        True: Memo1.Lines.Text := 'Your password was successfully changed!';
        False: Memo1.Lines.Text := 'Sorry, server can''t change your password.';
      end;
    end;
    Show;
  end;
end;

procedure TMainForm.StatusBar1DrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
begin
  StatusBar1.Canvas.FillRect(Rect);
  StatusBar1.Canvas.Draw(Rect.Left, Rect.Top, Icon);
  StatusBar1.Canvas.TextOut(20, 5, StatusBar1.Panels[0].Text);
end;

procedure TMainForm.ICQClient1OfflineMsgRecv(Sender: TObject;
  DateTime: TDateTime; Msg, UIN: String);
var
  i: Integer;
  ListItem: TListItem;
begin
  with TRecvMsgForm.Create(nil) do
  begin
    RichEdit1.Text := Msg;
    FNick := UIN;
    FSource := UIN;
    FDateTime := ConvertDateTime(LocalTime(DateTime));
    lblDateTime.Caption := Format('Received at: %s', [FDateTime]);
    Self.AdvIconList.GetIcon(7, Image1.Picture.Icon);
    lblUIN.Caption := UIN;
    if ListView1.Items.Count > 0 then
      for i := 0 to ListView1.Items.Count - 1 do
        if ListView1.Items.Item[i].Caption = UIN then
        begin
          ListItem := ListView1.Items.Item[i];
          FNick := ListItem.SubItems[LV_INDEX_NICK];   //Nick
          Break;
        end;
    if FNick = UIN then
      Caption := Format('Message from: %s', [UIN])
    else
      Caption := Format('Message from: %s (%s)', [FNick, UIN]);
    Show;
  end;
end;

procedure TMainForm.ICQClient1OfflineURLRecv(Sender: TObject;
  DateTime: TDateTime; Description, URL, UIN: String);
var
  i: Integer;
  ListItem: TListItem;
begin
  with TRecvURLForm.Create(nil) do
  begin
    EditURL.Text := URL;
    memoURLDscrb.Text := Description;
    FNick := UIN;
    FSource := UIN;
    FDateTime := ConvertDateTime(LocalTime(DateTime));
    lblDateTime.Caption := Format('Received at: %s', [FDateTime]);
    Self.AdvIconList.GetIcon(7, Image1.Picture.Icon);
    lblUIN.Caption := UIN;
    if ListView1.Items.Count > 0 then
      for i := 0 to ListView1.Items.Count - 1 do
        if ListView1.Items.Item[i].Caption = UIN then
        begin
          ListItem := ListView1.Items.Item[i];
          FNick := ListItem.SubItems[LV_INDEX_NICK];   //Nick
          Break;
        end;
    if FNick = UIN then
      Caption := Format('URL from: %s', [UIN])
    else
      Caption := Format('URL from: %s (%s)', [FNick, UIN]);
    Show;
  end;
end;

procedure TMainForm.ICQClient1URLRecv(Sender: TObject; Description, URL,
  UIN: String);
var
  i: Integer;
  ListItem: TListItem;
begin
  with TRecvURLForm.Create(nil) do
  begin
    EditURL.Text := URL;
    memoURLDscrb.Text := Description;
    FNick := UIN;
    FSource := UIN;
    FDateTime := Format('Received at: %s', [ConvertDateTime(Now)]);
    lblDateTime.Caption := FDateTime;
    Self.AdvIconList.GetIcon(7, Image1.Picture.Icon);
    lblUIN.Caption := UIN;
    if ListView1.Items.Count > 0 then
      for i := 0 to ListView1.Items.Count - 1 do
        if ListView1.Items.Item[i].Caption = UIN then
        begin
          ListItem := ListView1.Items.Item[i];
          FNick := ListItem.SubItems[LV_INDEX_NICK];   //Nick
          Break;
        end;
    if FNick = UIN then
      Caption := Format('URL from: %s', [UIN])
    else
      Caption := Format('URL from: %s (%s)', [FNick, UIN]);
    Show;
  end;
end;

procedure TMainForm.ICQClient1MessageRecv(Sender: TObject; Msg, UIN: String);
var
  i: Integer;
  ListItem: TListItem;
begin
  with TRecvMsgForm.Create(nil) do
  begin
    RichEdit1.Text := Msg;
    FNick := UIN;
    FSource := UIN;
    FDateTime := Format('Received at: %s', [ConvertDateTime(Now)]);
    lblDateTime.Caption := FDateTime;
    Self.AdvIconList.GetIcon(7, Image1.Picture.Icon);
    lblUIN.Caption := UIN;
    if ListView1.Items.Count > 0 then
      for i := 0 to ListView1.Items.Count - 1 do
        if ListView1.Items.Item[i].Caption = UIN then
        begin
          ListItem := ListView1.Items.Item[i];
          FNick := ListItem.SubItems[LV_INDEX_NICK];   //Nick
          Break;
        end;
    if FNick = UIN then
      Caption := Format('Message from: %s', [UIN])
    else
      Caption := Format('Message from: %s (%s)', [FNick, UIN]);
    Show;
  end;
end;

procedure TMainForm.ICQClient1OnlineInfo(Sender: TObject; UIN: String;
  Port: Word; InternalIP, ExternalIP: String; OnlineTime: TDateTime;
  Idle: Word; ICQVersion, MirandaVersion: Cardinal; ProtoVer,
  UserCaps: Byte);
var
  i: Integer;
  ListItem: TListItem;
begin
  //ListView Filling
  if ListView1.Items.Count > 0 then
    for i := 0 to ListView1.Items.Count - 1 do
      if ListView1.Items.Item[i].Caption = UIN then
      begin
        ListItem := ListView1.Items.Item[i];

        if InternalIP = '0.0.0.0' then      //Internal IP
          ListItem.SubItems[LV_INDEX_INTIP] := ''
        else
          ListItem.SubItems[LV_INDEX_INTIP] := InternalIP;

        if ExternalIP = '0.0.0.0' then      //External IP
          ListItem.SubItems[LV_INDEX_EXTIP] := ''
        else
          ListItem.SubItems[LV_INDEX_EXTIP] := ExternalIP;

        ListItem.SubItems[LV_INDEX_PORT] := IntToStr(Port);         //Port
        ListItem.SubItems[LV_INDEX_PROTOVER] := IntToStr(ProtoVer); //ProtoVer
        ListItem.SubItems[LV_INDEX_USERCAPS] := IntToStr(UserCaps); //UserCaps

        ListItem.SubItems[LV_INDEX_ONLINETIME] := FloatToStr(LocalTime(OnlineTime));

        ListItem.SubItems[LV_INDEX_CLIENT] := IntToStr(ICQVersion);         //GetICQClientName(ICQver); //ICQ Client
        ListItem.SubItems[LV_INDEX_MIRANDAVER] := IntToStr(MirandaVersion); //MirandaVersionToString(MirandaVer); //Miranda version

        ListItem.SubItems[LV_INDEX_IDLE] := IntToStr(Idle);  //Idle

        ListView1.Invalidate;

        Exit;
      end;
end;

procedure TMainForm.ICQClient1AddedYou(Sender: TObject; UIN: String);
begin
  with TInfoMsgsForm.Create(Self) do
  begin
    Caption := UIN + ': Added you...';
    LabMsg.Caption := Format('You were added by %s.', [UIN]);
    FSource := StrToInt64(UIN);
    btnDeny.Enabled := False;
    btnDeny.Visible := False;
    btnAccept.Enabled := False;
    btnAccept.Visible := False;
    Show;
  end;
end;

procedure TMainForm.ICQClient1AuthResponse(Sender: TObject; UIN: String;
  Granted: Boolean; Reason: String);
begin
  with TInfoMsgsForm.Create(Self) do
  begin
    Caption := UIN + ': Authorization response...';
    case Granted of
      True:
        LabMsg.Caption := Format('You were authorized by %s.', [UIN]);

      False:
        LabMsg.Caption := Format('Authorization wasn''t granted by %s.' +
          #13#10 + #13#10 + '%s', [UIN, Reason]);
    end;
    FSource := StrToInt64(UIN);
    btnDeny.Enabled := False;
    btnDeny.Visible := False;
    btnAccept.Enabled := False;
    btnAccept.Visible := False;
    Show;
  end;
end;

procedure TMainForm.ICQClient1AuthRequest(Sender: TObject; UIN,
  Reason: String);
begin
  with TInfoMsgsForm.Create(Self) do
  begin
    Caption := UIN + ': Authorization request...';
    LabMsg.Caption := Format('Authorization request from %s.' +
      #13#10 + #13#10 + '%s', [UIN, Reason]);
    FSource := StrToInt64(UIN);
    btnClose.Enabled := False;
    btnClose.Visible := False;
    Show;
  end;
end;

procedure TMainForm.ICQClient1AuthorizationChangedOk(Sender: TObject);
begin
  with TInfoMsgsForm.Create(Self) do
  begin
    Caption := 'Authorization settings...';
    case bSetAuthorization of
      True: LabMsg.Caption := #13#10 + 'Authorization was set!' + #13#10;

      False: LabMsg.Caption := #13#10 + 'Authorization was unset!' + #13#10;
    end;
    btnDeny.Enabled := False;
    btnDeny.Visible := False;
    btnAccept.Enabled := False;
    btnAccept.Visible := False;
    sbAddUser.Visible := False;
    sbUserInfo.Visible := False;
    Show;
  end;
end;

procedure TMainForm.ListView1DrawItem(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);
var
  IconHeight: Integer;
  FontHeight: Integer;
  r: TRect;
  DC: HDC;
  img: Integer;
  Fg, Style: Cardinal;
  itemStatus: DWORD;
begin
  Sender.Canvas.Font.Name := 'Tahoma';

  r := Rect;
  r.BottomRight.X :=  Sender.Canvas.TextWidth(Item.SubItems[LV_INDEX_NICK]) + 23 + 2;
  r.TopLeft.X := 21;

  itemStatus := StrToInt64(Item.SubItems[LV_INDEX_STATUS]);

  case StatusToInt(itemStatus) of
    S_ONLINE     : img := ICON_ONLINE;
    S_AWAY       : img := ICON_AWAY;
    S_DND        : img := ICON_DND;
    S_NA         : img := ICON_NA;
    S_INVISIBLE  : img := ICON_INVISIBLE;
    S_OCCUPIED   : img := ICON_OCCUPIED;
    S_FFC        : img := ICON_FFC;
  else
    img := ICON_OFFLINE;
  end;

  if not ICQClient1.LoggedIn then img := ICON_OFFLINE;

  if (TestHit <> nil) and (TestHit = Item) and (not (odSelected in State)) then begin
    Sender.Canvas.Brush.Color := clWhite;
    Sender.Canvas.Font.Color := clSilver;
    Fg := GetSysColor(COLOR_MENU);
    Style := ILD_BLEND25;
  end
  else begin
    if odSelected in State then
    begin
      Sender.Canvas.Brush.Color := clNavy;
      Sender.Canvas.Font.Color := clWhite;
      Fg := GetSysColor(COLOR_MENU);
      Style := ILD_BLEND50;
    end else
    begin
      Sender.Canvas.Brush.Color := clWhite;
      Sender.Canvas.Font.Color := clBlue;
      if img = ICON_OFFLINE then
        Sender.Canvas.Font.Color := clRed;
      Fg := CLR_NONE;
      Style := ILD_NORMAL;
    end;
  end;

  if (TestHit <> OldTestHit) and (OldTestHit <> nil) and (OldTestHit = Item) then begin
    if odSelected in State then begin
      Sender.Canvas.Brush.Color := clNavy;
      Sender.Canvas.Font.Color := clWhite;
      Fg := GetSysColor(COLOR_MENU);
      Style := ILD_BLEND50;
    end
    else begin
      Sender.Canvas.Brush.Color := clWhite;
      Sender.Canvas.Font.Color := clBlue;
      if img = ICON_OFFLINE then
        Sender.Canvas.Font.Color := clRed;
      Fg := CLR_NONE;
      Style := ILD_NORMAL;
    end;
  end;

  Sender.Canvas.FillRect(r);

  IconHeight := 16;
  FontHeight := Sender.Canvas.TextHeight('Xy');

  DC := GetDC(Item.Handle);

  ImageList_DrawEx(hImlMenuIcons, img, DC,
    Rect.Left + 3, Rect.Top + (LVItemHeight - IconHeight) div 2,
     0, 0, CLR_NONE, Fg, Style);

  ReleaseDC(Item.Handle, DC);

  Sender.Canvas.TextOut(Rect.left + 23, Rect.Top + (LVItemHeight - FontHeight) div 2, Item.SubItems[LV_INDEX_NICK]);
end;

procedure TMainForm.findcontactClick(Sender: TObject);
begin
  if not Assigned(UserSearchForm) then
    UserSearchForm := TUserSearchForm.Create(Application);
  UserSearchForm.Show;
end;

procedure TMainForm.mydetailsClick(Sender: TObject);
begin
  bMyDetails := True;
  DoCreateInfoQuery(IntToStr(ICQClient1.UIN));
end;

procedure TMainForm.StatusBar1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  GetCursorPos(P);
  if Button = mbLeft then
    popupStatus.Popup(P.X - X, P.Y - Y - 19
                      * (popupStatus.Items.Count));
  if Button = mbRight then popupMenu.Popup(P.X, P.Y);
end;

procedure TMainForm.Exit2Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.SetMyAuthorization2Click(Sender: TObject);
begin
  if not ICQClient1.LoggedIn then begin
    MessageBox(Self.Handle,'You cannot perform this action when you are offline.','Error',MB_OK);
    Exit;
  end;

  ICQClient1.SetAuthorization(True, True);
  bSetAuthorization := True;
end;

procedure TMainForm.UnsetMyAuthorization2Click(Sender: TObject);
begin
  if not ICQClient1.LoggedIn then begin
    MessageBox(Self.Handle,'You cannot perform this action when you are offline.','Error',MB_OK);
    Exit;
  end;

  ICQClient1.SetAuthorization(False, False);
  bSetAuthorization := False;
end;

procedure TMainForm.PacketDump1Click(Sender: TObject);
begin
  PktDumpForm.Show;
end;

procedure TMainForm.LoadContactListfromServer1Click(Sender: TObject);
begin
  ICQClient1.RequestContactList;
end;

procedure TMainForm.RegisterNewUIN2Click(Sender: TObject);
begin
  if not Assigned(UserRegNewForm) then
    UserRegNewForm := TUserRegNewForm.Create(Application);
  UserRegNewForm.Show;
end;

procedure TMainForm.UnregisterUIN1Click(Sender: TObject);
begin
  if not Assigned(UserUnRegForm) then
    UserUnRegForm := TUserUnRegForm.Create(Application);
  UserUnRegForm.Show;
end;

procedure TMainForm.ChangeMyPassword1Click(Sender: TObject);
begin
  if not Assigned(PasswdForm) then
    PasswdForm := TPasswdForm.Create(Application);
  PasswdForm.Show;
end;

procedure TMainForm.Message1Click(Sender: TObject);
begin
  SetCapture(ListView1.Handle);
  if ListView1.Selected = nil then Exit;
  with TSendMsgForm.Create(Self) do
  begin
    FDest := ListView1.Selected.Caption;
    FName := ListView1.Selected.SubItems[LV_INDEX_NICK];
    Caption := Format('Send message to %s (%s)', [FName, FDest]);
    Self.AdvIconList.GetIcon(7, Image1.Picture.Icon);
    lblUIN.Caption := FDest;
    Show;
  end;
end;

procedure TMainForm.WebPageAddressURL1Click(Sender: TObject);
begin
  SetCapture(ListView1.Handle);
  if ListView1.Selected = nil then Exit;
  with TSendURLForm.Create(Self) do
  begin
    FDest := ListView1.Selected.Caption;
    FName := ListView1.Selected.SubItems[LV_INDEX_NICK];
    Caption := Format('Send URL to %s (%s)', [FName, FDest]);
    Self.AdvIconList.GetIcon(7, Image1.Picture.Icon);
    lblUIN.Caption := FDest;
    Show;
  end;
end;

procedure TMainForm.ICQClient1SMSAck(Sender: TObject; Source, Network,
  MsgId: String; Deliverable: Boolean);
var
  SysMessage: String;
begin
  with TUserRegForm.Create(Self) do
  begin
    SysMessage := Format('SMS was added to the cellurar provider''s que'
      + #13#10 + 'Source: %s' + #13#10
      + 'Network: %s' + #13#10 + 'ID: %s', [Source, Network, MsgId]);
    case Deliverable of
      True: SysMessage := SysMessage + #13#10 + 'and will be soon delivered!';
      False: SysMessage := SysMessage + #13#10 + 'and couldn''t to be delivered!';
    end;
    Memo1.Lines.Text := SysMessage;
    Show;
  end;
end;

procedure TMainForm.ICQClient1SMSRefused(Sender: TObject);
begin
  with TUserRegForm.Create(Self) do
  begin
    Memo1.Lines.Text := 'SMS cannot be delivered.'
      + ' Possible you''ve set wrong cellular number.';
    Show;
  end;
end;

procedure TMainForm.ICQClient1SMSReply(Sender: TObject; Source, SmsSender,
  Time, Text: String);
begin
  with TRecvSMSForm.Create(Self) do
  begin
    editPhone.Text := SmsSender;
    memoSMSText.Lines.Text := Format('Source: %s' + #13#10 + #13#10 +
      'Text: %s', [Source, Text]);
    lblDateTime.Caption := Format('Received at: %s', [Time]);
    Show;
  end;
end;

procedure TMainForm.sendsmsClick(Sender: TObject);
begin
  with TSendSMSForm.Create(Self) do
    Show;
end;

procedure TMainForm.ListView1ContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  TestItem: TListItem;
  CursorPos: TPoint;
begin
  //Popup when ContactList item selected
  TestItem := ListView1.GetItemAt(MousePos.X, MousePos.Y);
  GetCursorPos(CursorPos);
  if TestItem <> nil then popupListMenu.Popup(CursorPos.X, CursorPos.Y)
end;

procedure TMainForm.eICQWebsite1Click(Sender: TObject);
begin
  OpenURL('http://eraser.wz.cz/', False);
end;

procedure TMainForm.Exit3Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.HideShow1Click(Sender: TObject);
begin
  if Self.Visible then
    Self.Hide
  else begin
    SetWindowPos(Handle, OnTop, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
    Self.Show;
    Windows.SetForegroundWindow(Handle);
  end;
end;

procedure TMainForm.StatusClick(Sender: TObject);
var
  dwStatus: DWORD;
  DBSet: _DBSETTINGS;
begin
  if Sender = Offline2 then
  begin
    ICQClient1.LogOff;
    IconList.GetIcon(ICON_OFFLINE, Icon);
    StatusBar1.Panels[0].Text := 'Offline';
    //ReCreateContactList;
    RepaintOfflineCL;

    tnid.hIcon := Icon.Handle;
    StrPCopy(tnid.szTip, Format('ICQ - %s ', ['Offline']));
    Shell_NotifyIconA(NIM_MODIFY, @tnid);

    dwLastStatus := S_OFFLINE;
  end else
  begin
    if Sender = Online2 then
      dwStatus := S_ONLINE
    else if Sender = Away2 then
      dwStatus := S_AWAY
    else if Sender = NA2 then
      dwStatus := S_NA
    else if Sender = Occupied2 then
      dwStatus := S_OCCUPIED
    else if Sender = DND2 then
      dwStatus := S_DND
    else if Sender = Freeforchat1 then
      dwStatus := S_FFC
    else
      dwStatus := S_INVISIBLE;

    DoStatusChange(dwStatus or fDCFlag);

    dwLastStatus := dwStatus;
  end;

  with Sender as TMenuItem do
    Default := True;

  DBSet := ReadSettings();
  DBSet.dwLastStatus := dwLastStatus;
  WriteSettings(DBSet);
end;

procedure TMainForm.ListView1Edited(Sender: TObject; Item: TListItem;
  var S: String);
var
  dwUIN: DWORD;
begin
  dwUIN := StrToInt64(sUIN);
  if S = '' then
    Item.SubItems[LV_INDEX_NICK] := sUIN
  else
    Item.SubItems[LV_INDEX_NICK] := S;
  S := sUIN;

  DBData.dwUIN := dwUIN;
  DBData.dwStatus := S_OFFLINE;
  DBData.sInternalIP := Item.SubItems[LV_INDEX_INTIP];
  DBData.sExternalIP := Item.SubItems[LV_INDEX_EXTIP];
  DBData.wPort := StrToInt(Item.SubItems[LV_INDEX_PORT]);
  DBData.byProtoVer := StrToInt(Item.SubItems[LV_INDEX_PROTOVER]);
  DBData.byUserCaps := StrToInt(Item.SubItems[LV_INDEX_USERCAPS]);
  DBData.dtOnlineTime := StrToFloat(Item.SubItems[LV_INDEX_ONLINETIME]);
  DBData.dwClient := StrToInt64(Item.SubItems[LV_INDEX_CLIENT]);
  DBData.dwMirandaVer := StrToInt64(Item.SubItems[LV_INDEX_MIRANDAVER]);
  DBData.sNick := Item.SubItems[LV_INDEX_NICK];

  MainForm.WriteToDB(dwUIN, DBData);
end;

procedure TMainForm.ListView1Editing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  if bEditAble then AllowEdit := True
    else AllowEdit := False;

  bEditAble := False;
{
  Item.Caption := sUIN;
}
end;

procedure TMainForm.Rename1Click(Sender: TObject);
begin
  SetCapture(ListView1.Handle);
  bEditAble := True;
  sUIN := ListView1.Items.Item[ListView1.Selected.Index].Caption;
  ListView1.Items.Item[ListView1.Selected.Index].Caption := ListView1.Items.Item[ListView1.Selected.Index].SubItems[LV_INDEX_NICK];
  SendMessage(ListView1.Handle, LVM_EDITLABEL, ListView1.Selected.Index, 0);
end;

function IsWorkstationLocked(): Boolean;
var
  hd: HDESK;
  buf: array[0..MAX_PATH] of Char;
  lpnLengthNeeded: Cardinal;
begin
  hd := OpenInputDesktop(0, False, MAXIMUM_ALLOWED); // if it fails then the workstation is prolly locked anyway
  if hd = 0 then begin
    Result := True;
    Exit;
  end;
  GetUserObjectInformation(hd, UOI_NAME, @buf, sizeof(buf), lpnLengthNeeded); // if we got it (hmm,) get a name
  CloseDesktop(hd);

  if CompareStr(buf, 'Winlogon') = 0 then
    Result := True
  else
    Result := False;
end;

procedure TMainForm.AutoAwayTimerTimer(Sender: TObject);
var
  shouldBeAway: Boolean;
  shouldBeNA: Boolean;
  currentMode: DWORD;
  pt: TPoint;
  lii: TLastInputInfo;
label
  TestSetAway;
begin
  shouldBeAway := False;
  shouldBeNA := False;

  if not ICQClient1.LoggedIn then Exit;

  if OnSaver then begin
    SystemParametersInfo(SPI_GETSCREENSAVERRUNNING, 0, @shouldBeAway, 0);
    if shouldBeAway then goto TestSetAway;
  end;

	if OnWLock and IsWorkstationLocked() then begin
		shouldBeAway := True;
    goto TestSetAway;
  end;

  if @MyGetLastInputInfo <> nil then begin
    ZeroMemory(@lii, SizeOf(lii));
    lii.cbSize := SizeOf(lii);
    MyGetLastInputInfo(lii);
    mouseStationaryTimer := (GetTickCount - lii.dwTime) div 1000;
  end
  else begin
    GetCursorPos(pt);
    if (pt.X <> lastMousePos.X) or (pt.Y <> lastMousePos.Y) then begin
      lastMousePos := pt;
      mouseStationaryTimer := 0;
    end
    else Inc(mouseStationaryTimer, 5);
  end;

  if OnMouse then begin
    if mouseStationaryTimer >=  60*AwayTime then shouldBeAway := True;
  end
  else shouldBeAway := False;

  if SetNA then begin
    if mouseStationaryTimer >=  60*NATime then shouldBeNA := True;
  end else
    shouldBeNA := False;

  if shouldBeNA then shouldBeAway := False;

  TestSetAway:

    currentMode := StatusToInt(ICQClient1.Status);

    if (not Boolean(awaySet)) and shouldBeAway and ( (currentMode = S_ONLINE) or (currentMode = S_FFC) ) then begin
      DoStatusChange(S_AWAY or fDCFlag);
      awaySet := 1;
      originalStatusMode := currentMode;
    end
    else if (not shouldBeAway) and (Boolean(awaySet)) and (not shouldBeNA) then begin
        DoStatusChange(originalStatusMode  or fDCFlag);
        awaySet := 0;
    end
    else if (not Boolean(naSet)) and shouldBeNA and ( (currentMode = S_ONLINE) or (currentMode = S_FFC) or Boolean(awaySet) ) then begin
      DoStatusChange(S_NA or fDCFlag);
      naSet := 1;
      if not Boolean(awaySet) then originalStatusMode := currentMode;
    end
   	else if (not shouldBeNA) and Boolean(naSet) then begin
      DoStatusChange(originalStatusMode or fDCFlag);
      naSet := 0;
    end;
end;

procedure TMainForm.optionsClick(Sender: TObject);
begin
  if not Assigned(OptionsForm) then
    OptionsForm := TOptionsForm.Create(Application);
  OptionsForm.Show;
end;

procedure TMainForm.KeepAliveTimerTimer(Sender: TObject);
begin
  ICQClient1.SendKeepAlive;         // safe - if not LoggedIn
end;

procedure TMainForm.ICQClient1LogOff(Sender: TObject);
begin
  SetCriticalSettings(ReadSettings());
  AutoAwayTimer.Enabled := False;
end;

procedure TMainForm.About1Click(Sender: TObject);
begin
  if not Assigned(AboutForm) then
    AboutForm := TAboutForm.Create(Application);
  AboutForm.Show;
end;

procedure TMainForm.GeneralLincence1Click(Sender: TObject);
begin
  OpenURL(ExtractFilePath(ParamStr(0)) + 'Docs\GNU_SK.htm', False);
end;

procedure TMainForm.ViewHistory1Click(Sender: TObject);
var
  FileName, strUIN: String;
begin
  SetCapture(ListView1.Handle);
  strUIN := ListView1.Selected.Caption;
  FileName := Format('%s%s%s%s', [ExtractFileDir(ParamStr(0)), '\History\', strUIN, '.txt']);
  if FileExists(FileName) then OpenURL(FileName , False)
  else MessageBox(Self.Handle,'History is empty! There is no entry.','Information',MB_OK);
end;

procedure TMainForm.ListView1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  rc: TRect;
begin
  OldTestHit := TestHit;
  if ListView1.IsEditing then Exit;
  if ((GetKeyState(VK_F10) and 128) = 128) or ((GetKeyState(VK_SHIFT) and 128) = 128) then Exit;
  TestHit := ListView1.GetItemAt(X, Y);
    if OldTestHit <> TestHit then begin
      if OldTestHit = nil then SetCapture(ListView1.Handle)
      else if TestHit = nil then ReleaseCapture;
      if OldTestHit <> nil then begin
        rc := OldTestHit.DisplayRect(drBounds);
        InvalidateRect(ListView1.Handle, @rc, True);
      end;
      if TestHit <> nil then begin
        rc := TestHit.DisplayRect(drBounds);
        InvalidateRect(ListView1.Handle, @rc, True);
      end;
    end;
end;

procedure TMainForm.ListView1Click(Sender: TObject);
begin
  SetCapture(ListView1.Handle);
end;

procedure TMainForm.ListView1SelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  rc: TRect;
begin
  OldTestHit := TestHit;
  if ListView1.IsEditing then Exit;

  if OldTestHit = nil then SetCapture(ListView1.Handle)
  else if TestHit = nil then ReleaseCapture;
  if OldTestHit <> nil then begin
    rc := OldTestHit.DisplayRect(drBounds);
    InvalidateRect(ListView1.Handle, @rc, True);
  end;
  if TestHit <> nil then begin
    rc := TestHit.DisplayRect(drBounds);
    InvalidateRect(ListView1.Handle, @rc, True);
  end;
end;

procedure TMainForm.Support1Click(Sender: TObject);
begin
  OpenURL(ExtractFilePath(ParamStr(0)) + 'Help\index.htm ', False);
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then Support1.Click;
end;

procedure TMainForm.IdleTimerTimer(Sender: TObject);
var
  i: Integer;
  ListItem: TListItem;
  dwIdle: DWORD;
begin
  if ListView1.Items.Count > 0 then
    for i := 0 to ListView1.Items.Count - 1 do
    begin
      ListItem := ListView1.Items.Item[i];
      dwIdle := StrToInt(ListItem.SubItems[LV_INDEX_IDLE]);
      if dwIdle <> 0 then
      begin
        Inc(dwIdle);
        ListItem.SubItems[LV_INDEX_IDLE] := IntToStr(dwIdle);  //Idle
      end;
    end;
end;

end.

