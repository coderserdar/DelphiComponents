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

unit UserInfo;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  ComCtrls, StdCtrls, ICQWorks, ExtCtrls, WinSock;

const
  TIMEOUT = 3000;
  UPDATEANIMFRAMES = 20;

type
  TUserInfoForm = class(TForm)
    pgUserInfo: TPageControl;
    tsICQ: TTabSheet;
    tbWork: TTabSheet;
    tbAbout: TTabSheet;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    AboutMemo: TMemo;
    tsPast: TTabSheet;
    GroupBox7: TGroupBox;
    GroupBox8: TGroupBox;
    PastsListView: TListView;
    btnUpdate: TButton;
    Button2: TButton;
    LocalTimer: TTimer;
    tbSummary: TTabSheet;
    tbLocation: TTabSheet;
    Label17: TLabel;
    NickNameLabel: TLabel;
    Label2: TLabel;
    FirstNameLabel: TLabel;
    Label3: TLabel;
    LastNameLabel: TLabel;
    Label4: TLabel;
    EmailLabel: TLabel;
    Label10: TLabel;
    AgeLabel: TLabel;
    GenderLabel: TLabel;
    Label11: TLabel;
    Label13: TLabel;
    DayOfBirthLabel: TLabel;
    Label18: TLabel;
    CellularLabel: TLabel;
    HomePageLabel: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    Language1Label: TLabel;
    Language2Label: TLabel;
    Language3Label: TLabel;
    Label7: TLabel;
    StreetLabel: TLabel;
    Label6: TLabel;
    CityLabel: TLabel;
    Label5: TLabel;
    CountryLabel: TLabel;
    Label8: TLabel;
    ZipLabel: TLabel;
    Label9: TLabel;
    TimeZoneLabel: TLabel;
    Label31: TLabel;
    LocalTimeLabel: TLabel;
    Label1: TLabel;
    UINLabel: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    WCityLabel: TLabel;
    WStateLabel: TLabel;
    WPhoneLabel: TLabel;
    WFaxLabel: TLabel;
    WAddressLabel: TLabel;
    WZipLabel: TLabel;
    WCountryLabel: TLabel;
    WCompanyLabel: TLabel;
    WDepartmentLabel: TLabel;
    WPositionLabel: TLabel;
    WOccupationLabel: TLabel;
    WHomePageLabel: TLabel;
    InterestsView: TListView;
    AffiliationsListView: TListView;
    Label15: TLabel;
    StateLabel: TLabel;
    Panel: TPanel;
    Bevel1: TBevel;
    Image1: TImage;
    Label16: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    ExtIPLabel: TLabel;
    IntIPLabel: TLabel;
    PortLabel: TLabel;
    IcqVerLabel: TLabel;
    MiraVerLabel: TLabel;
    OnlineSinceLabel: TLabel;
    TimerPing: TTimer;
    Label38: TLabel;
    stPing: TStaticText;
    lblAnim: TLabel;
    TimerAnim: TTimer;
    lblMyDetailsURL: TLabel;
    Label39: TLabel;
    lbIdle: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnUpdateClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure LocalTimeTimerTimer(Sender: TObject);
    procedure TimerPingTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EmailLabelClick(Sender: TObject);
    procedure WHomePageLabelClick(Sender: TObject);
    procedure HomePageLabelClick(Sender: TObject);
    procedure TimerAnimTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lblMyDetailsURLClick(Sender: TObject);
  private
    updateAnimFrame: Byte;
    function ChangeColor: Integer;
  protected
    procedure CreateParams(var Params : TCreateParams); override;
  public
    FSource: String;
    dwUIN: DWORD;
    dwStatus: DWORD;
    sInternalIP, sExternalIP: String[15];
    wPort: Word;
    byProtoVer, byUserCaps: Byte;
    dtOnlineTime: TDateTime;
    dwClient, dwMirandaVer: DWORD;
    dwIdle: DWORD;
  	hInstIcmp: HINST;
    stHandle: HWND;
  end;

  TPingThread = class(TThread)
  private
    { Private declarations }
    ip: u_long;
    sLabelPing: String;
  protected
    procedure Execute; override;
    procedure Ping;
  public
    FHandle: HWND;
  end;

var
  UserInfoForm: TUserInfoForm;
  TimeZoneDiff: Integer;
  TimeCheck: Integer;
  MyEdit1: HWND;
  PT: TPingThread;
  ThreadFinished: Boolean = False;

  MyIcmpCreateFile: function(): THandle; stdcall;
  MyIcmpCloseHandle: function(IcmpHandle: THandle): BOOL; stdcall;
  MyIcmpSendEcho: function(IcmpHandle: THandle;
                           DestinationAddress: DWORD;
                           RequestData: Pointer;
                           RequestSize: WORD;
                           RequestOptions: Pointer;
                           ReplyBuffer: Pointer;
                           ReplySize: DWORD;
                           Timeout: DWORD): DWORD; stdcall;

implementation
uses
  Main, SysUtils, DateUtils;

{$R *.dfm}

procedure TUserInfoForm.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params);     //Don't ever forget to do this!!!
  Params.WndParent := GetDesktopWindow;
end;

procedure TUserInfoForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  i: Integer;
begin
  Action := caFree;
  i := MainForm.GetUserInfoIdx(FSource);
  if i > -1 then
    MainForm.FInfoList.Delete(i);

  LocalTimer.Enabled := False;
  FreeLibrary(hInstIcmp);
  TimerAnim.Enabled := False;  
  MainForm.UserInfoReader := 0;
end;

procedure TUserInfoForm.btnUpdateClick(Sender: TObject);
begin
  MainForm.ICQClient1.RequestInfo(StrToInt64(FSource));
  TimerAnim.Enabled := True;
  lblAnim.Visible := True;
end;

procedure TUserInfoForm.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TUserInfoForm.LocalTimeTimerTimer(Sender: TObject);
var
  lpSystemTime: TSystemTime;
  lpDateTime: TDateTime;

  function LocalTime(DateTime: TDateTime): String;
  var
    TimeZoneInfo: TTimeZoneInformation;
    Ret: TDateTime;
  begin
    case GetTimeZoneInformation(TimeZoneInfo) of
      TIME_ZONE_ID_STANDARD:
        Ret := DateTime - ((TimeZoneDiff * 30) /60 / 24);
      TIME_ZONE_ID_DAYLIGHT:
        Ret := DateTime - (((TimeZoneDiff * 30) + TimeZoneInfo.DaylightBias) / 60 / 24);
      else
        Ret := 0;
    end;

    if Ret <> 0 then Result := TimeToStr(Ret)
      else Result := '<error>';
  end;

begin
  LocalTimer.Interval := 1000;
  if TimeZoneLabel.Enabled then
  begin
    GetSystemTime(lpSystemTime);
    lpDateTime := SystemTimeToDateTime(lpSystemTime);
    MainForm.SetLabel(LocalTimeLabel, True, False, LocalTime(lpDateTime));
  end else
    MainForm.SetLabel(LocalTimeLabel, False, False, NA);
end;

procedure TPingThread.Ping;
var
	hPing: THandle;
  szPacket: array [0..31] of Char;
	replyBuffer: array [0..511] of Char;
	startTime, endTime: DWORD;
	Result: DWORD;

  wVersionRequested : WORD;
  lwsaData : WSAData;
  error : DWORD;
begin

  wVersionRequested := MakeWord(1,1);
  error := WSAStartup(wVersionRequested,lwsaData);

 if (error <> 0) then
    Exit;

	hPing := MyIcmpCreateFile;

  szPacket := 'eICQ Data User Details Pinger';
	startTime := GetTickCount;

  {$WARNINGS OFF}
  if ip = INADDR_NONE then
    Result := 0
  {$WARNINGS ON}
  else
    Result := MyIcmpSendEcho(hPing,
                             ip,
                             @szPacket,
                             sizeof(szPacket),
                             nil,
                             @replyBuffer,
                             sizeof(replyBuffer),
                             TIMEOUT);


	endTime := GetTickCount;

  MyIcmpCloseHandle(hPing);

	if Result = 0 then
  begin
    sLabelPing := '<error>';
    EnableWindow(FHandle, False);
	end	else
  begin
    sLabelPing := Format('%d ms', [endTime - startTime]);
    EnableWindow(FHandle, True);    
	end;

  WSACleanup();

  //Hell yeah, WinAPI...
{
    len := Sendmessage(stPing.Handle, WM_GETTEXTLENGTH, 0, 0);
    SetLength(S, len);
    SendMessage(stPing.Handle, WM_GETTEXT, len+1, lparam(@s[1]));
    SendMessage(stText.Handle, WM_SETTEXT, 0, Integer(PChar(s)));
}
  SendMessage(FHandle, WM_SETTEXT, 0, Integer(PChar(sLabelPing)));
end;

procedure TPingThread.Execute;
begin
  FreeOnTerminate := True;
  //Synchronize(Ping);         //!@#$% SYNCHRONIZE!!!
                               //!@#$% VCL!!!
  Ping;
  ThreadFinished := True;
end;

procedure TUserInfoForm.TimerPingTimer(Sender: TObject);
begin
  if ThreadFinished then
  begin
    ThreadFinished := False;
    TimerPing.Interval := TIMEOUT + 2000;
    PT := TPingThread.Create(False);          //Start
    PT.FHandle := Self.stHandle;
    PT.ip := inet_addr(PChar(ExtIPLabel.Caption));
  end;
end;

procedure TUserInfoForm.FormCreate(Sender: TObject);
var
  is_error: Boolean;
begin
  updateAnimFrame := 0;

  hInstIcmp := LoadLibrary('icmp.dll');

  if hInstIcmp <> 0 then
  begin
    is_error := False;
    @MyIcmpCreateFile := GetProcAddress(hInstIcmp, 'IcmpCreateFile');
    @MyIcmpSendEcho := GetProcAddress(hInstIcmp, 'IcmpSendEcho');
    @MyIcmpCloseHandle := GetProcAddress(hInstIcmp, 'IcmpCloseHandle');


    if @MyIcmpCreateFile = nil then
      is_error := True
    else if @MyIcmpSendEcho = nil then
      is_error := True
    else if @MyIcmpCloseHandle = nil then
      is_error := True;
  end
    else is_error := True;

  if is_error then
  begin
    stPing.Caption := '<not supported>';
    ThreadFinished := False;
  end
    else ThreadFinished := True;
end;

procedure TUserInfoForm.EmailLabelClick(Sender: TObject);
begin
  if EmailLabel.Enabled then
    MainForm.OpenURL(EmailLabel.Caption, True);
end;

procedure TUserInfoForm.WHomePageLabelClick(Sender: TObject);
begin
  if WHomePageLabel.Enabled then
    MainForm.OpenURL(WHomePageLabel.Caption, False);
end;

procedure TUserInfoForm.HomePageLabelClick(Sender: TObject);
begin
  if HomePageLabel.Enabled then
    MainForm.OpenURL(HomePageLabel.Caption, False);
end;

procedure TUserInfoForm.TimerAnimTimer(Sender: TObject);
var
  x: Byte;
  Bodky: String[64];
begin
  Bodky := '..........';
  Inc(updateAnimFrame);
  if updateAnimFrame = UPDATEANIMFRAMES then updateAnimFrame := 1;
  x := updateAnimFrame mod 10;
  Delete(Bodky, x, 10-x);
  lblAnim.Font.Color := ChangeColor;
  lblAnim.Caption := Format('%sUpdating%s', [bodky, bodky]);

  if MainForm.UserInfoReader = USERINFO_ENTRIES then
  begin
    TimerAnim.Enabled := False;
    MainForm.UserInfoReader := 0;
    lblAnim.Visible := False;
  end;
end;

function TUserInfoForm.ChangeColor: Integer;
var
  NewCol, textCol, bgCol: Integer;
  ratio: Real;
begin

  textCol := GetSysColor(COLOR_BTNTEXT);
  bgCol := GetSysColor(COLOR_3DFACE);

  ratio := abs(UPDATEANIMFRAMES/2 - updateAnimFrame)*510/UPDATEANIMFRAMES;

  newCol := RGB(Trunc(GetRValue(bgCol)+(GetRValue(textCol)-GetRValue(bgCol))*ratio/256),
             Trunc(GetGValue(bgCol)+(GetGValue(textCol)-GetGValue(bgCol))*ratio/256),
             Trunc(GetBValue(bgCol)+(GetBValue(textCol)-GetBValue(bgCol))*ratio/256));
  Result := newCol;

end;

procedure TUserInfoForm.FormShow(Sender: TObject);
begin
  if btnUpdate.Enabled then btnUpdate.Click;
end;

procedure TUserInfoForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
end;

procedure TUserInfoForm.lblMyDetailsURLClick(Sender: TObject);
begin
  MainForm.OpenURL('http://web.icq.com/whitepages/login/', False);
end;

end.
