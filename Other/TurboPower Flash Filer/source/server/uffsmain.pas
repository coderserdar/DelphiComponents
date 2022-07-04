{*********************************************************}
{* Main window for server                                *}
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

unit uFFSMain;

{$I FFDEFINE.INC}

{$IFDEF SingleEXE}
!! Error: This application should not be compiled with SingleEXE mode enabled
{$ENDIF}

{$DEFINE UseTrayIcon} 

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, Menus, Buttons,
  Windows,
  {$IFDEF UseTrayIcon}
  ShellAPI,
  {$ENDIF}
  ffconst,
  ffnetmsg,
  ffdtmsgq,
  ffllbase,
  fflllgcy,
  fflleng,
  ffhash,
  ffllprot,
  ffsrbase,
  ffsrcfg,
  ffsreng,
  ffsrcmd,
  ffllthrd,
  uffegmgr,
  ffsrintm,
  ComCtrls,
  {$IFDEF DCC4ORLATER}
  ImgList,
  {$ENDIF}
  ToolWin;

type
  TffServerState = (
              ssInitializing,
              ssDown,
              ssComingUp,
              ssUp,
              ssUpMinimized);

const
  {$IFDEF UseTrayIcon}
  ffc_tiCallBack = WM_USER + $300;
  {$ENDIF}
  ffc_Activate = WM_USER + $301;
  ffc_Minimize = WM_USER + $302;

type
  TfrmFFServer = class(TForm)
    pnlBottom: TPanel;
    Timer1: TTimer;
    PopupMenu: TPopupMenu;
    pumDownServer: TMenuItem;
    N3: TMenuItem;
    pumExit: TMenuItem;
    MainMenu: TMainMenu;
    mnuServer: TMenuItem;
    mnuServerUp: TMenuItem;
    mnuServerDown: TMenuItem;
    N1: TMenuItem;
    mnuServerExit: TMenuItem;
    mnuConfig: TMenuItem;
    mnuConfigGeneral: TMenuItem;
    mnuConfigNetwork: TMenuItem;
    mnuConfigUsers: TMenuItem;
    mnuConfigAliases: TMenuItem;
    mnuConfigIndexes: TMenuItem;
    mnuDebug: TMenuItem;
    mnuDebugLog: TMenuItem;
    mnuResetCounters: TMenuItem;
    mnuHelp: TMenuItem;
    mnuHelpAbout: TMenuItem;
    mnuHelpWWW: TMenuItem;
    mnuHelpEmail: TMenuItem;
    pnlBig: TPanel;
    pnlServers: TPanel;
    gbServers: TGroupBox;
    lvServers: TListView;
    Splitter1: TSplitter;
    pnlTransports: TPanel;
    gbTransports: TGroupBox;
    lvTransports: TListView;
    pmuTrans: TPopupMenu;
    pmuTransLog: TMenuItem;
    pmuTransLogAll: TMenuItem;
    pmuTransLogSep: TMenuItem;
    pmuTransLogErr: TMenuItem;
    pmuTransLogReq: TMenuItem;
    pmuTransLogRep: TMenuItem;
    ToolBar: TToolBar;
    btnProps: TToolButton;
    ToolButton2: TToolButton;
    btnStart: TToolButton;
    btnStop: TToolButton;
    ImageList: TImageList;
    pnlTray: TPanel;
    imgUnlocked: TImage;
    imgStarted: TImage;
    lblTime: TLabel;
    imgStopped: TImage;
    imgLocked: TImage;
    imgLogging: TImage;
    HelpTopics1: TMenuItem;
    N2: TMenuItem;
    procedure mnuConfigAliasesClick(Sender: TObject);
    procedure mnuDebugLogClick(Sender: TObject);
    procedure mnuServerExitClick(Sender: TObject);
    procedure pumDownServerClick(Sender: TObject);
    procedure pumExitClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure mnuResetCountersClick(Sender: TObject);
    procedure mnuHelpAboutClick(Sender: TObject);
    procedure mnuConfigUsersClick(Sender: TObject);
    procedure mnuHelpWWWClick(Sender: TObject);
    procedure mnuHelpEmailClick(Sender: TObject);
    procedure mnuConfigGeneralClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure mnuConfigIndexesClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure mnuConfigNetworkClick(Sender: TObject);
    procedure lvServersClick(Sender: TObject);
    procedure pmuTransLogAllClick(Sender: TObject);
    procedure pmuTransLogErrClick(Sender: TObject);
    procedure pmuTransLogReqClick(Sender: TObject);
    procedure pmuTransLogRepClick(Sender: TObject);
    procedure lvTransportsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnPropsClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure HelpTopics1Click(Sender: TObject);
  private
    { Private d
    procedure FormResize(Sender: TObject);eclarations }
    FIgnoreNextLogin : Boolean;
    FState        : TffServerState;
    FStartTime    : TDateTime;
    FMsgStartTime : TDateTime;
    cuIsGreen     : Boolean;
    FCloseClick   : Boolean;
    FElapsedTime  : TDateTime;
    FMustClose    : Boolean;
    FClosingFromTray : Boolean;
    {$IFDEF UseTrayIcon}
    tiActive      : Boolean;
    tiNotifyData  : TNotifyIconData;
    tiPresent     : Boolean;
    {$ENDIF}


    procedure AppException(Sender : TObject; E : Exception);
    procedure DisplayHint(Sender : TObject);
    function  Login : Boolean;
    procedure MainFormMinimize(Sender : TObject);
    procedure MainFormRestore(Sender : TObject);
    procedure PaintPadlockBitmap;

    {$IFDEF UseTrayIcon}
    procedure tiAdd;
    procedure tiCallBack(var Msg : TMessage); message ffc_tiCallBack;
    procedure tiDelete;
    function  tiGetIconHandle : hIcon;
    procedure tiInitNotifyData;
    {$ENDIF}
  public
    { Public declarations }
    ServerName : TffNetAddress;

    procedure BringServerUp;
    procedure BringServerDown;
    procedure CreateEngineMgr;
    procedure FreeEngineMgr;                                          {!!.06}
    procedure DownServer;
    function  ElapsedTimeToStr(T : TDateTime) : string;
    procedure FFCActivate(var Msg : TMessage); message ffc_Activate;
    procedure FFCMinimize(var Msg : TMessage); message ffc_Minimize;
    function  GetMsgsPerSecAsString(aMsgCount : Longint) : string;
    procedure LoadServers;
    procedure LoadTransports;
    procedure ResetStatistics;
    procedure SetControls;
    procedure SetServerName;
    procedure SetServerPriority(aPriority : longint);
    procedure SetState(S : TffServerState);
    procedure UpdateServers;
    procedure UpdateTransports;
    procedure UpServer;
    procedure WMSysCommand(var Msg : TMessage); message WM_SYSCOMMAND;
    procedure WMQueryEndSession(var msg : TMessage); message WM_QUERYENDSESSION;

    property State : TffServerState read FState write SetState;
  end;

var
  frmFFServer: TfrmFFServer;

implementation

uses
  FFAbout,
  FFLLComm,
  FFLLComp,
  uFFSAlas,
  uFFSUser,
  uFFSGenl,
  uFFSIndx,
  uFFSNet,
  FFSrJour,
  FFLogDlg, uFFsCfg;

{$R *.DFM}

{===Main Menu options================================================}
procedure TfrmFFServer.mnuConfigAliasesClick(Sender: TObject);
begin
  CreateEngineMgr;
  with TFFAliasForm.Create(Application) do
    try
      ServerEngine := TffServerEngine(lvServers.Selected.Data);
      ShowModal;
    finally
      Free;
    end;
end;
{--------}
procedure TfrmFFServer.mnuConfigGeneralClick(Sender: TObject);
begin
  CreateEngineMgr;
  with TFFGenConfigForm.Create(Application) do
    try
      ServerEngine := TffServerEngine(lvServers.Selected.Data);
      ShowModal;
      { Get the bits in which we're interested. }
      with ServerEngine.Configuration.GeneralInfo^ do begin
        ServerName := giServerName;
        ServerEngine.MaxRAM := giMaxRAM;                              {!!.01}
//        ServerEngine.CollectGarbage := giCollectEnabled;            {Deleted !!.01}
//        ServerEngine.CollectFrequency := giCollectFreq;             {Deleted !!.01}
        SetServerName;
        mnuDebugLog.Checked := giDebugLog;
        FFEngineManager.EventLogEnabled := giDebugLog;
        imgLogging.Visible := (giDebugLog and (not giReadOnly));

        SetServerPriority(giPriority);
        PaintPadlockBitmap;
      end;
      SetControls;
    finally
      Free;
    end;
    lvServers.Selected.Caption := ServerName;
end;
{--------}
procedure TfrmFFServer.mnuConfigNetworkClick(Sender: TObject);
begin
  CreateEngineMgr;
  with TffNetConfigForm.Create(Application) do
    try
      ServerEngine := TffServerEngine(lvServers.Selected.Data);
      ShowModal;
      if (State = ssUp) then
        with ServerEngine.Configuration.GeneralInfo^ do begin
          if Assigned(FFEngineManager.IPXSPXTransport) then
            FFEngineManager.IPXSPXTransport.RespondToBroadcasts := giIPXSPXLFB;
          if Assigned(FFEngineManager.TCPIPTransport) then
            FFEngineManager.TCPIPTransport.RespondToBroadcasts := giTCPIPLFB;
        end;
    finally
      Free;
    end;
end;
{--------}
procedure TfrmFFServer.mnuConfigIndexesClick(Sender: TObject);
begin
  CreateEngineMgr;
  with TffIndexForm.Create(Application) do
    try
      ServerEngine := TffServerEngine(lvServers.Selected.Data);
      ShowModal;
    finally
      Free;
    end;
end;
{--------}
procedure TfrmFFServer.mnuConfigUsersClick(Sender: TObject);
begin
  CreateEngineMgr;
  with TFFUserForm.Create(Application) do
    try
      ServerEngine := TffServerEngine(lvServers.Selected.Data);
      ShowModal;
    finally
      Free;
    end;
end;
{--------}
procedure TfrmFFServer.mnuDebugLogClick(Sender: TObject);
begin
  CreateEngineMgr;
  mnuDebugLog.Checked := not mnuDebugLog.Checked;
  with TffServerEngine(lvServers.Selected.Data) do begin
    Configuration.GeneralInfo^.giDebugLog := mnuDebugLog.Checked;
    WriteGeneralInfo(False);
    FFEngineManager.EventLogEnabled := mnuDebugLog.Checked;
    imgLogging.Visible := (mnuDebugLog.Checked and (not Configuration.GeneralInfo^.giReadOnly));
  end;
end;
{--------}
procedure TfrmFFServer.mnuHelpAboutClick(Sender: TObject);
var
  AboutBox : TFFAboutBox;
begin
  AboutBox := TFFAboutBox.Create(Application);
  try
    AboutBox.IsServer := true;
    AboutBox.Caption := 'About FlashFiler Server';
    AboutBox.ProgramName.Caption := 'FlashFiler Server';
    AboutBox.ShowModal;
  finally
    AboutBox.Free;
  end;
end;
{--------}
procedure TfrmFFServer.mnuHelpWWWClick(Sender: TObject);
begin
  ShellToWWW;
end;
{--------}
procedure TfrmFFServer.mnuHelpEmailClick(Sender: TObject);
begin
  ShellToEmail;
end;
{--------}
procedure TfrmFFServer.mnuResetCountersClick(Sender : TObject);
begin
  FMsgStartTime := Now;
  ResetStatistics;
end;
{--------}
procedure TfrmFFServer.mnuServerExitClick(Sender : TObject);
begin
  Close;
end;
{====================================================================}


{===Popup menu options===============================================}
procedure TfrmFFServer.pumDownServerClick(Sender : TObject);
begin
  if not Login then
    Exit;
  FIgnoreNextLogin := True;
  Application.Restore;
  Application.ProcessMessages;
  BringServerDown;
//  FreeEngineMgr;                                                       {!!.06}{Deleted !!.13}
end;
{--------}
procedure TfrmFFServer.pumExitClick(Sender : TObject);
begin
  FClosingFromTray := True;
  Application.Restore;
  Close;
end;
{====================================================================}


{===Message handlers=================================================}
procedure TfrmFFServer.FFCActivate(var Msg : TMessage);
begin
  Update;
  BringServerUp;
end;
{--------}
procedure TfrmFFServer.FFCMinimize(var Msg : TMessage);
begin
  Application.Minimize;
end;
{--------}
procedure TfrmFFServer.WMQueryEndSession(var msg : TMessage);
begin
  FMustClose := true;
  inherited;
end;
{--------}
procedure TfrmFFServer.WMSysCommand(var Msg : TMessage);
begin
  if (((Msg.wParam) and $FFF0) = SC_CLOSE) then
    FCloseClick := true;
  inherited;
end;
{====================================================================}


{===Timer handler====================================================}
procedure TfrmFFServer.Timer1Timer(Sender: TObject);
begin
  if State = ssUpMinimized then                                       {!!.11}
    Exit;                                                             {!!.11}
  UpdateServers;
  UpdateTransports;
  case State of
    ssComingUp :
      begin
        cuIsGreen := not cuIsGreen;
        if cuIsGreen then begin
          imgStarted.Visible := True;
          imgStopped.Visible := False;
        end else begin
          imgStarted.Visible := False;
          imgStopped.Visible := True;
        end;
      end;
    ssUp :
      begin
        FElapsedTime := Now - FStartTime;
        lblTime.Caption := ElapsedTimeToStr(FElapsedTime);
      end;
  end;{case}
end;
{====================================================================}


{===Form events======================================================}
procedure TfrmFFServer.FormActivate(Sender: TObject);
{$IFDEF UseTrayIcon}
var
  OSVerInfo : TOSVersionInfo;
{$ENDIF}
begin
  {$IFDEF UseTrayIcon}
  tiPresent := false;
  OSVerInfo.dwOSVersionInfoSize := sizeof(OSVerInfo);
  if GetVersionEx(OSVerInfo) then begin
    {Note: Windows95 returns version major:minor = 4:0}
    if (OSVerInfo.dwPlatformID = VER_PLATFORM_WIN32_WINDOWS) or {Windows95}
       (OSVerInfo.dwPlatformID = VER_PLATFORM_WIN32_NT) then    {WindowsNT}
      tiPresent := OSVerInfo.dwMajorVersion > 3
  end;
  {$ENDIF}

  if Assigned(FFEngineManager) and Assigned(lvServers.Selected) then   {!!.13}
    if TffServerEngine(lvServers.Selected.Data).Configuration.GeneralInfo^.giAutoMini {!!.13}
     and (State = ssInitializing) then FIgnoreNextLogin := true;       {!!.13}

  if not Login then begin
    PostMessage(Handle, WM_QUIT, 0, 0);
    Exit;
  end;
  try
    if assigned(FFEngineManager) and assigned(lvServers.Selected) then {!!.02}
      with TffServerEngine(lvServers.Selected.Data).Configuration.GeneralInfo^ do begin
        {..server name, etc}
        ServerName := giServerName;
        TffServerEngine(lvServers.Selected.Data).BufferManager.MaxRAM := giMaxRAM;
        {..ports}
        FFSetTCPPort(giTCPPort);
        FFSetUDPPortServer(giUDPPortSr);
        FFSetUDPPortClient(giUDPPortCl);
        FFSetIPXSocketServer(giIPXSocketSr);
        FFSetIPXSocketClient(giIPXSocketCl);
        FFSetSPXSocket(giSPXSocket);
        {..keepalive stuff}
        ffc_LastMsgInterval := giLastMsgInterval;
        ffc_KeepAliveInterval := giKAInterval;
        ffc_KeepAliveRetries := giKARetries;
        {..priority}
        SetServerPriority(giPriority);
        {..auto up}
        if giAutoUp and (ServerName <> '') then
          PostMessage(Handle, ffc_Activate, 0, 0);
        {..auto minimize}
        if giAutoMini then
          PostMessage(Handle, ffc_Minimize, 0, 0);
      end;
    Application.ShowHint := true;
    State := ssDown;
    SetServerName;
    {$IFDEF UseTrayIcon}
    if tiPresent then
       tiAdd;
    {$ENDIF}
  except
    {????}
    raise;
  end;
  (******
  {parse out the command line}
  ActivateIt := false;
  MinimizeIt := false;
  ParmCount := ParamCount;
  if (ParmCount > 0) then begin
    {the server name is parameter 1}
    ServerName := ParamStr(1);
    {whether to make the server active is parameter 2: the value
     should be either 'Up' or 'Down', but actually all other values are ignored
     and assumed to be 'Down'}
    if (ParmCount > 1) then begin
      ParmValue := ParamStr(2);
      if (FFCmpShStrUC(ParmValue, 'UP', length(ParmValue)) = 0) then
        ActivateIt := true;
    end;
    if (ParmCount > 2 ) then begin
      ParmValue := ParamStr(3);
      if ( FFCmpShStrUC( ParmValue, 'WINSOCK', length( ParmValue) ) = 0 ) then
        mnuConfigWinsockClick( Self )
      else
      if ( FFCmpShStrUC( ParmValue, 'NETBIOS',
                              length( ParmValue) ) = 0 ) then
        mnuConfigNetBIOSClick( Self );
      Application.ProcessMessages;
    end;
    if ( ParmCount > 3 ) then begin
      ParmValue := ParamStr(4);
      if (FFCmpShStrUC(ParmValue, 'MINIMIZE', length(ParmValue)) = 0) then
        MinimizeIt := true;
    end;
  end;
  *****)
end;
{--------}
procedure TfrmFFServer.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FMustClose then begin
    if (State <> ssDown) then begin
      if (State = ssUpMinimized) then
        Application.Restore;
      BringServerDown;
    end;
    CanClose := true;
  end else begin
    if (State = ssUp) and FCloseClick then begin
      FCloseClick := false;
      Application.Minimize;
    end
    else if (State <> ssDown) then begin
      if Login then begin
        if (State = ssUpMinimized) then
          Application.Restore;
        BringServerDown;
      end;
    end;
    CanClose := (State = ssDown);
  end;
end;
{--------}
procedure TfrmFFServer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  {$IFDEF UseTrayIcon}
  tiDelete;
  {$ENDIF}
end;
{--------}
procedure TfrmFFServer.FormCreate(Sender : TObject);
begin
  FClosingFromTray := False;
  FIgnoreNextLogin := False;
  State := ssInitializing;
  Application.OnMinimize := MainFormMinimize;
  Application.OnRestore := MainFormRestore;
  Application.OnException := AppException;
  Application.OnHint := DisplayHint;
  FFEngineManager := nil;
  IsMultiThread := True;
  pumDownServer.Enabled := False;                                     {!!.01 Added}
  FFSConfigGetFormPos('Main', Self);                                  {!!.06}
end;
{--------}
procedure TfrmFFServer.AppException(Sender : TObject; E : Exception);
begin
  Application.ShowException(E)
end;
{--------}
procedure TfrmFFServer.DisplayHint(Sender : TObject);
begin
  if Application.MainForm.Active then
    pnlBottom.Caption := ' ' + Application.Hint;                      {!!.06}
end;
{--------}
procedure TfrmFFServer.FormDestroy(Sender: TObject);
begin
  FFSConfigSaveFormPrefs('Main', Self);
  FreeEngineMgr;                                                      {!!.06}
end;
{--------}
procedure TfrmFFServer.FormHide(Sender : TObject);
begin
  ShowWindow(Application.Handle, SW_HIDE);
end;
{--------}
procedure TfrmFFServer.FormPaint(Sender : TObject);
begin
  PaintPadlockBitmap;
end;
{--------}
procedure TfrmFFServer.FormShow(Sender : TObject);
begin
  FFSConfigGetFormPrefs('Main', Self);
  { Create the engine manager. }
  CreateEngineMgr;
{Begin !!.02}
  try
    LoadServers;
    LoadTransports;
  finally
    SetControls;
  end;
{End !!.02}
end;
{--------}
function TfrmFFServer.Login : Boolean;
var
  LoginDlg : TFFLoginDialog;
  Hash     : TffWord32;
  Pwd      : TffName;
  Server   : TffServerEngine;
  User     : TffUserItem;
  UserInx  : integer;
  UserID   : TffName;
begin
  if FMustClose then begin                                           {!!.11}
    Result := True;                                                  {!!.11}
    Exit;                                                            {!!.11}
  end;                                                               {!!.11}
  if FIgnoreNextLogin then begin
    FIgnoreNextLogin := False;
    Result := True;
    Exit;
  end;
  Server := nil;
  if lvServers.SelCount > 0 then
    Server := TffServerEngine(lvServers.Selected.Data);
  if (not assigned(Server)) or
     (not Server.Configuration.GeneralInfo^.giIsSecure) then begin
    Result := true;
    Exit;
  end;
  Result := false;
  LoginDlg := TFFLoginDialog.Create(Application);
  try
    if (LoginDlg.ShowModal = mrOK) then begin
      UserID := LoginDlg.edtUserName.Text;
      Pwd := LoginDlg.edtPassword.Text;
      UserInx := Server.Configuration.UserList.UserIndex(UserID);
      if (UserInx <> -1) then begin
        User := Server.Configuration.UserList[UserInx];
        Hash := FFCalcShStrElfHash(Pwd);
        if (Hash = User.PasswordHash) and
           (arAdmin in User.Rights) then
          Result := true;
      end
    end;
  finally
    LoginDlg.Free;
  end;{try..finally}
end;
{--------}
procedure TfrmFFServer.MainFormMinimize(Sender : TObject);
begin
  if (State = ssUp) then
    State := ssUpMinimized;
  {$IFDEF UseTrayIcon}
  if tiPresent and (State = ssUpMinimized) then begin
    Hide;
  end;
  {$ENDIF}
end;
{--------}
procedure TfrmFFServer.MainFormRestore(Sender : TObject);
begin
  {$IFDEF UseTrayIcon}
  if tiPresent and (State = ssUpMinimized) then begin
    Show;
    SetForegroundWindow(Handle);
  end;
  {$ENDIF}
  if (State = ssUpMinimized) then
    State := ssUp;
  if (not FClosingFromTray) and (not Login) then
    Application.Minimize;
end;
{--------}
procedure TfrmFFServer.PaintPadlockBitmap;
begin
  if lvServers.SelCount > 0 then begin
    imgLocked.Visible := TffServerEngine(lvServers.Selected.Data).Configuration.GeneralInfo^.giIsSecure;
    imgUnlocked.Visible := not imgLocked.Visible;
  end else begin
    imgLocked.Visible := False;
    imgUnlocked.Visible := False;
  end;
end;
{====================================================================}


{===property handlers================================================}
procedure TfrmFFServer.SetState(S : TffServerState);
begin
  if (S = FState) and (S <> ssInitializing) then
    Exit;
  FState := S;
  case S of
    ssInitializing :
      begin
        {..edit controls}
        lblTime.Caption := ElapsedTimeToStr(0.0);
      end;
    ssDown :
      begin
        {..menu items}
        if (ServerName = '') then
          mnuServerUp.Enabled := false
        else
          mnuServerUp.Enabled := true;
        mnuServerDown.Enabled := false;
        if ((assigned(FFEngineManager)) and (lvServers.SelCount > 0)) then
          with TffServerEngine(lvServers.Selected.Data).Configuration do begin
            FFEngineManager.EventLogEnabled := mnuDebugLog.Checked;
          end;

        {..speedbutton actions}
        if (ServerName = '') then
          btnStart.Enabled := false
        else
          btnStart.Enabled := true;
        btnStop.Enabled := false;

        {..online indicator}
        imgStarted.Visible := False;
        imgStopped.Visible := True;

        {..timer}
        Timer1.Enabled := false;
      end;
    ssComingUp :
      begin
        {..menu items}
        mnuServerUp.Enabled := false;
        {..speedbutton actions}
        btnStart.Enabled := false;
        {..edit controls}
        lblTime.Caption := ElapsedTimeToStr(0.0);
        {..form fields}
        FStartTime := Now;
        FMsgStartTime := Now;
        {..timer}
        Timer1.Enabled := true;
      end;
    ssUp :
      begin
        {..menu items}
        mnuServerUp.Enabled := false;
        mnuServerDown.Enabled := true;
        {..speedbutton actions}
        btnStart.Enabled := false;
        btnStop.Enabled := true;
        {..online indicator}
        imgStarted.Visible := True;
        imgStopped.Visible := False;
        {..timer}
        Timer1.Enabled := true;
      end;
    ssUpMinimized :
      begin
        {..timer}
        Timer1.Enabled := false;
      end;
  end;{case}
end;
{====================================================================}


{===Utility methods==================================================}
procedure TfrmFFServer.BringServerDown;
begin
  if (State = ssUp) then begin
    DownServer;
    State := ssDown;
  end;
end;
{--------}
procedure TfrmFFServer.BringServerUp;
begin
  if (State = ssDown) then begin
    Timer1.Interval := 250;
    cuIsGreen := false;
    State := ssComingUp;
    try
      UpServer;
      Timer1.Interval := 1000;
      State := ssUp;
    except
      State := ssDown;
      raise;
    end;{try..except}
  end;
  LoadServers;
  LoadTransports;
end;
{--------}
procedure TfrmFFServer.CreateEngineMgr;
begin
  if not assigned(FFEngineManager) then begin
    FFEngineManager := TffEngineManager.Create(nil);
    if ParamCount > 0 then
      FFEngineManager.ScriptFile := ParamStr(1);
  end;
end;
{--------}
procedure TfrmFFServer.FreeEngineMgr;                                 {!!.06}
begin
  if assigned(FFEngineManager) then
    try
      FFEngineManager.ShutDown;
    finally
      FFEngineManager.Free;
      FFEngineManager := nil;
    end;
end;
{--------}
procedure TfrmFFServer.DownServer;
{Deleted !!.01}
{var
  Idx : Integer;}
begin
  Screen.Cursor := crHourGlass;
  try
    {free all server engines}
{Begin !!.01}
    if assigned(FFEngineManager) then begin
      pumDownServer.Enabled := False;
      FFEngineManager.ShutDown;
{      for Idx := 0 to Pred(lvServers.Items.Count) do
        with TffServerEngine(lvServers.Items[Idx].Data) do
          State := ffesInactive; }
    end;
{End !!.01}
  finally
    Screen.Cursor := crDefault;
  end;
  {redisplay the counters}
  ResetStatistics;
  Timer1Timer(Self);
end;
{--------}
function TfrmFFServer.ElapsedTimeToStr(T : TDateTime) : string;
var
  Dy : integer;
  Hr : integer;
  Mi : integer;
  Se : integer;
  WorkSt : string[9];
begin
  Dy := trunc(T);
  T := frac(T) * 24.0;
  Hr := trunc(T);
  T := frac(T) * 60.0;
  Mi := trunc(T);
  Se := trunc(frac(T) * 60.0);
            {123456789012345678}
  Result := 'Up:     0:00:00:00';
  Result[10] := TimeSeparator;
  Result[12] := TimeSeparator;
  Result[16] := TimeSeparator;
  Str(Dy:5, WorkSt);
  Move(WorkSt[1], Result[5], 5);
  Str(Hr:2, WorkSt);
  Result[12] := WorkSt[2];
  if (Hr > 9) then
    Result[11] := WorkSt[1];
  Str(Mi:2, WorkSt);
  Result[15] := WorkSt[2];
  if (Mi > 9) then
    Result[14] := WorkSt[1];
  Str(Se:2, WorkSt);
  Result[18] := WorkSt[2];
  if (Se > 9) then
    Result[17] := WorkSt[1];
end;
{--------}
function TfrmFFServer.GetMsgsPerSecAsString(aMsgCount: Integer): string;
var
  MsgsPerSec   : double;
begin
  if (FElapsedTime > 0.0) then
    MsgsPerSec := aMsgCount / (FElapsedTime * 86400.0)
  else
    MsgsPerSec := 0.0;
  Str(MsgsPerSec:0:4, Result);
end;
{--------}
procedure TfrmFFServer.LoadServers;
var
  ListItem   : TListItem;
  SelServIdx : Integer;
  Servers    : TffList;
  i          : Integer;
begin
  if assigned(FFEngineManager) then begin
    if lvServers.SelCount > 0 then
      SelServIdx := lvServers.Selected.Index
    else
      SelServIdx := 0;
    lvServers.Items.BeginUpdate;
    Servers := TffList.Create;
    try
      lvServers.Items.Clear;
      FFEngineManager.GetServerEngines(Servers);
      for i := 0 to Pred(Servers.Count) do begin
        ListItem := lvServers.Items.Add;
        with TffServerEngine(TffIntListItem(Servers[i]).KeyAsInt) do begin
          ListItem.Caption := Configuration.ServerName;
          ListItem.Data := Pointer(TffIntListItem(Servers[i]).KeyAsInt);
          ListItem.SubItems.Add(FFMapStateToString(State));
          ListItem.SubItems.Add(FFCommaizeChL(ClientList.ClientCount, ThousandSeparator));
          ListItem.SubItems.Add(FFCommaizeChL(SessionList.SessionCount, ThousandSeparator));
          ListItem.SubItems.Add(FFCommaizeChL(DatabaseList.DatabaseCount, ThousandSeparator));
          ListItem.SubItems.Add(FFCommaizeChL(TableList.TableCount, ThousandSeparator));
          ListItem.SubItems.Add(FFCommaizeChL(CursorList.CursorCount, ThousandSeparator));
          ListItem.SubItems.Add(FFCommaizeChL(BufferManager.RAMUsed, ThousandSeparator));
        end;
        if i = SelServIdx then
          lvServers.Selected := ListItem;
      end;
    finally
      Servers.Free;
      lvServers.Items.EndUpdate;
    end;
  end;
end;
{--------}
procedure TfrmFFServer.LoadTransports;
var
  i            : Integer;
  NewTransItem : TListItem;
  Transports   : TffList;
begin
  Transports := TffList.Create;
  try
    lvTransports.Items.Clear;
    if lvServers.SelCount > 0 then begin
      if assigned(FFEngineManager) then begin
        FFEngineManager.GetTransports(TffServerEngine(lvServers.Selected.Data),
                                      Transports);
        lvTransports.Items.BeginUpdate;
        try
          lvTransports.Items.Clear;
          for i := 0 to Pred(Transports.Count) do begin
            with TffBaseTransport(TffIntListItem(Transports[i]).KeyAsInt) do begin
              NewTransItem := lvTransports.Items.Add;
              NewTransItem.Caption := GetName;
              NewTransItem.Data := Pointer(TffIntListItem(Transports[i]).KeyAsInt);
              NewTransItem.SubItems.Add(ServerName);
              NewTransItem.SubItems.Add(FFMapStateToString(State));
              NewTransItem.SubItems.Add(FFCommaizeChL(ConnectionCount, ThousandSeparator));
              NewTransItem.SubItems.Add(FFCommaizeChL(MsgCount, ThousandSeparator));
              NewTransItem.SubItems.Add(GetMsgsPerSecAsString(MsgCount));
            end;
          end;
        finally
          lvTransports.Items.EndUpdate;
        end;
      end;
    end;
  finally
    Transports.Free;
  end;
end;
{--------}
procedure TfrmFFServer.lvServersClick(Sender: TObject);
begin
  LoadTransports;
  SetControls;
  PaintPadlockBitmap;
end;
{--------}
procedure TfrmFFServer.ResetStatistics;
var
  i : Integer;
begin
  for i := 0 to pred(lvTransports.Items.Count) do begin
    with TffBaseTransport(lvTransports.Items[i].Data) do begin
      ResetMsgCount;
    end;
  end;
end;
{--------}
procedure TfrmFFServer.SetControls;
var
  SelectedServer   : TffServerEngine;
  IsServerSelected : Boolean;
begin
  SelectedServer := nil;
  IsServerSelected := lvServers.SelCount > 0;
  if IsServerSelected then begin
    SelectedServer := TffServerEngine(lvServers.Selected.Data);
    gbTransports.Caption := format(' Transports for %s ', [lvServers.Selected.Caption]);
    imgLogging.Visible := SelectedServer.Configuration.GeneralInfo^.giDebugLog;
    mnuDebugLog.Checked := imgLogging.Visible;
  end else begin
    gbTransports.Caption := ' Transports for selected server ';
    imgLogging.Visible := False;
    mnuDebugLog.Checked := False;
  end;
  mnuConfigGeneral.Enabled := IsServerSelected;
  mnuConfigNetwork.Enabled := IsServerSelected;
  mnuConfigAliases.Enabled := IsServerSelected;
  mnuConfigIndexes.Enabled := IsServerSelected;
  mnuConfigUsers.Enabled := IsServerSelected;
  btnProps.Enabled := IsServerSelected;
  if IsServerSelected and Assigned(SelectedServer) then
    mnuDebugLog.Enabled := not SelectedServer.Configuration.GeneralInfo^.giReadOnly
  else
    mnuDebugLog.Enabled := False;
  mnuResetCounters.Enabled := IsServerSelected;
end;
{--------}
procedure TfrmFFServer.SetServerName;
begin
  if assigned(FFEngineManager) and assigned(lvServers.Selected)then   {!!.02}
    with TffServerEngine(lvServers.Selected.Data) do
      Configuration.GeneralInfo^.giServerName := ServerName;
  if (ServerName <> '') then begin
    Caption := 'TurboPower FlashFiler [' + ServerName + ']';
    Application.Title := Caption;
    if (State = ssDown) then begin
      mnuServerUp.Enabled := true;
      btnStart.Enabled := true;
    end;
    {$IFDEF UseTrayIcon}
    tiInitNotifyData;
    {$ENDIF}
  end else begin
    Caption := 'FlashFiler Server';
    mnuServerUp.Enabled := false;
    btnStart.Enabled := false;
  end;
end;
{--------}
procedure TfrmFFServer.SetServerPriority(aPriority : longint);
const
  ThreadPriority : array [0..4] of integer =
                   (THREAD_PRIORITY_LOWEST,
                    THREAD_PRIORITY_BELOW_NORMAL,
                    THREAD_PRIORITY_NORMAL,
                    THREAD_PRIORITY_ABOVE_NORMAL,
                    THREAD_PRIORITY_HIGHEST);
begin
  if (aPriority < -2) or (aPriority > 2) then
    aPriority := 2
  else
    inc(aPriority, 2);
  SetThreadPriority(GetCurrentThread, ThreadPriority[aPriority]);
end;
{--------}
procedure TfrmFFServer.UpdateServers;
var
  i : Integer;
begin
  for i := 0 to Pred(lvServers.Items.Count) do begin
    with lvServers.Items[i], TffServerEngine(lvServers.Items[i].Data) do begin
      SubItems[0] := FFMapStateToString(State);
      SubItems[1] := FFCommaizeChL(ClientList.ClientCount, ThousandSeparator);
      SubItems[2] := FFCommaizeChL(SessionList.SessionCount, ThousandSeparator);
      SubItems[3] := FFCommaizeChL(DatabaseList.DatabaseCount, ThousandSeparator);
      SubItems[4] := FFCommaizeChL(TableList.TableCount, ThousandSeparator);
      SubItems[5] := FFCommaizeChL(CursorList.CursorCount, ThousandSeparator);
      SubItems[6] := FFCommaizeChL(BufferManager.RAMUsed, ThousandSeparator);
    end;
  end;
end;
{--------}
procedure TfrmFFServer.UpdateTransports;
var
  i : Integer;
begin
  for i := 0 to pred(lvTransports.Items.Count) do begin
    with lvTransports.Items[i],
         TffBaseTransport(lvTransports.Items[i].Data) do begin
      SubItems[0] := ServerName;
      SubItems[1] := FFMapStateToString(State);
      SubItems[2] := FFCommaizeChL(ConnectionCount, ThousandSeparator);
      SubItems[3] := FFCommaizeChL(MsgCount, ThousandSeparator);
      SubItems[4] := GetMsgsPerSecAsString(MsgCount);
    end;
  end;
end;
{--------}
procedure TfrmFFServer.UpServer;
var
  SaveCursor : TCursor;
begin
  SaveCursor := Cursor;
  Cursor := crHourglass;
  try
    { Create & set up the transports. }
    with FFEngineManager.ServerEngine.Configuration.GeneralInfo^ do begin

      FFEngineManager.SUPTransport.Enabled := giSingleUser;
      if giSingleUser then begin
        FFEngineManager.SUPTransport.BeginUpdate;
        try
          FFEngineManager.SUPTransport.ServerName := giServerName;
          FFEngineManager.SUPTransport.Mode := fftmListen;
          FFEngineManager.SUPTransport.EndUpdate;
        except
          FFEngineManager.SUPTransport.CancelUpdate;
        end;
      end;

      FFEngineManager.IPXSPXTransport.Enabled := giIPXSPX;
      if giIPXSPX then begin
        FFEngineManager.IPXSPXTransport.BeginUpdate;
        try
          FFEngineManager.IPXSPXTransport.ServerName := giServerName;
          FFEngineManager.IPXSPXTransport.RespondToBroadcasts := giIPXSPXLFB;
          FFEngineManager.IPXSPXTransport.Mode := fftmListen;
          FFEngineManager.IPXSPXTransport.EndUpdate;
        except
          FFEngineManager.IPXSPXTransport.CancelUpdate;
        end;
      end;

      FFEngineManager.TCPIPTransport.Enabled := giTCPIP;
      if giTCPIP then begin
        FFEngineManager.TCPIPTransport.BeginUpdate;
        try
          FFEngineManager.TCPIPTransport.ServerName := giServerName;
          FFEngineManager.TCPIPTransport.RespondToBroadcasts := giTCPIPLFB;
          FFEngineManager.TCPIPTransport.Mode := fftmListen;
          FFEngineManager.TCPIPTransport.EndUpdate;
          ffc_TCPInterface := giTCPInterface;                         {!!.01 Added}
        except
          FFEngineManager.TCPIPTransport.CancelUpdate;
        end;
      end;
      pumDownServer.Enabled := True;                                  {!!.01 Added}
    end;
    { Start the engine manager. }
    FFEngineManager.StartUp;
  finally
    Cursor := SaveCursor;
  end;
end;
{====================================================================}

{===Transport logging================================================}
procedure TfrmFFServer.lvTransportsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  anItem : TListItem;
  aTransport : TffBaseTransport;
  MousePos : TPoint;
begin
  if Button <> mbRight then exit;

  { Find the transport under the mouse. }
  anItem := lvTransports.GetItemAt(X, Y);
  if assigned(anItem) then begin
    aTransport := TffBaseTransport(anItem.Data);

    { Attach the transport to the popup menu so that the popup menu handlers
      may quickly access the transport. }
    lvTransports.Selected := anItem;
    pmuTrans.Tag := longInt(aTransport);

    { Update the menu options to reflect the transport. }
    pmuTransLogErr.Checked := fftpLogErrors in aTransport.EventLogOptions;
    pmuTransLogReq.Checked := fftpLogRequests in aTransport.EventLogOptions;
    pmuTransLogRep.Checked := fftpLogReplies in aTransport.EventLogOptions;
    pmuTransLogAll.Checked := (pmuTranslogErr.Checked and
                               pmuTransLogReq.Checked and
                               pmuTransLogRep.Checked);

    MousePos := lvTransports.ClientToScreen(Point(X, Y));
    pmuTrans.Popup(MousePos.X, MousePos.Y);
  end;
end;
{--------}
procedure TfrmFFServer.pmuTransLogAllClick(Sender: TObject);
var
  aTransport : TffBaseTransport;
begin
  { Enable/Disable logging of all options on the selected transport. }
  pmuTransLogAll.Checked := not pmuTransLogAll.Checked;
  pmuTransLogErr.Checked := pmuTransLogAll.Checked;
  pmuTransLogReq.Checked := pmuTransLogAll.Checked;
  pmuTransLogRep.Checked := pmuTransLogAll.Checked;

  { Get the transport of interest. }
  aTransport := TffBaseTransport(pmuTrans.Tag);

  { Update the transport as required. }
  if pmuTransLogAll.Checked then begin
    aTransport.EventLogOptions := [fftpLogErrors, fftpLogRequests, fftpLogReplies];
    aTransport.EventLogEnabled := True;
  end
  else begin
    aTransport.EventLogOptions := [];
    aTransport.EventLogEnabled := False;
  end;

end;
{--------}
procedure TfrmFFServer.pmuTransLogErrClick(Sender: TObject);
var
  aTransport : TffBaseTransport;
  aSet : TffTransportLogOptions;
begin
  pmuTransLogErr.Checked := not pmuTransLogErr.Checked;
  if not pmuTransLogErr.Checked then
    pmuTransLogAll.Checked := False;

  { Get the transport of interest. }
  aTransport := TffBaseTransport(pmuTrans.Tag);

  aSet := aTransport.EventLogOptions;
  if pmuTransLogErr.Checked then begin
    Include(aSet, fftpLogErrors);
    aTransport.EventLogEnabled := True;
  end else
    Exclude(aSet, fftpLogErrors);
  aTransport.EventLogOptions := aSet;
end;
{--------}
procedure TfrmFFServer.pmuTransLogReqClick(Sender: TObject);
var
  aTransport : TffBaseTransport;
  aSet : TffTransportLogOptions;
begin

  pmuTransLogReq.Checked := not pmuTransLogReq.Checked;
  if not pmuTransLogReq.Checked then
    pmuTransLogAll.Checked := False;

  { Get the transport of interest. }
  aTransport := TffBaseTransport(pmuTrans.Tag);

  aSet := aTransport.EventLogOptions;
  if pmuTransLogReq.Checked then begin
    Include(aSet, fftpLogRequests);
    aTransport.EventLogEnabled := True;
  end else
    Exclude(aSet, fftpLogRequests);
  aTransport.EventLogOptions := aSet;
end;
{--------}
procedure TfrmFFServer.pmuTransLogRepClick(Sender: TObject);
var
  aTransport : TffBaseTransport;
  aSet : TffTransportLogOptions;
begin

  pmuTransLogRep.Checked := not pmuTransLogRep.Checked;
  if not pmuTransLogRep.Checked then
    pmuTransLogAll.Checked := False;

  { Get the transport of interest. }
  aTransport := TffBaseTransport(pmuTrans.Tag);

  aSet := aTransport.EventLogOptions;
  if pmuTransLogRep.Checked then begin
    Include(aSet, fftpLogReplies);
    aTransport.EventLogEnabled := True;
  end else
    Exclude(aSet, fftpLogReplies);
  aTransport.EventLogOptions := aSet;                                 {!!.03}
    
end;
{====================================================================}

{===Tray Icon stuff==================================================}
{$IFDEF UseTrayIcon}
procedure TfrmFFServer.tiAdd;
begin
  if tiPresent and (not tiActive) then begin
    tiInitNotifyData;
    tiActive := Shell_NotifyIcon(NIM_ADD, @tiNotifyData);
  end;
end;
{--------}
procedure TfrmFFServer.tiCallBack(var Msg : TMessage);
var
  P : TPoint;
begin
  if (State = ssUpMinimized) then begin
    with Msg do begin
      case lParam of
        WM_RBUTTONDOWN :
          begin
            GetCursorPos(P);
            SetForegroundWindow(Application.Handle);
            Application.ProcessMessages;
            PopupMenu.Popup(P.X, P.Y);
          end;
        WM_LBUTTONDBLCLK :
          Application.Restore;
      end;{case}
    end;
  end else begin
    case Msg.lParam of
      WM_LBUTTONDOWN :
        SetForegroundWindow(Handle);
      WM_RBUTTONDOWN :
        begin
          SetForegroundWindow(Handle);
          GetCursorPos(P);
          Application.ProcessMessages;
          PopupMenu.Popup(P.X, P.Y);
        end;
    end;
  end;
end;
{--------}
procedure TfrmFFServer.tiDelete;
begin
  if tiPresent and tiActive then begin
    tiActive := not Shell_NotifyIcon(NIM_DELETE, @tiNotifyData);
  end;
end;
{--------}
function TfrmFFServer.tiGetIconHandle : hIcon;
begin
  Result := Application.Icon.Handle;
  if Result = 0 then
    Result := LoadIcon(0, IDI_Application);
end;
{--------}
procedure TfrmFFServer.tiInitNotifyData;
var
  Tip : string;
begin
  if tiPresent then begin
    FillChar(tiNotifyData, sizeof(tiNotifyData), 0);
    with tiNotifyData do begin
      cbSize := sizeof(tiNotifyData);
      Wnd    := Handle;
      uID    := 1;
      uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
      Tip := 'TurboPower FlashFiler [' + ServerName + ']';
      StrCopy(szTip, @Tip[1]);
      uCallBackMessage := ffc_tiCallBack;
      hIcon := tiGetIconHandle;
      Shell_NotifyIcon(NIM_MODIFY, @tiNotifyData)
    end;
  end;
end;
{$ENDIF}
{====================================================================}

procedure TfrmFFServer.btnStartClick(Sender: TObject);
begin
  CreateEngineMgr;
  if not Login then
    Exit;
  BringServerUp;
  SetControls;
end;

procedure TfrmFFServer.btnStopClick(Sender: TObject);
begin
  if not Login then
    Exit;
  BringServerDown;
//  FreeEngineMgr;                                                      {!!.06}{Deleted !!.13}
  SetControls;
end;

procedure TfrmFFServer.btnPropsClick(Sender : TObject);
begin
  mnuConfigGeneralClick(Sender);
end;

procedure TfrmFFServer.FormResize(Sender: TObject);                   {begin !!.06}
begin
  if Width < 605 then
    Width := 605;
  if Height < 300 then
    Height := 300;
end;                                                                  {end !!.06}

procedure TfrmFFServer.HelpTopics1Click(Sender: TObject);
begin
  Application.HelpCommand(HELP_FINDER, 0);
end;

end.

