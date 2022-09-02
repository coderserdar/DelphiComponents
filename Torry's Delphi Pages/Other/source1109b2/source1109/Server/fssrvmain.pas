Unit fssrvmain;

{$I FSDEFINE.INC}

{$DEFINE UseTrayIcon}

Interface

Uses
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  Stdctrls,
  Menus,
  ComCtrls,
  Buttons,
  Windows,
  {$IFDEF UseTrayIcon}
  ShellAPI,
  {$ENDIF}
  fsconst,
  fsnetmsg,
  fsdtmsgq,
  fsllbase,
  fslllgcy,
  fslleng,
  fshash,
  fsllprot,
  fssrbase,
  fssrcfg,
  fsserverclass,
  fssrcmd,
  fsllthrd,
  fssrvegmgr,
  fssrintm,
  {$IFDEF DCC4ORLATER}
  ImgList,
  {$ENDIF}
  ToolWin,
  Tabnotbk;

Resourcestring
  cBrowserError = 'Unable to start web browser. Make sure you have it properly setup on your system.';

Type
  TfsServerState = (
    ssInitializing,
    ssDown,
    ssComingUp,
    ssUp,
    ssUpMinimized);

Const
  {$IFDEF UseTrayIcon}
  fsc_tiCallBack = WM_USER + $300;
  {$ENDIF}
  fsc_Activate = WM_USER + $301;
  fsc_Minimize = WM_USER + $302;

Type
  TfsFSSQLServerForm = Class(TForm)
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
    mnuConfigUsers: TMenuItem;
    mnuConfigAliases: TMenuItem;
    mnuDebug: TMenuItem;
    mnuDebugLog: TMenuItem;
    mnuResetCounters: TMenuItem;
    pnlBig: TPanel;
    pmuTrans: TPopupMenu;
    pmuTransLog: TMenuItem;
    pmuTransLogAll: TMenuItem;
    pmuTransLogSep: TMenuItem;
    pmuTransLogErr: TMenuItem;
    pmuTransLogReq: TMenuItem;
    pmuTransLogRep: TMenuItem;
    TabInfo: TTabbedNotebook;
    Panel1: TPanel;
    lvServers: TListView;
    Panel2: TPanel;
    lvTransports: TListView;
    Panel3: TPanel;
    LClient: TListView;
    Panel4: TPanel;
    btnProps: TSpeedButton;
    btnStart: TSpeedButton;
    btnStop: TSpeedButton;
    SpeedButton4: TSpeedButton;
    Help1: TMenuItem;
    Show1: TMenuItem;
    N2: TMenuItem;
    pumStartServer: TMenuItem;
    Properties1: TMenuItem;
    N4: TMenuItem;
    About1: TMenuItem;
    DataBase1: TMenuItem;
    Users1: TMenuItem;
    D1: TMenuItem;
    DisconnectAllClients1: TMenuItem;
    Help2: TMenuItem;
    Donate1: TMenuItem;
    Forum1: TMenuItem;
    FSSQLhomepage1: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    Show2: TMenuItem;
    N9: TMenuItem;
    N7: TMenuItem;
    Save1: TMenuItem;
    Load1: TMenuItem;
    Saveconfigurationto1: TMenuItem;
    Loadconfigurationfrom1: TMenuItem;
    N8: TMenuItem;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    Procedure mnuConfigAliasesClick(Sender: TObject);
    Procedure mnuDebugLogClick(Sender: TObject);
    Procedure mnuServerExitClick(Sender: TObject);
    Procedure pumDownServerClick(Sender: TObject);
    Procedure pumExitClick(Sender: TObject);
    Procedure FormActivate(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var Action: TCloseAction);
    Procedure FormCreate(Sender: TObject);
    Procedure FormHide(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
    Procedure mnuResetCountersClick(Sender: TObject);
    Procedure mnuConfigUsersClick(Sender: TObject);
    Procedure mnuConfigGeneralClick(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormShow(Sender: TObject);
    Procedure mnuConfigNetworkClick(Sender: TObject);
    Procedure lvServersClick(Sender: TObject);
    Procedure pmuTransLogAllClick(Sender: TObject);
    Procedure pmuTransLogErrClick(Sender: TObject);
    Procedure pmuTransLogReqClick(Sender: TObject);
    Procedure pmuTransLogRepClick(Sender: TObject);
    Procedure lvTransportsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure btnStartClick(Sender: TObject);
    Procedure btnStopClick(Sender: TObject);
    Procedure btnPropsClick(Sender: TObject);
    Procedure ToolButton1Click(Sender: TObject);
    Procedure Help1Click(Sender: TObject);
    Procedure Show1Click(Sender: TObject);
    Procedure pumStartServerClick(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure D1Click(Sender: TObject);
    Procedure Properties1Click(Sender: TObject);
    Procedure DataBase1Click(Sender: TObject);
    Procedure Users1Click(Sender: TObject);
    Procedure FSSQLhomepage1Click(Sender: TObject);
    Procedure Forum1Click(Sender: TObject);
    Procedure Donate1Click(Sender: TObject);
    Procedure Save1Click(Sender: TObject);
    Procedure Load1Click(Sender: TObject);
    Procedure Loadconfigurationfrom1Click(Sender: TObject);
    Procedure Saveconfigurationto1Click(Sender: TObject);
  Private
    { Private d
    procedure FormResize(Sender: TObject);eclarations }
    FIgnoreNextLogin: Boolean;
    FState: TfsServerState;
    FStartTime: TDateTime;
    FMsgStartTime: TDateTime;
    cuIsGreen: Boolean;
    FCloseClick: Boolean;
    FElapsedTime: TDateTime;
    FMustClose: Boolean;
    FClosingFromTray: Boolean;
    {$IFDEF UseTrayIcon}
    tiActive: Boolean;
    tiNotifyData: TNotifyIconData;
    tiPresent: Boolean;
    {$ENDIF}

    Procedure AppException(Sender: TObject; E: Exception);
    Procedure DisplayHint(Sender: TObject);
    Function Login: Boolean;
    Procedure MainFormMinimize(Sender: TObject);
    Procedure MainFormRestore(Sender: TObject);

    {$IFDEF UseTrayIcon}
    Procedure tiAdd;
    Procedure tiCallBack(Var Msg: TMessage); Message fsc_tiCallBack;
    Procedure tiDelete;
    Function tiGetIconHandle: hIcon;
    Procedure tiInitNotifyData;
    {$ENDIF}
  Public
    { Public declarations }
    ServerName: TffNetAddress;
    fView: boolean;
    Procedure BringServerUp;
    Procedure BringServerDown;
    Procedure CreateEngineMgr;
    Procedure FreeEngineMgr; {!!.06}
    Procedure DownServer;
    Function ElapsedTimeToStr(T: TDateTime): String;
    Procedure FFCActivate(Var Msg: TMessage); Message fsc_Activate;
    Procedure FFCMinimize(Var Msg: TMessage); Message fsc_Minimize;
    Function GetMsgsPerSecAsString(aMsgCount: Longint): String;
    Procedure LoadServers;
    Procedure LoadTransports;
    Procedure ResetStatistics;
    Procedure SetControls;
    Procedure SetServerName;
    Procedure SetServerPriority(aPriority: Longint);
    Procedure SetState(S: TfsServerState);
    Procedure UpdateServers;
    Procedure UpdateTransports;
    Procedure UpServer;
    Procedure WMSysCommand(Var Msg: TMessage); Message WM_SYSCOMMAND;
    Procedure WMQueryEndSession(Var msg: TMessage); Message WM_QUERYENDSESSION;

    Property State: TfsServerState Read FState Write SetState;
  End;

Var
  fsFSSQLServerForm: TfsFSSQLServerForm;

Implementation

Uses
  FsLLComm,
  FsLLComp,
  fssrvalas,
  fssrvuser,
  fssrvgenl,
  FsSrJour,
  FsLogDlg,
  FsSrBDE,
  fssrvcfg,
  FsLLExcp,
  About;

{$R *.DFM}

{===Main Menu options================================================}

Procedure TfsFSSQLServerForm.mnuConfigAliasesClick(Sender: TObject);
Begin
  CreateEngineMgr;
  With TfsAliasForm.Create(Application) Do
    Try
      ServerEngine := TFSServer(lvServers.Selected.Data);
      ShowModal;
    Finally
      Free;
    End;
End;
{--------}

Procedure TfsFSSQLServerForm.mnuConfigGeneralClick(Sender: TObject);
Begin
  CreateEngineMgr;
  With TfsGenConfigForm.Create(Application) Do
    Try
      ServerEngine := TFSServer(lvServers.Selected.Data);
      ShowModal;
      { Get the bits in which we're interested. }
      With ServerEngine.Configuration.GeneralInfo^ Do
        Begin
          ServerName := giServerName;
          ServerEngine.MaxRAM := giMaxRAM; {!!.01}
          SetServerName;
          mnuDebugLog.Checked := giDebugLog;
          FSEngineManager.EventLogEnabled := giDebugLog;
          SetServerPriority(giPriority);
          If (State = ssUp) Then
            With ServerEngine.Configuration.GeneralInfo^ Do
              Begin
                If Assigned(FSEngineManager.IPXSPXTransport) Then
                  FSEngineManager.IPXSPXTransport.RespondToBroadcasts := giIPXSPXLFB;
                If Assigned(FSEngineManager.TCPIPTransport) Then
                  FSEngineManager.TCPIPTransport.RespondToBroadcasts := giTCPIPLFB;
              End;
        End;
      SetControls;
    Finally
      Free;
    End;
  lvServers.Selected.Caption := ServerName;
End;
{--------}

Procedure TfsFSSQLServerForm.mnuConfigNetworkClick(Sender: TObject);
Begin

End;
{--------}

Procedure TfsFSSQLServerForm.mnuConfigUsersClick(Sender: TObject);
Begin
  CreateEngineMgr;
  With TFSUserForm.Create(Application) Do
    Try
      ServerEngine := TFSServer(lvServers.Selected.Data);
      ShowModal;
    Finally
      Free;
    End;
End;
{--------}

Procedure TfsFSSQLServerForm.mnuDebugLogClick(Sender: TObject);
Begin
  CreateEngineMgr;
  mnuDebugLog.Checked := Not mnuDebugLog.Checked;
  With TFSServer(lvServers.Selected.Data) Do
    Begin
      Configuration.GeneralInfo^.giDebugLog := mnuDebugLog.Checked;
      SaveConfiguration;
      FSEngineManager.EventLogEnabled := mnuDebugLog.Checked;
    End;
End;

Procedure TfsFSSQLServerForm.mnuResetCountersClick(Sender: TObject);
Begin
  FMsgStartTime := Now;
  ResetStatistics;
End;
{--------}

Procedure TfsFSSQLServerForm.mnuServerExitClick(Sender: TObject);
Begin
  Close;
End;
{====================================================================}

{===Popup menu options===============================================}

Procedure TfsFSSQLServerForm.pumDownServerClick(Sender: TObject);
Begin
  If Not Login Then
    Exit;
  FIgnoreNextLogin := True;
  Application.Restore;
  Application.ProcessMessages;
  BringServerDown;
End;
{--------}

Procedure TfsFSSQLServerForm.pumExitClick(Sender: TObject);
Begin
  FClosingFromTray := True;
  Application.Restore;
  Close;
End;
{====================================================================}

{===Message handlers=================================================}

Procedure TfsFSSQLServerForm.FFCActivate(Var Msg: TMessage);
Begin
  Update;
  BringServerUp;
End;
{--------}

Procedure TfsFSSQLServerForm.FFCMinimize(Var Msg: TMessage);
Begin
  Application.Minimize;
End;
{--------}

Procedure TfsFSSQLServerForm.WMQueryEndSession(Var msg: TMessage);
Begin
  FMustClose := True;
  Inherited;
End;
{--------}

Procedure TfsFSSQLServerForm.WMSysCommand(Var Msg: TMessage);
Begin
  If (((Msg.wParam) And $FFF0) = SC_CLOSE) Then
    FCloseClick := True;
  Inherited;
End;
{====================================================================}

{===Timer handler====================================================}

Procedure TfsFSSQLServerForm.Timer1Timer(Sender: TObject);
Begin
  If State = ssUpMinimized Then {!!.11}
    Exit; {!!.11}
  Try
    Case State Of
      ssComingUp:
        Begin
          cuIsGreen := Not cuIsGreen;
        End;
    End; {case}
    If pnlBig.Visible Then
      Begin
        UpdateServers;
        UpdateTransports;
        Case State Of
          ssUp:
            Begin
              FElapsedTime := Now - FStartTime;
            End;
        End; {case}
      End;
  Except
  End;
End;
{====================================================================}

{===Form events======================================================}

Procedure TfsFSSQLServerForm.FormActivate(Sender: TObject);
{$IFDEF UseTrayIcon}
Var
  OSVerInfo: TOSVersionInfo;
  {$ENDIF}
Begin
  {$IFDEF UseTrayIcon}
  tiPresent := False;
  OSVerInfo.dwOSVersionInfoSize := sizeof(OSVerInfo);
  If GetVersionEx(OSVerInfo) Then
    Begin
      {Note: Windows95 returns version major:minor = 4:0}
      If (OSVerInfo.dwPlatformID = VER_PLATFORM_WIN32_WINDOWS) Or {Windows95}
      (OSVerInfo.dwPlatformID = VER_PLATFORM_WIN32_NT) Then {WindowsNT}
        tiPresent := OSVerInfo.dwMajorVersion > 3
    End;
  {$ENDIF}

  If Assigned(FSEngineManager) And Assigned(lvServers.Selected) Then {!!.13}
    If TFSServer(lvServers.Selected.Data).Configuration.GeneralInfo^.giAutoMini {!!.13}
    And (State = ssInitializing) Then
      FIgnoreNextLogin := True; {!!.13}

  If Not Login Then
    Begin
      PostMessage(Handle, WM_QUIT, 0, 0);
      Exit;
    End;
  Try
    If assigned(FSEngineManager) And assigned(lvServers.Selected) Then {!!.02}
      With TFSServer(lvServers.Selected.Data).Configuration.GeneralInfo^ Do
        Begin
          {..server name, etc}
          ServerName := giServerName;
          TFSServer(lvServers.Selected.Data).BufferManager.MaxRAM := giMaxRAM;
          {..ports}
          FsSetTCPPort(giTCPPort);
          FsSetUDPPortServer(giUDPPortSr);
          FsSetUDPPortClient(giUDPPortCl);
          FsSetIPXSocketServer(giIPXSocketSr);
          FsSetIPXSocketClient(giIPXSocketCl);
          FsSetSPXSocket(giSPXSocket);
          {..keepalive stuff}
          fsc_LastMsgInterval := giLastMsgInterval;
          fsc_KeepAliveInterval := giKAInterval;
          fsc_KeepAliveRetries := giKARetries;
          {..priority}
          SetServerPriority(giPriority);
          {..auto up}
          If giAutoUp And (ServerName <> '') Then
            PostMessage(Handle, fsc_Activate, 0, 0);
          {..auto minimize}
          If giAutoMini Then
            PostMessage(Handle, fsc_Minimize, 0, 0);
        End;
    Application.ShowHint := True;
    State := ssDown;
    SetServerName;
    {$IFDEF UseTrayIcon}
    If tiPresent Then
      tiAdd;
    {$ENDIF}
  Except
    {????}
    Raise;
  End;
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
End;
{--------}

Procedure TfsFSSQLServerForm.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  If FMustClose Then
    Begin
      If (State <> ssDown) Then
        Begin
          If (State = ssUpMinimized) Then
            Application.Restore;
          BringServerDown;
        End;
      CanClose := True;
    End
  Else
    Begin
      If (State = ssUp) And FCloseClick Then
        Begin
          FCloseClick := False;
          Application.Minimize;
        End
      Else If (State <> ssDown) Then
        Begin
          If Login Then
            Begin
              If (State = ssUpMinimized) Then
                Application.Restore;
              BringServerDown;
            End;
        End;
      CanClose := (State = ssDown);
    End;
End;
{--------}

Procedure TfsFSSQLServerForm.FormClose(Sender: TObject; Var Action: TCloseAction);
Begin
  {$IFDEF UseTrayIcon}
  tiDelete;
  {$ENDIF}
End;
{--------}

Procedure TfsFSSQLServerForm.FormCreate(Sender: TObject);
Begin
  fView := True;
  ToolButton1Click(Nil);
  FClosingFromTray := False;
  FIgnoreNextLogin := False;
  State := ssInitializing;
  Application.OnMinimize := MainFormMinimize;
  Application.OnRestore := MainFormRestore;
  Application.OnException := AppException;
  Application.OnHint := DisplayHint;
  FSEngineManager := Nil;
  IsMultiThread := True;
  pumDownServer.Enabled := False;
End;
{--------}

Procedure TfsFSSQLServerForm.AppException(Sender: TObject; E: Exception);
Begin
  Application.ShowException(E)
End;
{--------}

Procedure TfsFSSQLServerForm.DisplayHint(Sender: TObject);
Begin
End;
{--------}

Procedure TfsFSSQLServerForm.FormDestroy(Sender: TObject);
Begin
  FreeEngineMgr;
End;
{--------}

Procedure TfsFSSQLServerForm.FormHide(Sender: TObject);
Begin
  ShowWindow(Application.Handle, SW_HIDE);
End;
{--------}

Procedure TfsFSSQLServerForm.FormShow(Sender: TObject);
Begin
  CreateEngineMgr;
  Try
    LoadServers;
    LoadTransports;
  Finally
    SetControls;
  End;
End;
{--------}

Function TfsFSSQLServerForm.Login: Boolean;
Var
  LoginDlg: TFsLoginDialog;
  Hash: TffWord32;
  Pwd: TffName;
  Server: TFSServer;
  User: TfsUserItem;
  UserInx: Integer;
  UserID: TffName;
Begin
  If FMustClose Then
    Begin {!!.11}
      Result := True; {!!.11}
      Exit; {!!.11}
    End; {!!.11}
  If FIgnoreNextLogin Then
    Begin
      FIgnoreNextLogin := False;
      Result := True;
      Exit;
    End;
  Server := Nil;
  If lvServers.SelCount > 0 Then
    Server := TFSServer(lvServers.Selected.Data);
  If (Not assigned(Server)) Or
    (Not Server.Configuration.GeneralInfo^.giIsSecure) Then
    Begin
      Result := True;
      Exit;
    End;
  Result := False;
  LoginDlg := TFsLoginDialog.Create(Application);
  Try
    If (LoginDlg.ShowModal = mrOK) Then
      Begin
        UserID := LoginDlg.edtUserName.Text;
        Pwd := LoginDlg.edtPassword.Text;
        UserInx := Server.Configuration.UserList.UserIndex(UserID);
        If (UserInx <> -1) Then
          Begin
            User := Server.Configuration.UserList[UserInx];
            Hash := FSCalcShStrElfHash(Pwd);
            If (Hash = User.PasswordHash) And
              (arAdmin In User.Rights) Then
              Result := True;
          End
      End;
  Finally
    LoginDlg.Free;
  End; {try..finally}
End;
{--------}

Procedure TfsFSSQLServerForm.MainFormMinimize(Sender: TObject);
Begin
  If (State = ssUp) Then
    State := ssUpMinimized;
  {$IFDEF UseTrayIcon}
  If tiPresent And (State = ssUpMinimized) Then
    Begin
      Hide;
    End;
  {$ENDIF}
End;
{--------}

Procedure TfsFSSQLServerForm.MainFormRestore(Sender: TObject);
Begin
  {$IFDEF UseTrayIcon}
  If tiPresent And (State = ssUpMinimized) Then
    Begin
      Show;
      SetForegroundWindow(Handle);
    End;
  {$ENDIF}
  If (State = ssUpMinimized) Then
    State := ssUp;
  If (Not FClosingFromTray) And (Not Login) Then
    Application.Minimize;
End;
{--------}

{====================================================================}

{===property handlers================================================}

Procedure TfsFSSQLServerForm.SetState(S: TfsServerState);
Begin
  If (S = FState) And (S <> ssInitializing) Then
    Exit;
  FState := S;
  Case S Of
    ssInitializing:
      Begin
        {..edit controls}
        //lblTime.Caption := ElapsedTimeToStr(0.0);
      End;
    ssDown:
      Begin
        {..menu items}
        If (ServerName = '') Then
          mnuServerUp.Enabled := False
        Else
          mnuServerUp.Enabled := True;
        mnuServerDown.Enabled := False;
        If ((assigned(FSEngineManager)) And (lvServers.SelCount > 0)) Then
          With TFSServer(lvServers.Selected.Data).Configuration Do
            Begin
              FSEngineManager.EventLogEnabled := mnuDebugLog.Checked;
            End;

        {..speedbutton actions}
        If (ServerName = '') Then
          btnStart.Enabled := False
        Else
          btnStart.Enabled := True;
        btnStop.Enabled := False;

        {..timer}
        Timer1.Enabled := False;
      End;
    ssComingUp:
      Begin
        {..menu items}
        mnuServerUp.Enabled := False;
        {..speedbutton actions}
        btnStart.Enabled := False;
        {..form fields}
        FStartTime := Now;
        FMsgStartTime := Now;
        {..timer}
        Timer1.Enabled := True;
      End;
    ssUp:
      Begin
        {..menu items}
        mnuServerUp.Enabled := False;
        mnuServerDown.Enabled := True;
        {..speedbutton actions}
        btnStart.Enabled := False;
        btnStop.Enabled := True;
        {..online indicator}
        {..timer}
        Timer1.Enabled := True;
      End;
    ssUpMinimized:
      Begin
        {..timer}
        Timer1.Enabled := False;
      End;
  End; {case}
End;
{====================================================================}

{===Utility methods==================================================}

Procedure TfsFSSQLServerForm.BringServerDown;
Begin
  If (State = ssUp) Then
    Begin
      DownServer;
      State := ssDown;
    End;
End;
{--------}

Procedure TfsFSSQLServerForm.BringServerUp;
Begin
  If (State = ssDown) Then
    Begin
      Timer1.Interval := 250;
      cuIsGreen := False;
      State := ssComingUp;
      Try
        UpServer;
        Timer1.Interval := 2000;
        State := ssUp;
      Except
        State := ssDown;
        Raise;
      End; {try..except}
    End;
  LoadServers;
  LoadTransports;
End;
{--------}

Procedure TfsFSSQLServerForm.CreateEngineMgr;
Begin
  If Not assigned(FSEngineManager) Then
    Begin
      FSEngineManager := TFSEngineManager.Create(Nil);
      If ParamCount > 0 Then
        FSEngineManager.ScriptFile := ParamStr(1);
    End;
End;
{--------}

Procedure TfsFSSQLServerForm.FreeEngineMgr; {!!.06}
Begin
  If assigned(FSEngineManager) Then
    Try
      FSEngineManager.ShutDown;
    Finally
      FSEngineManager.Free;
      FSEngineManager := Nil;
    End;
End;
{--------}

Procedure TfsFSSQLServerForm.DownServer;
Begin
  Screen.Cursor := crHourGlass;
  Try
    If assigned(FSEngineManager) Then
      Begin
        pumDownServer.Enabled := False;
        pumStartServer.Enabled := True;
        FSEngineManager.ShutDown;
      End;
  Finally
    Screen.Cursor := crDefault;
  End;
  ResetStatistics;
  Timer1Timer(Self);
End;
{--------}

Function TfsFSSQLServerForm.ElapsedTimeToStr(T: TDateTime): String;
Var
  Dy: Integer;
  Hr: Integer;
  Mi: Integer;
  Se: Integer;
  WorkSt: String[9];
Begin
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
  Str(Dy: 5, WorkSt);
  Move(WorkSt[1], Result[5], 5);
  Str(Hr: 2, WorkSt);
  Result[12] := WorkSt[2];
  If (Hr > 9) Then
    Result[11] := WorkSt[1];
  Str(Mi: 2, WorkSt);
  Result[15] := WorkSt[2];
  If (Mi > 9) Then
    Result[14] := WorkSt[1];
  Str(Se: 2, WorkSt);
  Result[18] := WorkSt[2];
  If (Se > 9) Then
    Result[17] := WorkSt[1];
End;
{--------}

Function TfsFSSQLServerForm.GetMsgsPerSecAsString(aMsgCount: Integer): String;
Var
  MsgsPerSec: Double;
Begin
  If (FElapsedTime > 0.0) Then
    MsgsPerSec := aMsgCount / (FElapsedTime * 86400.0)
  Else
    MsgsPerSec := 0.0;
  Str(MsgsPerSec: 0: 4, Result);
End;
{--------}

Procedure TfsFSSQLServerForm.LoadServers;
Var
  ListItem: TListItem;
  SelServIdx: Integer;
  Servers: TFSNormalList;
  i: Integer;
Begin
  If assigned(FSEngineManager) Then
    Begin
      If lvServers.SelCount > 0 Then
        SelServIdx := lvServers.Selected.Index
      Else
        SelServIdx := 0;
      lvServers.Items.BeginUpdate;
      Servers := TFSNormalList.Create;
      Try
        lvServers.Items.Clear;
        FSEngineManager.GetServerEngines(Servers);
        For i := 0 To Pred(Servers.Count) Do
          Begin
            ListItem := lvServers.Items.Add;
            With TFSServer(TfsIntListItem(Servers[i]).KeyAsInt) Do
              Begin
                ListItem.Caption := Configuration.ServerName;
                ListItem.Data := Pointer(TfsIntListItem(Servers[i]).KeyAsInt);
                ListItem.SubItems.Add(FSMapStateToString(State));
                ListItem.SubItems.Add(FsCommaizeChL(ClientList.ClientCount, ThousandSeparator));
                ListItem.SubItems.Add(FsCommaizeChL(SessionList.SessionCount, ThousandSeparator));
                ListItem.SubItems.Add(FsCommaizeChL(DatabaseList.DatabaseCount, ThousandSeparator));
                ListItem.SubItems.Add(FsCommaizeChL(TableList.TableCount, ThousandSeparator));
                ListItem.SubItems.Add(FsCommaizeChL(CursorList.CursorCount, ThousandSeparator));
                ListItem.SubItems.Add(FsCommaizeChL(BufferManager.RAMUsed, ThousandSeparator));
              End;
            If i = SelServIdx Then
              lvServers.Selected := ListItem;
          End;
      Finally
        Servers.Free;
        lvServers.Items.EndUpdate;
      End;
    End;
End;
{--------}

Procedure TfsFSSQLServerForm.LoadTransports;
Var
  i: Integer;
  NewTransItem: TListItem;
  ListItem: TListItem;
  Transports: TFSNormalList;
  sv: TFSServer;
Begin
  Transports := TFSNormalList.Create;
  Try
    lvTransports.Items.Clear;
    If lvServers.SelCount > 0 Then
      Begin
        sv := TFSServer(lvServers.Selected.Data);
        If assigned(FSEngineManager) Then
          Begin
            FSEngineManager.GetTransports(TFSServer(lvServers.Selected.Data), Transports);
            lvTransports.Items.BeginUpdate;
            Try
              lvTransports.Items.Clear;
              For i := 0 To Pred(Transports.Count) Do
                Begin
                  With TFSBaseTransport(TfsIntListItem(Transports[i]).KeyAsInt) Do
                    Begin
                      NewTransItem := lvTransports.Items.Add;
                      NewTransItem.Caption := GetName;
                      NewTransItem.Data := Pointer(TfsIntListItem(Transports[i]).KeyAsInt);
                      NewTransItem.SubItems.Add(ServerName);
                      NewTransItem.SubItems.Add(FSMapStateToString(State));
                      NewTransItem.SubItems.Add(FsCommaizeChL(ConnectionCount, ThousandSeparator));
                      NewTransItem.SubItems.Add(FsCommaizeChL(MsgCount, ThousandSeparator));
                      NewTransItem.SubItems.Add(GetMsgsPerSecAsString(MsgCount));
                    End;
                End;
            Finally
              lvTransports.Items.EndUpdate;
            End;
            lClient.Items.BeginUpdate;
            Try
              lClient.Items.Clear;
              If Sv.ClientList.ClientCount > 0 Then
                Begin
                  For i := 0 To Sv.ClientList.ClientCount - 1 Do
                    Begin
                      ListItem := lClient.Items.Add;
                      ListItem.Caption := Sv.ClientList.Client[ftFromIndex, i].ClientName;
                      ListItem.SubItems.Add(IntToStr(Sv.ClientList.Client[ftFromIndex, i].ClientId));
                      ListItem.SubItems.Add(IntToStr(Sv.ClientList.Client[ftFromIndex, i].ClientVersion));
                      ListItem.SubItems.Add(IntToStr(Sv.ClientList.Client[ftFromIndex, i].Timeout));
                    End;
                End;
            Finally
              lClient.Items.EndUpdate;
            End;

          End;
      End;
  Finally
    Transports.Free;
  End;
End;
{--------}

Procedure TfsFSSQLServerForm.lvServersClick(Sender: TObject);
Begin
  LoadTransports;
  SetControls;
End;
{--------}

Procedure TfsFSSQLServerForm.ResetStatistics;
Var
  i: Integer;
Begin
  For i := 0 To pred(lvTransports.Items.Count) Do
    Begin
      With TFSBaseTransport(lvTransports.Items[i].Data) Do
        Begin
          ResetMsgCount;
        End;
    End;
End;
{--------}

Procedure TfsFSSQLServerForm.SetControls;
Var
  SelectedServer: TFSServer;
  IsServerSelected: Boolean;
Begin
  SelectedServer := Nil;
  IsServerSelected := lvServers.SelCount > 0;
  If IsServerSelected Then
    Begin
      SelectedServer := TFSServer(lvServers.Selected.Data);
    End
  Else
    Begin
      mnuDebugLog.Checked := False;
    End;
  mnuConfigGeneral.Enabled := IsServerSelected;
  mnuConfigAliases.Enabled := IsServerSelected;
  mnuConfigUsers.Enabled := IsServerSelected;
  btnProps.Enabled := IsServerSelected;
  If IsServerSelected And Assigned(SelectedServer) Then
    mnuDebugLog.Enabled := Not SelectedServer.Configuration.GeneralInfo^.giReadOnly
  Else
    mnuDebugLog.Enabled := False;
  mnuResetCounters.Enabled := IsServerSelected;
End;
{--------}

Procedure TfsFSSQLServerForm.SetServerName;
Begin
  If assigned(FSEngineManager) And assigned(lvServers.Selected) Then {!!.02}
    With TFSServer(lvServers.Selected.Data) Do
      Configuration.GeneralInfo^.giServerName := ServerName;
  If (ServerName <> '') Then
    Begin
      Caption := 'FSSQL: ' + ServerName;
      Application.Title := Caption;
      If (State = ssDown) Then
        Begin
          mnuServerUp.Enabled := True;
          btnStart.Enabled := True;
        End;
      {$IFDEF UseTrayIcon}
      tiInitNotifyData;
      {$ENDIF}
    End
  Else
    Begin
      Caption := 'FSSQL DataBase';
      mnuServerUp.Enabled := False;
      btnStart.Enabled := False;
    End;
End;
{--------}

Procedure TfsFSSQLServerForm.SetServerPriority(aPriority: Longint);
Const
  ThreadPriority: Array[0..4] Of Integer =
  (THREAD_PRIORITY_LOWEST,
    THREAD_PRIORITY_BELOW_NORMAL,
    THREAD_PRIORITY_NORMAL,
    THREAD_PRIORITY_ABOVE_NORMAL,
    THREAD_PRIORITY_HIGHEST);
Begin
  If (aPriority < -2) Or (aPriority > 2) Then
    aPriority := 2
  Else
    inc(aPriority, 2);
  SetThreadPriority(GetCurrentThread, ThreadPriority[aPriority]);
End;
{--------}

Procedure TfsFSSQLServerForm.UpdateServers;
Var
  i: Integer;
  ListItem: TListItem;
  Sv: TFSServer;
Begin
  For i := 0 To Pred(lvServers.Items.Count) Do
    Begin
      Sv := TFSServer(lvServers.Items[i].Data);
      With lvServers.Items[i], Sv Do
        Begin
          SubItems[0] := FSMapStateToString(State);
          SubItems[1] := FsCommaizeChL(ClientList.ClientCount, ThousandSeparator);
          SubItems[2] := FsCommaizeChL(SessionList.SessionCount, ThousandSeparator);
          SubItems[3] := FsCommaizeChL(DatabaseList.DatabaseCount, ThousandSeparator);
          SubItems[4] := FsCommaizeChL(TableList.TableCount, ThousandSeparator);
          SubItems[5] := FsCommaizeChL(CursorList.CursorCount, ThousandSeparator);
          SubItems[6] := FsCommaizeChL(BufferManager.RAMUsed, ThousandSeparator);
        End;
    End;
  If lvServers.SelCount > 0 Then
    Begin
      sv := TFSServer(lvServers.Selected.Data);
      lClient.Items.BeginUpdate;
      Try
        lClient.Items.Clear;
        If Sv.ClientList.ClientCount > 0 Then
          Begin
            For i := 0 To Sv.ClientList.ClientCount - 1 Do
              Begin
                ListItem := lClient.Items.Add;
                ListItem.Caption := Sv.ClientList.Client[ftFromIndex, i].ClientName;
                ListItem.SubItems.Add(IntToStr(Sv.ClientList.Client[ftFromIndex, i].ClientId));
                ListItem.SubItems.Add(IntToStr(Sv.ClientList.Client[ftFromIndex, i].ClientVersion));
                ListItem.SubItems.Add(IntToStr(Sv.ClientList.Client[ftFromIndex, i].Timeout));
              End;
          End;
      Finally
        lClient.Items.EndUpdate;
      End;
    End;

End;
{--------}

Procedure TfsFSSQLServerForm.UpdateTransports;
Var
  i: Integer;
Begin
  For i := 0 To pred(lvTransports.Items.Count) Do
    Begin
      With lvTransports.Items[i],
        TFSBaseTransport(lvTransports.Items[i].Data) Do
        Begin
          SubItems[0] := ServerName;
          SubItems[1] := FSMapStateToString(State);
          SubItems[2] := FsCommaizeChL(ConnectionCount, ThousandSeparator);
          SubItems[3] := FsCommaizeChL(MsgCount, ThousandSeparator);
          SubItems[4] := GetMsgsPerSecAsString(MsgCount);
        End;
    End;
End;
{--------}

Procedure TfsFSSQLServerForm.UpServer;
Var
  SaveCursor: TCursor;
Begin
  SaveCursor := Cursor;
  Cursor := crHourglass;
  Try
    { Create & set up the transports. }
    With FSEngineManager.ServerEngine.Configuration.GeneralInfo^ Do
      Begin
        FsSetTCPPort(giTCPPort);
        FsSetUDPPortServer(giUDPPortSr);
        FsSetUDPPortClient(giUDPPortCl);
        FsSetIPXSocketServer(giIPXSocketSr);
        FsSetIPXSocketClient(giIPXSocketCl);
        FsSetSPXSocket(giSPXSocket);

        FSEngineManager.SUPTransport.Enabled := giSingleUser;
        If giSingleUser Then
          Begin
            FSEngineManager.SUPTransport.BeginUpdate;
            Try
              FSEngineManager.SUPTransport.ServerName := giServerName;
              FSEngineManager.SUPTransport.Mode := fstmListen;
              FSEngineManager.SUPTransport.EndUpdate;
            Except
              FSEngineManager.SUPTransport.CancelUpdate;
            End;
          End;

        FSEngineManager.IPXSPXTransport.Enabled := giIPXSPX;
        If giIPXSPX Then
          Begin
            FSEngineManager.IPXSPXTransport.BeginUpdate;
            Try
              FSEngineManager.IPXSPXTransport.ServerName := giServerName;
              FSEngineManager.IPXSPXTransport.RespondToBroadcasts := giIPXSPXLFB;
              FSEngineManager.IPXSPXTransport.Mode := fstmListen;
              FSEngineManager.IPXSPXTransport.EndUpdate;
            Except
              FSEngineManager.IPXSPXTransport.CancelUpdate;
            End;
          End;

        FSEngineManager.TCPIPTransport.Enabled := giTCPIP;
        If giTCPIP Then
          Begin
            FSEngineManager.TCPIPTransport.BeginUpdate;
            Try
              FSEngineManager.TCPIPTransport.ServerName := giServerName;
              FSEngineManager.TCPIPTransport.RespondToBroadcasts := giTCPIPLFB;
              FSEngineManager.TCPIPTransport.Mode := fstmListen;
              FSEngineManager.TCPIPTransport.EndUpdate;
              fsc_TCPInterface := giTCPInterface;
            Except
              FSEngineManager.TCPIPTransport.CancelUpdate;
            End;
          End;
        pumDownServer.Enabled := True;
        pumStartServer.Enabled := False;
      End;
    { Start the engine manager. }
    FSEngineManager.StartUp;
  Finally
    Cursor := SaveCursor;
  End;
End;
{====================================================================}

{===Transport logging================================================}

Procedure TfsFSSQLServerForm.lvTransportsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  anItem: TListItem;
  aTransport: TFSBaseTransport;
  MousePos: TPoint;
Begin
  If Button <> mbRight Then
    Exit;

  { Find the transport under the mouse. }
  anItem := lvTransports.GetItemAt(X, Y);
  If assigned(anItem) Then
    Begin
      aTransport := TFSBaseTransport(anItem.Data);

      { Attach the transport to the popup menu so that the popup menu handlers
        may quickly access the transport. }
      lvTransports.Selected := anItem;
      pmuTrans.Tag := Longint(aTransport);

      { Update the menu options to reflect the transport. }
      pmuTransLogErr.Checked := fstpLogErrors In aTransport.EventLogOptions;
      pmuTransLogReq.Checked := fstpLogRequests In aTransport.EventLogOptions;
      pmuTransLogRep.Checked := fstpLogReplies In aTransport.EventLogOptions;
      pmuTransLogAll.Checked := (pmuTranslogErr.Checked And
        pmuTransLogReq.Checked And
        pmuTransLogRep.Checked);

      MousePos := lvTransports.ClientToScreen(Point(X, Y));
      pmuTrans.Popup(MousePos.X, MousePos.Y);
    End;
End;
{--------}

Procedure TfsFSSQLServerForm.pmuTransLogAllClick(Sender: TObject);
Var
  aTransport: TFSBaseTransport;
Begin
  { Enable/Disable logging of all options on the selected transport. }
  pmuTransLogAll.Checked := Not pmuTransLogAll.Checked;
  pmuTransLogErr.Checked := pmuTransLogAll.Checked;
  pmuTransLogReq.Checked := pmuTransLogAll.Checked;
  pmuTransLogRep.Checked := pmuTransLogAll.Checked;

  { Get the transport of interest. }
  aTransport := TFSBaseTransport(pmuTrans.Tag);

  { Update the transport as required. }
  If pmuTransLogAll.Checked Then
    Begin
      aTransport.EventLogOptions := [fstpLogErrors, fstpLogRequests, fstpLogReplies];
      aTransport.EventLogEnabled := True;
    End
  Else
    Begin
      aTransport.EventLogOptions := [];
      aTransport.EventLogEnabled := False;
    End;

End;
{--------}

Procedure TfsFSSQLServerForm.pmuTransLogErrClick(Sender: TObject);
Var
  aTransport: TFSBaseTransport;
  aSet: TfsTransportLogOptions;
Begin
  pmuTransLogErr.Checked := Not pmuTransLogErr.Checked;
  If Not pmuTransLogErr.Checked Then
    pmuTransLogAll.Checked := False;

  { Get the transport of interest. }
  aTransport := TFSBaseTransport(pmuTrans.Tag);

  aSet := aTransport.EventLogOptions;
  If pmuTransLogErr.Checked Then
    Begin
      Include(aSet, fstpLogErrors);
      aTransport.EventLogEnabled := True;
    End
  Else
    Exclude(aSet, fstpLogErrors);
  aTransport.EventLogOptions := aSet;
End;
{--------}

Procedure TfsFSSQLServerForm.pmuTransLogReqClick(Sender: TObject);
Var
  aTransport: TFSBaseTransport;
  aSet: TfsTransportLogOptions;
Begin

  pmuTransLogReq.Checked := Not pmuTransLogReq.Checked;
  If Not pmuTransLogReq.Checked Then
    pmuTransLogAll.Checked := False;

  { Get the transport of interest. }
  aTransport := TFSBaseTransport(pmuTrans.Tag);

  aSet := aTransport.EventLogOptions;
  If pmuTransLogReq.Checked Then
    Begin
      Include(aSet, fstpLogRequests);
      aTransport.EventLogEnabled := True;
    End
  Else
    Exclude(aSet, fstpLogRequests);
  aTransport.EventLogOptions := aSet;
End;
{--------}

Procedure TfsFSSQLServerForm.pmuTransLogRepClick(Sender: TObject);
Var
  aTransport: TFSBaseTransport;
  aSet: TfsTransportLogOptions;
Begin

  pmuTransLogRep.Checked := Not pmuTransLogRep.Checked;
  If Not pmuTransLogRep.Checked Then
    pmuTransLogAll.Checked := False;

  { Get the transport of interest. }
  aTransport := TFSBaseTransport(pmuTrans.Tag);

  aSet := aTransport.EventLogOptions;
  If pmuTransLogRep.Checked Then
    Begin
      Include(aSet, fstpLogReplies);
      aTransport.EventLogEnabled := True;
    End
  Else
    Exclude(aSet, fstpLogReplies);
  aTransport.EventLogOptions := aSet; {!!.03}

End;
{====================================================================}

{===Tray Icon stuff==================================================}
{$IFDEF UseTrayIcon}

Procedure TfsFSSQLServerForm.tiAdd;
Begin
  If tiPresent And (Not tiActive) Then
    Begin
      tiInitNotifyData;
      tiActive := Shell_NotifyIcon(NIM_ADD, @tiNotifyData);
    End;
End;
{--------}

Procedure TfsFSSQLServerForm.tiCallBack(Var Msg: TMessage);
Var
  P: TPoint;
Begin
  If (State = ssUpMinimized) Then
    Begin
      With Msg Do
        Begin
          Case lParam Of
            WM_RBUTTONDOWN:
              Begin
                GetCursorPos(P);
                SetForegroundWindow(Application.Handle);
                Application.ProcessMessages;
                PopupMenu.Popup(P.X, P.Y);
              End;
            WM_LBUTTONDBLCLK:
              Begin
                Application.Restore;
                Show;
              End;
          End; {case}
        End;
    End
  Else
    Begin
      Case Msg.lParam Of
        WM_LBUTTONDOWN:
          SetForegroundWindow(Handle);
        WM_RBUTTONDOWN:
          Begin
            SetForegroundWindow(Handle);
            GetCursorPos(P);
            Application.ProcessMessages;
            PopupMenu.Popup(P.X, P.Y);
          End;
      End;
    End;
End;
{--------}

Procedure TfsFSSQLServerForm.tiDelete;
Begin
  If tiPresent And tiActive Then
    Begin
      tiActive := Not Shell_NotifyIcon(NIM_DELETE, @tiNotifyData);
    End;
End;
{--------}

Function TfsFSSQLServerForm.tiGetIconHandle: hIcon;
Begin
  Result := Application.Icon.Handle;
  If Result = 0 Then
    Result := LoadIcon(0, IDI_Application);
End;
{--------}

Procedure TfsFSSQLServerForm.tiInitNotifyData;
Var
  Tip: String;
Begin
  If tiPresent Then
    Begin
      FillChar(tiNotifyData, sizeof(tiNotifyData), 0);
      With tiNotifyData Do
        Begin
          cbSize := sizeof(tiNotifyData);
          Wnd := Handle;
          uID := 1;
          uFlags := NIF_MESSAGE Or NIF_ICON Or NIF_TIP;
          Tip := 'FSSQL DataBase Server ' + Format('32-bit Ver %5.3f', [fsVersionNumber / 1000.0]) + ' :' + ServerName;
          StrCopy(szTip, @Tip[1]);
          uCallBackMessage := fsc_tiCallBack;
          hIcon := tiGetIconHandle;
          Shell_NotifyIcon(NIM_MODIFY, @tiNotifyData)
        End;
    End;
End;
{$ENDIF}
{====================================================================}

Procedure TfsFSSQLServerForm.btnStartClick(Sender: TObject);
Begin
  CreateEngineMgr;
  If Not Login Then
    Exit;
  BringServerUp;
  SetControls;
End;

Procedure TfsFSSQLServerForm.btnStopClick(Sender: TObject);
Begin
  If Not Login Then
    Exit;
  BringServerDown;
  SetControls;
End;

Procedure TfsFSSQLServerForm.btnPropsClick(Sender: TObject);
Begin
  mnuConfigGeneralClick(Sender);
End;

Procedure TfsFSSQLServerForm.ToolButton1Click(Sender: TObject);
Begin
  application.ProcessMessages;
  If Not fView Then
    Begin
      fView := True;
      fsFSSQLServerForm.Width := 520;
      fsFSSQLServerForm.Height := 240;
      pnlBig.Visible := True;
    End
  Else
    Begin
      fView := False;
      fsFSSQLServerForm.Width := 200;
      fsFSSQLServerForm.Height := 80;
      pnlBig.Visible := False;
    End;
End;

Procedure TfsFSSQLServerForm.Help1Click(Sender: TObject);
Var
  ft: TfsAboutForm;
Begin
  Ft := TfsAboutForm.Create(Nil);
  Try
    Ft.ShowModal;
  Finally
    ft.free;
  End;
End;

Procedure TfsFSSQLServerForm.Show1Click(Sender: TObject);
Begin
  Application.Restore;
  Show;
End;

Procedure TfsFSSQLServerForm.pumStartServerClick(Sender: TObject);
Begin
  CreateEngineMgr;
  If Not Login Then
    Exit;
  BringServerUp;
  SetControls;
End;

Procedure TfsFSSQLServerForm.Button1Click(Sender: TObject);
Begin
  {RMMSnapShot.Display(RMMUsageSnapShot);
  exit;
    Screen.Cursor := crHourGlass;
    Try
      If assigned( FSEngineManager ) Then
        Begin
          pumDownServer.Enabled := False;
          pumStartServer.Enabled := True;
          FSEngineManager.stop;
        End;
    Finally
      Screen.Cursor := crDefault;
    End;
    ResetStatistics;
    Timer1Timer( Self );  }
End;

Procedure TfsFSSQLServerForm.D1Click(Sender: TObject);
Var
  anIndex, i: Integer;
  Server: TFSServer;
  User: TfsUserItem;
  aClient: TfsSrcClient;
  UserInx: Integer;
  UserID: TffName;
  ClientDoneEvent: TFSNormalEvent;
  seEvtClientDone: TFSNormalEvent;

  Procedure ClientRemovePrim(Const aaClient: TfsSrcClient);
  Begin
    If aaClient.CanClose(True) Then
      Begin
        Server.ClientList.DeleteClient(aaClient.ClientID);
        Server.TableList.RemoveUnusedTables;
        Server.FolderList.RemoveUnusedFolders;
        If ((Assigned(seEvtClientDone)) And
          (Server.ClientList.ClientCount = 0)) Then
          seEvtClientDone.SignalEvent;
      End
    Else
      aaClient.RequestClose;
  End;
Begin
  Server := Nil;
  If lvServers.SelCount > 0 Then
    Server := TFSServer(lvServers.Selected.Data);
  If (Not assigned(Server)) Then
    Exit;
  If Not Login Then
    Exit;

  If Server.ClientList.ClientCount > 0 Then
    Server.ClientList.RemoveUnused;
  While Server.ClientList.ClientCount > 0 Do
    Begin
      ClientDoneEvent := TFSNormalEvent.Create;
      Try
        seEvtClientDone := ClientDoneEvent;
        Try
          ClientDoneEvent.WaitFor(fsc_ClientShutdownTime);
        Except
          For i := Pred(Server.ClientList.ClientCount) Downto 0 Do
            Begin
              Try
                aClient := Server.ClientList.Client[ftFromIndex, i];
                aClient.ForceClose;
                ClientRemovePrim(aClient);
              Except
              End;
            End;
        End;
      Finally
        seEvtClientDone := Nil;
        ClientDoneEvent.Free;
      End;
    End;
End;

Procedure TfsFSSQLServerForm.Properties1Click(Sender: TObject);
Begin
  If Not Login Then
    Exit;
  mnuConfigGeneralClick(Sender);
End;

Procedure TfsFSSQLServerForm.DataBase1Click(Sender: TObject);
Begin
  If Not Login Then
    Exit;
  CreateEngineMgr;
  With TfsAliasForm.Create(Application) Do
    Try
      ServerEngine := TFSServer(lvServers.Selected.Data);
      ShowModal;
    Finally
      Free;
    End;
End;

Procedure TfsFSSQLServerForm.Users1Click(Sender: TObject);
Begin
  If Not Login Then
    Exit;
  CreateEngineMgr;
  With TFSUserForm.Create(Application) Do
    Try
      ServerEngine := TFSServer(lvServers.Selected.Data);
      ShowModal;
    Finally
      Free;
    End;
End;

Procedure TfsFSSQLServerForm.FSSQLhomepage1Click(Sender: TObject);
Begin
  If ShellExecute(0, 'open', 'http://www.fssql.com', '',
    '', SW_SHOWNORMAL) <= 32 Then
    ShowMessage(cBrowserError);
End;

Procedure TfsFSSQLServerForm.Forum1Click(Sender: TObject);
Begin
  If ShellExecute(0, 'open', 'http://www.fssql.com/forum', '',
    '', SW_SHOWNORMAL) <= 32 Then
    ShowMessage(cBrowserError);
End;

Procedure TfsFSSQLServerForm.Donate1Click(Sender: TObject);
Begin
  If ShellExecute(0, 'open', 'http://www.fssql.com/donate.php', '',
    '', SW_SHOWNORMAL) <= 32 Then
    ShowMessage(cBrowserError);
End;

Procedure TfsFSSQLServerForm.Save1Click(Sender: TObject);
Var
  Server: TFSServer;
  aResult: TffResult;
  errStr: Array[0..127] Of Char;
Begin
  If lvServers.SelCount > 0 Then
    Server := TFSServer(lvServers.Selected.Data);
  If (Not assigned(Server)) Then
    Exit;
  If Not Login Then
    Exit;
  aResult := Server.SaveConfiguration;
  If (aResult <> 0) Then
    Begin
      fsStrResBDE.GetASCIIZ(aResult, errStr, sizeof(DBIMSG));
      ShowMessage(Format('Error saved configuration: %s [$%x/%d])',
        [strPas(errStr), aResult, aResult]));
    End;
End;

Procedure TfsFSSQLServerForm.Load1Click(Sender: TObject);
Var
  Server: TFSServer;
Begin
  If lvServers.SelCount > 0 Then
    Server := TFSServer(lvServers.Selected.Data);
  If (Not assigned(Server)) Then
    Exit;
  If Not Login Then
    Exit;
  Server.loadConfiguration;
End;

Procedure TfsFSSQLServerForm.Loadconfigurationfrom1Click(Sender: TObject);
Var
  Server: TFSServer;
Begin
  If lvServers.SelCount > 0 Then
    Server := TFSServer(lvServers.Selected.Data);
  If (Not assigned(Server)) Then
    Exit;
  If Not Login Then
    Exit;

  If OpenDialog1.Execute Then
    Begin
      Server.loadConfiguration(OpenDialog1.filename);
      If Not Server.Configuration.GeneralInfo^.giNoAutoSaveCfg Then
        Server.SaveConfiguration;
    End;
End;

Procedure TfsFSSQLServerForm.Saveconfigurationto1Click(Sender: TObject);
Var
  Server: TFSServer;
Begin
  If lvServers.SelCount > 0 Then
    Server := TFSServer(lvServers.Selected.Data);
  If (Not assigned(Server)) Then
    Exit;
  If Not Login Then
    Exit;

  If SaveDialog1.Execute Then
    Server.SaveConfiguration(SaveDialog1.filename);
End;
End.

