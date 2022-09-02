Unit fssrvgenl;

{$I FsDEFINE.INC}

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
  Buttons,
  fsserverclass,
  FsSrCfg,
  fsllwsck,
  ComCtrls,
  Tabnotbk,
  Spin;

Type
  TfsGenConfigForm = Class(TForm)
    btnDiscard: TBitBtn;
    btnSave: TBitBtn;
    Panel1: TPanel;
    TabbedNotebook1: TTabbedNotebook;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    gbxStartup: TGroupBox;
    boxServerUp: TCheckBox;
    boxMinimize: TCheckBox;
    gbCollect: TGroupBox;
    lblCollectFreq: TLabel;
    boxCollectEnabled: TCheckBox;
    edtCollectFreq: TEdit;
    gbxKeepAlive: TGroupBox;
    lblLMInterval: TLabel;
    lblBetKeeps: TLabel;
    lblKARetries: TLabel;
    edtLastMsg: TEdit;
    edtKAInterval: TEdit;
    edtKARetries: TEdit;
    grpTCPIP: TGroupBox;
    Label2: TLabel;
    lblTCPPort: TLabel;
    lblUDPSr: TLabel;
    lblUDPCl: TLabel;
    chkTCPEnabled: TCheckBox;
    chkTCPListen: TCheckBox;
    cmbTCPIntf: TComboBox;
    edtTCPPort: TEdit;
    edtUDPServer: TEdit;
    edtUDPClient: TEdit;
    grpIPXSPX: TGroupBox;
    lblIPXSocket: TLabel;
    lblIPXClient: TLabel;
    lblSPX: TLabel;
    chkIPXEnabled: TCheckBox;
    chkIPXListen: TCheckBox;
    edtIPXServer: TEdit;
    edtIPXClient: TEdit;
    edtSPXSocket: TEdit;
    grpSUP: TGroupBox;
    chkSUPEnabled: TCheckBox;
    Panel5: TPanel;
    lblServerName: TLabel;
    edtServerName: TEdit;
    edtMaxRAM: TEdit;
    Label5: TLabel;
    edtTempStoreSize: TEdit;
    lblTempStoreSize: TLabel;
    lblPriority: TLabel;
    cbxPriority: TComboBox;
    boxEncrypt: TCheckBox;
    boxNoSaveCfg: TCheckBox;
    boxSecurity: TCheckBox;
    boxReadOnly: TCheckBox;
    boxTrigers: TCheckBox;
    boxConstrains: TCheckBox;
    boxDebugLog: TCheckBox;
    boxclearcache: TCheckBox;
    boxgarclosetables: TCheckBox;
    Panel6: TPanel;
    boxupdate: TCheckBox;
    boxclosetable: TCheckBox;
    mFlushPerOperation: TSpinEdit;
    Label4: TLabel;
    MMaxDatabase: TSpinEdit;
    MMaxDuplicateUsers: TSpinEdit;
    Label3: TLabel;
    Label1: TLabel;
    MClients: TSpinEdit;
    edttmp: TEdit;
    Label6: TLabel;
    boxEtmp: TCheckBox;
    SpeedButton1: TSpeedButton;
    Procedure btnSaveClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure boxReadOnlyClick(Sender: TObject);
    Procedure boxNoSaveCfgClick(Sender: TObject);
    Procedure boxCollectEnabledClick(Sender: TObject);
    Procedure SpeedButton1Click(Sender: TObject);
  Private
    { Private declarations }
    FEngine: TFSServer;
    OurGenInfo: TfsGeneralInfo;
    TCPPort: Integer;
    UDPPortS: Integer;
    UDPPortC: Integer;
    IPXPortS: Integer;
    IPXPortC: Integer;
    SPXPort: Integer;

    Procedure InitCtrlStates;
    Procedure SetControls;
    Procedure RefreshBindings;
    Procedure SetEngine(anEngine: TFSServer);
    Function ValidateValues: boolean;
    { Used to validate user input once the user clicks the OK button.
      If all the network config values are valid then this function
      returns True and fills in several private variables (listed above)
      with the appropriate information. }

  Public
    { Public declarations }
    Property ServerEngine: TFSServer Read FEngine Write SetEngine;
    { The server engine being configured by this dialog. }
  End;

Var
  fsGenConfigForm: TfsGenConfigForm;

Procedure EnableGroupBox(aBox: TGroupBox; Value: Boolean);
Implementation

Uses
  FsLLComp,
  FsLLEng,
  FsLLExcp,
  FsLLBase,
  FsSRBde,
  FsLLProt,
  fsutil,
  fsllunc,
  fssrvbrws,
  fssrvegmgr;

{$R *.DFM}

{=====================================================================}

Procedure TfsGenConfigForm.boxReadOnlyClick(Sender: TObject);
Begin
  SetControls;
End;
{--------}

Procedure TfsGenConfigForm.btnSaveClick(Sender: TObject);
Const
  PortError = 'Port number should be a unique number between 1024 and 65535 inclusive';
  MaxRAMError = 'Max RAM should have a value between 1 and 2048MB';
  IntervalError = 'The interval should be a number between 1,000 and 86,400,000 milliseconds, inclusive';
  RetriesError = 'The number of retries should be a number between 1 and 100, inclusive';
  TempStorSizeError = 'The temporary storage size should have a value between 1 and 2048MB';
  CollectFreqError = 'The garbage collection frequency should be between 30,000 (30 seconds) and 3,600,000 (60 minutes) milliseconds';
Var
  ec: Integer;
  MaxRAM: Integer;
  TempStorSize: Integer;
  CollectFreq: Longint;
  LMInterval: Longint;
  KAInterval: Longint;
  KARetries: Integer;
  errStr: Array[0..127] Of char;
  aResult: TffResult;
Begin
  Val(edtMaxRAM.Text, MaxRAM, ec);
  If (ec <> 0) Or (MaxRAM < 1) Or (MaxRam > 2048) Then
    Begin
      ActiveControl := edtMaxRAM;
      ShowMessage(MaxRAMError);
      Exit;
    End;
  Val(edtTempStoreSize.Text, TempStorSize, ec);
  If (ec <> 0) Or (TempStorSize < 1) Or (TempStorSize > 2048) Then
    Begin
      ActiveControl := edtTempStoreSize;
      ShowMessage(TempStorSizeError);
      Exit;
    End;
  Val(edtCollectFreq.Text, CollectFreq, ec);
  If (ec <> 0) Or (CollectFreq < 30000) Or (CollectFreq > 3600000) Then
    Begin
      ActiveControl := edtCollectFreq;
      ShowMessage(CollectFreqError);
      Exit;
    End;
  Val(edtLastMsg.Text, LMInterval, ec);
  If (ec <> 0) Or (LMInterval < 1000) Or (LMInterval > 86400000) Then
    Begin
      ActiveControl := edtLastMsg;
      ShowMessage(IntervalError);
      Exit;
    End;
  Val(edtKAInterval.Text, KAInterval, ec);
  If (ec <> 0) Or (KAInterval < 1000) Or (KAInterval > 86400000) Then
    Begin
      ActiveControl := edtKAInterval;
      ShowMessage(IntervalError);
      Exit;
    End;
  Val(edtKARetries.Text, KARetries, ec);
  If (ec <> 0) Or (KARetries < 1) Or (KARetries > 100) Then
    Begin
      ActiveControl := edtKARetries;
      ShowMessage(RetriesError);
      Exit;
    End;
  With OurGenInfo Do
    Begin
      If (edtServerName.Text <> '') Then
        giServerName := edtServerName.Text;
      giMaxRAM := MaxRAM;
      giEncrypt := boxEncrypt.Checked;
      giReadOnly := boxReadOnly.Checked;
      giIsSecure := boxSecurity.Checked;
      giAutoUp := boxServerUp.Checked;
      giAutoMini := boxMinimize.Checked;
      giDebugLog := boxDebugLog.Checked;
      giLastMsgInterval := LMInterval;
      giKAInterval := KAInterval;
      giKARetries := KARetries;
      giTempStoreSize := TempStorSize;
      giCollectEnabled := boxCollectEnabled.Checked;
      giCollectFreq := CollectFreq;
      fsc_LastMsgInterval := giLastMsgInterval;
      fsc_KeepAliveInterval := giKAInterval;
      fsc_KeepAliveRetries := giKARetries;
      If (0 <= cbxPriority.ItemIndex) And
        (cbxPriority.ItemIndex <= 4) Then
        giPriority := cbxPriority.ItemIndex - 2
      Else
        giPriority := 0;
      giNoAutoSaveCfg := boxNoSaveCfg.Checked;
      giEnabledTrigers := boxTrigers.Checked;
      giEnabledReferential := boxConstrains.Checked;
      giMaxDuplicateUsers := MMaxDuplicateUsers.Value;
      giMaxClients := MClients.Value;
      giMaxDbOpen := MMaxDatabase.Value;
      giClearCachePerCount := mFlushPerOperation.Value;
      giClearCacheIfUpdate := boxUpdate.Checked;
      giCloseInactiveTablesAfterCommitOrRoolback := boxclosetable.Checked;
      giClearCache := boxClearCache.Checked;
      giCloseInactiveTables := boxgarclosetables.Checked;
      giEncryptTempStorage := boxEtmp.Checked;
      edtTmp.Text := Trim(edtTmp.Text);
      FEngine.tempPath := edtTmp.Text;
    End;
  {we have to override the ReadOnly setting if we're changing
   ReadOnly or NoAutoSaveCfg from False to True.}
  With FEngine.Configuration Do
    Begin
      GeneralInfo^ := OurGenInfo;
    End;

  If ValidateValues Then
    Begin
      With OurGenInfo Do
        Begin
          giTCPPort := TCPPort;
          giUDPPortSr := UDPPortS;
          giUDPPortCl := UDPPortC;
          giIPXSocketSr := IPXPortS;
          giIPXSocketCl := IPXPortC;
          giSPXSocket := SPXPort;
          FsSetTCPPort(TCPPort);
          FsSetUDPPortServer(UDPPortS);
          FsSetUDPPortClient(UDPPortC);
          FsSetIPXSocketServer(IPXPortS);
          FsSetIPXSocketClient(IPXPortC);
          FsSetSPXSocket(SPXPort);
          giSingleUser := chkSUPEnabled.Checked;
          giIPXSPX := chkIPXEnabled.Checked;
          giIPXSPXLFB := chkIPXListen.Checked;
          giTCPIP := chkTCPEnabled.Checked;
          giTCPIPLFB := chkTCPListen.Checked;
          giTCPInterface := cmbTCPIntf.ItemIndex - 1;
        End;
      FEngine.Configuration.GeneralInfo^ := OurGenInfo;

      If Not OurGenInfo.giNoAutoSaveCfg Then
        FEngine.SaveConfiguration;
      ModalResult := mrOK;
    End
  Else
    ModalResult := mrNone;
End;
{--------}

Procedure TfsGenConfigForm.FormCreate(Sender: TObject);
Begin
  FEngine := Nil;
End;
{--------}

Procedure TfsGenConfigForm.FormShow(Sender: TObject);
Begin
  If FEngine = Nil Then
    Exit;
  InitCtrlStates;
  RefreshBindings;
  If FEngine.State = fsesStarted Then
    edtMaxRAM.SetFocus
  Else
    edtServerName.SetFocus;
End;

{--------}

Procedure TfsGenConfigForm.InitCtrlStates;
Var
  ServerUp: Boolean;
Begin
  ServerUp := (FEngine.State = fsesStarted);
  edtServerName.Enabled := (Not ServerUp);
  EnableGroupBox(grpTCPIP, FSEngineManager.TCPIPTransport.Supported);
  EnableGroupBox(grpIPXSPX, FSEngineManager.IPXSPXTransport.Supported);

  cmbTCPIntf.Enabled := (Not ServerUp);
  edtTCPPort.Enabled := (Not ServerUp);
  edtUDPServer.Enabled := (Not ServerUp);
  edtUDPClient.Enabled := (Not ServerUp);
  edtIPXServer.Enabled := (Not ServerUp);
  edtIPXClient.Enabled := (Not ServerUp);
  edtSPXSocket.Enabled := (Not ServerUp);
End;
{--------}

Procedure TfsGenConfigForm.RefreshBindings;
Var
  Idx: Integer;
Begin
  FsWSGetLocalHosts(cmbTCPIntf.Items);
  Idx := OurGenInfo.giTCPInterface + 1;
  If Idx > Pred(cmbTCPIntf.Items.Count) Then
    Begin
      MessageDlg('The bound interface is no longer available. ' + #13 + #10 +
        'Bindings will be reset to all adapters.',
        mtInformation, [mbOK], 0);
      cmbTCPIntf.ItemIndex := 0;
    End
  Else
    cmbTCPIntf.ItemIndex := Idx;
End;

Function TfsGenConfigForm.ValidateValues: boolean;
Const
  PortError = 'Port number should be a unique number between 1024 and 65535 inclusive';
Var
  aControl: TWinControl;
  ec: Integer;
Begin
  aControl := Nil;
  Result := True;

  Val(edtTCPPort.Text, TCPPort, ec);
  If (ec <> 0) Or (TCPPort < 1024) Or (TCPPort > 65535) Then
    Begin
      Result := False;
      If aControl = Nil Then
        aControl := edtTCPPort;
      ShowMessage(PortError);
    End;

  Val(edtUDPServer.Text, UDPPortS, ec);
  If (ec <> 0) Or (UDPPortS < 1024) Or (UDPPortS > 65535) Then
    Begin
      Result := False;
      If aControl = Nil Then
        aControl := edtUDPServer;
      ShowMessage(PortError);
    End;

  Val(edtUDPClient.Text, UDPPortC, ec);
  If (ec <> 0) Or (UDPPortC < 1024) Or (UDPPortC > 65535) Or
    (UDPPortS = UDPPortC) Then
    Begin
      Result := False;
      If aControl = Nil Then
        aControl := edtUDPClient;
      ShowMessage(PortError);
    End;

  Val(edtIPXServer.Text, IPXPortS, ec);
  If (ec <> 0) Or (IPXPortS < 1024) Or (IPXPortS > 65535) Then
    Begin
      Result := False;
      If aControl = Nil Then
        aControl := edtIPXServer;
      ShowMessage(PortError);
    End;

  Val(edtIPXClient.Text, IPXPortC, ec);
  If (ec <> 0) Or (IPXPortC < 1024) Or (IPXPortC > 65535) Or
    (IPXPortC = IPXPortS) Then
    Begin
      Result := False;
      If aControl = Nil Then
        aControl := edtIPXClient;
      ShowMessage(PortError);
    End;

  Val(edtSPXSocket.Text, SPXPort, ec);
  If (ec <> 0) Or (SPXPort < 1024) Or (SPXPort > 65535) Or
    (SPXPort = IPXPortS) Or (SPXPort = IPXPortC) Then
    Begin
      Result := False;
      If aControl = Nil Then
        aControl := edtTCPPort;
      ShowMessage(PortError);
    End;

  If assigned(aControl) Then
    ActiveControl := aControl;
End;

Procedure TfsGenConfigForm.SetControls;
Begin
  boxNoSaveCfg.Enabled := Not boxReadOnly.Checked;
  edtCollectFreq.Enabled := boxCollectEnabled.Checked;
End;
{--------}

Procedure TfsGenConfigForm.SetEngine(anEngine: TFSServer);
Begin
  FEngine := anEngine;
  If assigned(FEngine) Then
    Begin
      OurGenInfo := FEngine.Configuration.GeneralInfo^;
      With OurGenInfo Do
        Begin
          edtServerName.Text := giServerName;
          edtMaxRAM.Text := IntToStr(giMaxRAM);
          boxEncrypt.Checked := giEncrypt;
          boxReadOnly.Checked := giReadOnly;
          boxNoSaveCfg.Checked := giNoAutoSaveCfg;
          boxSecurity.Checked := giIsSecure;
          boxServerUp.Checked := giAutoUp;
          boxMinimize.Checked := giAutoMini;
          boxDebugLog.Checked := giDebugLog;
          edtLastMsg.Text := IntToStr(giLastMsgInterval);
          edtKAInterval.Text := IntToStr(giKAInterval);
          edtKARetries.Text := IntToStr(giKARetries);
          If (giPriority < -2) Or (giPriority > 2) Then
            cbxPriority.ItemIndex := 2
          Else
            cbxPriority.ItemIndex := giPriority + 2;
          edtTempStoreSize.Text := IntToStr(giTempStoreSize);
          boxCollectEnabled.Checked := giCollectEnabled;
          edtCollectFreq.Text := IntToStr(giCollectFreq);
          boxTrigers.Checked := OurGenInfo.giEnabledTrigers;
          boxConstrains.Checked := OurGenInfo.giEnabledReferential;
          MMaxDuplicateUsers.Value := OurGenInfo.giMaxDuplicateUsers;
          MClients.Value := OurGenInfo.giMaxClients;
          MMaxDatabase.Value := OurGenInfo.giMaxDbOpen;

          mFlushPerOperation.Value := OurGenInfo.giClearCachePerCount;
          boxUpdate.Checked := OurGenInfo.giClearCacheIfUpdate;
          boxclosetable.Checked := OurGenInfo.giCloseInactiveTablesAfterCommitOrRoolback;

          boxClearCache.Checked := OurGenInfo.giClearCache;
          boxgarclosetables.Checked := OurGenInfo.giCloseInactiveTables;
          chkSUPEnabled.Checked := giSingleUser;
          chkIPXEnabled.Checked := giIPXSPX;
          chkIPXListen.Checked := giIPXSPXLFB;
          chkTCPEnabled.Checked := giTCPIP;
          chkTCPListen.Checked := giTCPIPLFB;
          edtTCPPort.Text := IntToStr(giTCPPort);
          edtUDPServer.Text := IntToStr(giUDPPortSr);
          edtUDPClient.Text := IntToStr(giUDPPortCl);
          edtIPXServer.Text := IntToStr(giIPXSocketSr);
          edtIPXClient.Text := IntToStr(giIPXSocketCl);
          edtSPXSocket.Text := IntToStr(giSPXSocket);
          edtTmp.Text := FEngine.tempPath;
          boxEtmp.Checked := giEncryptTempStorage;
        End;
    End;
End;
{=====================================================================}

Procedure TfsGenConfigForm.boxNoSaveCfgClick(Sender: TObject);
Begin
  SetControls;
End;

Procedure TfsGenConfigForm.boxCollectEnabledClick(Sender: TObject);
Begin
  SetControls;
End;

Procedure EnableGroupBox(aBox: TGroupBox; Value: Boolean);
Var
  anIndex: Integer;
Begin

  aBox.Enabled := Value;
  If Not Value Then
    aBox.Font.Color := clGrayText;

  { Disable the child controls. }
  For anIndex := 0 To pred(aBox.ControlCount) Do
    Begin
      aBox.Controls[anIndex].Enabled := Value;
    End;

End;

Procedure TfsGenConfigForm.SpeedButton1Click(Sender: TObject);
Var
  BrowseForm: TfsDirBrowseForm;
Begin
  BrowseForm := TfsDirBrowseForm.Create(Application);
  Try
    If fsDirectoryExists(edttmp.Text) Then
      BrowseForm.dirBox.Directory := edttmp.Text;
    If (BrowseForm.ShowModal = mrOK) Then
      edttmp.Text := FFExpandUNCFileName(BrowseForm.DirBox.Directory);
  Finally
    BrowseForm.Free;
  End;
End;

End.

