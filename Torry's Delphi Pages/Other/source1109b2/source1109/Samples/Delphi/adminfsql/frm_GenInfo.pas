Unit frm_GenInfo;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  fsAdminPlug,
  ExtCtrls,
  ComCtrls,
  Stdctrls,
  FsSrBDE;

Type
  TfrmGenInfo = Class(TForm)
    pnlButtons: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    PageCtrl: TPageControl;
    tsGeneral: TTabSheet;
    tsOperating: TTabSheet;
    tsStartup: TTabSheet;
    tsGarbage: TTabSheet;
    tsKeepAlive: TTabSheet;
    Label1: TLabel;
    edtServerName: TEdit;
    Label2: TLabel;
    edtMaxRAM: TEdit;
    Label3: TLabel;
    edtTempSize: TEdit;
    Label4: TLabel;
    cboPriority: TComboBox;
    cbEncrypt: TCheckBox;
    cbReadOnly: TCheckBox;
    cbNoSaveCfg: TCheckBox;
    cbSecurity: TCheckBox;
    cbDebugLog: TCheckBox;
    cbAutoStart: TCheckBox;
    cbMinimize: TCheckBox;
    cbCollectEnabled: TCheckBox;
    Label5: TLabel;
    edtCollectFreq: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    edtKARetries: TEdit;
    edtKAInterval: TEdit;
    edtLastMsg: TEdit;
    Procedure FormShow(Sender: TObject);
    Procedure Validate(Sender: TObject);
    Procedure SetControls(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
  Private
    OurGenInfo: TfsnmConfAdmin;
    ptrOurGenInfo: PfsnmConfAdmin;
    Procedure ShowConfig;
  Public
    PlugIn: TfsBaseAdminPlugin;
  End;

Var
  frmGenInfo: TfrmGenInfo;

Implementation

{$R *.DFM}

Const
  MaxRAMError = 'Max RAM should have a value between 1 and 2048MB';
  TempStorSizeError = 'The temporary storage size should have a value between 1 and 2048MB';
  CollectFreqError = 'The garbage collection frequency should be between 30,000 (30 seconds) and 3,600,000 (60 minutes) milliseconds';
  IntervalError = 'The interval should be a number between 1,000 and 86,400,000 milliseconds, inclusive';
  RetriesError = 'The number of retries should be a number between 1 and 100, inclusive';

Procedure TfrmGenInfo.Validate(Sender: TObject);
Var
  Value: Longint;
  ErrorCode: Integer;
Begin
  If (ActiveControl = btnCancel) Then Exit;
  // validate MaxRAM
  Val(edtMaxRAM.Text, Value, ErrorCode);
  If (ErrorCode <> 0) Or (Value < 1) Or (Value > 2048) Then
    Begin
      ActiveControl := TEdit(Sender);
      MessageBeep(MB_OK);
      ShowMessage(MaxRAMError);
    End;
  // validate Temp Storage Size
  Val(edtTempSize.Text, Value, ErrorCode);
  If (ErrorCode <> 0) Or (Value < 1) Or (Value > 2048) Then
    Begin
      ActiveControl := TEdit(Sender);
      MessageBeep(MB_OK);
      ShowMessage(TempStorSizeError);
    End;
  // validate Garbage Collection Frequency
  Val(edtCollectFreq.Text, Value, ErrorCode);
  If (ErrorCode <> 0) Or (Value < 30000) Or (Value > 3600000) Then
    Begin
      ActiveControl := TEdit(Sender);
      MessageBeep(MB_OK);
      ShowMessage(CollectFreqError);
    End;
  // validate Keep Alive Last Message Interval
  Val(edtLastMsg.Text, Value, ErrorCode);
  If (ErrorCode <> 0) Or (Value < 1000) Or (Value > 86400000) Then
    Begin
      ActiveControl := TEdit(Sender);
      MessageBeep(MB_OK);
      ShowMessage(IntervalError);
    End;
  // validate Keep Alive Interval
  Val(edtKAInterval.Text, Value, ErrorCode);
  If (ErrorCode <> 0) Or (Value < 1000) Or (Value > 86400000) Then
    Begin
      ActiveControl := TEdit(Sender);
      MessageBeep(MB_OK);
      ShowMessage(IntervalError);
    End;
  // validate Keep Alive Reties
  Val(edtKARetries.Text, Value, ErrorCode);
  If (ErrorCode <> 0) Or (Value < 1) Or (Value > 100) Then
    Begin
      ActiveControl := TEdit(Sender);
      MessageBeep(MB_OK);
      ShowMessage(RetriesError);
    End;
End;

Procedure TfrmGenInfo.SetControls(Sender: TObject);
Begin
  cbNoSaveCfg.Enabled := Not cbReadOnly.Checked;
  edtCollectFreq.Enabled := cbCollectEnabled.Checked;
End;

Procedure TfrmGenInfo.FormShow(Sender: TObject);
Begin
  // read the configuration data through plugin
  ptrOurGenInfo := @OurGenInfo;
  If (Plugin.GetConfig(ptrOurGenInfo) = DBIERR_NONE) Then
    Begin
      ShowConfig; // populate form with config data
      PageCtrl.ActivePageIndex := 0;
      edtMaxRAM.SetFocus;
    End
  Else
    ShowMessage('No compatible server running !');
End;

Procedure TfrmGenInfo.ShowConfig;
Begin
  With OurGenInfo Do
    Begin
      // General Settings
      edtServerName.Text := giServerName;
      edtMaxRAM.Text := IntToStr(giMaxRAM);
      edtTempSize.Text := IntToStr(giTempStoreSize);
      If (giPriority < -2) Or (giPriority > 2) Then
        cboPriority.ItemIndex := 2
      Else
        cboPriority.ItemIndex := giPriority + 2;
      // Operating Mode
      cbEncrypt.Checked := giEncrypt;
      cbReadOnly.Checked := giReadOnly;
      cbNoSaveCfg.Checked := giNoAutoSaveCfg;
      cbSecurity.Checked := giIsSecure;
      cbDebugLog.Checked := giDebugLog;
      // Startup Mode
      cbAutoStart.Checked := giAutoUp;
      cbMinimize.Checked := giAutoMini;
      // Garbage Collection
      cbCollectEnabled.Checked := giCollectEnabled;
      edtCollectFreq.Text := IntToStr(giCollectFreq);
      // Keep Alive
      edtLastMsg.Text := IntToStr(giLastMsgInterval);
      edtKAInterval.Text := IntToStr(giKAInterval);
      edtKARetries.Text := IntToStr(giKARetries);
    End;
End;

Procedure TfrmGenInfo.btnOKClick(Sender: TObject);
Begin
  // copy from data back to message structure; data is valid
  With OurGenInfo Do
    Begin
      // General Settings
      giMaxRAM := StrToInt(edtMaxRAM.Text);
      giTempStoreSize := StrToInt(edtTempSize.Text);
      If (0 <= cboPriority.ItemIndex) And (cboPriority.ItemIndex <= 4) Then
        giPriority := cboPriority.ItemIndex - 2
      Else
        giPriority := 0;
      // Operating Mode
      giEncrypt := cbEncrypt.Checked;
      giReadOnly := cbReadOnly.Checked;
      giNoAutoSaveCfg := cbNoSaveCfg.Checked;
      giIsSecure := cbSecurity.Checked;
      giDebugLog := cbDebugLog.Checked;
      // Startup Mode
      giAutoUp := cbAutoStart.Checked;
      giAutoMini := cbMinimize.Checked;
      // Garbage Collection
      giCollectEnabled := cbCollectEnabled.Checked;
      giCollectFreq := StrToInt64(edtCollectFreq.Text);
      // Keep Alive
      giLastMsgInterval := StrToInt64(edtLastMsg.Text);
      giKAInterval := StrToInt64(edtKAInterval.Text);
      giKARetries := StrToInt(edtKARetries.Text);
    End;
  // update remote configuration through plugin
  If (Plugin.SetConfig(ptrOurGenInfo) <> DBIERR_NONE) Then
    Begin
      ShowMessage('Could not save configuration !');
      ModalResult := mrNone;
    End
  Else
    ModalResult := mrOK;
End;

End.

