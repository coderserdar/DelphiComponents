Unit Unit1;

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
  Db,
  Stdctrls,
  ExtCtrls,
  DBCtrls,
  Grids,
  DBGrids,
  fsllprot,
  fsdb,
  fsllcomp,
  fsllcomm,
  fslllgcy,
  fsllbase,
  fsdbbase,
  fslleng,
  fssrintm,
  fslogdlg,
  Spin,
  fssrcmd,
  fssrsec,
  fslllog,
  ComCtrls,
  fsserverclass,
  fsserverremoteclass;

Type
  TForm1 = Class(TForm)
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Panel1: TPanel;
    Button1: TButton;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    FSSession1: TFSSession;
    FSDatabase1: TFSDatabase;
    FSTable1: TFSTable;
    FSTable2: TFSTable;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    FSClient1: TFSClient;
    FSRemoteServer1: TFSRemoteServer;
    FSParamConnect1: TFSParamConnect;
    ServerProtocol: TRadioGroup;
    Edit3: TEdit;
    Button2: TButton;
    Button3: TButton;
    Label4: TLabel;
    FSQuery1: TFSQuery;
    Panel2: TPanel;
    Panel3: TPanel;
    DBGrid2: TDBGrid;
    DBNavigator2: TDBNavigator;
    DataSource3: TDataSource;
    Panel4: TPanel;
    Splitter1: TSplitter;
    DBGrid3: TDBGrid;
    DBNavigator3: TDBNavigator;
    DBMemo1: TDBMemo;
    DBMemo2: TDBMemo;
    SpinEdit1: TSpinEdit;
    Label7: TLabel;
    SpinEdit2: TSpinEdit;
    Label8: TLabel;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    RadioGroup2: TRadioGroup;
    CheckBox1: TCheckBox;
    Memo1: TMemo;
    Edit4: TEdit;
    CheckBox2: TCheckBox;
    FSServer1: TFSServer;
    CheckBox3: TCheckBox;
    RadioGroup1: TRadioGroup;
    Button4: TButton;
    Button5: TButton;
    Button9: TButton;
    CheckBox4: TCheckBox;
    Button10: TButton;
    Label3: TLabel;
    Label5: TLabel;
    Procedure Button1Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure FSClient1ConnectionLost(aSource: TObject);
    Procedure SpinEdit1Change(Sender: TObject);
    Procedure SpinEdit2Change(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure RadioGroup2Click(Sender: TObject);
    Procedure CheckBox1Click(Sender: TObject);
    Procedure CheckBox3Click(Sender: TObject);
    Procedure RadioGroup1Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button9Click(Sender: TObject);
    Procedure CheckBox4Click(Sender: TObject);
    Procedure Button10Click(Sender: TObject);
    Procedure DataSource1DataChange(Sender: TObject; Field: TField);
  Private
    { Private declarations }
    FCurrentUser,
      FCurrentPswd: String;
    FSLoginDialog: TfsLoginDialog;
    Procedure FSLogin(aSource: TObject; Var aUserName,
      aPassword: TffName; Var aResult: Boolean);
  Public
    { Public declarations }
  End;

Var
  Form1: TForm1;

Implementation

{$R *.DFM}
Uses fschangeport;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  fsSession1.Active := False;
  FSSession1.Passwords.Assign(Memo1.lines);
  fsClient1.Active := False;
  FSParamConnect1.Enabled := False;
  Case ServerProtocol.itemIndex Of
    0: FSParamConnect1.Protocol := ptSingleUser;
    1: FSParamConnect1.Protocol := ptTCPIP;
    2: FSParamConnect1.Protocol := ptIPXSPX;
  End;
  If CheckBox2.Checked Then
    Begin
      FSClient1.ServerEngine := FSServer1;
    End
  Else
    Begin
      FSServer1.Shutdown;
      FSClient1.ServerEngine := FSRemoteServer1;
      FSParamConnect1.ServerName := edit3.text;
      FSParamConnect1.Enabled := True;
      fsSession1.SetLoginParameters(edit1.text, edit2.text);
    End;
  RadioGroup2Click(Nil);
  RadioGroup1Click(Nil);
  CheckBox1Click(Nil);

  fsClient1.Active := True;
  fsSession1.Active := True;
  If fsSession1.Active Then
    Begin
      Try
        FSDatabase1.AliasName := Edit4.Text;
        FSDatabase1.Open;
        FSTable1.Open;
        FSTable1.Refresh;
        FSTable2.Open;
        FSQuery1.Open;
      Except
        fsSession1.Active := False;
        fsClient1.Active := False;
        Raise;
      End;
    End;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  With fschangeport.TFSChangePortClient.Create(Self) Do
    Try
      If ShowModal = mrOK Then
        Begin
          fsSession1.Active := False;
          FSParamConnect1.Enabled := False;
        End;
    Finally
      Free;
    End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  FSTable1.Close;
  FSTable2.Close;
  FSQuery1.Close;
  FSQuery1.Unprepare;
  fsSession1.Active := False;
  fsClient1.Active := False;
  FSParamConnect1.Enabled := False;
End;

Procedure TForm1.FSLogin(aSource: TObject; Var aUserName,
  aPassword: TffName; Var aResult: Boolean);
Begin
  With FSLoginDialog Do
    Begin
      UserName := FCurrentUser;
      Password := FCurrentPswd;
      ShowModal;
      aResult := ModalResult = mrOK;
      If aResult Then
        Begin
          aUserName := UserName;
          aPassword := Password;
          FCurrentUser := fsLoginDialog.UserName;
          FCurrentPswd := fsLoginDialog.Password;
          aResult := True;
        End;
    End;
End;

Procedure TForm1.FSClient1ConnectionLost(aSource: TObject);
Resourcestring
  cMsg = 'The connection to the server has been lost!';
Begin
  Try
    Try
      FSQuery1.Close;
      FSQuery1.Unprepare;
    Except
    End;
    Try
      FSTable2.Close;
      FSTable1.Close;
    Except
    End;
    MessageDlg(cMsg, mtError, [mbOk], 1);
  Except
    //MessageDlg( cMsg, mtError, [mbOk], 1 );
    //application.Terminate;
  End;
End;

Procedure TForm1.SpinEdit1Change(Sender: TObject);
Begin
  FSTable1.CheckTimeout := SpinEdit1.Value;
  FSTable2.CheckTimeout := SpinEdit1.Value;
End;

Procedure TForm1.SpinEdit2Change(Sender: TObject);
Begin
  FSClient1.Timeout := SpinEdit2.Value;
  FSTable1.Timeout := SpinEdit2.Value;
  FSTable2.Timeout := SpinEdit2.Value;
  FSQuery1.Timeout := SpinEdit2.Value;
  FSSession1.Timeout := SpinEdit2.Value;
  FSDatabase1.Timeout := SpinEdit2.Value;
End;

Procedure TForm1.FormShow(Sender: TObject);
Begin
  SpinEdit1Change(Nil);
  SpinEdit2Change(Nil);
  edit4.text := ExtractFilePath(ParamStr(0));
End;

Procedure TForm1.Button6Click(Sender: TObject);
Begin
  If Not FSDatabase1.InTransaction Then
    FSDatabase1.StartTransaction;
End;

Procedure TForm1.Button7Click(Sender: TObject);
Begin
  If FSDatabase1.InTransaction Then
    FSDatabase1.Commit;
End;

Procedure TForm1.Button8Click(Sender: TObject);
Begin
  If FSDatabase1.InTransaction Then
    FSDatabase1.Rollback;
End;

Procedure TForm1.RadioGroup2Click(Sender: TObject);
Begin
  Case RadioGroup2.ItemIndex Of
    0: FSDatabase1.RecLocking := tlOptimisticNoWait;
    1: FSDatabase1.RecLocking := tlOptimisticWait;
    2: FSDatabase1.RecLocking := tlPessimisticNoWait;
    3: FSDatabase1.RecLocking := tlPessimisticWait;
  End;
End;

Procedure TForm1.CheckBox1Click(Sender: TObject);
Begin
  FSTable1.RecLockedBeforeEdit := CheckBox1.Checked;
  FSTable2.RecLockedBeforeEdit := CheckBox1.Checked;
End;

Procedure TForm1.CheckBox3Click(Sender: TObject);
Begin
  FSTable1.FlipOrder := CheckBox3.Checked;
  FSTable2.FlipOrder := CheckBox3.Checked;
  FSQuery1.FlipOrder := CheckBox3.Checked;
End;

Procedure TForm1.RadioGroup1Click(Sender: TObject);
Begin
  Case RadioGroup1.ItemIndex Of
    0:
      Begin
        FSTable1.RecLockedType := tluDataBase;
        FSTable2.RecLockedType := tluDataBase;
      End;
    1:
      Begin
        FSTable1.RecLockedType := tluOptimisticNoWait;
        FSTable2.RecLockedType := tluOptimisticNoWait;
      End;
    2:
      Begin
        FSTable1.RecLockedType := tluOptimisticWait;
        FSTable2.RecLockedType := tluOptimisticWait;
      End;
    3:
      Begin
        FSTable1.RecLockedType := tluPessimisticNoWait;
        FSTable2.RecLockedType := tluPessimisticNoWait;
      End;
    4:
      Begin
        FSTable1.RecLockedType := tluPessimisticWait;
        FSTable2.RecLockedType := tluPessimisticWait;
      End;
  End;

End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  fstable1.LockRecord;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Begin
  fstable1.UnLockRecord;
End;

Procedure TForm1.Button9Click(Sender: TObject);
Var I: Longint;
Begin
  If fstable1.RecordIsLocked Then
    ShowMessage('Yes')
  Else
    ShowMessage('No');
End;

Procedure TForm1.CheckBox4Click(Sender: TObject);
Begin
  FSTable1.SupportRecNo := CheckBox4.Checked;
  FSTable2.SupportRecNo := CheckBox4.Checked;
  FSQuery1.SupportRecNo := CheckBox4.Checked;
End;

Procedure TForm1.Button10Click(Sender: TObject);
Begin
  fstable1.UnLockRecordAll;
End;

Procedure TForm1.DataSource1DataChange(Sender: TObject; Field: TField);
Begin
  label3.caption := IntToStr(fstable1.RecNo);
  label5.caption := IntToStr(fstable1.getrefnr.ilow) + ':' + IntToStr(fstable1.getrefnr.ihigh);
End;

End.

