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
  fssql,
  fssqldef,
  ComCtrls,
  dgParams,
  Buttons, fsserverremoteclass;

Const
  strExecutionTime = 'Execution time = %d ms';

Type
  TfrmTestSQL = Class(TForm)
    DataSource1: TDataSource;
    FSSession1: TFSSession;
    FSDatabase1: TFSDatabase;
    FSClient1: TFSClient;
    FSRemoteServer1: TFSRemoteServer;
    FSParamConnect1: TFSParamConnect;
    StatusBar: TStatusBar;
    pnl1: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    pgc1: TPageControl;
    ts1: TTabSheet;
    memSQL: TMemo;
    ts2: TTabSheet;
    mempsw: TMemo;
    ts3: TTabSheet;
    ServerProtocol: TRadioGroup;
    ts4: TTabSheet;
    rg1: TRadioGroup;
    TabSheet1: TTabSheet;
    ListBox1: TListBox;
    Panel4: TPanel;
    btn7: TButton;
    Panel5: TPanel;
    cbb1: TComboBox;
    lbl4: TLabel;
    Panel6: TPanel;
    btn1: TButton;
    SpeedButton1: TSpeedButton;
    SyntaxOnly: TCheckBox;
    btn4: TButton;
    btn5: TButton;
    btn6: TButton;
    btnParamValues: TButton;
    edt3: TEdit;
    lbl3: TLabel;
    lbl2: TLabel;
    edt2: TEdit;
    edt1: TEdit;
    lbl1: TLabel;
    se1: TSpinEdit;
    lbl5: TLabel;
    PageControl1: TPageControl;
    TabSheet2: TTabSheet;
    Panel1: TPanel;
    grdResults: TDBGrid;
    DBNavigator1: TDBNavigator;
    Timer1: TTimer;
    Panel2: TPanel;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    fsquery1: TFSQuery;
    Procedure btn1Click(Sender: TObject);
    Procedure btn7Click(Sender: TObject);
    Procedure btn3Click(Sender: TObject);
    Procedure FSClient1ConnectionLost(aSource: TObject);
    Procedure se1Change(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure btn4Click(Sender: TObject);
    Procedure btn5Click(Sender: TObject);
    Procedure btn6Click(Sender: TObject);
    Procedure rg1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure btnParamValuesClick(Sender: TObject);
    Procedure SyntaxOnlyClick(Sender: TObject);
    Procedure SpeedButton1Click(Sender: TObject);
    Procedure pnl1Resize(Sender: TObject);
    Procedure cbb1KeyUp(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure cbb1Click(Sender: TObject);
    Procedure ListBox1DblClick(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
    Procedure Panel2Resize(Sender: TObject);
    Procedure Panel5Resize(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  Private
    { Private declarations }
    dlgParams: TdlgParams;
    FCurrentUser,
      FCurrentPswd: String;
    FIsLastQuerySelect, FSyntaxOnly,
      FSupressSyntaxOKDialog, FSuppressParamsDialog: Boolean;
    FSLoginDialog: TfsLoginDialog;
    Procedure CheckLastQueryType;
    Procedure qExecute;
    Procedure FSLogin(aSource: TObject; Var aUserName,
      aPassword: TffName; Var aResult: Boolean);
  Public
    { Public declarations }
  End;

Var
  frmTestSQL: TfrmTestSQL;

Implementation

{$R *.DFM}
Uses fschangeport;

Procedure SQLErrorDlg(Const AMessage: String);
Var
  Form: TForm;
  Memo: TMemo;
  Btn: TButton;
  Pnl: TPanel;
  PnlBottom: TPanel;
Resourcestring
  cErrCaption = 'Query Error';
Begin
  Form := TForm.Create(Application);
  With Form Do
    Try
      Canvas.Font := Font;
      BorderStyle := bsSizeable;
      Caption := CErrCaption;
      Position := poScreenCenter;
      Width := 480;
      BorderIcons := BorderIcons - [biMinimize];
      Pnl := TPanel.Create(Form);
      With Pnl Do
        Begin
          Parent := Form;
          Caption := '';
          Align := alClient;
          BevelInner := bvNone;
          BevelOuter := bvNone;
        End;
      Memo := TMemo.Create(Form);
      With Memo Do
        Begin
          Parent := Pnl;
          Align := alClient;
          //BorderStyle:= bsNone;
          Font.Name := 'Courier';
          ReadOnly := True;
          Scrollbars := ssBoth;
          Text := aMessage;
          WordWrap := False;
        End;
      Btn := TButton.Create(Form);
      With Btn Do
        Begin
          Caption := 'OK';
          ModalResult := mrOk;
          Default := True;
          Cancel := True;
          Left := 4;
          Top := 4;
        End;
      PnlBottom := TPanel.Create(Form);
      With PnlBottom Do
        Begin
          Parent := Pnl;
          Caption := '';
          Align := alBottom;
          Height := Btn.Height + 8;
          BevelInner := bvNone;
          BevelOuter := bvNone;
        End;
      Btn.Parent := PnlBottom;
      ActiveControl := Btn;
      ShowModal;
    Finally
      Form.Free;
    End;
End;

Procedure TfrmTestSQL.btnParamValuesClick(Sender: TObject);
Var
  SaveSyntaxOnly: Boolean;
Begin
  If Not fsQuery1.Active Then
    Begin
      SaveSyntaxOnly := FSyntaxOnly;
      FSupressSyntaxOKDialog := True;
      Try
        FSyntaxOnly := True;
        qExecute;
      Finally
        FSyntaxOnly := SaveSyntaxOnly;
        FSupressSyntaxOKDialog := False;
      End;
    End;

  If fsQuery1.ParamCount = 0 Then
    Begin
      MessageDlg('Current Query has no parameters', mtInformation, [mbOK], 0);
      Exit;
    End;

  If Not Assigned(dlgParams) Then
    dlgParams := TdlgParams.Create(Self);

  If dlgParams.EditParamValues(fsQuery1.Params) Then
    If fsQuery1.Active Then
      Begin
        FSuppressParamsDialog := True;
        Try
          qExecute;
        Finally
          FSuppressParamsDialog := False;
        End;
      End;
End;

Procedure TfrmTestSQL.CheckLastQueryType;
Var
  Buffer: PChar;
  BuffSize: Integer;
  ffSqlParser: TfsSql;
Begin
  ffSqlParser := TfsSql.Create(Nil);
  BuffSize := Length(memSQL.Text) + 1;
  GetMem(Buffer, BuffSize);
  Try
    StrPCopy(Buffer, memSQL.Text);

    ffSqlParser.SourceStream.SetSize(BuffSize);
    move(Buffer^, ffSqlParser.SourceStream.Memory^, BuffSize);
    ffSqlParser.Execute;
    FIsLastQuerySelect := Assigned(ffsqlParser.RootNode) And
      Assigned(ffsqlParser.RootNode.TableExp(0));

  Finally
    ffsqlParser.Free;
    FreeMem(Buffer, BuffSize);
  End;
End;

Procedure TfrmTestSQL.btn1Click(Sender: TObject);
Begin
  If btn1.Caption = 'Disconnect' Then
    Begin
      btn1.Caption := 'Connect';
      If FSQuery1.Active Then
        Begin
          FSQuery1.Close;
          FSQuery1.Unprepare;
        End;
      SpeedButton1.Caption := 'Run SQL';
      FSDatabase1.Close;
      fsSession1.Active := False;
      fsClient1.Active := False;
      FSParamConnect1.Enabled := False;
    End
  Else
    Begin
      fsSession1.Active := False;
      FSSession1.Passwords.Assign(Mempsw.lines);
      fsClient1.Active := False;
      FSParamConnect1.Enabled := False;
      Case ServerProtocol.itemIndex Of
        0: FSParamConnect1.Protocol := ptSingleUser;
        1: FSParamConnect1.Protocol := ptTCPIP;
        2: FSParamConnect1.Protocol := ptIPXSPX;
      End;
      rg1Click(Nil);
      FSParamConnect1.ServerName := edt3.text;
      FSParamConnect1.Enabled := True;

      fsSession1.SetLoginParameters(edt1.text, edt2.text);
      Try
        fsClient1.Active := True;
        fsSession1.Active := True;
        If fsSession1.Active Then
          Begin
            pgc1.ActivePageIndex := 0;
            btn3Click(Nil);
            If Trim(cbb1.Text) <> '' Then
              Begin
                FSDatabase1.AliasName := Trim(cbb1.Text);
                FSDatabase1.Open;
              End;
          End;
      Except
        btn1.Caption := 'Connect';
        fsSession1.Active := False;
        fsClient1.Active := False;
        FSParamConnect1.Enabled := False;
        Raise;
      End;
      btn1.Caption := 'Disconnect';
    End;
End;

Procedure TfrmTestSQL.btn7Click(Sender: TObject);
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

Procedure TfrmTestSQL.btn3Click(Sender: TObject);
Var
  s: String;
Begin
  If Not FSSession1.Active Then
    Begin
      btn1.Caption := 'Connect';
      btn1Click(Nil);
    End;
  ListBox1.Items.Clear;
  If FSSession1.Active Then
    Begin
      FSSession1.GetAliasNames(cbb1.Items);
      If cbb1.Items.Count > 0 Then
        Begin
          cbb1.itemindex := 0;
          s := cbb1.Items[cbb1.itemindex];
          FSSession1.GetTableNames(s, '', False, False, ListBox1.Items);
        End;
    End;
End;

Procedure TfrmTestSQL.FSLogin(aSource: TObject; Var aUserName,
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

Procedure TfrmTestSQL.FSClient1ConnectionLost(aSource: TObject);
Resourcestring
  cMsg = 'The connection to the server has been lost!';
Begin
  Try
    Try
      btn1.Caption := 'Connect';
      FSQuery1.Close;
      FSQuery1.Unprepare;

      fsSession1.Active := False;
      fsClient1.Active := False;
      FSParamConnect1.Enabled := False;
    Except
    End;
    MessageDlg(cMsg, mtError, [mbOk], 1);
  Except
    //MessageDlg( cMsg, mtError, [mbOk], 1 );
    //application.Terminate;
  End;
End;

Procedure TfrmTestSQL.se1Change(Sender: TObject);
Begin
  FSClient1.Timeout := Se1.Value;
  FSQuery1.Timeout := Se1.Value;
  FSSession1.Timeout := Se1.Value;
  FSDatabase1.Timeout := Se1.Value;
End;

Procedure TfrmTestSQL.FormShow(Sender: TObject);
Begin
  Se1Change(Nil);
  pnl1Resize(Nil);
  Panel2Resize(Nil);
  Panel2Resize(Nil);
  ComboBox1.ItemIndex := 0;
End;

Procedure TfrmTestSQL.btn4Click(Sender: TObject);
Begin
  If Not fsSession1.Active Then Exit;
  If Not FSDatabase1.Connected Then FSDatabase1.Connected := True;
  If Not FSDatabase1.Connected Then
    ShowMessage('Database is not connected!')
  Else If Not FSDatabase1.InTransaction Then
    FSDatabase1.StartTransaction;
End;

Procedure TfrmTestSQL.btn5Click(Sender: TObject);
Begin
  If Not fsSession1.Active Then Exit;
  If Not FSDatabase1.Connected Then FSDatabase1.Connected := True;
  If Not FSDatabase1.Connected Then
    ShowMessage('Database is not connected!')
  Else If FSDatabase1.InTransaction Then
    FSDatabase1.Commit;
End;

Procedure TfrmTestSQL.btn6Click(Sender: TObject);
Begin
  If Not fsSession1.Active Then Exit;
  If Not FSDatabase1.Connected Then FSDatabase1.Connected := True;
  If Not FSDatabase1.Connected Then
    ShowMessage('Database is not connected!')
  Else If FSDatabase1.InTransaction Then
    FSDatabase1.Rollback;
End;

Procedure TfrmTestSQL.rg1Click(Sender: TObject);
Begin
  Case rg1.ItemIndex Of
    0: FSDatabase1.RecLocking := tlOptimisticNoWait;
    1: FSDatabase1.RecLocking := tlOptimisticWait;
    2: FSDatabase1.RecLocking := tlPessimisticNoWait;
    3: FSDatabase1.RecLocking := tlPessimisticWait;
  End;
End;

Procedure TfrmTestSQL.qExecute;
Var
  I: Integer;
  Buffer: PChar;
  BuffSize, ExecutionTime: Integer;
  ExecTime: DWord;
Begin
  Screen.Cursor := crSQLWait;
  ExecutionTime := 0;
  Try
    Application.ProcessMessages;
    FSQuery1.SQL.Clear;
    StatusBar.Panels[0].Text := 'Checking syntax...';
    If memSQL.SelLength > 0 Then
      Begin
        BuffSize := memSQL.SelLength + 1;
        GetMem(Buffer, BuffSize);
        memSQL.GetSelTextBuf(Buffer, BuffSize);
        FSQuery1.SQL.Add(StrPas(Buffer));
        FreeMem(Buffer, BuffSize);
      End
    Else
      FSQuery1.SQL.Text := memSQL.Text;

    Try
      CheckLastQueryType;
      FSQuery1.Prepare;
      If (Not FSyntaxOnly) Then
        Begin
          If Not FSuppressParamsDialog Then
            Begin
              If fsQuery1.ParamCount > 0 Then
                Begin
                  If Not Assigned(dlgParams) Then
                    Begin
                      dlgParams := TdlgParams.Create(Self);
                    End;
                  If Not dlgParams.EditParamValues(fsQuery1.Params) Then
                    Exit;
                End
              Else If Assigned(dlgParams) Then
                Begin
                  dlgParams.Free;
                  dlgParams := Nil;
                End;
            End
          Else
            dlgParams.GetParamValues(fsQuery1.Params);

          If FIsLastQuerySelect Then
            Begin
              StatusBar.Panels[0].Text := 'Executing query...';
              ExecTime := GetTickCount;
              Application.ProcessMessages;
              fsQuery1.Open;
              SpeedButton1.Caption := 'Close SQL';
              Application.ProcessMessages;
              ExecTime := GetTickCount - ExecTime;
              ExecutionTime := ExecTime;
              StatusBar.Panels[0].Text := 'Query retrieved';
              StatusBar.Panels[1].Text := 'Record count = ' +
                FsCommaizeChL(FSQuery1.RecordCount,
                ThousandSeparator);
              StatusBar.Panels[2].Text := Format(strExecutionTime, [ExecutionTime]);

              For I := 0 To grdResults.Columns.Count - 1 Do
                Begin
                  If grdResults.Columns[i].Width > (Width Div 5) * 4 Then
                    grdResults.Columns[i].Width := (Width Div 5) * 4;
                End;

            End
          Else
            Begin
              StatusBar.Panels[0].Text := 'Executing SQL...';
              ExecTime := GetTickCount;
              Application.ProcessMessages;
              fsQuery1.ExecSQL;
              Application.ProcessMessages;
              ExecTime := GetTickCount - ExecTime;
              ExecutionTime := ExecTime;
              StatusBar.Panels[0].Text := 'Query executed';
              StatusBar.Panels[1].Text := 'Rows affected = ' +
                FsCommaizeChL(FSQuery1.RowsAffected,
                ThousandSeparator);
              StatusBar.Panels[2].Text := Format(strExecutionTime, [ExecutionTime]);
              FSQuery1.Unprepare;
            End;
        End
      Else
        Begin
          If Not FSupressSyntaxOKDialog Then
            Begin
              ShowMessage('Syntax is valid');
              StatusBar.Panels[0].Text := 'Syntax is valid';
            End;
        End;
    Except
      On E: EfsDatabaseError Do
        If (E.ErrorCode = fsdse_QueryPrepareFail) Or
          (E.ErrorCode = fsdse_QuerySetParamsFail) Or
          (E.ErrorCode = fsdse_QueryExecFail) Then
          Begin
            SQLErrorDlg(E.Message);
            StatusBar.Panels[0].Text := 'Query failed!';
            StatusBar.Panels[1].Text := 'Record count = 0';
            StatusBar.Panels[2].Text := Format(strExecutionTime, [0]); {!!.03}
          End
        Else
          Raise
      Else
        Raise;
    End;
  Finally
    Application.ProcessMessages;
    Screen.Cursor := crDefault;
  End;
End;

Procedure TfrmTestSQL.FormCreate(Sender: TObject);
Begin
  FIsLastQuerySelect := True;
  FSyntaxOnly := False;
  FSupressSyntaxOKDialog := False;
  memSQL.text:= 'Select * from ';
End;

Procedure TfrmTestSQL.SyntaxOnlyClick(Sender: TObject);
Begin
  FSyntaxOnly := SyntaxOnly.Checked;
End;

Procedure TfrmTestSQL.SpeedButton1Click(Sender: TObject);
Begin
  Application.ProcessMessages;
  If SpeedButton1.Caption = 'Close SQL' Then
    Begin
      SpeedButton1.Caption := 'Run SQL';
      Application.ProcessMessages;
      If FSQuery1.Active Then
        Begin
          FSsession1.CloseInactiveTables;
          FSQuery1.Close;
          FSQuery1.Unprepare;
        End;
      Application.ProcessMessages;
      Timer1.Enabled := False;
    End
  Else
    Begin
      If Trim(cbb1.Text) = '' Then
        Begin
          ShowMessage('Alias or Path DB is Empty!');
          pgc1.ActivePageIndex := 0;
          Exit;
        End;
      If fsSession1.Active Then
        Begin
          // FSDatabase1.Close;
           //FSDatabase1.AliasName := Trim(cbb1.Text);
          // FSDatabase1.Open;
          If Not FSDatabase1.Connected Then
            ShowMessage('Database is not connected!')
          Else
            Begin
              Timer1.Enabled := True;
              qExecute;
              Application.ProcessMessages;
            End;
        End
      Else
        Begin
          Timer1.Enabled := False;
          ShowMessage('Session is not active!');
          btn1.Caption := 'Disconnect';
          btn1Click(Nil);
        End;
    End;
End;

Procedure TfrmTestSQL.pnl1Resize(Sender: TObject);
Begin
  btn4.Height := pnl1.Height - btn4.Top - 4;
  btn5.Height := pnl1.Height - btn5.Top - 4;
  btn6.Height := pnl1.Height - btn6.Top - 4;
  btnParamValues.Height := pnl1.Height - btnParamValues.Top - 4;
End;

Procedure TfrmTestSQL.cbb1KeyUp(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If Key = VK_RETURN Then
    cbb1Click(Nil);
End;

Procedure TfrmTestSQL.cbb1Click(Sender: TObject);
Var
  s: String;
Begin
  If Not FSSession1.Active Then
    Begin
      btn1.Caption := 'Connect';
      btn1Click(Nil);
    End;
  ListBox1.Items.Clear;
  If FSSession1.Active Then
    Begin
      If cbb1.Text <> '' Then
        Begin
          s := Trim(cbb1.Text);
          FSSession1.GetTableNames(s, '', False, False, ListBox1.Items);
          If FSDatabase1.AliasName <> s Then
            Begin
              FSDatabase1.Close;
              FSDatabase1.AliasName := s;
              FSDatabase1.Open;
            End;
        End;
    End;
End;

Procedure TfrmTestSQL.ListBox1DblClick(Sender: TObject);
Begin
  If ListBox1.Items.Count > 0 Then
    Begin
      memSQL.Text := Trim(memSQL.Text) + ' "' + ListBox1.Items[ListBox1.itemindex] + '"';
      pgc1.ActivePageIndex := 1;
    End;
End;

Procedure TfrmTestSQL.Timer1Timer(Sender: TObject);
Begin
  Application.ProcessMessages;
End;

Procedure TfrmTestSQL.Panel2Resize(Sender: TObject);
Begin
  ComboBox1.Width := Panel2.Width + 3;
End;

Procedure TfrmTestSQL.Panel5Resize(Sender: TObject);
Begin
  Cbb1.Width := Panel5.Width - cbb1.Left + 2;
End;

procedure TfrmTestSQL.CheckBox1Click(Sender: TObject);
begin
  FSQuery1.RequestLive:= CheckBox1.Checked;
end;

End.

