{$I fsdefine.inc}

Unit dgquery;

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
  Stdctrls,
  Grids,
  DBGrids,
  ComCtrls,
  ExtCtrls,
  ToolWin,
  Menus,
  DBCtrls,
  Db,
  fslleng,
  fssrintm,
  fsserverremoteclass,
  fsllcomp,
  fsllcomm,
  fslllgcy,
  fsllbase,
  fsllprot, {!!.07}
  fsdbbase,
  fsdb,
  fslllog,
  {$IFDEF DCC4OrLater}
  ImgList,
  {$ENDIF}
  Buttons,
  usqlcfg,
  fsclbase,
  dgParams,
  fsexfield,
  uentity;

Type
  TfsSqlConnection = Class;

  TdlgQuery = Class(TForm)
    StatusBar: TStatusBar;
    ImageList1: TImageList;
    MainMenu: TMainMenu;
    pnlCenter: TPanel;
    pnlSQL: TPanel;
    Splitter: TSplitter;
    pnlResults: TPanel;
    grdResults: TDBGrid;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    DataSource: TDataSource;
    Transport: TfsParamConnect;
    SQLRSE: TFSRemoteServer;
    Options1: TMenuItem;
    mnuQuery: TMenuItem;
    mnuExecute: TMenuItem;
    mnuSave: TMenuItem;
    mnuLoad: TMenuItem;
    mnuProps: TMenuItem;
    mnuConnect: TMenuItem;
    mnuNew: TMenuItem;
    pnlMenuBar: TPanel;
    pnlButtons: TPanel;
    pnlConnections: TPanel;
    ToolBar1: TToolBar;
    btnGo: TToolButton;
    btnLoad: TToolButton;
    btnSave: TToolButton;
    ToolButton7: TToolButton;
    btnProp: TToolButton;
    ToolBar2: TToolBar;
    btnNew: TToolButton;
    cmbQuery: TComboBox;
    Delete1: TMenuItem;
    mnuOptionsDebug: TMenuItem;
    N1: TMenuItem;
    mnuTableClose: TMenuItem;
    N3: TMenuItem;
    mnuQueryCopyToTable: TMenuItem;
    ToolButton1: TToolButton;
    btnParamValues: TToolButton;
    N4: TMenuItem;
    mnuQueryParamValues: TMenuItem;
    SpeedButton1: TSpeedButton;
    memSQL: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    DBNavigator: TDBNavigator;
    CheckBox1: TCheckBox;
    SupportRecNo1: TMenuItem;
    Procedure pbPropertiesClick(Sender: TObject);
    Procedure pbExecuteClick(Sender: TObject);
    Procedure pbSaveClick(Sender: TObject);
    Procedure pbLoadClick(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure btnPropClick(Sender: TObject);
    Procedure btnLoadClick(Sender: TObject);
    Procedure btnSaveClick(Sender: TObject);
    Procedure btnNewClick(Sender: TObject);
    Procedure cmbQueryChange(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure btnGoClick(Sender: TObject);
    Procedure mnuExecuteClick(Sender: TObject);
    Procedure mnuSaveClick(Sender: TObject);
    Procedure mnuLoadClick(Sender: TObject);
    Procedure mnuNewClick(Sender: TObject);
    Procedure mnuLiveClick(Sender: TObject);
    Procedure btnLiveDSClick(Sender: TObject);
    Procedure mnuPropsClick(Sender: TObject);
    Procedure FormKeyDown(Sender: TObject;
      Var Key: Word;
      Shift: TShiftState);
    Procedure grdResultsKeyDown(Sender: TObject;
      Var Key: Word;
      Shift: TShiftState);
    Procedure cmbQueryKeyDown(Sender: TObject;
      Var Key: Word;
      Shift: TShiftState);
    Procedure Delete1Click(Sender: TObject);
    Procedure FormClose(Sender: TObject;
      Var Action: TCloseAction);
    Procedure StatusBarDrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; Const Rect: TRect);
    Procedure cmbQueryEnter(Sender: TObject);
    Procedure memSQLExit(Sender: TObject);
    Procedure memSQLKeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure mnuOptionsDebugClick(Sender: TObject);
    Procedure FormDeactivate(Sender: TObject);
    Procedure mnuTableCloseClick(Sender: TObject);
    Procedure mnuQueryCopyToTableClick(Sender: TObject);
    Procedure btnParamValuesClick(Sender: TObject);
    Procedure SpeedButton1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure pnlSQLResize(Sender: TObject);
    Procedure grdResultsDrawColumnCell(Sender: TObject; Const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    Procedure CheckBox1Click(Sender: TObject);
    Procedure SupportRecNo1Click(Sender: TObject);
    Procedure DataSourceDataChange(Sender: TObject; Field: TField);
  Private
    { Private declarations }
    FSyntaxOnly: Boolean;
    FServerName: String;
    FProtocol: TfsProtocolType;
    FDatabaseName: String;
    FConfig: TffeSQLConfig;
    FConnections: TFSNormalList;
    FUserName: String;
    FPassword: String;
    FDatabaseItem: TffeDatabaseItem;
    FIsLastQuerySelect: Boolean;
    FSuppressParamsDialog: Boolean; {!!.11}
    FSupressSyntaxOKDialog: Boolean; {!!.11}
    FStmt: String; {!!.11}
    fRecordCount: LongWord;

    Procedure CheckLastQueryType;
    Procedure SetControls;
    Procedure NewQuery(Const Stmt: String); {!!.11}
    Procedure GetNewConnection(Const Stmt: String); {!!.11}
    Procedure DisplayHint(Sender: TObject);
    Procedure ReloadCombo;
    Procedure LoadConfig;
    Procedure SaveConfig;
    Procedure SaveQuery;
    Procedure WMGetMinMaxInfo(Var Message: TWMGetMinMaxInfo);
      Message WM_GETMINMAXINFO;
    {Begin !!.02}
  Protected
    FLog: TFSBaseLog;
    {End !!.02}
  Public
    { Public declarations }
    Strpasswd: TStringList;
    Procedure UpdateDefaultTimeout; {!!.11}
    Property ServerName: String
      Read FServerName Write FServerName;
    {Begin !!.07}
    Property Protocol: TfsProtocolType
      Read FProtocol Write FProtocol;
    {End !!.07}
    Property DataBaseName: String
      Read FDatabaseName Write FDatabaseName;
    {Begin !!.02}
    Property Log: TFSBaseLog
      Read FLog Write FLog;
    {End !!.02}
    Property Password: String
      Read FPassword Write FPassword;
    Property InitialStatement: String {!!.11}
    Read FStmt Write FStmt; {!!.11}
    Property UserName: String
      Read FUserName Write FUserName;
    Property DatabaseItem: TffeDatabaseItem
      Read FDatabaseItem Write FDatabaseItem;
  End;

  {This class maintains the objects required for each SQL client
   connection.}
  TfsSqlConnection = Class(TfsSelfListItem)
  Protected
    FClient: TFSClient;
    FQuery: TfsQuery;
    FSession: TFSSession;
    FName: String;
    FText: String;
    FExecutionTime: DWord; {!!.05}
    FdlgParams: TdlgParams;
  Public
    Constructor Create(anEngine: TfsBaseServerEngine;
      aDatabaseName, aUserName, aPassword: String; Sp: TStringList; DataBaseItem: TffeDatabaseItem);
    Destructor Destroy; Override;

    Property Client: TFSClient Read FClient;
    Property ExecutionTime: DWord Read FExecutionTime Write FExecutionTime;
    Property Name: String Read FName Write FName;
    Property Query: TfsQuery Read FQuery;
    Property Session: TFSSession Read FSession;
    Property Text: String Read FText Write FText;
    { The text of the query as last entered into the SQL window.
      We save it aside from the TffQuery so that we don't trash the
      query's resultset. }
    Property dlgParams: TdlgParams Read FdlgParams Write FdlgParams;
    { we keep an instance of the params dialog around
      when a query has parameters; thus saving the values }
  End;

Var
  dlgQuery: TdlgQuery;
  Q: TfsQuery;

Implementation

Uses
  fssrbase,
  dgCpyTbl, {!!.10} {!!.07}
  dgsqlops,
  fssql, {!!.10}
  fssqldef, {!!.10}
  uConfig; {!!.11}

{$R *.DFM}

Resourcestring
  ffConnChanged = 'Connection changed';

Const
  ciDefaultTimeout = 10000;
  strExecutionTime = 'Execution time = %d ms'; {!!.07}

  {====SQL Error Dialog================================================}

Procedure SQLErrorDlg(Const AMessage: String);
Var
  Form: TForm;
  Memo: TMemo; {!!.01}
  //  Msg       : TLabel;                                                {Deleted !!.01}
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
      {Begin !!.01}
      Width := 480;
      BorderIcons := BorderIcons - [biMinimize];
      //      with TPanel.Create(Form) do begin
      //        Parent := Form;
      //        Caption := '';
      //        Align := alLeft;
      //        Width := 8;
      //        BevelInner := bvNone;
      //        BevelOuter := bvNone;
      //      end;
      //      with TPanel.Create(Form) do begin
      //        Parent := Form;
      //        Caption := '';
      //        Align := alRight;
      //        Width := 8;
      //        BevelInner := bvNone;
      //        BevelOuter := bvNone;
      //      end;
      {End !!.01}
      Pnl := TPanel.Create(Form);
      With Pnl Do
        Begin
          Parent := Form;
          Caption := '';
          Align := alClient;
          BevelInner := bvNone;
          BevelOuter := bvNone;
        End;
      {Begin !!.01}
            { Display the error message in a memo. }
      Memo := TMemo.Create(Form);
      With Memo Do
        Begin
          Parent := Pnl;
          Align := alClient;
          Font.Name := 'Courier';
          ReadOnly := True;
          Scrollbars := ssBoth;
          Text := aMessage;
          WordWrap := False;
        End;
      {End !!.01}
      Btn := TButton.Create(Form);
      With Btn Do
        Begin
          Caption := 'OK';
          ModalResult := mrOk;
          Default := True;
          Cancel := True;
          Left := 0;
          Top := 2;
        End;
      PnlBottom := TPanel.Create(Form);
      With PnlBottom Do
        Begin
          Parent := Pnl;
          Caption := '';
          Align := alBottom;
          Height := Btn.Height + 4;
          BevelInner := bvNone;
          BevelOuter := bvNone;
        End;
      Btn.Parent := PnlBottom;
      {Begin !!.01}
      //      Msg := TLabel.Create(Form);
      //      with Msg do begin
      //        Parent := Pnl;
      //        AutoSize := True;
      //        Left := 8;
      //        Top := 8;
      //        Caption := AMessage;
      //      end;
      Btn.Left := (Form.Width Div 2) - (Btn.Width Div 2);
      ActiveControl := Btn;
      //      Pnl.Height := Msg.Height + 16;
      {End !!.01}
      ShowModal;
    Finally
      Form.Free;
    End;
End;

{====================================================================}

Constructor TfsSqlConnection.Create(anEngine: TfsBaseServerEngine;
  aDatabaseName, aUserName, aPassword: String; Sp: TStringList; DataBaseItem: TffeDatabaseItem);
Var
  OldPassword: String;
  OldUserName: String;
Begin
  Inherited Create;
  FExecutionTime := 0; {!!.05}
  FClient := TFSClient.Create(Nil);
  With FClient Do
    Begin
      AutoClientName := True;
      ServerEngine := anEngine;
      TimeOut := Config.DefaultTimeout; {!!.11}
    End;

  FSession := TFSSession.Create(Nil);
  FSession.Passwords.Assign(sp);
  With FSession Do
    Begin
      ClientName := FClient.ClientName;
      AutoSessionName := True;
      OldPassword := fsclPassword;
      OldUserName := fsclUsername;
      Try
        fsclPassword := aPassword;
        fsclUsername := aUserName;
        Open;
      Finally
        fsclPassword := OldPassword;
        fsclUsername := OldUserName;
      End;
    End;

  FQuery := TfsQuery.Create(Nil);
  With FQuery Do
    Begin
      SessionName := FSession.SessionName;
      DataBaseName := aDatabaseName;
      DataBase.RecLocking := DataBaseItem.Database.RecLocking;
      Name := 'Query' + IntToStr(GetTickCount);
      RequestLive := True;
      Timeout := ciDefaultTimeout;
    End;

  FName := 'New Query';
  FText := '';
End;
{--------}

Destructor TfsSqlConnection.Destroy;
Begin
  FQuery.Free;
  FSession.Free;
  FClient.Free;
  {Begin !!.11}
  If Assigned(dlgParams) Then
    dlgParams.Free;
  {End !!.11}
  Inherited Destroy;
End;
{====================================================================}

{===TdlgQuery========================================================}

Procedure TdlgQuery.btnGoClick(Sender: TObject);
Begin
  pbExecuteClick(Sender);
End;
{--------}

Procedure TdlgQuery.btnLiveDSClick(Sender: TObject);
Begin
End;
{--------}

Procedure TdlgQuery.btnLoadClick(Sender: TObject);
Begin
  pbLoadClick(Sender);
End;
{--------}

Procedure TdlgQuery.btnNewClick(Sender: TObject);
Begin
  GetNewConnection(''); {!!.11}
End;
{--------}

Procedure TdlgQuery.btnPropClick(Sender: TObject);
Begin
  pbPropertiesClick(Sender);
End;
{--------}

Procedure TdlgQuery.btnSaveClick(Sender: TObject);
Begin
  pbSaveClick(Sender);
End;
{--------}

Procedure TdlgQuery.cmbQueryChange(Sender: TObject);
Var
  aConn: TfsSqlConnection;
Begin
  aConn := TfsSqlConnection(FConnections[cmbQuery.ItemIndex]);
  Q := aConn.Query;
  memSQL.Clear;
  memSQL.Text := aConn.Text;
  DataSource.DataSet := aConn.Query;
  StatusBar.Panels[0].Text := ffConnChanged;
  CheckLastQueryType;
  SetControls;
End;
{--------}

Procedure TdlgQuery.cmbQueryKeyDown(Sender: TObject;
  Var Key: Word;
  Shift: TShiftState);
Begin
  FormKeyDown(Sender, Key, Shift);
End;
{--------}

Procedure TdlgQuery.Delete1Click(Sender: TObject);
Var
  anIndex: Integer;
Begin
  { Deletes the current connection. }
  anIndex := cmbQuery.ItemIndex;
  If anIndex >= 0 Then
    Begin
      anIndex := cmbQuery.ItemIndex;
      FConnections.DeleteAt(anIndex);
      cmbQuery.Items.Delete(anIndex);
    End;

  { Any connections left? }
  If cmbQuery.Items.Count = 0 Then
    Begin
      { No. Create a new connection. }
      NewQuery(''); {!!.11}
      GetNewConnection(''); {!!.11}
      ReloadCombo;
      cmbQuery.ItemIndex := 0;
    End
  Else
    Begin
      ReloadCombo;
      If anIndex < cmbQuery.Items.Count Then
        cmbQuery.ItemIndex := anIndex
      Else
        cmbQuery.ItemIndex := Pred(anIndex);
      cmbQueryChange(Sender);
    End;

  SetControls;
  StatusBar.Panels[0].Text := 'Connection deleted';

End;
{--------}

Procedure TdlgQuery.DisplayHint(Sender: TObject);
Begin
  StatusBar.Panels[0].Text := Application.Hint;
End;
{--------}

Procedure TdlgQuery.FormClose(Sender: TObject; Var Action: TCloseAction);
Begin
  Action := caFree;
End;
{--------}

Procedure TdlgQuery.FormDestroy(Sender: TObject);
Begin
  FConnections.Free;
  Strpasswd.free;
  SaveConfig;
  If Assigned(FConfig) Then
    FConfig.Free;
End;
{--------}

Procedure TdlgQuery.FormKeyDown(Sender: TObject;
  Var Key: Word;
  Shift: TShiftState);
Begin
  If (Not (TfsSqlConnection(FConnections[cmbQuery.ItemIndex]).Query.State In [dsInsert, dsEdit])) And {!!.07} { prepare for live datasets }
  (Key = VK_ESCAPE) Then
    Close;
  If ssCtrl In Shift Then
    Begin
      With cmbQuery Do
        Begin
          If (Key = VK_UP) Then
            Begin
              SaveQuery;
              If ItemIndex = 0 Then
                ItemIndex := pred(Items.Count)
              Else
                ItemIndex := Pred(ItemIndex);
              cmbQueryChange(Sender);
            End;
          If (Key = VK_DOWN) Then
            Begin
              SaveQuery;
              If ItemIndex = pred(Items.Count) Then
                ItemIndex := 0
              Else
                ItemIndex := Succ(ItemIndex);
              cmbQueryChange(Sender);
            End;
        End;
    End;
End;
{--------}

Procedure TdlgQuery.FormShow(Sender: TObject);
Begin
  FIsLastQuerySelect := True; {!!.10}
  FConfig := TffeSQLConfig.Create(FServerName, FDatabaseName);
  FConnections := TFSNormalList.Create;
  FConnections.Sorted := False;
  LoadConfig;
  Transport.ServerName := FServerName; {!!.01}
  Transport.Protocol := FProtocol; {!!.07}
  If assigned(FLog) Then {!!.02}
    Transport.EventLog := FLog; {!!.02}

  SetControls;
  GetNewConnection(FStmt); {!!.11}
  {create a new session, client, query}
  cmbQuery.ItemIndex := 0;
  Caption := ServerName + ' : ' + DataBaseName;
  Application.OnHint := DisplayHint;
  FSyntaxOnly := False;
  { large font support... }
  If (Screen.PixelsPerInch / PixelsPerInch) > 1.001 Then
    Begin
      Height := Round(Height * (Screen.PixelsPerInch / PixelsPerInch));
      Width := Round(Width * (Screen.PixelsPerInch / PixelsPerInch));
      Statusbar.Height := Round(Statusbar.Height * (Screen.PixelsPerInch / PixelsPerInch));
    End;
End;
{--------}

Procedure TdlgQuery.GetNewConnection(Const Stmt: String); {!!.11}
Var
  anIndex: Integer;
  aSQLConn: TfsSqlConnection;
Begin
  {Save the existing query if it hasn't been saved.}
  SaveQuery;
  NewQuery(Stmt); {!!.11}
  aSQLConn := TfsSqlConnection.Create(SQLRSE, FDatabaseName, FUserName,
    FPassword, Strpasswd, fDatabaseItem);
  anIndex := FConnections.InsertPrim(aSQLConn);
  { Add new connection to the list box and select it. }
  ReloadCombo;
  cmbQuery.ItemIndex := anIndex;
  DataSource.DataSet := aSQLConn.Query;
  SetControls;
End;
{--------}

Procedure TdlgQuery.grdResultsKeyDown(Sender: TObject;
  Var Key: Word;
  Shift: TShiftState);
Begin
  FormKeyDown(Sender, Key, Shift);
End;
{--------}

Procedure TdlgQuery.LoadConfig;
Begin
  FConfig.Refresh;

  WindowState := FConfig.WindowState;
  With FConfig Do
    Begin
      memSQL.Font.Name := FontName;
      //memSQL.Font.Size := FontSize;
      If (WindowState <> wsMaximized) And
        (WindowPos.Bottom <> 0) Then
        Begin
          Left := WindowPos.Left;
          Top := WindowPos.Top;
          Height := WindowPos.Bottom - WindowPos.Top;
          Width := WindowPos.Right - WindowPos.Left;
        End;
      pnlSQL.Height := SplitterPos;
    End;
End;
{--------}

Procedure TdlgQuery.mnuExecuteClick(Sender: TObject);
Begin
  pbExecuteClick(Sender);
End;
{--------}

Procedure TdlgQuery.mnuLiveClick(Sender: TObject);
Begin

End;
{--------}

Procedure TdlgQuery.mnuLoadClick(Sender: TObject);
Begin
  pbLoadClick(Sender);
End;
{--------}

Procedure TdlgQuery.mnuNewClick(Sender: TObject);
Begin
  GetNewConnection(''); {!!.11}
End;
{--------}

Procedure TdlgQuery.mnuPropsClick(Sender: TObject);
Begin
  pbPropertiesClick(Sender);
End;
{--------}

Procedure TdlgQuery.mnuSaveClick(Sender: TObject);
Begin
  pbSaveClick(Sender);
End;
{--------}

Procedure TdlgQuery.NewQuery(Const Stmt: String); {!!.11}
Begin
  With memSQL Do
    Begin
      Clear;
      {Begin !!.11}
      If Stmt = '' Then
        Lines[0] := 'SELECT '
      Else
        Lines[0] := Stmt;

      SelStart := 7;
      SetFocus;
    End;
  FIsLastQuerySelect := True;
End;
{--------}

Procedure TdlgQuery.pbExecuteClick(Sender: TObject);
Var
  aConn: TfsSqlConnection;
  I,
    anIndex: Integer;
  Buffer: PChar;
  BuffSize: Integer;
  ExecTime: DWord;
Begin
  Screen.Cursor := crHourGlass;
  anIndex := 0;
  Try
    Application.ProcessMessages;
    anIndex := cmbQuery.ItemIndex;
    aConn := TfsSqlConnection(FConnections.Items[anIndex]);
    Q := aConn.Query;
    StatusBar.Panels[0].Text := 'Checking syntax...';
    aConn.Query.SQL.Clear;
    If memSQL.SelLength > 0 Then
      Begin
        BuffSize := memSQL.SelLength + 1;
        GetMem(Buffer, BuffSize);
        memSQL.GetSelTextBuf(Buffer, BuffSize);
        aConn.Query.SQL.Add(StrPas(Buffer));
        aConn.Name := StrPas(Buffer);
        FreeMem(Buffer, BuffSize);
      End
    Else
      Begin
        aConn.Query.SQL.Text := memSQL.Text;
        aConn.Name := memSQL.Lines[0];
      End;

    Try
      CheckLastQueryType; {!!.10}
      aConn.Query.Prepare;
      If (Not FSyntaxOnly) Then
        Begin
          {Begin !!.11}
          { do we need to present the Params dialog? }
          If Not FSuppressParamsDialog Then
            Begin
              If aConn.Query.ParamCount > 0 Then
                Begin
                  If Not Assigned(aConn.dlgParams) Then
                    Begin
                      aConn.dlgParams := TdlgParams.Create(Self);
                    End;
                  If Not aConn.dlgParams.EditParamValues(aConn.Query.Params) Then
                    Exit;
                End
              Else If Assigned(aConn.dlgParams) Then
                Begin
                  aConn.dlgParams.Free;
                  aConn.dlgParams := Nil;
                End;
            End
          Else
            { get stored values }
            aConn.dlgParams.GetParamValues(aConn.Query.Params);
          {End !!.11}
          If FIsLastQuerySelect Then
            Begin
              StatusBar.Panels[0].Text := 'Executing query...';
              ExecTime := GetTickCount;
              aConn.Query.Open;
              ExecTime := GetTickCount - ExecTime;
              aConn.ExecutionTime := ExecTime; {!!.05}
              StatusBar.Panels[0].Text := 'Query retrieved';
              fRecordCount := aConn.Query.RecordCount;
              StatusBar.Panels[2].Text := 'RecNo: ' + IntToStr(aConn.Query.RecNo) + ' Count = ' +
                FsCommaizeChL(fRecordCount,
                ThousandSeparator);
              StatusBar.Panels[3].Text := Format(strExecutionTime, [aConn.ExecutionTime]); {!!.05}

              { make sure no column exceeds screen width }{!!.07}
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
              aConn.Query.ExecSQL;
              ExecTime := GetTickCount - ExecTime;
              aConn.ExecutionTime := ExecTime; {!!.05}
              StatusBar.Panels[0].Text := 'Query executed';
              StatusBar.Panels[2].Text := 'Rows affected = ' +
                FsCommaizeChL(aConn.Query.RowsAffected,
                ThousandSeparator);
              StatusBar.Panels[3].Text := Format(strExecutionTime, [aConn.ExecutionTime]); {!!.05}
            End;
        End
      Else
        Begin
          If Not FSupressSyntaxOKDialog Then
            Begin {!!.11}
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
            StatusBar.Panels[2].Text := 'Count = 0';
            StatusBar.Panels[3].Text := Format(strExecutionTime, [0]); {!!.03}
          End
        Else
          Raise
      Else
        Raise;
    End;
  Finally
    SetControls;
    Screen.Cursor := crDefault;
    ReloadCombo;
    cmbQuery.ItemIndex := anIndex;
  End;
End;
{--------}

Procedure TdlgQuery.pbLoadClick(Sender: TObject);
Var
  aConn: TfsSqlConnection;
  anIndex: Integer;
Begin
  { Load a query from a file.  Update combobox. }
  If OpenDialog.Execute Then
    Begin
      {should we start a new connection?}
      If Assigned(DataSource.DataSet) Then
        GetNewConnection(''); {!!.11}
      anIndex := cmbQuery.ItemIndex;
      memSQL.Lines.LoadFromFile(OpenDialog.Files[0]);
      cmbQuery.Items[anIndex] := memSQL.Lines[0];
      aConn := TfsSqlConnection(FConnections[anIndex]);
      Q := aConn.Query;
      aConn.Text := memSQL.Lines.Text;
      aConn.Query.SQL.Clear;
      cmbQuery.ItemIndex := anIndex;
    End;
End;
{--------}

Procedure TdlgQuery.pbPropertiesClick(Sender: TObject);
Var
  aConn: TfsSqlConnection;
  OptionsForm: TfrmSQLOps;
  anIndex: Integer;
Begin
  {displays a set of options for the sql window and current query}
  OptionsForm := TfrmSQLOps.Create(Self);
  With OptionsForm Do
    Begin
      SyntaxOnly := FSyntaxOnly;
      anIndex := cmbQuery.ItemIndex;
      aConn := TfsSqlConnection(FConnections[anIndex]);
      Q := aConn.Query;
      With aConn.Query Do
        Begin
          OptionsForm.Timeout := Timeout;
          RequestLiveDS := RequestLive;
          QueryName := cmbQuery.Items[anIndex];
          Font := memSQL.Font;
          Try
            If ShowModal = mrOK Then
              Begin
                Timeout := OptionsForm.Timeout;
                RequestLive := RequestLiveDS;
                aConn.Name := QueryName; {!!.12}
                cmbQuery.Items[anIndex] := QueryName;
                cmbQuery.ItemIndex := anIndex;
                cmbQuery.Update;
                memSQL.Font := Font;
                FSyntaxOnly := SyntaxOnly;
              End;
          Finally
            OptionsForm.Free;
          End;
        End;
      SaveConfig;
    End;
End;
{--------}

Procedure TdlgQuery.pbSaveClick(Sender: TObject);
Begin
  { Save the query to a file. }
  If SaveDialog.Execute Then
    Begin
      {does the file already exist?}
      If FileExists(SaveDialog.Files[0]) Then
        DeleteFile(SaveDialog.Files[0]);
      memSQL.Lines.SaveToFile(SaveDialog.Files[0]);
    End;
End;
{--------}

Procedure TdlgQuery.ReloadCombo;
Var
  i: Integer;
Begin
  cmbQuery.Clear;
  For i := 0 To Pred(FConnections.Count) Do
    cmbQuery.Items.Insert(i,
      TfsSqlConnection(FConnections[i]).Name);
End;
{--------}

Procedure TdlgQuery.SaveConfig;
Var
  TempRect: TRect;
Begin
  {save the current settings to the INI file}
  If Assigned(FConfig) Then
    Begin
      FConfig.WindowState := WindowState;
      With FConfig Do
        Begin
          FontName := memSQL.Font.Name;
          //FontSize := memSQL.Font.Size;
          TempRect.Left := Left;
          TempRect.Right := Left + Width;
          TempRect.Bottom := Top + Height;
          TempRect.Top := Top;
          WindowPos := TempRect;
          SplitterPos := pnlSQL.Height;
          Save;
        End;
    End;
End;
{--------}

Procedure TdlgQuery.SetControls;
Var
  aConn: TfsSqlConnection;
Begin
  DBNavigator.VisibleButtons :=
    DBNavigator.VisibleButtons -
    [nbInsert, nbDelete, nbEdit, nbPost, nbCancel];
  If (cmbQuery.ItemIndex <> -1) Then
    Begin
      aConn := TfsSqlConnection(FConnections.Items[cmbQuery.ItemIndex]);
      Q := aConn.Query;
      With aConn.Query Do
        Begin
          If RequestLive And CanModify Then
            Begin
              DBNavigator.VisibleButtons :=
                DBNavigator.VisibleButtons +
                [nbInsert, nbDelete, nbEdit, nbPost, nbCancel];
            End;
          If FIsLastQuerySelect Then
            Begin {!!.10}
              If Active Then
                Begin
                  fRecordCount := aConn.Query.RecordCount;
                  StatusBar.Panels[2].Text := 'RecNo: ' + IntToStr(aConn.Query.RecNo) + ' Count = ' +
                    FsCommaizeChL(fRecordCount,
                    ThousandSeparator);
                  StatusBar.Panels[3].Text := Format(strExecutionTime, [aConn.ExecutionTime]); {!!.05}
                End
              Else
                Begin
                  StatusBar.Panels[2].Text := 'Count = 0';
                  StatusBar.Panels[3].Text := Format(strExecutionTime, [0]); {!!.05}
                End;
            End
              {Begin !!.10}
          Else
            Begin
              StatusBar.Panels[2].Text := 'Rows affected = ' +
                FsCommaizeChL(RowsAffected, ThousandSeparator);
              StatusBar.Panels[3].Text := Format(strExecutionTime, [aConn.ExecutionTime]); {!!.05}
            End;
        End;
    End;

  StatusBar.Panels[1].Text := format('Queries = %d', [cmbQuery.Items.Count]); {!!.05}
  StatusBar.Refresh;
End;
{====================================================================}

Procedure TdlgQuery.StatusBarDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel;
  Const Rect: TRect);
Var
  aConn: TfsSqlConnection;
Begin
  With StatusBar Do
    Begin
      If cmbQuery.ItemIndex > -1 Then
        Begin
          aConn := TfsSqlConnection(FConnections.Items[cmbQuery.ItemIndex]);
          With aConn.Query Do
            Begin
              If RequestLive And CanModify Then
                ImageList1.Draw(StatusBar.Canvas, Rect.Left + 3, Rect.Top, 9)
              Else
                ImageList1.Draw(StatusBar.Canvas, Rect.Right - 30, Rect.Top, 10);
            End
        End
      Else
        ImageList1.Draw(StatusBar.Canvas, Rect.Left + 3, Rect.Top, 10);
    End;
End;

Procedure TdlgQuery.SaveQuery;
Var
  aSQLConn: TfsSqlConnection;
Begin
  {Save the existing query if it hasn't been saved.}
  If cmbQuery.ItemIndex > -1 Then
    Begin
      aSQLConn := TfsSqlConnection(FConnections.Items[cmbQuery.ItemIndex]);
      aSQLConn.Text := memSQL.Text;
    End;
End;

Procedure TdlgQuery.cmbQueryEnter(Sender: TObject);
Begin
  SaveQuery;
End;

Procedure TdlgQuery.WMGetMinMaxInfo(Var Message: TWMGetMinMaxInfo);
Var
  MinMax: PMinMaxInfo;
Begin
  Inherited;
  MinMax := Message.MinMaxInfo;
  MinMax^.ptMinTrackSize.x := 535;
End;

Procedure TdlgQuery.memSQLExit(Sender: TObject);
Var
  aConn: TfsSqlConnection;
Begin
  { Save the text in the memo so that it is preserved in the event the
    user switches to another connection or creates a new connection. }
  aConn := TfsSqlConnection(FConnections[cmbQuery.ItemIndex]);
  Q := aConn.Query;
  aConn.Text := memSQL.Text;
End;

Procedure TdlgQuery.memSQLKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  { Make sure Ctrl+Up and Ctrl+Down are recognized. }
  FormKeyDown(Sender, Key, Shift);
  {Begin !!.11}
  { support Ctrl-A for Select All }
  If (Key = Ord('A')) And
    (Shift = [ssCtrl]) Then
    memSQL.SelectAll;
  {End !!.11}
End;

{Start !!.02}

Procedure TdlgQuery.mnuOptionsDebugClick(Sender: TObject);
Begin
  mnuOptionsDebug.Checked := Not mnuOptionsDebug.Checked;
  If mnuOptionsDebug.Checked Then
    Transport.EventLogOptions := [fstpLogErrors, fstpLogRequests,
      fstpLogReplies]
  Else
    Transport.EventLogOptions := [fstpLogErrors];
End;
{End !!.02}

Procedure TdlgQuery.FormDeactivate(Sender: TObject);
Begin
  Application.OnHint := Nil; {!!.06}
End;

Procedure TdlgQuery.mnuTableCloseClick(Sender: TObject);
Begin
  Close;
End;

Procedure TdlgQuery.mnuQueryCopyToTableClick(Sender: TObject);
Var
  ExcludeIndex,
    TableIndex, CommitPerTr: Longint;
  CopyBlobs: Boolean;
  aConn: TfsSqlConnection;
  SaveTimeout: Integer;
  Dummy: TffeTableItem; {!!.11}
Begin
  aConn := TfsSqlConnection(FConnections[cmbQuery.ItemIndex]);
  Q := aConn.Query;
  ExcludeIndex := -1;
  If ShowCopyTableDlg(FDatabaseItem, ExcludeIndex, aConn.FQuery,
    TableIndex, CopyBlobs, Dummy, CommitPerTr) = mrOK Then
    Begin {!!.11}
      With FDatabaseItem.Tables[TableIndex] Do
        Begin
          Screen.Cursor := crHourGlass;
          { the copy operation is used in the context of the table
            that's being copied to. Use the timeout of the active
            table, otherwise the user has no way of setting timeout. }
          SaveTimeout := Table.Timeout;
          Table.Timeout := aConn.FQuery.Timeout;
          Try
            Update;
            CopyRecords(aConn.FQuery, CopyBlobs, CommitPerTr);
          Finally
            Screen.Cursor := crDefault;
            Table.Timeout := SaveTimeout;
            { force the second table to close if it wasn't open before }
            aConn.FSession.CloseInactiveTables; {!!.11}
          End;
        End;
    End;
End;

Procedure TdlgQuery.CheckLastQueryType;
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

{Begin !!.11}

Procedure TdlgQuery.UpdateDefaultTimeout;
Var
  i: Integer;
Begin
  For i := 0 To Pred(FConnections.Count) Do
    TfsSqlConnection(FConnections[i]).FClient.TimeOut := Config.DefaultTimeout;
End;

Procedure TdlgQuery.btnParamValuesClick(Sender: TObject);
Var
  aConn: TfsSqlConnection;
  SaveSyntaxOnly: Boolean;
Begin
  aConn := TfsSqlConnection(FConnections[cmbQuery.ItemIndex]);
  Q := aConn.Query;
  { if query isn't active then update from memo etc and prepare statement }
  If Not aConn.Query.Active Then
    Begin
      SaveSyntaxOnly := FSyntaxOnly;
      FSupressSyntaxOKDialog := True;
      Try
        FSyntaxOnly := True;
        pbExecuteClick(Sender);
      Finally
        FSyntaxOnly := SaveSyntaxOnly;
        FSupressSyntaxOKDialog := False;
      End;
    End;

  If aConn.Query.ParamCount = 0 Then
    Begin
      MessageDlg('Current Query has no parameters', mtInformation, [mbOK], 0);
      Exit;
    End;

  If Not Assigned(aConn.dlgParams) Then
    aConn.dlgParams := TdlgParams.Create(Self);

  If aConn.dlgParams.EditParamValues(aConn.Query.Params) Then
    If aConn.Query.Active Then
      Begin
        FSuppressParamsDialog := True;
        Try
          pbExecuteClick(Sender);
        Finally
          FSuppressParamsDialog := False;
        End;
      End;
End;
{End !!.11}

Procedure TdlgQuery.SpeedButton1Click(Sender: TObject);
Var
  aConn: TfsSqlConnection;
  I,
    anIndex: Integer;
  Buffer: PChar;
  BuffSize: Integer;
  ExecTime: DWord;
Begin
  Screen.Cursor := crHourGlass;
  anIndex := 0;
  Try
    Application.ProcessMessages;
    anIndex := cmbQuery.ItemIndex;
    aConn := TfsSqlConnection(FConnections.Items[anIndex]);
    StatusBar.Panels[0].Text := 'Checking syntax...';
    aConn.Query.SQL.Clear;
    Q := aConn.Query;
    If memSQL.SelLength > 0 Then
      Begin
        BuffSize := memSQL.SelLength + 1;
        GetMem(Buffer, BuffSize);
        memSQL.GetSelTextBuf(Buffer, BuffSize);
        aConn.Query.SQL.Add(StrPas(Buffer));
        aConn.Name := StrPas(Buffer);
        FreeMem(Buffer, BuffSize);
      End
    Else
      Begin
        aConn.Query.SQL.Text := memSQL.Text;
        aConn.Name := memSQL.Lines[0];
      End;

    Try
      CheckLastQueryType; {!!.10}
      aConn.Query.Prepare;
      If (Not FSyntaxOnly) Then
        Begin
          {Begin !!.11}
          { do we need to present the Params dialog? }
          If Not FSuppressParamsDialog Then
            Begin
              If aConn.Query.ParamCount > 0 Then
                Begin
                  If Not Assigned(aConn.dlgParams) Then
                    Begin
                      aConn.dlgParams := TdlgParams.Create(Self);
                    End;
                  If Not aConn.dlgParams.EditParamValues(aConn.Query.Params) Then
                    Exit;
                End
              Else If Assigned(aConn.dlgParams) Then
                Begin
                  aConn.dlgParams.Free;
                  aConn.dlgParams := Nil;
                End;
            End
          Else
            { get stored values }
            aConn.dlgParams.GetParamValues(aConn.Query.Params);
          StatusBar.Panels[0].Text := 'Executing query...';
          ExecTime := GetTickCount;
          aConn.Query.Open;
          ExecTime := GetTickCount - ExecTime;
          aConn.ExecutionTime := ExecTime; {!!.05}
          StatusBar.Panels[0].Text := 'Query retrieved';
          fRecordCount := aConn.Query.RecordCount;
          StatusBar.Panels[2].Text := 'RecNo: ' + IntToStr(aConn.Query.RecNo) + ' Count = ' +
            FsCommaizeChL(fRecordCount,
            ThousandSeparator);
          StatusBar.Panels[3].Text := Format(strExecutionTime, [aConn.ExecutionTime]); {!!.05}

          { make sure no column exceeds screen width }{!!.07}
          For I := 0 To grdResults.Columns.Count - 1 Do
            Begin
              If grdResults.Columns[i].Width > (Width Div 5) * 4 Then
                grdResults.Columns[i].Width := (Width Div 5) * 4;
            End;
        End
      Else
        Begin
          If Not FSupressSyntaxOKDialog Then
            Begin {!!.11}
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
            StatusBar.Panels[2].Text := 'Count = 0';
            StatusBar.Panels[3].Text := Format(strExecutionTime, [0]); {!!.03}
          End
        Else
          Raise
      Else
        Raise;
    End;
  Finally
    SetControls;
    Screen.Cursor := crDefault;
    ReloadCombo;
    cmbQuery.ItemIndex := anIndex;
  End;

End;

Procedure TdlgQuery.FormCreate(Sender: TObject);
Begin
  Strpasswd := TStringList.Create;
End;

Procedure TdlgQuery.pnlSQLResize(Sender: TObject);
Begin
  memSQL.Width := pnlSQL.Width;
  memSQL.Height := pnlSQL.Height;
End;

Procedure TdlgQuery.grdResultsDrawColumnCell(Sender: TObject;
  Const Rect: TRect; DataCol: Integer; Column: TColumn;
  State: TGridDrawState);
Begin
  If q = Nil Then Exit;
  If Not q.Active Then Exit;
  // for flags record
  {frDataRecord = 1;
  frEmptyRecord = 2; // block empty
  frDeleteRecord = 4;
  frUndeletedRecord = 8;
  frProtectDeleteRecord = 16;
  frProtectUpdateRecord = 32; }

  //If getflags(fTable.GetFlagRecord, frDataRecord) Then // normal
    //grdResults.Canvas.Font.Color := clblack;
  If (getflags(q.GetFlagRecord, frUndeletedRecord) And getflags(q.GetFlagRecord, frProtectUpdateRecord)) Then
    grdResults.Canvas.Font.Color := clolive
  Else If getflags(q.GetFlagRecord, frUndeletedRecord) Then
    grdResults.Canvas.Font.Color := clgreen
  Else If (getflags(Q.GetFlagRecord, frProtectUpdateRecord) And getflags(Q.GetFlagRecord, frProtectDeleteRecord)) Then
    grdResults.Canvas.Font.Color := clRed
  Else If getflags(Q.GetFlagRecord, frProtectDeleteRecord) Then
    grdResults.Canvas.Font.Color := clYellow
  Else If getflags(Q.GetFlagRecord, frProtectUpdateRecord) Then
    grdResults.Canvas.Font.Color := clFuchsia;
  grdResults.DefaultDrawcolumnCell(Rect, DataCol, Column, State);

End;

Procedure TdlgQuery.CheckBox1Click(Sender: TObject);
Begin
  Q.FlipOrder := CheckBox1.Checked;
End;

Procedure TdlgQuery.SupportRecNo1Click(Sender: TObject);
Begin
  SupportRecNo1.Checked := Not SupportRecNo1.Checked;
  q.SupportRecNo := SupportRecNo1.Checked;
End;

Procedure TdlgQuery.DataSourceDataChange(Sender: TObject; Field: TField);
Begin
  StatusBar.Panels[2].Text := 'RecNo: ' + IntToStr(q.RecNo) + ' Count = ' +
    FsCommaizeChL(fRecordCount,
    ThousandSeparator);
End;

End.

