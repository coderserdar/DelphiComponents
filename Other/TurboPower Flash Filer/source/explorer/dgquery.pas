{*********************************************************}
{* FlashFiler Query Dialog                               *}
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

{$I ffdefine.inc}

unit dgquery;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Grids,
  DBGrids,
  ComCtrls,
  ExtCtrls,
  ToolWin,
  Menus,
  DBCtrls,
  Db,
  fflleng,
  ffsrintm,
  ffclreng,
  ffllcomp,
  ffllcomm,
  fflllgcy,
  ffllbase,
  ffllprot,  {!!.07}
  ffdbbase,
  ffdb,
  fflllog,
  {$IFDEF DCC4OrLater}
  ImgList,
  {$ENDIF}
  Buttons,
  usqlcfg,
  ffclbase,
  dgParams,   {!!.11}
  uentity;    {!!.10}

type
  TffSQLConnection = class;

  TdlgQuery = class(TForm)
    StatusBar: TStatusBar;
    ImageList1: TImageList;
    MainMenu: TMainMenu;
    pnlCenter: TPanel;
    pnlSQL: TPanel;
    memSQL: TMemo;
    Splitter: TSplitter;
    pnlResults: TPanel;
    grdResults: TDBGrid;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    DBNavigator: TDBNavigator;
    DataSource: TDataSource;
    Transport: TffLegacyTransport;
    SQLRSE: TFFRemoteServerEngine;
    Options1: TMenuItem;
    mnuQuery: TMenuItem;
    mnuExecute: TMenuItem;
    mnuSave: TMenuItem;
    mnuLoad: TMenuItem;
    mnuLive: TMenuItem;
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
    btnLiveDS: TToolButton;
    ToolBar2: TToolBar;
    btnNew: TToolButton;
    cmbQuery: TComboBox;
    Delete1: TMenuItem;
    mnuOptionsDebug: TMenuItem;
    N1: TMenuItem;
    mnuQueryPrintPreview: TMenuItem;
    mnuQueryDesignReport: TMenuItem;
    N2: TMenuItem;
    mnuTableClose: TMenuItem;
    N3: TMenuItem;
    mnuQueryCopyToTable: TMenuItem;
    ToolButton1: TToolButton;
    btnParamValues: TToolButton;
    N4: TMenuItem;
    mnuQueryParamValues: TMenuItem;
    procedure pbPropertiesClick(Sender: TObject);
    procedure pbExecuteClick(Sender: TObject);
    procedure pbSaveClick(Sender: TObject);
    procedure pbLoadClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnPropClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure cmbQueryChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure mnuExecuteClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure mnuLoadClick(Sender: TObject);
    procedure mnuNewClick(Sender: TObject);
    procedure mnuLiveClick(Sender: TObject);
    procedure btnLiveDSClick(Sender: TObject);
    procedure mnuPropsClick(Sender: TObject);
    procedure FormKeyDown(Sender : TObject;
                      var Key    : Word;
                          Shift  : TShiftState);
    procedure grdResultsKeyDown(Sender : TObject;
                            var Key    : Word;
                                Shift  : TShiftState);
    procedure cmbQueryKeyDown(Sender : TObject;
                          var Key    : Word;
                              Shift  : TShiftState);
    procedure Delete1Click(Sender : TObject);
    procedure FormClose(Sender : TObject;
                    var Action : TCloseAction);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; const Rect: TRect);
    procedure cmbQueryEnter(Sender: TObject);
    procedure memSQLExit(Sender: TObject);
    procedure memSQLKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure mnuOptionsDebugClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure mnuTableCloseClick(Sender: TObject);
    procedure mnuQueryPrintPreviewClick(Sender: TObject);
    procedure mnuQueryDesignReportClick(Sender: TObject);
    procedure mnuQueryCopyToTableClick(Sender: TObject);
    procedure btnParamValuesClick(Sender: TObject);
  private
    { Private declarations }
    FSyntaxOnly : Boolean;
    FServerName : string;
    FProtocol : TffProtocolType;
    FDatabaseName : string;
    FConfig : TffeSQLConfig;
    FConnections : TffList;
    FUserName: string;
    FPassword: string;
    FDatabaseItem: TffeDatabaseItem;
    FIsLastQuerySelect: Boolean;
    FSuppressParamsDialog : Boolean;                                   {!!.11}
    FSupressSyntaxOKDialog : Boolean;                                  {!!.11}
    FStmt : string;                                                    {!!.11}

    procedure CheckLastQueryType;
    procedure SetControls;
    procedure NewQuery(const Stmt : string);                           {!!.11}
    procedure GetNewConnection(const Stmt : string);                   {!!.11}
    procedure DisplayHint(Sender : TObject);
    procedure ReloadCombo;
    procedure LoadConfig;
    procedure SaveConfig;
    procedure SaveQuery;
    procedure WMGetMinMaxInfo(var Message : TWMGetMinMaxInfo);
      message WM_GETMINMAXINFO;
{Begin !!.02}
  protected
    FLog          : TffBaseLog;
{End !!.02}
  public
    { Public declarations }
    procedure UpdateDefaultTimeout;                                    {!!.11}
    property ServerName : string
      read FServerName write FServerName;
{Begin !!.07}
    property Protocol : TffProtocolType
      read FProtocol write FProtocol;
{End !!.07}
    property DatabaseName : string
      read FDatabaseName write FDatabaseName;
{Begin !!.02}
    property Log : TffBaseLog
      read FLog write FLog;
{End !!.02}
    property Password : string
      read FPassword write FPassword;
    property InitialStatement : string                                 {!!.11}
      read FStmt write FStmt;                                          {!!.11}
    property UserName : string
      read FUserName write FUserName;
    property DatabaseItem: TffeDatabaseItem
      read FDatabaseItem write FDatabaseItem;
  end;

  {This class maintains the objects required for each SQL client
   connection.}
  TffSQLConnection = class(TffSelfListItem)
  protected
    FClient        : TffClient;
    FQuery         : TffQuery;
    FSession       : TffSession;
    FName          : string;
    FText          : string;
    FExecutionTime : DWord;                                           {!!.05}
    FdlgParams     : TdlgParams;
  public
    constructor Create(anEngine : TffBaseServerEngine;
                       aDatabaseName, aUserName, aPassword : string);
    destructor Destroy; override;

    property Client : TffClient read FClient;
    property ExecutionTime : DWord read FExecutionTime write FExecutionTime;
    property Name : string read FName write FName;
    property Query : TffQuery read FQuery;
    property Session : TffSession read FSession;
    property Text : string read FText write FText;
      { The text of the query as last entered into the SQL window.
        We save it aside from the TffQuery so that we don't trash the
        query's resultset. }
    property dlgParams : TdlgParams read FdlgParams write FdlgParams;
      { we keep an instance of the params dialog around
        when a query has parameters; thus saving the values }
  end;

var
  dlgQuery : TdlgQuery;

implementation

uses
  dgCpyTbl,                                                             {!!.10}
  uReportEngineInterface,                                               {!!.07}
  dgsqlops,
  ffsql,                                                                {!!.10}
  ffsqldef,                                                             {!!.10}
  uConfig;                                                              {!!.11}

{$R *.DFM}

resourcestring
  ffConnChanged = 'Connection changed';

const
  ciDefaultTimeout = 10000;
  strExecutionTime = 'Execution time = %d ms';                          {!!.07}

{====SQL Error Dialog================================================}

procedure SQLErrorDlg(const AMessage : string);
var
  Form      : TForm;
  Memo      : TMemo;                                                   {!!.01}
//  Msg       : TLabel;                                                {Deleted !!.01}
  Btn       : TButton;
  Pnl       : TPanel;
  PnlBottom : TPanel;
resourcestring
  cErrCaption = 'Query Error';
begin
  Form := TForm.Create(Application);
  with Form do
    try
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
      with Pnl do begin
        Parent := Form;
        Caption := '';
        Align := alClient;
        BevelInner := bvNone;
        BevelOuter := bvNone;
      end;
{Begin !!.01}
      { Display the error message in a memo. }
      Memo := TMemo.Create(Form);
      with Memo do begin
        Parent := Pnl;
        Align := alClient;
        Font.Name := 'Courier';
        ReadOnly := True;
        Scrollbars := ssBoth;
        Text := aMessage;
        WordWrap := False;
      end;
{End !!.01}
      Btn := TButton.Create(Form);
      with Btn do begin
        Caption := 'OK';
        ModalResult := mrOk;
        Default := True;
        Cancel := True;
        Left := 0;
        Top := 2;
      end;
      PnlBottom := TPanel.Create(Form);
      with PnlBottom do begin
        Parent := Pnl;
        Caption := '';
        Align := alBottom;
        Height := Btn.Height + 4;
        BevelInner := bvNone;
        BevelOuter := bvNone;
      end;
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
      Btn.Left := (Form.Width div 2) - (Btn.Width div 2);
      ActiveControl := Btn;
//      Pnl.Height := Msg.Height + 16;
{End !!.01}
      ShowModal;
    finally
      Form.Free;
    end;
end;


{====================================================================}
constructor TffSQLConnection.Create(anEngine : TffBaseServerEngine;
                                    aDatabaseName, aUserName, aPassword : string);
var
  OldPassword : string;
  OldUserName : string;
begin
  inherited Create;
  FExecutionTime := 0;                                                {!!.05}
  FClient := TffClient.Create(nil);
  with FClient do begin
    AutoClientName := True;
    ServerEngine := anEngine;
    TimeOut := Config.DefaultTimeout;                        {!!.11}
  end;

  FSession := TffSession.Create(nil);
  with FSession do begin
    ClientName := FClient.ClientName;
    AutoSessionName := True;
    OldPassword := ffclPassword;
    OldUserName := ffclUsername;
    try
      ffclPassword := aPassword;
      ffclUsername := aUserName;
      Open;
    finally
      ffclPassword := OldPassword;
      ffclUsername := OldUserName;
    end;
  end;

  FQuery := TffQuery.Create(nil);
  with FQuery do begin
    SessionName := FSession.SessionName;
    DatabaseName := aDatabaseName;
    Name := 'Query' + IntToStr(GetTickCount);
    RequestLive := True;
    Timeout := ciDefaultTimeout;
  end;

  FName := 'New Query';
  FText := '';
end;
{--------}
destructor TffSQLConnection.Destroy;
begin
  FQuery.Free;
  FSession.Free;
  FClient.Free;
  {Begin !!.11}
  if Assigned(dlgParams) then
    dlgParams.Free;
  {End !!.11}
  inherited Destroy;
end;
{====================================================================}

{===TdlgQuery========================================================}
procedure TdlgQuery.btnGoClick(Sender : TObject);
begin
  pbExecuteClick(Sender);
end;
{--------}
procedure TdlgQuery.btnLiveDSClick(Sender : TObject);
var
  aConn : TffSQLConnection;
begin
  { Switch to requesting live datasets. }
  aConn := TffSQLConnection(FConnections[cmbQuery.ItemIndex]);
  aConn.Query.RequestLive := not aConn.Query.RequestLive;
  SetControls;
end;
{--------}
procedure TdlgQuery.btnLoadClick(Sender : TObject);
begin
  pbLoadClick(Sender);
end;
{--------}
procedure TdlgQuery.btnNewClick(Sender : TObject);
begin
  GetNewConnection('');                                                {!!.11}
end;
{--------}
procedure TdlgQuery.btnPropClick(Sender : TObject);
begin
  pbPropertiesClick(Sender);
end;
{--------}
procedure TdlgQuery.btnSaveClick(Sender : TObject);
begin
  pbSaveClick(Sender);
end;
{--------}
procedure TdlgQuery.cmbQueryChange(Sender : TObject);
var
  aConn : TffSQLConnection;
begin
  aConn := TffSQLConnection(FConnections[cmbQuery.ItemIndex]);
  memSQL.Clear;
  memSQL.Text := aConn.Text;
  DataSource.DataSet := aConn.Query;
  StatusBar.Panels[0].Text := ffConnChanged;
  CheckLastQueryType;
  SetControls;
end;
{--------}
procedure TdlgQuery.cmbQueryKeyDown(Sender : TObject;
                                var Key    : Word;
                                    Shift  : TShiftState);
begin
  FormKeyDown(Sender, Key, Shift);
end;
{--------}
procedure TdlgQuery.Delete1Click(Sender : TObject);
var
  anIndex : Integer;
begin
  { Deletes the current connection. }
  anIndex := cmbQuery.ItemIndex;
  if anIndex >= 0 then begin
    anIndex := cmbQuery.ItemIndex;
    FConnections.DeleteAt(anIndex);
    cmbQuery.Items.Delete(anIndex);
  end;

  { Any connections left? }
  if cmbQuery.Items.Count = 0 then begin
    { No. Create a new connection. }
    NewQuery('');                                                      {!!.11}
    GetNewConnection('');                                              {!!.11} 
    ReloadCombo;
    cmbQuery.ItemIndex := 0;
  end else begin
    ReloadCombo;
    if anIndex < cmbQuery.Items.Count then
      cmbQuery.ItemIndex := anIndex
    else
      cmbQuery.ItemIndex := Pred(anIndex);
    cmbQueryChange(Sender);
  end;

  SetControls;
  StatusBar.Panels[0].Text := 'Connection deleted';

end;
{--------}
procedure TdlgQuery.DisplayHint(Sender : TObject);
begin
  StatusBar.Panels[0].Text := Application.Hint;
end;
{--------}
procedure TdlgQuery.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;
{--------}
procedure TdlgQuery.FormDestroy(Sender: TObject);
begin
  FConnections.Free;
  SaveConfig;
  if Assigned(FConfig) then
    FConfig.Free;
end;
{--------}
procedure TdlgQuery.FormKeyDown(Sender : TObject;
                            var Key    : Word;
                                Shift  : TShiftState);
begin
  if (not (TffSQLConnection(FConnections[cmbQuery.ItemIndex]).Query.State IN [dsInsert, dsEdit])) and {!!.07} { prepare for live datasets }
     (Key = VK_ESCAPE) then
    Close;
  if ssCtrl in Shift then begin
    with cmbQuery do begin
      if (Key = VK_UP) then begin
        SaveQuery;
        if ItemIndex = 0 then
          ItemIndex := pred(Items.Count)
        else
          ItemIndex := Pred(ItemIndex);
        cmbQueryChange(Sender);
      end;
      if (Key = VK_DOWN) then begin
        SaveQuery;
        if ItemIndex = pred(Items.Count) then
          ItemIndex := 0
        else
          ItemIndex := Succ(ItemIndex);
        cmbQueryChange(Sender);
      end;
    end;
  end;
end;
{--------}
procedure TdlgQuery.FormShow(Sender : TObject);
begin
  FIsLastQuerySelect := True;                                           {!!.10}
  FConfig := TffeSQLConfig.Create(FServerName, FDatabaseName);
  FConnections := TffList.Create;
  FConnections.Sorted := False;
  LoadConfig;
  Transport.ServerName := FServerName;                                 {!!.01}
  Transport.Protocol := FProtocol;                                     {!!.07}
  if assigned(FLog) then                                               {!!.02}
    Transport.EventLog := FLog;                                        {!!.02}

//  NewQuery;                                                          {Deleted !!.11}
  SetControls;
  GetNewConnection(FStmt);                                             {!!.11}
  {create a new session, client, query}
  cmbQuery.ItemIndex := 0;
  Caption := ServerName + ' : ' + DatabaseName;
  Application.OnHint := DisplayHint;
  FSyntaxOnly := False;
  { large font support... }
  if (Screen.PixelsPerInch/PixelsPerInch)>1.001 then begin
    Height := Round(Height * (Screen.PixelsPerInch/PixelsPerInch));
    Width := Round(Width * (Screen.PixelsPerInch/PixelsPerInch));
    Statusbar.Height := Round(Statusbar.Height * (Screen.PixelsPerInch/PixelsPerInch));
  end;
  { report menuitems }
  mnuQueryPrintPreview.Enabled := ReportEngineDLLLoaded;
  mnuQueryDesignReport.Enabled := ReportEngineDLLLoaded;
end;
{--------}
procedure TdlgQuery.GetNewConnection(const Stmt : string);             {!!.11}
var
  anIndex     : Integer;
  aSQLConn    : TffSQLConnection;
begin
  {Save the existing query if it hasn't been saved.}
  SaveQuery;
  NewQuery(Stmt);                                                      {!!.11}
  aSQLConn := TffSQLConnection.Create(SQLRSE, FDatabaseName, FUserName,
                                      FPassword);
  anIndex := FConnections.InsertPrim(aSQLConn);
  { Add new connection to the list box and select it. }
  ReloadCombo;
  cmbQuery.ItemIndex := anIndex;
  DataSource.DataSet := aSQLConn.Query;
  SetControls;
end;
{--------}
procedure TdlgQuery.grdResultsKeyDown(Sender : TObject;
                                  var Key    : Word;
                                      Shift  : TShiftState);
begin
  FormKeyDown(Sender, Key, Shift);
end;
{--------}
procedure TdlgQuery.LoadConfig;
begin
  FConfig.Refresh;

  WindowState := FConfig.WindowState;
  with FConfig do begin
    memSQL.Font.Name := FontName;
    memSQL.Font.Size := FontSize;
    if (WindowState <> wsMaximized) and
       (WindowPos.Bottom <> 0) then begin
      Left   := WindowPos.Left;
      Top    := WindowPos.Top;
      Height := WindowPos.Bottom - WindowPos.Top;
      Width  := WindowPos.Right - WindowPos.Left;
    end;
    pnlSQL.Height := SplitterPos;
  end;
end;
{--------}
procedure TdlgQuery.mnuExecuteClick(Sender : TObject);
begin
  pbExecuteClick(Sender);
end;
{--------}
procedure TdlgQuery.mnuLiveClick(Sender : TObject);
begin
  btnLiveDSClick(Sender);
end;
{--------}
procedure TdlgQuery.mnuLoadClick(Sender : TObject);
begin
  pbLoadClick(Sender);
end;
{--------}
procedure TdlgQuery.mnuNewClick(Sender : TObject);
begin
  GetNewConnection('');                                                {!!.11}
end;
{--------}
procedure TdlgQuery.mnuPropsClick(Sender : TObject);
begin
  pbPropertiesClick(Sender);
end;
{--------}
procedure TdlgQuery.mnuSaveClick(Sender : TObject);
begin
  pbSaveClick(Sender);
end;
{--------}
procedure TdlgQuery.NewQuery(const Stmt : string);                     {!!.11}
begin
  with memSQL do begin
    Clear;
{Begin !!.11}
    if Stmt = '' then
      Lines[0] := 'SELECT '
    else
      Lines[0] := Stmt;
    SelStart := 7;
{End !!.11}
    SetFocus;
  end;
  FIsLastQuerySelect := True;                                          {!!.10}
end;
{--------}
procedure TdlgQuery.pbExecuteClick(Sender : TObject);
var
  aConn    : TffSQLConnection;
  I,
  anIndex  : Integer;
  Buffer   : PChar;
  BuffSize : Integer;
  ExecTime : DWord;
begin
  Screen.Cursor := crHourGlass;
  anIndex := 0;
  try
    Application.ProcessMessages;
    anIndex := cmbQuery.ItemIndex;
    aConn := TffSQLConnection(FConnections.Items[anIndex]);
    StatusBar.Panels[0].Text := 'Checking syntax...';
    aConn.Query.SQL.Clear;
    if memSQL.SelLength > 0 then begin
      BuffSize := memSQL.SelLength + 1;
      GetMem(Buffer, BuffSize);
      memSQL.GetSelTextBuf(Buffer, BuffSize);
      aConn.Query.SQL.Add(StrPas(Buffer));
      aConn.Name := StrPas(Buffer);
      FreeMem(Buffer, BuffSize);
    end else begin
      aConn.Query.SQL.Text := memSQL.Text;
      aConn.Name := memSQL.Lines[0];
    end;

    try
      CheckLastQueryType;                                              {!!.10}
      aConn.Query.Prepare;
      if (not FSyntaxOnly) then begin
        {Begin !!.11}
        { do we need to present the Params dialog? }
        if not FSuppressParamsDialog then begin
          if aConn.Query.ParamCount>0 then begin
            if not Assigned(aConn.dlgParams) then begin
              aConn.dlgParams := TdlgParams.Create(Self);
            end;
            if not aConn.dlgParams.EditParamValues(aConn.Query.Params) then
              Exit;
          end
          else
          { params not needed anymore? }
          if Assigned(aConn.dlgParams) then begin
            aConn.dlgParams.Free;
            aConn.dlgParams := Nil;
          end;
        end
        else
          { get stored values }
          aConn.dlgParams.GetParamValues(aConn.Query.Params);
        {End !!.11}
        if FIsLastQuerySelect then begin
        StatusBar.Panels[0].Text := 'Executing query...';
        ExecTime := GetTickCount;
        aConn.Query.Open;
        ExecTime := GetTickCount - ExecTime;
        aConn.ExecutionTime := ExecTime;                              {!!.05}
        StatusBar.Panels[0].Text := 'Query retrieved';
        StatusBar.Panels[2].Text := 'Record count = ' +
                                     FFCommaizeChL(aConn.Query.RecordCount,
                                                   ThousandSeparator);
        StatusBar.Panels[3].Text := Format(strExecutionTime, [aConn.ExecutionTime]); {!!.05}

        { make sure no column exceeds screen width }                        {!!.07}
        for I := 0 to grdResults.Columns.Count-1 do begin
          if grdResults.Columns[i].Width>(Width DIV 5)*4 then
            grdResults.Columns[i].Width := (Width DIV 5)*4;
        end;

        end else begin
          StatusBar.Panels[0].Text := 'Executing SQL...';
          ExecTime := GetTickCount;
          aConn.Query.ExecSQL;
          ExecTime := GetTickCount - ExecTime;
          aConn.ExecutionTime := ExecTime;                              {!!.05}
          StatusBar.Panels[0].Text := 'Query executed';
          StatusBar.Panels[2].Text := 'Rows affected = ' +
                                       FFCommaizeChL(aConn.Query.RowsAffected,
                                                     ThousandSeparator);
          StatusBar.Panels[3].Text := Format(strExecutionTime, [aConn.ExecutionTime]); {!!.05}
        end;
      end else begin
        if not FSupressSyntaxOKDialog then begin              {!!.11}
          ShowMessage('Syntax is valid');
          StatusBar.Panels[0].Text := 'Syntax is valid';
        end;
      end;
    except
      on E: EffDatabaseError do
        if (E.ErrorCode = ffdse_QueryPrepareFail) or
           (E.ErrorCode = ffdse_QuerySetParamsFail) or
           (E.ErrorCode = ffdse_QueryExecFail) then begin
              SQLErrorDlg(E.Message);
              StatusBar.Panels[0].Text := 'Query failed!';
              StatusBar.Panels[2].Text := 'Record count = 0';
              StatusBar.Panels[3].Text := Format(strExecutionTime, [0]);    {!!.03}
        end else
          raise
      else
        raise;
    end;
  finally
    SetControls;
    Screen.Cursor := crDefault;
    ReloadCombo;
    cmbQuery.ItemIndex := anIndex;
  end;
end;
{--------}
procedure TdlgQuery.pbLoadClick(Sender : TObject);
var
  aConn : TffSQLConnection;
  anIndex : Integer;
begin
  { Load a query from a file.  Update combobox. }
  if OpenDialog.Execute then begin
    {should we start a new connection?}
    if Assigned(DataSource.DataSet) then
      GetNewConnection('');                                            {!!.11}
    anIndex := cmbQuery.ItemIndex;
    memSQL.Lines.LoadFromFile(OpenDialog.Files[0]);
    cmbQuery.Items[anIndex] := memSQL.Lines[0];
    aConn := TffSQLConnection(FConnections[anIndex]);
    aConn.Text := memSQL.Lines.Text;
    aConn.Query.SQL.Clear;
    cmbQuery.ItemIndex := anIndex;
  end;
end;
{--------}
procedure TdlgQuery.pbPropertiesClick(Sender: TObject);
var
  aConn       : TffSQLConnection;
  OptionsForm : TfrmSQLOps;
  anIndex     : Integer;
begin
  {displays a set of options for the sql window and current query}
  OptionsForm := TfrmSQLOps.Create(Self);
  with OptionsForm do begin
    SyntaxOnly := FSyntaxOnly;
    anIndex := cmbQuery.ItemIndex;
    aConn := TffSQLConnection(FConnections[anIndex]);
    with aConn.Query do begin
      OptionsForm.Timeout := Timeout;
      RequestLiveDS := RequestLive;
      QueryName := cmbQuery.Items[anIndex];
      Font := memSQL.Font;
      try
        if ShowModal = mrOK then begin
          Timeout := OptionsForm.Timeout;
          RequestLive := RequestLiveDS;
          aConn.Name := QueryName;                                     {!!.12}
          cmbQuery.Items[anIndex] := QueryName;
          cmbQuery.ItemIndex := anIndex;
          cmbQuery.Update;
          memSQL.Font := Font;
          FSyntaxOnly := SyntaxOnly;
        end;
      finally
        OptionsForm.Free;
      end;
    end;
    SaveConfig;
  end;
end;
{--------}
procedure TdlgQuery.pbSaveClick(Sender : TObject);
begin
  { Save the query to a file. }
  if SaveDialog.Execute then begin
    {does the file already exist?}
    if FileExists(SaveDialog.Files[0]) then
      DeleteFile(SaveDialog.Files[0]);
    memSQL.Lines.SaveToFile(SaveDialog.Files[0]);
  end;
end;
{--------}
procedure TdlgQuery.ReloadCombo;
var
  i : Integer;
begin
  cmbQuery.Clear;
  for i := 0 to Pred(FConnections.Count) do
    cmbQuery.Items.Insert(i,
                          TffSQLConnection(FConnections[i]).Name);
end;
{--------}
procedure TdlgQuery.SaveConfig;
var
  TempRect : TRect;
begin
  {save the current settings to the INI file}
  if Assigned(FConfig) then begin
    FConfig.WindowState := WindowState;
    with FConfig do begin
      FontName := memSQL.Font.Name;
      FontSize := memSQL.Font.Size;
      TempRect.Left   := Left;
      TempRect.Right  := Left + Width;
      TempRect.Bottom := Top + Height;
      TempRect.Top    := Top;
      WindowPos := TempRect;
      SplitterPos := pnlSQL.Height;
      Save;
    end;
  end;
end;
{--------}
procedure TdlgQuery.SetControls;
var
  aConn : TffSQLConnection;
begin
  DBNavigator.VisibleButtons :=
    DBNavigator.VisibleButtons -
      [nbInsert, nbDelete, nbEdit, nbPost, nbCancel];
  btnLiveDS.Enabled := False;
  if (cmbQuery.ItemIndex <> -1) then begin
    btnLiveDS.Enabled := True;
    aConn := TffSQLConnection(FConnections.Items[cmbQuery.ItemIndex]);
    with aConn.Query do begin
      if RequestLive and CanModify then begin
          DBNavigator.VisibleButtons :=
            DBNavigator.VisibleButtons +
            [nbInsert, nbDelete, nbEdit, nbPost, nbCancel];
      end;
      if FIsLastQuerySelect then begin                                              {!!.10}
        if Active then begin
          StatusBar.Panels[2].Text := 'Record count = ' +
                                      FFCommaizeChL(RecordCount, ThousandSeparator);
          StatusBar.Panels[3].Text := Format(strExecutionTime, [aConn.ExecutionTime]); {!!.05}
        end else begin
          StatusBar.Panels[2].Text := 'Record count = 0';
          StatusBar.Panels[3].Text := Format(strExecutionTime, [0]);          {!!.05}
        end;
      end
      {Begin !!.10}
      else begin
        StatusBar.Panels[2].Text := 'Rows affected = ' +
                                    FFCommaizeChL(RowsAffected, ThousandSeparator);
        StatusBar.Panels[3].Text := Format(strExecutionTime, [aConn.ExecutionTime]); {!!.05}
      end;
    end;
  end;

  mnuLive.Checked := btnLiveDS.Enabled;
  StatusBar.Panels[1].Text := format('Queries = %d', [cmbQuery.Items.Count]); {!!.05}
  StatusBar.Refresh;
end;
{====================================================================}
procedure TdlgQuery.StatusBarDrawPanel(StatusBar : TStatusBar;
                                       Panel     : TStatusPanel;
                                 const Rect      : TRect);
var
  aConn : TffSQLConnection;
begin
  with StatusBar do begin
    if cmbQuery.ItemIndex > -1 then begin
      aConn := TffSQLConnection(FConnections.Items[cmbQuery.ItemIndex]);
      with aConn.Query do begin
        if RequestLive and CanModify then
          ImageList1.Draw(StatusBar.Canvas, Rect.Left + 3, Rect.Top, 9)
        else
          ImageList1.Draw(StatusBar.Canvas, Rect.Right - 30, Rect.Top, 10);
      end
    end
    else
      ImageList1.Draw(StatusBar.Canvas, Rect.Left + 3, Rect.Top, 10);
  end;
end;

procedure TdlgQuery.SaveQuery;
var
  aSQLConn : TffSQLConnection;
begin
  {Save the existing query if it hasn't been saved.}
  if cmbQuery.ItemIndex > -1 then begin
    aSQLConn := TffSQLConnection(FConnections.Items[cmbQuery.ItemIndex]);
    aSQLConn.Text := memSQL.Text;
  end;
end;

procedure TdlgQuery.cmbQueryEnter(Sender: TObject);
begin
  SaveQuery;
end;

procedure TdlgQuery.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
var
  MinMax : PMinMaxInfo;
begin
  inherited;
  MinMax := Message.MinMaxInfo;
  MinMax^.ptMinTrackSize.x := 535;
end;

procedure TdlgQuery.memSQLExit(Sender: TObject);
var
  aConn : TffSQLConnection;
begin
  { Save the text in the memo so that it is preserved in the event the
    user switches to another connection or creates a new connection. }
 aConn := TffSQLConnection(FConnections[cmbQuery.ItemIndex]);
 aConn.Text := memSQL.Text;
end;

procedure TdlgQuery.memSQLKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  { Make sure Ctrl+Up and Ctrl+Down are recognized. }
  FormKeyDown(Sender, Key, Shift);
  {Begin !!.11}
  { support Ctrl-A for Select All }
  if (Key=Ord('A')) and
     (Shift=[ssCtrl]) then
    memSQL.SelectAll;
  {End !!.11}
end;

{Start !!.02}
procedure TdlgQuery.mnuOptionsDebugClick(Sender: TObject);
begin
  mnuOptionsDebug.Checked := not mnuOptionsDebug.Checked;
  if mnuOptionsDebug.Checked then
    Transport.EventLogOptions := [fftpLogErrors, fftpLogRequests,
                                  fftpLogReplies]
  else
    Transport.EventLogOptions := [fftpLogErrors];
end;
{End !!.02}

procedure TdlgQuery.FormDeactivate(Sender: TObject);
begin
  Application.OnHint := nil;                                          {!!.06}
end;

procedure TdlgQuery.mnuTableCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TdlgQuery.mnuQueryPrintPreviewClick(Sender: TObject);
var
  Filter,
  DatabaseName : Array[0..1024] of Char;
  SQL : Array[0..65536] of Char;
  aConn    : TffSQLConnection;
begin
  aConn := TffSQLConnection(FConnections.Items[cmbQuery.ItemIndex]);
  StrPCopy(DatabaseName, FDatabaseName);
  if aConn.Query.Filtered then begin
    StrPCopy(Filter, aConn.Query.Filter);
  end
  else
    StrCopy(Filter, '');
  StrPCopy(SQL, aConn.Query.SQL.Text);
  SingleQueryReport(FProtocol,
                    FServerName,
                    FUserName,
                    FPassword,
                    DatabaseName,
                    SQL,
                    Filter);
end;

procedure TdlgQuery.mnuQueryDesignReportClick(Sender: TObject);
var
  DatabaseName : Array[0..1024] of Char;
begin
  StrPCopy(DatabaseName, FDatabaseName);
  DesignReport(FProtocol,
               FServerName,
               FUserName,
               FPassword,
               DatabaseName);
end;

procedure TdlgQuery.mnuQueryCopyToTableClick(Sender: TObject);
var
  ExcludeIndex,
  TableIndex: LongInt;
  CopyBlobs : Boolean;
  aConn : TffSQLConnection;
  SaveTimeout : Integer;
  Dummy : TffeTableItem;                                               {!!.11}
begin
  aConn := TffSQLConnection(FConnections[cmbQuery.ItemIndex]);
  ExcludeIndex := -1;
  if ShowCopyTableDlg(FDatabaseItem, ExcludeIndex, aConn.FQuery,
                      TableIndex, CopyBlobs, Dummy) = mrOK then begin  {!!.11}
    with FDatabaseItem.Tables[TableIndex] do begin
      Screen.Cursor := crHourGlass;
      { the copy operation is used in the context of the table
        that's being copied to. Use the timeout of the active
        table, otherwise the user has no way of setting timeout. }
      SaveTimeout := Table.Timeout;
      Table.Timeout := aConn.FQuery.Timeout;
      try
        Update;
        CopyRecords(aConn.FQuery, CopyBlobs);
      finally
        Screen.Cursor := crDefault;
        Table.Timeout := SaveTimeout;
        { force the second table to close if it wasn't open before }
        aConn.FSession.CloseInactiveTables;                         {!!.11}
      end;
    end;
  end;
end;

procedure TdlgQuery.CheckLastQueryType;
var
  Buffer   : PChar;
  BuffSize : Integer;
  ffSqlParser : TffSql;
begin
  ffSqlParser := TffSql.Create(NIL);
  BuffSize := Length(memSQL.Text) + 1;
  GetMem(Buffer, BuffSize);
  try
    StrPCopy(Buffer, memSQL.Text);

    ffSqlParser.SourceStream.SetSize(BuffSize);
    move(Buffer^, ffSqlParser.SourceStream.Memory^, BuffSize);
    ffSqlParser.Execute;
    FIsLastQuerySelect := Assigned(ffsqlParser.RootNode) and
                          Assigned(ffsqlParser.RootNode.TableExp);

  finally
    ffsqlParser.Free;
    FreeMem(Buffer, BuffSize);
  end;
end;

{Begin !!.11}
procedure TdlgQuery.UpdateDefaultTimeout;
var
  i : Integer;
begin
  for i := 0 to Pred(FConnections.Count) do
    TffSQLConnection(FConnections[i]).FClient.TimeOut := Config.DefaultTimeout;
end;

procedure TdlgQuery.btnParamValuesClick(Sender: TObject);
var
  aConn : TffSQLConnection;
  SaveSyntaxOnly : Boolean;
begin
  aConn := TffSQLConnection(FConnections[cmbQuery.ItemIndex]);
  { if query isn't active then update from memo etc and prepare statement }
  if not aConn.Query.Active then begin
    SaveSyntaxOnly := FSyntaxOnly;
    FSupressSyntaxOKDialog := True;
    try
      FSyntaxOnly := True;
      pbExecuteClick(Sender);
    finally
      FSyntaxOnly := SaveSyntaxOnly;
      FSupressSyntaxOKDialog := False;
    end;
  end;

  if aConn.Query.ParamCount=0 then begin
    MessageDlg('Current Query has no parameters', mtInformation, [mbOK], 0);
    Exit;
  end;

  if not Assigned(aConn.dlgParams) then
    aConn.dlgParams := TdlgParams.Create(Self);

  if aConn.dlgParams.EditParamValues(aConn.Query.Params) then
  if aConn.Query.Active then begin
    FSuppressParamsDialog := True;
    try
      pbExecuteClick(Sender);
    finally
      FSuppressParamsDialog := False;
    end;
  end;
end;
{End !!.11}

end.

