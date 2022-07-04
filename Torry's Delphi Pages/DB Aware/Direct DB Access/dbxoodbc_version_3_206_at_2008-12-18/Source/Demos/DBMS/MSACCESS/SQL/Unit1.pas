unit Unit1;

  { Delphi 6/7/2005/2006/2007 }

interface

uses
  {$IF CompilerVersion < 18.50}
  DBXpress,
  {$ELSE}
  DBXCommon,
  {$IFEND}
  {$IF CompilerVersion > 17.00}
  WideStrings,
  {$IFEND}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB, SqlExpr,
  ExtCtrls, FMTBcd, DBClient, DBCtrls, Grids, DBGrids, Provider, ComCtrls;

type
  TForm1 = class(TForm)
    SQLConnection: TSQLConnection;
    btn_connect: TButton;
    sh1: TShape;
    SQLQuery: TSQLQuery;
    btn_open_query: TButton;
    CDS: TClientDataSet;
    mem_sql_text: TMemo;
    btn_query_exec: TButton;
    btn_cds_open: TButton;
    DSP: TDataSetProvider;
    DataSource: TDataSource;
    dbnav1: TDBNavigator;
    btn_cds_close: TButton;
    btn_open_close: TButton;
    btn_disconnect: TButton;
    btn_cds_apply: TButton;
    chk_unicode_dbx: TCheckBox;
    chk_ansi_string: TCheckBox;
    chk_unicode_odbc: TCheckBox;
    PC: TPageControl;
    ts_grid: TTabSheet;
    ts_blob: TTabSheet;
    grd1: TDBGrid;
    p1: TPanel;
    db_memo: TDBMemo;
    cbx_fields: TComboBox;
    txt1: TStaticText;
    cbx_query: TComboBox;
    pc_log: TPageControl;
    sh_mem_log: TTabSheet;
    mem_log: TMemo;
    sh_sql_monitor: TTabSheet;
    mem_sql_monitor: TMemo;
    procedure btn_connectClick(Sender: TObject);
    procedure SQLConnectionAfterConnect(Sender: TObject);
    procedure SQLConnectionAfterDisconnect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn_open_queryClick(Sender: TObject);
    procedure btn_query_execClick(Sender: TObject);
    procedure btn_cds_openClick(Sender: TObject);
    procedure btn_open_closeClick(Sender: TObject);
    procedure btn_cds_closeClick(Sender: TObject);
    procedure btn_disconnectClick(Sender: TObject);
    procedure btn_cds_applyClick(Sender: TObject);
    procedure CDSBeforeClose(DataSet: TDataSet);
    procedure CDSAfterOpen(DataSet: TDataSet);
    procedure cbx_fieldsChange(Sender: TObject);
    procedure cbx_queryChange(Sender: TObject);
    {$IF CompilerVersion >= 18.50}
    procedure SQLMonitorTrace(Sender: TObject; TraceInfo: TDBXTraceInfo; var LogTrace: Boolean);
    {$ELSE}
    procedure SQLMonitorTrace(Sender: TObject; CBInfo: pSQLTRACEDesc; var LogTrace: Boolean);
    {$IFEND}
  private
    { Private declarations }
    SQLMonitor: TSQLMonitor;
    procedure CheckConnection();
  public
    { Public declarations }
    procedure print_dataset(D: TDataSet; ClearMemo: Boolean = True);
  end;

var
  Form1: TForm1;

implementation

uses
  dbx_access_connect;

{$R *.dfm}

function FxtSetScrollBarPos(AControl: TWinControl; Kind: TScrollBarKind;
  Position: Integer; bRedraw: Boolean = True): Boolean;
const
  cSBCode: array[Boolean] of integer = (WS_HSCROLL, WS_VSCROLL);
var
  vSBCode: Integer;
begin
  Result := (AControl <> nil) and (AControl.HandleAllocated);
  if not Result then
    Exit;
  vSBCode := cSBCode[Kind <> sbHorizontal];
  Result := (GetWindowLong(AControl.Handle, GWL_STYLE) and vSBCode) <> 0;
  if Result then
    SetScrollPos(AControl.Handle, vSBCode, Position, bRedraw);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SQLMonitor := TSQLMonitor.Create(Self);
  SQLMonitor.OnTrace := SQLMonitorTrace;
  SQLMonitor.SQLConnection := SQLConnection;

  SQLMonitor.Active := True;
//  SQLMonitor.Active := False;

  SQLConnectionAfterDisconnect(SQLConnection);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
//  cds.Close;
//  SQLConnection.Connected := False; { !!!: else AV on close }
end;

procedure TForm1.btn_connectClick(Sender: TObject);
begin
{
procedure AccessConnect(SQLConnection: TSQLConnection;
  const mdb_file_name: string;
  const DNS_NAME: string = '';
  DirectOdbc: Boolean = True;
  LoginPrompt: Boolean = False;
  UserName: string = '';
  Password: string = '';
  const AdditionalOptions: string = '';
  bUnicodeOdbcApi: Boolean = False;
  bAnsiStringField: Boolean = True;
  bUnicodeDriver: Boolean = False
);
{}
  dbx_access_connect.AccessConnect(SQLConnection,
    // mdb_file_name:
      ExtractFilePath(ParamStr(0)) + 'dbdemos.mdb',
    // DNS_NAME
      '',
    // DirectOdbc
      True,
    // LoginPrompt
      False,
    // UserName
      '',
    // Password
      '',
    // AdditionalOptions
      '',
    // bUnicodeOdbcApi
      chk_unicode_odbc.Checked,
    // bAnsiStringField
      chk_ansi_string.Checked,
    // bUnicodeDriver
      chk_unicode_dbx.Checked
  );
end;

procedure TForm1.CDSAfterOpen(DataSet: TDataSet);
var
  i: Integer;
begin
  cbx_fields.Items.Clear;
  for i := 0 to CDS.FieldCount - 1 do
    cbx_fields.Items.Add(CDS.Fields[i].FieldName);
end;

procedure TForm1.CDSBeforeClose(DataSet: TDataSet);
begin
  db_memo.DataField := '';
  cbx_fields.ItemIndex := -1;
  cbx_fields.Items.Clear;
  cbx_fields.Text := '';
end;

procedure TForm1.CheckConnection();
begin
  if not SQLConnection.Connected then
    //Abort;
    raise Exception.Create('Set connection with server MSAccess');
end;

procedure TForm1.btn_disconnectClick(Sender: TObject);
begin
  cds.Close;
  SQLConnection.Connected := False;
end;

procedure TForm1.SQLConnectionAfterConnect(Sender: TObject);
begin
  sh1.Brush.Color := clRed;

  chk_unicode_odbc.Enabled := False;
  chk_ansi_string.Enabled := False;
  chk_unicode_dbx.Enabled := False;

  btn_query_exec.Enabled := True;
  btn_open_query.Enabled := True;
  btn_open_close.Enabled := True;
  btn_cds_open.Enabled := True;
  btn_cds_close.Enabled := True;
  btn_cds_apply.Enabled := True;
end;

procedure TForm1.SQLConnectionAfterDisconnect(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
  begin
    sh1.Brush.Color := clGray;

    chk_unicode_odbc.Enabled := True;
    chk_ansi_string.Enabled := True;
    chk_unicode_dbx.Enabled := True;

    btn_query_exec.Enabled := False;
    btn_open_query.Enabled := False;
    btn_open_close.Enabled := False;
    btn_cds_open.Enabled := False;
    btn_cds_close.Enabled := False;
    btn_cds_apply.Enabled := False;
  end;
end;

{$IF CompilerVersion >= 18.50}
procedure TForm1.SQLMonitorTrace(Sender: TObject; TraceInfo: TDBXTraceInfo; var LogTrace: Boolean);
begin
  if LogTrace then
  begin
    mem_sql_monitor.Lines.Add(Format('Monitor Active: %d; TraceFlag: %2d; Message: %s', [
      Integer(SqlMonitor.Active),
      TraceInfo.TraceFlag,
      StringReplace(TraceInfo.Message, #13#10, ' ', [rfReplaceAll])])
    );
    //FxtSetScrollBarPos(mem_sql_monitor, sbVertical, mem_sql_monitor.Lines.Count, True);
  end;
end;

{$ELSE}
procedure TForm1.SQLMonitorTrace(Sender: TObject; CBInfo: pSQLTRACEDesc; var LogTrace: Boolean);
var
  ws: WideString;
  sMsg, sTraceCat: string;
begin
  if Assigned(CBInfo) and (CBInfo.uTotalMsgLen > 0) then
  begin
    SetLength(ws, CBInfo.uTotalMsgLen);
    Move(CBInfo.pszTrace[0], ws[1], CBInfo.uTotalMsgLen * SizeOf(WideChar));
    sMsg := Trim(AnsiString(ws));
    case CBInfo.eTraceCat of
      trQPREPARE: sTraceCat := 'prepare';  { prepared query statements }
      trQEXECUTE: sTraceCat := 'execute';  { executed query statements }
      trERROR:    sTraceCat := 'error';    { vendor errors }
      trSTMT:     sTraceCat := 'stmt';     { statement ops (i.e. allocate, free) }
      trCONNECT:  sTraceCat := 'connect';  { connect / disconnect }
      trTRANSACT: sTraceCat := 'transact'; { transaction }
      trBLOB:     sTraceCat := 'blob i/o'; { blob i/o }
      trMISC:     sTraceCat := 'misc';     { misc. }
      trVENDOR:   sTraceCat := 'vendor';   { vendor calls }
      trDATAIN:   sTraceCat := 'datain';   { parameter bound data }
      trDATAOUT:  sTraceCat := 'dataout';  { trace fetched data }
      else { trUNKNOWN: }
                  sTraceCat := 'unknown ';
    end;
    mem_sql_monitor.Lines.Add('**************** ' + sTraceCat + ':');
    mem_sql_monitor.Lines.Add(sMsg);
    FxtSetScrollBarPos(mem_sql_monitor, sbVertical, mem_sql_monitor.Lines.Count, True);
  end;
end;
{$IFEND}

procedure TForm1.btn_query_execClick(Sender: TObject);
begin
  mem_log.Lines.Clear;
  //if not SQLConnection.Connected then
  //  Exit;
  CheckConnection();
  SQLQuery.Close;
  SQLQuery.SQL.Text := mem_sql_text.Lines.Text;
  SQLQuery.ExecSQL;
end;

procedure TForm1.cbx_fieldsChange(Sender: TObject);
begin
  db_memo.DataField := '';
  if cbx_fields.ItemIndex >= 0 then
    db_memo.DataField := cbx_fields.Text;
end;

procedure TForm1.cbx_queryChange(Sender: TObject);
begin
  mem_sql_text.Lines.Text := cbx_query.Text;
end;

procedure TForm1.btn_open_queryClick(Sender: TObject);
begin
  pc_log.ActivePageIndex := 0;
  mem_log.Lines.Clear;
  //if not SQLConnection.Connected then
  //  Exit;
  CheckConnection();
  SQLQuery.Close;
  SQLQuery.SQL.Text := mem_sql_text.Lines.Text;
  SQLQuery.Open;
  print_dataset(SQLQuery);
end;

procedure TForm1.btn_open_closeClick(Sender: TObject);
begin
  SQLQuery.Close;
  mem_log.Lines.Clear;
end;

procedure TForm1.btn_cds_openClick(Sender: TObject);
begin
  mem_log.Lines.Clear;
  cds.Close;
  //if not SQLConnection.Connected then
  //  Exit;
  CheckConnection();
  SQLQuery.Close;
  SQLQuery.SQL.Text := mem_sql_text.Lines.Text;
  cds.Open;
end;

procedure TForm1.btn_cds_applyClick(Sender: TObject);
begin
  //if not SQLConnection.Connected then
  //  Exit;
  CheckConnection();
  if not cds.Active then
    //Exit;
    raise Exception.Create('CDS is not open');
  cds.ApplyUpdates(0);
end;

procedure TForm1.btn_cds_closeClick(Sender: TObject);
begin
  mem_log.Lines.Clear;
  cds.Close;
  SQLQuery.Close;
end;

procedure TForm1.print_dataset(D: TDataSet; ClearMemo: Boolean = True);
const
  iColMaxCount = 17;
  iRowMaxCount = 7;
var
  iCol, iRow, i: Integer;
  s: string;
begin
  if ClearMemo then
    mem_log.Lines.Clear;

  iCol := D.FieldCount-1;
  if iCol > iColMaxCount-1 then
    iCol := iColMaxCount-1;
  iRow := 0;

  while (not D.Eof) and (iRow < iRowMaxCount) do with mem_log.Lines do
  begin
    Inc(iRow);

    Add('-----------------------------');
    Add(Format('  Row num %d: values', [iRow]));
    Add('-----------------------------');
    for i := 0 to iCol do
    begin
      with D.Fields[i] do
        S := '    ' + FieldName + ' = ' + DisplayText;
      Add(S);
    end;

    D.Next;
  end;
end;

end.
