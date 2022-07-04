unit Unit1;

  { Delphi 6/7/2005/2006/2007 } //todo: 2009

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FMTBcd, DB, SqlExpr, StdCtrls, DBClient,
  {$IF CompilerVersion > 18.00}
  DBXCommon,
  {$ELSE}
  DBXpress,
  {$IFEND}
  {$IF CompilerVersion >= 19.00}
  DBXDynalink,
  {$IFEND}
  Provider, Grids, DBGrids, ComCtrls, ExtCtrls,
  {.$IF CompilerVersion > 17.00}
  WideStrings,
  {.$IFEND}
  {.$IF CompilerVersion > 18.00}
  DBXOracle,
  {.$IFEND}
  DbxOpenOdbcCallback, dbx_ora_connect, dbxoodbc_nextrs;

const
  ora_tns_name = 'TNS_DBDEMOS';
  ora_user_name = 'scott'; // scott
  ora_user_pswd = 'tiger'; // tiger

type
  // fix: TSQLStoredProc.NextrecordSet
  TSQLStoredProc = class(dbxoodbc_nextrs.TSQLStoredProcEx);

  TForm1 = class(TForm)
    SQLConnection1: TSQLConnection;
    SQLStoredProc1: TSQLStoredProc;
    DataSource1: TDataSource;
    DataSetProvider1: TDataSetProvider;
    ClientDataSet1: TClientDataSet;
    SQLQuery1: TSQLQuery;
    Panel1: TPanel;
    Panel2: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    StaticText1: TStaticText;
    cmbDriver: TComboBox;
    DBGrid1: TDBGrid;
    memLog: TMemo;
    btnOpen: TButton;
    btnNextRecordSet: TButton;
    btnClose: TButton;
    btnOpenStoredProc: TButton;
    btnTest: TButton;
    EStoredProcName: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure cmbDriverChange(Sender: TObject);
    procedure btnOpenStoredProcClick(Sender: TObject);
    procedure btnNextRecordSetClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
  private
    { Private declarations }
    SQLMonitor: TSQLMonitor;
    procedure SQLMonitorTrace(Sender: TObject; TraceInfo: TDBXTraceInfo; var LogTrace: Boolean);
    procedure CheckConnection;
    procedure CloseDS;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  SQLMonitor := TSQLMonitor.Create(Self);
  SQLMonitor.OnTrace := SQLMonitorTrace;
  SQLMonitor.SQLConnection := SQLConnection1;
  SQLMonitor.Active := True;
  //SQLMonitor.Active := False;

  //ora_user_name
  EStoredProcName.Items.Add( ora_user_name + '.pkg_test_dbxoodbc_01."Test_Proc_RefCursor_C"');

  cmbDriver.ItemIndex := 0;
  cmbDriverChange(cmbDriver); // connect to oracle
end;

procedure TForm1.cmbDriverChange(Sender: TObject);
begin
  CloseDS;
  SQLConnection1.Close;

  SQLConnection1.DriverName := '';
    SQLConnection1.GetDriverFunc := '';
  SQLConnection1.LibraryName := '';
  SQLConnection1.VendorLib := '';
  SQLConnection1.Params.Clear;

  if cmbDriver.Text = 'Oracle' then
  begin
    SQLConnection1.DriverName := 'Oracle';
    SQLConnection1.GetDriverFunc := 'getSQLDriverORACLE';
    SQLConnection1.LibraryName := 'dbxora.dll';
    SQLConnection1.VendorLib := 'oci.dll';
    SQLConnection1.Params.Values['Database'] := ora_tns_name;
  end
  else if cmbDriver.Text = 'OpenODBC' then
  begin
    SQLConnection1.DriverName := {dbx_ora_connect.pas:}oracle_dbxoodbc_driver_name;
    {$IF CompilerVersion < 17.50}
    SQLConnection1.GetDriverFunc := 'getSQLDriverODBC';
    {$ELSE}
    SQLConnection1.GetDriverFunc := 'getSQLDriverODBCW';
    {$IFEND}
    SQLConnection1.LibraryName := 'dbxoodbc.dll';
    SQLConnection1.VendorLib := 'SQORA32.DLL';
    SQLConnection1.Params.Text :=
      'DriverUnit=DBXDynalink' + #13#10 +
      'DriverPackageLoader=TDBXDynalinkDriverLoader' + #13#10 +
      ';DriverPackageLoader=TDBXDynalinkDriverCommonLoader' + #13#10 +
      'DriverPackage=DbxCommonDriver120.bpl' + #13#10 +
      'LibraryName=dbxoodbc.dll' + #13#10 +
      'GetDriverFunc=getSQLDriverODBCW' + #13#10 +
      'VendorLib=SQORA32.DLL' + #13#10 +
      oracle_dbxoodbc_driver_name + ' TransIsolation=ReadCommited' + #13#10 +
      'RowsetSize=20' + #13#10 +
      'BlobSize=-1' + #13#10 +
      'Trim Char=True' + #13#10 +
      'Custom String=coNetPacketSize=8192;coLockMode=-1;coBlobChunkSize=40960;coSchemFlt=0;coCatPrefix=UID';
    SQLConnection1.Params.Values['Database'] := 'DBQ=' + ora_tns_name + ';DBA=W;APA=T;FEN=T;QTO=T;FRC=10;FDL=10;LOB=T;RST=T;FRL=F;MTS=F;CSR=F;PFC=10;TLO=0';
    // OpenODBC calculate params automaticaly
    SQLStoredProc1.Params.Clear; { optional }
  end
  else if cmbDriver.Text = 'CoreLab'then
  begin
    SQLConnection1.DriverName := 'Oracle (Core Lab)';
    SQLConnection1.GetDriverFunc := 'getSQLDriverORA';
    {$IF CompilerVersion >= 18.00}
    SQLConnection1.LibraryName := 'dbexpoda30.dll';
    {$ELSE}
    SQLConnection1.LibraryName := 'dbexpoda.dll';
    {$IFEND}
    SQLConnection1.VendorLib := 'OCI.DLL';
    SQLConnection1.Params.Values['Database'] := ora_tns_name;
  end;

  SQLConnection1.Params.Values['User_Name'] := ora_user_name;
  SQLConnection1.Params.Values['Password'] := ora_user_pswd;
end;

procedure TForm1.btnOpenClick(Sender: TObject);
begin
  CheckConnection;
  CloseDS;
  if SQLStoredProc1 is TSQLStoredProcEx then
    SQLStoredProc1.Open;
  ClientDataSet1.Open;
end;

procedure TForm1.btnNextRecordSetClick(Sender: TObject);
begin
  if not SQLConnection1.Connected then
    Exit;
  if SQLStoredProc1 is TSQLStoredProcEx then
  begin
    ClientDataSet_NextRecordSet(ClientDataSet1, DataSetProvider1);
  end
  else
  begin
    ClientDataSet1.Close;
    if not SQLStoredProc1.Active then
      Exit;
    if (SQLStoredProc1.NextRecordSet <> nil) then
      ClientDataSet1.Open
    else
      ShowMessage('Next recordset not found !');
  end;
end;

procedure TForm1.btnCloseClick(Sender: TObject);
begin
  if not SQLConnection1.Connected then
    Exit;
  CloseDS;
  SQLConnection1.Close;
end;

procedure TForm1.btnOpenStoredProcClick(Sender: TObject);
begin
  ClientDataSet1.Close;
  ClientDataSet1.FieldDefs.Clear;
  ClientDataSet1.IndexDefs.Clear;
  //SQLStoredProc1.Close;
  //SQLStoredProc1.FieldDefs.Clear;
  //SQLStoredProc1.IndexDefs.Clear;
  CheckConnection;
  ClientDataSet1.Open;
end;

procedure TForm1.btnTestClick(Sender: TObject);
var
  s: String;
  i: Integer;
begin
  CheckConnection;
  CloseDS;

  SQLStoredProc1.Open;

  s := 'active = ' + IntToStr(Integer(SQLStoredProc1.Active)) + #$A;
  s := s + 'params.count = ' + IntToStr(SQLStoredProc1.Params.Count);

  for i := 0 to SQLStoredProc1.Params.Count - 1 do
    s := s + #$A'params[' + IntToStr(i) + '].DataType = ft' + FieldTypeNames[SQLStoredProc1.Params[i].DataType];

  if SQLStoredProc1.NextRecordSet <> nil then
    s := s + #$A'next record set - true'
  else
    s := s + #$A'next record set - false';
  ShowMessage(s);
end;

procedure TForm1.CheckConnection;
begin
  if not SQLConnection1.Connected then
    SQLConnection1.Connected := True;
end;

procedure TForm1.CloseDS;
begin
  ClientDataSet1.Close;
  ClientDataSet1.FieldDefs.Clear;
  ClientDataSet1.IndexDefs.Clear;
  // reset stored procedure
  SQLStoredProc1.Close;
  SQLStoredProc1.FieldDefs.Clear;
  SQLStoredProc1.IndexDefs.Clear;
  SQLStoredProc1.StoredProcName := '';
  SQLStoredProc1.Params.Clear;
  // reset provider
  DataSetProvider1.DataSet.Close;
  ClientDataSet1.ProviderName := '';
  ClientDataSet1.ProviderName := 'DataSetProvider1';
  // reset stored procedure name
  SQLStoredProc1.SQLConnection := nil; // disable read metadata/autologon
  SQLStoredProc1.StoredProcName := EStoredProcName.Items[EStoredProcName.ItemIndex];
  SQLStoredProc1.SQLConnection := SQLConnection1;
  SQLStoredProc1.Params.Clear;
  if cmbDriver.Text <> 'OpenODBC' then { optional }
  begin
    SQLStoredProc1.Params.CreateParam(ftCursor, 'rc1', ptOutput);
    SQLStoredProc1.Params.CreateParam(ftCursor, 'rc2', ptOutput);
  end;
end;

//
// SQLMonitor bugs:
//
//   Delphi 2007: not tracced connect/disconnect events (adapter cleared callback info)
//

procedure TForm1.SQLMonitorTrace(Sender: TObject; TraceInfo: TDBXTraceInfo; var LogTrace: Boolean);
var
  iTraceFlag: Integer;
  sTraceFlag, sTraceMessage: string;
begin
  if LogTrace then
  begin
    // compact log
    if memLog.Lines.Count > 1000 then
    begin
      with memLog.Lines do
      begin
        BeginUpdate;
        while Count >500 do
          Delete(0);
        EndUpdate;
      end;
    end;
    // append log new information
    DbxOpenOdbcCallback.DecodeTraceInfo(TraceInfo, iTraceFlag, sTraceFlag, sTraceMessage);

    memLog.Lines.Add('---- ---- ---- ---- ---- ---- ---- ----');
    memLog.Lines.Add(Format('Monitor Active: %d; TraceFlag: ( %3d: %s ); Message: %s', [
      Integer(SqlMonitor.Active), iTraceFlag, sTraceFlag,
      StringReplace(sTraceMessage, #13#10, ' ', [rfReplaceAll])]));
  end;
end;

end.
