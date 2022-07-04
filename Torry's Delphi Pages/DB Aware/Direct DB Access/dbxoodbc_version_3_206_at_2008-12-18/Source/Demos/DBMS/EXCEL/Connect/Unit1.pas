unit Unit1;

interface

  { Delphi 6/7/2005/2006/2007 } //todo: 2009

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, SqlExpr, ExtCtrls, StdCtrls,
  {$IF CompilerVersion < 18.50}
  DBXpress,
  {$ELSE}
  DBXCommon,
  {$IFEND}
  {$IF CompilerVersion >= 18.00}
  WideStrings,
  {$IFEND}
  dbx_excel_connect, DbxOpenOdbcCallback;

type
  TForm1 = class(TForm)
    SQLConnection: TSQLConnection;
    LUSER: TLabel;
    EUSER: TEdit;
    LPWD: TLabel;
    EPWD: TEdit;
    LDNS: TLabel;
    EDNS: TEdit;
    CDirectOdbc: TCheckBox;
    BConnect: TButton;
    BDisconnect: TButton;
    LAdd: TLabel;
    EAdditional: TEdit;
    LDB: TLabel;
    EMDBFILENAME: TEdit;
    lbl1: TLabel;
    btn_mdb_load: TButton;
    sh1: TShape;
    CUnicodeDriver: TCheckBox;
    OD: TOpenDialog;
    memLog: TMemo;
    procedure BConnectClick(Sender: TObject);
    procedure BDisconnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SQLConnectionAfterConnect(Sender: TObject);
    procedure SQLConnectionAfterDisconnect(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn_mdb_loadClick(Sender: TObject);
  private
    { Private declarations }
    SQLMonitor: TSQLMonitor;
    procedure SQLMonitorTrace(Sender: TObject; TraceInfo: TDBXTraceInfo; var LogTrace: Boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  SQLMonitor := TSQLMonitor.Create(Self);
  SQLMonitor.OnTrace := SQLMonitorTrace;
  SQLMonitor.SQLConnection := SQLConnection;
  SQLMonitor.Active := True;
  //SQLMonitor.Active := False;

  {$IF CompilerVersion >= 18.00}
  {$IF CompilerVersion >= 18.50}
  CUnicodeDriver.Checked := True;
  CUnicodeDriver.Enabled := False;
  {$ELSE}
  CUnicodeDriver.Checked := True;
  CUnicodeDriver.Enabled := True;
  {$IFEND}
  {$ELSE}
  CUnicodeDriver.Checked := False;
  CUnicodeDriver.Enabled := False;
  {$IFEND}
  //EMDBFILENAME.Text := ExtractFilePath(ParamStr(0)) + EMDBFILENAME.Text; //'dbdemos.xls';
  EMDBFILENAME.Text := '..\' + EMDBFILENAME.Text; //'dbdemos.xls';
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  SQLConnection.Connected := False;
end;

procedure TForm1.BConnectClick(Sender: TObject);
begin
  if (not CDirectOdbc.Enabled) and (EDNS.Text = '') then
    raise Exception.Create('It is necessary to set DNS (DataSource Name).');
  if CUnicodeDriver.Checked then
    dbx_excel_connect.ExcelConnectW(SQLConnection,
      EMDBFILENAME.Text, EDNS.Text, {AutoCreate:}True,
      CDirectOdbc.Enabled,
      EUser.Text <> '', EUser.Text, EAdditional.Text)
  else
    dbx_excel_connect.ExcelConnect(SQLConnection,
      EMDBFILENAME.Text, EDNS.Text, {AutoCreate:}True,
      CDirectOdbc.Enabled,
      EUser.Text <> '', EUser.Text, EAdditional.Text);
  {
  ShowMessage('SQLConnection:'#13+
    #13'GetDriverFunc='+SQLConnection.GetDriverFunc+
    #13'DriverName='+SQLConnection.DriverName+
    #13'LibraryName='+SQLConnection.LibraryName+
    #13'VendorLib'+SQLConnection.VendorLib+
    #13'Params='#13+SQLConnection.Params.Text
  );
  {}
end;

procedure TForm1.BDisconnectClick(Sender: TObject);
begin
  SQLConnection.Connected := False;
end;

procedure TForm1.SQLConnectionAfterConnect(Sender: TObject);
begin
  sh1.Brush.Color := clRed;
end;

procedure TForm1.SQLConnectionAfterDisconnect(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
    sh1.Brush.Color := clGray;
end;


procedure TForm1.btn_mdb_loadClick(Sender: TObject);
begin
  if OD.Execute then
  begin
    EMDBFILENAME.Text := OD.FileName;
    SQLConnection.Connected := False;
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
