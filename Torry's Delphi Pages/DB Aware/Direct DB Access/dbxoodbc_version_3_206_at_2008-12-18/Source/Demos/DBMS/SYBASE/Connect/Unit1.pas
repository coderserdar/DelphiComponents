unit Unit1;

interface

// !!! dbx_sybase_connect

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  {$IF CompilerVersion < 18.50}
  DBXpress,
  {$ELSE}
  DBXCommon,
  {$IFEND}
  Dialogs, DB, SqlExpr, StdCtrls, FMTBcd, Grids,
  DBGrids, DBClient, Provider, ExtCtrls, dbx_sybase_connect;

type
  TForm1 = class(TForm)
    SQLConnection: TSQLConnection;
    LSRV: TLabel;
    ESRV: TEdit;
    LUSER: TLabel;
    EUSER: TEdit;
    LPWD: TLabel;
    EPWD: TEdit;
    LDNS: TLabel;
    EDNS: TEdit;
    CDirectOdbc: TCheckBox;
    BConnect: TButton;
    BDisconnect: TButton;
    SQLStoredProc: TSQLStoredProc;
    BSPExec: TButton;
    LAdd: TLabel;
    EAdditional: TEdit;
    DataSource: TDataSource;
    DSProv: TDataSetProvider;
    CDS: TClientDataSet;
    Grid: TDBGrid;
    LDB: TLabel;
    EDB: TEdit;
    RServerType: TRadioGroup;
    procedure BConnectClick(Sender: TObject);
    procedure BDisconnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BSPExecClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    fServerTypes: array[TSybaseServerType] of Boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.BConnectClick(Sender: TObject);
begin
  if (not CDirectOdbc.Enabled) and (EDNS.Text = '') then
    raise Exception.Create('It is necessary to set DNS (DataSource Name).');

  dbx_sybase_connect.SybaseConnect(
    SQLConnection,
    {SybaseServerType=}TSybaseServerType(RServerType.ItemIndex+1),
    {SERVER=} ESRV.Text,
    {DATABASE=} EDB.Text,
    {User=} EUSER.Text,
    {Password=} EPWD.Text,
    {DirectOdbc=} CDirectOdbc.Checked,
    {LoginPrompt=} EUSER.Text = '',
    {DNS=}EDNS.Text,
    {AdditionalOptions=}EAdditional.Text
  );
end;

procedure TForm1.BDisconnectClick(Sender: TObject);
begin
  SQLConnection.Connected := False;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: TSybaseServerType;
  bIsAnyServer: Boolean;
begin
  { Check Sybase Client APIs }

  fServerTypes[sstUnknown] := True;
  bIsAnyServer := False;
  RServerType.ItemIndex := -1;
  for i := Succ(Low(TSybaseServerType)) to High(TSybaseServerType) do
  begin
    fServerTypes[i] := IsPresentedSybaseDriver(i);
    if (fServerTypes[i]) and (not bIsAnyServer) then
    begin
      bIsAnyServer := True;
      RServerType.ItemIndex := Integer(i);
    end;
  end;

  RServerType.Enabled := bIsAnyServer;

  CDirectOdbc.Checked := bIsAnyServer;
  CDirectOdbc.Enabled := bIsAnyServer;

  { Grid position }

  Grid.Left := 4;
  Grid.Top := BConnect.Top + BConnect.Height + 10;
  Grid.Height := ClientHeight - Grid.Top - 4;
  Grid.Width := ClientWidth - 8;
end;

procedure TForm1.BSPExecClick(Sender: TObject);
var
  iResult: Integer;
//  S: string;
begin
  SQLStoredProc.Close;
  SQLStoredProc.ParamCheck := False;
  iResult := SQLStoredProc.ExecProc();
  ShowMessage('Result = "' + IntToStr(iResult) + '"');
(*
  //S := SQLStoredProc.StoredProcName;
  //SQLStoredProc.StoredProcName := '';
  //SQLStoredProc.StoredProcName := S;
 // SQLStoredProc.Open();
  //ShowMessage('Result = "' + SQLStoredProc.Params[0].AsString);

  //SQLStoredProc.NextRecordSet();

  //CDS.Close;
  //DSProv.DataSet := SQLStoredProc.NextRecordSet();
  //CDS.Open;

//*)
end;

end.
