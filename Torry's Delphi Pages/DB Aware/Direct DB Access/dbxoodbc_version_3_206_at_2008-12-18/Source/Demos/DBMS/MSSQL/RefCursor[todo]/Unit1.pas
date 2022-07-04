unit Unit1;

{$B-,O-,D+,L+}

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
  //dxoodbcldr,
  //DBXDynalinkNative,
  DBXDynalink,
  {$IFEND}
  Provider, Grids, DBGrids, ComCtrls, ExtCtrls,
  {.$IF CompilerVersion > 17.00}
  WideStrings,
  {.$IFEND}
  {.$IF CompilerVersion > 18.00}
  DbxMSSQL,
  {.$IFEND}
  dbxoodbc_nextrs;

type
  // fix: TSQLStoredProc.NextrecordSet
  TSQLStoredProc = class(dbxoodbc_nextrs.TSQLStoredProcEx);

  TForm1 = class(TForm)
    SQLConnection1: TSQLConnection;
    SQLStoredProc1: TSQLStoredProc;
    btnTest: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
  private
    { Private declarations }
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

const
  sVendorLib_MSSQL_2000 = 'sqlsrv32.dll';  // is present by default in Windows
  sVendorLib_MSSQL_2005 = 'sqlncli.dll';   // need install into PC
  sVendorLib_MSSQL_2009 = 'sqlncli10.dll'; // need install into PC

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$IF CompilerVersion >= 15.00}
  SQLStoredProc1.GetMetadata := False;
  {$IFEND}
  SQLConnection1.DriverName := 'DbxSQLServer';
  {$IF CompilerVersion < 17.50}
  SQLConnection1.GetDriverFunc := 'getSQLDriverODBC';
  {$ELSE}
  SQLConnection1.GetDriverFunc := 'getSQLDriverODBCW';
  {$IFEND}
  SQLConnection1.LibraryName := 'dbxoodbc.dll';
  SQLConnection1.VendorLib := sVendorLib_MSSQL_2000;
  SQLConnection1.Params.Clear;
  SQLConnection1.Params.Values['Database'] := 'UID=user1;PWD=pwd1;DATABASE=dbxoodbc;SERVER=mssql-host';
end;

procedure TForm1.CheckConnection;
begin
  SQLConnection1.Connected := True;
end;

procedure TForm1.CloseDS;
begin
//  ClientDataSet1.Close;
//  ClientDataSet1.FieldDefs.Clear;
//  ClientDataSet1.IndexDefs.Clear;
  // reset stored procedure
  SQLStoredProc1.Close;
  SQLStoredProc1.FieldDefs.Clear;
  SQLStoredProc1.IndexDefs.Clear;
  SQLStoredProc1.StoredProcName := '';
  SQLStoredProc1.Params.Clear;
  // reset provider
//  DataSetProvider1.DataSet.Close;
//  ClientDataSet1.ProviderName := '';
//  ClientDataSet1.ProviderName := 'DataSetProvider1';
  // reset stored procedure name
  SQLStoredProc1.SQLConnection := nil; // disable read metadata/autologon
  SQLStoredProc1.StoredProcName := '[dbo].[test_refcursor2]';
  SQLStoredProc1.SQLConnection := SQLConnection1;
  SQLStoredProc1.Params.Clear;
  //if cmbDriver.Text <> 'OpenODBC' then { optional }
  //begin
  //  SQLStoredProc1.Params.CreateParam(ftCursor, 'rc1', ptOutput);
  //  SQLStoredProc1.Params.CreateParam(ftCursor, 'rc2', ptOutput);
  //end;
end;

procedure TForm1.btnTestClick(Sender: TObject);
var
  s: String;
begin
  CheckConnection;
  CloseDS;

  SQLStoredProc1.Open;

  s := 'active = ' + IntToStr(Integer(SQLStoredProc1.Active)) + #$A;
  if SQLStoredProc1.NextRecordSet <> nil then
    s := s + #$A'next record set - true'
  else
    s := s + #$A'next record set - false';

  ShowMessage(s);

  SQLStoredProc1.Close;
end;

end.
