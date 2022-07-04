// basic unit test
// originator Sean Cross
//
unit ModTestU;

interface

uses
  SysUtils, Classes, ASGSQLite3, DB;

type
  TmodTest = class(TDataModule)
    DbTest: TASQLite3DB;
    InlineCreateTables: TASQLite3InlineSQL;
    InlineCreateData: TASQLite3InlineSQL;
    sqlTemp: TASQLite3Query;
    tblTemp: TASQLite3Table;
    tblCalc_: TASQLite3Table;
    tblCalc_FPKey: TIntegerField;
    tblCalc_FInt: TIntegerField;
    tblCalc_CalcInt: TIntegerField;
    tblCalc_CalcString: TStringField;
    tblCalc_FText: TStringField;
    tblMaster: TASQLite3Table;
    TBLDetail: TASQLite3Table;
    DSMaster: TDataSource;
    tblOutOfOrder_: TASQLite3Table;
    tblOutOfOrder_FPKey: TIntegerField;
    tblOutOfOrder_FBlob: TMemoField;
    sqlOutOfOrder_: TASQLite3Query;
    sqlOutOfOrder_FPKey: TIntegerField;
    sqlOutOfOrder_FBlob: TMemoField;
    procedure tblCalc_CalcFields(DataSet: TDataSet);
  private
    { Private declarations }
  public
    procedure CreateDatabase;
    function  GetIntegerFromSql(ASql: string): integer;
    procedure OpenSql(ASql: string);
    procedure OpenTable(ATableName: string);
  end;

//var
//  modTest: TmodTest;

implementation

{$R *.dfm}

{ TDataModule1 }

procedure TmodTest.CreateDatabase;
begin
  if DbTest.Connected then
    DbTest.Close;

  DbTest.Open;
  DbTest.SQLite3_ExecSQL(InlineCreateData.SQL.Text);
end;


function TmodTest.GetIntegerFromSql(ASql: string): integer;
begin
  OpenSql(ASql);

  result:= sqlTemp.Fields[0].AsInteger;
  sqlTemp.Close;
end;

procedure TmodTest.OpenSql(ASql: string);
begin
  sqlTemp.Close;
  sqlTemp.SQL.Text:= ASql;
  sqlTemp.Open;
end;

procedure TmodTest.OpenTable(ATableName: string);
begin
  tblTemp.Close;
  tblTemp.TableName:= ATableName;
  tblTemp.Open;
end;

procedure TmodTest.tblCalc_CalcFields(DataSet: TDataSet);
begin
  tblCalc_CalcInt.AsInteger:= tblCalc_FInt.AsInteger + 10;
  tblCalc_CalcString.AsString:= 'Calc' + tblCalc_FInt.AsString;
end;

end.
