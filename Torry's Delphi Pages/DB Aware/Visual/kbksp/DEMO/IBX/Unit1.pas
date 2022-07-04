unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SelectPanel, ExtCtrls, DB, DBClient, Grids, DBGrids, DBCtrls,
  StdCtrls, DBTables, IBCustomDataSet, IBQuery, IBDatabase;

type
  TForm1 = class(TForm)
    SelectPanel1: TSelectPanel;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    SelectPanel2: TSelectPanel;
    IBDatabase1: TIBDatabase;
    IBTransaction1: TIBTransaction;
    Customer: TIBQuery;
    Button2: TButton;
    Memo1: TMemo;
    Employee: TIBQuery;
    DBGrid1: TDBGrid;
    DataSource3: TDataSource;
    IBQuery3: TIBQuery;
    IBQuery3PO_NUMBER: TIBStringField;
    IBQuery3CUST_NO: TIntegerField;
    IBQuery3SALES_REP: TSmallintField;
    IBQuery3ORDER_STATUS: TIBStringField;
    IBQuery3ORDER_DATE: TDateTimeField;
    IBQuery3SHIP_DATE: TDateTimeField;
    IBQuery3DATE_NEEDED: TDateTimeField;
    IBQuery3PAID: TIBStringField;
    IBQuery3QTY_ORDERED: TIntegerField;
    IBQuery3TOTAL_VALUE: TIBBCDField;
    IBQuery3DISCOUNT: TFloatField;
    IBQuery3ITEM_TYPE: TIBStringField;
    IBQuery3AGED: TFloatField;
    IBQuery3CUSTOMER: TStringField;
    IBQuery3FN: TStringField;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
//    A: TSelectPanel;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  IBDatabase1.Connected:=true;
  Customer.Open;
  Employee.Open;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  IBQuery3.Close;
  TruncateSQL(IBQuery3.SQL, 1);
  SelectPanel1.AddWhereClause(awcAnd, 'SALES_REP', IBQuery3.SQL);
  SelectPanel2.AddWhereClause(awcAnd, 'CUST_NO', IBQuery3.SQL);
  IBQuery3.SQL.Add('order by PO_NUMBER');
  Memo1.Lines.Assign(IBQuery3.SQL);
  IBQuery3.Open;
end;

end.
