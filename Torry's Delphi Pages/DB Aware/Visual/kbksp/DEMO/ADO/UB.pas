unit UB;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ADODB, StdCtrls, SelectPanel, Grids, DBGrids;

type
  TForm1 = class(TForm)
    SelectPanel1: TSelectPanel;
    ADOConnection1: TADOConnection;
    DataSource1: TDataSource;
    SelectPanel2: TSelectPanel;
    ADODataSet1: TADODataSet;
    ADODataSet2: TADODataSet;
    DataSource2: TDataSource;
    Memo1: TMemo;
    Button1: TButton;
    DBGrid1: TDBGrid;
    DataSource3: TDataSource;
    ADODataSet1SupplierID: TAutoIncField;
    ADODataSet1CompanyName: TWideStringField;
    ADODataSet1ContactName: TWideStringField;
    ADODataSet1ContactTitle: TWideStringField;
    ADODataSet1Address: TWideStringField;
    ADODataSet1City: TWideStringField;
    ADODataSet1Region: TWideStringField;
    ADODataSet1PostalCode: TWideStringField;
    ADODataSet1Country: TWideStringField;
    ADODataSet1Phone: TWideStringField;
    ADODataSet1Fax: TWideStringField;
    ADODataSet1HomePage: TMemoField;
    ADOQuery1: TADOQuery;
    ADOQuery1ProductID: TAutoIncField;
    ADOQuery1ProductName: TWideStringField;
    ADOQuery1SupplierID: TIntegerField;
    ADOQuery1CategoryID: TIntegerField;
    ADOQuery1QuantityPerUnit: TWideStringField;
    ADOQuery1UnitPrice: TBCDField;
    ADOQuery1UnitsInStock: TSmallintField;
    ADOQuery1UnitsOnOrder: TSmallintField;
    ADOQuery1ReorderLevel: TSmallintField;
    ADOQuery1Discontinued: TBooleanField;
    ADOQuery1Supplier: TStringField;
    ADOQuery1Category: TStringField;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ADODataSet1CompanyNameGetText(Sender: TField;
      var Text: String; DisplayText: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  ADOQuery1.Close;
  TruncateSQL(ADOQuery1.SQL, 1);
  SelectPanel1.AddWhereClause(awcAnd, 'SupplierID', ADOQuery1.SQL);
  SelectPanel2.AddWhereClause(awcAnd, 'CategoryID', ADOQuery1.SQL);
  ADOQuery1.SQL.Add('order by ProductName');
  Memo1.Lines.Assign(ADOQuery1.SQL);
  ADOQuery1.Open;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if not ADODataSet1.Active then ADODataSet1.Active:=true;
  if not ADODataSet2.Active then ADODataSet2.Active:=true;
  if not ADOQuery1.Active then ADOQuery1.Active:=true;
  Memo1.Lines.Assign(ADOQuery1.SQL);
end;

procedure TForm1.ADODataSet1CompanyNameGetText(Sender: TField;
  var Text: String; DisplayText: Boolean);
begin
  Text:=ADODataSet1CompanyName.AsString+', '+ADODataSet1Country.AsString;
end;

end.
