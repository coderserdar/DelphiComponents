unit MSData;

interface

uses
  SysUtils, Classes, DB, ADODB, FxCommon, FxStore, FxDB;

type
  TdmMSSQL = class(TDataModule)
    Connection: TADOConnection;
    sales: TADODataSet;
    salesOrderDate: TDateTimeField;
    salesQty: TIntegerField;
    salesProductName: TWideStringField;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmMSSQL: TdmMSSQL;

implementation

{$R *.dfm}

end.
