unit datasets;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, DBTables, DB;

type
  TCustomerData = class(TDataModule)
    Customers: TTable;
    Orders: TTable;
    LineItems: TTable;
    CustomersCustNo: TFloatField;
    CustomersCompany: TStringField;
    CustomersAddr1: TStringField;
    CustomersAddr2: TStringField;
    CustomersCity: TStringField;
    CustomersState: TStringField;
    CustomersZip: TStringField;
    CustomersCountry: TStringField;
    CustomersPhone: TStringField;
    CustomersFAX: TStringField;
    CustomersTaxRate: TFloatField;
    CustomersContact: TStringField;
    CustomersLastInvoiceDate: TDateTimeField;
    OrdersOrderNo: TFloatField;
    OrdersCustNo: TFloatField;
    OrdersSaleDate: TDateTimeField;
    OrdersShipDate: TDateTimeField;
    OrdersEmpNo: TIntegerField;
    OrdersShipToContact: TStringField;
    OrdersShipToAddr1: TStringField;
    OrdersShipToAddr2: TStringField;
    OrdersShipToCity: TStringField;
    OrdersShipToState: TStringField;
    OrdersShipToZip: TStringField;
    OrdersShipToCountry: TStringField;
    OrdersShipToPhone: TStringField;
    OrdersShipVIA: TStringField;
    OrdersPO: TStringField;
    OrdersTerms: TStringField;
    OrdersPaymentMethod: TStringField;
    OrdersItemsTotal: TCurrencyField;
    OrdersTaxRate: TFloatField;
    OrdersFreight: TCurrencyField;
    OrdersAmountPaid: TCurrencyField;
    OrdersCustCompany: TStringField;
    LineItemsOrderNo: TFloatField;
    LineItemsItemNo: TFloatField;
    LineItemsPartNo: TFloatField;
    LineItemsQty: TIntegerField;
    LineItemsDiscount: TFloatField;
    LineItemsPrice: TCurrencyField;
    Parts: TTable;
    PartsPartNo: TFloatField;
    PartsVendorNo: TFloatField;
    PartsDescription: TStringField;
    PartsOnHand: TFloatField;
    PartsOnOrder: TFloatField;
    PartsCost: TCurrencyField;
    PartsListPrice: TCurrencyField;
    LineItemsTotal: TCurrencyField;
    LineItemsPartName: TStringField;
    CustomerSource: TDataSource;
    OrderSource: TDataSource;
    LineItemSource: TDataSource;
    PartSource: TDataSource;
    LineItemsExtendedPrice: TCurrencyField;
    RepQuery: TQuery;
    RepQueryCustNo: TFloatField;
    RepQueryCompany: TStringField;
    RepQueryAddr1: TStringField;
    RepQueryAddr2: TStringField;
    RepQueryCity: TStringField;
    RepQueryState: TStringField;
    RepQueryZip: TStringField;
    RepQueryCountry: TStringField;
    RepQueryPhone: TStringField;
    RepQueryFAX: TStringField;
    RepQueryTaxRate: TFloatField;
    RepQueryContact: TStringField;
    RepQueryLastInvoiceDate: TDateTimeField;
    RepQueryOrderNo: TFloatField;
    RepQueryCustNo_1: TFloatField;
    RepQuerySaleDate: TDateTimeField;
    RepQueryShipDate: TDateTimeField;
    RepQueryEmpNo: TIntegerField;
    RepQueryShipToContact: TStringField;
    RepQueryShipToAddr1: TStringField;
    RepQueryShipToAddr2: TStringField;
    RepQueryShipToCity: TStringField;
    RepQueryShipToState: TStringField;
    RepQueryShipToZip: TStringField;
    RepQueryShipToCountry: TStringField;
    RepQueryShipToPhone: TStringField;
    RepQueryShipVIA: TStringField;
    RepQueryPO: TStringField;
    RepQueryTerms: TStringField;
    RepQueryPaymentMethod: TStringField;
    RepQueryItemsTotal: TCurrencyField;
    RepQueryTaxRate_1: TFloatField;
    RepQueryFreight: TCurrencyField;
    RepQueryAmountPaid: TCurrencyField;
    RepQueryOrderNo_1: TFloatField;
    RepQueryItemNo: TFloatField;
    RepQueryPartNo: TFloatField;
    RepQueryQty: TIntegerField;
    RepQueryDiscount: TFloatField;
    RepQueryPartNo_1: TFloatField;
    RepQueryVendorNo: TFloatField;
    RepQueryDescription: TStringField;
    RepQueryOnHand: TFloatField;
    RepQueryOnOrder: TFloatField;
    RepQueryCost: TCurrencyField;
    RepQueryListPrice: TCurrencyField;
    RepQuerySource: TDataSource;
    procedure LineItemsCalcFields(DataSet: TDataSet);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CustomerData: TCustomerData;

implementation

{$R *.DFM}

procedure TCustomerData.LineItemsCalcFields(DataSet: TDataSet);
begin
  LineItemsExtendedPrice.Value := LineItemsPrice.Value *
    ((100 - LineItemsDiscount.Value) / 100);
  LineItemsTotal.Value := LineItemsExtendedPrice.Value *
    LineItemsQty.Value;
end;

end.
