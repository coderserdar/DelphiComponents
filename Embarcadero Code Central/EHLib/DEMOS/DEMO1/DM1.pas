unit DM1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, PrnDbgeh, Db, DBTables, ImgList;

type
  TDataModule1 = class(TDataModule)
    Table1: TTable;
    DataSource2: TDataSource;
    Table2: TTable;
    DataSource3: TDataSource;
    ImageList2: TImageList;
    ilArrows: TImageList;
    ImageList1: TImageList;
    Query1: TQuery;
    Query1VNo: TFloatField;
    Query1VName: TStringField;
    Query1PNo: TFloatField;
    Query1PDescription: TStringField;
    Query1PCost: TCurrencyField;
    Query1IQty: TIntegerField;
    Query1VName1: TStringField;
    Query1VPreferred: TBooleanField;
    DataSource1: TDataSource;
    qrVendors: TQuery;
    qrVendorsVendorNo: TFloatField;
    qrVendorsVendorName: TStringField;
    qrVendorsAddress1: TStringField;
    qrVendorsAddress2: TStringField;
    qrVendorsCity: TStringField;
    qrVendorsState: TStringField;
    qrVendorsZip: TStringField;
    qrVendorsCountry: TStringField;
    qrVendorsPhone: TStringField;
    qrVendorsFAX: TStringField;
    qrVendorsPreferred: TBooleanField;
    dsVendors: TDataSource;
    tCustomer: TTable;
    dstCustomer: TDataSource;
    tEmployee: TTable;
    tEmployeeEmpNo: TIntegerField;
    tEmployeeLastName: TStringField;
    tEmployeeFirstName: TStringField;
    tEmployeePhoneExt: TStringField;
    tEmployeeHireDate: TDateTimeField;
    tEmployeeSalary: TFloatField;
    tEmployeeSalaryType: TIntegerField;
    qCustomer: TQuery;
    qCustomer2: TQuery;
    dsCustomer2: TDataSource;
    dsCustomer: TDataSource;
    dsEmployee: TDataSource;
    dsPartsDescriprion: TDataSource;
    qrPartsDescriprion: TQuery;
    ilYesNo: TImageList;
    ilPaymentType: TImageList;
    procedure tEmployeeCalcFields(DataSet: TDataSet);
    procedure Query1UpdateRecord(DataSet: TDataSet;
      UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
    procedure qCustomerUpdateRecord(DataSet: TDataSet;
      UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
    procedure qCustomer2UpdateRecord(DataSet: TDataSet;
      UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModule1: TDataModule1;

implementation

{$R *.DFM}

procedure TDataModule1.tEmployeeCalcFields(DataSet: TDataSet);
begin
  if (tEmployeeSalary.AsFloat < 15000) then
    tEmployeeSalaryType.AsFloat := 5
  else if (tEmployeeSalary.AsFloat < 20000) then
    tEmployeeSalaryType.AsFloat := 4
  else if (tEmployeeSalary.AsFloat < 25000) then
    tEmployeeSalaryType.AsFloat := 3
  else if (tEmployeeSalary.AsFloat < 30000) then
    tEmployeeSalaryType.AsFloat := 2
  else if (tEmployeeSalary.AsFloat < 50000) then
    tEmployeeSalaryType.AsFloat := 1
  else
    tEmployeeSalaryType.AsFloat := 0;
end;

procedure TDataModule1.Query1UpdateRecord(DataSet: TDataSet;
  UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
begin
  //
end;

procedure TDataModule1.qCustomerUpdateRecord(DataSet: TDataSet;
  UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
begin
//
end;

procedure TDataModule1.qCustomer2UpdateRecord(DataSet: TDataSet;
  UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
begin
//
end;

end.
