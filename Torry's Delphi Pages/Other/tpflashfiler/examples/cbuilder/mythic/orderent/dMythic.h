//---------------------------------------------------------------------------

#ifndef dMythicH
#define dMythicH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "ffdb.hpp"
#include "ffdbbase.hpp"
#include "ffllbase.hpp"
#include <Db.hpp>
#include "ffclreng.hpp"
#include "ffllcomm.hpp"
#include "ffllcomp.hpp"
#include "fflleng.hpp"
#include "fflllgcy.hpp"
#include "ffsrintm.hpp"
//---------------------------------------------------------------------------

enum TOrderRange {rtAll, rtLast7, rtLast30, rt1Year};

class TdtmMythic : public TDataModule
{
__published:	// IDE-managed Components
  TffSession *ffssMythic;
  TffDatabase *ffdbMythic;
  TDataSource *dtsCustomerOrders;
  TffTable *tblCustomerOrders;
  TAutoIncField *tblCustomerOrdersOrderNo;
  TIntegerField *tblCustomerOrdersCustNo;
  TDateTimeField *tblCustomerOrdersSaleDate;
  TDateTimeField *tblCustomerOrdersShipDate;
  TIntegerField *tblCustomerOrdersEmpNo;
  TStringField *tblCustomerOrdersShipToContact;
  TStringField *tblCustomerOrdersShipToAddr1;
  TStringField *tblCustomerOrdersShipToAddr2;
  TStringField *tblCustomerOrdersShipToCity;
  TStringField *tblCustomerOrdersShipToState;
  TStringField *tblCustomerOrdersShipToZip;
  TStringField *tblCustomerOrdersShipToCountry;
  TStringField *tblCustomerOrdersShipToPhone;
  TStringField *tblCustomerOrdersShipVIA;
  TStringField *tblCustomerOrdersPO;
  TStringField *tblCustomerOrdersTerms;
  TStringField *tblCustomerOrdersPaymentMethod;
  TCurrencyField *tblCustomerOrdersItemsTotal;
  TFloatField *tblCustomerOrdersTaxRate;
  TCurrencyField *tblCustomerOrdersFreight;
  TCurrencyField *tblCustomerOrdersAmountPaid;
  TCurrencyField *tblCustomerOrdersTaxTotal;
  TCurrencyField *tblCustomerOrdersAmountDue;
  TStringField *tblCustomerOrdersStatus;
  TStringField *tblCustomerOrdersCCNumber;
  TSmallintField *tblCustomerOrdersCCExpMonth;
  TSmallintField *tblCustomerOrdersCCExpYear;
  TffTable *tblCustomer;
  TDataSource *dtsCustomer;
  TffTable *tblOrderItems;
  TAutoIncField *tblOrderItemsID;
  TIntegerField *tblOrderItemsOrderNo;
  TIntegerField *tblOrderItemsPartNo;
  TIntegerField *tblOrderItemsQty;
  TCurrencyField *tblOrderItemsSalePrice;
  TStringField *tblOrderItemsOEM;
  TStringField *tblOrderItemsName;
  TDataSource *dtsOrderItems;
  TffTable *tblOrderItemsProductLookup;
  TAutoIncField *tblOrderItemsProductLookupID;
  TStringField *tblOrderItemsProductLookupName;
  TStringField *tblOrderItemsProductLookupUOM;
  TStringField *tblOrderItemsProductLookupOEM;
  TCurrencyField *tblOrderItemsProductLookupSalePrice;
    TffClient *ffClient;
    TFFRemoteServerEngine *ffRemoteServerEngine;
    TffLegacyTransport *ffTransport;
    TAutoIncField *tblCustomerID;
    TStringField *tblCustomerCompany;
    TStringField *tblCustomerAddress1;
    TStringField *tblCustomerAddress2;
    TStringField *tblCustomerCity;
    TStringField *tblCustomerState;
    TStringField *tblCustomerZip;
    TStringField *tblCustomerCountry;
    TStringField *tblCustomerPhone;
    TStringField *tblCustomerFAX;
    TFloatField *tblCustomerTaxRate;
    TStringField *tblCustomerContact;
    TDateTimeField *tblCustomerLastInvoiceDate;
    TStringField *tblCustomerDeliveryMethod;
  void __fastcall tblCustomerOrdersCalcFields(TDataSet *DataSet);
  void __fastcall tblCustomerOrdersNewRecord(TDataSet *DataSet);
  void __fastcall tblCustomerAfterScroll(TDataSet *DataSet);
  void __fastcall tblOrderItemsAfterPost(TDataSet *DataSet);
private:	// User declarations
  TOrderRange FCustomerOrderRange;
  void __fastcall SetCustomerOrderRange(const TOrderRange Value);
public:		// User declarations
  __fastcall TdtmMythic(TComponent* Owner);
  void Connect();
  void Disconnect();
  __property TOrderRange CustomerOrderRange =
    {read = FCustomerOrderRange, write = SetCustomerOrderRange};
};
//---------------------------------------------------------------------------
extern PACKAGE TdtmMythic *dtmMythic;
//---------------------------------------------------------------------------
#endif
