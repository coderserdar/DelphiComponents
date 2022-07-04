//---------------------------------------------------------------------------

#include <vcl\vcl.h>
#pragma hdrstop

#include "dMythic.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "ffdb"
#pragma link "ffdbbase"
#pragma link "ffllbase"
#pragma link "ffclreng"
#pragma link "ffllcomm"
#pragma link "ffllcomp"
#pragma link "fflleng"
#pragma link "fflllgcy"
#pragma link "ffsrintm"
#pragma resource "*.dfm"
TdtmMythic *dtmMythic;
//---------------------------------------------------------------------------
__fastcall TdtmMythic::TdtmMythic(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TdtmMythic::SetCustomerOrderRange(const TOrderRange Value)
{
  // This method is called when the TfrmMain->cboOrderRangeChange event is
  //  triggered-> Since we need to update the range we call the
  //  data module's tblCustomerAfterScroll method-> Before we do this, though
  //  we check to make sure that a different range option was passed in via
  //  the Value property.
  if (FCustomerOrderRange !=Value) {
    FCustomerOrderRange = Value;
    tblCustomerAfterScroll(tblCustomer);
  }
}

void __fastcall TdtmMythic::tblCustomerOrdersCalcFields(TDataSet *DataSet)
{
  // These calculated fields are used to generate information that is
  // displayed on the OrderEntry dialog.

  tblCustomerOrdersTaxTotal->Value = (tblCustomerOrdersTaxRate->Value / 100) *
    tblCustomerOrdersItemsTotal->Value;

  tblCustomerOrdersAmountDue->Value =
    tblCustomerOrdersItemsTotal->Value +
    tblCustomerOrdersFreight->Value -
    tblCustomerOrdersAmountPaid->Value +
    tblCustomerOrdersTaxTotal->Value;
}
//---------------------------------------------------------------------------
void __fastcall TdtmMythic::tblCustomerOrdersNewRecord(TDataSet *DataSet)
{
  // Set some default values for the fields in the new record

  // Since we are emulating a master-detail relationship between customer and
  // orders, we must manually set the CustNo value for this record
  tblCustomerOrdersCustNo->Value = tblCustomerID->Value;

  tblCustomerOrdersSaleDate->Value = Date();
  tblCustomerOrdersTerms->Value = "net 30";
  tblCustomerOrdersPaymentMethod->Value = "Check";
  tblCustomerOrdersItemsTotal->Value = 0;
  tblCustomerOrdersTaxRate->Value = 0;
  tblCustomerOrdersFreight->Value = 0;
  tblCustomerOrdersAmountPaid->Value = 0;

  // Mark the record with a status of "C" (for creating). This will keep the
  //  back end from processing the order-> Once the order is complete, this status
  //  field will be set to "N" for new so the backend will know to process it.
  tblCustomerOrdersStatus->Value = "C";

  // Now we pull in some information from the customer table to act as
  // defaults for the shipping information.
  tblCustomerOrdersShipToContact->Value = tblCustomerContact->Value;
  tblCustomerOrdersShipToAddr1->Value = tblCustomerAddress1->Value;
  tblCustomerOrdersShipToAddr2->Value = tblCustomerAddress2->Value;
  tblCustomerOrdersShipToCity->Value = tblCustomerCity->Value;
  tblCustomerOrdersShipToState->Value = tblCustomerState->Value;
  tblCustomerOrdersShipToZip->Value = tblCustomerZip->Value;
  tblCustomerOrdersShipToCountry->Value = tblCustomerCountry->Value;
  tblCustomerOrdersShipToPhone->Value = tblCustomerPhone->Value;
  tblCustomerOrdersShipVIA->Value = tblCustomerDeliveryMethod->Value;
}
//---------------------------------------------------------------------------
void __fastcall TdtmMythic::tblCustomerAfterScroll(TDataSet *DataSet)
{
  // variable to store customer ID for easy access
  double ID;
  // The customer table has completed a scroll operation, and we must set a
  // range on the order table to emulate a mater-detail relationship. The
  // range is based on the ID of the customer, and the date-range of orders
  // to display based on the internal CustomerOrderRange property. The property
  //  is set by the GUI"s main form whenever the show combobox is changed.

  ID = tblCustomerID->Value;

  // Set range on Orders table
  switch(CustomerOrderRange) {
    case rtLast7 : tblCustomerOrders->SetRange(
      ARRAYOFCONST((ID, (double)(Date() - 7))),
      ARRAYOFCONST((ID, (double)Date()))); break;
      // include orders from the last 7 days that match ID
    case rtLast30 : tblCustomerOrders->SetRange(
      ARRAYOFCONST((ID, (double)(Date() - 30))),
      ARRAYOFCONST((ID, (double)Date()))); break;
      // include orders from the last 30 days that match ID
    case rt1Year : tblCustomerOrders->SetRange(
      ARRAYOFCONST((ID, (double)(Date() - 365))),
      ARRAYOFCONST((ID, (double)Date()))); break;
      // include orders for 1 year
    default :
      tblCustomerOrders->SetRange(
        ARRAYOFCONST((ID)), ARRAYOFCONST((ID)));
      // Include all orders
  }
}
//---------------------------------------------------------------------------
void __fastcall TdtmMythic::tblOrderItemsAfterPost(TDataSet *DataSet)
{
  // Update order itemstotal
  double Total;
  DataSet->DisableControls();
  try {
    TBookmark* BM = (TBookmark*)DataSet->GetBookmark();
    try {
      DataSet->First();
      Total = 0;
      while (!DataSet->Eof) {
        Total = Total + (DataSet->FieldByName("SalePrice")->AsCurrency *
                          DataSet->FieldByName("Qty")->AsInteger);
        DataSet->Next();
      }
    }
    __finally {
      DataSet->GotoBookmark(BM);
      DataSet->FreeBookmark(BM);
    }
  }
  __finally {
    DataSet->EnableControls();
  }
  if (tblCustomerOrders->State != dsEdit)
    tblCustomerOrders->Edit();
  tblCustomerOrdersItemsTotal->Value = Total;
}
//---------------------------------------------------------------------------

void TdtmMythic::Connect()
{
// Begin !!.02
  String anAlias;
  String aPath;
  TffSession* aSession;
  TffSession* aTmpSession;

  aSession = NULL;
  anAlias = "";

  /* Scan for session and database components. We need to activate the
    session in order to test for an alias. We need to obtain the required
    alias from the database component. */
  for (int i = 0; i <  ComponentCount; i++)
  {
    aTmpSession = dynamic_cast<TffSession*>(Components[i]);
    if (aTmpSession)
    {
      aSession = aTmpSession;
      aSession->Open();
    }
    TffDatabase* aDB = dynamic_cast<TffDatabase*>(Components[i]);
    if (aDB)
      anAlias = aDB->AliasName;
  }

  /* Did we find an alias? Do we have a session? */
  if ((anAlias != "") && aSession)
    /* Make sure the alias exists on the server. */
    if (!aSession->IsAlias(anAlias))
    {
      aPath = ExtractFilePath(Application->ExeName);
      if (aPath[aPath.Length()] != '\\')
        aPath = aPath + '\\';
      /* Path should point to the folder containing the Mythic tables. */
      aSession->AddAlias(anAlias, aPath + "..\\..\\..\\MythicDB", False);
    }

  /* Open the tables. */
  for (int i = 0; i < ComponentCount; i++)
  {
    TffTable* table = dynamic_cast<TffTable*>(Components[i]);
    if (table)
      table->Open();
  }
// End !!.02

}

void TdtmMythic::Disconnect()
{
  ffssMythic->Close();
}
