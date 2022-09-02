//---------------------------------------------------------------------------

#include <vcl\vcl.h>
#pragma hdrstop

#include "uOrdEnt.h"
#include "dMythic.h"
#include "uItemSel.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "ffdb"
#pragma resource "*.dfm"
TfrmOrderEntry *frmOrderEntry;
//---------------------------------------------------------------------------
__fastcall TfrmOrderEntry::TfrmOrderEntry(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
TDataSet* TfrmOrderEntry::GetOrderDataset()
{
  return dbeShipToContact->DataSource->DataSet;
}

TDataSet* TfrmOrderEntry::GetOrderItemsDataset()
{
  return dtmMythic->tblOrderItems;
}

void __fastcall TfrmOrderEntry::btnLogOrderClick(TObject *Sender)
{
  // Mark order as New, save changes and close the dialog
  if (OrderDataset->State != dsEdit)
    OrderDataset->Edit();
  OrderDataset->FieldByName("Status")->Value = "N";
  OrderDataset->Post();
  Close();
}
//---------------------------------------------------------------------------

void __fastcall TfrmOrderEntry::btnCancelOrderClick(TObject *Sender)
{
  dtmMythic->ffdbMythic->StartTransaction();

  // Remove detail records
  OrderItemsDataset->First();
  while (!OrderItemsDataset->Eof)
    OrderItemsDataset->Delete();

  // Remove master record
  OrderDataset->Delete();

  dtmMythic->ffdbMythic->Commit();

  Close();
}
//---------------------------------------------------------------------------

void __fastcall TfrmOrderEntry::btnAddItemClick(TObject *Sender)
{
  // Make sure the Order items dataset is in browse mode
  OrderItemsDataset->CheckBrowseMode();

  // Grab the OEM from the edit box for easy access
  String OEM = edtOEM->Text;

  // perform a filter against the product table based on item id.
  // Filters are in the following format
  //     FieldName == Value
  // FieldNames that contain spaces must be surrounded by brackets. Brackets
  // are optional for field names without spaces. When a field is a string
  // field, the value must be surrounded by quotes. The QuotedStr call from
  // SysUtils can be used to do this. We add the "*" to the end of the item
  // ID to retrieve partial matches of OEM numbers}

  tblProducts->Filter = "[OEM] = " + QuotedStr(OEM + "*");
  tblProducts->Filtered = true;

  bool CancelItem = false;
  // We only want to retrieve record count once. When filters are active, record
  // count can be a slow operation
  int RecCount = tblProducts->RecordCount;
  if (RecCount > 1) {
    // if (more) 1 records are found, display selection dialog with the result
    // set. This will place the cursor on the correct item so we can add it to
    // the order items table.
    TfrmItemSelection* form = new TfrmItemSelection(0);
    try {
      form->ProductsDataset = tblProducts;
      CancelItem = (form->ShowModal() != mrOk);
    }
    __finally {
      delete form;
    }
  }
  else if (RecCount == 0) {
    MessageDlg("No products found!", mtError, TMsgDlgButtons() << mbOK, 0);
    CancelItem = true;
  }

  if (!CancelItem) {
    // Add the item to thee orditems table and change focus to the the grid"s
    // quantity option.
    OrderItemsDataset->InsertRecord(
      ARRAYOFCONST((0, OrderDataset->FieldByName("OrderNo")->Value,
                  tblProductsID->Value, 1, tblProductsSalePrice->Value,1)));
    grdOrderItems->SetFocus();
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmOrderEntry::btnRemoveItemClick(TObject *Sender)
{
  // Make sure we have a record to delete
  if (OrderItemsDataset->RecordCount > 0)
    OrderItemsDataset->Delete();
}
//---------------------------------------------------------------------------

void __fastcall TfrmOrderEntry::FormShow(TObject *Sender)
{
  tblEmployees->Open();
  tblProducts->Open();
}
//---------------------------------------------------------------------------

