//---------------------------------------------------------------------------

#include <vcl\vcl.h>
#pragma hdrstop

#include "uCustEnt.h"
#include "dMythic.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmCustomerEntry *frmCustomerEntry;
//---------------------------------------------------------------------------
__fastcall TfrmCustomerEntry::TfrmCustomerEntry(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

TDataSet* TfrmCustomerEntry::GetDataset()
{
  return dbeCompany->DataSource->DataSet;
}

void __fastcall TfrmCustomerEntry::btnCloseClick(TObject *Sender)
{
  // make sure that all the changes have been saved
  Dataset->CheckBrowseMode();
  Close();
}
//---------------------------------------------------------------------------
