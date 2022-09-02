//---------------------------------------------------------------------------

#include <vcl\vcl.h>
#pragma hdrstop

#include "uMain.h"
#include "uEmpCfg.h"
#include "uCustEnt.h"
#include "uOrdEnt.h"
#include "dMythic.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
TDataSet* TfrmMain::GetCustDataset()
{
  return grdCustomer->DataSource->DataSet;
}

TDataSet* TfrmMain::GetOrderDataset()
{
  return grdOrders->DataSource->DataSet;
}

void __fastcall TfrmMain::mnuExitClick(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::mnuEmployeesClick(TObject *Sender)
{
  TfrmEmployees* form = new TfrmEmployees(0);
  try {
    form->ShowModal();
  }
  __finally {
    delete form;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::mnuDeleteCustomerClick(TObject *Sender)
{
  // make sure the dataset is in browse mode
  CustDataset->CheckBrowseMode();

  // confirm before deleting the record
  String S = "Delete " + CustDataset->FieldByName("Company")->AsString + "?";
  if (MessageDlg(S, mtConfirmation, TMsgDlgButtons() << mbOK << mbCancel, 0) == mrOk)
    CustDataset->Delete();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::mnuNewCustomerClick(TObject *Sender)
{
  // make sure the dataset is in browse mode
  CustDataset->CheckBrowseMode();

  // Retrieve the company name, and open the customer entry dialog
  String NameStr;

  if (InputQuery("Create new customer", "Company Name", NameStr)) {
    CustDataset->AppendRecord(OPENARRAY(TVarRec, (0, NameStr)));
    TfrmCustomerEntry* form = new TfrmCustomerEntry(0);
    try {
      form->ShowModal();
    }
    __finally {
      delete form;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::mnuCreateOrderClick(TObject *Sender)
{
  // Insert a new record, and open the order entry { screen-> We call InsertRecord
  // because we want to insert an empty record and immediately post it-> This
  // is to make sure the Order record has an autoinc key assigned-> We need
  // a proper autoinc value before we can add order items-> The order entry { screen
  // will take care of posting or cancelling the record changes as necessary
  OrderDataset->InsertRecord(ARRAYOFCONST((0)));
  TfrmOrderEntry* form = new TfrmOrderEntry(0);
  try {
    form->ShowModal();
  }
  __finally {
    delete form;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::cboOrderRangeChange(TObject *Sender)
{
  dtmMythic->CustomerOrderRange = TOrderRange(cboOrderRange->ItemIndex);
  if (dynamic_cast<TComboBox*>(Sender))
    // update the menu item to reflect the current range selection
    switch (cboOrderRange->ItemIndex) {
      case 0 : mnuOrderRangeAll->Checked = true; break;
      case 1 : mnuOrderRangeLast7->Checked = true; break;
      case 2 : mnuOrderRangeLast30->Checked = true; break;
      case 3 : mnuOrderRange1Year->Checked = true; break;
    }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  cboOrderRange->ItemIndex = 0;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::mnuOrderRange1YearClick(TObject *Sender)
{
  TMenuItem* MI = dynamic_cast<TMenuItem*>(Sender);
  if (MI) {
    MI->Checked = true;
    cboOrderRange->ItemIndex = MI->Tag;
    cboOrderRangeChange(this);
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::spdFindByCompanyClick(TObject *Sender)
{
  TffTable* table = dynamic_cast<TffTable*>(CustDataset);
  if (table)
    table->FindNearest(ARRAYOFCONST((edtCompanySearch->Text)));
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormShow(TObject *Sender)
{
  dtmMythic->Connect();
}
//---------------------------------------------------------------------------
