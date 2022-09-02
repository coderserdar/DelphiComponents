//---------------------------------------------------------------------------

#include <vcl\vcl.h>
#pragma hdrstop

#include "uItemSel.h"
#include "dMythic.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmItemSelection *frmItemSelection;
//---------------------------------------------------------------------------
__fastcall TfrmItemSelection::TfrmItemSelection(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
TDataSet* TfrmItemSelection::GetProductsDataset()
{
  return dtsProducts->DataSet;
}

void TfrmItemSelection::SetProductsDataset(TDataSet* Value)
{
  dtsProducts->DataSet = Value;
}

