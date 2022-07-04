//---------------------------------------------------------------------------

#include <vcl\vcl.h>
#pragma hdrstop

#include "ExOrderU.h"
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
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TForm1::LinesTableCalcFields(TDataSet *DataSet)
{
  LinesTableTotal->AsFloat =
    LinesTablePrice->AsFloat * LinesTableCount->AsInteger;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Open1Click(TObject *Sender)
{
  ProductTable->Active = true;
  CustomerTable->Active = true;
  OrdersTable->Active = true;
  LinesTable->Active = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Close1Click(TObject *Sender)
{
  LinesTable->Active = false;
  OrdersTable->Active = false;
  CustomerTable->Active = false;
  ProductTable->Active = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Exit1Click(TObject *Sender)
{
  Close1Click(0);
  Close();
}
//---------------------------------------------------------------------------
