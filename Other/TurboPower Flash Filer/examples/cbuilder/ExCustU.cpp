//---------------------------------------------------------------------------

#include <vcl\vcl.h>
#pragma hdrstop

#include "ExCustU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "ffdb"
#pragma link "ffdbbase"
#pragma link "ffllbase"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Open1Click(TObject *Sender)
{
  CustomerTable->Active = true;
  Close1->Enabled = true;
  Navigate1->Enabled = true;
  Edit1->Enabled = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Close1Click(TObject *Sender)
{
  CustomerTable->Active = false;
  Close1->Enabled = false;
  Navigate1->Enabled = false;
  Edit1->Enabled = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Exit1Click(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::First1Click(TObject *Sender)
{
  CustomerTable->First();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Last1Click(TObject *Sender)
{
  CustomerTable->Last();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Next1Click(TObject *Sender)
{
  CustomerTable->Next();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Prior1Click(TObject *Sender)
{
  CustomerTable->Prior();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Append1Click(TObject *Sender)
{
  CustomerTable->Append();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Insert1Click(TObject *Sender)
{
  CustomerTable->Insert();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Post1Click(TObject *Sender)
{
  CustomerTable->Post();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Refresh1Click(TObject *Sender)
{
  CustomerTable->Refresh();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Cancel1Click(TObject *Sender)
{
  CustomerTable->Cancel();
}
//---------------------------------------------------------------------------
