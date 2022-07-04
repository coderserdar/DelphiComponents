//---------------------------------------------------------------------------

#include <vcl\vcl.h>
#pragma hdrstop

#include "ExBlobU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "ffdb"
#pragma link "ffdbbase"
#pragma link "ffllbase"
#pragma link "FFDB"
#pragma link "FFDBBase"
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
  BlobTable->Active = true;
  Close1->Enabled = true;
  Navigate1->Enabled = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Close1Click(TObject *Sender)
{
  BlobTable->Active = false;
  Close1->Enabled = false;
  Navigate1->Enabled = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Exit1Click(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::First1Click(TObject *Sender)
{
  BlobTable->First();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Last1Click(TObject *Sender)
{
  BlobTable->Last();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Next1Click(TObject *Sender)
{
  BlobTable->Next();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Prior1Click(TObject *Sender)
{
  BlobTable->Prior();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::All1Click(TObject *Sender)
{
  BlobTable->Filtered = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Bmp1Click(TObject *Sender)
{
  Bmp1->Checked = true;
  BlobTable->Filter = "Type = 'Bmp'";
  BlobTable->Filtered = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Jpeg1Click(TObject *Sender)
{
  Jpeg1->Checked = true;
  BlobTable->Filter = "Type = 'Jpeg'";
  BlobTable->Filtered = true;
}
//---------------------------------------------------------------------------
