//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Main.h"
#include "csv.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ButtonOpenFileClick(TObject *Sender)
{
  if (OpenDialog->Execute())
  {
	Memo->Clear();
	TCsv *csv = new TCsv(',');
	try
	{
	  csv->LoadUtf8File(OpenDialog->FileName);
	  Memo->Text = csv->ToString();
	}
	__finally
	{
	  csv->Free();
	}
  }
}
//---------------------------------------------------------------------------

