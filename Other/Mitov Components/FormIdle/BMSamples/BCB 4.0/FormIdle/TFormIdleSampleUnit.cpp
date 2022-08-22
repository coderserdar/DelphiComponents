//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "TFormIdleSampleUnit.h"
#include "TFormIdleSampleUnit2.h"
//---------------------------------------------------------------------------
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  Form2->ShowModal ();
}
//---------------------------------------------------------------------------