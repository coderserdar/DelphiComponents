//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "TFormIdleSampleUnit2.h"
//---------------------------------------------------------------------------
#pragma link "Formidle"
#pragma link "FormIdle"
#pragma resource "*.dfm"
TForm2 *Form2;
//---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm2::FormIdle1Idle(TObject *Sender)
{
  OKButton->Enabled = ( ! NameEdit->Text.IsEmpty ());	
}
//---------------------------------------------------------------------------