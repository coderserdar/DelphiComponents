//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "DemoUnit.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BMShapedButton"
#pragma link "BMWave"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TForm1::BMShapedButton4Click(TObject *Sender)
{
  BMShapedButton4->CloudColor = clRed;
  OpenDialog1->Execute ();
  BMShapedButton4->CloudColor = clBlue;
}
//---------------------------------------------------------------------------


