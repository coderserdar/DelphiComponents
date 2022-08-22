//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "DemoUnit.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BMShapedForm"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
    : TForm(Owner)
{
  Stretched = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BitBtn1Click(TObject *Sender)
{
  Close ();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BitBtn2Click(TObject *Sender)
{
  if ( Stretched )
    {
    Visible = false;
    Width = 299;
    Stretched = false;
    Visible = true;
    }

  else
    {
    Visible = false;
    Width = 500;
    Stretched = true;
    Visible = true;
    }
}
//---------------------------------------------------------------------------

