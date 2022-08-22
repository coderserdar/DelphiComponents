//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "BMPageDemoUnit.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BMPAGE"
#pragma link "BMWave"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BitBtn1Click(TObject *Sender)
{
  if ( BMPageControl1->ActivePage )
    {
    BMPageControl1->ActivePage->PageControl = BMPageControl2;
    BMPageControl1->ActivePage = BMPageControl1->Pages [ 0 ];
    }

  BitBtn2->Enabled = ( BMPageControl2->PageCount > 1 );
  BitBtn1->Enabled = ( BMPageControl1->PageCount > 1 );
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BitBtn2Click(TObject *Sender)
{
  if ( BMPageControl2->ActivePage )
    {
    BMPageControl2->ActivePage->PageControl = BMPageControl1;
    BMPageControl2->ActivePage = BMPageControl2->Pages [ 0 ];
    }

  BitBtn2->Enabled = ( BMPageControl2->PageCount > 1 );
  BitBtn1->Enabled = ( BMPageControl1->PageCount > 1 );
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SoundsCheckBoxClick(TObject *Sender)
{
  BMPageControl1->EnableSounds = SoundsCheckBox->Checked;
  BMPageControl2->EnableSounds = SoundsCheckBox->Checked;
}
//---------------------------------------------------------------------------

