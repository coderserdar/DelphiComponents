//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MainUnit.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "FBCustomDataSet"
#pragma link "jvuib"
#pragma link "mydbUnit"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
        : TForm(Owner)
{
   JvUIBDataBase->Connected = true;
   PageControl1Change(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::PageControl2Change(TObject *Sender)
{
   quSprCountry->Active =(PageControl2->ActivePage == tabSprCountry) && (PageControl1->ActivePage == tabDirectories);
   quSprCustomer->Active=(PageControl2->ActivePage == tabCustomer)   && (PageControl1->ActivePage == tabDirectories);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::PageControl1Change(TObject *Sender)
{
   PageControl2Change(NULL);
}
//---------------------------------------------------------------------------
