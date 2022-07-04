//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "CB4Ubde.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "QBEBDE"
#pragma link "QBuilder"
#pragma resource "*.dfm"
TQBDemoForm *QBDemoForm;
//---------------------------------------------------------------------------
__fastcall TQBDemoForm::TQBDemoForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TQBDemoForm::BtnQBuilderClick(TObject *Sender)
{
  if (OQBuilderDialog->Execute()) {
    Memo->Lines->Assign(OQBuilderDialog->SQL);
  }

}
//---------------------------------------------------------------------------
