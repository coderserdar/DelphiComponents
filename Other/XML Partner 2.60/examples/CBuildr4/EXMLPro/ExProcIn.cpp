/***********************************************************/
/* XMLPartner: ExProcIn.CPP 2.55                           */
/* Copyright (c) TurboPower Software Co 2002               */
/* All rights reserved.                                    */
/***********************************************************/
/* XMLPartner: XML Editor Processing Instruction Edit form */
/***********************************************************/
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ExProcIn.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TPIForm *PIForm;
//---------------------------------------------------------------------------
__fastcall TPIForm::TPIForm(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TPIForm::OkBtnClick(TObject *Sender)
{
  PINameEdit->Text = Trim(PINameEdit->Text);
  if (PINameEdit->Text == "")
  {
    MessageDlg("Processing instruction name cannot be blank!",
               mtError, TMsgDlgButtons() << mbOK, 0);
    ModalResult = mrNone;
  }

}
//---------------------------------------------------------------------------

