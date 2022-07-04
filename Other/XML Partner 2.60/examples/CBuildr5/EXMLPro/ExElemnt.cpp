/*********************************************************/
/* XMLPartner: ExElemnt.CPP 2.55                         */
/* Copyright (c) TurboPower Software Co 2002             */
/* All rights reserved.                                  */
/*********************************************************/
/* XMLPartner: XML Editor Element Edit form              */
/*********************************************************/
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ExElemnt.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TElementForm *ElementForm;
//---------------------------------------------------------------------------
__fastcall TElementForm::TElementForm(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TElementForm::FormShow(TObject *Sender)
{
  NameEdit->SetFocus();
  NameEdit->SelectAll();
}
//---------------------------------------------------------------------------

void __fastcall TElementForm::OkBtnClick(TObject *Sender)
{
  NameEdit->Text = Trim(NameEdit->Text);
  if (NameEdit->Text == "")
  {
    MessageDlg("Element name cannot be blank!", mtError,
               TMsgDlgButtons() << mbOK, 0);
    ModalResult = mrNone;
  }
}
//---------------------------------------------------------------------------

