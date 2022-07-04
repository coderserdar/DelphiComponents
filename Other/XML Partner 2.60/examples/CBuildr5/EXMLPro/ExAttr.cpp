/*********************************************************/
/* XMLPartner: ExAttr.CPP 2.55                           */
/* Copyright (c) TurboPower Software Co 2002             */
/* All rights reserved.                                  */
/*********************************************************/
/* XMLPartner: XML Editor Attribute Edit form            */
/*********************************************************/
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ExAttr.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TAttributeForm *AttributeForm;
//---------------------------------------------------------------------------
__fastcall TAttributeForm::TAttributeForm(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TAttributeForm::OkBtnClick(TObject *Sender)
{
  AttrNameEdit->Text = Trim(AttrNameEdit->Text);
  if (AttrNameEdit->Text == "")
  {
    MessageDlg("Attribute name cannot be blank!", mtError,
               TMsgDlgButtons() << mbOK, 0);
    ModalResult = mrNone;
  }

}
//---------------------------------------------------------------------------
