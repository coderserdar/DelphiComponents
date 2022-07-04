/*********************************************************/
/* XMLPartner: ExCommnt.CPP 2.55                         */
/* Copyright (c) TurboPower Software Co 2002             */
/* All rights reserved.                                  */
/*********************************************************/
/* XMLPartner: XML Editor Comment Edit form              */
/*********************************************************/
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ExCommnt.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TCommentForm *CommentForm;
//---------------------------------------------------------------------------
__fastcall TCommentForm::TCommentForm(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TCommentForm::FormShow(TObject *Sender)
{
  CommentEdit->SetFocus();
}
//---------------------------------------------------------------------------

