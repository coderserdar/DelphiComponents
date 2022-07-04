/*********************************************************/
/* XMLPartner: ExErr.CPP 2.55                            */
/* Copyright (c) TurboPower Software Co 2002             */
/* All rights reserved.                                  */
/*********************************************************/
/* XMLPartner: XML Editor Error form                     */
/*********************************************************/
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ExErr.h"
#include "ExUtil.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmErrors *frmErrors;

const String csSection = "ErrorWindow";

//---------------------------------------------------------------------------
__fastcall TfrmErrors::TfrmErrors(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmErrors::FormClose(TObject *Sender, TCloseAction &Action)
{
  if (FINIFile != NULL)
  {
    SaveFormState(this, FINIFile, csSection);
  }

}
//---------------------------------------------------------------------------

void __fastcall TfrmErrors::FormResize(TObject *Sender)
{
  pbOK->Left = (pnlBottom->ClientWidth / 2) - (pbOK->Width / 2);
}
//---------------------------------------------------------------------------

void __fastcall TfrmErrors::FormShow(TObject *Sender)
{
  if (FINIFile != NULL)
  {
    RestoreFormState(this, FINIFile, csSection);
  }

  if (FErrors != NULL)
  {
    memErrors->Lines->Assign(FErrors);
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmErrors::pbOKClick(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------

