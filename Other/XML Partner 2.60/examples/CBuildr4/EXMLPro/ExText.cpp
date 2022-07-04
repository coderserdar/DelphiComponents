/*********************************************************/
/* XMLPartner: ExText.CPP 2.55                           */
/* Copyright (c) TurboPower Software Co 2002             */
/* All rights reserved.                                  */
/*********************************************************/
/* XMLPartner: XML Editor Text Edit form                 */
/*********************************************************/
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ExText.h"
#include "ExUtil.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TTextForm *TextForm;

const String csSection = "TextEditor";

//---------------------------------------------------------------------------
__fastcall TTextForm::TTextForm(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TTextForm::FormClose(TObject *Sender, TCloseAction &Action)
{
  if (FINIFile != NULL)
    SaveFormState(this, FINIFile, csSection);
}
//---------------------------------------------------------------------------
void __fastcall TTextForm::FormShow(TObject *Sender)
{
  Width = 400;
  Height = 300;
  Left = (GetSystemMetrics(SM_CXSCREEN) - Width) >> 1;
  Top = (GetSystemMetrics(SM_CYSCREEN) - Height) >> 1;
  if (FINIFile != NULL)
    if (FINIFile->ReadBool(csSection, "SaveTextWinPos", False))
    {
      RestoreFormState(this, FINIFile, csSection);
      if (Width < 150)
        Width = 150;
      if (Height < 150)
        Height = 150;
    }
  TextEdit->SetFocus();
}
//---------------------------------------------------------------------------

