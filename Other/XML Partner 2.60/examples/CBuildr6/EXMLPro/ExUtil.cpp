/*********************************************************/
/* XMLPartner: ExUtil.CPP 2.55                           */
/* Copyright (c) TurboPower Software Co 2002             */
/* All rights reserved.                                  */
/*********************************************************/
/* XMLPartner: XML Editor Utility routines               */
/*********************************************************/
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ExUtil.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)

const String csLeft   = "Left";
const String csTop    = "Top";
const String csWidth  = "Width";
const String csHeight = "Height";
const String csState  = "State";

void __fastcall RestoreFormState(TForm* aForm, TIniFile* anINIFile,
                           const String aSection)
{
  aForm->Left  = anINIFile->ReadInteger(aSection, csLeft, aForm->Left);
  aForm->Top   = anINIFile->ReadInteger(aSection, csTop, aForm->Top);
  aForm->Width = anINIFile->ReadInteger(aSection, csWidth, aForm->Width);
  aForm->Height = anINIFile->ReadInteger(aSection, csHeight, aForm->Height);
  aForm->WindowState = TWindowState(anINIFile->ReadInteger
                                                 (aSection, csState,
                                                  int(aForm->WindowState)));
}

void __fastcall SaveFormState(TForm* aForm, TIniFile* anINIFile,
                           const String aSection)
{
  anINIFile->WriteInteger(aSection, csState, int(aForm->WindowState));
  if (aForm->WindowState != wsMaximized)
  {
    anINIFile->WriteInteger(aSection, csLeft, aForm->Left);
    anINIFile->WriteInteger(aSection, csTop, aForm->Top);
    anINIFile->WriteInteger(aSection, csWidth, aForm->Width);
    anINIFile->WriteInteger(aSection, csHeight, aForm->Height);
  }
}
