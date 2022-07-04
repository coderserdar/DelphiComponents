/*********************************************************/
/* XMLPartner: ExPrefs.CPP 2.55                          */
/* Copyright (c) TurboPower Software Co 2002             */
/* All rights reserved.                                  */
/*********************************************************/
/* XMLPartner: XML Editor Preferences form               */
/*********************************************************/
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ExPrefs.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TPrefsForm *PrefsForm;

const String csAppSection     = "Application";
const String csPrefSection    = "Preferences";
const String csFormatted      = "Formatted";
const String csNormalize      = "Normalize";
const String csSaveBackup     = "SaveBackup";
const String csSaveTextWinPos = "SaveTextWinPos";
const String csSaveAppWinPos  = "SaveAppWinPos";

//---------------------------------------------------------------------------
__fastcall TPrefsForm::TPrefsForm(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TPrefsForm::btnXMLDefaultClick(TObject *Sender)
{

  TRegistry* oReg = new TRegistry();

  try
  {
    oReg->RootKey = HKEY_CLASSES_ROOT;
    oReg->OpenKey("\.xml", true);
    oReg->WriteString("", "xmlfile");
    oReg->WriteString("Content Type", "text/plain");
    oReg->OpenKey("\xmlfile", true);
    oReg->WriteString("", "XML Document");
    oReg->OpenKey("\xmlfile\DefaultIcon", true);
    oReg->WriteString("", Application->ExeName);
    oReg->OpenKey("\xmlfile\shell\edit\command", true);
    oReg->WriteString("", Application->ExeName + " ""%1""");
    oReg->OpenKey("\xmlfile\shell\open\command", true);
    oReg->WriteString("", Application->ExeName + " ""%1""");
  }
  __finally
  {
    oReg->Free();
  }
}
//---------------------------------------------------------------------------

void __fastcall TPrefsForm::btnXSLDefaultClick(TObject *Sender)
{

  TRegistry* oReg = new TRegistry();

  try
  {
    oReg->RootKey = HKEY_CLASSES_ROOT;
    oReg->OpenKey("\.xsl", true);
    oReg->WriteString("", "xslfile");
    oReg->WriteString("Content Type", "text/plain");
    oReg->OpenKey("\xslfile", true);
    oReg->WriteString("", "XML Stylesheet");
    oReg->OpenKey("\xslfile\DefaultIcon", true);
    oReg->WriteString("", Application->ExeName);
    oReg->OpenKey("\xslfile\shell\edit\command", true);
    oReg->WriteString("", Application->ExeName + " ""%1""");
    oReg->OpenKey("\xslfile\shell\open\command", true);
    oReg->WriteString("", Application->ExeName + " ""%1""");
  }
  __finally
  {
    oReg->Free();
  }
}
//---------------------------------------------------------------------------

void __fastcall TPrefsForm::OkBtnClick(TObject *Sender)
{
  if (FINIFile != NULL)
  {
    FINIFile->WriteBool(csPrefSection, csFormatted, FormattedCheckBox->Checked);
    FINIFile->WriteBool(csPrefSection, csNormalize, NormalizeCheckBox->Checked);
    FINIFile->WriteBool(csPrefSection, csSaveBackup, BackupCheckBox->Checked);
    FINIFile->WriteBool(csPrefSection, csSaveTextWinPos, TextWinCheckBox->Checked);
    FINIFile->WriteBool(csPrefSection, csSaveAppWinPos, AppWinCheckBox->Checked);
  }
}
//---------------------------------------------------------------------------

void __fastcall TPrefsForm::FormShow(TObject *Sender)
{
  if (FINIFile != NULL)
  {
    FormattedCheckBox->Checked = FINIFile->ReadBool(csPrefSection, csFormatted, true);
    NormalizeCheckBox->Checked = FINIFile->ReadBool(csPrefSection, csNormalize, true);
    BackupCheckBox->Checked    = FINIFile->ReadBool(csPrefSection, csSaveBackup, true);
    TextWinCheckBox->Checked   = FINIFile->ReadBool(csPrefSection, csSaveTextWinPos, true);
    AppWinCheckBox->Checked    = FINIFile->ReadBool(csPrefSection, csSaveAppWinPos, true);
  }
}
//---------------------------------------------------------------------------

