/* ***** BEGIN LICENSE BLOCK *****                                            */
/* Version: MPL 1.1                                                           */
/*                                                                            */
/* The contents of this file are subject to the Mozilla Public License        */
/* version 1.1 (the "License"); you may not use this file except in           */
/* compliance with the License. You may obtain a copy of the License at       */
/* http://www.mozilla.org/MPL/                                                */
/*                                                                            */
/* Software distributed under the License is distributed on an "AS IS" basis, */
/* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   */
/* for the specific language governing rights and limitations under the       */
/* License.                                                                   */
/*                                                                            */
/* The Original Code is TurboPower Visual PlanIt                              */
/*                                                                            */
/* The Initial Developer of the Original Code is TurboPower Software          */
/*                                                                            */
/* Portions created by TurboPower Software Co. are Copyright (C) 2002         */
/* TurboPower SOftware Co. All Rights Reserved.                               */
/*                                                                            */
/* Contributor(s):                                                            */
/*                                                                            */
/* ***** END LICENSE BLOCK *****                                              */

//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "EdFormat.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "CSPIN"
#pragma resource "*.dfm"
TfrmEditFormat *frmEditFormat;
//---------------------------------------------------------------------------
__fastcall TfrmEditFormat::TfrmEditFormat(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
AnsiString GetDayTypeName(TVpDayUnits du)
{
  AnsiString Rslt = "";
  switch (du) {
    case duDay:
      Rslt = "duDay";
      break;
    case duWeek:
      Rslt = "duWeek";
      break;
    case duMonth:
      Rslt = "duMonth";
      break;
    case duYear:
      Rslt = "duYear";
      break;
  }
  return Rslt;
}
//---------------------------------------------------------------------------
TVpDayUnits GetDayTypeValue(AnsiString Name)
{
  TVpDayUnits Rslt = duDay;

  if (Name == "duDay") {
    Rslt = duDay;
  }
  if (Name == "duWeek") {
    Rslt = duWeek;
  }
  if (Name == "duMonth") {
    Rslt = duMonth;
  }
  if (Name == "duYear") {
    Rslt = duYear;
  }

  return Rslt;
}
//---------------------------------------------------------------------------
void __fastcall TfrmEditFormat::btnCancelClick(TObject *Sender)
{
  ModalResult = mrCancel;
}
//---------------------------------------------------------------------------
void __fastcall TfrmEditFormat::btnOkClick(TObject *Sender)
{
  ModalResult = mrOk;
}
//---------------------------------------------------------------------------
bool __fastcall TfrmEditFormat::Execute(TVpPrintFormatItem* AFormat)
{
  SetData(AFormat);
  bool Result = ShowModal() == mrOk;
  if (Result) {
    SaveData(AFormat);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TfrmEditFormat::SaveData(TVpPrintFormatItem* AFormat)
{
  AFormat->FormatName = edName->Text;
  AFormat->Description = edDescription->Text;
  AFormat->DayInc = spedIncrement->Value;

  AnsiString Name = "du" + rgDayIncrement->Items->Strings[rgDayIncrement->ItemIndex];
  int EnumVal = GetDayTypeValue(Name);
  if (EnumVal > -1) {
    AFormat->DayIncUnits = (TVpDayUnits)EnumVal;
  }
  else {
    AFormat->DayIncUnits = duDay;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmEditFormat::SetData(TVpPrintFormatItem* AFormat)
{
  edName->Text = AFormat->FormatName;
  edDescription->Text = AFormat->Description;
  spedIncrement->Value = AFormat->DayInc;

  AnsiString IncName = GetDayTypeName(AFormat->DayIncUnits);
  if (IncName != "") {
    AnsiString Item = IncName.SubString(3, IncName.Length() - 2);
    rgDayIncrement->ItemIndex = rgDayIncrement->Items->IndexOf(Item);
  }
  else {
    rgDayIncrement->ItemIndex = 0;
  }
}
//---------------------------------------------------------------------------

