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

#include "EdElement.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "CSPIN"
#pragma resource "*.dfm"
TfrmEditElement *frmEditElement;
//---------------------------------------------------------------------------
__fastcall TfrmEditElement::TfrmEditElement(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmEditElement::FormCreate(TObject *Sender)
{
  btnShape->Enabled = false;
}
//---------------------------------------------------------------------------
void __fastcall TfrmEditElement::btnOkClick(TObject *Sender)
{
  ModalResult = mrOk;
}
//---------------------------------------------------------------------------
void __fastcall TfrmEditElement::btnCancelClick(TObject *Sender)
{
  ModalResult = mrCancel;
}
//---------------------------------------------------------------------------
bool __fastcall TfrmEditElement::Execute(TVpPrintFormatElementItem* AnElement)
{
  SetData(AnElement);
  int Result = ShowModal() == mrOk;
  if (Result) {
    SaveData(AnElement);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TfrmEditElement::rgItemTypeClick(TObject *Sender)
{
  SetItemType(rgItemType->ItemIndex);
}
//---------------------------------------------------------------------------
void __fastcall TfrmEditElement::SaveData(TVpPrintFormatElementItem* AnElement)
{
  AnElement->ElementName = edName->Text;

  AnElement->DayOffset = spedOffset->Value;

  AnElement->Top   = StrToFloat(edTop->Text);
  AnElement->Left  = StrToFloat(edLeft->Text);
  AnElement->Height= StrToFloat(edHeight->Text);
  AnElement->Width = StrToFloat(edWidth->Text);

  AnElement->ItemType       =  (TVpItemType)rgItemType->ItemIndex;
  AnElement->DayOffsetUnits =  (TVpDayUnits)rgDayOffset->ItemIndex;
  AnElement->Rotation       =  (TVpRotationAngle)rgRotation->ItemIndex;
  AnElement->Measurement    =  (TVpItemMeasurement)rgMeasurement->ItemIndex;
}
//---------------------------------------------------------------------------
void __fastcall TfrmEditElement::SetData(TVpPrintFormatElementItem* AnElement)
{
  edName->Text = AnElement->ElementName;

  spedOffset->Value = AnElement->DayOffset;

  edTop->Text = FloatToStr(AnElement->Top);
  edLeft->Text = FloatToStr(AnElement->Left);
  edHeight->Text = FloatToStr(AnElement->Height);
  edWidth->Text = FloatToStr(AnElement->Width);

  rgItemType->ItemIndex = (int)AnElement->ItemType;
  rgDayOffset->ItemIndex = (int)AnElement->DayOffsetUnits;
  rgRotation->ItemIndex = (int)AnElement->Rotation;
  rgMeasurement->ItemIndex = (int)AnElement->Measurement;
}
//---------------------------------------------------------------------------
void __fastcall TfrmEditElement::SetItemType(int Index)
{
  rgItemType->ItemIndex = Index;
  btnShape->Enabled = false;
  if (Index == 4) {
    btnShape->Enabled = true;
  }
}
//---------------------------------------------------------------------------
