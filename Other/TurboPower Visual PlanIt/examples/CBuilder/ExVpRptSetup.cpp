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

#include "ExVpRptSetup.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "VpDateEdit"
#pragma link "VpEdPop"
#pragma link "VpPrtFmtCBox"
#pragma resource "*.dfm"
TfrmReportSetup *frmReportSetup;
//---------------------------------------------------------------------------
__fastcall TfrmReportSetup::TfrmReportSetup(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
bool __fastcall TfrmReportSetup::Execute()
{
  return ShowModal() == mrOk;
}
//---------------------------------------------------------------------------
void __fastcall TfrmReportSetup::Button2Click(TObject *Sender)
{
  ModalResult = mrCancel;
}
//---------------------------------------------------------------------------
void __fastcall TfrmReportSetup::Button1Click(TObject *Sender)
{
  SaveData();
  ModalResult = mrOk;
}
//---------------------------------------------------------------------------
void __fastcall TfrmReportSetup::SaveData()
{
  ReportData.StartDate = VpDateEdit1->Date;
  ReportData.EndDate = VpDateEdit2->Date;
  ReportData.Format = VpPrintFormatComboBox1->Text;
}
//---------------------------------------------------------------------------
TVpControlLink* __fastcall TfrmReportSetup::GetControlLink()
{
  return VpPrintFormatComboBox1->ControlLink;
}
//---------------------------------------------------------------------------
void __fastcall TfrmReportSetup::SetControlLink(TVpControlLink* Value)
{
  VpPrintFormatComboBox1->ControlLink = Value;
}
//---------------------------------------------------------------------------
TDateTime __fastcall TfrmReportSetup::GetDate(int Index)
{
  TDateTime Result = 0.0;
  switch (Index) {
    case 1: Result = VpDateEdit1->Date;
    break;
    case 2: Result = VpDateEdit2->Date;
    break;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TfrmReportSetup::SetDate(int Index, TDateTime Value)
{
  switch (Index) {
    case  1: VpDateEdit1->Date = Value;
    case  2: VpDateEdit2->Date = Value;
  }
}
//---------------------------------------------------------------------------


