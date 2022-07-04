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

#include "ExDayPlannerU1.h"
#include "ExVpAbout.h"
#include "ExVpRptSetup.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "VpBase"
#pragma link "VpBaseDS"
#pragma link "VpBDEDS"
#pragma link "VpClock"
#pragma link "VpContactGrid"
#pragma link "VpDayView"
#pragma link "VpDBDS"
#pragma link "VpDlg"
#pragma link "VpLEDLabel"
#pragma link "VpMonthView"
#pragma link "VpNavBar"
#pragma link "VpPrtPrvDlg"
#pragma link "VpResEditDlg"
#pragma link "VpTaskList"
#pragma link "VpEvntEditDlg"
#pragma link "VpTaskEditDlg"
#pragma resource "*.dfm"
TMainForm *MainForm;
//---------------------------------------------------------------------------
AnsiString GetItemTypeName(TVpItemType it)
{
  AnsiString Rslt = "";
  switch (it) {
    case itDayView:
      Rslt = "itDayView";
      break;
    case itWeekView:
      Rslt = "itWeekView";
      break;
    case itMonthView:
      Rslt = "itMonthView";
      break;
    case itCalendar:
      Rslt = "itCalendar";
      break;
    case itShape:
      Rslt = "itShape";
      break;
    case itCaption:
      Rslt = "itCaption";
      break;
    case itTasks:
      Rslt = "itTasks";
      break;
    case itContacts:
      Rslt = "itContacts";
      break;
  }
  return Rslt;
}
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormCreate(TObject *Sender)
{
  Graphics::TBitmap* Bmp = new Graphics::TBitmap;
  // Load ImageList from the resource
  try {
    Bmp->Width = 32;
    Bmp->Height = 32;
    Bmp->Handle = LoadBitmap(HInstance, "ROLODEX");
    ImageList1->AddMasked(Bmp, clOlive);
    Bmp->Handle = LoadBitmap(HInstance, "TODOLIST");
    ImageList1->AddMasked(Bmp, clOlive);
    Bmp->Handle = LoadBitmap(HInstance, "JOURNAL");
    ImageList1->AddMasked(Bmp, clOlive);
    Bmp->Handle = LoadBitmap(HInstance, "CALENDAR");
    ImageList1->AddMasked(Bmp, clOlive);
    Bmp->Handle = LoadBitmap(HInstance, "PEOPLE");
    ImageList1->AddMasked(Bmp, clOlive);
  }
  __finally {
      delete Bmp;
  }

  SetActivePage(CalendarPage);
  MakeDefaultFormats();
  HeaderImage->AutoSize = true;

}
//---------------------------------------------------------------------------
void __fastcall TMainForm::BtnAddResClick(TObject *Sender)
{
  VpResourceEditDialog1->AddNewResource();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::BtnDelResClick(TObject *Sender)
{
  if (VpBDEDataStore1->Resource) {
    AnsiString Msg = "This will permanently delete the resource and all of its Event, Contact and Task data.\nAre you sure?";
    if (MessageDlg(Msg, mtConfirmation, TMsgDlgButtons() << mbYes << mbNo, 0) == mrYes)
    {
      VpBDEDataStore1->PurgeResource(VpBDEDataStore1->Resource);
      LoadMaintResFields();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::DoPrintCurrent()
{
  if (PrintDialog1->Execute()) {
    Printer()->BeginDoc();
    VpControlLink1->Printer->CurFormat = VpControlLink1->Printer->Find(ReportData.Format);
    VpControlLink1->Printer->Print(Printer(), ReportData.StartDate, ReportData.EndDate);
    Printer()->EndDoc();
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::EditResDescrChange(TObject *Sender)
{
  BtnUndoRes->Enabled  = true;
  BtnApplyRes->Enabled = true;
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::LoadFormats()
{
  if (OpenDialog1->Execute()) {
    VpControlLink1->Printer->LoadFromFile(OpenDialog1->FileName, false);
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::LoadMaintResFields()
{
  if (!VpBDEDataStore1->Resource) {
    return;
  }
  EditResDescr->Enabled = true;
  MemoResNotes->Enabled = true;
  EditResDescr->Color = clWindow;
  MemoResNotes->Color = clWindow;
  EditResID->Text =  IntToStr(VpBDEDataStore1->ResourceID);
  EditResDescr->Text = VpBDEDataStore1->Resource->Description;
  MemoResNotes->Text = VpBDEDataStore1->Resource->Notes;
  BtnUndoRes->Enabled = false;
  BtnApplyRes->Enabled= false;
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::MakeDefaultFormats()
{

  TVpPrintFormatItem* NewFormat;
  TVpPrintFormatElementItem* NewElement;

  AnsiString Nm;

  for (TVpItemType it = itDayView; it <= itContacts; it = (TVpItemType)(it + 1)) {
    /* portrait layout */
    NewFormat = (TVpPrintFormatItem*)(VpControlLink1->Printer->PrintFormats->Add());
    Nm = GetItemTypeName(it);
    NewFormat->FormatName = Nm.SubString(3, Nm.Length() - 2) + " Format Test (portrait)";
    NewElement = (TVpPrintFormatElementItem*)(NewFormat->Elements->Add());
    NewElement->ElementName = "Element 1";
    NewElement->ItemType = it;

    /* landscape layout */
    NewFormat = (TVpPrintFormatItem*)(VpControlLink1->Printer->PrintFormats->Add());
    Nm = GetItemTypeName(it);
    NewFormat->FormatName = Nm.SubString(3, Nm.Length() - 2) + " Format Test (landscape)";
    NewElement = (TVpPrintFormatElementItem*)(NewFormat->Elements->Add());
    NewElement->ElementName = "Element 1";
    NewElement->ItemType = it;
    NewElement->Rotation = ra270;
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::mnuAddScheduleItemClick(TObject *Sender)
{
  TDateTime StartTime = LineToStartTime(VpDayView1->ActiveRow, VpDayView1->Granularity);
  TDateTime EndTime = LineToStartTime(VpDayView1->ActiveRow + 1, VpDayView1->Granularity);
  VpEventEditDialog1->AddNewEvent(StartTime, EndTime);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::mnuDeleteScheduleItemClick(TObject *Sender)
{
  if ((VpDayView1->ActiveRow > -1) && (VpDayView1->ActiveCol > -1)) {
    VpDayView1->DeleteActiveEvent(true);
  }
  else {
    ShowMessage("You must select a schedule item to delete");
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::mnuDeleteTaskClick(TObject *Sender)
{
  if (VpTaskList2->ActiveTask) {
    VpTaskList2->DeleteActiveTask(true);
  }
  else {
    ShowMessage("You must select a task to delete");
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::mnuEditScheduleItemClick(TObject *Sender)
{
  TDateTime StartTime = LineToStartTime(VpDayView1->ActiveRow, VpDayView1->Granularity);
  TDateTime EndTime = LineToStartTime(VpDayView1->ActiveRow + 1, VpDayView1->Granularity);

  if (VpDayView1->ActiveEvent) {
    VpEventEditDialog1->Execute(VpDayView1->ActiveEvent, VpDayView1->TimeFormat);
  }
  else {
    VpEventEditDialog1->AddNewEvent(StartTime, EndTime);
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::mnuEditTaskClick(TObject *Sender)
{
  if (VpTaskList2->ActiveTask) {
    VpTaskEditDialog1->Execute(VpTaskList2->ActiveTask);
  }
  else {
    VpTaskEditDialog1->AddNewTask();
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::mnuFileExitClick(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::mnuFileLoadPrintFormatsClick(TObject *Sender)
{
  LoadFormats();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::mnuFilePrintClick(TObject *Sender)
{
  DoPrintCurrent();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::mnuFilePrintPreviewClick(TObject *Sender)
{
  if (VpControlLink1->Printer->PrintFormats->Count > 0) {
    VpPrintPreviewDialog1->StartDate = VpMonthView1->Date;
    VpPrintPreviewDialog1->EndDate = VpMonthView1->Date + 30;
    if (VpPrintPreviewDialog1->Execute())
      DoPrintCurrent();
  }
  else {
    ShowMessage("No print format defined");
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::mnuFilePrintSetupClick(TObject *Sender)
{
  PrinterSetupDialog1->Execute();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::mnuFileReportSetupClick(TObject *Sender)
{
  frmReportSetup->StartDate = VpMonthView1->Date;
  frmReportSetup->EndDate = VpMonthView1->Date;
  frmReportSetup->ControlLink = VpControlLink1;
  frmReportSetup->Execute();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::mnuHelpAboutClick(TObject *Sender)
{
  // Display about box
  frmAbout->Execute();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::mnuMaintResourcesClick(TObject *Sender)
{
  SetActivePage(10);
  VpNavBar1->ActiveFolder = 1;
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::mnuNewTaskClick(TObject *Sender)
{
  VpTaskEditDialog1->AddNewTask();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::ResChange(TObject *Sender)
{
  BtnUndoRes->Enabled = false;
  BtnApplyRes->Enabled= false;

  if (Sender == BtnUndoRes) {
    LoadMaintResFields();
  }
  else {
    VpBDEDataStore1->Resource->Description = EditResDescr->Text;
    VpBDEDataStore1->Resource->Notes = MemoResNotes->Text;
    VpBDEDataStore1->PostResources();
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::SetActivePage(int Page)
{
  PanelCalendar->Visible = false;
  PanelCalendar->Align = alClient;

  PanelContacts->Visible = false;
  PanelContacts->Align = alClient;

  PanelTasks->Visible = false;
  PanelTasks->Align = alClient;

  PanelMaintResources->Visible = false;
  PanelMaintResources->Align = alClient;

  Graphics::TBitmap* Bmp = new Graphics::TBitmap;
  try {
    switch (Page) {
      case CalendarPage : {
        PanelCalendar->Visible = true;
        Bmp->Handle = LoadBitmap(HInstance, "CALENDAR");
        LabelHeader->Caption = "Calendar";
      }
      break;

      case ContactsPage : {
        PanelContacts->Visible = true;
        Bmp->Handle = LoadBitmap(HInstance, "ROLODEX");
        LabelHeader->Caption = "Contacts";
      }
      break;

      case TasksPage    : {
        PanelTasks->Visible = true;
        Bmp->Handle = LoadBitmap(HInstance, "TODOLIST");
        LabelHeader->Caption = "Task List";
      }
      break;

      case MaintResourcePage: {
        PanelMaintResources->Visible = true;
        Bmp->Handle = LoadBitmap(HInstance, "PEOPLE");
        LabelHeader->Caption = "Resource Maintenance";
        BtnAddRes->Glyph->Handle = LoadBitmap(HInstance, "VPPLUS");
        if (VpBDEDataStore1->Resource) {
          LoadMaintResFields();
        }
        else {
          EditResDescr->Enabled = false;
          EditResDescr->Color = clBtnFace;
          MemoResNotes->Enabled = false;
          MemoResNotes->Color = clBtnFace;
        }

      }

    }
    /* paint the image in the corner */
    HeaderImage->Canvas->Brush->Color = clBtnFace;
    HeaderImage->Canvas->BrushCopy(Rect(0, 0, 32, 32), Bmp, Rect(0, 0, 32, 32),
      clOlive);
  }
  __finally {
    delete Bmp;
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::TrackBar1Change(TObject *Sender)
{
  VpDayView1->NumDays = TrackBar1->Position;
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::VpBDEDataStore1ResourceChange(TObject *Sender,
      TVpResource *Resource)
{
  LoadMaintResFields();

  if ((VpNavBar1->ActiveFolder == 2)
  && (VpBDEDataStore1->Resource)) {
    LabelEventsLED->Caption = IntToStr(
      VpBDEDataStore1->Resource->Schedule->EventCount);
    LabelContactsLED->Caption = IntToStr(
      VpBDEDataStore1->Resource->Contacts->Count());
    LabelTasksLED->Caption = IntToStr(
      VpBDEDataStore1->Resource->Tasks->Count());
  } else {
    LabelEventsLED->Caption = '0';
    LabelContactsLED->Caption = '0';
    LabelTasksLED->Caption = '0';
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::VpNavBar1FolderChange(TObject *Sender,
      int Index, bool &AllowChange, bool Dragging)
{
  if (Index == 2) {
    if (VpBDEDataStore1->Resource) {
      LabelEventsLED->Caption = IntToStr(
        VpBDEDataStore1->Resource->Schedule->EventCount);
      LabelContactsLED->Caption = IntToStr(
        VpBDEDataStore1->Resource->Contacts->Count());
      LabelTasksLED->Caption = IntToStr(
        VpBDEDataStore1->Resource->Tasks->Count());
    } else {
      LabelEventsLED->Caption = '0';
      LabelContactsLED->Caption = '0';
      LabelTasksLED->Caption = '0';
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::VpNavBar1ItemClick(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int Index)
{
  if (!VpNavBar1->ActiveFolder) {
    SetActivePage(Index);
  }
  else {
    if (VpNavBar1->ActiveFolder == 1) {
      SetActivePage(Index + 10);
    }
  }
}
//---------------------------------------------------------------------------

