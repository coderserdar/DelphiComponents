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

#include "ExFormatEdU1.h"
#include "EdFormat.h"
#include "EdElement.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "VpBase"
#pragma link "VpBaseDS"
#pragma link "VpPrtPrv"
#pragma resource "*.dfm"
TfrmPrnFormat *frmPrnFormat;
//---------------------------------------------------------------------------
__fastcall TfrmPrnFormat::TfrmPrnFormat(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::FormCreate(TObject *Sender)
{
  OpenDialog1->InitialDir = ExtractFilePath(Application->ExeName);
  SaveDialog1->InitialDir = ExtractFilePath(Application->ExeName);
  IsDirty = false;
  DoNewFile();
  UpdateCaption();
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::btnCloseClick(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::btnDeleteElementClick(TObject *Sender)
{
  TVpPrintFormatItem* Format = (TVpPrintFormatItem*)(lbFormats->Items->Objects[lbFormats->ItemIndex]);
  AnsiString Item = "";
  if (lbElements->ItemIndex > -1)
    Item = lbElements->Items->Strings[lbElements->ItemIndex];

  if (Item != "") {
    for (int Idx = (Format->Elements->Count - 1); Idx >= 0; Idx--) {
      if (Format->Elements->Items[Idx]->ElementName == Item) {
        Format->Elements->Items[Idx]->Free();
        lbElements->Items->Delete(lbElements->ItemIndex);
        IsDirty = true;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::btnDeleteFormatClick(TObject *Sender)
{
  TVpPrinter* Prn = LocalControlLink->Printer;
  int Idx = Prn->Find(lbFormats->Items->Strings[lbFormats->ItemIndex]);
  Prn->PrintFormats->Items[Idx]->Free();
  lbFormats->Items->Delete(lbFormats->ItemIndex);
  IsDirty = true;
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::btnEditElementClick(TObject *Sender)
{
  DoEditElement();
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::btnEditFormatClick(TObject *Sender)
{
  DoEditFormat();
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::btnLoadFileClick(TObject *Sender)
{
  if (IsDirty) {
    int Rslt = DirtyPrompt();

    switch (Rslt) {
      case ID_YES: {
        DoSave();
        break;
      }

      case ID_NO: {
        // nothing
        break;
      }

      case ID_CANCEL: {
        return;
      }
    }
  }

  if (OpenDialog1->Execute()) {
    FileName = OpenDialog1->FileName;
    lbFormats->Items->Clear();
    TVpPrinter* Prn = LocalControlLink->Printer;
    Prn->LoadFromFile(FileName, false);
    for (int i = 0; i < Prn->PrintFormats->Count; i++) {
      lbFormats->Items->AddObject(Prn->PrintFormats->Items[i]->FormatName, Prn->PrintFormats->Items[i]);
    }
    UpdateCaption();
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::btnNewElementClick(TObject *Sender)
{
  DoNewElement();
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::btnNewFileClick(TObject *Sender)
{
  int Rslt;
  if (IsDirty) {
    Rslt = DirtyPrompt();

    switch (Rslt) {
      case ID_YES: {
        DoSave();
        DoNewFile();
        break;
      }

      case ID_NO: {
        DoNewFile();
        break;
      }

      case ID_CANCEL:
      return;
    }
  }
  else
    DoNewFile();
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::btnNewFormatClick(TObject *Sender)
{
  DoNewFormat();
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::btnSaveFileClick(TObject *Sender)
{
  DoSave();
}
//---------------------------------------------------------------------------
int __fastcall TfrmPrnFormat::DirtyPrompt()
{
  AnsiString Msg = "Save changes to " + FileName + '?';
  int Rslt = Application->MessageBox(Msg.c_str(),
      "Inquiry",
      MB_ICONQUESTION | MB_YESNOCANCEL);
  return Rslt;
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::DoEditElement()
{
  if (lbElements->ItemIndex > -1) {
    TVpPrintFormatElementItem* E = dynamic_cast<TVpPrintFormatElementItem*>(lbElements->Items->Objects[lbElements->ItemIndex]);
    if (frmEditElement->Execute(E)) {
      IsDirty = true;
    }
  }
  else {
    DoNewElement();
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::DoEditFormat()
{
  if (lbFormats->ItemIndex > -1) {
    TVpPrintFormatItem* AFormat = dynamic_cast<TVpPrintFormatItem*>(lbFormats->Items->Objects[lbFormats->ItemIndex]);
    if (frmEditFormat->Execute(AFormat)) {
      IsDirty = true;
    }
  }
  else {
    DoNewFormat();
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::DoNewElement()
{
  TVpPrintFormatItem* Format = dynamic_cast<TVpPrintFormatItem*>(lbFormats->Items->Objects[lbFormats->ItemIndex]);
  TVpPrintFormatElementItem* E = new TVpPrintFormatElementItem(Format->Elements);
  if (frmEditElement->Execute(E)) {
    lbElements->Items->AddObject(E->ElementName, E);
    IsDirty = true;
  }
  else {
    Format->Elements->Items[E->Index]->Free();
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::DoNewFile()
{
  TVpPrinter* Prn = LocalControlLink->Printer;
  Prn->PrintFormats->Clear();
  lbFormats->Clear();
  lbElements->Clear();
  FileName = UnnamedFile;
  IsDirty = false;
  PrintPreview->ControlLink = NULL;
  EnableFormatButtons(false);
  btnNewFormat->Enabled = true;
  EnableElementButtons(false);
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::DoNewFormat()
{
  TVpPrinter* Prn = LocalControlLink->Printer;
  TVpPrintFormatItem* AFormat = new TVpPrintFormatItem(Prn->PrintFormats);
  if (frmEditFormat->Execute(AFormat)) {
    lbFormats->Items->AddObject(AFormat->FormatName, AFormat);
    IsDirty = true;
  }
  else {
    Prn->PrintFormats->Items[AFormat->Index]->Free();
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::DoSave()
{
  if (FileName != UnnamedFile) {
    SaveDialog1->FileName = FileName;
  }
  else {
    SaveDialog1->FileName = "Unnamed.xml";
  }

  if (SaveDialog1->Execute()) {
    FileName = SaveDialog1->FileName;
    LocalControlLink->Printer->SaveToFile(FileName);
    IsDirty = false;
    UpdateCaption();
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::EnableElementButtons(bool Enable)
{
  btnNewElement->Enabled = Enable;
  btnEditElement->Enabled = Enable;
  btnDeleteElement->Enabled = Enable;
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::EnableFormatButtons(bool Enable)
{
  btnNewFormat->Enabled = Enable;
  btnEditFormat->Enabled = Enable;
  btnDeleteFormat->Enabled = Enable;
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::FormCloseQuery(TObject *Sender,
      bool &CanClose)
{
  int Rslt;

  if (IsDirty) {
    Rslt = DirtyPrompt();

    switch (Rslt) {
      case ID_YES: {
        DoSave();
        CanClose = true;
        break;
      }

      case ID_NO: {
        CanClose = true;
        break;
      }

      case ID_CANCEL: {
        CanClose = false;
        return;
      }
    }
  }
  else {
    CanClose = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::Execute()
{
  ShowModal();
}
//---------------------------------------------------------------------------
TVpControlLink* __fastcall TfrmPrnFormat::GetControlLink()
{
  if (!FLocalControlLink) {
    FLocalControlLink = VpControlLink;
  }
  return FLocalControlLink;
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::lbElementsClick(TObject *Sender)
{
  lbElements->Items->Clear();
  TVpPrinter* Prn = LocalControlLink->Printer;
  int Idx = Prn->Find(lbFormats->Items->Strings[lbFormats->ItemIndex]);

  Prn->CurFormat = Idx;

  PrintPreview->ControlLink = LocalControlLink;

  TVpPrintFormatElementItem* E;
  for (int i = 0; i < Prn->PrintFormats->Items[Idx]->Elements->Count; i++) {
    E = Prn->PrintFormats->Items[Idx]->Elements->Items[i];
    lbElements->Items->AddObject(E->ElementName, E);
  }
  EnableElementButtons(false);
  btnNewElement->Enabled = true;
  EnableFormatButtons(true);
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::lbFormatsClick(TObject *Sender)
{
  EnableElementButtons(true);
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::SetControlLink(TVpControlLink* Value)
{
  if (LocalControlLink != Value) {
    if (!Value) {
      FLocalControlLink = VpControlLink;
    }
    else {
      FLocalControlLink = Value;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmPrnFormat::UpdateCaption()
{
  Caption = Format(FileCaption, ARRAYOFCONST(((AnsiString)FileName)));
}
//---------------------------------------------------------------------------











