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

#include "ExVpAbout.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmAbout *frmAbout;
//---------------------------------------------------------------------------
__fastcall TfrmAbout::TfrmAbout(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmAbout::OKButtonClick(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------

void __fastcall TfrmAbout::FormActivate(TObject *Sender)
{
  WORD year, junk;
  ProgramName->Caption = VpProductName;
  ProgramName->Caption = ProgramName->Caption + " ";
  ProgramName->Caption = ProgramName->Caption + VpVersionStr;
  DecodeDate(Now(), year, junk, junk);
  CopyrightLabel->Caption = "\169 Copyright 2000 - " + IntToStr(year)
    + ", TurboPower Software Company.";
}
//---------------------------------------------------------------------------
void __fastcall TfrmAbout::lblTurboLinkClick(TObject *Sender)
{
  if (!ShellExecute(0, "open", "http://www.turbopower.com", "", "", SW_SHOWNORMAL)) {
    ShowMessage(cBrowserError);
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmAbout::lblTurboLinkMouseMove(TObject *Sender,
      TShiftState Shift, int X, int Y)
{
  dynamic_cast<TLabel *>(Sender)->Font->Style = TFontStyles() << fsUnderline;
}
//---------------------------------------------------------------------------

void __fastcall TfrmAbout::FormMouseMove(TObject *Sender,
      TShiftState Shift, int X, int Y)
{
  lblTurboLink->Font->Style.Clear();
  lblFreeUpdateCenter->Font->Style.Clear();
  lblTurboPowerLive->Font->Style.Clear();
  lblNewsGeneral->Font->Style.Clear();
  lblNewsSpecific->Font->Style.Clear();
}
//---------------------------------------------------------------------------
void __fastcall TfrmAbout::Execute()
{
  ShowModal();
}
//---------------------------------------------------------------------------
void __fastcall TfrmAbout::lblFreeUpdateCenterClick(TObject *Sender)
{
  if (!ShellExecute(0, "open", "http://www.turbopower.com/updates", "", "", SW_SHOWNORMAL)) {
    ShowMessage(cBrowserError);
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmAbout::lblNewsGeneralClick(TObject *Sender)
{
  if (!ShellExecute(0, "open", "news://news.turbopower.com", "", "", SW_SHOWNORMAL)) {
    ShowMessage(cBrowserError);
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmAbout::lblNewsSpecificClick(TObject *Sender)
{
  if (!ShellExecute(0, "open",
    "news://news.turbopower.com/turbopower.public.support.visualplanit",
    "", "", SW_SHOWNORMAL)) {
      ShowMessage(cBrowserError);
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmAbout::lblTurboPowerLiveClick(TObject *Sender)
{
  if (!ShellExecute(0, "open", "http://www.turbopower.com/tpslive", "", "", SW_SHOWNORMAL)) {
    ShowMessage(cBrowserError);
  }
}
//---------------------------------------------------------------------------

