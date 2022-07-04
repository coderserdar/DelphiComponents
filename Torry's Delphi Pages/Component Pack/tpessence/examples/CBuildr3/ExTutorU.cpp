// ***** BEGIN LICENSE BLOCK *****
// * Version: MPL 1.1
// *
// * The contents of this file are subject to the Mozilla Public License Version
// * 1.1 (the "License"); you may not use this file except in compliance with
// * the License. You may obtain a copy of the License at
// * http://www.mozilla.org/MPL/
// *
// * Software distributed under the License is distributed on an "AS IS" basis,
// * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
// * for the specific language governing rights and limitations under the
// * License.
// *
// * The Original Code is TurboPower Essentials Vol I
// *
// * The Initial Developer of the Original Code is
// * TurboPower Software
// *
// * Portions created by the Initial Developer are Copyright (C) 1997-2002
// * the Initial Developer. All Rights Reserved.
// *
// * Contributor(s):
// *
// * ***** END LICENSE BLOCK *****

//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "ExTutorU.h"
//---------------------------------------------------------------------------
#pragma link "EsTile"
#pragma link "EsBase"
#pragma link "EsLabel"
#pragma link "EsMarque"
#pragma link "EsEdCal"
#pragma link "EsEdPop"
#pragma link "EsEdCalc"
#pragma link "EsRollUp"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  // these can be set at design-time
  BorderStyle = bsDialog;
  AutoScroll = false;

  // move "less" button into same positon as "more" button
  LessBtn->Top = MoreBtn->Top;
  LessBtn->Left = MoreBtn->Left;

  // load text to display
  ListBox1->Items->LoadFromFile("EXTUTOR.TXT");

  // set initial state
  LessBtn->Visible = false;
  MoreBtn->Visible = true;
  EsRollUp1->MinHeight = MoreBtn->Top + MoreBtn->Height + 4;
  EsRollUp1->RolledUp = true;
  ListBox1->ItemIndex = 0;
  EsScrollingMarquee1->Caption = ListBox1->Items->Strings[0];
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BtnClick(TObject *Sender)
{
  EsRollUp1->RolledUp = !EsRollUp1->RolledUp;
  if (EsRollUp1->RolledUp) {
    MoreBtn->Visible = true;
    LessBtn->Visible = false;
  }
  else {
    LessBtn->Visible = true;
    MoreBtn->Visible = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::EsRollUp1RollDown(TObject *Sender)
{
  EsScrollingMarquee1->Active = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::EsRollUp1RollUp(TObject *Sender)
{
  EsScrollingMarquee1->Active = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::EsScrollingMarquee1Cycle(TObject *Sender)
{
  // pick the next string to display
  int i = ListBox1->ItemIndex + 1;
  if (i > ListBox1->Items->Count - 1)
    i = 0;
  ListBox1->ItemIndex = i;
  EsScrollingMarquee1->Caption = ListBox1->Items->Strings[i];
}
//---------------------------------------------------------------------------
void __fastcall TForm1::EsDateEdit1Exit(TObject *Sender)
{
  Label1->Caption = FormatDateTime(LongDateFormat, EsDateEdit1->Date);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::EsNumberEdit1Exit(TObject *Sender)
{
  Label2->Caption = EsNumberEdit1->AsString;
}
//---------------------------------------------------------------------------