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

#include "ExNumU.h"
//---------------------------------------------------------------------------
#pragma link "EsEdCalc"
#pragma link "EsEdPop"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::EsNumberEdit1Exit(TObject *Sender)
{
  try {
    Label1->Caption = FloatToStr(EsNumberEdit1->AsFloat);
  }
  catch (...) {
    EsNumberEdit1->SetFocus();
    throw;
  }
}
//--- ------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  Close();  
}
//---------------------------------------------------------------------------