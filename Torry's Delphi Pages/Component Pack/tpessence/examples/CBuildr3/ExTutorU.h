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
#ifndef ExTutorUH
#define ExTutorUH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include "EsTile.hpp"
#include "EsBase.hpp"
#include "EsLabel.hpp"
#include <vcl\Buttons.hpp>
#include <vcl\ExtCtrls.hpp>
#include "EsMarque.hpp"
#include "EsEdCal.hpp"
#include "EsEdPop.hpp"
#include "EsEdCalc.hpp"
#include "EsRollUp.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
  TEsTile *EsTile1;
  TLabel *Label1;
  TLabel *Label2;
  TBitBtn *MoreBtn;
  TBitBtn *LessBtn;
  TPanel *Panel1;
  TEsScrollingMarquee *EsScrollingMarquee1;
  TListBox *ListBox1;
  TEsDateEdit *EsDateEdit1;
  TEsNumberEdit *EsNumberEdit1;
  TEsRollUp *EsRollUp1;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall BtnClick(TObject *Sender);
  void __fastcall EsRollUp1RollDown(TObject *Sender);
  void __fastcall EsRollUp1RollUp(TObject *Sender);
  void __fastcall EsScrollingMarquee1Cycle(TObject *Sender);
  void __fastcall EsDateEdit1Exit(TObject *Sender);
  void __fastcall EsNumberEdit1Exit(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
