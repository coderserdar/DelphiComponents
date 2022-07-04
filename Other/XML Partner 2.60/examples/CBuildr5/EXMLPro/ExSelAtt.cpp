/*********************************************************/
/* XMLPartner: ExSelAtt.CPP 2.55                         */
/* Copyright (c) TurboPower Software Co 2002             */
/* All rights reserved.                                  */
/*********************************************************/
/* XMLPartner: XML Editor Attribute Selection form       */
/*********************************************************/
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ExSelAtt.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "XpDom"
#pragma resource "*.dfm"
TSelAttrsForm *SelAttrsForm;
//---------------------------------------------------------------------------
__fastcall TSelAttrsForm::TSelAttrsForm(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TSelAttrsForm::Run(TXpNodeList* oList, TXpElement* oNode)
{
  TListItem* oItem;
  TXpElement* oElem;
  int i;
  String sName;

  FList = oList;
  FNode = oNode;
  for (i = 0; i < FList->Length; i++)
  {
    oItem = AttrsListView->Items->Add();
    oElem = (TXpElement*) FList->Item(i);
    sName = oElem->GetAttribute("name");
    oItem->Caption = sName;
    oItem->Checked = (oElem->GetAttribute("checked") = "true") |
                     !(FNode->GetAttribute(sName) = "");
  }
  ShowModal();

}
//---------------------------------------------------------------------------
void __fastcall TSelAttrsForm::FormResize(TObject *Sender)
{
  AttrsListView->Columns->Items[0]->Width = AttrsListView->ClientWidth;
}
//---------------------------------------------------------------------------

void __fastcall TSelAttrsForm::FormClose(TObject *Sender,
      TCloseAction &Action)
{
  AttrsListView->Items->Clear();
}
//---------------------------------------------------------------------------

void __fastcall TSelAttrsForm::FormShow(TObject *Sender)
{
  AttrsListView->SetFocus();
}
//---------------------------------------------------------------------------

void __fastcall TSelAttrsForm::OkBtnClick(TObject *Sender)
{
  int i;
  String sName, sValue;
  TXpElement* oElem;

  for (i = 0; i < FList->Length; i++)
  {
    oElem = (TXpElement*) FList->Item(i);
    sName = oElem->GetAttribute("name");
    if (AttrsListView->Items->Item[i]->Checked)
    {
      if (FNode->GetAttribute(sName) = "")
      {
        sValue = oElem->GetAttribute("value");
        FNode->SetAttribute(sName, sValue);
      }
    }
    else
      FNode->RemoveAttribute(sName);
  }
}
//---------------------------------------------------------------------------

