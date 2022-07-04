/*********************************************************/
/* XMLPartner: ExSelAtt.CPP 2.55                         */
/* Copyright (c) TurboPower Software Co 2002             */
/* All rights reserved.                                  */
/*********************************************************/
/* XMLPartner: XML Editor Attribute Selection form       */
/*********************************************************/
//---------------------------------------------------------------------------
#ifndef ExSelAttH
#define ExSelAttH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include "XpDom.hpp"
//---------------------------------------------------------------------------
class TSelAttrsForm : public TForm
{
__published:	// IDE-managed Components
    TLabel *Label1;
    TListView *AttrsListView;
    TButton *OkBtn;
    TButton *CancelBtn;
    void __fastcall FormResize(TObject *Sender);
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
    void __fastcall FormShow(TObject *Sender);
    void __fastcall OkBtnClick(TObject *Sender);
private:	// User declarations
    TXpNodeList* FList;
    TXpElement* FNode;

public:		// User declarations
    void __fastcall Run(TXpNodeList* oList, TXpElement* oNode);

    __fastcall TSelAttrsForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSelAttrsForm *SelAttrsForm;
//---------------------------------------------------------------------------
#endif
