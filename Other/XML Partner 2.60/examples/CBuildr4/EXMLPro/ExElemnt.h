/*********************************************************/
/* XMLPartner: ExElemnt.CPP 2.55                         */
/* Copyright (c) TurboPower Software Co 2002             */
/* All rights reserved.                                  */
/*********************************************************/
/* XMLPartner: XML Editor Element Edit form              */
/*********************************************************/
//---------------------------------------------------------------------------
#ifndef ExElemntH
#define ExElemntH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class TElementForm : public TForm
{
__published:	// IDE-managed Components
    TLabel *Label1;
    TEdit *NameEdit;
    TButton *OkBtn;
    TButton *CancelBtn;
    void __fastcall FormShow(TObject *Sender);
    void __fastcall OkBtnClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
    __fastcall TElementForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TElementForm *ElementForm;
//---------------------------------------------------------------------------
#endif
