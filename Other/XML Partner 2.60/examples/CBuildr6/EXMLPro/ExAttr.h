/*********************************************************/
/* XMLPartner: ExAttr.CPP 2.55                           */
/* Copyright (c) TurboPower Software Co 2002             */
/* All rights reserved.                                  */
/*********************************************************/
/* XMLPartner: XML Editor Attribute Edit form            */
/*********************************************************/
//---------------------------------------------------------------------------
#ifndef ExAttrH
#define ExAttrH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class TAttributeForm : public TForm
{
__published:	// IDE-managed Components
    TLabel *Label1;
    TLabel *Label2;
    TButton *OkBtn;
    TButton *CancelBtn;
    TEdit *AttrNameEdit;
    TEdit *AttrValueEdit;
    void __fastcall OkBtnClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
    __fastcall TAttributeForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TAttributeForm *AttributeForm;
//---------------------------------------------------------------------------
#endif
