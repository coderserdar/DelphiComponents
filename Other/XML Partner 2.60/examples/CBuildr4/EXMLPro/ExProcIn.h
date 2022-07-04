/***********************************************************/
/* XMLPartner: ExProcIn.CPP 2.55                           */
/* Copyright (c) TurboPower Software Co 2002               */
/* All rights reserved.                                    */
/***********************************************************/
/* XMLPartner: XML Editor Processing Instruction Edit form */
/***********************************************************/
//---------------------------------------------------------------------------
#ifndef ExProcInH
#define ExProcInH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class TPIForm : public TForm
{
__published:	// IDE-managed Components
    TLabel *Label1;
    TLabel *Label2;
    TEdit *PINameEdit;
    TEdit *PIValueEdit;
    TButton *OkBtn;
    TButton *CancelBtn;
    void __fastcall OkBtnClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
    __fastcall TPIForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TPIForm *PIForm;
//---------------------------------------------------------------------------
#endif
