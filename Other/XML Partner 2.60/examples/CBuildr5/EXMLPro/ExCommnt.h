/*********************************************************/
/* XMLPartner: ExCommnt.CPP 2.55                         */
/* Copyright (c) TurboPower Software Co 2002             */
/* All rights reserved.                                  */
/*********************************************************/
/* XMLPartner: XML Editor Comment Edit form              */
/*********************************************************/
//---------------------------------------------------------------------------
#ifndef ExCommntH
#define ExCommntH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class TCommentForm : public TForm
{
__published:	// IDE-managed Components
    TLabel *Label1;
    TEdit *CommentEdit;
    TButton *OkBtn;
    TButton *CancelBtn;
    void __fastcall FormShow(TObject *Sender);
private:	// User declarations
public:		// User declarations
    __fastcall TCommentForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TCommentForm *CommentForm;
//---------------------------------------------------------------------------
#endif
