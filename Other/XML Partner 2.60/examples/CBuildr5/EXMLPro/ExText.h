/*********************************************************/
/* XMLPartner: ExText.CPP 2.55                           */
/* Copyright (c) TurboPower Software Co 2002             */
/* All rights reserved.                                  */
/*********************************************************/
/* XMLPartner: XML Editor Text Edit form                 */
/*********************************************************/
//---------------------------------------------------------------------------
#ifndef ExTextH
#define ExTextH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <inifiles.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TTextForm : public TForm
{
__published:	// IDE-managed Components
    TPanel *Panel1;
    TButton *OkBtn;
    TButton *CancelBtn;
    TLabel *Label1;
    TMemo *TextEdit;
    void __fastcall FormShow(TObject *Sender);
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
private:	// User declarations
    TIniFile* FINIFile;

public:		// User declarations
    __fastcall TTextForm(TComponent* Owner);

    __property TIniFile* INIFile={read=FINIFile, write=FINIFile, nodefault};
};
//---------------------------------------------------------------------------
extern PACKAGE TTextForm *TextForm;
//---------------------------------------------------------------------------
#endif
