/*********************************************************/
/* XMLPartner: ExErr.CPP 2.55                            */
/* Copyright (c) TurboPower Software Co 2002             */
/* All rights reserved.                                  */
/*********************************************************/
/* XMLPartner: XML Editor Error form                     */
/*********************************************************/
//---------------------------------------------------------------------------
#ifndef ExErrH
#define ExErrH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <inifiles.hpp>
//---------------------------------------------------------------------------
class TfrmErrors : public TForm
{
__published:	// IDE-managed Components
    TMemo *memErrors;
    TPanel *pnlBottom;
    TButton *pbOK;
    TPanel *pnlMsg;
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
    void __fastcall FormResize(TObject *Sender);
    void __fastcall FormShow(TObject *Sender);
    void __fastcall pbOKClick(TObject *Sender);
private:	// User declarations
    TIniFile* FINIFile;
    TStringList* FErrors;
      /* Calling form is responsible for freeing the object referenced
         by FErrors. */
public:		// User declarations
    __fastcall TfrmErrors(TComponent* Owner);

    __property TStringList* Errors={read=FErrors, write=FErrors, nodefault};
    __property TIniFile* INIFile={read=FINIFile, write=FINIFile, nodefault};
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmErrors *frmErrors;
//---------------------------------------------------------------------------
#endif
