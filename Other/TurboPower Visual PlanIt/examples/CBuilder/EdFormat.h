//---------------------------------------------------------------------------
#ifndef EdFormatH
#define EdFormatH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "CSPIN.h"
#include <ExtCtrls.hpp>
#include "VpPrtFmt.hpp"
//---------------------------------------------------------------------------
class TfrmEditFormat : public TForm
{
__published:    // IDE-managed Components
        TLabel *Label1;
        TLabel *Label2;
        TLabel *Label3;
        TButton *btnOk;
        TButton *btnCancel;
        TRadioGroup *rgDayIncrement;
        TEdit *edDescription;
        TEdit *edName;
        TCSpinEdit *spedIncrement;
        void __fastcall btnOkClick(TObject *Sender);
        void __fastcall btnCancelClick(TObject *Sender);
private:        // User declarations
        void __fastcall SaveData(TVpPrintFormatItem* AFormat);
        void __fastcall SetData(TVpPrintFormatItem* AFormat);

public:         // User declarations
        __fastcall TfrmEditFormat(TComponent* Owner);

        bool __fastcall Execute(TVpPrintFormatItem* AFormat);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmEditFormat *frmEditFormat;
//---------------------------------------------------------------------------
#endif
