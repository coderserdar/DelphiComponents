//---------------------------------------------------------------------------
#ifndef EdElementH
#define EdElementH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "CSPIN.h"
#include <ExtCtrls.hpp>
#include "VpPrtFmt.hpp"
//---------------------------------------------------------------------------
class TfrmEditElement : public TForm
{
__published:    // IDE-managed Components
        TLabel *Label1;
        TLabel *Label2;
        TLabel *Label3;
        TLabel *Label4;
        TLabel *Label5;
        TLabel *Label6;
        TRadioGroup *rgItemType;
        TRadioGroup *rgDayOffset;
        TButton *btnOk;
        TButton *btnCancel;
        TEdit *edName;
        TRadioGroup *rgMeasurement;
        TRadioGroup *rgRotation;
        TButton *btnShape;
        TEdit *edTop;
        TEdit *edLeft;
        TEdit *edHeight;
        TEdit *edWidth;
        TCSpinEdit *spedOffset;
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall btnOkClick(TObject *Sender);
        void __fastcall btnCancelClick(TObject *Sender);
        void __fastcall rgItemTypeClick(TObject *Sender);
private:        // User declarations

        void __fastcall SaveData(TVpPrintFormatElementItem* AnElement);
        void __fastcall SetData(TVpPrintFormatElementItem* AnElement);
        void __fastcall SetItemType(int Index);

public:         // User declarations
        __fastcall TfrmEditElement(TComponent* Owner);

        bool __fastcall Execute(TVpPrintFormatElementItem* AnElement);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmEditElement *frmEditElement;
//---------------------------------------------------------------------------
#endif
