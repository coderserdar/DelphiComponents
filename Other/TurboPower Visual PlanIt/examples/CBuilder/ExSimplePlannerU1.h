//---------------------------------------------------------------------------
#ifndef ExSimplePlannerU1H
#define ExSimplePlannerU1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "VpBase.hpp"
#include "VpBaseDS.hpp"
#include "VpBDEDS.hpp"
#include "VpDayView.hpp"
#include "VpDBDS.hpp"
#include "VpDlg.hpp"
#include "VpMonthView.hpp"
#include "VpResEditDlg.hpp"
#include "VpTaskList.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:    // IDE-managed Components
        TVpDayView *VpDayView1;
        TVpMonthView *VpMonthView1;
        TVpTaskList *VpTaskList1;
        TButton *Button1;
        TVpResourceCombo *VpResourceCombo1;
        TVpBDEDataStore *VpBDEDataStore1;
        TVpControlLink *VpControlLink1;
        TVpResourceEditDialog *VpResourceEditDialog1;
        void __fastcall Button1Click(TObject *Sender);
private:        // User declarations
public:         // User declarations
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
