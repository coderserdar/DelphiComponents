//---------------------------------------------------------------------------
#ifndef ExVpRptSetupH
#define ExVpRptSetupH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "VpDateEdit.hpp"
#include "VpEdPop.hpp"
#include "VpPrtFmtCBox.hpp"
//---------------------------------------------------------------------------
struct TReportDataRec {
   TDateTime StartDate;
   TDateTime EndDate;
   String Format;
   } ReportData;
//---------------------------------------------------------------------------
class TfrmReportSetup : public TForm
{
__published:    // IDE-managed Components
        TLabel *Label1;
        TLabel *Label2;
        TLabel *Label3;
        TButton *Button1;
        TButton *Button2;
        TVpPrintFormatComboBox *VpPrintFormatComboBox1;
        TVpDateEdit *VpDateEdit1;
        TVpDateEdit *VpDateEdit2;
        void __fastcall Button2Click(TObject *Sender);
        void __fastcall Button1Click(TObject *Sender);
private:        // User declarations
        TVpControlLink* __fastcall GetControlLink();
        void __fastcall SetControlLink(TVpControlLink* Value);
        TDateTime __fastcall GetDate(int Index);
        void __fastcall SetDate(int Index, TDateTime Value);
public:         // User declarations
        __fastcall TfrmReportSetup(TComponent* Owner);

        bool __fastcall Execute();
        void __fastcall SaveData();

        __property TVpControlLink* ControlLink = {read=GetControlLink, write=SetControlLink};
        __property TDateTime StartDate = {read=GetDate, write=SetDate, index=1};
        __property TDateTime EndDate = {read=GetDate, write=SetDate, index=2};
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmReportSetup *frmReportSetup;
//---------------------------------------------------------------------------
#endif
