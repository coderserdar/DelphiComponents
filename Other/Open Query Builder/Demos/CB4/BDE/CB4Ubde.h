//---------------------------------------------------------------------------
#ifndef CB4UbdeH
#define CB4UbdeH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include "QBEBDE.hpp"
#include "QBuilder.hpp"
//---------------------------------------------------------------------------
class TQBDemoForm : public TForm
{
__published:  // IDE-managed Components
        TPanel *Panel1;
        TButton *BtnQBuilder;
        TMemo *Memo;
        TOQBuilderDialog *OQBuilderDialog;
        TOQBEngineBDE *OQBEngineBDE;
        void __fastcall BtnQBuilderClick(TObject *Sender);
private:  // User declarations
public:   // User declarations
        __fastcall TQBDemoForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TQBDemoForm *QBDemoForm;
//---------------------------------------------------------------------------
#endif
