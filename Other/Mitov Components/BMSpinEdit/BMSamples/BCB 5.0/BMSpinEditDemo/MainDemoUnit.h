//---------------------------------------------------------------------------

#ifndef MainDemoUnitH
#define MainDemoUnitH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BMSpinEdit.h"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
    TBMSpinEdit *BMSpinEdit1;
    TBMSpinEdit *BMSpinEdit2;
    TBMSpinEdit *BMSpinEdit3;
private:	// User declarations
public:		// User declarations
    __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
