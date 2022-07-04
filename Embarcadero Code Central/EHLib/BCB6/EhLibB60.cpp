//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("CalculatorEh.pas", Calculatoreh, CalculatorEh);
USEFORMNS("PrnDgDlg.pas", Prndgdlg, fPrnDBGridEHSetupDialog);
USEFORMNS("PrvFrmEh.pas", Prvfrmeh, PreviewFormEh);
USEFORMNS("DBGridEhFindDlgs.pas", Dbgridehfinddlgs, DBGridEhFindDlg);
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
