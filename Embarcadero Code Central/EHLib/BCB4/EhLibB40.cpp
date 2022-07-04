//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("EhLibB40.res");
USEUNIT("DBSumLst.pas");
USEPACKAGE("vclx40.bpi");
USEPACKAGE("vcl40.bpi");
USEPACKAGE("vcldb40.bpi");
USEPACKAGE("vcldbx40.bpi");
USEUNIT("PrnDbgeh.pas");
USEFORMNS("PrnDGDlg.pas", Prndgdlg, fPrnDBGridEHSetupDialog);
USEUNIT("PrntsEh.pas");
USEFORMNS("PrvFrmEh.pas", Prvfrmeh, PreviewFormEh);
USEUNIT("PrViewEh.pas");
USEUNIT("DBGridEh.pas");
USEUNIT("DBGridEhImpExp.pas");
USEUNIT("DBLookupEh.pas");
USEUNIT("DBLookupGridsEh");
USEUNIT("DBCtrlsEh.pas");
USEUNIT("ToolCtrlsEh.pas");
USEUNIT("EhLibConsts.pas");
USEUNIT("DbUtilsEh.pas");
USEFORMNS("CalculatorEh.pas", Calculatoreh, CalculatorEh);
USEUNIT("PropFilerEh.pas");
USEUNIT("PropStorageEh.pas");
USEFORMNS("DBGridEhFindDlgs.pas", Dbgridehfinddlgs, DBGridEhFindDlg);
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
    return 1;
}
//---------------------------------------------------------------------------
