//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("EhLibB50.res");
USEUNIT("DBSumLst.pas");
USEUNIT("PrnDbgeh.pas");
USEFORMNS("PrnDGDlg.pas", Prndgdlg, fPrnDBGridEHSetupDialog);
USEUNIT("PrntsEh.pas");
USEFORMNS("PrvFrmEh.pas", Prvfrmeh, PreviewFormEh);
USEUNIT("PrViewEh.pas");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("vcldbx50.bpi");
USEUNIT("DBGridEh.pas");
USEUNIT("DBGridEhImpExp.pas");
USEUNIT("DBLookupEh.pas");
USEUNIT("DBLookupGridsEh");
USEUNIT("ToolCtrlsEh.pas");
USEUNIT("DBCtrlsEh.pas");
USEUNIT("EhLibConsts.pas");
USEUNIT("DbUtilsEh.pas");
USEFORMNS("CalculatorEh.pas", Calculatoreh, CalculatorEh);
USEUNIT("PropStorageEh.pas");
USEUNIT("PropFilerEh.pas");
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
