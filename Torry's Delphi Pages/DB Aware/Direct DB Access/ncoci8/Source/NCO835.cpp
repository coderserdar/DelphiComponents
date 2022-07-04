//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("NCO835.res");
USEPACKAGE("vcl35.bpi");
USEUNIT("NCSQLMon.pas");
USEUNIT("NCOciBreakDlg.pas");
USEUNIT("NCOciBuff.pas");
USEUNIT("NCOciDB.pas");
USEUNIT("NCOciDM.pas");
USEUNIT("NCOciErrorDlg.pas");
USEUNIT("NCOciFilter.pas");
USEUNIT("NCOciLoginDlg.pas");
USEUNIT("NCOciMsg.pas");
USEUNIT("NCOciParams.pas");
USEUNIT("NCOciReg.pas");
USEUNIT("NCOciUpdateSQL.pas");
USEUNIT("NCOciUtil.pas");
USEUNIT("NCOciWrapper.pas");
USEUNIT("NCOci.pas");
USEUNIT("NCOciPLSQLGenSetupDlg.pas");
USEUNIT("NCOciPLSQLGen.pas");
USEUNIT("NCOciCompNamer.pas");
USEUNIT("NCOciCompNamerSetupDlg.pas");

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
