//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("nco855.res");
USEUNIT("NCOciDB.pas");
USEUNIT("NCOci.pas");
USEFORMNS("NCOciErrorDlg.pas", Ncocierrordlg, NCOciErrorFrm);
USEUNIT("NCOciMsg.pas");
USEUNIT("NCOciWrapper.pas");
USEUNIT("NCOciBuff.pas");
USEFORMNS("NCOciLoginDlg.pas", Ncocilogindlg, NCOciLoginFrm);
USEUNIT("NCOciParams.pas");
USEFORMNS("NCOciDM.pas", Ncocidm, OCIDM);
USEUNIT("NCOciUpdateSQL.pas");
USEUNIT("NCOciFilter.pas");
USEUNIT("NCOciReg.pas");
USEFORMNS("NCOciBreakDlg.pas", Ncocibreakdlg, NCOciBreakFrm);
USEUNIT("NCOciUtil.pas");
USEUNIT("NCSQLMon.pas");
USEFORMNS("NCOciPLSQLGenSetupDlg.pas", Ncociplsqlgensetupdlf, NCOciPLSQLGenSetupFrm);
USEUNIT("NCOciPLSQLGen.pas");
USEUNIT("NCOciCompNamer.pas");
USEFORMNS("NCOciCompNamerSetupDlg.pas", Ncocicompnamersetupdlg, OciCompNamerSetupFrm);
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("bcbsmp50.bpi");
USEPACKAGE("dcldb50.bpi");
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
