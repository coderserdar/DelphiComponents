//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEFORMNS("NCOciErrorDlg.pas", Ncocierrordlg, NCOciErrorFrm);
USEFORMNS("NCOciLoginDlg.pas", Ncocilogindlg, NCOciLoginFrm);
USEFORMNS("NCOciDM.pas", Ncocidm, OCIDM);
USEFORMNS("NCOciBreakDlg.pas", Ncocibreakdlg, NCOciBreakFrm);
USEFORMNS("NCOciPLSQLGenSetupDlg.pas", Ncociplsqlgensetupdlf, NCOciPLSQLGenSetupFrm);
USEFORMNS("NCOciCompNamerSetupDlg.pas", Ncocicompnamersetupdlg, OciCompNamerSetupFrm);
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
