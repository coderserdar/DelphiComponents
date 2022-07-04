//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("KADaoC5.res");
USEUNIT("KDaoDBEngine.pas");
USEUNIT("KDaoWorkspace.pas");
USEUNIT("KDaoDataBase.pas");
USEUNIT("KDaoTable.pas");
USEUNIT("KADaoInfo.pas");
USEUNIT("KADaoEncrypter.pas");
USEUNIT("KADaoConnectionCheck.pas");
USEUNIT("KADaoReg.pas");
USERES("KDaoDataBase.dcr");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("vclbde50.bpi");
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
