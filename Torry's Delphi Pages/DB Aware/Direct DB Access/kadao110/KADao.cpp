//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("KADao.res");
USEPACKAGE("vcl35.bpi");
USEUNIT("KDaoDBEngine.pas");
USEUNIT("KDaoWorkspace.pas");
USEUNIT("KDaoDataBase.pas");
USEUNIT("KDaoTable.pas");
USEUNIT("KADaoInfo.pas");
USEUNIT("KADaoEncrypter.pas");
USEUNIT("KADaoConnectionCheck.pas");
USEUNIT("KADaoReg.pas");
USERES("KDaoDataBase.dcr");
USEPACKAGE("vclx35.bpi");
USEPACKAGE("vcldb35.bpi");
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
