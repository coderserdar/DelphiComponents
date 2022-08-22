//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("TDB_C5D.res");
USERES("TinyDB.dcr");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vclbde50.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("TDB_C5R.bpi");
USEUNIT("TinyDBReg.pas");
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
