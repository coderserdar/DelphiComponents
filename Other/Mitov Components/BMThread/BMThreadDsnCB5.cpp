//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("BMThreadDsnCB5.res");
USEPACKAGE("vcl50.bpi");
USEFORM("BMSynchroMethods.cpp", SynchroMethodsForm);
USEPACKAGE("BMThreadPkgCB5.bpi");
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
