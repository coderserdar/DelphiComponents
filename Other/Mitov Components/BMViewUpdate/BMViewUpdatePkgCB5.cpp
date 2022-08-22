//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("BMViewUpdatePkgCB5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("BMViewUpdate.cpp");
USERES("BMViewUpdate.dcr");
USEPACKAGE("BMMessageHookPkgCB5.bpi");
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
