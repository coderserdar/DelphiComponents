//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("BMViewUpdateDsnCB5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("BMViewUpdateEditors.cpp");
USEPACKAGE("BMMessageHookPkgCB5.bpi");
USEPACKAGE("BMViewUpdatePkgCB5.bpi");
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
