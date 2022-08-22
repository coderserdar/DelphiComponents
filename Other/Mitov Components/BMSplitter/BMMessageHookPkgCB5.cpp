//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("BMMessageHookPkgCB5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("BMMessageHook.cpp");
USERES("BMMessageHook.dcr");
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
