//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("BMThreadPkgCB5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("BMThread.cpp");
USERES("BMThread.dcr");
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
