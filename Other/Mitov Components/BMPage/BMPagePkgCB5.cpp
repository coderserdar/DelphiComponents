//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("BMPagePkgCB5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("BMPage.cpp");
USERES("BMPage.dcr");
USEPACKAGE("BMWavePkgCB5.bpi");
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
