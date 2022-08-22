//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("BMWavePkgCB5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("BMWave.cpp");
USERES("BMWave.dcr");
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
