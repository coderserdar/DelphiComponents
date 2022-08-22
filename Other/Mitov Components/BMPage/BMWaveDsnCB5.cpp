//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("BMWaveDsnCB5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("BMWaveEditors.cpp");
USEPACKAGE("BMWavePkgCB5.bpi");
USEPACKAGE("Vclx50.bpi");
USEPACKAGE("BMMediaDialogsPkgCB5.bpi");
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
