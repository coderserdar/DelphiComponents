//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("BMMediaDialogsPkgCB5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("BMMediaDialogs.cpp");
USERES("BMMediaDialogs.dcr");
USEPACKAGE("Vclx50.bpi");
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
