//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("BMSplitterPkgCB5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("BMSplitter.cpp");
USERES("BMSplitter.dcr");
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
