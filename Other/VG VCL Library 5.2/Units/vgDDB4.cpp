//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("vgDDB4.res");
USEUNIT("vgDBRg.pas");
USEPACKAGE("vcl40.bpi");
USEPACKAGE("vcldb40.bpi");
USEPACKAGE("dclstd40.bpi");
USEPACKAGE("vgDVCL4.bpi");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
