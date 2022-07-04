//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("vgDDB.res");
USEPACKAGE("vcl35.bpi");
USEPACKAGE("vcldb35.bpi");
USEPACKAGE("dclstd35.bpi");
USEPACKAGE("dcldb35.bpi");
USEPACKAGE("vgDVCL.bpi");
USEUNIT("vgDBRg.pas");
USEPACKAGE("vclx35.bpi");
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
