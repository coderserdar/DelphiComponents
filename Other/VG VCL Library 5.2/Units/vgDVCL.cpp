//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("vgDVCL.res");
USEUNIT("vgVCLRg.pas");
USEUNIT("vgComObj.pas");
USEUNIT("vgOleUtl.pas");
USEUNIT("vgI0_TLB.pas");
USEPACKAGE("vcl35.bpi");
USEPACKAGE("vclx35.bpi");
USEPACKAGE("dclstd35.bpi");
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
