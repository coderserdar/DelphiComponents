//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("vgDRXDB4.res");
USEUNIT("vgRxDBRg.pas");
USEPACKAGE("vcl40.bpi");
USEPACKAGE("vclx40.bpi");
USEPACKAGE("vcldb40.bpi");
USEPACKAGE("vgDVCL4.bpi");
USEPACKAGE("vgDDB4.bpi");
USEPACKAGE("Rxctl4.bpi");
USEPACKAGE("Rxdb4.bpi");
USEPACKAGE("dcldb40.bpi");
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
