//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("vgDRXDB.res");
USEPACKAGE("vcl35.bpi");
USEPACKAGE("vclx35.bpi");
USEPACKAGE("vcldb35.bpi");
USEPACKAGE("dclstd35.bpi");
USEPACKAGE("dcldb35.bpi");
USEPACKAGE("vgDVCL.bpi");
USEPACKAGE("vgDDB.bpi");
USEPACKAGE("Rxctl.bpi");
USEPACKAGE("Rxdb.bpi");
USEUNIT("vgRxDBRg.pas");

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
