//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("X260PD51.res");
USEUNIT("XpReg.pas");
USERES("XpReg.dcr");
USEPACKAGE("Vcl50.bpi");
USEPACKAGE("X260PR51.bpi");
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
