//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("XMLPartner_PD41.res");
USERES("XpReg.dcr");
USEPACKAGE("vcl40.bpi");
USEUNIT("XpReg.pas");
USEPACKAGE("XMLPartner_PR41.bpi");
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
