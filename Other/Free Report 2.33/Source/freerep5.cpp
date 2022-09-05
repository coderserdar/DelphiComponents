//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEUNIT("fr_reg.pas");
USERES("fr_reg.dcr");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vclsmp50.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("vclbde50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("vcljpg50.bpi");
USEPACKAGE("tee50.bpi");
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
