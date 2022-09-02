//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("fsutils_d_bcb51.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("fsbackup.pas");
USEUNIT("fsadminplug.pas");
USEPACKAGE("Vclx50.bpi");
USEPACKAGE("Vcldb50.bpi");
USEPACKAGE("fsql1_r_bcb51.bpi");
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
