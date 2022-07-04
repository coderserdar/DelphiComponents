//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("tbxdsgn_cb5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("..\TBXStrEdit.pas");
USEUNIT("..\TBXReg.pas");
USERES("..\TBXReg.dcr");
USEPACKAGE("tb2k_cb5.bpi");
USEPACKAGE("tb2kdsgn_cb5.bpi");
USEPACKAGE("tbx_cb5.bpi");
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
