//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("color.res");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("color50.bpi");
USEUNIT("..\colorado\colorpe.pas");
USERES("..\colorado\colorpe.res);
USEUNIT("..\colorx\colorxpe.pas");
USERES("..\colorx\colorxpe.res);
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
