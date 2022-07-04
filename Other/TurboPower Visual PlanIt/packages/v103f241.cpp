//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("v103f241.res");
USEPACKAGE("vcl40.bpi");
USEPACKAGE("vclx40.bpi");
USEPACKAGE("vcldb40.bpi");
USEUNIT("..\source\VpRegF2.pas");
USEUNIT("..\source\VpFF2DS.pas");
USEPACKAGE("v103_d41.bpi");
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
