//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("v103is41.res");
USEPACKAGE("vcl40.bpi");
USEPACKAGE("vclx40.bpi");
USEPACKAGE("vcldb40.bpi");
USEUNIT("..\source\VpDBIsamDS.pas");
USEUNIT("..\source\VpRegIs.pas");
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
