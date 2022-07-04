//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("v103ad41.res");
USEPACKAGE("vclx40.bpi");
USEPACKAGE("vcl40.bpi");
USEPACKAGE("bcbsmp40.bpi");
USEUNIT("..\source\VpAdvDS.pas");
USEUNIT("..\source\VpRegAd.pas");
USEPACKAGE("Vcldb40.bpi");
USEPACKAGE("V103_D41.bpi");
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
