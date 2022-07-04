//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("v103f251.res");
USEUNIT("..\source\VpRegF2.pas");
USEUNIT("..\source\VpFF2DS.pas");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("v103_d51.bpi");
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
