//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("v103is51.res");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("vclbde50.bpi");
USEUNIT("..\source\VpRegIs.pas");
USEUNIT("..\source\VpDBIsamDS.pas");
USEPACKAGE("v103_d51.bpi");
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
