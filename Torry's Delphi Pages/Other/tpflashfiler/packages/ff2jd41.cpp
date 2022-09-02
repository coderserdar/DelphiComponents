//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("FF2JD41.res");
USEPACKAGE("vcl40.bpi");
USEPACKAGE("vcldb40.bpi");
USEPACKAGE("FF2JR41.bpi");
USEPACKAGE("FF2_D41.bpi");
USEUNIT("..\source\FFwwReg.pas");
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
