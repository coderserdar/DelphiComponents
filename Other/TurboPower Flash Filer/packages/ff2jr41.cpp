//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("FF2JR41.res");
USEPACKAGE("vcl40.bpi");
USEPACKAGE("vcldb40.bpi");
USEPACKAGE("FF2_R41.bpi");
USEPACKAGE("IP40_C4.bpi");
USEUNIT("..\source\FFwwTabl.pas");
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
