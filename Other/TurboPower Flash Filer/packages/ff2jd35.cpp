//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("ff2jd35.res");
USEPACKAGE("VCL35.bpi");
USEPACKAGE("vcldb35.bpi");
USEPACKAGE("ff2jr35.bpi");
USEPACKAGE("ff2_d35.bpi");
USEUNIT("..\source\ffwwreg.pas");
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
