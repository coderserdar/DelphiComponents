//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("ff2jr35.res");
USEPACKAGE("VCL35.bpi");
USEPACKAGE("VCLDB35.bpi");
USEPACKAGE("ff2_r35.bpi");
USEPACKAGE("IP40_C3.bpi");
USEUNIT("..\source\ffwwtabl.pas");
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
