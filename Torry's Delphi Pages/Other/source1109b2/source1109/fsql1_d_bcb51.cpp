//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("fsql1_d_bcb51.res");
USERES("fsclreg.dcr");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcldb50.bpi");
USEUNIT("fsclcoln.pas");
USEUNIT("fsclexps.pas");
USEUNIT("fsclfldg.pas");
USEUNIT("fsclreg.pas");
USEUNIT("fsclsqle.pas");
USEPACKAGE("fsql1_r_bcb51.bpi");
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
