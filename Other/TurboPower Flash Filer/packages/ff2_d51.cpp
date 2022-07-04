//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("ff2_d51.res");
USERES("..\source\ffclreg.dcr");
USEPACKAGE("ff2_r51.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcldb50.bpi");
USEUNIT("..\source\ffabout.pas");
USEUNIT("..\source\ffclcoln.pas");     
USEUNIT("..\source\ffclexps.pas");
USEUNIT("..\source\ffclexpt.pas");
USEUNIT("..\source\ffclfldg.pas");
USEUNIT("..\source\ffclreg.pas");
USEUNIT("..\source\ffclsqle.pas");
USEUNIT("..\source\ffclver.pas");

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
