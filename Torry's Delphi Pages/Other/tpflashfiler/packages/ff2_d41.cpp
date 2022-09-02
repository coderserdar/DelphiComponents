//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("ff2_d41.res");
USERES("..\source\ffclreg.dcr");
USEPACKAGE("ff2_r41.bpi");
USEPACKAGE("VCL40.bpi");
USEPACKAGE("VCLDB40.bpi");
USEFORMNS("..\source\ffabout.pas", Ffabout, FFAboutBox);
USEFORMNS("..\source\ffclcoln.pas", Ffclcoln, ffParamEditor);
USEFORMNS("..\source\ffclexps.pas", Ffclexps, frmSelectProtocols);
USEUNIT("..\source\ffclexpt.pas");
USEFORMNS("..\source\ffclfldg.pas", Ffclfldg, frmFieldLinkDesigner);
USEUNIT("..\source\ffclreg.pas");
USEFORMNS("..\source\ffclsqle.pas", Ffclsqle, ffSqlEditor);

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
