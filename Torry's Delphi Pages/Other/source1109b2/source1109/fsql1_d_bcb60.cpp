//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEFORMNS("fsclexps.pas", Fsclexps, fsSelectProtocols);
USEFORMNS("fsclcoln.pas", Fsclcoln, fsParamEditor);
USEFORMNS("fsclfldg.pas", Fsclfldg, fsFieldLinkDesigner);
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
