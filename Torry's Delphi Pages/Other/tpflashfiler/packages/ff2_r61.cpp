//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEFORMNS("..\source\fflogdlg.pas", Fflogdlg, FFLoginDialog);
USEFORMNS("..\source\ffsrvdlg.pas", Ffsrvdlg, FFPickServerDlg);
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
