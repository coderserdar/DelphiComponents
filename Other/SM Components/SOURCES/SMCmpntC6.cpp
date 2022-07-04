//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("SMDBFind.pas", Smdbfind, frmFind);
USEFORMNS("SMDBFltr.pas", Smdbfltr, frmFilterDialog);
USEFORMNS("SMDBFltrFile.pas", Smdbfltrfile, frmFilterFileDialog);
USEFORMNS("SMDBGSet.pas", Smdbgset, frmGridSetup);
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
