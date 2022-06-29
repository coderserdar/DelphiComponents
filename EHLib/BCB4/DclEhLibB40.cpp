//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("DclEhLibB40.res");
USEFORMNS("GridEhEd.pas", Gridehed, DBGridEHColumnsEditor);
USEPACKAGE("vclx40.bpi");
USEPACKAGE("vcl40.bpi");
USEPACKAGE("vcldb40.bpi");
USEPACKAGE("vcldbx40.bpi");
USEUNIT("EhLibReg.pas");
USERES("EhLibReg.dcr");
USEFORMNS("RichEdEh.pas", Richedeh, RichStrEditDlgEh);
USEPACKAGE("dclstd40.bpi");
USEPACKAGE("EhLibB40.bpi");
USEFORMNS("PropStorageEditEh.pas", Propstorageediteh, PropStorageEditEhForm);
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
