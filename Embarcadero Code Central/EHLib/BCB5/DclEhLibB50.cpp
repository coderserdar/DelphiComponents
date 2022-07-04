//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("DclEhLibB50.res");
USEFORMNS("GridEhEd.pas", Gridehed, DBGridEHColumnsEditor);
USEUNIT("EhLibReg.pas");
USERES("EhLibReg.dcr");
USEFORMNS("RichEdEh.pas", Richedeh, RichStrEditDlgEh);
USEPACKAGE("vclx50.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("vcldbx50.bpi");
USEPACKAGE("dsnide50.bpi");
USEFORMNS("PropStorageEditEh.pas", Propstorageediteh, PropStorageEditEhForm);
USEPACKAGE("EhLibB50.bpi");
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
