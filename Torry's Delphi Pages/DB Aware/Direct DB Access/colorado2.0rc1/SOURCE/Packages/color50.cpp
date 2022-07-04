//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#pragma package(smart_init)
USERES("color.res");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcldb50.bpi");
USEUNIT("..\colorado\cadodb.pas");
USEUNIT("..\colorado\cblobdat.pas");
USEUNIT("..\colorado\cconsts.pas");
USEFORMNS("..\colorado\cdlink.pas", Cdlink, CDataLink);
USEFORMNS("..\colorado\cerrdlg.pas", Cerrdlg, OLEDBProviderErrorsForm);
USEUNIT("..\colorado\cfields.pas");
USEUNIT("..\colorado\cmdacver.pas");
USEUNIT("..\colorado\cmsdasc.pas");
USEUNIT("..\colorado\colorado.pas");
USEUNIT("..\colorado\cschema.pas");
USEUNIT("..\colorado\oledbadm.pas");
USEUNIT("..\colorx\cadoxdb.pas");
USEUNIT("..\colorx\colorx.pas");
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
