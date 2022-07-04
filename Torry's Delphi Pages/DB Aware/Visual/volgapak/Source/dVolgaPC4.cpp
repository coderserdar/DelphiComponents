//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("dVolgaPC4.res");
USEPACKAGE("vcl40.bpi");
USERES("VolPeriod.dcr");
USERES("VolCalend.dcr");
USEUNIT("VolColEditor.pas");
USERES("VolDBEdit.dcr");
USERES("VolDBGrid.dcr");
USEUNIT("VolDBReg.pas");
USERES("VolFndEd.dcr");
USEFORMNS("VolHintProp.pas", Volhintprop, frmVHintEdit);
USERES("VolMeter.dcr");
USEPACKAGE("vcldb40.bpi");
USEPACKAGE("VolgaPC4.bpi");
USEPACKAGE("dclstd40.bpi");
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
