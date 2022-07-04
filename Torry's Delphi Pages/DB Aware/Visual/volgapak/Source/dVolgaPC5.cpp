//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("dVolgaPC5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("VolColEditor.pas");
USEUNIT("VolDBReg.pas");
USEFORMNS("VolHintProp.pas", Volhintprop, frmVHintEdit);
USERES("VolFndEd.dcr");
USERES("VolMeter.dcr");
USERES("VolPeriod.dcr");
USERES("VolCalend.dcr");
USERES("VolDBEdit.dcr");
USERES("VolDBGrid.dcr");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("VolgaPC5.bpi");
USEPACKAGE("dsnide50.bpi");
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
