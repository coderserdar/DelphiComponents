//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("VolgaPC5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("VolCalc.pas");
USEUNIT("VolCalend.pas");
USEUNIT("VolDBConst.pas");
USEUNIT("VolDBEdit.pas");
USEUNIT("VolDBGrid.pas");
USEUNIT("VolFndEd.pas");
USEUNIT("VolMeter.pas");
USEUNIT("VolPeriod.pas");
USEPACKAGE("Vcldb50.bpi");
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
