//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("VolgaPC4.res");
USEPACKAGE("vcl40.bpi");
USEUNIT("VolPeriod.pas");
USEUNIT("VolCalc.pas");
USEUNIT("VolCalend.pas");
USEUNIT("VolDBConst.pas");
USEUNIT("VolDBEdit.pas");
USEUNIT("VolDBGrid.pas");
USEUNIT("VolFndEd.pas");
USEUNIT("VolMeter.pas");
USEPACKAGE("vcldb40.bpi");
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
