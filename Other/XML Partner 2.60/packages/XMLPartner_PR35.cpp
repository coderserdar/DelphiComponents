//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("XMLPartner_PR35.res");
USEPACKAGE("vcl35.bpi");
USEUNIT("XpParser.pas");
USEUNIT("xpbase.pas");
USEUNIT("xpchrflt.pas");
USEUNIT("XpDOM.pas");
USEUNIT("xpexcept.pas");
USEUNIT("xpinet.pas");
USEFORMNS("xpAboutw.pas", Xpaboutw, XpAboutBox);
USEUNIT("XpvFlBas.pas");
USEUNIT("XpvFlHTM.pas");
USERES("XpvFlHTM.dcr");
USEUNIT("XpvFlPrt.pas");
USERES("XpvFlPrt.dcr");
USEUNIT("XpvFlRTF.pas");
USERES("XpvFlRTF.dcr");
USEUNIT("XpvFlXML.pas");
USERES("XpvFlXML.dcr");
USEUNIT("XpHash.pas");
USEUNIT("XpSort.pas");
USEUNIT("XpXSLCon.pas");
USEUNIT("XpvXSLFO.pas");
USEUNIT("XpvXSLPr.pas");
USERES("XpvXSLPr.dcr");
USEUNIT("XpvXSLT.pas");
USEUNIT("XpvFOHsh.pas");
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
