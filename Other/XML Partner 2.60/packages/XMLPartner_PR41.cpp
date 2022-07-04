//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("XMLPartner_PR41.res");
USEPACKAGE("vcl40.bpi");
USEUNIT("XpParser.pas");
USEUNIT("XpBase.pas");
USEUNIT("XpChrflt.pas");
USEUNIT("XpDOM.pas");
USEUNIT("XpExcept.pas");
USEUNIT("XpInet.pas");
USEPACKAGE("bcbsmp40.bpi");
USEUNIT("XpvFlBas.pas");
USEUNIT("XpvFlXML.pas");
USERES("XpvFlXML.dcr");
USEUNIT("XpvFlHTM.pas");
USERES("XpvFlHTM.dcr");
USEUNIT("XpvFlPrt.pas");
USERES("XpvFlPrt.dcr");
USEUNIT("XpvFlRTF.pas");
USERES("XpvFlRTF.dcr");
USEUNIT("XpXSLCon.pas");
USEFORMNS("XpAboutw.pas", Xpaboutw, XpAboutBox);
USEUNIT("XpvXSLT.pas");
USEUNIT("XpvXSLPr.pas");
USERES("XpvXSLPr.dcr");
USEUNIT("XpvXSLFO.pas");
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
