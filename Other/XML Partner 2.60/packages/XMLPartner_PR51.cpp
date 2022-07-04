//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("XMLPartner_PR51.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("XpBase.pas");
USEUNIT("XpChrFlt.pas");
USEUNIT("XpDOM.pas");
USEUNIT("XpExcept.pas");
USEUNIT("XpvFlBas.pas");
USEUNIT("XpvFlHTM.pas");
USERES("XpvFlHTM.dcr");
USEUNIT("XpvFlPrt.pas");
USERES("XpvFlPrt.dcr");
USEUNIT("XpvFlRTF.pas");
USERES("XpvFlRTF.dcr");
USEUNIT("XpvFlXML.pas");
USERES("XpvFlXML.dcr");
USEUNIT("XpInet.pas");
USEUNIT("XpParser.pas");
USEUNIT("XpXSLCon.pas");
USEFORMNS("XpAboutw.pas", Xpaboutw, XpAboutBox);
USEUNIT("XpHash.pas");
USEUNIT("XpSort.pas");
USEUNIT("XpvXSLT.pas");
USEUNIT("XpvFOHsh.pas");
USEUNIT("XpvXSLFO.pas");
USEUNIT("XpvXSLPr.pas");
USERES("XpvXSLPr.dcr");
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
