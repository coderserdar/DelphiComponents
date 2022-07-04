//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("ATViewerC5.res");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("Vclx50.bpi");
USEPACKAGE("vcljpg50.bpi");
USEPACKAGE("vclie50.bpi");
USEUNIT("..\Source\MediaPlayer9_TLB.pas");
USERES("..\Source\MediaPlayer9_TLB.dcr");
USEUNIT("..\Source\MediaPlayer_TLB.pas");
USERES("..\Source\MediaPlayer_TLB.dcr");
USEUNIT("..\Source\ATBinHex.pas");
USERES("..\Source\ATBinHex.dcr");
USEUNIT("..\Source\ATFileNotification.pas");
USERES("..\Source\ATFileNotification.dcr");
USEUNIT("..\Source\ATImageBox.pas");
USERES("..\Source\ATImageBox.dcr");
USEUNIT("..\Source\ATStreamSearch.pas");
USERES("..\Source\ATStreamSearch.dcr");
USEUNIT("..\Source\ATViewer.pas");
USERES("..\Source\ATViewer.dcr");
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
