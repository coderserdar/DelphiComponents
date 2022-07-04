//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("IcsBcb50.res");
USEUNIT("WSocket.pas");
USERES("WSocket.dcr");
USEUNIT("Httpprot.pas");
USERES("Httpprot.dcr");
USEUNIT("Ftpcli.pas");
USERES("Ftpcli.dcr");
USEUNIT("Wait.pas");
USERES("Wait.dcr");
USEUNIT("TnScript.pas");
USERES("TnScript.dcr");
USEUNIT("Fingcli.pas");
USERES("Fingcli.dcr");
USEUNIT("Nntpcli.pas");
USERES("Nntpcli.dcr");
USEUNIT("Ping.pas");
USERES("Ping.dcr");
USEUNIT("TnCnx.pas");
USERES("TnCnx.dcr");
USEUNIT("TnEmulVT.pas");
USERES("TnEmulVT.dcr");
USEUNIT("EmulVT.pas");
USERES("EmulVT.dcr");
USEUNIT("FtpSrv.pas");
USERES("FtpSrv.dcr");
USEUNIT("SmtpProt.pas");
USERES("SmtpProt.dcr");
USEUNIT("WSockBuf.pas");
USEUNIT("Pop3Prot.pas");
USERES("Pop3Prot.dcr");
USEUNIT("MimeDec.pas");
USERES("MimeDec.dcr");
USEUNIT("DnsQuery.pas");
USERES("DnsQuery.dcr");
USEUNIT("WSocketS.pas");
USERES("WSocketS.dcr");
USEUNIT("HttpSrv.pas");
USERES("HttpSrv.dcr");
USEPACKAGE("vcl50.bpi");
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
