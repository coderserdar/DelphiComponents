//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("IcsBcb30.res");
USEUNIT("WSocket.pas");
USERES("WSocket.dcr");
USEUNIT("WSockets.pas");
USERES("WSockets.dcr");
USEUNIT("httpprot.pas");
USERES("httpprot.dcr");
USEUNIT("ftpcli.pas");
USERES("ftpcli.dcr");
USEUNIT("wait.pas");
USERES("wait.dcr");
USEUNIT("tnscript.pas");
USERES("tnscript.dcr");
USEUNIT("fingcli.pas");
USERES("fingcli.dcr");
USEUNIT("nntpcli.pas");
USERES("nntpcli.dcr");
USEUNIT("ping.pas");
USERES("ping.dcr");
USEUNIT("tncnx.pas");
USERES("tncnx.dcr");
USEUNIT("tnemulvt.pas");
USERES("tnemulvt.dcr");
USEUNIT("emulvt.pas");
USERES("emulvt.dcr");
USEUNIT("FtpSrv.pas");
USERES("FtpSrv.dcr");
USEUNIT("SmtpProt.pas");
USERES("SmtpProt.dcr");
USEPACKAGE("VCL35.bpi");
USEUNIT("wsockbuf.pas");
USEUNIT("Pop3Prot.pas");
USERES("Pop3Prot.dcr");
USEUNIT("MimeDec.pas");
USERES("MimeDec.dcr");
USEUNIT("DnsQuery.pas");
USERES("DnsQuery.dcr");
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
