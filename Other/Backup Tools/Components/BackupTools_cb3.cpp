//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("BackupTools_cb3.res");
USEPACKAGE("VCLX35.bpi");
USEPACKAGE("VCL35.bpi");
USEPACKAGE("VCLDB35.bpi");
USEPACKAGE("VCLDBX35.bpi");
USEUNIT("aDiff.pas");
USEUNIT("aCRC32.pas");
USEUNIT("Archiver\RegisterArchiver.pas");
USERES("Archiver\RegisterArchiver.dcr");
USEUNIT("ZLib\Adler.pas");
USEUNIT("ZLib\BZlib.pas");
USEUNIT("ZLib\Crc.pas");
USEUNIT("ZLib\Infblock.pas");
USEUNIT("ZLib\Infcodes.pas");
USEUNIT("ZLib\Inffast.pas");
USEUNIT("ZLib\Inftrees.pas");
USEUNIT("ZLib\Infutil.pas");
USEUNIT("ZLib\Sutils.pas");
USEUNIT("ZLib\Trees.pas");
USEUNIT("ZLib\Zcompres.pas");
USEUNIT("ZLib\zDeflate.pas");
USEUNIT("ZLib\Zinflate.pas");
USEUNIT("ZLib\Zlib.pas");
USEUNIT("ZLib\Zuncompr.pas");
USEUNIT("ZLib\zutil.pas");
USEUNIT("Crypto\Cryptcon.pas");
USEUNIT("Crypto\Blowunit.pas");
USERES("Crypto\Blowunit.dcr");
USEUNIT("ExtBackup.pas");
USEUNIT("CopyFile.pas");
USERES("CopyFile.dcr");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Source du paquet.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
    return 1;
}
//---------------------------------------------------------------------------
