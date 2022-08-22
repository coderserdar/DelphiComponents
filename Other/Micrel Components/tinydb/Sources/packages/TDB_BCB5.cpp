//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("TDB_BCB5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("Compress_Zlib.pas");
USEUNIT("Enc_Blowfish.pas");
USEUNIT("Enc_Gost.pas");
USEUNIT("Enc_Twofish.pas");
USEUNIT("EncryptBase.pas");
USEUNIT("Hash_CheckSum.pas");
USEUNIT("Hash_MD.pas");
USEUNIT("Hash_RipeMD.pas");
USEUNIT("Hash_SHA.pas");
USEUNIT("HashBase.pas");
USEUNIT("TinyDB.pas");
USERES("TinyDB.dcr");
USEUNIT("TinyDBIni.pas");
USEUNIT("TinyDBReg.pas");
USEUNIT("ZlibUnit.pas");
USEPACKAGE("Vcldb50.bpi");
USEPACKAGE("VCLBDE50.bpi");
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
