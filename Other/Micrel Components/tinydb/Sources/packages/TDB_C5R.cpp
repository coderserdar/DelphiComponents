//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("TDB_C5R.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("TinyDB.pas");
USEPACKAGE("VCLDB50.bpi");
USEPACKAGE("VCLBDE50.bpi");
USEUNIT("Enc_Blowfish.pas");
USEUNIT("Enc_Gost.pas");
USEUNIT("Enc_Twofish.pas");
USEUNIT("EncryptBase.pas");
USEUNIT("Hash_CheckSum.pas");
USEUNIT("Hash_MD.pas");
USEUNIT("Hash_RipeMD.pas");
USEUNIT("Hash_SHA.pas");
USEUNIT("HashBase.pas");
USEUNIT("Compress_Zlib.pas");
USEUNIT("ZlibUnit.pas");
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
