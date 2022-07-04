//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("BackupTools_cb4.res");
USEPACKAGE("vcl40.bpi");
USEFORMNS("prgform.pas", Prgform, ProgForm);
USEUNIT("aDiff.pas");
USEUNIT("CopyFile.pas");
USERES("CopyFile.dcr");
USEUNIT("ExtBackup.pas");
USEUNIT("MyArchBackup.pas");
USEUNIT("MyBackup.pas");
USERES("MyBackup.dcr");
USEUNIT("aCRC32.pas");
USEUNIT("Archiver\RegisterArchiver.pas");
USERES("Archiver\RegisterArchiver.dcr");
USEPACKAGE("vcldb40.bpi");
USEPACKAGE("vclsmp40.bpi");
USEPACKAGE("vclx40.bpi");
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
