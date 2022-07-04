//---------------------------------------------------------------------------
#ifndef ExUtilH
#define ExUtilH

#include <inifiles.hpp>
//---------------------------------------------------------------------------
void __fastcall RestoreFormState(TForm* aForm, TIniFile* anINIFile,
                           const String aSection);
void __fastcall SaveFormState(TForm* aForm, TIniFile* anINIFile,
                           const String aSection);
#endif
