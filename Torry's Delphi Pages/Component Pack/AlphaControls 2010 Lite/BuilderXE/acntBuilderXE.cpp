//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("sInternalSkins.pas", Sinternalskins, FormInternalSkins);
USEFORMNS("sStrEdit.pas", Sstredit, StrEditDlg);
USEFORMNS("acSkinInfo.pas", Acskininfo, SkinInfoForm);
USEFORMNS("ac3dNewClass.pas", Ac3dnewclass, FormNewThirdClass);
USEFORMNS("ac3rdPartyEditor.pas", Ac3rdpartyeditor, Form3rdPartyEditor);
USEFORMNS("sImgListEditor.pas", Simglisteditor, FormImgListEditor);
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
