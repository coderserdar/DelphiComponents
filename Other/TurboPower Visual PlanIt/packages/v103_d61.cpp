//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEFORMNS("..\source\VpAbout.pas", Vpabout, frmAbout);
USEFORMNS("..\source\VpContactEditDlg.pas", Vpcontacteditdlg, ContactEditForm);
USEFORMNS("..\source\VpNabEd.pas", Vpnabed, frmNavBarEd);
USEFORMNS("..\source\VpPrtPrvDlg.pas", Vpprtprvdlg, frmPrintPreview);
USEFORMNS("..\source\VpResEditDlg.pas", Vpreseditdlg, ResEditForm);
USEFORMNS("..\source\VpWavDlg.pas", Vpwavdlg, FrmSoundDialog);
USEFORMNS("..\source\VpDatePropEdit.pas", Vpdatepropedit, frmDatePropertyEditor);
USEFORMNS("..\source\VpFlxDsEd1.pas", Vpflxdsed1, frmFieldMapper);
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
