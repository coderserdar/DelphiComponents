//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("v103_d51.res");
USEFORMNS("..\source\VpAbout.pas", Vpabout, frmAbout);
USEUNIT("..\source\VpAlarmDlg.pas");
USEUNIT("..\source\VpBase.pas");
USEUNIT("..\source\VpBaseDS.pas");
USEUNIT("..\source\VpBDEDS.pas");
USEUNIT("..\source\VpCalendar.pas");
USEUNIT("..\source\VpCanvasUtils.pas");
USEUNIT("..\source\VpClock.pas");
USEUNIT("..\source\VpConst.pas");
USEFORMNS("..\source\VpContactEditDlg.pas", Vpcontacteditdlg, ContactEditForm);
USEUNIT("..\source\VpContactGrid.pas");
USEUNIT("..\source\VpData.pas");
USEUNIT("..\source\VpDateEdit.pas");
USEUNIT("..\source\VpDayView.pas");
USEUNIT("..\source\VpDBDS.pas");
USEUNIT("..\source\VpDlg.pas");
USEUNIT("..\source\VpEdPop.pas");
USEUNIT("..\source\VpEvntEditDlg.pas");
USEUNIT("..\source\VpException.pas");
USEUNIT("..\source\VpFlxDS.pas");
USEUNIT("..\source\VpLEDLabel.pas");
USEUNIT("..\source\VpMisc.pas");
USEUNIT("..\source\VpMonthView.pas");
USEFORMNS("..\source\VpNabEd.pas", Vpnabed, frmNavBarEd);
USEUNIT("..\source\VpNavBar.pas");
USEUNIT("..\source\VpPrtFmt.pas");
USEUNIT("..\source\VpPrtFmtCBox.pas");
USEUNIT("..\source\VpPrtPrv.pas");
USEFORMNS("..\source\VpPrtPrvDlg.pas", Vpprtprvdlg, frmPrintPreview);
USEUNIT("..\source\VpReg.pas");
USEFORMNS("..\source\VpResEditDlg.pas", Vpreseditdlg, ResEditForm);
USEUNIT("..\source\VpSR.pas");
USEUNIT("..\source\VpTaskEditDlg.pas");
USEUNIT("..\source\VpTaskList.pas");
USEUNIT("..\source\VpTimerPool.pas");
USEFORMNS("..\source\VpWavDlg.pas", Vpwavdlg, FrmSoundDialog);
USEUNIT("..\source\VpWavPE.pas");
USEUNIT("..\source\VpWeekView.pas");
USEUNIT("..\source\VpXBase.pas");
USEUNIT("..\source\VpXChrFlt.pas");
USEUNIT("..\source\VpXParsr.pas");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("vclbde50.bpi");
USEFORMNS("..\source\VpDatePropEdit.pas", Vpdatepropedit, frmDatePropertyEditor);
USEUNIT("..\source\VpLocalize.pas");
USEFORMNS("..\source\Vpflxdsed1.pas", Vpflxdsed1, frmFieldMapper);
USEUNIT("..\source\VpContactButtons.pas");
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
