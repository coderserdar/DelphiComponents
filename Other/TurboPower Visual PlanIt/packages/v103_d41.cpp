/* ***** BEGIN LICENSE BLOCK *****                                            */
/* Version: MPL 1.1                                                           */
/*                                                                            */
/* The contents of this file are subject to the Mozilla Public License Version*/
/* 1.1 (the "License"); you may not use this file except in compliance with   */
/* the License. You may obtain a copy of the License at                       */
/* http://www.mozilla.org/MPL/                                                */
/*                                                                            */
/* Software distributed under the License is distributed on an "AS IS" basis, */
/* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   */
/* for the specific language governing rights and limitations under the       */
/* License.                                                                   */
/*                                                                            */
/* The Original Code is TurboPower VisualPlanIt                               */
/*                                                                            */
/* The Initial Developer of the Original Code is TurboPower Software          */
/*                                                                            */
/* Portions created by TurboPower Software Co. are Copyright (C) 2002         */
/* TurboPower SOftware Co. All Rights Reserved.                               */
/*                                                                            */
/* Contributor(s):                                                            */
/*                                                                            */
/* ***** END LICENSE BLOCK *****                                              */

//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("V103_D41.res");
USEPACKAGE("vclx40.bpi");
USEPACKAGE("vcl40.bpi");
USEPACKAGE("Vcldb40.bpi");
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
USEFORMNS("..\source\VpDatePropEdit.pas", Vpdatepropedit, frmDatePropertyEditor);
USEUNIT("..\source\VpLocalize.pas");
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
