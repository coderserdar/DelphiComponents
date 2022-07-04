//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("smcmpntC5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("Smdbgrid.pas");
USERES("Smdbgrid.dcr");
USEUNIT("SMCnst.pas");
USEUNIT("RXUtils.pas");
USEPACKAGE("VCLDB50.bpi");
USEPACKAGE("VCLBDE50.bpi");
USEUNIT("RunText.pas");
USERES("RunText.dcr");
USEUNIT("AngleLbl.pas");
USEUNIT("BrazilCombo.pas");
USERES("BrazilCombo.dcr");
USEUNIT("charmap.pas");
USERES("charmap.dcr");
USEUNIT("Conerbtn.pas");
USERES("Conerbtn.dcr");
USEUNIT("FileNotify.pas");
USERES("FileNotify.dcr");
USEUNIT("Gradpnl.pas");
USERES("Gradpnl.dcr");
USEUNIT("Limitfrm.pas");
USERES("Limitfrm.dcr");
USEUNIT("Moneystr.pas");
USERES("Moneystr.dcr");
USEUNIT("Obj2XML.pas");
USEUNIT("URLLbl.pas");
USERES("URLLbl.dcr");
USEUNIT("Scrltool.pas");
USERES("Scrltool.dcr");
USEUNIT("SendMail.pas");
USERES("SendMail.dcr");
USEUNIT("sensors.pas");
USERES("sensors.dcr");
USEUNIT("SMBar.pas");
USERES("SMBar.dcr");
USEUNIT("SMBevel.pas");
USERES("SMBevel.dcr");
USEUNIT("SMBox.pas");
USEUNIT("SMBoxReg.pas");
USEUNIT("SMCVersInfo.pas");
USERES("SMCVersInfo.dcr");
USEUNIT("SMDBAcc.pas");
USERES("SMDBAcc.dcr");
USEUNIT("SMDBComb.pas");
USERES("SMDBComb.dcr");
USEUNIT("SMDBCombReg.pas");
USEUNIT("SMDBCtrl.pas");
USERES("SMDBCtrl.dcr");
USEFORMNS("SMDBFind.pas", Smdbfind, frmFind);
USERES("SMDBFind.dcr");
USEFORMNS("SMDBFltr.pas", Smdbfltr, frmFilterDialog);
USERES("SMDBFltr.dcr");
USEFORMNS("SMDBFltrFile.pas", Smdbfltrfile, frmFilterFileDialog);
USEFORMNS("SMDBGSet.pas", Smdbgset, frmGridSetup);
USERES("SMDBGSet.dcr");
USEUNIT("SMDBStat.pas");
USERES("SMDBStat.dcr");
USEUNIT("SMHLLbl.pas");
USERES("SMHLLbl.dcr");
USEUNIT("SMLang.pas");
USERES("SMLang.dcr");
USEUNIT("SMLangR.pas");
USEUNIT("SMPanel.pas");
USEUNIT("SMScale.pas");
USERES("SMScale.dcr");
USEUNIT("SMScript.pas");
USERES("SMScript.dcr");
USEUNIT("SMSQLScript.pas");
USEUNIT("SMSummInfo.pas");
USERES("SMSummInfo.dcr");
USEUNIT("SMToolBar.pas");
USEUNIT("SMTray.pas");
USERES("SMTray.dcr");
USEUNIT("SMultiBtn.pas");
USEUNIT("SQLGen.pas");
USERES("SQLGen.dcr");
USEUNIT("TVNavigator.pas");
USERES("TVNavigator.dcr");
USEPACKAGE("Vclx50.bpi");
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
