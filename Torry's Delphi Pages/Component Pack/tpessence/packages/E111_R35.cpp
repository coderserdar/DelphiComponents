// ***** BEGIN LICENSE BLOCK *****
// * Version: MPL 1.1
// *
// * The contents of this file are subject to the Mozilla Public License Version
// * 1.1 (the "License"); you may not use this file except in compliance with
// * the License. You may obtain a copy of the License at
// * http://www.mozilla.org/MPL/
// *
// * Software distributed under the License is distributed on an "AS IS" basis,
// * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
// * for the specific language governing rights and limitations under the
// * License.
// *
// * The Original Code is TurboPower Essentials Vol I
// *
// * The Initial Developer of the Original Code is
// * TurboPower Software
// *
// * Portions created by the Initial Developer are Copyright (C) 1997-2002
// * the Initial Developer. All Rights Reserved.
// *
// * Contributor(s):
// *
// * ***** END LICENSE BLOCK *****

//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("E111_R35.res");
USEPACKAGE("VCL35.bpi");
USEUNIT("EsUtil.PAS");
USEUNIT("EsCal.pas");
USEUNIT("EsCalc.pas");
USEUNIT("EsClrCbx.pas");
USEUNIT("EsConst.pas");
USEUNIT("EsData.pas");
USEUNIT("EsEdCal.PAS");
USEUNIT("EsEdCalc.pas");
USEUNIT("EsEdPop.pas");
USEUNIT("EsGrad.pas");
USEUNIT("EsLabel.pas");
USEUNIT("EsMarque.pas");
USEUNIT("EsMnuBtn.pas");
USEUNIT("EsRollUp.pas");
USEUNIT("EsSrMgr.pas");
USEUNIT("EsTile.pas");
USEUNIT("Esbase.pas");
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