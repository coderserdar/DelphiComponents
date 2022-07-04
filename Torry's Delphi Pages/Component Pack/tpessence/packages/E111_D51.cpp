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
USERES("E111_D51.res");
USEUNIT("EsWebPE.pas");
USEUNIT("EsDir.pas");
USEFORMNS("EsLabel0.PAS", Eslabel0, EsLabelFrm);
USEFORMNS("EsLabel1.PAS", Eslabel1, SaveSchemeFrm);
USEUNIT("EsReg.pas");
USEFORMNS("EsAbout0.pas", Esabout0, AboutFrm);
USEUNIT("EsBase0.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("E111_R51.bpi");
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