/* ***** BEGIN LICENSE BLOCK *****                                            */
/* Version: MPL 1.1                                                           */
/*                                                                            */
/* The contents of this file are subject to the Mozilla Public License        */
/* version 1.1 (the "License"); you may not use this file except in           */
/* compliance with the License. You may obtain a copy of the License at       */
/* http://www.mozilla.org/MPL/                                                */
/*                                                                            */
/* Software distributed under the License is distributed on an "AS IS" basis, */
/* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   */
/* for the specific language governing rights and limitations under the       */
/* License.                                                                   */
/*                                                                            */
/* The Original Code is TurboPower Visual PlanIt                              */
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
USERES("ExFormatEdit.res");
USEFORM("ExFormatEdU1.cpp", frmPrnFormat);
USEFORM("EdElement.cpp", frmEditElement);
USEFORM("EdFormat.cpp", frmEditFormat);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TfrmPrnFormat), &frmPrnFormat);
                 Application->CreateForm(__classid(TfrmEditElement), &frmEditElement);
                 Application->CreateForm(__classid(TfrmEditFormat), &frmEditFormat);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
