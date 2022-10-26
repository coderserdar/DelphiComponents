//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "LinkUnit.h"
#include "mainunit.h"
//---------------------------------------------------------------------------
void __fastcall PopupToolbarMenu()
{
  RxDemoMainForm->ToolbarMenu->Popup(RxDemoMainForm->Toolbar->Left,
    RxDemoMainForm->Toolbar->Top);
}

void __fastcall RxWebSite()
{
  ShellExecute(Application->Handle, NULL, "http://www.rxlib.com", NULL,
    NULL, SW_SHOWNOACTIVATE);
}
