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
#include <vcl\vcl.h>
#pragma hdrstop

#include "ExMdi1.h"
#include "ExMdi2.h"
//---------------------------------------------------------------------------
#pragma link "EsTile"
#pragma link "EsGrad"
#pragma resource "*.dfm"
TMainForm *MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void TMainForm::CreateMDIChild(const String Name)
{
  // create a new MDI child window
  TMDIChild* Child = new TMDIChild(Application);
  Child->Caption = Name;
}

void __fastcall TMainForm::ClientWndProc(TMessage& Message)
{
  TRect R;
    switch (Message.Msg) {
      case WM_ERASEBKGND : {
          // erase the background
          CallWindowProc((FARPROC)FPrevClientProc, ClientHandle,
            Message.Msg, Message.WParam, Message.LParam);

          // get the MDI client's rectangle
          GetWindowRect(ClientHandle, (RECT*)&R);

          // Delphi and the Essentials PaintTo methods think that
          // an HDC is an int so we'll use an int here.
          int hDC = (int)Message.WParam;
          if (Tile1->Checked) // draw the tile
            EsTile1->PaintTo(hDC, R);

          if (Gradient1->Checked) // draw the gradient
            EsGradient1->PaintTo(hDC, R);

          Message.Result = 1;
          break;
        }

      case WM_VSCROLL :
      case WM_HSCROLL : {
          Message.Result = CallWindowProc((FARPROC)FPrevClientProc,
            ClientHandle, Message.Msg, Message.WParam, Message.LParam);
          InvalidateRect(ClientHandle, NULL, true);
          break;
      }
      default :
        Message.Result = CallWindowProc((FARPROC)FPrevClientProc,
          ClientHandle, Message.Msg, Message.WParam, Message.LParam);
  }
}

void __fastcall TMainForm::FileNewItemClick(TObject *Sender)
{
  CreateMDIChild("NONAME" + String((int)MDIChildCount + 1));
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FileOpenItemClick(TObject *Sender)
{
  if (OpenDialog->Execute())
    CreateMDIChild(OpenDialog->FileName);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FileCloseItemClick(TObject *Sender)
{
  if (ActiveMDIChild)
    ActiveMDIChild->Close();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FileSaveItemClick(TObject *Sender)
{
  // save current file (ActiveMDIChild points to the window)
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FileSaveAsItemClick(TObject *Sender)
{
  // save current file under new name
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FileExitItemClick(TObject *Sender)
{
  Close();  
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::CutItemClick(TObject *Sender)
{
  // cut selection to clipboard
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::CopyItemClick(TObject *Sender)
{
  // copy selection to clipboard
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::PasteItemClick(TObject *Sender)
{
  // paste selection from clipboard
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::WindowCascadeItemClick(TObject *Sender)
{
  Cascade();  
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::WindowTileItemClick(TObject *Sender)
{
  Tile();  
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::WindowArrangeItemClick(TObject *Sender)
{
  ArrangeIcons();  
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::WindowMinimizeItemClick(TObject *Sender)
{
  // Must be done backwards through the MDIChildren array
  for (int i=MDIChildCount - 1;i>0;i++)
    MDIChildren[i]->WindowState = wsMinimized;
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::UpdateMenuItems(TObject* Sender)
{
  FileCloseItem->Enabled = MDIChildCount > 0;
  FileSaveItem->Enabled = MDIChildCount > 0;
  FileSaveAsItem->Enabled = MDIChildCount > 0;
  CutItem->Enabled = MDIChildCount > 0;
  CopyItem->Enabled = MDIChildCount > 0;
  PasteItem->Enabled = MDIChildCount > 0;
  SaveBtn->Enabled = MDIChildCount > 0;
  CutBtn->Enabled = MDIChildCount > 0;
  CopyBtn->Enabled = MDIChildCount > 0;
  PasteBtn->Enabled = MDIChildCount > 0;
  WindowCascadeItem->Enabled = MDIChildCount > 0;
  WindowTileItem->Enabled = MDIChildCount > 0;
  WindowArrangeItem->Enabled = MDIChildCount > 0;
  WindowMinimizeItem->Enabled = MDIChildCount > 0;
}

void __fastcall TMainForm::Tile1Click(TObject *Sender)
{
  Tile1->Checked = True;
  Gradient1->Checked = False;
  InvalidateRect(ClientHandle, NULL, true);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::Gradient1Click(TObject *Sender)
{
  Tile1->Checked = False;
  Gradient1->Checked = True;
  InvalidateRect(ClientHandle, NULL, true);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormCreate(TObject *Sender)
{
  Screen->OnActiveFormChange = UpdateMenuItems;

  // hook the window procedure
  FClientInstance = MakeObjectInstance(ClientWndProc);
  FPrevClientProc = reinterpret_cast<void*>(GetWindowLong(ClientHandle, GWL_WNDPROC));
  SetWindowLong(ClientHandle, GWL_WNDPROC, reinterpret_cast<long>(FClientInstance));
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormDestroy(TObject *Sender)
{
  Screen->OnActiveFormChange = 0;

  // restore previous window proc
  SetWindowLong(ClientHandle, GWL_WNDPROC, reinterpret_cast<long>(FPrevClientProc));
  FreeObjectInstance(FClientInstance);
}
//---------------------------------------------------------------------------