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
#ifndef ExMdi1H
#define ExMdi1H
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include "EsTile.hpp"
#include "EsGrad.hpp"
#include <vcl\ExtCtrls.hpp>
#include <vcl\Buttons.hpp>
#include <vcl\Menus.hpp>
#include <vcl\Dialogs.hpp>
//---------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:	// IDE-managed Components
  TEsTile *EsTile1;
  TEsGradient *EsGradient1;
  TPanel *SpeedPanel;
  TSpeedButton *OpenBtn;
  TSpeedButton *SaveBtn;
  TSpeedButton *CutBtn;
  TSpeedButton *CopyBtn;
  TSpeedButton *PasteBtn;
  TSpeedButton *ExitBtn;
  TMainMenu *MainMenu1;
  TMenuItem *File1;
  TMenuItem *FileNewItem;
  TMenuItem *FileOpenItem;
  TMenuItem *FileCloseItem;
  TMenuItem *FileSaveItem;
  TMenuItem *FileSaveAsItem;
  TMenuItem *N1;
  TMenuItem *FileExitItem;
  TMenuItem *Edit1;
  TMenuItem *CutItem;
  TMenuItem *CopyItem;
  TMenuItem *PasteItem;
  TMenuItem *Window1;
  TMenuItem *WindowCascadeItem;
  TMenuItem *WindowTileItem;
  TMenuItem *WindowArrangeItem;
  TMenuItem *WindowMinimizeItem;
  TMenuItem *Help1;
  TMenuItem *HelpAboutItem;
  TMenuItem *Background1;
  TMenuItem *Tile1;
  TMenuItem *Gradient1;
  TOpenDialog *OpenDialog;
  void __fastcall FileNewItemClick(TObject *Sender);
  void __fastcall FileOpenItemClick(TObject *Sender);
  void __fastcall FileCloseItemClick(TObject *Sender);
  void __fastcall FileSaveItemClick(TObject *Sender);
  void __fastcall FileSaveAsItemClick(TObject *Sender);
  void __fastcall FileExitItemClick(TObject *Sender);
  void __fastcall CutItemClick(TObject *Sender);
  void __fastcall CopyItemClick(TObject *Sender);
  void __fastcall PasteItemClick(TObject *Sender);
  void __fastcall WindowCascadeItemClick(TObject *Sender);
  void __fastcall WindowTileItemClick(TObject *Sender);
  void __fastcall WindowArrangeItemClick(TObject *Sender);
  void __fastcall WindowMinimizeItemClick(TObject *Sender);
  void __fastcall Tile1Click(TObject *Sender);
  void __fastcall Gradient1Click(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormDestroy(TObject *Sender);
private:	// User declarations
  TFarProc FClientInstance;
  TFarProc FPrevClientProc;
  void CreateMDIChild(const String Name);
  void __fastcall ClientWndProc(TMessage& Message);
  void __fastcall UpdateMenuItems(TObject* Sender);
public:		// User declarations
  __fastcall TMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif
