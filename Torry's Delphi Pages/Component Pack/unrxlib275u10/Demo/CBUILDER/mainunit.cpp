//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "mainunit.h"
#include "About.h"
#include "LinkUnit.h"
#include "ctrls.h"
#include "DBAware.h"
#include "Tools.h"
#include "PageDemo.h"
//---------------------------------------------------------------------------
#pragma link "SpeedBar"
#pragma link "Placemnt"
#pragma link "DbPrgrss"
#pragma link "AppEvent"
#pragma link "RXShell"
#pragma link "DbExcpt"
#pragma link "RxMenus"
#pragma link "VCLUtils"
#pragma link "RxHints"
#pragma link "PicClip"
#pragma link "RxGrdCpt"
#pragma resource "*.dfm"
#pragma resource "BackGrnd.res"
//---------------------------------------------------------------------------
TRxDemoMainForm *RxDemoMainForm;
//---------------------------------------------------------------------------
int InitializationProc();
static int InitializerVar = InitializationProc();
//---------------------------------------------------------------------------
int InitializationProc()
{
  DbErrorIntercept();
  return 0;
}
//---------------------------------------------------------------------------
void __fastcall TRxDemoMainForm::UpdateToolbar()
{
  if (ColorToRGB(Toolbar->Color) != ColorToRGB(clBlack)) {
    Toolbar->Wallpaper->Bitmap->Handle = LoadBitmap((void *)HInstance,
      "BACKGROUND");
  }
  else {
    Toolbar->Wallpaper = NULL;
  }
  Toolbar->SetFontDefault();
}
//---------------------------------------------------------------------------
void __fastcall TRxDemoMainForm::ActiveFormChange(TObject *Sender)
{
  TSpeedItem *Btn;
  TPersistentClass AFormClass;

  Btn = NULL;
  CascadeItem->Enabled = MDIChildCount > 0;
  TileItem->Enabled = CascadeItem->Enabled;
  CascadeBtn->Enabled = CascadeItem->Enabled;
  TileBtn->Enabled = CascadeItem->Enabled;
  ArrangeBtn->Enabled = CascadeItem->Enabled;
  if (Screen->ActiveForm != NULL) {
    AFormClass = Screen->ActiveForm->ClassType();
    if (AFormClass == __classid(TControlsForm)) {
      Btn = View1Btn;
    }
    else if (AFormClass == __classid(TDBAwareForm)) {
      Btn = View2Btn;
    }
    else if (AFormClass == __classid(TToolsForm)) {
      Btn = View3Btn;
    }
    else {
      Btn = NULL;
    }
  }
  if (Btn != NULL) {
    Btn->Down = true;
  }
  else {
    View1Btn->Down = false;
    View2Btn->Down = false;
    View3Btn->Down = false;
  }
}
//---------------------------------------------------------------------------
__fastcall TRxDemoMainForm::TRxDemoMainForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TRxDemoMainForm::FormCreate(TObject *Sender)
{
  TPersistentClass classes[] = { __classid(TControlsForm),
                                 __classid(TToolsForm),
                                 __classid(TDBAwareForm)
                               };

  RegisterClasses(classes, (sizeof(classes)/sizeof(classes[0])) - 1);
  UpdateToolbar();
  SetHintStyle(hsRectangle, 2, false, taCenter);  
  Screen->OnActiveFormChange = ActiveFormChange;
  if (NewStyleControls) {
    TrayIcon->Active = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TRxDemoMainForm::FormDestroy(TObject *Sender)
{
  TrayIcon->Active = false;
  Screen->OnActiveFormChange = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TRxDemoMainForm::FormPlacement1RestorePlacement(TObject *Sender)
{
  RestoreMDIChildren(this, FormPlacement1->IniFile);
}
//---------------------------------------------------------------------------
void __fastcall TRxDemoMainForm::FormPlacement1SavePlacement(TObject *Sender)
{
  SaveMDIChildren(this, FormPlacement1->IniFile);
}
//---------------------------------------------------------------------------
void __fastcall TRxDemoMainForm::AppEventsMinimize(TObject *Sender)
{
  if (NewStyleControls) {
    ShowWindow(Application->Handle, SW_HIDE);
  }
}
//---------------------------------------------------------------------------
void __fastcall TRxDemoMainForm::AppEventsRestore(TObject *Sender)
{
  if (NewStyleControls) {
    ShowWindow(Application->Handle, SW_SHOW);
  }
}
//---------------------------------------------------------------------------
void __fastcall TRxDemoMainForm::CustomizeItemClick(TObject *Sender)
{
  Toolbar->Customize(0);
}
//---------------------------------------------------------------------------
void __fastcall TRxDemoMainForm::Exit(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------
void __fastcall TRxDemoMainForm::View(TObject *Sender)
{
  TPersistentClass AFormClass;

  switch (((TComponent *)Sender)->Tag) {
    case 1: {
      AFormClass = __classid(TControlsForm);
      break;
    }
    case 2: {
      AFormClass = __classid(TDBAwareForm);
      break;
    }
    case 3: {
      AFormClass = __classid(TToolsForm);
      break;
    }
    default: return;
  }
  Screen->Cursor = crHourGlass;
  try {
    FindShowForm(AFormClass, "");
    ActiveFormChange(NULL);
  }
  catch(...) {
    Screen->Cursor = crDefault;
    throw;
  }
  Screen->Cursor = crDefault;
}
//---------------------------------------------------------------------------
void __fastcall TRxDemoMainForm::WindowItemClick(TObject *Sender)
{
  switch (((TComponent *)Sender)->Tag) {
    case 1: {
      Cascade();
      break;
    }
    case 2: {
      Tile();
      break;
    }
    case 3: {
      ArrangeIcons();
      break;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TRxDemoMainForm::AboutItemClick(TObject *Sender)
{
  TrayIcon->Animated = false;
  try {
    ShowDialog(__classid(TAboutForm));
  }
  catch(...) {
    TrayIcon->Animated = true;
    throw;
  }
  TrayIcon->Animated = true;
}
//---------------------------------------------------------------------------
void __fastcall TRxDemoMainForm::SaveLayoutItemClick(TObject *Sender)
{
  TIniFile *AIniFile;

  AIniFile = new TIniFile(GetDefaultIniName());
  try {
    Toolbar->SaveLayout(AIniFile);
  }
  catch(...) {
    delete AIniFile;
    throw;
  }
  delete AIniFile;
}
//---------------------------------------------------------------------------
void __fastcall TRxDemoMainForm::RestoreLayoutItemClick(TObject *Sender)
{
  TIniFile *AIniFile;

  AIniFile = new TIniFile(GetDefaultIniName());
  try {
    Toolbar->RestoreLayout(AIniFile);
  }
  catch(...) {
    delete AIniFile;
    throw;
  }
  delete AIniFile;
}
//---------------------------------------------------------------------------
void __fastcall TRxDemoMainForm::TrayClick(TObject *Sender)
{
  Application->Restore();
  Application->BringToFront();
}
//---------------------------------------------------------------------------
void __fastcall TRxDemoMainForm::RXSiteItemClick(TObject *Sender)
{
  RxWebSite();
}
//---------------------------------------------------------------------------
void __fastcall TRxDemoMainForm::WizardBtnClick(TObject *Sender)
{
  ShowDialog(__classid(TClientAssistant));
}
//---------------------------------------------------------------------------
void __fastcall TRxDemoMainForm::AppEventsSettingsChanged(TObject *Sender)
{
  UpdateToolbar();
}
//---------------------------------------------------------------------------
void __fastcall TRxDemoMainForm::TrayMenuGetItemParams(TMenu *Sender,
        TMenuItem *Item, TMenuOwnerDrawState State, TFont *AFont, TColor &Color,
        TGraphic *&Graphic, int &NumGlyphs)
{
  AFont->Name = "Arial";
  switch (Item->Tag) {
    case 1:
    case 2:
    case 3:
    case 4: {
      Graphic = TrayImg->GraphicCell[Item->Tag - 1];
      break;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TRxDemoMainForm::ToolbarMenuGetItemParams(TMenu *Sender,
        TMenuItem *Item, TMenuOwnerDrawState State, TFont *AFont, TColor &Color,
        TGraphic *&Graphic, int &NumGlyphs)
{
  switch (Item->Tag) {
    case 1:
    case 2:
    case 3: {
      Graphic = ToolbarImg->GraphicCell[Item->Tag - 1];
      break;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TRxDemoMainForm::MainMenu1GetItemParams(TMenu *Sender,
        TMenuItem *Item, TMenuOwnerDrawState State, TFont *AFont, TColor &Color,
        TGraphic *&Graphic, int &NumGlyphs)
{
  if (Item == View1Item) {
    // controls
    Graphic = View1Btn->Glyph;
  }
  else if (Item == View3Item) {
    // data-aware
    Graphic = View2Btn->Glyph;
  }
  else if (Item == View4Item) {
    // tools
    Graphic = View3Btn->Glyph;
  }
}
//---------------------------------------------------------------------------
void __fastcall TRxDemoMainForm::TrayMenuDrawMargin(TMenu *Sender, TRect &Rect)
{
  TrayMenu->Canvas->Font->Name = "Courier New";
  TrayMenu->Canvas->Font->Style =
    TrayMenu->Canvas->Font->Style << fsBold;
  TrayMenu->Canvas->Font->Size = 20;
  TrayMenu->Canvas->Font->Color = clWhite;
  TrayMenu->Canvas->Font->Handle =
    CreateRotatedFont(TrayMenu->Canvas->Font, 90);
  TrayMenu->DefaultDrawMargin(Rect, clLime, clBlack);
  SetBkMode((HDC)TrayMenu->Canvas->Handle, TRANSPARENT);
  System::AnsiString rx_text = "RXLib";
  ExtTextOut((HDC)TrayMenu->Canvas->Handle, Rect.Left,
    Rect.Bottom - 5, ETO_CLIPPED, &RECT(Rect), rx_text.c_str(),
    rx_text.Length(), NULL);
}
//---------------------------------------------------------------------------