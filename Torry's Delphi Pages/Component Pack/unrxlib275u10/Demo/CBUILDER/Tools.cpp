#include <vcl\vcl.h>
#pragma hdrstop

#include "Tools.h"
#include "mainunit.h"
//---------------------------------------------------------------------------
#pragma link "PicClip"
#pragma link "RXCalc"
#pragma link "Placemnt"
#pragma link "DualList"
#pragma link "ClipView"
#pragma link "ToolEdit"
#pragma link "CurrEdit"
#pragma link "RXCtrls"
#pragma link "Clipbrd"
#pragma link "ClipIcon"
#pragma link "MaxMin"
#pragma resource "*.dfm"
TToolsForm *ToolsForm;
//---------------------------------------------------------------------------
int Initialization();
static int Initializer = Initialization();

//---------------------------------------------------------------------------
int Initialization()
{
  TPersistentClass classes[] = { __classid(TButton) };

  // to copy button to clipboard
  RegisterClasses(classes, (sizeof(classes)/sizeof(classes[0])) - 1);
  return 0;
}
//---------------------------------------------------------------------------
void __fastcall TToolsForm::PopulateListBox1(void)
{
  ListBox1->Clear();
  ListBox1->Items->AddStrings(DualListDialog1->List2);
  ListBox1->ApplyState(cbChecked, false);
}
//---------------------------------------------------------------------------
__fastcall TToolsForm::TToolsForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TToolsForm::FormClose(TObject *Sender, TCloseAction &Action)
{
  Action = caFree;
}
//---------------------------------------------------------------------------
void __fastcall TToolsForm::FormCreate(TObject *Sender)
{
  PicImage->Picture = PicClip1->Picture;
}
//---------------------------------------------------------------------------
void __fastcall TToolsForm::FormShow(TObject *Sender)
{
  ComboBox2->ItemIndex = 0;
  PopulateListBox1();
  SpinEdit1Change(this);
  ComboBox5->ItemIndex = 2;
}
//---------------------------------------------------------------------------
void __fastcall TToolsForm::Button1Click(TObject *Sender)
{
  TPoint p;

  p = Point(RxDemoMainForm->Toolbar->Left + 50,
    RxDemoMainForm->Toolbar->Top + 6);
  p = RxDemoMainForm->ClientToScreen(p);
  RxDemoMainForm->ToolbarMenu->Popup(p.x, p.y);
}
//---------------------------------------------------------------------------
void __fastcall TToolsForm::ComboBox1Change(TObject *Sender)
{
  switch (ComboBox1->ItemIndex) {
    case 0: {
      Clipboard()->AsText = RxDemoMainForm->Caption;
      break;
    }
    case 1: {
      Clipboard()->Assign(Image5->Picture);
      break;
    }
    case 2: {
      CopyIconToClipboard(Application->Icon, clWindow);
      break;
    }
    case 3: {
      Clipboard()->SetComponent(Button1);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TToolsForm::ComboBox2Change(TObject *Sender)
{
  if (ComboBox2->ItemIndex >= 0) {
    TClipboardViewFormat v = ClipboardFormatToView(Clipboard()->Formats[
      (int)ComboBox2->Items->Objects[ComboBox2->ItemIndex]]);
    ClipboardViewer1->ViewFormat = v;
  }
}
//---------------------------------------------------------------------------
void __fastcall TToolsForm::Button2Click(TObject *Sender)
{
  DualListDialog1->Execute();
  PopulateListBox1();
}
//---------------------------------------------------------------------------
void __fastcall TToolsForm::CheckBox1Click(TObject *Sender)
{
  PicImage->Visible = CheckBox1->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TToolsForm::SpinEdit1Change(TObject *Sender)
{
  Image4->Picture->Bitmap = PicClip1->GraphicCell[(int)(SpinEdit1->AsInteger)];
}
//---------------------------------------------------------------------------
void __fastcall TToolsForm::Button3Click(TObject *Sender)
{
  if (ComboBox5->ItemIndex > 1) {
    SecretPanel1->Font->Size = 11;
  }
  else {
    SecretPanel1->Font->Size = 10;
  }
  SecretPanel1->Active = true;
}
//---------------------------------------------------------------------------
void __fastcall TToolsForm::ComboBox5Change(TObject *Sender)
{
  SecretPanel1->GlyphLayout = (TGlyphLayout)ComboBox5->ItemIndex;        
}
//---------------------------------------------------------------------------
void __fastcall TToolsForm::ClipboardViewer1Change(TObject *Sender)
{
  AnsiString s;

  ComboBox2->Items->Clear();
  ComboBox2->Items->AddObject("Auto", (TObject *)(-1));
  for (int i = 0; i < Clipboard()->FormatCount; i++) {
    s = ClipboardViewer1->ClipboardFormatNames[i];
    if (s != "") {
      ComboBox2->Items->AddObject(s, (TObject *)i);
    }
  }
  ComboBox2->ItemIndex = 0;
}
//---------------------------------------------------------------------------
void __fastcall TToolsForm::ComboEdit1ButtonClick(TObject *Sender)
{
  try {
    rxCalculator1->Value = StrToFloat(ComboEdit1->Text);
  }
  catch(...) {
    ComboEdit1->Text = "0";
  }
  if (rxCalculator1->Execute()) {
    ComboEdit1->Text = FloatToStrF(rxCalculator1->Value, ffGeneral,
      Max(2, rxCalculator1->Precision), 0);
  }
}
//---------------------------------------------------------------------------
void __fastcall TToolsForm::SecretPanel1DblClick(TObject *Sender)
{
  if (SecretPanel1->Active) {
    SecretPanel1->Active = false;
  }
}
//---------------------------------------------------------------------------

