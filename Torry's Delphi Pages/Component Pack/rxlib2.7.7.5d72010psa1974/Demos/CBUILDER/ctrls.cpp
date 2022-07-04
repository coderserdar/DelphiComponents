//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "ctrls.h"
//---------------------------------------------------------------------------
#pragma link "RXCtrls"
#pragma link "RXCombos"
#pragma link "RXSlider"
#pragma link "RXSwitch"
#pragma link "ToolEdit"
#pragma link "CurrEdit"
#pragma link "RXSpin"
#pragma link "RXSplit"
#pragma link "Animate"
#pragma link "RXClock"
#pragma link "RXDice"
#pragma link "Placemnt"
#pragma link "AniFile"
#pragma link "DateUtil"
#pragma link "PickDate"
#pragma link "RxNotify"
#pragma resource "*.dfm"
TControlsForm *ControlsForm;
//---------------------------------------------------------------------------
__fastcall TControlsForm::TControlsForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::SpinEdit3Change(TObject *Sender)
{
  AnimatedImage1->Interval = SpinEdit3->AsInteger;
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::ComboBox7Change(TObject *Sender)
{
  bool animated = AnimatedImage1->Active;
  AnimatedImage1->Active = false;
  switch (ComboBox7->ItemIndex) {
    case 0: {
      AnimatedImage1->Glyph = RunnerImage->Picture->Bitmap;
      AnimatedImage1->NumGlyphs = 12;
      break;
    }
    case 1: {
      AnimatedImage1->Glyph = FlagImage->Picture->Bitmap;
      AnimatedImage1->NumGlyphs = 4;
      break;
    }
    case 2: {
      AnimatedImage1->Glyph = SearchImage->Picture->Bitmap;
      AnimatedImage1->NumGlyphs = 12;
      break;
    }
    case 3: {
      AnimatedImage1->Glyph = BookImage->Picture->Bitmap;
      AnimatedImage1->NumGlyphs = 5;
    }
  }
  AnimatedImage1->Active = animated;
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::SpinEdit4Change(TObject *Sender)
{
  if (!AnimatedImage1->Active) {
    AnimatedImage1->GlyphNum = SpinEdit4->Value;
  }
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::AnimatedImage1StartStop(TObject *Sender)
{
  SpinEdit4->AsInteger = AnimatedImage1->GlyphNum;
  SpinEdit3->AsInteger = AnimatedImage1->Interval;
  if (AnimatedImage1->Active) {
    Button2->Caption = "Stop";
  }
  else {
    Button2->Caption = "Start";
  }
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::Button2Click(TObject *Sender)
{
  AnimatedImage1->Active = !(AnimatedImage1->Active);
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::Button6Click(TObject *Sender)
{
  TAnimatedCursorImage *Image;

  if (OpenDialog->Execute()) {
    OpenDialog->InitialDir = ExtractFilePath(OpenDialog->FileName);
    Image = new TAnimatedCursorImage;
    try {
      AnimatedImage1->Active = false;
      Image->LoadFromFile(OpenDialog->FileName);
      Image->AssignToBitmap(AnimatedImage1->Glyph, clFuchsia, false,
        (AnimatedImage1->Orientation == goVertical));
      ComboBox7->ItemIndex = -1;
      AnimatedImage1->Interval = Image->DefaultRate;
      SpinEdit3->Value = AnimatedImage1->Interval;
      AnimatedImage1->TransparentColor = clFuchsia;
      AnimatedImage1->Active = true;
    }
    catch(...) {
      delete Image;
      throw;
    }
    delete Image;
  }
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::Button5Click(TObject *Sender)
{
  if (TextListBox1->Items->Count == 3) {
    TextListBox1->Items->Insert(2, "Very Long Item Very Long Item Very Long Item");
    Button5->Caption = "Back";
  }
  else {
    TextListBox1->Items->Delete(2);
    Button5->Caption = "Show";
  }
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::ComboBox8Change(TObject *Sender)
{
  rxClock1->ShowMode = (TShowClock)ComboBox8->ItemIndex;
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::rxClock1Alarm(TObject *Sender)
{
  MessageDlg("Alarm!", mtInformation, TMsgDlgButtons() << mbOK, 0);
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::Button3Click(TObject *Sender)
{
  FontDialog1->Font = rxClock1->Font;
  if (FontDialog1->Execute()) {
    rxClock1->Font = FontDialog1->Font;
  }
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::CheckBox6Click(TObject *Sender)
{
  rxClock1->AlarmEnabled = CheckBox6->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::AlarmEditChange(TObject *Sender)
{
  if (Sender == SpinEdit5) {
    rxClock1->AlarmHour = (Byte)SpinEdit5->AsInteger;
  }
  else if (Sender == SpinEdit6) {
    rxClock1->AlarmMinute = (Byte)SpinEdit6->AsInteger;
  }
  else if (Sender == SpinEdit7) {
    rxClock1->AlarmSecond = (Byte)SpinEdit7->AsInteger;
  }
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::rxDice1Change(TObject *Sender)
{
  SpinEdit9->AsInteger = rxDice1->Value;
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::Button4Click(TObject *Sender)
{
  rxDice1->Rotate = !(rxDice1->Rotate);
  if (rxDice1->Rotate) {
    Button4->Caption = "Stop";
  }
  else {
    Button4->Caption = "Start";
  }
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::SpinEdit9Change(TObject *Sender)
{
  rxDice1->Value = (TRxDiceValue)SpinEdit9->AsInteger;
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::SpinEdit8Change(TObject *Sender)
{
  rxDice1->Interval = (Word)SpinEdit8->AsInteger;
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::CheckBox4Click(TObject *Sender)
{
  ColorComboBox1->DisplayNames = CheckBox4->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::ColorComboBox1Change(TObject *Sender)
{
  Shape1->Brush->Color = ColorComboBox1->ColorValue;
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::FontComboBox1Change(TObject *Sender)
{
  Label2->Font->Name = FontComboBox1->FontName;
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::ComboBox1Change(TObject *Sender)
{
  FontComboBox1->Device = (TFontDevice)ComboBox1->ItemIndex;
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::CheckBox1Click(TObject *Sender)
{
  FontComboBox1->TrueTypeOnly = CheckBox1->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::rxSlider1Change(TObject *Sender)
{
  int i;

  i = rxSlider1->Value;
  Label6->Caption = Format("Value: %d", OPENARRAY(TVarRec, (i)));
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::ComboBox2Change(TObject *Sender)
{
  if (rxSlider1->Orientation != (TSliderOrientation)ComboBox2->ItemIndex) {
    rxSlider1->Orientation = (TSliderOrientation)ComboBox2->ItemIndex;
  }
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::ComboBox3Change(TObject *Sender)
{
  if (ComboBox3->ItemIndex == 0) {
    rxSlider1->NumThumbStates = 2;
    rxSlider1->ImageHRuler = NULL;
    rxSlider1->ImageHThumb = NULL;
    rxSlider1->ImageVRuler = NULL;
    rxSlider1->ImageVThumb = NULL;
  }
  else if (ComboBox3->ItemIndex == 1) {
    rxSlider1->NumThumbStates = 1;
    rxSlider1->ImageHRuler = ADHRuler->Picture->Bitmap;
    rxSlider1->ImageHThumb = ADHThumb->Picture->Bitmap;
    rxSlider1->ImageVRuler = ADVRuler->Picture->Bitmap;
    rxSlider1->ImageVThumb = ADVThumb->Picture->Bitmap;
  }
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::CheckBox3Click(TObject *Sender)
{
  if (CheckBox3->Checked) {
    rxSlider1->Options = rxSlider1->Options << soShowPoints;
  }
  else {
    rxSlider1->Options = rxSlider1->Options >> soShowPoints;
  }
  rxSlider1->Refresh();
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::CheckBox2Click(TObject *Sender)
{
  if (CheckBox2->Checked) {
    rxSlider1->Options = rxSlider1->Options << soSmooth;
  }
  else {
    rxSlider1->Options = rxSlider1->Options >> soSmooth;
  }
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::ComboBox4Change(TObject *Sender)
{
  rxSwitch1->TextPosition = (TTextPos)ComboBox4->ItemIndex;
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::rxSwitch1Off(TObject *Sender)
{
  rxSwitch1->Caption = "Off";
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::rxSwitch1On(TObject *Sender)
{
  rxSwitch1->Caption = "On";
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::CheckBox5Click(TObject *Sender)
{
  if (CheckBox5->Checked) {
    rxSwitch1->BorderStyle = bsSingle;
  }
  else {
    rxSwitch1->BorderStyle = bsNone;
  }
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::ComboBox5Change(TObject *Sender)
{
  if (ComboBox5->ItemIndex == 0) {
    rxSwitch1->GlyphOn = NULL;
    rxSwitch1->GlyphOff = NULL;
  }
  else if (ComboBox5->ItemIndex == 1) {
    rxSwitch1->GlyphOn = SwOn->Picture->Bitmap;
    rxSwitch1->GlyphOff = SwOff->Picture->Bitmap;
  }
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::ComboEdit1ButtonClick(TObject *Sender)
{
  TDateTime ADate;

  ADate = StrToDateDef(ComboEdit1->Text, Now());
  if (PopupDate(ADate, ComboEdit1)) {
    ComboEdit1->Text = DateToStr(ADate);
  }
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::Edit1Change(TObject *Sender)
{
  CurrencyEdit1->DisplayFormat = Edit1->Text;
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::CheckBox7Click(TObject *Sender)
{
  CurrencyEdit1->FormatOnEditing = CheckBox7->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::ComboBox9Change(TObject *Sender)
{
  rxSpinEdit1->ValueType = (Rxspin::TValueType)ComboBox9->ItemIndex;
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::SpinEdit1Change(TObject *Sender)
{
  rxSpinEdit1->Increment = SpinEdit1->Value;
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::ColorComboBox2Change(TObject *Sender)
{
  rxLabel1->ShadowColor = ColorComboBox2->ColorValue;
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::ComboBox6Change(TObject *Sender)
{
  rxLabel1->ShadowPos = (TShadowPosition)ComboBox6->ItemIndex;
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::Button1Click(TObject *Sender)
{
  FontDialog1->Font = rxLabel1->Font;
  if (FontDialog1->Execute()) {
    rxLabel1->Font = FontDialog1->Font;
  }
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::FormClose(TObject *Sender, TCloseAction &Action)
{
  Action = caFree;
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::FormShow(TObject *Sender)
{
  FontComboBox1->FontName = "Ms Sans Serif";
  ComboBox1->ItemIndex = 0;
  ComboBox2->ItemIndex = 0;
  ComboBox3->ItemIndex = 0;
  ComboBox4->ItemIndex = 0;
  ComboBox5->ItemIndex = 0;
  ComboBox6->ItemIndex = 0;
  ComboBox7->ItemIndex = 0;
  ComboBox7Change(ComboBox7);
  ComboBox8->ItemIndex = 1;
  ComboBox9->ItemIndex = 0;
  DirectoryListBox1Change(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::SpinEdit2Change(TObject *Sender)
{
  rxLabel1->ShadowSize = (Byte)SpinEdit2->AsInteger;
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::FileListBox1Change(TObject *Sender)
{
  try {
    Memo1->Lines->LoadFromFile(FileListBox1->FileName);
  }
  catch(...) {
    Memo1->Clear();
  }
}
//---------------------------------------------------------------------------

void __fastcall TControlsForm::DirectoryListBox1Change(TObject *Sender)
{
  try {
    RxFolderMonitor->FolderName = DirectoryListBox1->Directory;
    RxFolderMonitor->Active = true;
  }
  catch(...) {
    RxFolderMonitor->Active = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TControlsForm::RxFolderMonitorChange(TObject *Sender)
{
  FileListBox1->Update();
}
//---------------------------------------------------------------------------
