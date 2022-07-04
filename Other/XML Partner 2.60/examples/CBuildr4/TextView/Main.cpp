//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Main.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "XpBase"
#pragma link "XpParser"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TForm1::btnParseClick(TObject *Sender)
{
  memo->Lines->Clear();
  if (edtFile->Text != "")
  {
    if (!Parser->ParseDataSource(edtFile->Text))
      memo->Lines->Assign(Parser->Errors);
  }
  else
    ShowMessage("You must select an XML document to parse.");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnOpenFileClick(TObject *Sender)
{
  fodXMLDoc->Execute();
  edtFile->Text = fodXMLDoc->FileName;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ParserAttribute(TObject *oOwner, WideString sName,
      WideString sValue, bool bSpecified)
{
  if (sName == (WideString)"FontName")
    memo->SelAttributes->Name = sValue;
  else if (sName == (WideString)"PointSize")
    memo->SelAttributes->Size = StrToInt(sValue);
  else if (sName == (WideString)"Color")
    memo->SelAttributes->Color = (TColor)StrToInt(sValue);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ParserStartElement(TObject *oOwner,
      WideString sValue)
{
  if (sValue == (WideString)"B")
    memo->SelAttributes->Style = memo->SelAttributes->Style << fsBold;
  if (sValue == (WideString)"I")
    memo->SelAttributes->Style = memo->SelAttributes->Style << fsItalic;
  if (sValue == (WideString)"U")
    memo->SelAttributes->Style = memo->SelAttributes->Style << fsUnderline;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ParserEndElement(TObject *oOwner,
      WideString sValue)
{
  if (sValue == (WideString)"B")
    memo->SelAttributes->Style = memo->SelAttributes->Style >> fsBold;
  if (sValue == (WideString)"I")
    memo->SelAttributes->Style = memo->SelAttributes->Style >> fsItalic;
  if (sValue == (WideString)"U")
    memo->SelAttributes->Style = memo->SelAttributes->Style >> fsUnderline;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ParserCharData(TObject *oOwner, WideString sValue)
{
  memo->SelLength = 0;
  memo->SelText = sValue;
}
//---------------------------------------------------------------------------
