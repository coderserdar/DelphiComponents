//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ExMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "XpBase"
#pragma link "XpDom"
#pragma link "XpParser"
#pragma link "XpDOM"
#pragma resource "*.dfm"
TMainForm *MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::btnBrowseClick(TObject *Sender)
{
  if (odFile->Execute())
    edtParse->Text = odFile->FileName;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::btnParseClick(TObject *Sender)
{
  if (edtParse->Text == "")
  {
    ShowMessage("You have not specified a file name.");
    return;
  }
  Screen->Cursor = crHourGlass;
  try
  {
    lbData->Items->BeginUpdate();
    try
    {
      lbData->Clear();
      try
      {
        if (Parser->ParseDataSource(edtParse->Text))
          ShowMessage("Parse succeeded!");
        else
          ShowMessage("Parse failed!");
      }
      catch (EXpParserError *E)
      {
        TVarRec ErrData[] = {E->Line, E->LinePos, E->FilePos, (AnsiString)E->Reason};
        ShowMessageFmt("Error on line: %d position: %d abs at: %d error msg: %s",
                       ErrData, ARRAYSIZE(ErrData) - 1);
      }
    }
    __finally
    {
      lbData->Items->EndUpdate();
    }
  }
  __finally
  {
    Screen->Cursor = crDefault;
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::ParserAttribute(TObject *oOwner, WideString sName,
      WideString sValue, bool bSpecified)
{
  lbData->Items->Add(Format("Attribute = %s:%s", ARRAYOFCONST(((AnsiString)sName,
                                                               (AnsiString)sValue))));
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::ParserCDATASection(TObject *oOwner,
      WideString sValue)
{
  lbData->Items->Add(Format("CDATA Section = [%s]",
                            ARRAYOFCONST(((AnsiString)sValue))));
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::ParserCharData(TObject *oOwner, WideString sValue)
{
  lbData->Items->Add(Format("CDATA Section = [%s]",
                            ARRAYOFCONST(((AnsiString)sValue))));
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::ParserComment(TObject *oOwner, WideString sValue)
{
  lbData->Items->Add("Comment = " + sValue);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::ParserDocTypeDecl(TObject *oOwner,
      WideString sDecl, WideString sId0, WideString sId1)
{
  lbData->Items->Add(Format("Doc type decl = %s:%s:%s",
                            ARRAYOFCONST(((AnsiString)sDecl,
                                          (AnsiString)sId0,
                                          (AnsiString)sId1))));
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::ParserEndDocument(TObject *Sender)
{
  lbData->Items->Add("End document");
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::ParserEndElement(TObject *oOwner,
      WideString sValue)
{
  lbData->Items->Add("End element = " + sValue);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::ParserNonXMLEntity(TObject *oOwner,
      WideString sEntityName, WideString sPublicId, WideString sSystemId,
      WideString sNotationName)
{
  lbData->Items->Add(Format("NON-XML = %s:%s:%s:%s",
                            ARRAYOFCONST(((AnsiString)sEntityName,
                                          (AnsiString)sPublicId,
                                          (AnsiString)sSystemId,
                                          (AnsiString)sNotationName))));
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::ParserProcessingInstruction(TObject *oOwner,
      WideString sName, WideString sValue)
{
  lbData->Items->Add(Format("Processing Instruction = %s:%s",
                            ARRAYOFCONST(((AnsiString)sName,
                                          (AnsiString)sValue))));
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::ParserStartDocument(TObject *Sender)
{
  lbData->Items->Add("Start document");
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::ParserStartElement(TObject *oOwner,
      WideString sValue)
{
  lbData->Items->Add("Start element = " + sValue);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::btnBrowse2Click(TObject *Sender)
{
  if (odFile->Execute())
    ObjModelEdit->Text = odFile->FileName;
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::btnDOMLoadClick(TObject *Sender)
{
  if (ObjModelEdit->Text == "")
  {
    ShowMessage("You have not specified a file name.");
    return;
  }

  Screen->Cursor = crHourGlass;
  try
  {
    memNodes->Lines->BeginUpdate();
    try
    {
      memNodes->Clear();
      try
      {
        if (DOM->LoadDataSource(ObjModelEdit->Text))
        {
          memNodes->Text = DOM->XmlDocument;
          NodesLabel->Caption = "";
          ShowMessage("Load succeeded!");
        }
        else
          ShowMessage("Load failed!");
      }
      catch(EXpParserError* X)
      {
        ShowMessageFmt("Error on line: %d position: %d abs at: %d error msg: %s",
                       ARRAYOFCONST((X->Line,
                                     X->LinePos,
                                     X->FilePos,
                                     (AnsiString)X->Reason)));
      }
    }
    __finally
    {
      memNodes->Lines->EndUpdate();
    }
  }
  __finally
  {
    Screen->Cursor = crDefault;
  }
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::btnSearchClick(TObject *Sender)
{
  TXpNodeList* oList;
  if ((edtSearch->Text != "") &&
      (DOM->Document->DocumentElement != NULL))
  {
    oList = DOM->Document->DocumentElement->SelectNodes(edtSearch->Text);
    try
    {
      NodesLabel->Caption = IntToStr(oList->Length);
      memNodes->Clear();
      memNodes->Text = oList->XmlDocument;
    }
    __finally
    {
      oList->Free();
    }
  }
}
//---------------------------------------------------------------------------
