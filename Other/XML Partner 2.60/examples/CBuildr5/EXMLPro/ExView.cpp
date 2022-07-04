//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ExView.h"
#include "ExChildW.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "XpBase"
#pragma link "XpvFlBas"
#pragma link "XpvFlHTM"
#pragma link "XpvFlPrt"
#pragma link "XpvFlRTF"
#pragma link "XpvFlXML"
#pragma link "XpvXSLPr"
#pragma link "XpvXSLT"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
const ciTabNone = -1;
const ciTabRTF = 0;
const ciTabPrinter = 1;
const ciTabXML = 2;
const ciTabText = 3;
//---------------------------------------------------------------------------
TfrmPreview *frmPreview;
//---------------------------------------------------------------------------
__fastcall TfrmPreview::TfrmPreview(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
class TXpFilterDetail : public TObject
  // This class is used to store filter-specific data. One instance of this
  // class is associated with each entry in the filter combobox.
{
public:
  TXpFilterDisplayMethod Display;
    // The method used to display the rendered XML document.

  TXpFilterBase* Filter;
    // The filter component to be used to render the XML document.

  int TabIndex;
    // The notebook tab associated with the filter. If set to ciTabNone then
    // the filter does not have a tab.
};
//---------------------------------------------------------------------------
void __fastcall TfrmPreview::DisplayHTML(TXpFilterBase* aFilter)
{
  String EX_Error = "Unable to start web browser. Make sure you have it properly set up on your system.";

  String sFile;

  if (FFilename == "")
    FFilename = "temp";

  sFile = XpMakeFilePath(ExtractFilePath(Application->ExeName),
                         ChangeFileExt(ExtractFileName(FFilename),".htm"));
  dynamic_cast<TXpFilterHTML*>(aFilter)->OutputEncoding = FDOM->OutCharSet;
  aFilter->SaveToFile(sFile);
  char* tmpFilename = sFile.c_str();
  if (int(ShellExecute(0, "open", tmpFilename, "", "", SW_SHOWNORMAL)) <= 32)
    ShowMessage(EX_Error);
}
//---------------------------------------------------------------------------
void __fastcall TfrmPreview::DisplayPrinter(TXpFilterBase* aFilter)
{
  FPrinterFilter = dynamic_cast<TXpFilterPrint*>(aFilter);
  UpdatePrinterUI();
}
//---------------------------------------------------------------------------
void __fastcall TfrmPreview::DisplayRTF(TXpFilterBase* aFilter)
{
  String sFile;

  sFile = XpMakeFilePath(ExtractFilePath(Application->ExeName),
                         ChangeFileExt(ExtractFileName(FFilename),".rtf"));
  aFilter->SaveToFile(sFile);
  RichEdit->Clear();
  RichEdit->Lines->LoadFromFile(sFile);
}
//---------------------------------------------------------------------------
void __fastcall TfrmPreview::DisplayText(TXpFilterBase* aFilter)
{
  String sFile;

  sFile = XpMakeFilePath(ExtractFilePath(Application->ExeName),
                         ChangeFileExt(ExtractFileName(FFilename),".txt"));
  dynamic_cast<TXpFilterText*>(aFilter)->OutputEncoding = FDOM->OutCharSet;
  aFilter->SaveToFile(sFile);
  memText->Lines->Clear();
  memText->Lines->LoadFromFile(sFile);
}
//---------------------------------------------------------------------------
void __fastcall TfrmPreview::DisplayXML(TXpFilterBase* aFilter)
{
  String sFile;

  sFile = XpMakeFilePath(ExtractFilePath(Application->ExeName),
                         ChangeFileExt(ExtractFileName(FFilename),".xml"));
  dynamic_cast<TXpFilterXML*>(aFilter)->OutputEncoding = FDOM->OutCharSet;
  aFilter->SaveToFile(sFile);
  memXML->Lines->Clear();
  memXML->Lines->LoadFromFile(sFile);
}
//---------------------------------------------------------------------------
WideString __fastcall TfrmPreview::GetStyleSheet(void)
{
  bool bIsBEorLE;
  WideString sXML;
  WideString sXMLBOM;

  sXML = dynamic_cast<TXMLChild*>(cboStylesheet->Items->Objects[cboStylesheet->ItemIndex])->XMLData;
  bIsBEorLE = ( ((sXML[1] == 254) & (sXML[2] == 257)) |
                ((sXML[1] == 257) & (sXML[2] == 254)) );
  // Marked as big or little endian?
  if (!bIsBEorLE)
  {
    // No. Must force it to be recognized as little endian.
    sXMLBOM.SetLength(1);
    sXMLBOM[1] = 65279;  // $FEFF;
    return(sXMLBOM + sXML);
  }
  else
    return(sXML);
}
//---------------------------------------------------------------------------
void __fastcall TfrmPreview::RestoreStylesheetSelection(void)
{
  int wInx;

  if (FCurSheet != "")
  {
    wInx = cboStylesheet->Items->IndexOf(FCurSheet);
    // Is the stylesheet still in the list?
    if (wInx >= 0)
      // Yes. Position combobox to that stylesheet.
      cboStylesheet->ItemIndex = wInx;
    else
    {
      // No. Clear out the combobobox & the XSL Processor's style data.
      cboStylesheet->Text = "";
      xslprocessor->StyleData = "";
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmPreview::SaveStylesheetSelection(void)
{
  if (cboStylesheet->ItemIndex >= 0)
    FCurSheet = cboStylesheet->Items->Strings[cboStylesheet->ItemIndex];
  else
    FCurSheet = "";
}
//---------------------------------------------------------------------------
void __fastcall TfrmPreview::ShowTab(void)
{
  TXpFilterDetail* aFilterDetail;

  aFilterDetail = dynamic_cast<TXpFilterDetail*>(cboFilter->Items->Objects[cboFilter->ItemIndex]);

  if (FLastPage == aFilterDetail->TabIndex)
    return;

  // Hide the previously shown page.
  if (FLastPage != ciTabNone)
    pgPreview->Pages[FLastPage]->TabVisible = false;

  FLastPage = aFilterDetail->TabIndex;
  if (FLastPage != ciTabNone)
  {
    pgPreview->ActivePage = pgPreview->Pages[FLastPage];
    pgPreview->Pages[FLastPage]->TabVisible = true;
    pgPreview->Pages[FLastPage]->SetFocus();
    if (FLastPage == ciTabPrinter)
      UpdatePrinterUI();
  }

}
//---------------------------------------------------------------------------
void __fastcall TfrmPreview::UpdatePrinterUI(void)
{
  bool bFirstPage;
  bool bHasPages;
  bool bLastPage;

  if (FPrinterFilter != NULL)
  {
    pnlPage->Caption = Format("Page %d of %d",
                              ARRAYOFCONST((FPrinterFilter->PageCurrent,
                                            FPrinterFilter->PageCount)));
    bHasPages = (FPrinterFilter->PageCount > 0);
  }
  else
    bHasPages = false;

  if (bHasPages)
  {
    PrinterVScroll->Position = 0;
    PrinterHScroll->Position = 0;
    bFirstPage = (FPrinterFilter->PageCurrent == 1);
    bLastPage = (FPrinterFilter->PageCurrent == FPrinterFilter->PageCount);
    pbFirstPage->Enabled = (!bFirstPage);
    pbPrevPage->Enabled = (!bFirstPage);
    pbNextPage->Enabled = (!bLastPage);
    pbLastPage->Enabled = (!bLastPage);
  }
  else
  {
    pbFirstPage->Enabled = False;
    pbPrevPage->Enabled = False;
    pbNextPage->Enabled = False;
    pbLastPage->Enabled = False;
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
void __fastcall TfrmPreview::cboFilterChange(TObject *Sender)
{
  // Display the tab associated with the selected filter.
  ShowTab();
}
//---------------------------------------------------------------------------

void __fastcall TfrmPreview::pbBrowseClick(TObject *Sender)
{
  if (dlgOpen->Execute())
  {
    cboStylesheet->ItemIndex = -1;
    cboStylesheet->Text = dlgOpen->FileName;
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmPreview::pbRenderClick(TObject *Sender)
{
  TXpFilterDetail *aFilterDetail;
  TXpFilterBase *aFilter;
  TCursor savCursor;

  // Get the filter.
  aFilterDetail = dynamic_cast<TXpFilterDetail*>(cboFilter->Items->Objects[cboFilter->ItemIndex]);
  aFilter = dynamic_cast<TXpFilterBase*>(aFilterDetail->Filter);
  xslprocessor->Filter = aFilter;
  xslprocessor->XmlObjModel = FDOM;
  // Did the user specify an external stylesheet?
  if (cboStylesheet->ItemIndex < 0)
  {
    // Yes. Get the name of the file.
    if (cboStylesheet->Text != "")
      xslprocessor->StyleURL = cboStylesheet->Text;
  }
  else
    // No. Obtain the data for the stylesheet.
    xslprocessor->StyleData = GetStyleSheet();

  savCursor = Screen->Cursor;
  Screen->Cursor = crHourGlass;
  try
  {
    if (xslprocessor->ApplyStyle())
      // Display the file using the method specific to the filter.
      aFilterDetail->Display(aFilter);
    else
    {
      Screen->Cursor = savCursor;
      MessageDlg("Render failed!", mtError, TMsgDlgButtons() << mbOK, 0);
    }
  }
  __finally
  {
    Screen->Cursor = savCursor;
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmPreview::filtPrinterResize(TObject *Sender)
{
  PrinterVScroll->Max = filtPrinter->VSlideFactor;
  PrinterVScroll->Position = filtPrinter->VSlidePosition;

  PrinterHScroll->Max = filtPrinter->HSlideFactor;
  PrinterHScroll->Position = filtPrinter->HSlidePosition;
}
//---------------------------------------------------------------------------

void __fastcall TfrmPreview::PrinterVScrollChange(TObject *Sender)
{
  FPrinterFilter->VSlidePosition = PrinterVScroll->Position;
}
//---------------------------------------------------------------------------

void __fastcall TfrmPreview::PrinterHScrollChange(TObject *Sender)
{
  FPrinterFilter->HSlidePosition = PrinterHScroll->Position;
}
//---------------------------------------------------------------------------

void __fastcall TfrmPreview::pbFirstPageClick(TObject *Sender)
{
  FPrinterFilter->PageCurrent = 1;
  UpdatePrinterUI();
}
//---------------------------------------------------------------------------

void __fastcall TfrmPreview::pbPrevPageClick(TObject *Sender)
{
  if (FPrinterFilter->PageCurrent <= 1)
  {
    MessageBeep(MB_OK);
    return;
  }

  FPrinterFilter->PageCurrent = FPrinterFilter->PageCurrent - 1;
  UpdatePrinterUI();

}
//---------------------------------------------------------------------------

void __fastcall TfrmPreview::pbNextPageClick(TObject *Sender)
{
  if (FPrinterFilter->PageCurrent >= FPrinterFilter->PageCount)
  {
    MessageBeep(MB_OK);
    return;
  }

  FPrinterFilter->PageCurrent = FPrinterFilter->PageCurrent + 1;
  UpdatePrinterUI();

}
//---------------------------------------------------------------------------

void __fastcall TfrmPreview::pbLastPageClick(TObject *Sender)
{
  FPrinterFilter->PageCurrent = FPrinterFilter->PageCount;
  UpdatePrinterUI();
}
//---------------------------------------------------------------------------
void __fastcall TfrmPreview::AddStylesheet(const String sName, TForm* oChildWin)
{
  SaveStylesheetSelection();
  try
  {
    cboStylesheet->Items->AddObject(sName, oChildWin);
  }
  __finally
  {
    RestoreStylesheetSelection();
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmPreview::CopyToClipboard(void)
{
  if (pgPreview->ActivePage == tabRTF)
    RichEdit->CopyToClipboard();
  else if (pgPreview->ActivePage == tabXML)
    memXML->CopyToClipboard();
  else if (pgPreview->ActivePage == tabText)
    memText->CopyToClipboard();
}
//---------------------------------------------------------------------------
void __fastcall TfrmPreview::RemoveStylesheet(const String sName)
{
  int wInx;

  SaveStylesheetSelection();
  try
  {
    wInx = cboStylesheet->Items->IndexOf(sName);
    if (wInx >= 0)
      cboStylesheet->Items->Delete(wInx);
  }
  __finally
  {
    RestoreStylesheetSelection();
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmPreview::RenameStylesheet(const String sOldName, const String sNewName)
{
  int wInx;

  SaveStylesheetSelection();
  try
  {
    if (FCurSheet == sOldName)
      FCurSheet = sNewName;
    wInx = cboStylesheet->Items->IndexOf(sOldName);
    if (wInx >= 0)
      cboStylesheet->Items->Strings[wInx] = sNewName;
  }
  __finally
  {
    RestoreStylesheetSelection();
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmPreview::FormShow(TObject *Sender)
{
  TXpFilterBase* aFilter;
  TXpFilterDetail* aFilterDetail;
  int Inx;

  FLastPage = ciTabNone;
  // Find the filters on the form & populate the Filter combobox.
  for (Inx = 0; Inx < ComponentCount; Inx++)
  {
    aFilter = dynamic_cast<TXpFilterBase*>(Components[Inx]);
    if (aFilter != NULL)
    {
      aFilterDetail = new TXpFilterDetail();
      aFilterDetail->Filter = aFilter;
      if (Components[Inx]->ClassType() == __classid(TXpFilterHTML))
      {
        aFilterDetail->Display = DisplayHTML;
        aFilterDetail->TabIndex = ciTabNone;
      }
      else if (Components[Inx]->ClassType() == __classid(TXpFilterPrint))
      {
        aFilterDetail->Display = DisplayPrinter;
        aFilterDetail->TabIndex = ciTabPrinter;
      }
      else if (Components[Inx]->ClassType() == __classid(TXpFilterRTF))
      {
        aFilterDetail->Display = DisplayRTF;
        aFilterDetail->TabIndex = ciTabRTF;
      }
      else if (Components[Inx]->ClassType() == __classid(TXpFilterXML))
      {
        aFilterDetail->Display = DisplayXML;
        aFilterDetail->TabIndex = ciTabXML;
      }
      else
      {
        aFilterDetail->Display = DisplayText;
        aFilterDetail->TabIndex = ciTabText;
      }
      cboFilter->Items->AddObject(aFilter->DisplayName, aFilterDetail);
    }
  }

  // Position to the first filter.
  if (cboFilter->Items->Count > 0)
    cboFilter->ItemIndex = 0;
  else
  {
    cboFilter->Items->Add("<None available>");
    cboFilter->ItemIndex = 0;
    cboFilter->Enabled = false;
  }

  // Position to the first stylesheet.
  if (cboStylesheet->Items->Count > 0)
    cboStylesheet->ItemIndex = 0;

  // Set to the appropriate notebook tab.
  ShowTab();
}
//---------------------------------------------------------------------------

void __fastcall TfrmPreview::FormCreate(TObject *Sender)
{
  int Inx;

  // Make sure the tab pages are invisible.
  for (Inx = 0; Inx < pgPreview->PageCount; Inx++)
    pgPreview->Pages[Inx]->TabVisible = false;

}
//---------------------------------------------------------------------------

void __fastcall TfrmPreview::FormClose(TObject *Sender,
      TCloseAction &Action)
{
  int Inx;

  // Free the filter detail objects associated with the items in the combobox.
  for (Inx = cboFilter->Items->Count - 1; Inx >= 0; Inx--)
    cboFilter->Items->Objects[Inx]->Free();

}
//---------------------------------------------------------------------------

