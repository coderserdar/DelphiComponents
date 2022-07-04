/*********************************************************/
/* XMLPartner: ExMain.CPP 2.55                           */
/* Copyright (c) TurboPower Software Co 2002             */
/* All rights reserved.                                  */
/*********************************************************/
/* XMLPartner: XML Editor Child window                   */
/*********************************************************/
//---------------------------------------------------------------------------
#include <vcl.h>
#include <vcl/wstring.h>
#include <clipbrd.hpp>
#include <alloc.h>
#ifdef DCC4OrLater
#include <ComObj.hpp>
#endif
#pragma hdrstop

#ifdef XPDPRO
#include "ExView.h"
#endif
#include "ExChildW.h"
#include "ExProcIn.h"
#include "ExCommnt.h"
#include "ExText.h"
#include "ExElemnt.h"
#include "ExAttr.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "XpBase"
#pragma link "XpDom"
#pragma link "XpDOM"
#pragma resource "*.dfm"
TXMLChild *XMLChild;
//---------------------------------------------------------------------------
__fastcall TXMLChild::TXMLChild(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TXMLChild::CopyToClipboard(bool bIncludeChildren)
{
  HGLOBAL aHandle;
  int iLen;
  TXpNode* oNode;
  Pointer pBuffer;
  WideString sXMLText;

  if (PageControl->ActivePage == SourceTab)
    EditView->CopyToClipboard();
#ifdef XPDPRO
  else if (PageControl->ActivePage == FPreviewTab)
    FPreviewForm->CopyToClipboard();
#endif
  else
  {
    // The Clipboard methods do not support Unicode. So we must obtain a buffer
    // via global memory
    // (see http://msdn.microsoft.com/library/psdk/winbase/clipbrd_5nfo.htm),
    // manually move the bytes, and then associate the buffer with the
    // clipboard.

    // First, obtain the set of text describing the nodes to be copied.
    if (TreeView->Selected == NULL)
      sXMLText = FXml->XmlDocument;
    else if (bIncludeChildren)
      sXMLText = ((TXpNode*)(TreeView->Selected->Data))->XmlDocument;
    else
    {
      oNode = ((TXpNode*)(TreeView->Selected->Data))->CloneNode(false);
      sXMLText = oNode->XmlDocument;
      oNode->Free();
    }

    // Next, obtain a global memory buffer and move the text into the buffer.
    iLen = sXMLText.Length();
    aHandle = GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, 2 * iLen + 2);
    try
    {
      // Convert the handle into a pointer so that we may load the memory with
      // the XML text.
      pBuffer = GlobalLock(aHandle);
      try
      {
        // Load the buffer.
        memcpy(pBuffer, sXMLText.c_bstr(), (iLen * 2) + 2);

        // Associate the buffer with the clipboard.
        Clipboard()->SetAsHandle(CF_UNICODETEXT, (int)aHandle);
      }
      __finally
      {
        GlobalUnlock(aHandle);
      }
    }
    catch(...)
    {
      GlobalFree(aHandle);
      throw;
    }
  }
}
//---------------------------------------------------------------------------
TXpElement* __fastcall TXMLChild::CreateNewNode(String sElemName)
{
  FChanged = true;
  return(((TXpElement*)(TreeView->Selected->Data))->CreateChildElement(sElemName));
}
//---------------------------------------------------------------------------
void __fastcall TXMLChild::CutToClipboard(void)
{
  if (PageControl->ActivePage == SourceTab)
    EditView->CutToClipboard();
  else if (TreeView->Selected != NULL)
  {
    CopyToClipboard(true);
    mnuDeleteNodeClick(this);
  }
}
//---------------------------------------------------------------------------
void __fastcall TXMLChild::EditNodeDialog(TXpNode* oNode)
{
  TPIForm* oPIForm;

  switch(oNode->NodeType)
  {
    case PROCESSING_INSTRUCTION_NODE :
      oPIForm = new TPIForm(this);
      oPIForm->PINameEdit->Text = oNode->NodeName;
      oPIForm->PIValueEdit->Text = oNode->NodeValue;
      if (oPIForm->ShowModal() == mrOk)
      {
        oNode->NodeName = oPIForm->PINameEdit->Text;
        oNode->NodeValue = oPIForm->PIValueEdit->Text;
        FChanged = true;
      }
      oPIForm->Free();
      break;
    case COMMENT_NODE :
      CommentForm->CommentEdit->Text = oNode->NodeValue;
      if (CommentForm->ShowModal() == mrOk)
      {
        oNode->NodeValue = CommentForm->CommentEdit->Text;
        FChanged = true;
      }
      break;
    case TEXT_NODE :
    case CDATA_SECTION_NODE :
      TextForm->TextEdit->Text = oNode->NodeValue;
      if (TextForm->ShowModal() == mrOk)
      {
        oNode->NodeValue = TextForm->TextEdit->Text;
        FChanged = true;
      }
      break;
    default :
      break;
  }
}
//---------------------------------------------------------------------------
TStringList* __fastcall TXMLChild::GetErrors(void)
{
  return(XmlObjModelA->Errors);
}
//---------------------------------------------------------------------------
bool __fastcall TXMLChild::GetFormattedOutput(void)
{
  return(XmlObjModelA->FormattedOutput);
}
//---------------------------------------------------------------------------
String __fastcall TXMLChild::GetListViewText(TXpNode* oNode)
{

  int i;
  TXpAttribute* oAttr;
  String Result;

  Result = "";
  switch(oNode->NodeType)
  {
    case ELEMENT_NODE :
      if (oNode->HasAttributes())
        for (i = 0; i < oNode->Attributes->Length; i++)
        {
          oAttr = (TXpAttribute*)(oNode->Attributes->Item(i));
          Result = Result + "(" + oAttr->NodeName + "=""" + oAttr->NodeValue + """) ";
        }
      break;
    case PROCESSING_INSTRUCTION_NODE :
    case COMMENT_NODE :
    case TEXT_NODE :
    case CDATA_SECTION_NODE :
      Result = oNode->NodeValue;
      break;
    default :
      break;
  }
  return(Result);
}
//---------------------------------------------------------------------------
bool __fastcall TXMLChild::GetNormalize(void)
{
  return(XmlObjModelA->NormalizeData);
}
//---------------------------------------------------------------------------
int __fastcall TXMLChild::GetTreeNodeImageIndex(TXpNode* oNode)
{
  int Result = -1;
  switch(oNode->NodeType)
  {
    case TEXT_NODE :
      Result = 2;
      break;
    case CDATA_SECTION_NODE :
      Result = 3;
      break;
    case PROCESSING_INSTRUCTION_NODE :
      Result = 4;
      break;
    case COMMENT_NODE :
      Result = 5;
      break;
    default :
      if (oNode->HasAttributes())
        Result = 1;
      else
        Result = 0;
  }
  return(Result);
}
//---------------------------------------------------------------------------
TTreeNode* __fastcall TXMLChild::GetTreeNodeForNode(TXpNode* oNode)
{
  TTreeNode* oTreeNode;
  TTreeNode* Result = NULL;

  oTreeNode = TreeView->Items->GetFirstNode();
  while (oTreeNode != NULL)
  {
    if (oTreeNode->Data == oNode)
    {
      Result = oTreeNode;
      oTreeNode = NULL;
    }
    else
      oTreeNode = oTreeNode->getNextSibling();
  }
  return(Result);
}
//---------------------------------------------------------------------------
String __fastcall TXMLChild::GetTreeNodeText(TXpNode* oNode)
{
  String Result = "[UNKNOWN]";
  switch(oNode->NodeType)
  {
    case TEXT_NODE :
      Result = "[TEXT]";
      break;
    case CDATA_SECTION_NODE :
      Result = "[CDATA]";
      break;
    case COMMENT_NODE :
      Result = "[COMMENT]";
      break;
    default :
      Result = oNode->NodeName;
  }
  return(Result);
}
//---------------------------------------------------------------------------
bool __fastcall TXMLChild::LoadDataSource(String sFile, String sUserId,
                                          String sPassword)
{
  TCursor oSavCursor;

  bool Result = false;

  if (FileExists(sFile))
  {
    FFileName = LowerCase(sFile);
    Caption = FFileName;
  }
  else
  {
    FFileName = "";
    Caption = sFile;
  }

  try
  {
    XmlObjModelA->UserName = sUserId;
    XmlObjModelA->Password = sPassword;
    SetCapture(Handle);
    oSavCursor = Screen->Cursor;
    Screen->Cursor = crHourGlass;
    try
    {
      Result = XmlObjModelA->LoadDataSource(sFile);
      FXml = XmlObjModelA;
#ifdef XPDPRO
      IntegratePreviewPage();
#endif
      TreeView->Items->Clear();
      UpdateTreeFromObjModel(NULL);
      GetTreeNodeForNode(FXml->Document->DocumentElement)->Expand(false);
    }
    __finally
    {
      Screen->Cursor = oSavCursor;
      ReleaseCapture();
    }
  }
  catch (EXpParserError &X)
  {
    if (MessageDlg("An error occurred while parsing the document: " +
                   X.Message + char(13) + char(10) + char(13) + char(10) +
                   "Would you like to load the document into the " +
                   "source code editor?",
                  mtError, TMsgDlgButtons() << mbOK << mbNo, 0) == mrYes)
    {
      PageControl->ActivePage = SourceTab;
      EditView->Lines->LoadFromFile(sFile);
      StatusBar->SimpleText = "Error on line: " + IntToStr(X.Line) +
                              " position: " + IntToStr(X.LinePos) +
                              " abs at: " + IntToStr(X.FilePos) +
                              " error msg: " + X.Reason;
      EditView->SelStart = X.FilePos;
      EditView->Modified = true;
      EditView->SetFocus();
      Result = true;
    }
    else
      Close();
  }
  catch (Exception &X)
  {
    if (MessageDlg("An error occurred while parsing the document: " +
                   X.Message + 13 + 10 + 13 + 10 +
                   "Would you like to load the document into the " +
                   "source code editor?",
                  mtError, TMsgDlgButtons() << mbOK << mbNo, 0) == mrYes)
    {
      PageControl->ActivePage = SourceTab;
      EditView->Lines->LoadFromFile(sFile);
      EditView->Modified = true;
      EditView->SetFocus();
      Result = true;
    }
    else
      Close();
  }
  return(Result);
}
//---------------------------------------------------------------------------
void __fastcall TXMLChild::NewDocument(void)
{
  String aBuffer = "<?xml version=\"1.0\"?><root/>";

  XmlObjModelA->LoadMemory(aBuffer.c_str(), aBuffer.Length());
  FXml = XmlObjModelA;
#ifdef XPDPRO
  IntegratePreviewPage();
#endif
  FFileName = "";
  UpdateTreeFromObjModel(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TXMLChild::PasteFromClipboard(void)
{
  const String csNotXMLData = "Not valid XML data...cannot paste!";

  HGLOBAL aHandle;
  bool bIsBEorLE;
  int i;
  int iLen;
  TXpObjModel* oWorkXml;
  TXpNode* oNode;
  TXpNode* oInsertNode;
  TTreeNode* oNewTreeNode;
  TTreeNode* oTreeNode;
  PWideChar pBuffer;
  PWideChar pTmpBuffer;

  if (PageControl->ActivePage == SourceTab)
    EditView->PasteFromClipboard();
#ifdef XPDPRO
  else if (PageControl->ActivePage == FPreviewTab)
    return;
#endif
  else
  {
    if (FXml == XmlObjModelA)
      oWorkXml = XmlObjModelB;
    else
      oWorkXml = XmlObjModelA;

    iLen = 0;
    bIsBEorLE = false;
    pTmpBuffer = NULL;

    try
    {
      // Clipboard must contain text or unicode text.
      if (!Clipboard()->HasFormat(CF_TEXT))
      {
        MessageDlg(csNotXMLData, mtError, TMsgDlgButtons() << mbOK, 0);
        return;
      }

      // Ask for text in Unicode format. The clipboard will automatically
      // perform conversion if standard text was pasted to clipboard
      // (see http://msdn.microsoft.com/library/psdk/winbase/clipbrd_64j7.htm).
      aHandle = reinterpret_cast<HGLOBAL>(Clipboard()->GetAsHandle(CF_UNICODETEXT));
      if (aHandle != 0)
        try
        {
          // Convert the handle into a pointer. Obtain the buffer length.
          pBuffer = static_cast<wchar_t*>(GlobalLock(aHandle));
          iLen = GlobalSize(aHandle) * 2 + 2;

          // Are there any marks to indicate it is big endian or little endian?
          bIsBEorLE = ( ((pBuffer[0] == 254) && (pBuffer[1] == 257)) ||
                        ((pBuffer[0] == 257) && (pBuffer[1] == 254)) );
          if (!bIsBEorLE)
          {
            // No. Must force it to be recognized as little endian.
            pTmpBuffer = static_cast<wchar_t*>(malloc(iLen + 2));
            Move(&pBuffer, &pTmpBuffer + 1, iLen);
            pTmpBuffer[0] = 65279;  // $FEFF;
          }
          else
            pTmpBuffer = pBuffer;

          // Load the buffer into the DOM.
          oWorkXml->LoadMemory(pTmpBuffer, iLen + 2);
        }
        __finally
        {
          // The operating system handles freeing of the text.
          GlobalUnlock(aHandle);
          if (!bIsBEorLE)
            free(pTmpBuffer);
        }

      // Add contents of new tree to existing tree
      oTreeNode = TreeView->Selected;
      for(i = 0; i < oWorkXml->Document->ChildNodes->Length; i++)
      {
        oNode = oWorkXml->Document->ChildNodes->Item(i)->CloneNode(true);
        FXml->Document->DocumentElement->ForceOwnerDocument(oNode);
        if ((oTreeNode != NULL) &&
           (((TXpNode*)(oTreeNode->Data))->ParentNode->NodeType !=
            DOCUMENT_NODE))
        {
          oInsertNode = (TXpNode*)(oTreeNode->Data);
          oInsertNode->ParentNode->InsertBefore(oNode, oInsertNode->NextSibling);
          oNewTreeNode = TreeView->Items->InsertObject(oTreeNode->getNextSibling(),
                                                       GetTreeNodeText(oNode),
                                                       oNode);
        }
        else
        {
          oInsertNode = FXml->Document->DocumentElement;
          oInsertNode->AppendChild(oNode);
          oNewTreeNode = TreeView->Items->AddChildObject
                            (GetTreeNodeForNode(oInsertNode),
                             GetTreeNodeText(oNode), oNode);
        }
        // Add tree nodes for the new node's children.
        UpdateTreeChildrenFromObjModel(oNode, oNewTreeNode);
        oNode->Release();
      }
      FChanged = true;
      SetCtrlStates();
    }
    catch(EXpParserError &X)
    {
      MessageDlg(csNotXMLData, mtError, TMsgDlgButtons() << mbOK, 0);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TXMLChild::RefreshList(void)
{

  int aCount = ListView->Items->Count;
  int aVisCount = ListView->VisibleRowCount;

  // Do we have room for all the items that can possibly be displayed?
  if (aCount != aVisCount)
    ListView->Items->Count = aVisCount;
  InvalidateRect(ListView->Handle, NULL, false);
}
//---------------------------------------------------------------------------
bool __fastcall TXMLChild::SaveFile(String sFile)
{
  String sData;
  bool Result = false;

  // Backup file
  if (sFile == "")
    sFile = FFileName;

  SetCapture(Handle);
  Screen->Cursor = crHourGlass;
  if (FileExists(sFile))
  {
    sData = sFile;
    sData.Delete(sData.Length() - 2, 3);
    sData = sData + "~ml";
    DeleteFile(sData);
    RenameFile(sFile, sData);
  }

  Result = FXml->SaveToFile(sFile);

  Result = false;

  // Are we on the source view?
  if (PageControl->ActivePage == SourceTab)
  {
    // Yes. Save the contents of the source view to the file.
    try
    {
      EditView->Lines->SaveToFile(sFile);
      Result = true;
    }
    catch (Exception &X)
    {
      ShowMessage(X.Message);
    }
  }
  else
    // No. Save the object model to the file.
    Result = FXml->SaveToFile(sFile);

  if (Result)
  {
    FFileName = LowerCase(sFile);
    Caption = FFileName;
    FChanged = false;
  }
  Screen->Cursor = crDefault;
  ReleaseCapture();
  SetCtrlStates();
  return(Result);
}
//---------------------------------------------------------------------------
bool __fastcall TXMLChild::SaveToURL(String sURL, String sUserId, String sPassword)
{
  bool Result = false;
  TCursor savCursor = Screen->Cursor;

  FXml->UserName = sUserId;
  FXml->Password = sPassword;
  SetCapture(Handle);
  Screen->Cursor = crHourGlass;
  try
  {
    Result = FXml->SaveToFile(sURL);
    FChanged = !Result;
  }
  __finally
  {
    Screen->Cursor = savCursor;
    SetCtrlStates();
    ReleaseCapture();
  }
  return(Result);
}
//---------------------------------------------------------------------------
void __fastcall TXMLChild::SetCtrlStates(void)
{
  bool OnStructTab;
  bool InTree;
  bool NodeIsElement;
  bool ParentNotDoc;
  bool ParentNotDocElement;
  bool NextSiblingExists;
  bool PrevSiblingExists;
  bool PrevSiblingIsElement;
  bool AttrSelected;
  TXpElement* aDocElement;
  TTreeNode* aTreeNode = TreeView->Selected;
  TXpNode* aNode;
  TXpElement* anElement;

  // Check preconditions.
  OnStructTab = (PageControl->ActivePage == StructTab);

  AttrSelected = (AttrProperties->Selected >= 0);

  InTree = OnStructTab && (aTreeNode != NULL);
  if (InTree)
    aNode = (TXpNode*)aTreeNode->Data;

  NextSiblingExists = InTree && (aNode->NextSibling != NULL);

  NodeIsElement = InTree && (aNode->NodeType == ELEMENT_NODE);

  ParentNotDoc = InTree && (aNode->ParentNode->NodeType != DOCUMENT_NODE);

  ParentNotDocElement = InTree &&
                        (aNode->ParentNode != FXml->Document->DocumentElement);

  PrevSiblingExists = InTree && (aNode->PreviousSibling != NULL);

  PrevSiblingIsElement = PrevSiblingExists &&
                         (aNode->PreviousSibling->NodeType == ELEMENT_NODE);

  // Enable controls.
  btnInsert->Enabled = (PageControl->ActivePage == StructTab);

  mnuAddElementasChild->Enabled = NodeIsElement;
  btnAddChildElement->Enabled = NodeIsElement;

  mnuInsertText->Enabled = ParentNotDoc;

  mnuAddTextAsChild->Enabled = NodeIsElement;
  btnAddChildText->Enabled = NodeIsElement;

  mnuInsertComment->Enabled = InTree;

  mnuInsertPI->Enabled = InTree;

  mnuInsertCDATA->Enabled = ParentNotDoc;

  mnuAddCDATA->Enabled = NodeIsElement;

  btnDuplicate->Enabled = InTree;

  btnPromote->Enabled = ParentNotDoc && ParentNotDocElement;

  btnDemote->Enabled = PrevSiblingIsElement;

  btnMoveUp->Enabled = PrevSiblingExists;

  btnMoveDown->Enabled = NextSiblingExists;

  btnCollapseAll->Enabled = OnStructTab;

  btnExpandAll->Enabled = OnStructTab;

  btnNormalize->Enabled = NodeIsElement;

  mnuPreserveSpace->Enabled = NodeIsElement;
  if (aNode != NULL)
    anElement = (TXpElement*)aNode;
  mnuPreserveSpace->Checked = NodeIsElement &&
                            (anElement->GetAttribute("xml:space") = "preserve");

  mnuAddAttr->Enabled = NodeIsElement;

  mnuRemoveAttr->Enabled = OnStructTab && AttrSelected;

  mnuEditAttr->Enabled = OnStructTab && AttrSelected;

  aDocElement = NULL;
  if (FOnGetXSLDocElement != NULL)
    aDocElement = FOnGetXSLDocElement(this);

  btnXSL->Enabled = (aDocElement != NULL) && NodeIsElement;

  mnuSelectXSLAttr->Enabled = (aDocElement != NULL) && InTree &&
                              ((aNode->Prefix == WideString("xsl")) ||
                               (aNode->Prefix == WideString("fo")));

#ifdef DCC4OrLater
  mnuAddGUID->Enabled = NodeIsElement;
#else
  mnuAddGUID->Enabled = false;
#endif

  if (FOnSetCtrlStates != NULL)
    FOnSetCtrlStates(this);

}
//---------------------------------------------------------------------------
void __fastcall TXMLChild::SetFormattedOutput(bool aValue)
{
  XmlObjModelA->FormattedOutput = aValue;
  XmlObjModelB->FormattedOutput = aValue;
}
//---------------------------------------------------------------------------
void __fastcall TXMLChild::SetListViewText(TXpNode* oNode, String sValue)
{
  switch(oNode->NodeType)
  {
    case PROCESSING_INSTRUCTION_NODE :
    case COMMENT_NODE :
    case TEXT_NODE :
    case CDATA_SECTION_NODE :
      oNode->NodeValue = sValue;
      FChanged = true;
      break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TXMLChild::SetNormalize(bool aValue)
{
  XmlObjModelA->NormalizeData = aValue;
  XmlObjModelB->NormalizeData = aValue;
}
//---------------------------------------------------------------------------
bool __fastcall TXMLChild::SetTreeNodeText(TXpNode* oNode, String &sValue)
{
  bool Result = false;
  switch(oNode->NodeType)
  {
    case TEXT_NODE :
      sValue = "[TEXT]";
      break;
    case CDATA_SECTION_NODE :
      sValue = "[CDATA]";
      break;
    case COMMENT_NODE :
      sValue = "[COMMENT]";
      break;
    default :
      if (sValue == "")
        sValue = oNode->NodeName;
      else
      {
        oNode->NodeName = sValue;
        Result = true;
      }
  }
  return(Result);
}
//---------------------------------------------------------------------------
void __fastcall TXMLChild::UpdateTreeChildrenFromObjModel(TXpNode* oNode, TTreeNode* oTreeNode)
{
  int aCount;
  int i;
  TXpNode* oChild;
  TTreeNode* oRefNode;

  aCount = oTreeNode->Count;
  for (i = 0; i < oNode->ChildNodes->Length; i++)
  {
    oChild = oNode->ChildNodes->Item(i);
    if (i >= aCount)
      oRefNode = TreeView->Items->AddChildObject(oTreeNode,
                                                 GetTreeNodeText(oChild), oChild);
    else if (oTreeNode->Item[i]->Data == oChild)
    {
      oRefNode = oTreeNode->Item[i];
      oRefNode->Text = GetTreeNodeText(oChild);
    }
    else
      oRefNode = TreeView->Items->InsertObject(oTreeNode->Item[i],
                                               GetTreeNodeText(oChild), oChild);
    if (oChild->HasChildNodes())
      UpdateTreeChildrenFromObjModel(oChild, oRefNode);
    else if (oRefNode->Count > 0)
      oRefNode->DeleteChildren();
  }

  while (oTreeNode->Count > oNode->ChildNodes->Length)
    oTreeNode->Item[oTreeNode->Count-1]->Delete();
}
//---------------------------------------------------------------------------
void __fastcall TXMLChild::UpdateTreeFromObjModel(TXpNode* oNewSelNode)
{
  int aCount;
  TXpNode* oNode;
  TXpNode* oSelNode;
  TXpNode* oChild;
  TTreeNode* oTreeNode;
  TTreeNode* oRefNode;
  int i, j;
  bool bCallEnd;

  if (FXml == NULL)
    return;

  // Get selected node
  oSelNode = oNewSelNode;
  if ((oSelNode == NULL) && (TreeView->Selected != NULL))
    oSelNode = (TXpNode*)TreeView->Selected->Data;
  oNode = FXml->Document;
  oTreeNode = TreeView->Items->GetFirstNode();
  bCallEnd = false;
  if (oTreeNode == NULL)
  {
    TreeView->Items->BeginUpdate();
    bCallEnd = true;
  }
  for (i = 0; i < oNode->ChildNodes->Length; i++)
  {
    oChild = oNode->ChildNodes->Item(i);
    if (oTreeNode == NULL)
      oRefNode = TreeView->Items->AddObject(NULL, GetTreeNodeText(oChild), oChild);
    else if (oTreeNode->Data == oChild)
    {
      oRefNode = oTreeNode;
      oRefNode->Text = GetTreeNodeText(oChild);
      oTreeNode = oTreeNode->getNextSibling();
    }
    else
      oRefNode = TreeView->Items->InsertObject(oTreeNode, GetTreeNodeText(oChild), oChild);
    if (oChild->HasChildNodes())
      UpdateTreeChildrenFromObjModel(oChild, oRefNode);
    else if (oRefNode->Count > 0)
      oRefNode->DeleteChildren();
  }
  if (bCallEnd)
    TreeView->Items->EndUpdate();
  if (oTreeNode != NULL)
  {
    j = oTreeNode->AbsoluteIndex;
    aCount = TreeView->Items->Count - 1;
    for (i = aCount; i >= j; i--)
      TreeView->Items->Item[i]->Delete();
  }
  // Set selected node
  if (oSelNode != NULL)
  {
    j = TreeView->Items->Count - 1;
    for (i = 0; i <= j; i++)
    {
      if (TreeView->Items->Item[i]->Data == oSelNode)
      {
        TreeView->Selected = TreeView->Items->Item[i];
        break;
      }
    }
  }
  RefreshList();
}
//---------------------------------------------------------------------------
void __fastcall TXMLChild::mnuDeleteNodeClick(TObject* Sender)
{
  TTreeNode* oTreeNode;
  TXpNode* oNode;

  // Obtain the selected tree node.
  oTreeNode = TreeView->Selected;
  if (oTreeNode != NULL)
  {
    // Obtain the corresponding DOM node.
    oNode = (TXpNode*)oTreeNode->Data;
    if (oNode == FXml->Document->DocumentElement)
    {
      MessageDlg("You are not allowed to remove the document element.",
                 mtError, TMsgDlgButtons() << mbOK, 0);
      return;
    }

    // The tree's new selected node is the next sibling of the currently selected
    //  tree node.
    TreeView->Selected = oTreeNode->getNextSibling();
    oNode->ParentNode->RemoveChild(oNode);
    oNode->Release();
    oTreeNode->Delete();
    FChanged = true;
    SetCtrlStates();
  }
  else
    MessageDlg("No node selected.", mtError, TMsgDlgButtons() << mbOK, 0);
}
//---------------------------------------------------------------------------



void __fastcall TXMLChild::FormClose(TObject *Sender, TCloseAction &Action)
{
  Action = caFree;
  if (FOnClosing != NULL)
    FOnClosing(Sender);
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  int wRet;

  CanClose = true;
  if (FChanged)
  {
    wRet = MessageDlg("File " + Caption +
                      " has changed.  Do you want to save the changes?",
                      mtConfirmation,
                      TMsgDlgButtons() << mbYes << mbNo << mbCancel, 0);
    switch(wRet)
    {
      case mrYes :
        if (FFileName == "")
        {
          CanClose = false;
          if (FOnSaveAs != NULL)
            FOnSaveAs(this);
        }
        else
          SaveFile("");
        break;
      case mrCancel :
        CanClose = false;
        break;
    }
  }
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::FormCreate(TObject *Sender)
{
  FAttrNode = NULL;
  FLoadingSrc = false;
#ifdef XPDPRO
  FPreviewForm = NULL;
  FPreviewTab = NULL;
#endif
  FXml = NULL;
  AttrProperties = new TXpPropertiesWindow(this);
  PageControl->ActivePage = StructTab;
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::FormDestroy(TObject *Sender)
{
  AttrProperties->Free();
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::FormShow(TObject *Sender)
{
  btnXSL->Enabled = false;
  AttrProperties->Parent = Panel2;
  AttrProperties->Height = 105;
  AttrProperties->Width = 177;
  AttrProperties->Top = 140;
  AttrProperties->ValueHeader = "Attr. Value";
  AttrProperties->NameHeader = "Attr. Name";
  AttrProperties->NameWidth = 75;
  AttrProperties->PopupMenu = AttrPopupMenu;
  AttrProperties->RowHeight = 15;
  AttrProperties->ShowHeader = true;
  AttrProperties->ShowLines = true;
  AttrProperties->OnClick = AttrPropertiesClick;
  AttrProperties->OnPropertyName = AttrPropertiesPropertyName;
  AttrProperties->OnPropertyValue = AttrPropertiesPropertyValue;
  AttrProperties->OnValueChange = AttrPropertiesValueChange;
  AttrProperties->OnValueQueryEdit = AttrPropertiesValueQueryEdit;
  AttrProperties->Align = alBottom;

  Splitter2->Align = alBottom;
  TreeView->Align = alClient;
  SetCtrlStates();
}
//---------------------------------------------------------------------------
void __fastcall TXMLChild::AttrPropertiesClick(TObject *Sender)
{
  SetCtrlStates();
}
//---------------------------------------------------------------------------
void __fastcall TXMLChild::AttrPropertiesPropertyName(TObject* oOwner, int wIndex,
                                                      String &sData)
{
  if ((FAttrNode != NULL) && FAttrNode->HasAttributes() &&
      (wIndex < FAttrNode->Attributes->Length))
    sData = FAttrNode->Attributes->Item(wIndex)->NodeName;
}
//---------------------------------------------------------------------------
void __fastcall TXMLChild::AttrPropertiesPropertyValue(TObject* oOwner, int wIndex,
                                                       String &sData)
{
  if ((FAttrNode != NULL) && FAttrNode->HasAttributes() &&
      (wIndex < FAttrNode->Attributes->Length))
    sData = FAttrNode->Attributes->Item(wIndex)->NodeValue;
}
//---------------------------------------------------------------------------
void __fastcall TXMLChild::AttrPropertiesValueQueryEdit(TObject* oOwner, int wIndex,
                                                        EXpEditType &oEditType,
                                                        String &sFunctionName)
{
  oEditType = etEdit;
}
//---------------------------------------------------------------------------
void __fastcall TXMLChild::AttrPropertiesValueChange(TObject *oOwner, int wIndex,
                                                     String sValue)
{
  if ((FAttrNode != NULL) && FAttrNode->HasAttributes() &&
      (wIndex < FAttrNode->Attributes->Length))
    if (sValue != FAttrNode->Attributes->Item(wIndex)->NodeValue)
    {
      FAttrNode->Attributes->Item(wIndex)->NodeValue = sValue;
      InvalidateRect(ListView->Handle, NULL, false);
      FChanged = true;
    }
  SetCtrlStates();
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::btnAddChildElementClick(TObject *Sender)
{
  TXpElement* oElem;
  TXpNode* oNode;
  TTreeNode* oTreeNode;

  oTreeNode = TreeView->Selected;
  if (oTreeNode != NULL)
    oNode = (TXpNode*)oTreeNode->Data;

  if ((oNode != NULL) && (oNode->NodeType == ELEMENT_NODE))
  {
    if (ElementForm->ShowModal() == mrOk)
    {
      oElem = FXml->Document->CreateElement(ElementForm->NameEdit->Text);
      oNode->AppendChild(oElem);
      TreeView->Items->AddChildObject(oTreeNode, GetTreeNodeText(oElem), oElem);
      if (!oTreeNode->Expanded)
        oTreeNode->Expand(false);
      oElem->Release();
      FChanged = true;
      SetCtrlStates();
    }
  }
  else
    MessageDlg("No node selected or cannot add a child to this kind of node.",
                 mtError, TMsgDlgButtons() << mbOK, 0);

}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::btnAddChildTextClick(TObject *Sender)
{
  TXpText* oText;
  TXpNode* oNode;
  TTreeNode* oTreeNode;

  oTreeNode = TreeView->Selected;
  if (oTreeNode != NULL)
    oNode = (TXpNode*)oTreeNode->Data;

  if ((oNode != NULL) && (oNode->NodeType == ELEMENT_NODE))
  {
    TextForm->TextEdit->Text = "";
    if (TextForm->ShowModal() == mrOk)
    {
      oText = FXml->Document->CreateTextNode(TextForm->TextEdit->Text);
      oNode->AppendChild(oText);
      TreeView->Items->AddChildObject(oTreeNode, GetTreeNodeText(oText), oText);
      if (!oTreeNode->Expanded)
        oTreeNode->Expand(false);
      oText->Release();
      FChanged = true;
      SetCtrlStates();
    }
  }
  else
    MessageDlg("No node selected or cannot add text below this node.",
                 mtError, TMsgDlgButtons() << mbOK, 0);

}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::btnCollapseAllClick(TObject *Sender)
{
  TCursor savCursor = Screen->Cursor;

  SetCapture(Handle);
  Screen->Cursor = crHourGlass;
  TreeView->FullCollapse();
  Screen->Cursor = savCursor;
  ReleaseCapture();
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::btnDemoteClick(TObject *Sender)
{
  TXpNode* oNode;
  TXpNode* oPrev;
  TTreeNode *oTreeNode;

  oTreeNode = TreeView->Selected;
  if (oTreeNode != NULL)
    oNode = (TXpNode*)oTreeNode->Data;

  if ((oNode != NULL) &&
      (oNode->PreviousSibling != NULL) &&
      (oNode->PreviousSibling->NodeType == ELEMENT_NODE))
  {
    oPrev = oNode->PreviousSibling;
    oNode = oNode->ParentNode->RemoveChild(oNode);
    oPrev->InsertBefore(oNode, oPrev->FirstChild);
    oNode->Release();
    oTreeNode->MoveTo(oTreeNode->getPrevSibling(), naAddChildFirst);
    FChanged = true;
    SetCtrlStates();
  }
  else
    MessageDlg("No node selected or cannot demote this node.",
               mtError, TMsgDlgButtons() << mbOK, 0);
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::btnDuplicateClick(TObject *Sender)
{
  TXpNode* oNode;
  TXpNode* oNewNode;
  TTreeNode *oTreeNode;
  TTreeNode *oNewTreeNode;

  oTreeNode = TreeView->Selected;
  if (oTreeNode != NULL)
  {
    oNode = (TXpNode*)oTreeNode->Data;
    if (oNode == FXml->Document->DocumentElement)
    {
      MessageDlg("You are not allowed to duplicate the document element.",
                 mtError, TMsgDlgButtons() << mbOK, 0);
      return;
    }

    // Duplicate via button click is always recursive.
    oNewNode = oNode->CloneNode(true);
    if (oNewNode == NULL)
      return;

    oNode->ParentNode->InsertBefore(oNewNode, oNode->NextSibling);
    oNewTreeNode = TreeView->Items->InsertObject(oTreeNode->getNextSibling(),
                                                 GetTreeNodeText(oNewNode),
                                                 oNewNode);
    // Add tree nodes for the new node's children.
    UpdateTreeChildrenFromObjModel(oNewNode, oNewTreeNode);
    oNewNode->Release();
    FChanged = true;
    SetCtrlStates();
  }
  else
    MessageDlg("No node selected.",
               mtError, TMsgDlgButtons() << mbOK, 0);
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::btnExpandAllClick(TObject *Sender)
{
  TCursor savCursor = Screen->Cursor;
  TTreeNode* oNode;

  SetCapture(Handle);
  Screen->Cursor = crHourGlass;
  oNode = TreeView->TopItem;
  TreeView->FullExpand();
  TreeView->TopItem = oNode;
  Screen->Cursor = savCursor;
  ReleaseCapture();
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::btnMoveDownClick(TObject *Sender)
{
  bool bExpanded;
  TXpNode* oNode;
  TXpNodeList* oList;
  TTreeNode* oTreeNode;
  TTreeNode* oTreeNodeSibling;

  oTreeNode = TreeView->Selected;
  if (oTreeNode != NULL)
  {
    oTreeNodeSibling = oTreeNode->getNextSibling();
    if (oTreeNodeSibling != NULL)
    {
      TreeView->Items->BeginUpdate();
      try
      {
        // Swap the nodes in the tree view & the DOM.
        oNode = (TXpNode*)oTreeNode->Data;
        oList = oNode->ParentNode->ChildNodes;
        oList->Move(oList->IndexOf(oNode), oList->IndexOf(oNode) + 1);
        bExpanded = oTreeNodeSibling->Expanded;
        oTreeNodeSibling->MoveTo(oTreeNode, naInsert);
        oTreeNodeSibling->Expanded = bExpanded;
        FChanged = true;
        SetCtrlStates();
      }
      __finally
      {
        TreeView->Items->EndUpdate();
      }
    }
    else
      MessageDlg("No node selected or cannot move this node down.",
                 mtError, TMsgDlgButtons() << mbOK, 0);
  }
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::btnMoveUpClick(TObject *Sender)
{
  bool bExpanded;
  TXpNode* oNode;
  TXpNodeList* oList;
  TTreeNode* oTreeNode;
  TTreeNode* oTreeNodeSibling;

  oTreeNode = TreeView->Selected;
  if (oTreeNode != NULL)
  {
    oTreeNodeSibling = oTreeNode->getPrevSibling();
    if (oTreeNodeSibling != NULL)
    {
      TreeView->Items->BeginUpdate();
      try
      {
        // Swap the nodes in the tree view & the DOM.
        oNode = (TXpNode*)oTreeNode->Data;
        oList = oNode->ParentNode->ChildNodes;
        oList->Move(oList->IndexOf(oNode), oList->IndexOf(oNode) - 1);
        bExpanded = oTreeNode->Expanded;
        oTreeNode->MoveTo(oTreeNodeSibling, naInsert);
        oTreeNode->Expanded = bExpanded;
        FChanged = true;
        SetCtrlStates();
      }
      __finally
      {
        TreeView->Items->EndUpdate();
      }
    }
    else
      MessageDlg("No node selected or cannot move this node up.",
                 mtError, TMsgDlgButtons() << mbOK, 0);
  }
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::btnNormalizeClick(TObject *Sender)
{
  TXpElement* oNode;
  TTreeNode* oTreeNode;

  oTreeNode = TreeView->Selected;
  if (oTreeNode != NULL)
    oNode = (TXpElement*)oTreeNode->Data;

  if ((oNode != NULL) && (oNode->NodeType == ELEMENT_NODE))
  {
    oNode->Normalize(true);
    FChanged = true;
    UpdateTreeFromObjModel(NULL);
    SetCtrlStates();
  }
  else
    MessageDlg("No element node selected...cannot normalize.",
                mtError, TMsgDlgButtons() << mbOK, 0);
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::btnPromoteClick(TObject *Sender)
{
  TXpNode* oNode;
  TXpNode* oParent;
  TTreeNode* oTreeNode;

  oTreeNode = TreeView->Selected;
  if (oTreeNode != NULL)
    oNode = (TXpNode*)oTreeNode->Data;

  if ((oNode != NULL) &&
      (oNode->ParentNode->NodeType != DOCUMENT_NODE))
  {
    oParent = (TXpNode*)oNode->ParentNode;
    oNode = oParent->RemoveChild(oNode);
    oParent->ParentNode->InsertBefore(oNode, oParent->NextSibling);
    oNode->Release();
    oTreeNode->MoveTo(oTreeNode->Parent->getNextSibling(), naInsert);
    FChanged = true;
    SetCtrlStates();
  }
  else
    MessageDlg("No node selected or cannot promote this node.",
               mtError, TMsgDlgButtons() << mbOK, 0);
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::EditViewChange(TObject *Sender)
{
  if (!FLoadingSrc)
  {
    FChanged = true;
    if (FOnSetCtrlStates != NULL)
      FOnSetCtrlStates(this);
  }
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::ListViewData(TObject* Sender, TListItem* Item)
{
  int i;
  TTreeNode* oItem;

  if (TreeView->TopItem == NULL)
    return;

  if (Item->Index == 0)
  {
    Item->Caption = GetListViewText((TXpNode*)TreeView->TopItem->Data);
    FLastIndex = 0;
    FLastItem = TreeView->TopItem;
  }
  else if ((Item->Index == FLastIndex + 1) && (FLastItem != NULL))
  {
    FLastIndex = Item->Index;
    FLastItem = FLastItem->GetNextVisible();
    if (FLastItem != NULL)
      Item->Caption = GetListViewText((TXpNode*)FLastItem->Data);
  }
  else
  {
    i = Item->Index;
    oItem = TreeView->TopItem;
    while ((i > 0) && (oItem != NULL))
    {
      oItem = oItem->GetNextVisible();
      i--;
    }
    if (oItem != NULL)
      Item->Caption = GetListViewText((TXpNode*)oItem->Data);
  }
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::ListViewDblClick(TObject *Sender)
{
  int i;
  TTreeNode* oItem;

  if ((TreeView->TopItem == NULL) ||
      (ListView->Selected == NULL))
    return;

  i = ListView->Selected->Index;
  oItem = TreeView->TopItem;
  while ((i > 0) && (oItem != NULL))
  {
    oItem = oItem->GetNextVisible();
    i--;
  }
  if (oItem != NULL)
  {
    EditNodeDialog((TXpNode*)oItem->Data);
    oItem->Text = GetTreeNodeText((TXpNode*)oItem->Data);
  }
  SetCtrlStates();
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::ListViewEdited(TObject *Sender, TListItem *Item,
      AnsiString &S)
{
  int i;
  TTreeNode* oItem;

  if (TreeView->TopItem == NULL)
    return;

  i = Item->Index;
  oItem = TreeView->TopItem;
  while ((i > 0) && (oItem != NULL))
  {
    oItem = oItem->GetNextVisible();
    i--;
  }
  if (oItem != NULL)
  {
    SetListViewText((TXpNode*)oItem->Data, S);
    oItem->Text = GetTreeNodeText((TXpNode*)oItem->Data);
  }
  SetCtrlStates();
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::ListViewEditing(TObject *Sender,
      TListItem *Item, bool &AllowEdit)
{
  int i;
  TTreeNode* oItem;
  TXpNode* oNode;

  AllowEdit = false;
  if ((TreeView->TopItem == NULL) ||
      (ListView->Selected == NULL))
    return;

  i = ListView->Selected->Index;
  oItem = TreeView->TopItem;
  while ((i > 0) && (oItem != NULL))
  {
    oItem = oItem->GetNextVisible();
    i--;
  }
  if (oItem != NULL)
  {
    oNode = (TXpNode*)oItem->Data;
    switch(oNode->NodeType)
    {
      case PROCESSING_INSTRUCTION_NODE :
      case COMMENT_NODE :
      case TEXT_NODE :
      case CDATA_SECTION_NODE :
        AllowEdit = true;
        break;
    }
  }
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::ListViewKeyPress(TObject *Sender, char &Key)
{
  inherited::KeyPress(Key);
  if ((Key == 13) && (ListView->Selected != NULL) &&
      !ListView->IsEditing())
  {
    ListView->Selected->EditCaption();
    Key = 0;
  }
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::mnuAddAttrClick(TObject *Sender)
{
  TAttributeForm* oAttrForm;

  if (FAttrNode != NULL)
  {
    oAttrForm = new TAttributeForm(this);
    if (oAttrForm->ShowModal() == mrOk)
    {
      FAttrNode->SetAttribute(oAttrForm->AttrNameEdit->Text,
                              oAttrForm->AttrValueEdit->Text);
      AttrProperties->RowCount = FAttrNode->Attributes->Length;
      ListView->Invalidate();
      FChanged = true;
    }
    oAttrForm->Free();
  }
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::mnuAddCDATAClick(TObject *Sender)
{
  TXpCDataSection* oText;
  TXpNode* oNode;
  TTreeNode* oTreeNode;

  oTreeNode = TreeView->Selected;
  if (oTreeNode != NULL)
    oNode = (TXpNode*)oTreeNode->Data;

  if ((oNode != NULL) && (oNode->NodeType == ELEMENT_NODE))
  {
    TextForm->TextEdit->Text = "";
    if (TextForm->ShowModal() == mrOk)
    {
      oText = FXml->Document->CreateCDATASection(TextForm->TextEdit->Text);
      oNode->AppendChild(oText);
      TreeView->Items->AddChildObject(oTreeNode, GetTreeNodeText(oText), oText);
      if (!oTreeNode->Expanded)
        oTreeNode->Expand(false);
      oText->Release();
      FChanged = true;
      SetCtrlStates();
    }
  }
  else
    MessageDlg("No node selected or cannot add CDATA section below this node.",
               mtError, TMsgDlgButtons() << mbOK, 0);
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::mnuAddElementasChildClick(TObject *Sender)
{
  TXpElement *oElem;
  TXpNode* oNode = NULL;
  TTreeNode* oTreeNode;

  oTreeNode = TreeView->Selected;
  if (oTreeNode != NULL)
    oNode = (TXpNode*)oTreeNode->Data;

  if ((oNode != NULL) && (oNode->NodeType == ELEMENT_NODE))
  {
    if (ElementForm->ShowModal() == mrOk)
    {
      oElem = FXml->Document->CreateElement(ElementForm->NameEdit->Text);
      oNode->AppendChild(oElem);
      TreeView->Items->AddChildObject(oTreeNode, GetTreeNodeText(oElem), oElem);
      if (!oTreeNode->Expanded)
        oTreeNode->Expand(false);
      oElem->Release();
      FChanged = true;
      SetCtrlStates();
    }
  }
  else
    MessageDlg("No node selected or cannot add a child to this kind of node.",
               mtError, TMsgDlgButtons() << mbOK, 0);
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::mnuAddGUIDClick(TObject *Sender)
{
#ifdef DCC4OrLater
  if (FAttrNode != NULL)
  {
    FAttrNode->SetAttribute("id", CreateClassID);
    AttrProperties->RowCount = FAttrNode->Attributes->Length;
    ListView->Invalidate();
    FChanged = true;
  }
#endif
}
//---------------------------------------------------------------------------


void __fastcall TXMLChild::mnuCopyClick(TObject *Sender)
{
  CopyToClipboard(true);
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::mnuCutClick(TObject *Sender)
{
  CutToClipboard();
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::mnuEditAttrClick(TObject *Sender)
{
  TAttributeForm* oAttrForm;
  TXpNode* oNode;

  if ((FAttrNode != NULL) &&
      (FAttrNode->HasAttributes()) &&
      (AttrProperties->Selected != -1) &&
      (AttrProperties->Selected < FAttrNode->Attributes->Length))
  {
    oAttrForm = new TAttributeForm(this);
    oNode = FAttrNode->Attributes->Item(AttrProperties->Selected);
    oAttrForm->AttrValueEdit->Text = oNode->NodeValue;
    oAttrForm->AttrNameEdit->Text = oNode->NodeName;
    if (oAttrForm->ShowModal() == mrOk)
    {
      oNode->NodeValue = oAttrForm->AttrValueEdit->Text;
      oNode->NodeName = oAttrForm->AttrNameEdit->Text;
      AttrProperties->Invalidate();
      FChanged = true;
    }
    oAttrForm->Free();
  }
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::mnuInsertCDATAClick(TObject *Sender)
{
  TXpCDataSection* oText;
  TXpNode* oNode;
  TTreeNode* oTreeNode;

  oTreeNode = TreeView->Selected;
  if (oTreeNode != NULL)
    oNode = (TXpNode*)oTreeNode->Data;

  if ((oNode != NULL) && (oNode->ParentNode->NodeType != DOCUMENT_NODE))
  {
    TextForm->TextEdit->Text = "";
    if (TextForm->ShowModal() == mrOk)
    {
      oText = FXml->Document->CreateCDATASection(TextForm->TextEdit->Text);
      oNode->ParentNode->InsertBefore(oText, oNode);
      TreeView->Items->InsertObject(oTreeNode, GetTreeNodeText(oText), oText);
      oText->Release();
      FChanged = true;
      SetCtrlStates();
    }
  }
  else
    MessageDlg("No node selected or cannot insert CDATA section at this level.",
               mtError, TMsgDlgButtons() << mbOK, 0);
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::mnuInsertCommentClick(TObject *Sender)
{
  TXpComment* oComment;
  TXpNode* oNode;
  TTreeNode* oTreeNode;


  oTreeNode = TreeView->Selected;
  if (oTreeNode != NULL)
  {
    CommentForm->CommentEdit->Text = "";
    if (CommentForm->ShowModal() == mrOk)
    {
      oComment = FXml->Document->CreateComment(CommentForm->CommentEdit->Text);
      oNode = (TXpNode*)oTreeNode->Data;
      oNode->ParentNode->InsertBefore(oComment, oNode);
      TreeView->Items->InsertObject(oTreeNode, GetTreeNodeText(oComment), oComment);
      oComment->Release();
      FChanged = true;
      SetCtrlStates();
    }
  }
  else
    MessageDlg("No node selected.", mtError, TMsgDlgButtons() << mbOK, 0);
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::mnuInsertElementClick(TObject *Sender)
{
  TXpElement* oElem;
  TXpNode* oNode;
  TTreeNode* oTreeNode;

  if (ElementForm->ShowModal() == mrOk)
  {
    oElem = FXml->Document->CreateElement(ElementForm->NameEdit->Text);
    oTreeNode = TreeView->Selected;
    if (oTreeNode != NULL)
      oNode = (TXpNode*)oTreeNode->Data;

    if ((oNode != NULL) && (oNode->ParentNode->NodeType != DOCUMENT_NODE))
    {
      oNode->ParentNode->InsertBefore(oElem, oNode);
      TreeView->Items->InsertObject(oTreeNode, GetTreeNodeText(oElem), oElem);
    }
    else
    {
      oNode = FXml->Document->DocumentElement;
      oNode->AppendChild(oElem);
      TreeView->Items->AddChildObject(GetTreeNodeForNode(oNode),
                                      GetTreeNodeText(oElem), oElem);
    }
    oElem->Release();
    FChanged = true;
    SetCtrlStates();
  }
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::mnuInsertPIClick(TObject *Sender)
{
  TXpProcessingInstruction* oPI;
  TXpNode* oNode;
  TPIForm* oPIForm;
  TTreeNode* oTreeNode;

  oTreeNode = TreeView->Selected;
  if (oTreeNode != NULL)
  {
    oPIForm = new TPIForm(this);
    if (oPIForm->ShowModal() == mrOk)
    {
      oPI = FXml->Document->CreateProcessingInstruction(oPIForm->PINameEdit->Text, "");
      oPI->NodeValue = oPIForm->PIValueEdit->Text;
      oNode = (TXpNode*)oTreeNode->Data;
      oNode->ParentNode->InsertBefore(oPI, oNode);
      TreeView->Items->InsertObject(oTreeNode, GetTreeNodeText(oPI), oPI);
      oPI->Release();
      FChanged = true;
      SetCtrlStates();
    }
    oPIForm->Free();
  }
  else
    MessageDlg("No node selected.", mtError, TMsgDlgButtons() << mbOK, 0);
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::mnuInsertTextClick(TObject *Sender)
{
  TXpText* oText;
  TXpNode* oNode;
  TTreeNode* oTreeNode;

  oTreeNode = TreeView->Selected;
  if (oTreeNode != NULL)
    oNode = (TXpNode*)oTreeNode->Data;

  if ((oNode != NULL) && (oNode->ParentNode->NodeType != DOCUMENT_NODE))
  {
    TextForm->TextEdit->Text = "";
    if (TextForm->ShowModal() == mrOk)
    {
      oText = FXml->Document->CreateTextNode(TextForm->TextEdit->Text);
      oNode->ParentNode->InsertBefore(oText, oNode);
      TreeView->Items->InsertObject(oTreeNode, GetTreeNodeText(oText), oText);
      oText->Release();
      FChanged = true;
      SetCtrlStates();
    }
  }
  else
    MessageDlg("No node selected or cannot insert text at this level.",
               mtError, TMsgDlgButtons() << mbOK, 0);
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::mnuPasteClick(TObject *Sender)
{
  PasteFromClipboard();
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::mnuPreserveSpaceClick(TObject *Sender)
{
  TXpElement* oNode;

  oNode = (TXpElement*)TreeView->Selected->Data;

  if (mnuPreserveSpace->Checked)
    oNode->RemoveAttribute("xml:space");
  else
    oNode->SetAttribute("xml:space", "preserve");
  if (FAttrNode->HasAttributes())
    AttrProperties->RowCount = FAttrNode->Attributes->Length;
  else
    AttrProperties->RowCount = 0;
  FChanged = true;
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::mnuRemoveAttrClick(TObject *Sender)
{
  TXpAttribute* oAttr;

  if ((FAttrNode != NULL) && FAttrNode->HasAttributes() &&
      (AttrProperties->Selected != -1) &&
      (AttrProperties->Selected < FAttrNode->Attributes->Length))
  {
    oAttr = FAttrNode->RemoveAttributeNode((TXpAttribute*)(FAttrNode->Attributes->Item(AttrProperties->Selected)));
    oAttr->Release();
    AttrProperties->RowCount = FAttrNode->Attributes->Length;
    TreeView->Invalidate();
    FChanged = true;
  }
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::PageControlChange(TObject *Sender)
{
  TCursor oSavCursor;
  String sTmp;

  if (PageControl->ActivePage == SourceTab)
  {
    FLoadingSrc = true;
    try
    {
      EditView->Text = "";
      StatusBar->SimpleText = "";
      if (FXml != NULL)
      {
        SetCapture(Handle);
        oSavCursor = Screen->Cursor;
        Screen->Cursor = crHourGlass;
        try
        {
          sTmp = FXml->XmlDocument;
          EditView->Text = sTmp;
          if (SendMessage(EditView->Handle, EM_GETLIMITTEXT, 0, 0) <=
             (sTmp.Length() + 32768))
            SendMessage(EditView->Handle, EM_SETLIMITTEXT, sTmp.Length() + 32736, 0);
        }
        __finally
        {
          Screen->Cursor = oSavCursor;
          ReleaseCapture();
        }
      }
      EditView->Modified = false;
      EditView->SetFocus();
    }
    __finally
    {
      FLoadingSrc = false;
    }
  }
  SetCtrlStates();
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::PageControlChanging(TObject *Sender,
      bool &AllowChange)
{
  String aBuffer;
  int wRet;
  TXpObjModel* oWorkXml;
  TCursor oSavCursor;

  AllowChange = true;
  if ((PageControl->ActivePage == SourceTab) && EditView->Modified)
  {
    wRet = MessageDlg("The document has changed.  Would you like to parse the changes back into the object model?",
                      mtConfirmation, TMsgDlgButtons() << mbYes << mbNo << mbCancel, 0);
    if ((wRet == mrCancel) || ((wRet == mrNo) && (FXml == NULL)))
    {
      AllowChange = false;
      return;
    }

    if (wRet == mrYes)
    {
      if (FXml == XmlObjModelA)
        oWorkXml = XmlObjModelB;
      else
        oWorkXml = XmlObjModelA;

      try
      {
        SetCapture(Handle);
        oSavCursor = Screen->Cursor;
        Screen->Cursor = crHourGlass;
        try
        {
          aBuffer = EditView->Text;
          oWorkXml->LoadMemory(aBuffer.c_str(), aBuffer.Length());
          FXml = oWorkXml;
#ifdef XPDPRO
          UpdatePreviewPage();
#endif
          FChanged = true;
          TreeView->Items->Clear();
          UpdateTreeFromObjModel(NULL);
          GetTreeNodeForNode(FXml->Document->DocumentElement)->Expand(false);
        }
        __finally
        {
          Screen->Cursor = oSavCursor;
          ReleaseCapture();
        }
      }
      catch(EXpParserError &X)
      {
        StatusBar->SimpleText = "Error on line: " + IntToStr(X.Line) +
                                " position: " + IntToStr(X.LinePos) +
                                " abs at: " + IntToStr(X.FilePos) +
                                " error msg: " + X.Reason;
        EditView->SelStart = X.FilePos;
        EditView->SetFocus();
        AllowChange = false;
      }
    }
  }
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::TreeViewChange(TObject *Sender, TTreeNode *Node)
{
  TTreeNode* aTreeNode = TreeView->Selected;
  TXpNode* aNode;

  FAttrNode = NULL;
  if (aTreeNode != NULL)
  {
    aNode = (TXpNode*)aTreeNode->Data;
    if (aNode->NodeType == ELEMENT_NODE)
    {
      FAttrNode = (TXpElement*)aNode;
      if (FAttrNode->HasAttributes())
        AttrProperties->RowCount = FAttrNode->Attributes->Length;
      else
        AttrProperties->RowCount = 0;
    }
    else
      AttrProperties->RowCount = 0;
  }
  SetCtrlStates();
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::TreeViewClick(TObject *Sender)
{
  RefreshList();
  SetCtrlStates();
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::TreeViewEdited(TObject *Sender, TTreeNode *Node,
      AnsiString &S)
{
  S = Trim(S);
  if (SetTreeNodeText((TXpNode*)Node->Data, S))
  {
    FChanged = true;
    SetCtrlStates();
  }
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::TreeViewEditing(TObject *Sender,
      TTreeNode *Node, bool &AllowEdit)
{
  TXpNode* oNode = (TXpNode*)Node->Data;

  AllowEdit = false;
  switch(oNode->NodeType)
  {
    case PROCESSING_INSTRUCTION_NODE :
    case ELEMENT_NODE :
      AllowEdit = true;
      break;
  }
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::TreeViewGetImageIndex(TObject *Sender,
      TTreeNode *Node)
{
  if (Node->Data != NULL)
    Node->ImageIndex = GetTreeNodeImageIndex((TXpNode*)Node->Data);
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::TreeViewGetSelectedIndex(TObject *Sender,
      TTreeNode *Node)
{
  if (Node->Data != NULL)
    Node->SelectedIndex = GetTreeNodeImageIndex((TXpNode*)Node->Data);
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::TreeViewKeyPress(TObject *Sender, char &Key)
{
  inherited::KeyPress(Key);
  if ((Key == 13) && (TreeView->Selected != NULL) &&
      (!TreeView->IsEditing()))
  {
    TreeView->Selected->EditText();
    Key = 0;
  }
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::TreeViewMouseDown(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
{
  TTreeNode* aNode;

  if (Button == mbRight)
  {
    aNode = TreeView->GetNodeAt(X,Y);
    if (aNode != NULL)
    {
      TreeView->Selected = aNode;
      SetCtrlStates();
      TreeView->PopupMenu->Popup(TreeView->ClientToScreen(Point(X, Y)).x + 5,
                                 TreeView->ClientToScreen(Point(X, Y)).y + 5);
    }
  }
}
//---------------------------------------------------------------------------

void __fastcall TXMLChild::TreeViewCustomDraw(TCustomTreeView *Sender,
      const TRect &ARect, bool &DefaultDraw)
{
  RefreshList();
}
//---------------------------------------------------------------------------

WideString __fastcall TXMLChild::GetXmlData(void)
{
  WideString Result = "";
  if (FXml != NULL)
  {
    if (PageControl->ActivePage == SourceTab)
      Result = EditView->Text;
    else
      Result = FXml->XmlDocument;
  }
  return(Result);
}
#ifdef XPDPRO
//---------------------------------------------------------------------------
void __fastcall TXMLChild::IntegratePreviewPage(void)
{
  // Assumption: This method called only after a document has been loaded into
  // the child window.

  // If this is an XML document then integrate the XSL preview page into the
  // notebook.
  if (UpperCase(ExtractFileExt(FFileName)) == ".XML")
  {
    FPreviewTab = new TTabSheet(this);
    FPreviewTab->Caption = "XSL Preview";
    FPreviewTab->PageControl = PageControl;
    FPreviewForm = new TfrmPreview(this);
    FPreviewForm->Parent = FPreviewTab;
    FPreviewForm->Align = alClient;
    FPreviewForm->BorderStyle = bsNone;
    FPreviewForm->Show();
    UpdatePreviewPage();
  }
}
//---------------------------------------------------------------------------
void __fastcall TXMLChild::UpdatePreviewPage(void)
{
  // Does this document have a preview page?
  if (FPreviewForm != NULL)
  {
    // Assign DOM & filename.
    FPreviewForm->DOM = FXml;
    FPreviewForm->Filename = FFileName;
  }
}
//---------------------------------------------------------------------------
void __fastcall TXMLChild::AddStylesheet(const String sName, TForm* oChildWin)
{
  if (FPreviewForm != NULL)
    FPreviewForm->AddStylesheet(sName, oChildWin);
}
//---------------------------------------------------------------------------
void __fastcall TXMLChild::RemoveStylesheet(const String sName)
{
  if (FPreviewForm != NULL)
    FPreviewForm->RemoveStylesheet(sName);
}
//---------------------------------------------------------------------------
void __fastcall TXMLChild::RenameStylesheet(const String sOldName, const String sNewName)
{
  if (FPreviewForm != NULL)
    FPreviewForm->RenameStylesheet(sOldName, sNewName);
}
#endif



