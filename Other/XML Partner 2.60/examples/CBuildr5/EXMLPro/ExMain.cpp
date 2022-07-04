/*********************************************************/
/* XMLPartner: ExMain.CPP 2.55                           */
/* Copyright (c) TurboPower Software Co 2002             */
/* All rights reserved.                                  */
/*********************************************************/
/* XMLPartner: XML Editor Main form                      */
/*********************************************************/
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ExMain.h"
#include "ExChildW.h"
#include "exErr.h"
#include "ExSelAtt.h"
#include "ExUtil.h"
#include <clipbrd.hpp>
#include "ExURL.h"
#include "exPrefs.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "XpBase"
#pragma link "XpDom"
#pragma link "XpAboutw"
#pragma link "XpDOM"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
const String csINIFile = "EXML.INI";
const String csAppSection = "Application";
const String csPrefSection = "Preferences";
const String csSectionRecent = "RecentFiles";
const String csFormatted = "Formatted";
const String csNormalize = "Normalize";
//---------------------------------------------------------------------------
TMainForm *MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::ActiveMDIorChild(const String sCaption,
                                            const String sUserID,
                                            const String sPassword)
{
  int anInx;
  TForm* oChild;
  String slCaption;

  slCaption = LowerCase(sCaption);
  oChild = NULL;
  // Is the file already loaded into a MDI child?
  for (anInx = 0; anInx < MDIChildCount; anInx++)
    if (MDIChildren[anInx]->Caption == slCaption)
    {
      oChild = MDIChildren[anInx];
      break;
    }

  if (oChild != NULL)
    oChild->BringToFront();
  else
    CreateMDIChild(slCaption, sUserID, sPassword);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::CreateMDIChild(const String sName, String sUserId,
                                          String sPassword)
{
  TXMLChild* Child;
#ifdef XPDPRO
  TXMLChild* Child2;
  int Inx;
#endif
  TfrmErrors* ErrForm;
  bool HasErrors;

  // Create a new MDI child window
  Child = new TXMLChild(Application);
  InitMDIChild(Child);
  HasErrors = !Child->LoadDataSource(sName, sUserId, sPassword);
  UpdateRecents(true, sName);
  if (MDIChildCount == 1)
    Child->WindowState = wsMaximized;
  if (HasErrors & (Child->Errors->Count > 0))
  {
    ErrForm = new TfrmErrors(this);
    try
    {
      ErrForm->Errors = Child->Errors;
      ErrForm->INIFile = FOptions;
      ErrForm->Caption = Format("Load errors for %s",ARRAYOFCONST((QuotedStr(sName))));
      ErrForm->ShowModal();
    }
    __finally
    {
      ErrForm->Free();
    }
  }
#ifdef XPDPRO
  if (LowerCase(ExtractFileExt(sName)) == ".xsl")
    StylesheetNotify(sName, "", Child, xpsaOpen);
  else if (MDIChildCount > 1)
    // If any stylesheets are open, make sure the new document sees them.
    for (Inx = 0; Inx < MDIChildCount; Inx++)
    {
      Child2 = (TXMLChild*)MDIChildren[Inx];
      if ((Child2 != Child) &
          (LowerCase(ExtractFileExt(Child2->FileName)) == ".xsl"))
        Child->AddStylesheet(ExtractFileName(Child2->FileName), Child2);
    }
#endif
  SetCtrlStates();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::DoSelectXslCommand(TObject* oOwner)
{
  String sCmd;
  TXpElement* oNewNode;
  TXpNodeList *oList, *oList2;

  sCmd = ((TMenuItem*)oOwner)->Caption;

  oNewNode = ((TXMLChild*)ActiveMDIChild)->CreateNewNode(sCmd);

  oList = XslInfo->Document->DocumentElement->GetElementsByTagNameWithAttribute
            ("exml:command", "name", sCmd);
  if (oList->Length > 0)
  {
    oList2 = ((TXpElement*)oList->Item(0))->SelectNodes("*");
    if (oList2->Length > 0)
    {
      // Display attributes dialog
      SelAttrsForm->Run(oList2, oNewNode);
    }
    oList2->Free();
  }
  oList->Free();
  ((TXMLChild*)ActiveMDIChild)->UpdateTreeFromObjModel(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::MDIChildNormal(void)
{
  int anInx;

  for (anInx = 0; anInx < MDIChildCount; anInx++)
    MDIChildren[anInx]->WindowState = wsNormal;
}
//---------------------------------------------------------------------------
TXpElement* __fastcall TMainForm::OnGetXSLDocElementEvent(TObject* Sender)
{
  return(XslInfo->Document->DocumentElement);
}
//---------------------------------------------------------------------------
int __fastcall TMainForm::OnMDIChildCountEvent(TObject* Sender)
{
  return(MDIChildCount);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::OnNextChildEvent(TObject* Sender)
{
  int i, j;
  TForm* ChildForm;

  ChildForm = reinterpret_cast<TForm*>(Sender);

  for (i = MDIChildCount - 1; i >= 0; i--)
    if (MDIChildren[i] == ChildForm)
    {
      j = i - 1;
      if (j < 0)
        j = MDIChildCount - 1;
      MDIChildren[j]->BringToFront();
      return;
    }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::OnPrevChildEvent(TObject* Sender)
{
  int i, j;
  TForm* ChildForm;

  ChildForm = reinterpret_cast<TForm*>(Sender);

  for (i = 0; i < MDIChildCount; i++)
    if (MDIChildren[i] == ChildForm)
    {
      j = i + 1;
      if (j >= MDIChildCount)
        j = 0;
      MDIChildren[j]->BringToFront();
      return;
    }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::OnSetCtrlStatesEvent(TObject* Sender)
{
  SetCtrlStates();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::SetCtrlStates(void)
{

  TXMLChild* ChildWin = (TXMLChild*)ActiveMDIChild;

  // File->Close
  mnuFileClose->Enabled = (ChildWin != NULL);

  // File->Save
  btnSave->Enabled = (ChildWin != NULL) && ChildWin->IsChanged;
  mnuFileSave->Enabled = btnSave->Enabled;

  // File->SaveAs
  mnuFileSaveAs->Enabled = (ChildWin != NULL);

  // File->SaveToURL
  mnuFileSaveURL->Enabled = mnuFileSaveAs->Enabled;

  // Edit->Copy
  btnCopy->Enabled = (ChildWin != NULL);
  CopyItem->Enabled = btnCopy->Enabled;


  // Edit->Copy without children
  CopyWOChildrenItem->Enabled = (ChildWin != NULL) &&
                                (ChildWin->TreeView->Selected != NULL) &&
                                (ChildWin->PageControl->ActivePage ==
                                 ChildWin->StructTab);

  // Edit->Cut
  btnCut->Enabled = (ChildWin != NULL) &&
                    (
                     (ChildWin->TreeView->Selected != NULL) ||
                     (ChildWin->PageControl->ActivePage == ChildWin->SourceTab)
                    );
  CutItem->Enabled = btnCut->Enabled;

  // Edit->Paste
  btnPaste->Enabled = (ChildWin != NULL) && Clipboard()->HasFormat(CF_UNICODETEXT);
  PasteItem->Enabled = btnPaste->Enabled;

}
//---------------------------------------------------------------------------
void __fastcall TMainForm::UpdateRecents(bool bChange, String sFile)
{
  int i, j;

  if (bChange)
  {
    // Check to see if project is in list yet
    for (i = 1; i <= 5; i++)
    {
      if (sFile == FOptions->ReadString(csSectionRecent, "File" + IntToStr(i), ""))
        break;
    }
    if (i > 5)
      i = 5;

    // If project is not in list then add it to the top
    for (j = i; j >= 2; j--)
      FOptions->WriteString(csSectionRecent, "File" + IntToStr(j),
                            FOptions->ReadString(csSectionRecent,
                                                 "File" + IntToStr(j - 1), ""));
    FOptions->WriteString(csSectionRecent, "File1", sFile);
  }

  // Set captions for menu items
  Recent1->Caption = "&1. " + FOptions->ReadString(csSectionRecent, "File1", "");
  Recent2->Caption = "&2. " + FOptions->ReadString(csSectionRecent, "File2", "");
  Recent3->Caption = "&3. " + FOptions->ReadString(csSectionRecent, "File3", "");
  Recent4->Caption = "&4. " + FOptions->ReadString(csSectionRecent, "File4", "");
  Recent5->Caption = "&5. " + FOptions->ReadString(csSectionRecent, "File5", "");

  // Determine which items are visible
  Recent1->Visible = Recent1->Caption.Length() > 4;
  Recent2->Visible = Recent2->Caption.Length() > 4;
  Recent3->Visible = Recent3->Caption.Length() > 4;
  Recent4->Visible = Recent4->Caption.Length() > 4;
  Recent5->Visible = Recent5->Caption.Length() > 4;
  S1->Visible = Recent1->Visible;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::mnuFileSaveAsClick(TObject *Sender)
{
  TXMLChild* oChild = dynamic_cast<TXMLChild*>(ActiveMDIChild);
#ifdef XPDPRO
  String sOldName;
#endif


  SaveDialog->InitialDir = ExtractFilePath(((TXMLChild*)(ActiveMDIChild))->FileName);
#ifdef XPDPRO
  sOldName = oChild->FileName;
#endif
  if (SaveDialog->Execute())
  {
    if (SaveDialog->FileName.Pos(".") == 0)
      SaveDialog->FileName = SaveDialog->FileName + ".xml";
    oChild->SaveFile(SaveDialog->FileName);
#ifdef XPDPRO
    if (LowerCase(ExtractFileExt(SaveDialog->FileName)) == ".xsl")
      StylesheetNotify(sOldName, oChild->FileName, oChild, xpsaSaveAs);
#endif
  }
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::btnCascadeClick(TObject *Sender)
{
  MDIChildNormal();
  Cascade();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::btnCopyClick(TObject *Sender)
{
  ((TXMLChild*)ActiveMDIChild)->CopyToClipboard(true);
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::btnCutClick(TObject *Sender)
{
  ((TXMLChild*)ActiveMDIChild)->CutToClipboard();

}
//---------------------------------------------------------------------------

void __fastcall TMainForm::btnNewClick(TObject *Sender)
{
  TXMLChild* Child;

  Child = new TXMLChild(Application);
  InitMDIChild(Child);
  Child->Caption = "Untitled Document " + IntToStr(MDIChildCount);
  Child->NewDocument();
  if (MDIChildCount == 1)
    Child->WindowState = wsMaximized;

}
//---------------------------------------------------------------------------

void __fastcall TMainForm::btnOpenClick(TObject *Sender)
{
  if (OpenDialog->Execute())
    ActiveMDIorChild(OpenDialog->FileName, "", "");
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::btnPasteClick(TObject *Sender)
{
  ((TXMLChild*)ActiveMDIChild)->PasteFromClipboard();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::btnSaveClick(TObject *Sender)
{
  if (((TXMLChild*)ActiveMDIChild)->FileName == "")
    mnuFileSaveAsClick(this);
  else
    ((TXMLChild*)ActiveMDIChild)->SaveFile("");
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::btnTileHorzClick(TObject *Sender)
{
  MDIChildNormal();
  TileMode = tbHorizontal;
  Tile();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::btnTileVertClick(TObject *Sender)
{
  MDIChildNormal();
  TileMode = tbVertical;
  Tile();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::CopyWOChildrenItemClick(TObject *Sender)
{
  ((TXMLChild*)ActiveMDIChild)->CopyToClipboard(false);
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::FileExitItemClick(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::HelpAboutItemClick(TObject *Sender)
{
  TXpAboutBox* aForm = new TXpAboutBox(this);

  try
  {
    aForm->ShowModal();
  }
  __finally
  {
    aForm->Free();
  }
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::FormClose(TObject *Sender, TCloseAction &Action)
{
  int anInx;
  
  if (FOptions != NULL)
    SaveFormState(this, FOptions, csAppSection);

  FOptions->Free();
  for (anInx = 0; anInx < MDIChildCount; anInx++)
    MDIChildren[anInx]->Close();
}
//---------------------------------------------------------------------------
String __fastcall MakeFilePath(String sPath, String sFile)
{
  String Result;

  if (sPath.Length() > 0)
  {
    if (sPath[sPath.Length()] == '\\')
      Result = sPath + sFile;
    else
      Result = sPath + "\\" + sFile;
  }
  else
    Result = ".\\" + sFile;
  return(Result);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormShow(TObject *Sender)
{
  int i;

  // Set window position
  try
  {
    FOptions = new TIniFile(MakeFilePath(ExtractFilePath(
                             Application->ExeName), csINIFile));
    if (FOptions->ReadBool(csPrefSection, "SaveAppWinPos", true))
    {
      RestoreFormState(this, FOptions, csAppSection);
      if (Width < 200)
        Width = 200;
      if (Height < 200)
        Height = 200;
    }
  }
  catch(...)
  {
    // Do nothing on exception
  }

  for (i = 1; i <= ParamCount(); i++)
    if (FileExists(ParamStr(i)))
      CreateMDIChild(ParamStr(i), "", "");
  UpdateRecents(false, "");
  SetCtrlStates();
#ifdef XPDPRO
  Caption = "EXMLPro Editor";
#else
  Caption = "EXML Editor";
#endif
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::mnuFileCloseClick(TObject *Sender)
{
#ifdef XPDPRO
  TXMLChild* oChild;

  oChild = dynamic_cast<TXMLChild*>(ActiveMDIChild);
  if (LowerCase(ExtractFileExt(oChild->FileName)) == ".xsl")
    StylesheetNotify(oChild->FileName, "", oChild, xpsaClose);
#endif
  ActiveMDIChild->Close();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::mnuFileOpenURLClick(TObject *Sender)
{
  TURLForm* aForm = new TURLForm(Application);

  try
  {
    if (aForm->ShowModal() == mrOk)
      ActiveMDIorChild(aForm->URL, aForm->FTPUser, aForm->FTPPassword);
  }
  __finally
  {
    aForm->Free();
  }
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::mnuFilePrefClick(TObject *Sender)
{
  TPrefsForm* aForm = new TPrefsForm(this);

  try
  {
    aForm->INIFile = FOptions;
    aForm->ShowModal();
  }
  __finally
  {
    aForm->Free();
  }
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::mnuFileSaveURLClick(TObject *Sender)
{
  TURLForm* aForm = new TURLForm(Application);

  try
  {
    if (aForm->ShowModal() == mrOk)
      if (!((TXMLChild*)ActiveMDIChild)->SaveToURL(aForm->URL, aForm->FTPUser,
                                                   aForm->FTPPassword))
        MessageDlg("Unable to save to URL.  Be sure to save a local copy!",
                   mtError, TMsgDlgButtons() << mbOK, 0);
  }
  __finally
  {
    aForm->Free();
  }
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::mnuWindowArrangeClick(TObject *Sender)
{
  MDIChildNormal();
  ArrangeIcons();
}
//---------------------------------------------------------------------------


void __fastcall TMainForm::mnuWindowMinimizeClick(TObject *Sender)
{
  int anInx;

  for (anInx = MDIChildCount - 1; anInx >= 0; anInx--)
    MDIChildren[anInx]->WindowState = wsMinimized;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::Recent1Click(TObject *Sender)
{
  TMenuItem* oMenuItem;

  oMenuItem = reinterpret_cast<TMenuItem*>(Sender);

  ActiveMDIorChild(oMenuItem->Caption.SubString(5, 257), "", "");
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::Edit1Click(TObject *Sender)
{
  SetCtrlStates();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::InitMDIChild(TForm* aChild)
{
  TXMLChild* XMLChild = dynamic_cast<TXMLChild*>(aChild);

  if (FOptions != NULL)
  {
    XMLChild->FormattedOutput = FOptions->ReadBool(csPrefSection, csFormatted, True);
    XMLChild->NormalizeData = FOptions->ReadBool(csPrefSection, csNormalize, True);
  }
  XMLChild->INIFile = FOptions;
  XMLChild->OnGetXSLDocElement = OnGetXSLDocElementEvent;
  XMLChild->OnMDIChildCount = OnMDIChildCountEvent;
  XMLChild->OnNextChild = OnNextChildEvent;
  XMLChild->OnPrevChild = OnPrevChildEvent;
  XMLChild->OnSaveAs = mnuFileSaveAsClick;
  XMLChild->OnSetCtrlStates = OnSetCtrlStatesEvent;
#ifdef XPDPRO
  XMLChild->OnClosing = OnXMLChildClose;
#endif
}

//---------------------------------------------------------------------------
#ifdef XPDPRO
void __fastcall TMainForm::OnXMLChildClose(TObject* Sender)
{
  TXMLChild* oChild = dynamic_cast<TXMLChild*>(Sender);

  if (LowerCase(ExtractFileExt(oChild->FileName)) == ".xsl")
    StylesheetNotify(oChild->FileName, "", oChild, xpsaClose);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::StylesheetNotify(const String sName1,
                                            const String sName2,
                                                  TForm* oChildWin,
                                                  TXpStyleAction eAction)
{
  TXMLChild* oForm;
  int wInx;

  for (wInx = 0; wInx < MDIChildCount; wInx++)
  {
    oForm = dynamic_cast<TXMLChild*>(MDIChildren[wInx]);
    if (oForm != oChildWin)
      switch(eAction)
      {
        case xpsaOpen :
          oForm->AddStylesheet(ExtractFileName(sName1), oChildWin);
          break;
        case xpsaClose :
          oForm->RemoveStylesheet(ExtractFileName(sName1));
          break;
        case xpsaSaveAs :
          oForm->RenameStylesheet(ExtractFileName(sName1),
                                  ExtractFileName(sName2));
          break;
      }
  }
}
#endif
//---------------------------------------------------------------------------


