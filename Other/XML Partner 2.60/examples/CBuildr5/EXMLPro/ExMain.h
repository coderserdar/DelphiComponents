/*********************************************************/
/* XMLPartner: ExMain.CPP 2.55                           */
/* Copyright (c) TurboPower Software Co 2002             */
/* All rights reserved.                                  */
/*********************************************************/
/* XMLPartner: XML Editor Main form                      */
/*********************************************************/
//---------------------------------------------------------------------------
#ifndef ExMainH
#define ExMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <Dialogs.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "XpBase.hpp"
#include "XpDom.hpp"
#include "XpAboutw.hpp"
#include <ComCtrls.hpp>
#include <Menus.hpp>
#include <ToolWin.hpp>
#include <inifiles.hpp>
#include <ImgList.hpp>
#include "XpDOM.hpp"
//---------------------------------------------------------------------------
#ifdef XPDPRO
#pragma option push -b-
enum TXpStyleAction { xpsaOpen, xpsaClose, xpsaSaveAs };
#pragma option pop
#endif


class TMainForm : public TForm
{
__published:	// IDE-managed Components
    TStatusBar *StatusBar;
    TToolBar *ToolBar2;
    TToolButton *btnNew;
    TToolButton *btnOpen;
    TToolButton *btnSave;
    TToolButton *ToolButton3;
    TToolButton *btnCut;
    TToolButton *btnCopy;
    TToolButton *btnPaste;
    TToolButton *ToolButton7;
    TToolButton *btnCascade;
    TToolButton *btnTileHorz;
    TToolButton *btnTileVert;
    TImageList *ImageList1;
    TMainMenu *MainMenu1;
    TMenuItem *mnuFile;
    TMenuItem *mnuFileNew;
    TMenuItem *mnuFileOpen;
    TMenuItem *mnuFileOpenURL;
    TMenuItem *mnuFileClose;
    TMenuItem *mnuFileSave;
    TMenuItem *mnuFileSaveAs;
    TMenuItem *mnuFileSaveURL;
    TMenuItem *N1;
    TMenuItem *mnuFilePref;
    TMenuItem *N3;
    TMenuItem *Recent1;
    TMenuItem *Recent2;
    TMenuItem *Recent3;
    TMenuItem *Recent4;
    TMenuItem *Recent5;
    TMenuItem *S1;
    TMenuItem *FileExitItem;
    TMenuItem *Edit1;
    TMenuItem *CutItem;
    TMenuItem *CopyItem;
    TMenuItem *CopyWOChildrenItem;
    TMenuItem *PasteItem;
    TMenuItem *Window1;
    TMenuItem *mnuWindowCascade;
    TMenuItem *mnuWindowTileH;
    TMenuItem *mnuWindowTileV;
    TMenuItem *mnuWindowMinimize;
    TMenuItem *mnuWindowArrange;
    TMenuItem *Help1;
    TMenuItem *HelpAboutItem;
    TOpenDialog *OpenDialog;
    TSaveDialog *SaveDialog;
    TPopupMenu *XslPopupMenu;
        TXpObjModel *XslInfo;
    void __fastcall mnuFileSaveAsClick(TObject *Sender);
    void __fastcall btnCascadeClick(TObject *Sender);
    void __fastcall btnCopyClick(TObject *Sender);
    void __fastcall btnCutClick(TObject *Sender);
    void __fastcall btnNewClick(TObject *Sender);
    void __fastcall btnOpenClick(TObject *Sender);
    void __fastcall btnPasteClick(TObject *Sender);
    void __fastcall btnSaveClick(TObject *Sender);
    void __fastcall btnTileHorzClick(TObject *Sender);
    void __fastcall btnTileVertClick(TObject *Sender);
    void __fastcall CopyWOChildrenItemClick(TObject *Sender);
    void __fastcall FileExitItemClick(TObject *Sender);
    void __fastcall HelpAboutItemClick(TObject *Sender);
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
    void __fastcall FormShow(TObject *Sender);
    void __fastcall mnuFileCloseClick(TObject *Sender);
    void __fastcall mnuFileOpenURLClick(TObject *Sender);
    void __fastcall mnuFilePrefClick(TObject *Sender);
    void __fastcall mnuFileSaveURLClick(TObject *Sender);
    void __fastcall mnuWindowArrangeClick(TObject *Sender);

    void __fastcall mnuWindowMinimizeClick(TObject *Sender);
    void __fastcall Recent1Click(TObject *Sender);
        void __fastcall Edit1Click(TObject *Sender);
private:	// User declarations
    TIniFile* FOptions;

    void __fastcall ActiveMDIorChild(const String sCaption, const String sUserID,
                                     const String sPassword);
    void __fastcall CreateMDIChild(const String sName, String sUserId,
                                         String sPassword);
    void __fastcall DoSelectXslCommand(TObject* oOwner);
    void __fastcall InitMDIChild(TForm* aChild);
    void __fastcall MDIChildNormal(void);
#ifdef XPDPRO
    void __fastcall OnXMLChildClose(TObject* Sender);
#endif
    TXpElement* __fastcall  OnGetXSLDocElementEvent(TObject* Sender);
    int __fastcall OnMDIChildCountEvent(TObject* Sender);
    void __fastcall OnNextChildEvent(TObject* Sender);
    void __fastcall OnPrevChildEvent(TObject* Sender);
    void __fastcall OnSetCtrlStatesEvent(TObject* Sender);
    void __fastcall SetCtrlStates(void);
#ifdef XPDPRO
    void __fastcall StylesheetNotify(const String sName1, const String sName2,
                                     TForm* oChildWin,
                                     TXpStyleAction eAction);
#endif
    void __fastcall UpdateRecents(bool bChange, String sFile);
public:		// User declarations
    __fastcall TMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif

