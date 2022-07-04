/*********************************************************/
/* XMLPartner: ExMain.H 2.55                             */
/* Copyright (c) TurboPower Software Co 2002             */
/* All rights reserved.                                  */
/*********************************************************/
/* XMLPartner: XML Editor Child window                   */
/*********************************************************/
//---------------------------------------------------------------------------
#ifndef ExChildWH
#define ExChildWH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "XpBase.hpp"
#include "XpDom.hpp"
#include "ExProps.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include <ToolWin.hpp>
#include <inifiles.hpp>
#include <ImgList.hpp>
#include "XpDOM.hpp"
#ifdef XPDPRO
#include "ExView.h"
#endif
//---------------------------------------------------------------------------
typedef int __fastcall (__closure *TXpMDIChildCount)(TObject* Sender);

typedef TXpElement* __fastcall (__closure *TXpGetXSLDocElement)(TObject* Sender);

class TXMLChild : public TForm
{
__published:	// IDE-managed Components
    TXpObjModel *XmlObjModelB;
    TXpObjModel *XmlObjModelA;
    TPopupMenu *TreePopupMenu;
    TMenuItem *mnuDeleteNode;
    TMenuItem *mnuPreserveSpace;
    TMenuItem *mnuTreeSep1;
    TMenuItem *mnuCut;
    TMenuItem *mnuCopy;
    TMenuItem *mnuPaste;
    TPopupMenu *AttrPopupMenu;
    TMenuItem *mnuAddAttr;
    TMenuItem *mnuRemoveAttr;
    TMenuItem *mnuEditAttr;
    TMenuItem *mnuSelectXSLAttr;
    TMenuItem *mnuAddGUID;
    TPopupMenu *InsertPopupMenu;
    TMenuItem *mnuInsertElement;
    TMenuItem *mnuAddElementasChild;
    TMenuItem *mnuInsertText;
    TMenuItem *mnuAddTextAsChild;
    TMenuItem *mnuInsertComment;
    TMenuItem *mnuInsertPI;
    TMenuItem *mnuInsertCDATA;
    TMenuItem *mnuAddCDATA;
    TImageList *ToolbarImageList;
    TImageList *NodeImageList;
    TToolBar *ToolBar1;
    TToolButton *ToolButton1;
    TToolButton *btnInsert;
    TToolButton *btnAddChildElement;
    TToolButton *btnAddChildText;
    TToolButton *btnDuplicate;
    TToolButton *ToolButton2;
    TToolButton *btnPromote;
    TToolButton *btnDemote;
    TToolButton *btnMoveUp;
    TToolButton *btnMoveDown;
    TToolButton *btnCollapseAll;
    TToolButton *btnExpandAll;
    TToolButton *ToolButton3;
    TToolButton *btnNormalize;
    TToolButton *btnXSL;
    TPageControl *PageControl;
    TTabSheet *StructTab;
    TPanel *Panel1;
    TSplitter *Splitter1;
    TPanel *Panel2;
    TSplitter *Splitter2;
    THeaderControl *HeaderControl1;
    TTreeView *TreeView;
    TPanel *Panel3;
    TListView *ListView;
    TTabSheet *SourceTab;
    TStatusBar *StatusBar;
    TMemo *EditView;
    void __fastcall mnuDeleteNodeClick(TObject* Sender);
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
    void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
    void __fastcall FormCreate(TObject *Sender);
    void __fastcall FormDestroy(TObject *Sender);
    void __fastcall FormShow(TObject *Sender);
    void __fastcall AttrPropertiesClick(TObject *Sender);
    void __fastcall AttrPropertiesPropertyName(TObject* oOwner, int wIndex,
                                               String &sData);
    void __fastcall AttrPropertiesPropertyValue(TObject* oOwner, int wIndex,
                                                String &sData);
    void __fastcall AttrPropertiesValueQueryEdit(TObject* oOwner, int wIndex,
                                                 EXpEditType &oEditType,
                                                 String &sFunctionName);
    void __fastcall AttrPropertiesValueChange(TObject* oOwner, int wIndex,
                                              String sValue);
    void __fastcall btnAddChildElementClick(TObject *Sender);
    void __fastcall btnAddChildTextClick(TObject *Sender);
    void __fastcall btnCollapseAllClick(TObject *Sender);
    void __fastcall btnDemoteClick(TObject *Sender);
    void __fastcall btnDuplicateClick(TObject *Sender);
    void __fastcall btnExpandAllClick(TObject *Sender);
    void __fastcall btnMoveDownClick(TObject *Sender);
    void __fastcall btnMoveUpClick(TObject *Sender);
    void __fastcall btnNormalizeClick(TObject *Sender);
    void __fastcall btnPromoteClick(TObject *Sender);
    void __fastcall EditViewChange(TObject *Sender);
    void __fastcall ListViewData(TObject* Sender, TListItem* Item);
    void __fastcall ListViewDblClick(TObject *Sender);
    void __fastcall ListViewEdited(TObject *Sender, TListItem *Item,
          AnsiString &S);
    void __fastcall ListViewEditing(TObject *Sender, TListItem *Item,
          bool &AllowEdit);
    void __fastcall ListViewKeyPress(TObject *Sender, char &Key);
    void __fastcall mnuAddAttrClick(TObject *Sender);
    void __fastcall mnuAddCDATAClick(TObject *Sender);
    void __fastcall mnuAddElementasChildClick(TObject *Sender);
    void __fastcall mnuAddGUIDClick(TObject *Sender);

    void __fastcall mnuCopyClick(TObject *Sender);
    void __fastcall mnuCutClick(TObject *Sender);
    void __fastcall mnuEditAttrClick(TObject *Sender);
    void __fastcall mnuInsertCDATAClick(TObject *Sender);
    void __fastcall mnuInsertCommentClick(TObject *Sender);
    void __fastcall mnuInsertElementClick(TObject *Sender);
    void __fastcall mnuInsertPIClick(TObject *Sender);
    void __fastcall mnuInsertTextClick(TObject *Sender);
    void __fastcall mnuPasteClick(TObject *Sender);
    void __fastcall mnuPreserveSpaceClick(TObject *Sender);
    void __fastcall mnuRemoveAttrClick(TObject *Sender);
    void __fastcall PageControlChange(TObject *Sender);
    void __fastcall PageControlChanging(TObject *Sender,
          bool &AllowChange);
    void __fastcall TreeViewChange(TObject *Sender, TTreeNode *Node);
    void __fastcall TreeViewClick(TObject *Sender);
    void __fastcall TreeViewEdited(TObject *Sender, TTreeNode *Node,
          AnsiString &S);
    void __fastcall TreeViewEditing(TObject *Sender, TTreeNode *Node,
          bool &AllowEdit);
    void __fastcall TreeViewGetImageIndex(TObject *Sender,
          TTreeNode *Node);
    void __fastcall TreeViewGetSelectedIndex(TObject *Sender,
          TTreeNode *Node);
    void __fastcall TreeViewKeyPress(TObject *Sender, char &Key);
    void __fastcall TreeViewMouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y);
        void __fastcall TreeViewCustomDraw(TCustomTreeView *Sender,
          const TRect &ARect, bool &DefaultDraw);
private:	// User declarations
    TXpPropertiesWindow* AttrProperties;
    TXpElement* FAttrNode;
    bool FChanged;
    String FFileName;
    TIniFile* FINIFile;
    int FLastIndex;
    TTreeNode* FLastItem;
    bool FLoadingSrc;
    TNotifyEvent FOnClosing;
    TXpGetXSLDocElement FOnGetXSLDocElement;
    TXpMDIChildCount FOnMDIChildCount;
    TNotifyEvent FOnNextChild;
    TNotifyEvent FOnPrevChild;
    TNotifyEvent FOnSaveAs;
    TNotifyEvent FOnSetCtrlStates;
#ifdef XPDPRO
    TfrmPreview* FPreviewForm;
    TTabSheet *FPreviewTab;
#endif
    TXpObjModel* FXml;

    void __fastcall EditNodeDialog(TXpNode* oNode);
    TTreeNode* __fastcall GetTreeNodeForNode(TXpNode* oNode);
    TStringList* __fastcall GetErrors(void);
    bool __fastcall GetFormattedOutput(void);
    String __fastcall GetListViewText(TXpNode* oNode);
    bool __fastcall GetNormalize(void);
    int __fastcall GetTreeNodeImageIndex(TXpNode* oNode);
    String __fastcall GetTreeNodeText(TXpNode* oNode);
    WideString __fastcall GetXmlData(void);
#ifdef XPDPRO
    void __fastcall IntegratePreviewPage(void);
#endif
    void __fastcall SetCtrlStates(void);
    void __fastcall SetFormattedOutput(bool aValue);
    void __fastcall SetListViewText(TXpNode* oNode, String sValue);
    void __fastcall SetNormalize(bool aValue);
    bool __fastcall SetTreeNodeText(TXpNode* oNode, String &sValue);
#ifdef XPDPRO
    void __fastcall UpdatePreviewPage(void);
      // Updates the preview page with the latest object model & filename.
#endif
    void __fastcall UpdateTreeChildrenFromObjModel(TXpNode* oNode, TTreeNode* oTreeNode);

public:		// User declarations

    __fastcall TXMLChild(TComponent* Owner);

    bool __fastcall LoadDataSource(String sFile, String sUserId, String sPassword);
    bool __fastcall SaveFile(String sFile);
    bool __fastcall SaveToURL(String sURL, String sUserId, String sPassword);
    void __fastcall NewDocument(void);

#ifdef XPDPRO
    void __fastcall AddStylesheet(const String sName, TForm* oChildWin);
      // Add a stylesheet to this window's list of stylesheets.

    void __fastcall RemoveStylesheet(const String sName);
      // Remove a stylesheet from this window's list of stylesheets.

    void __fastcall RenameStylesheet(const String sOldName, const String sNewName);
      // Rename a stylesheet in this window's list of stylesheets.
#endif

    void __fastcall CopyToClipboard(bool bIncludeChildren);
    void __fastcall CutToClipboard(void);
    void __fastcall PasteFromClipboard(void);
    void __fastcall UpdateTreeFromObjModel(TXpNode* oNewSelNode);
    void __fastcall RefreshList(void);
    TXpElement* __fastcall CreateNewNode(String sElemName);

    __property TStringList* Errors={read=GetErrors};
    __property String FileName={read=FFileName};
    __property bool FormattedOutput={read=GetFormattedOutput,write=SetFormattedOutput,nodefault};
    __property TIniFile* INIFile={read=FINIFile,write=FINIFile,nodefault};
    __property bool IsChanged={read=FChanged};
    __property bool NormalizeData={read=GetNormalize,write=SetNormalize,nodefault};

    /* Events */
    __property TNotifyEvent OnClosing={read=FOnClosing, write=FOnClosing, nodefault};
      // Raised when the child window is closing. Allows parent window to notify
      // other child windows of the event.
    __property TXpGetXSLDocElement OnGetXSLDocElement={read=FOnGetXSLDocElement,write=FOnGetXSLDocElement,nodefault};
    __property TXpMDIChildCount OnMDIChildCount={read=FOnMDIChildCount,write=FOnMDIChildCount,nodefault};
    __property TNotifyEvent OnNextChild={read=FOnNextChild,write=FOnNextChild,nodefault};
    __property TNotifyEvent OnPrevChild={read=FOnPrevChild,write=FOnPrevChild,nodefault};
    __property TNotifyEvent OnSaveAs={read=FOnSaveAs,write=FOnSaveAs,nodefault};
    __property TNotifyEvent OnSetCtrlStates={read=FOnSetCtrlStates,write=FOnSetCtrlStates,nodefault};
    __property WideString XMLData={read=GetXmlData};

};
//---------------------------------------------------------------------------
#define CDDS_PREPAINT 0x00000001
#define CDRF_DODEFAULT 0x00000000
//---------------------------------------------------------------------------
extern PACKAGE TXMLChild *XMLChild;
//---------------------------------------------------------------------------
#endif
