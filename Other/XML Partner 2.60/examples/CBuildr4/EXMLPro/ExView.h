//---------------------------------------------------------------------------
#ifndef ExViewH
#define ExViewH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "XpBase.hpp"
#include "XpvFlBas.hpp"
#include "XpvFlHTM.hpp"
#include "XpvFlPrt.hpp"
#include "XpvFlRTF.hpp"
#include "XpvFlXML.hpp"
#include "XpvXSLPr.hpp"
#include "XpvXSLT.hpp"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
#include <ToolWin.hpp>
#include "XpvFlBas.hpp"
#include "XpvFlHTM.hpp"
#include "XpvFlPrt.hpp"
#include "XpvFlRTF.hpp"
#include "XpvFlXML.hpp"
#include "XpvXSLPr.hpp"
#include "XpvXSLT.hpp"
#ifdef XPDPRO
#include "ExView.h"
#endif
#include <ShellAPI.hpp>
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
#include <ToolWin.hpp>
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TXpFilterDisplayMethod)(TXpFilterBase* aFilter);
  // This type describes the interface for a method that implements the
  // display of a transformed XML document.

class TfrmPreview : public TForm
{
__published:	// IDE-managed Components
        TComboBox *cboFilter;
        TComboBox *cboStylesheet;
        TRichEdit *RichEdit;
        TPageControl *pgPreview;
        TMemo *memText;
        TMemo *memXML;
        TXpFilterPrint *filtPrinter;
        TToolBar *tbPrint;
        TToolButton *pbFirstPage;
        TToolButton *pbLastPage;
        TToolButton *pbPrevPage;
        TToolButton *pbNextPage;
        TLabel *lblSpacer;
        TLabel *lblSpacer2;
        TPanel *pnlPage;
        TOpenDialog *dlgOpen;
        TXpXSLProcessor *xslprocessor;
        TXpFilterHTML *filtHTML;
        TXpFilterRTF *filtRTF;
        TXpFilterText *filtText;
        TXpFilterXML *filtXML;
        TLabel *lblFilter;
        TLabel *lblStylesheet;
        TButton *pbBrowse;
        TButton *pbRender;
        TPanel *pnlTop;
        TImageList *imMain;
        TPanel *pnlPreviewContainer;
        TScrollBar *PrinterHScroll;
        TTabSheet *tabPrinter;
        TTabSheet *tabRTF;
        TTabSheet *tabText;
        TTabSheet *tabXML;
        TScrollBar *PrinterVScroll;
        void __fastcall cboFilterChange(TObject *Sender);
        void __fastcall pbBrowseClick(TObject *Sender);
        void __fastcall pbRenderClick(TObject *Sender);
        void __fastcall filtPrinterResize(TObject *Sender);
        void __fastcall PrinterVScrollChange(TObject *Sender);
        void __fastcall PrinterHScrollChange(TObject *Sender);
        void __fastcall pbFirstPageClick(TObject *Sender);
        void __fastcall pbPrevPageClick(TObject *Sender);
        void __fastcall pbNextPageClick(TObject *Sender);
        void __fastcall pbLastPageClick(TObject *Sender);
        void __fastcall FormShow(TObject *Sender);
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
private:	// User declarations
    String FCurSheet;
      // Used by SaveStylesheetSelection and RestoreStylesheetSelection.

    TXpFilterPrint* FPrinterFilter;
      // The printer filter. For use by the printer page controls.
    TXpObjModel* FDOM;
      // The DOM containing the XML that is to be transformed.
    String FFilename;
      // The name of the document being viewed.
    int FLastPage;
      // The last visible notebook page.

    void __fastcall DisplayHTML(TXpFilterBase* aFilter);
    void __fastcall DisplayPrinter(TXpFilterBase* aFilter);
    void __fastcall DisplayRTF(TXpFilterBase* aFilter);
    void __fastcall DisplayText(TXpFilterBase* aFilter);
    void __fastcall DisplayXML(TXpFilterBase* aFilter);
    WideString __fastcall GetStyleSheet(void);
    void __fastcall RestoreStylesheetSelection(void);
    void __fastcall SaveStylesheetSelection(void);
    void __fastcall ShowTab(void);
    void __fastcall UpdatePrinterUI(void);
public:		// User declarations
    __fastcall TfrmPreview(TComponent* Owner);

    void __fastcall AddStylesheet(const String sName, TForm* oChildWin);
      // Add a stylesheet to this window's list of stylesheets.

    void __fastcall CopyToClipboard(void);
      // Copies text from the output control currently being viewed.

    void __fastcall RemoveStylesheet(const String sName);
      // Remove a stylesheet from this window's list of stylesheets.

    void __fastcall RenameStylesheet(const String sOldName, const String sNewName);
      // Rename a stylesheet in this window's list of stylesheets.

    __property TXpObjModel* DOM={read=FDOM, write=FDOM};
      // The DOM containing the XML that is to be transformed.

    __property String Filename={read=FFilename, write=FFilename};
      // The name of the XML document being viewed.
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmPreview *frmPreview;
//---------------------------------------------------------------------------
#endif
