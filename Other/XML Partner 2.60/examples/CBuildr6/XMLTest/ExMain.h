//---------------------------------------------------------------------------
#ifndef ExMainH
#define ExMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "XpBase.hpp"
#include "XpDom.hpp"
#include "XpParser.hpp"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include "XpDOM.hpp"
//---------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:	// IDE-managed Components
        TOpenDialog *odFile;
        TXpParser *Parser;
        TXpObjModel *DOM;
        TMainMenu *MainMenu;
        TMenuItem *File1;
        TMenuItem *Exit1;
        TPageControl *PageControl1;
        TTabSheet *TabSheet1;
        TTabSheet *TabSheet2;
        TPanel *Panel1;
        TLabel *Label1;
        TButton *btnParse;
        TEdit *edtParse;
        TButton *btnBrowse;
        TListBox *lbData;
        TPanel *Panel2;
        TLabel *lblFile;
        TLabel *lblXMLRender;
        TLabel *lblXQLExpr;
        TLabel *lblNumNodes;
        TLabel *NodesLabel;
        TEdit *ObjModelEdit;
        TButton *btnBrowse2;
        TButton *btnDOMLoad;
        TEdit *edtSearch;
        TButton *btnSearch;
        TMemo *memNodes;
        void __fastcall btnBrowseClick(TObject *Sender);
        void __fastcall btnParseClick(TObject *Sender);
        void __fastcall ParserAttribute(TObject *oOwner, WideString sName,
          WideString sValue, bool bSpecified);
        void __fastcall ParserCDATASection(TObject *oOwner,
          WideString sValue);
        void __fastcall ParserCharData(TObject *oOwner, WideString sValue);
        void __fastcall ParserComment(TObject *oOwner, WideString sValue);
        void __fastcall ParserDocTypeDecl(TObject *oOwner,
          WideString sDecl, WideString sId0, WideString sId1);
        void __fastcall ParserEndDocument(TObject *Sender);
        void __fastcall ParserEndElement(TObject *oOwner,
          WideString sValue);
        void __fastcall ParserNonXMLEntity(TObject *oOwner,
          WideString sEntityName, WideString sPublicId,
          WideString sSystemId, WideString sNotationName);
        void __fastcall ParserProcessingInstruction(TObject *oOwner,
          WideString sName, WideString sValue);
        void __fastcall ParserStartDocument(TObject *Sender);
        void __fastcall ParserStartElement(TObject *oOwner,
          WideString sValue);
        void __fastcall btnBrowse2Click(TObject *Sender);
        void __fastcall btnDOMLoadClick(TObject *Sender);
        void __fastcall btnSearchClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif
