//---------------------------------------------------------------------------
#ifndef MainH
#define MainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "XpBase.hpp"
#include "XpParser.hpp"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
        TEdit *edtFile;
        TLabel *Label1;
        TButton *btnOpenFile;
        TButton *btnParse;
        TRichEdit *memo;
        TXpParser *Parser;
        TOpenDialog *fodXMLDoc;
        void __fastcall btnParseClick(TObject *Sender);
        void __fastcall btnOpenFileClick(TObject *Sender);
        void __fastcall ParserAttribute(TObject *oOwner, WideString sName,
          WideString sValue, bool bSpecified);
        void __fastcall ParserStartElement(TObject *oOwner,
          WideString sValue);
        void __fastcall ParserEndElement(TObject *oOwner,
          WideString sValue);
        void __fastcall ParserCharData(TObject *oOwner, WideString sValue);
private:	// User declarations
public:		// User declarations
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
