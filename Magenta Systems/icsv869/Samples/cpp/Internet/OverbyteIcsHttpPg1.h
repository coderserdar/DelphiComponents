//---------------------------------------------------------------------------
#ifndef OverbyteIcsHttpPg1H
#define OverbyteIcsHttpPg1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include "OverbyteIcsHttpProt.hpp"
#include "OverbyteIcsWndControl.hpp"
//---------------------------------------------------------------------------
class THttpTestForm : public TForm
{
__published:	// IDE-managed Components
    TMemo *DisplayMemo;
    TPanel *Panel1;
    TLabel *Label1;
    TLabel *Label2;
    TLabel *Label3;
    TEdit *UserIDEdit;
    TEdit *EMailEdit;
    TEdit *MessageEdit;
    TButton *SendButton;
    THttpCli *HttpCli1;
    TLabel *Label4;
    TEdit *ProxyEdit;
    TButton *AbortButton;
    void __fastcall FormCreate(TObject *Sender);
    void __fastcall SendButtonClick(TObject *Sender);
    void __fastcall AbortButtonClick(TObject *Sender);
    void __fastcall FormShow(TObject *Sender);
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
private:	// User declarations
    BOOL       FInitialized;
    AnsiString FIniFileName;
public:		// User declarations
    __fastcall THttpTestForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern THttpTestForm *HttpTestForm;
//---------------------------------------------------------------------------
#endif
