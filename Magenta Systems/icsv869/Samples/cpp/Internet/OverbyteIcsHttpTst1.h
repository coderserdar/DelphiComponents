//---------------------------------------------------------------------------
#ifndef OverbyteIcsHttpTst1H
#define OverbyteIcsHttpTst1H
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
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label4;
	TLabel *Label5;
	TLabel *Label6;
	TButton *GetButton;
	TEdit *URLEdit;
	TEdit *ProxyHostEdit;
	TEdit *ProxyPortEdit;
	TButton *PostButton;
	TButton *Check64Button;
	TEdit *DataEdit;
	TEdit *DateTimeEdit;
	TButton *HeadButton;
	TButton *AbortButton;
	TMemo *DisplayMemo;
	TMemo *DocumentMemo;
	THttpCli *HttpCli1;
	void __fastcall FormShow(TObject *Sender);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall HeadButtonClick(TObject *Sender);
	void __fastcall GetButtonClick(TObject *Sender);
	void __fastcall PostButtonClick(TObject *Sender);
	void __fastcall HttpCli1DocBegin(TObject *Sender);
	void __fastcall HttpCli1DocEnd(TObject *Sender);
	void __fastcall Check64ButtonClick(TObject *Sender);
	void __fastcall HttpCli1RequestDone(TObject *Sender, THttpRequest RqType,
	WORD Error);
	void __fastcall AbortButtonClick(TObject *Sender);
private:	// User declarations
    BOOL Initialized;
    AnsiString FIniFileName;
    void __fastcall SetButtonState(BOOL State);
public:		// User declarations
    __fastcall THttpTestForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern THttpTestForm *HttpTestForm;
//---------------------------------------------------------------------------
#endif
