//---------------------------------------------------------------------------
#ifndef httptst1H
#define httptst1H
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\ExtCtrls.hpp>
#include <HttpProt.hpp>
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
        void __fastcall HttpCli1Command(TObject *Sender, AnsiString &S);
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
