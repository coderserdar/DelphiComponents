//---------------------------------------------------------------------------
#ifndef Finger1H
#define Finger1H
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\ExtCtrls.hpp>
#include "FingCli.hpp"
#include "WSocket.hpp"
//---------------------------------------------------------------------------
class TFingerDemoForm : public TForm
{
__published:	// IDE-managed Components
	TMemo *DisplayMemo;
	TPanel *Panel1;
	TEdit *QueryEdit;
	TButton *QueryButton;
	TButton *CancelButton;
	TFingerCli *FingerCli1;
	TWSocket *WSocket1;
	void __fastcall QueryButtonClick(TObject *Sender);
	void __fastcall FingerCli1SessionConnected(TObject *Sender, WORD Error);
	void __fastcall FingerCli1DataAvailable(TObject *Sender, WORD Error);
	void __fastcall FingerCli1QueryDone(TObject *Sender, WORD Error);
	void __fastcall CancelButtonClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TFingerDemoForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TFingerDemoForm *FingerDemoForm;
//---------------------------------------------------------------------------
#endif
