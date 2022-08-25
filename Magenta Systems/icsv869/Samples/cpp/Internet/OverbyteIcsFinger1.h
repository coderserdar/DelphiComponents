//---------------------------------------------------------------------------
#ifndef OverbyteIcsFinger1H
#define OverbyteIcsFinger1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include "OverbyteIcsFingCli.hpp"
#include "OverbyteIcsWSocket.hpp"
#include "OverbyteIcsWndControl.hpp"
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
