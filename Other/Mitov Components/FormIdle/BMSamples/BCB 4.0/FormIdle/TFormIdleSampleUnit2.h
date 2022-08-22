//---------------------------------------------------------------------------
#ifndef TFormIdleSampleUnit2H
#define TFormIdleSampleUnit2H
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include "Formidle.h"
#include "FormIdle.h"
//---------------------------------------------------------------------------
class TForm2 : public TForm
{
__published:	// IDE-managed Components
	TEdit *NameEdit;
	TLabel *Label1;
	TButton *OKButton;
	TButton *CancelButton;
	TFormIdle *FormIdle1;
	void __fastcall FormIdle1Idle(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TForm2(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TForm2 *Form2;
//---------------------------------------------------------------------------
#endif
