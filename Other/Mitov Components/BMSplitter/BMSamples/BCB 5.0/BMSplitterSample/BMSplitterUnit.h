//---------------------------------------------------------------------------
#ifndef BMSplitterUnitH
#define BMSplitterUnitH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include "BMSplitter.h"
#include <vcl\ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TBMSplitter *BMSplitter1;
	TPanel *Panel1;
	TPanel *Panel2;
	TBMSplitter *BMSplitter2;
	TPanel *Panel3;
	TMemo *Memo2;
	TPanel *Panel4;
	TMemo *Memo3;
	TPanel *Panel5;
        TMemo *Memo1;
private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
