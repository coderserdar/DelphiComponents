//---------------------------------------------------------------------------
#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BMGifControl.h"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
    TImage *Image1;
    TBMGIFControl *BMGIFControl1;
    TLabel *Label1;
    TBMGIFControl *BMGIFControl2;
    TLabel *Label2;
    TBMGIFControl *BMGIFControl3;
    TLabel *Label3;
    TBMGIFControl *BMGIFControl4;
    TBMGIFControl *BMGIFControl5;
    TBMGIFControl *BMGIFControl6;
private:	// User declarations
public:		// User declarations
    __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
